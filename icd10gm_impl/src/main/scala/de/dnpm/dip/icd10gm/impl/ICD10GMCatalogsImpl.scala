package de.dnpm.dip.icd10gm.impl


import java.io.{
  File,
  InputStream,
  FileInputStream
}
import java.time.{
  LocalDate,
}
import java.time.LocalTime.MIN
import java.time.format.DateTimeFormatter.ISO_LOCAL_DATE
import scala.xml.{
  XML,
  Elem
}
import scala.util.chaining._
import scala.collection.concurrent.{
  Map,
  TrieMap
}
import cats.data.NonEmptyList
import cats.Applicative
import cats.Eval
import de.dnpm.dip.util.{
  Logging,
  SPI,
  SPILoader
}
import de.dnpm.dip.coding.{
  Code,
  Coding,
  CodeSystem,
  CodeSystemProvider,
  CodeSystemProviderSPI,
  Version
}
import de.dnpm.dip.coding.icd.{
  ICD,
  ICD10GM
}


class ICD10GMCodeSystemProviderSPI extends CodeSystemProviderSPI
{

  def getInstance[F[_]]: CodeSystemProvider[Any,F,Applicative[F]] =
    new ICD10GMCatalogsImpl.Facade[F]
}


class ICD10GMCatalogsSPIImpl extends ICD10GM.CatalogsSPI
{

  def getInstance[F[_]]: ICD10GM.Catalogs[F,Applicative[F]] =
    new ICD10GMCatalogsImpl.Facade[F]
}


object ICD10GMCatalogsImpl extends Logging
{

  private val ModifiedBy    = "ModifiedBy"

  final case class Modifier
  (
    code: String,
    subClasses: Set[String]
  )

  final case class ModifierClass
  (
    code: String,
    modifier: String,
    superClass: String,
    metas: Option[List[ModifierClass.Meta]]
  )

  object ModifierClass
  {
    final case class Meta
    (
      name: String,
      value: String
    )

    val ExclusionRule = "excludeOnPrecedingModifier"
  }


  final object ClaMLParser
  {
    import CodeSystem.Concept

    def versionIn(in: InputStream): String = {

      val claml = XML.load(in)

      claml \ "Title" \@ "version"

    }


    def parse(in: InputStream): CodeSystem[ICD10GM] = {
 
      val claml = XML.load(in)
 
      val name    = (claml \ "Title" \@ "name")
      val version = Some(claml \ "Title" \@ "version")
      val title   = Some(((claml \ "Title") text))
      val date    = Some(LocalDate.parse(claml \ "Title" \@ "date", ISO_LOCAL_DATE).atTime(MIN))

      val modifiersByCode =
        (claml \\ "Modifier").map(
          modifier => Modifier(
            (modifier \@ "code"),
            (modifier \ "SubClass").map(_ \@ "code").toSet
          )
        )
        .map(m => m.code -> m)
        .toMap


      val modifierClassesByCode =
        (claml \\ "ModifierClass").map {
          mc => ModifierClass(
            (mc \@ "code"),
            (mc \@ "modifier"),
            (mc \ "SuperClass" \@ "code"),
            Option((mc \\ "Meta").map(meta => ModifierClass.Meta(meta \@ "name",meta \@ "value")).toList).filter(_.nonEmpty)
//            Option((mc \\ "Meta").map(meta => (meta \@ "name") -> (meta \@ "value")).toList).filter(_.nonEmpty)
          )
        }
        .toList
        .groupBy(_.modifier)


      val concepts =
        (claml \\ "Class").map { cl =>

          val kind   = (cl \@ "kind")
          val code   = (cl \@ "code")
          val rubric = (cl \ "Rubric")

          val label  =
            ((rubric.find(_ \@ "kind" == "preferredLong")
              .orElse(rubric.find(_ \@ "kind" == "preferred")).get) \ "Label").head match {
              case l: Elem =>
                l.child.collect {
                  case ref if ref.label == "Reference" => s"[${ref.text}]"
                  case node => node.text
                }
                .mkString(" ")
            }

 
          val superclass = Option(cl \ "SuperClass" \@ "code").filterNot(_.isEmpty).map(Code[ICD10GM](_))

          val subclasses = (cl \ "SubClass").map((_ \@ "code")).toSet.map(Code[ICD10GM](_))

          val modifiers =
            Option((cl \\ ModifiedBy).map(modifiedBy => (modifiedBy \@ "code")).toSet)
              .filter(_.nonEmpty)

          val validModifierClasses =
            (cl \ ModifiedBy).headOption.collect {
              case modifiedBy if (modifiedBy \@ "all") == "false" =>
                (modifiedBy \\ "ValidModifierClass").map((_ \@ "code")).toSet
            }

          val properties =
            Concept.properties(ICD.ClassKind -> Set(kind)) ++
              modifiers.map(ModifiedBy -> _) ++
              validModifierClasses.map(ICD10GM.ValidModifierClasses.name -> _)


          Concept[ICD10GM](
            Code(code),
            label,
            version,
            properties,
            superclass,
            Option.when(subclasses.nonEmpty)(subclasses)
          )

        }

      CodeSystem[ICD10GM](
        Coding.System[ICD10GM].uri,
        name,
        title,
        date,
        version,
        ICD10GM.properties,
        concepts,
        // ICD-10-specific code resolution logic, taking possible modifiers into account via Regex matching
        Some(
          (code: Code[ICD10GM]) => concepts.find(
            concept => concept.properties.get(ModifiedBy) match {

              // Check exact code equality when no modifiers are defined
              case None => concept.code == code
              
              case Some(modifiers) =>
                concept.get(ICD10GM.ValidModifierClasses) match {

                  // When a specific subset of ValidModifierClasses is defined, add these as optional capture group
                  case Some(validModifiers) =>
                    raw"${concept.code.value}(${validModifiers.mkString("|")})?".r matches code.value


                  // Special case when 2 Modifiers are defined:
                  // Then one always adds additional modifiers to another, i.e. there is a primary and secondary/dependent modifier
                  case None if modifiers.size == 2 =>

                    // resolve the primary modifier as that one referenced by another in an "exclusion rule"
                    val (primaryModifier,secondaryModifier) =
                      modifiers
                        .map(m => m -> modifierClassesByCode(m))
                        .collectFirst { 
                          case (modifier,mcs) if mcs exists (_.metas.exists(_.exists(_.name == ModifierClass.ExclusionRule))) =>
                            modifiersByCode(mcs.head.metas.get.head.value.split(" ")(0)) -> modifiersByCode(modifier)
                        }
                        .get


                    // Build tree of modifier codes as a Map of primary modifier code -> secondary modifier codes with applied exclusions
                    val modifierTree =
                      primaryModifier
                        .subClasses
                        .map(code => code -> secondaryModifier.subClasses)
                        .toMap
                        .pipe(
                          tree =>
                            modifierClassesByCode(secondaryModifier.code).foldLeft(tree){
                              (acc,mc) =>
                                mc.metas.map(_.filter(_.name == ModifierClass.ExclusionRule)) match {
                                  case Some(exclusionRules) =>
                                    exclusionRules.foldLeft(acc){
                                      (acc2,rule) => 
                                        val precedingCode = rule.value.split(" ")(1)

                                        acc2.updatedWith(precedingCode){
                                          case Some(set) => Some(set - mc.code)
                                          case None => None
                                        }
                                    }

                                  case _ => acc
                                }

                            }

                        )

                     raw"${concept.code.value}(${modifierTree.map{ case (primary,secondaries) => s"$primary(${secondaries.mkString("|")})?"}.mkString("|")})?".r matches code.value
                    

                  case None => 
                    raw"${concept.code.value}(${modifiers.flatMap(modifiersByCode(_).subClasses).mkString("|")})?".r matches code.value

                }
            }
          )
          .map(
            concept => concept.copy(
              code = code,
              properties =
                concept.properties
                  .removed(ModifiedBy)
                  .removed(ICD10GM.ValidModifierClasses.name) // remove modifier-related properties in the copied concept with modified/extended code
            )
          )
        )
      )

    }
  }


  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------
  trait Loader
  {
    def input: NonEmptyList[(String,Eval[CodeSystem[ICD10GM]])]
  }

  trait LoaderSPI extends SPI[Loader]

  private object Loader extends SPILoader[LoaderSPI]


  private class DefaultLoader extends Loader
  {
    val sysProp = "dnpm.dip.catalogs.dir"

    val icd10gmFile = """icd10gm.*\.xml""".r

    override def input: NonEmptyList[(String,Eval[CodeSystem[ICD10GM]])] = {
      Option(System.getProperty(sysProp)).map(new File(_)) match {

        case None => {
          val msg = s"Please define the directory from which to load ICD-10-GM catalogs using System Property $sysProp"
          log.error(msg)

          throw new NoSuchElementException(msg)
        }

        case Some(dir) =>
          NonEmptyList.fromListUnsafe(
            dir.listFiles
              .collect {
                file => file.getName match {
                  case icd10gmFile() =>
                    ClaMLParser.versionIn(new FileInputStream(file)) -> Eval.later(ClaMLParser.parse(new FileInputStream(file)))
                }
              }
              .toList
          )
      }
    }

  }
  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  private val catalogs: Map[String,Eval[CodeSystem[ICD10GM]]] =
    TrieMap.from(
      Loader.getInstance
        .getOrElse(new DefaultLoader)
        .input
        .toList
    )


  private [impl] class Facade[F[_]] extends ICD10GM.Catalogs[F,Applicative[F]]
  {
    import cats.syntax.functor._
    import cats.syntax.applicative._

    override val uri =
     Coding.System[ICD10GM].uri

    override val versionOrdering =
      Version.OrderedByYear

    override def versions(
      implicit F: Applicative[F]
    ): F[NonEmptyList[String]] = 
      NonEmptyList.fromListUnsafe(catalogs.keys.toList)
        .pure


    override def latestVersion(
      implicit F: Applicative[F]
    ): F[String] =
      versions.map(_.toList.maxBy(_.toInt))

    override def filters(
      implicit F: Applicative[F]
    ): F[List[CodeSystem.Filter[ICD10GM]]] =
      ICD10GM.filters.pure

    override def get(
      version: String
    )(
      implicit F: Applicative[F]
    ): F[Option[CodeSystem[ICD10GM]]] =
      catalogs.get(version).map(_.value).pure

    override def latest(
      implicit F: Applicative[F]
    ): F[CodeSystem[ICD10GM]] =
      latestVersion.map(
        catalogs(_).value
      )

  }

}
