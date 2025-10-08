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

      val modifiers =
        (claml \\ "Modifier").map {
          modifier => (modifier \@ "code") -> (modifier \ "SubClass").map(_ \@ "code").toSet
        }
        .toMap

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

          val validModifierClasses =
            (cl \ "ModifiedBy").headOption.map(
              modifiedBy => (modifiedBy \@ "all") match {
                case "false" => (modifiedBy \\ "ValidModifierClass").map((_ \@ "code")).toSet
                case _       => modifiers(modifiedBy \@ "code")
              }
            )

          val properties =
            Concept.properties(ICD.ClassKind -> Set(kind)) ++ validModifierClasses.map(ICD10GM.ValidModifierClasses.name -> _)


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
        Some {
          (code: Code[ICD10GM]) => concepts.find(
            concept =>
              concept.get(ICD10GM.ValidModifierClasses) match {
                case None            => concept.code == code
                case Some(modifiers) => raw"${concept.code.value}(${modifiers.mkString("|")})?".r matches code.value
              }
          )
          .map(
            concept => concept.copy(
              code = code,
              properties = concept.properties.removed(ICD10GM.ValidModifierClasses.name) // remove ICD10GM.ValidModifierClasses in the copied concept with modified/extended code
            )
          )
        }
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
