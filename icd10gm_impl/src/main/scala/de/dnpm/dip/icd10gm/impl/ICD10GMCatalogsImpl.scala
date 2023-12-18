package de.dnpm.dip.icd10gm.impl


import java.io.{
  File,
  InputStream,
  FileInputStream
}
import java.time.{
  LocalDate,
  LocalDateTime
}
import java.time.LocalTime.MIN
import java.time.format.DateTimeFormatter.ISO_LOCAL_DATE
import scala.xml.XML
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
import de.dnpm.dip.coding.icd.ICD10GM


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

      val concepts =
        (claml \\ "Class")
          .map { cl =>

            val kind   = (cl \@ "kind")

            val code   = (cl \@ "code")
            val rubric = (cl \ "Rubric")
            val label  = (rubric.find(_ \@ "kind" == "preferredLong")
                            .orElse(rubric.find(_ \@ "kind" == "preferred")).get) \ "Label" text
 
            val superclass = Option(cl \ "SuperClass" \@ "code").map(Code[ICD10GM](_))

            val subclasses = (cl \ "SubClass").map((_ \@ "code")).toSet.map(Code[ICD10GM](_))

            val properties = Map(ICD10GM.ClassKind.name -> Set(kind))
 
            Concept[ICD10GM](
              Code(code),
              label,
              version,
              properties,
              superclass,
              Option(subclasses).filterNot(_.isEmpty)
            )

          }

      CodeSystem[ICD10GM](
        Coding.System[ICD10GM].uri,
        name,
        title,
        date,
        version,
        ICD10GM.properties,
        concepts 
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

    import scala.util.matching._

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
