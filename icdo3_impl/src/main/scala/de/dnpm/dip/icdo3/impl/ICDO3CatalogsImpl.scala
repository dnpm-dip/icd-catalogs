package de.dnpm.dip.icdo3.impl


import java.io.{
  File,
  InputStream,
  FileInputStream
}
import java.net.URI
import java.time.{
  LocalDate,
  LocalDateTime
}
import java.time.format.DateTimeFormatter.ISO_LOCAL_DATE
import java.time.LocalTime.MIN
import scala.xml.XML
import scala.collection.concurrent.{
  Map,
  TrieMap
}
import scala.util.matching.Regex
import cats.data.NonEmptyList
import cats.Eval
import cats.Applicative
import de.dnpm.dip.util.{
  Logging,
  SPI,
  SPILoader
}
import de.dnpm.dip.coding.{
  Code,
  CodeSystem,
  Coding,
  ValueSet,
  CodeSystemProvider,
  CodeSystemProviderSPI
}
import de.dnpm.dip.coding.icd.ICDO3


//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
class ICDO3CodeSystemProviderSPI extends CodeSystemProviderSPI
{
  def getInstance[F[_]]: CodeSystemProvider[Any,F,Applicative[F]] =
    new ICDO3Catalogs.Facade[F]
}
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------


class ICDO3ProviderImpl extends ICDO3.CatalogsSPI
{
  def getInstance[F[_]]: ICDO3.Catalogs[F,Applicative[F]] =
    new ICDO3Catalogs.Facade[F]
}


object ICDO3Catalogs extends Logging
{

  final object ClaMLParser
  {
    import CodeSystem.Concept

    val morphologyCategory = """\d{4}:\d{1}""".r

    def versionDate(in: InputStream): (String,LocalDate) = {

      val claml = XML.load(in)

      val version = claml \ "Title" \@ "version"
      val date    = LocalDate.parse(claml \ "Title" \@ "date", ISO_LOCAL_DATE)

      version -> date
    }


    def parse(in: InputStream): CodeSystem[ICDO3] = {
  
      // In user-facing representation, the morphology category code
      // is always e.g. "8050/0", so replace colon : with slash
      def format(code: String): String =
        code match { 
          case morphologyCategory() => code.replace(":","/")
          case _                    => code
        }
              

      import scala.util.chaining._
 
      val claml = XML.load(in)
 
      val name    = (claml \ "Title" \@ "name")
      val version = Some(claml \ "Title" \@ "version")
      val title   = Some(((claml \ "Title") text))
      val date    = Some(LocalDate.parse(claml \ "Title" \@ "date", ISO_LOCAL_DATE).atTime(MIN))

      val concepts =
        (claml \\ "Class").map {
          cl =>

            val kind   = (cl \@ "kind")

            val code   =
              (cl \@ "code") pipe format


            val label  = (cl \ "Rubric").find(_ \@ "kind" == "preferred").get \ "Label" text
 
            val superclass = Option(cl \ "SuperClass" \@ "code").map(Code[ICDO3](_))

            val subclasses = (cl \ "SubClass").map((_ \@ "code")).map(format).toSet.map(Code[ICDO3](_))

            val properties = Map(ICDO3.ClassKind.name -> Set(kind))
 
            Concept[ICDO3](
              Code(code),
              label,
              version,
              properties,
              superclass,
              Option(subclasses).filterNot(_.isEmpty)
            )

          }

      CodeSystem[ICDO3](
        Coding.System[ICDO3].uri,
        name,
        title,
        date,
        version,
        ICDO3.properties,
        concepts 
      )  
 
    }
  }


  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------
  trait Loader
  {
    def input: NonEmptyList[(String,LocalDate,Eval[CodeSystem[ICDO3]])]
  }

  trait LoaderSPI extends SPI[Loader]

  private object Loader extends SPILoader[LoaderSPI]


  private class DefaultLoader extends Loader
  {
    val sysProp = "dnpm.dip.catalogs.dir"


    val icdo3File = """icdo3.*\.xml""".r

    override def input: NonEmptyList[(String,LocalDate,Eval[CodeSystem[ICDO3]])] = {

      Option(System.getProperty(sysProp)).map(new File(_)) match {

        case None => {
          val msg = s"Please define the directory from which to load ICD-O-3 catalogs using System Property $sysProp"
          log.error(msg)

          throw new NoSuchElementException(msg)
        }

        case Some(dir) =>
          NonEmptyList.fromListUnsafe(
            dir.listFiles
            .collect {
              file => file.getName match {
                case icdo3File() =>

                  val (version,date) =
                    ClaMLParser.versionDate(new FileInputStream(file))

                  (version,date,Eval.later(ClaMLParser.parse(new FileInputStream(file))))
              }
            }
            .toList
          )
      }
    }


  }
  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  private val versionsByDate: Map[LocalDate,String] =
    TrieMap.empty

  private val catalogs: Map[String,Eval[CodeSystem[ICDO3]]] =
    TrieMap.from(
      Loader.getInstance
        .getOrElse(new DefaultLoader)
        .input
        .map {
          case (version,date,in) =>
            versionsByDate += date -> version
            version -> in
        }
        .toList
    )

  private val topographyCatalogs: Map[String,Eval[CodeSystem[ICDO3.Topography]]] =
    TrieMap.from(
      catalogs.map {
        case (version,eval) => 
          version -> eval.flatMap {
            cs => Eval.later {

              val root = cs.concept(Code("T")).get
             
              cs.copy(
                concepts = cs.descendantsOf(root.code).toSeq
              ) 
              .asInstanceOf[CodeSystem[ICDO3.Topography]]

            }
          }
      }
    )

  private val morphologyCatalogs: Map[String,Eval[CodeSystem[ICDO3.Morphology]]] =
    TrieMap.from(
      catalogs.map {
        case (version,eval) => 
          version -> eval.flatMap {
            cs => Eval.later {

              val root = cs.concept(Code("M")).get
             
              cs.copy(
                concepts = cs.descendantsOf(root.code).toSeq
              ) 
              .asInstanceOf[CodeSystem[ICDO3.Morphology]]

            }
          }
      }
    )

  private [impl] class Facade[F[_]] extends ICDO3.Catalogs[F,Applicative[F]]
  {
    import cats.syntax.functor._
    import cats.syntax.applicative._

    override val uri =
      Coding.System[ICDO3].uri


    override val versionOrdering =
      new Ordering[String]{
        override def compare(v1: String, v2: String) =
          (
            for {
              (y1,_) <- versionsByDate.find(_._2 == v1)
              (y2,_) <- versionsByDate.find(_._2 == v2)
            } yield y1 compareTo y2
          )
          .get
      }


    override def versions(
      implicit F: Applicative[F]
    ): F[NonEmptyList[String]] = 
      NonEmptyList.fromListUnsafe(
        catalogs.keys.toList
      )
      .pure


    override def latestVersion(
      implicit F: Applicative[F]
    ): F[String] =
      versionsByDate.maxBy(_._1)
        ._2
        .pure

    override def get(
      version: String
    )(
      implicit F: Applicative[F]
    ): F[Option[CodeSystem[ICDO3]]] =
      catalogs.get(version)
        .map(_.value)
        .pure

    override def latest(
      implicit F: Applicative[F]
    ): F[CodeSystem[ICDO3]] =
      latestVersion.map(
        catalogs(_).value
      )

    override def topography(
      version: String
    )(
      implicit F: Applicative[F]
    ): F[Option[CodeSystem[ICDO3.Topography]]] =
      topographyCatalogs.get(version).map(_.value).pure

    override def topography(
      implicit F: Applicative[F]
    ): F[CodeSystem[ICDO3.Topography]] =
      latestVersion.map(topographyCatalogs(_).value)


    override def morphology(
      version: String
    )(
      implicit F: Applicative[F]
    ): F[Option[CodeSystem[ICDO3.Morphology]]] =
      morphologyCatalogs.get(version).map(_.value).pure

    override def morphology(
      implicit F: Applicative[F]
    ): F[CodeSystem[ICDO3.Morphology]] =
      latestVersion.map(morphologyCatalogs(_).value)

  }

}


