package de.dnpm.dip.icdo3.impl


import java.io.InputStream
import java.time.LocalDate
import cats.Eval
import cats.data.NonEmptyList
  
  
class TestLoaderSPI extends ICDO3Catalogs.LoaderSPI
{
  override lazy val getInstance =
    TestLoader
}


object TestLoader extends ICDO3Catalogs.Loader
{

  private val versions =
    NonEmptyList.of(2014,2019).map(_.toString)


  override def input =
    versions.map { v =>

      val in = s"icdo3${v}.xml"

      val (version,date) =
        ICDO3Catalogs.ClaMLParser.versionDate(
          this.getClass.getClassLoader.getResourceAsStream(in)
        )

      (
        version,
        date,
        Eval.later(
          ICDO3Catalogs.ClaMLParser.parse(
            this.getClass.getClassLoader.getResourceAsStream(in)
          )
        )
      )
    }


}
