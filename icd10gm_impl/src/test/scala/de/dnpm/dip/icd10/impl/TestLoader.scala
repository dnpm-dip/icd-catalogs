package de.dnpm.dip.icd10gm.impl


import java.io.InputStream
import cats.Eval
import cats.data.NonEmptyList
  
  
class TestLoaderSPI extends ICD10GMCatalogsImpl.LoaderSPI
{
  override lazy val getInstance =
    TestLoader
}


object TestLoader extends ICD10GMCatalogsImpl.Loader
{

  private val versions =
    NonEmptyList.of(2020,2021,2022,2023).map(_.toString)


  override def input =
    versions.map( v =>
      v -> Eval.now(
        ICD10GMCatalogsImpl.ClaMLParser.parse(
          this.getClass.getClassLoader
            .getResourceAsStream(s"icd10gm${v}syst_claml.xml")
        )
      )
    )

}
