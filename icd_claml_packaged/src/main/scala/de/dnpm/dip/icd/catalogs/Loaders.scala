package de.dnpm.dip.icd.catalogs


import cats.Eval
import cats.data.NonEmptyList
import de.dnpm.dip.icd10gm.impl.ICD10GMCatalogsImpl
import de.dnpm.dip.icdo3.impl.ICDO3Catalogs

  
class PackagedICD10GMCatalogLoaderSPI extends ICD10GMCatalogsImpl.LoaderSPI
{
  def getInstance = PackagedICD10GMCatalogLoader
}


object PackagedICD10GMCatalogLoader extends ICD10GMCatalogsImpl.Loader
{

  private val versions =
    NonEmptyList.fromListUnsafe(
      (2019 to 2025).map(_.toString).toList
    )

  override def input =
    versions.map { year =>

      val in = s"icd10gm${year}.xml"

      ICD10GMCatalogsImpl.ClaMLParser.versionIn(
        this.getClass.getClassLoader.getResourceAsStream(in)
      ) -> Eval.later(
        ICD10GMCatalogsImpl.ClaMLParser.parse(
          this.getClass.getClassLoader.getResourceAsStream(in)
        )
      )
    
    }    

}



class PackagedICDO3CatalogLoaderSPI extends ICDO3Catalogs.LoaderSPI
{
  def getInstance = PackagedICDO3CatalogLoader
}


object PackagedICDO3CatalogLoader extends ICDO3Catalogs.Loader
{

  private val versions =
    NonEmptyList.of(2014,2019).map(_.toString)


  override def input =
    versions.map { year =>

      val in = s"icdo3${year}.xml"

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
