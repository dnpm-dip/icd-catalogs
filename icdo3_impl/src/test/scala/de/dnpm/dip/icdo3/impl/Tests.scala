package de.dnpm.dip.icdo3.impl


import scala.util.Success
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers._
import org.scalatest.OptionValues._
import org.scalatest.Inspectors._
import cats.Id
import de.dnpm.dip.coding.{
  Coding,
  CodeSystemProvider
}
import de.dnpm.dip.coding.icd.ICDO3
import de.dnpm.dip.coding.icd.ClassKinds.Category
import de.dnpm.dip.coding.icd.ICD.extensions._
import de.dnpm.dip.coding.icd.ICD.ClassKind


class Tests extends AnyFlatSpec
{

  val icdo3CspTry = CodeSystemProvider.getInstance[Id]
  lazy val icdo3Csp = icdo3CspTry.get

  "ICD-O-3 Catalogs" must "have been loaded successfully as CodeSystemProvider[Any,...]" in {

    icdo3CspTry must be (a [Success[_]])

  }

  it must "have the expected Coding System URI" in {
    icdo3Csp.uri must be (Coding.System[ICDO3].uri)
  }


  val icdo3CatalogsTry = ICDO3.Catalogs.getInstance[Id]
  lazy val icdo3Catalogs = icdo3CatalogsTry.get

  "ICD-O-3 Catalogs" must "have been loaded successfully as ICDO3.Catalogs" in {

    icdo3CatalogsTry must be (a [Success[_]])

  }

  "ClassKind" must "be defined on all ICD-O-3 classes" in {

    forAll(
      icdo3Catalogs.latest.concepts
    ){
      _.get(ClassKind) must be (defined)
    }

  }

 
  "ICD-O-3 Topography codes" must "have been correctly filtered" in {

    val icdo3T = icdo3Catalogs.topography.concepts

    icdo3T must not be empty

    icdo3Catalogs.topography.conceptWithCode("T") mustBe defined

    all (icdo3T.map(_.code.value)) must (be ("T") or startWith ("C"))
    
  }


  "ICD-O-3 Morphology codes" must "have been correctly filtered" in {

    val icdo3M = icdo3Catalogs.morphology.concepts

    icdo3M must not be empty

    icdo3Catalogs.morphology.conceptWithCode("M") mustBe defined

    icdo3M.filter(_.classKind == Category) must not be empty

    all (icdo3M.map(_.code.value)) must (be ("M") or startWith regex """\d{1,}""")
    
  }

  "References" must "have been correctly extracted from Labels" in {

    val coding =
      icdo3Catalogs.morphology.conceptWithCode("8350/3")

    coding.value.display must include ("[C73.9]")

  }

}
