package de.dnpm.dip.icd10gm.impl


import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers._
import org.scalatest.Inspectors._
import scala.util.Success
import cats.Id

import de.dnpm.dip.coding.{
  Coding,
  CodeSystemProvider
}
import de.dnpm.dip.coding.icd.{
  ClassKinds,
  ICD,
  ICD10GM
}


class ICD10GMCatalogTests extends AnyFlatSpec
{

  import ICD10GM._
  import ICD.ClassKind
  import ICD.extensions._


  val icd10CspTry = CodeSystemProvider.getInstance[Id]
  lazy val icd10Csp = icd10CspTry.get
 
  "ICD10GMCatalogs" must "have been successfully loaded as CodeSystemProvider[Any,...]" in {
    icd10CspTry must be (a [Success[_]])
  }

  it must "have the expected Coding System URI" in {
    icd10Csp.uri must be (Coding.System[ICD10GM].uri)
  }


  val icd10CatalogsTry = ICD10GM.Catalogs.getInstance[Id]

  lazy val icd10Catalogs =
    icd10CatalogsTry.get
 
  "ICD10GMCatalogs" must "have been successfully loaded" in {
    icd10CatalogsTry must be (a [Success[_]])
  }


  "ClassKind" must "be defined on all ICD-10-GM classes" in {

    forAll(
      icd10Catalogs.latest.concepts
    ){
      _.get(ClassKind) must be (defined)
    }

  }


  "ClassKind filters" must "have worked" in {

    forAll( filterByClassKind ){

      case (kind,f) =>

        icd10Catalogs.latest
          .filter(f)
          .concepts
          .map(_.classKind) must contain only (kind)
      
    }

  }


  "ClassKind filters combinations" must "have worked" in {

    assert(
      ClassKinds.values.toSet
        .subsets(2)
        .forall { classKinds =>

        val f =
          classKinds.map(filterByClassKind(_)).reduce(_ or _)

        icd10Catalogs.latest
          .filter(f)
          .concepts
          .map(_.classKind)
          .forall(classKinds.contains)
      }
    )

  }


}
