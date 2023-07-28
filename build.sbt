
/*
 build.sbt adapted from https://github.com/pbassiner/sbt-multi-project-example/blob/master/build.sbt
*/


name := "icd"
ThisBuild / organization := "de.dnpm.dip"
ThisBuild / scalaVersion := "2.13.10"
ThisBuild / version      := "1.0-SNAPSHOT"


//-----------------------------------------------------------------------------
// PROJECTS
//-----------------------------------------------------------------------------

lazy val global = project
  .in(file("."))
  .settings(
    settings,
    publish / skip := true
  )
  .aggregate(
     icd10gm_impl,
     icdo3_impl,
     icd_claml_packaged,
//     tests
  )


lazy val icd10gm_impl = project
  .settings(
    name := "icd10gm-impl",
    settings,
    libraryDependencies ++= Seq(
      dependencies.scalatest,
      dependencies.core,
      dependencies.scala_xml,
    )
  )


lazy val icdo3_impl = project
  .settings(
    name := "icdo3-impl",
    settings,
    libraryDependencies ++= Seq(
      dependencies.scalatest,
      dependencies.core,
      dependencies.scala_xml,
    )
  )


lazy val icd_claml_packaged = project
  .settings(
    name := "icd-claml-packaged",
    settings,
    libraryDependencies ++= Seq( )
  )
  .dependsOn(
    icd10gm_impl,
    icdo3_impl
  )


lazy val tests = project
  .settings(
    name := "tests",
    settings,
    libraryDependencies ++= Seq(
      dependencies.scalatest,
    ),
    publish / skip := true
  )
  .dependsOn(
    icd10gm_impl % Test,
    icdo3_impl % Test,
    icd_claml_packaged % Test
  )


//-----------------------------------------------------------------------------
// DEPENDENCIES
//-----------------------------------------------------------------------------

lazy val dependencies =
  new {
    val scalatest  = "org.scalatest"           %% "scalatest"   % "3.1.1" % Test
    val slf4j      = "org.slf4j"               %  "slf4j-api"   % "1.7.32"
    val scala_xml  = "org.scala-lang.modules"  %% "scala-xml"   % "2.0.1"
    val core       = "de.dnpm.dip"             %% "core"        % "1.0-SNAPSHOT"
  }


//-----------------------------------------------------------------------------
// SETTINGS
//-----------------------------------------------------------------------------

lazy val settings = commonSettings

lazy val compilerOptions = Seq(
  "-encoding", "utf8",
  "-unchecked",
  "-feature",
  "-language:postfixOps",
  "-Xfatal-warnings",
  "-deprecation",
)

lazy val commonSettings = Seq(
  scalacOptions ++= compilerOptions,
  resolvers ++=
    Seq("Local Maven Repository" at "file://" + Path.userHome.absolutePath + "/.m2/repository") ++
    Resolver.sonatypeOssRepos("releases") ++
    Resolver.sonatypeOssRepos("snapshots")
)

