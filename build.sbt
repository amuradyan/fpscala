ThisBuild / scalaVersion := "3.2.1"
ThisBuild / organization := "am.dekanat"
ThisBuild / name := "FP in Scala"

lazy val fpscala = (project in file("."))
  .settings(
    name := "FP in Scala",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.15" % Test,
    parallelExecution in Test := false,
    // scalafix
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision
  )
