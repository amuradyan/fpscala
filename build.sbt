ThisBuild / scalaVersion := "3.1.0"
ThisBuild / organization := "am.dekanat"
ThisBuild / name := "FP in Scala"

lazy val hello = (project in file("."))
  .settings(
    name := "FP in Scala",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.11" % Test,
    parallelExecution in Test := false
  )
