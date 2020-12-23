ThisBuild / scalaVersion := "2.13.4"
ThisBuild / organization := "am.dekanat"
ThisBuild / name := "FP in Scala"

lazy val hello = (project in file("."))
  .settings(
    name := "FP in Scala",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.1" % Test,
    parallelExecution in Test := false
  )
