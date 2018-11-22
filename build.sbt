ThisBuild / scalaVersion := "2.12.7"
ThisBuild / organization := "bct"

lazy val bct = (project in file("."))
  .settings(
    name := "Bounded Connection Tableaux",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % Test,
)

