lazy val bct = (project in file("."))
  .settings(
    name := "Bounded Connection Tableaux",
    version := "0.1",
    scalaVersion := "2.12.7",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % Test,
    resolvers += "uuverifiers" at "http://logicrunch.research.it.uu.se/maven/",
    libraryDependencies += "uuverifiers" %% "princess" % "2018-12-06"
)

parallelExecution in Test := false


