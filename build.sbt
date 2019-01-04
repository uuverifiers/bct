lazy val bct = (project in file("."))
  .settings(
    name := "Bounded Connection Tableaux",
    version := "0.1",
    scalaVersion := "2.12.7",
    libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.5",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test",
    resolvers += "uuverifiers" at "http://logicrunch.research.it.uu.se/maven/",
    libraryDependencies += "uuverifiers" %% "princess" % "2018-12-06"
)

parallelExecution in Test := false
logBuffered in Test := false


