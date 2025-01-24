lazy val root = (project in file("."))
  .settings(
    scalaVersion := "2.13.16",
    crossScalaVersions ++= Seq("3.3.4"),
    name := "scraml-simple-test",
    version := "0.1",
    ramlFile := Some(file("api/simple.raml")),
    ramlFieldMatchPolicy := scraml.FieldMatchPolicy.Exact(),
    Compile / sourceGenerators += runScraml
  )
