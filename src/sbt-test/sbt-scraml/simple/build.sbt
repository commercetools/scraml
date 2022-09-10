lazy val root = (project in file("."))
  .settings(
    name := "scraml-simple-test",
    version := "0.1",
    ramlFile := Some(file("api/simple.raml")),
    ramlFieldMatchPolicy := scraml.FieldMatchPolicy.Exact(),
    Compile / sourceGenerators += runScraml
  )
