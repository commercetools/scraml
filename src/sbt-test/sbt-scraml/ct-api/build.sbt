lazy val root = (project in file("."))
  .settings(
    name := "scraml-ct-api-test",
    version := "0.1",
    ramlFile := Some(file("reference/api-specs/api/api.raml")),
    packageName := "de.commercetools.api"
  )