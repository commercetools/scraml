lazy val root = (project in file("."))
  .settings(
    name := "scraml-ct-api-test",
    version := "0.1",
    ramlFile := Some(file("reference/api-specs/api/api.raml")),
    basePackageName := "de.commercetools.api",
    jsonSupport := Some(scraml.Sphere),
    libraryDependencies += "com.commercetools" %% "sphere-json" % "0.12.5"
  )
