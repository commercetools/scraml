val circeVersion = "0.14.10"

lazy val root = (project in file("."))
  .settings(
    scalaVersion := "2.13.16",
    name := "scraml-ct-api-sphere-test",
    version := "0.1",
    ramlFile := Some(file("reference/api-specs/api/api.raml")),
    basePackageName := "de.commercetools.api",
    librarySupport := Set(scraml.libs.SphereJsonSupport),
    Compile / sourceGenerators += runScraml,
    libraryDependencies += "com.commercetools" %% "sphere-json" % "0.12.5"
  )

