val circeVersion = "0.14.1"

lazy val root = (project in file("."))
  .settings(
    scalaVersion := "2.12.12",
    name := "scraml-ct-api-sphere-test",
    version := "0.1",
    ramlFile := Some(file("reference/api-specs/api/api.raml")),
    basePackageName := "de.commercetools.api",
    librarySupport := Set(scraml.libs.SphereJsonSupport),
    Compile / sourceGenerators += runScraml,
    libraryDependencies += "com.commercetools" %% "sphere-json" % "0.12.5"
  )

