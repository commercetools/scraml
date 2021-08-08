val circeVersion = "0.14.1"

lazy val root = (project in file("."))
  .settings(
    name := "scraml-ct-api-test",
    version := "0.1",
    ramlFile := Some(file("reference/api-specs/api/api.raml")),
    basePackageName := "de.commercetools.api",
    jsonSupport := Some(scraml.Circe),
    libraryDependencies += "com.commercetools" %% "sphere-json" % "0.12.5",
    libraryDependencies ++= Seq(
        "io.circe" %% "circe-core",
        "io.circe" %% "circe-generic",
        "io.circe" %% "circe-parser"
    ).map(_ % circeVersion)
  )
