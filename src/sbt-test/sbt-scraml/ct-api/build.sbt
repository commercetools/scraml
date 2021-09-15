val circeVersion = "0.14.1"

lazy val root = (project in file("."))
  .settings(
    scalaVersion := "2.13.6",
    name := "scraml-ct-api-circe-test",
    version := "0.1",
    ramlFile := Some(file("reference/api-specs/api/api.raml")),
    basePackageName := "de.commercetools.api",
    librarySupport := Set(scraml.libs.CirceJsonSupport()),
    Compile / sourceGenerators += runScraml,
    libraryDependencies ++= Seq(
        "io.circe" %% "circe-core",
        "io.circe" %% "circe-generic",
        "io.circe" %% "circe-parser"
    ).map(_ % circeVersion)
  )

