val circeVersion = "0.14.1"
val tapirVersion = "0.19.0-M8"

lazy val root = (project in file("."))
  .settings(
    scalaVersion := "2.13.6",
    name := "scraml-ct-api-circe-test",
    version := "0.1",
    ramlFile := Some(file("reference/api-specs/api/api.raml")),
    basePackageName := "de.commercetools.api",
    librarySupport := Set(scraml.libs.CirceJsonSupport, scraml.libs.TapirSupport("Endpoints")),
    Compile / sourceGenerators += runScraml,
    libraryDependencies ++= Seq(
        "io.circe" %% "circe-core",
        "io.circe" %% "circe-generic",
        "io.circe" %% "circe-parser"
    ).map(_ % circeVersion),
    libraryDependencies += "com.softwaremill.sttp.tapir" %% "tapir-core" % tapirVersion,
    libraryDependencies += "com.softwaremill.sttp.tapir" %% "tapir-json-circe" % tapirVersion
  )

