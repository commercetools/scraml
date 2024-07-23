val circeVersion = "0.14.2"

lazy val root = (project in file("."))
  .settings(
    name := "scraml-json-test",
    scalaVersion := "2.12.16",
    version := "0.1",
    ramlFile := Some(file("api/json.raml")),
    basePackageName := "scraml",
    librarySupport := Set(scraml.libs.CirceJsonSupport(formats = Map("localDateTime" -> "io.circe.Decoder.decodeLocalDateTime"))),
    defaultEnumVariant := Some("Unknown"),
    Compile / sourceGenerators += runScraml,
    libraryDependencies += "com.commercetools" %% "sphere-json" % "0.12.5",
    libraryDependencies ++= Seq(
        "io.circe" %% "circe-core",
        "io.circe" %% "circe-generic",
        "io.circe" %% "circe-parser"
    ).map(_ % circeVersion)
  )
