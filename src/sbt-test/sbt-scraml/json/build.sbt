val circeVersion = "0.14.10"

lazy val root = (project in file("."))
  .settings(
    name := "scraml-json-test",
    scalaVersion := "2.13.16",
    crossScalaVersions ++= Seq("3.3.4"),
    version := "0.1",
    ramlFile := Some(file("api/json.raml")),
    basePackageName := "scraml",
    librarySupport := Set(scraml.libs.CirceJsonSupport(
      // formats = Map("localDateTime" -> "io.circe.Decoder.decodeLocalDateTime"),
      imports = Seq("io.circe.Decoder.decodeLocalDateTime") // alternative to formats to provide custom codecs via import
    )),
    defaultEnumVariant := Some("Unknown"),
    Compile / sourceGenerators += runScraml,
    libraryDependencies ++= (CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 13)) => Seq("com.commercetools" %% "sphere-json" % "0.12.5")
      case _             => Seq()
    }),
    libraryDependencies ++= Seq(
        "io.circe" %% "circe-core",
        "io.circe" %% "circe-generic",
        "io.circe" %% "circe-parser"
    ).map(_ % circeVersion)
  )
