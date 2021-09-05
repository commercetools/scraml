val circeVersion = "0.14.1"

lazy val root = (project in file("."))
  .settings(
    name := "scraml-json-test",
    version := "0.1",
    ramlFile := Some(file("api/json.raml")),
    basePackageName := "scraml",
    librarySupport := Set(scraml.libs.CirceJsonSupport),
    Compile / sourceGenerators += runScraml,
    libraryDependencies += "com.commercetools" %% "sphere-json" % "0.12.5",
    libraryDependencies ++= Seq(
        "io.circe" %% "circe-core",
        "io.circe" %% "circe-generic",
        "io.circe" %% "circe-parser"
    ).map(_ % circeVersion)
  )
