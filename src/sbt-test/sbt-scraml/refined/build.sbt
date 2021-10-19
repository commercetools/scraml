val circeVersion = "0.14.1"
val refinedVersion = "0.9.27"

lazy val root = (project in file("."))
  .settings(
    scalaVersion := "2.13.6",
    name := "scraml-refined-test",
    version := "0.1",
    ramlFile := Some(file("api/refined.raml")),
    librarySupport := Set(
      scraml.libs.CatsEqSupport,
      scraml.libs.CatsShowSupport,
      scraml.libs.CirceJsonSupport(
        formats = Map(
          "localDateTime" -> "io.circe.Decoder.decodeLocalDateTime"
        )
      ),
      scraml.libs.RefinedSupport
    ),
    Compile / sourceGenerators += runScraml,
	libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.7",
    libraryDependencies ++= Seq(
      "eu.timepit" %% "refined",
      "eu.timepit" %% "refined-cats"
    ).map(_ % refinedVersion),
    libraryDependencies ++= Seq(
        "io.circe" %% "circe-core",
        "io.circe" %% "circe-generic",
        "io.circe" %% "circe-parser",
        "io.circe" %% "circe-refined"
    ).map(_ % circeVersion)
  )
