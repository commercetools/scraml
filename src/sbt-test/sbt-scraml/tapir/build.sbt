val circeVersion = "0.14.2"
val refinedVersion = "0.9.27"
val tapirVersion = "1.0.5"

lazy val root = (project in file("."))
  .settings(
    scalaVersion := "2.13.8",
    name := "scraml-tapir",
    version := "0.1",
    ramlFile := Some(file("api/tapir.raml")),
    defaultTypes := scraml.DefaultTypes(
      float = "Double",
      number = "scala.math.BigDecimal"
    ),
    librarySupport := Set(
      scraml.libs.CirceJsonSupport(),
      scraml.libs.TapirSupport("Endpoints"),
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
    ).map(_ % circeVersion),
    libraryDependencies ++= Seq(
      "com.softwaremill.sttp.tapir" %% "tapir-core",
      "com.softwaremill.sttp.tapir" %% "tapir-json-circe"
    ).map (_ % tapirVersion)
  )
