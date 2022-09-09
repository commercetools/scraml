val circeVersion = "0.14.2"
val monocleVersion = "3.1.0"
val refinedVersion = "0.9.27"
val tapirVersion = "1.1.0"

lazy val root = (project in file("."))
  .settings(
    scalaVersion := "2.13.8",
    name := "scraml-ct-api-circe-test",
    version := "0.1",
    ramlFile := Some(file("reference/api-specs/api/api.raml")),
    basePackageName := "de.commercetools.api",
    defaultTypes := scraml.DefaultTypes(
      array = "scala.collection.immutable.Vector",
      float = "Double",
      number = "scala.math.BigDecimal"
    ),
    librarySupport := Set(
      scraml.libs.CatsEqSupport,
      scraml.libs.CatsShowSupport,
      scraml.libs.CirceJsonSupport(),
      scraml.libs.MonocleOpticsSupport,
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
      "dev.optics" %% "monocle-core",
      "dev.optics" %% "monocle-macro"
    ).map(_ % monocleVersion),
    libraryDependencies += "com.softwaremill.sttp.tapir" %% "tapir-core" % tapirVersion,
    libraryDependencies += "com.softwaremill.sttp.tapir" %% "tapir-json-circe" % tapirVersion
  )

