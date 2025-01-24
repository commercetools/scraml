val circeVersion = "0.14.10"
val refinedVersion = "0.11.3"
val tapirVersion = "1.11.9"

lazy val root = (project in file("."))
  .settings(
    scalaVersion := "2.13.16",
    crossScalaVersions ++= Seq("3.3.4"),
    name := "scraml-tapir",
    version := "0.1",
    defaultTypes := scraml.DefaultTypes(
      float = "Double",
      number = "scala.math.BigDecimal"
    ),
    librarySupport := Set(
      scraml.libs.CirceJsonSupport(),
      scraml.libs.TapirSupport("Endpoints"),
      scraml.libs.RefinedSupport
    ),
    ramlDefinitions := Seq(
      scraml.ModelDefinition(
        raml = file("api/tapir.raml"),
        basePackage = "scraml.tapir.simple",
        formatConfig = None,
        generateDateCreated = true
      ),
      scraml.ModelDefinition(
        raml = file("api/tapir-complex.raml"),
        basePackage = "scraml.tapir.complex",
        formatConfig = None,
        generateDateCreated = true
      )
    ),
    Compile / sourceGenerators += runScraml,
    libraryDependencies ++= Seq(
      "eu.timepit" %% "refined",
      "eu.timepit" %% "refined-cats"
    ).map(_ % refinedVersion),
    libraryDependencies ++= Seq(
        "io.circe" %% "circe-core",
        "io.circe" %% "circe-generic",
        "io.circe" %% "circe-parser",
    ).map(_ % circeVersion),
    libraryDependencies += "io.circe" %% "circe-refined" % "0.15.1",
    libraryDependencies ++= Seq(
      "com.softwaremill.sttp.tapir" %% "tapir-core",
      "com.softwaremill.sttp.tapir" %% "tapir-json-circe"
    ).map (_ % tapirVersion)
  )
