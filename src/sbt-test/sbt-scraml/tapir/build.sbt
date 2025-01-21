val circeVersion = "0.14.7"
val refinedVersion = "0.11.1"
val tapirVersion = "1.10.7"

lazy val root = (project in file("."))
  .settings(
    scalaVersion := "2.13.15",
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
