val circeVersion = "0.14.10"
val monocleVersion = "3.1.0"
val refinedVersion = "0.11.3"
val tapirVersion = "1.11.9"

lazy val root = (project in file("."))
  .settings(
    scalaVersion := "2.13.16",
    crossScalaVersions ++= Seq("3.3.4"),
    name := "scraml-ct-api-test",
    version := "0.1",
    ramlFile := Some(file("reference/api-specs/api/api.raml")),
    basePackageName := "de.commercetools.api",
    defaultTypes := scraml.DefaultTypes(
      array = "scala.collection.immutable.Vector",
      float = "Double",
      number = "scala.math.BigDecimal"
    ),
    librarySupport := Set(
      scraml.libs.BeanPropertiesSupport,
      scraml.libs.CatsEqSupport,
      scraml.libs.CatsShowSupport,
      scraml.libs.CirceJsonSupport(),
      scraml.libs.MonocleOpticsSupport,
      scraml.libs.TapirSupport("Endpoints"),
      scraml.libs.RefinedSupport
    ),
    beanProperties := scraml.BeanProperties(
      array = scraml.BeanProperties.UseJavaCollectionTypes,
      optional = scraml.BeanProperties.UseJavaOptionalType,
      scalaNumber = scraml.BeanProperties.UseJavaLangTypes
    ),
    Compile / sourceGenerators += runScraml,
    libraryDependencies ++= Seq(
      "eu.timepit" %% "refined",
      "eu.timepit" %% "refined-cats"
    ).map(_ % refinedVersion),
    libraryDependencies ++= Seq(
        "io.circe" %% "circe-core",
        "io.circe" %% "circe-generic",
        "io.circe" %% "circe-parser"
    ).map(_ % circeVersion),
    libraryDependencies += "io.circe" %% "circe-refined" % "0.15.1",
    libraryDependencies ++= Seq(
      "dev.optics" %% "monocle-core",
      "dev.optics" %% "monocle-macro"
    ).map(_ % monocleVersion),
    libraryDependencies += "com.softwaremill.sttp.tapir" %% "tapir-core" % tapirVersion,
    libraryDependencies += "com.softwaremill.sttp.tapir" %% "tapir-json-circe" % tapirVersion
  )

