lazy val root = (project in file("."))
  .settings(
    scalaVersion := "2.13.6",
    name := "scraml-two-specifications",
    version := "0.1",
    librarySupport := Set(
      scraml.libs.CatsEqSupport,
      scraml.libs.CatsShowSupport,
      scraml.libs.MonocleOpticsSupport
    ),
    ramlDefinitions := Seq(
      scraml.ModelDefinition(
        raml = file("api/inline-types.raml"),
        basePackage = "scraml.inline",
        // Explicitly override the default Scala types for this definition.
        defaultTypes = scraml.DefaultTypes(
          float = "Double",
          number = "scala.math.BigDecimal"
        ),
        // Use DataTypes for RAML type definitions which do not have a
        // (package) annotation.
        defaultPackageAnnotation = Some("DataTypes"),
        // Explicitly override the global librarySupport setting.
        librarySupport = Set(scraml.libs.CatsShowSupport),
        formatConfig = None,
        generateDateCreated = true
        ),
      scraml.ModelDefinition(
        raml = file("api/simple.raml"),
        basePackage = "scraml.simple",
        defaultPackageAnnotation = None,
        formatConfig = None,
        generateDateCreated = true
        ),
      ),
    Compile / sourceGenerators += runScraml,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.7.0",
      "dev.optics" %% "monocle-core" % "3.1.0"
    )
  )

