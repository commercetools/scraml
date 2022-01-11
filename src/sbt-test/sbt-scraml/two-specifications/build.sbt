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
        defaultTypes = scraml.DefaultTypes(),
        defaultPackageAnnotation = Some("DataTypes"),
        formatConfig = None,
        generateDateCreated = true
        ),
      scraml.ModelDefinition(
        raml = file("api/simple.raml"),
        basePackage = "scraml.simple",
        defaultTypes = scraml.DefaultTypes(),
        defaultPackageAnnotation = None,
        formatConfig = None,
        generateDateCreated = true
        ),
      ),
    Compile / sourceGenerators += runScraml,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.7.0",
      "dev.optics" %% "monocle-core" % "3.0.0",
    )
  )

