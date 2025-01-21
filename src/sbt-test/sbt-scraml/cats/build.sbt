lazy val root = (project in file("."))
  .settings(
    scalaVersion := "2.13.15",
    name := "scraml-cats-test",
    version := "0.1",
    ramlFile := Some(file("api/simple.raml")),
    librarySupport := Set(
      scraml.libs.CatsEqSupport,
      scraml.libs.CatsShowSupport,
      scraml.libs.MonocleOpticsSupport
    ),
    Compile / sourceGenerators += runScraml,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.6.1",
      "dev.optics" %% "monocle-core" % "3.0.0",
    )
  )
