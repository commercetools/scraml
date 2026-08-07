lazy val root = (project in file("."))
  .settings(
    scalaVersion := "2.13.16",
    crossScalaVersions ++= Seq("3.3.4"),
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

// sbt 1 and sbt 2 lay out `target` differently, so assert the generated
// sources against `Compile / sourceManaged` rather than a hardcoded path.
TaskKey[Unit]("checkGenerated") := {
  val dir = (Compile / sourceManaged).value
  val missing = Seq(
    "scraml/datatypes.scala",
    "scraml/package.scala"
  ).map(dir / _).filterNot(_.exists)
  if (missing.nonEmpty) sys.error("missing generated sources: " + missing.mkString(", "))
}
