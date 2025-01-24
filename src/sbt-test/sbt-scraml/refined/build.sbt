import scraml.FieldMatchPolicy._

val circeVersion = "0.14.10"
val refinedVersion = "0.11.3"

lazy val root = (project in file("."))
  .settings(
    scalaVersion := "2.13.16",
    crossScalaVersions ++= Seq("3.3.4"),
    name := "scraml-refined-test",
    version := "0.1",
    ramlFile := Some(file("api/refined.raml")),
    defaultTypes := scraml.DefaultTypes(long = "scala.math.BigInt"),
    // Override the default field match policy.
    ramlFieldMatchPolicy := MatchInOrder(
      // Generate exact field matching code except for these types.
      // Note that the exclusion set includes types which the next
      // policy is configured to exclude as well.  This allows them
      // to "fall through" to the last policy.
      Exact(
        excluding = Set(
          "ChildInheritsAll",
          "ChildOverridesAll",
          "DataType",
          "NoProps"
        )
      ) ::
      // Generate field matching code which ignores properties not
      // explicitly defined in the RAML *and* not matched above,
      // unless "excluding" matches.
      IgnoreExtra(
        excluding = Set(
          "DataType",
          "NoProps"
        )
      ) ::
      // If the above policies don't match, then try this one (which
      // will always match as its "excluding" Set is empty.
      KeepExtra() ::
      Nil
    ),
    librarySupport := Set(
      scraml.libs.CatsEqSupport,
      scraml.libs.CatsShowSupport,
      scraml.libs.CirceJsonSupport(imports = Seq("io.circe.Decoder.decodeLocalDateTime")),
      scraml.libs.RefinedSupport
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
  )

