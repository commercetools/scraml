lazy val javas = List(
  JavaSpec.temurin("21")
)

// The plugin is cross-built for sbt 1 (Scala 2.12) and sbt 2 (Scala 3).
lazy val scala212 = "2.12.20"
lazy val scala3   = "3.8.4"

// sbt version the sbt 1 artifact is compiled against, i.e. the minimum sbt 1
// release supported by this plugin. `scriptedSbt` below decides which sbt 1
// release the scripted tests actually run on, which is deliberately newer.
lazy val minSbt1 = "1.5.0"
lazy val sbt2    = "2.0.5"

// sbt 1 release used to run the scripted tests for the Scala 2.12 build.
lazy val scriptedSbt1 = "1.10.7"

inThisBuild(
  List(
    organization := "com.commercetools",
    // Must live in ThisBuild: sbt-github-actions derives the CI Scala matrix from
    // `ThisBuild / crossScalaVersions`, and only sees one version if it is set
    // at project scope.
    crossScalaVersions := Seq(scala3, scala212),
    scalaVersion       := scala3,
    homepage     := Some(uri("https://github.com/commercetools/scraml")),
    licenses     := List("Apache-2.0" -> uri("http://www.apache.org/licenses/LICENSE-2.0")),
    developers := List(
      Developer(
        "commercetools/priceless-backend-team",
        "Priceless Team",
        "priceless-backend@commercetools.com",
        uri("https://commercetools.com")
      )
    ),
    githubWorkflowJavaVersions := javas,
    githubWorkflowPublish := Seq(
      WorkflowStep.Sbt(
        List("ci-release"),
        env = Map(
          "PGP_PASSPHRASE"    -> "${{ secrets.PGP_PASSPHRASE }}",
          "PGP_SECRET"        -> "${{ secrets.PGP_SECRET }}",
          "SONATYPE_PASSWORD" -> "${{ secrets.SONATYPE_PASSWORD }}",
          "SONATYPE_USERNAME" -> "${{ secrets.SONATYPE_USERNAME }}"
        )
      ),
      WorkflowStep.Sbt(
        List("makeSite")
      ),
      WorkflowStep.Use(
        UseRef.Public("peaceiris", "actions-gh-pages", "v4"),
        params = Map(
          "github_token" -> "${{ secrets.GITHUB_TOKEN }}",
          "publish_dir"  -> "./target/site"
        )
      )
    ),
    // sbt 2 puts build output under `target/out/jvm/scala-<version>/<project>`, but
    // the generated tar step hardcodes a single Scala version, so it cannot work
    // across the two-version matrix. The publish job does a full build anyway.
    githubWorkflowArtifactUpload := false,
    githubWorkflowTargetTags ++= Seq("v*"),
    githubWorkflowPublishTargetBranches := Seq(RefPredicate.StartsWith(Ref.Tag("v"))),
    githubWorkflowBuild := Seq(
      WorkflowStep.Sbt(List("scalafmtCheckAll", "test", "scripted"))
    ),
    githubWorkflowPermissions := Some(sbtghactions.Permissions.WriteAll)
  )
)

val circeVersion = "0.14.10"

lazy val makeSite = taskKey[Unit]("build the paradox documentation site")

lazy val root = (project in file("."))
  .enablePlugins(SbtPlugin, ParadoxPlugin)
  .settings(
    name := "sbt-scraml",
    libraryDependencies += "com.commercetools.rmf" % "raml-model"       % "0.2.0-20240722205528",
    // 4.12.7's quasiquote macro crashes on Scala 3.8.x (AssertionError:
    // NoDenotation.owner); 4.17.x is the first release that expands cleanly.
    libraryDependencies += "org.scalameta"        %% "scalameta"        % "4.13.6",
    // scalafmt-dynamic pulls coursier_2.13, which drags in scala-xml_2.13 and
    // collides with the scala-xml_3 that sbt 2 itself depends on. sbt's copy wins
    // at runtime regardless (it sits in the parent classloader), so drop the 2.13
    // artifact rather than shipping duplicate `scala.xml` classes. The rule names
    // the _2.13 artifact explicitly, so it is a no-op on the Scala 2.12 build.
    libraryDependencies += ("org.scalameta" %% "scalafmt-dynamic" % "3.11.5")
      .exclude("org.scala-lang.modules", "scala-xml_2.13"),
    libraryDependencies += "org.typelevel"        %% "cats-effect"      % "3.5.7",
    libraryDependencies += "org.scalatest"        %% "scalatest"        % "3.2.19" % Test,
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core",
      "io.circe" %% "circe-generic",
      "io.circe" %% "circe-parser"
    ).map(_ % circeVersion % Test),
    pluginCrossBuild / sbtVersion := {
      scalaBinaryVersion.value match {
        case "2.12" => minSbt1
        case _      => sbt2
      }
    },
    scriptedSbt := {
      scalaBinaryVersion.value match {
        case "2.12" => scriptedSbt1
        case _      => sbt2
      }
    },
    scriptedLaunchOpts := {
      scriptedLaunchOpts.value ++
        Seq("-Xmx4G", "-Dplugin.version=" + version.value)
    },
    scriptedBufferLog := false,
    // sbt 2 flipped `exportJars` to true, which puts this project's resources on the
    // test classpath inside a jar. The specs resolve RAML fixtures via
    // `getResource(..).toURI` and hand them to the parser as real files, which a
    // `jar:` URI cannot satisfy ("URI is not hierarchical").
    exportJars := false,
    paradoxProperties += ("version" -> version.value),
    // Replaces sbt-site's `makeSite`, which has no sbt 2 release. The output
    // directory is kept at `target/site` so the gh-pages publish step in CI is
    // unaffected by sbt 2's versioned `target` layout.
    makeSite := {
      val paradoxOut = (Compile / paradox).value
      val dest       = (ThisBuild / baseDirectory).value / "target" / "site"

      IO.delete(dest)
      IO.copyDirectory(paradoxOut, dest)
      IO.copyFile((ThisBuild / baseDirectory).value / "LICENSE", dest / "LICENSE")

      streams.value.log.info(s"site generated in $dest")
    }
  )
