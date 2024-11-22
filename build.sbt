lazy val javas = List(
  JavaSpec(JavaSpec.Distribution.Adopt, "15.0.2+7")
)

inThisBuild(
  List(
    organization := "com.commercetools",
    homepage     := Some(url("https://github.com/commercetools/scraml")),
    licenses     := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    developers := List(
      Developer(
        "commercetools/priceless-backend-team",
        "Priceless Team",
        "priceless-backend@commercetools.com",
        url("https://commercetools.com")
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
    githubWorkflowTargetTags ++= Seq("v*"),
    githubWorkflowPublishTargetBranches := Seq(RefPredicate.StartsWith(Ref.Tag("v"))),
    githubWorkflowBuild := Seq(
      WorkflowStep.Sbt(List("scalafmtCheckAll", "test", "scripted"))
    ),
    githubWorkflowPermissions := Some(sbtghactions.Permissions.WriteAll)
  )
)

val circeVersion = "0.14.10"

lazy val root = (project in file("."))
  .enablePlugins(SbtPlugin, ParadoxSitePlugin, GhpagesPlugin)
  .settings(
    name                                          := "sbt-scraml",
    libraryDependencies += "com.commercetools.rmf" % "raml-model"       % "0.2.0-20240722205528",
    libraryDependencies += "org.scalameta"        %% "scalameta"        % "4.8.15",
    libraryDependencies += "org.scalameta"        %% "scalafmt-dynamic" % "3.8.3",
    libraryDependencies += "org.typelevel"        %% "cats-effect"      % "3.5.6",
    libraryDependencies += "org.scalatest"        %% "scalatest"        % "3.2.19" % Test,
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core",
      "io.circe" %% "circe-generic",
      "io.circe" %% "circe-parser"
    ).map(_ % circeVersion % Test),
    pluginCrossBuild / sbtVersion := {
      scalaBinaryVersion.value match {
        case "2.12" => "1.5.0" // set minimum sbt version
      }
    },
    scriptedLaunchOpts := {
      scriptedLaunchOpts.value ++
        Seq("-Xmx2048M", "-Dplugin.version=" + version.value)
    },
    scriptedBufferLog               := false,
    git.remoteRepo                  := "git@github.com:commercetools/scraml.git",
    paradoxProperties += ("version" -> version.value),
    makeSite / mappings ++= Seq(
      file("LICENSE") -> "LICENSE"
    )
  )
