import sbtghactions.JavaSpec

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
    githubWorkflowJavaVersions := Seq(
      JavaSpec(JavaSpec.Distribution.Adopt, "15.0.2+7")
    ),
    githubWorkflowPublish := Seq(
      WorkflowStep.Sbt(
        List("ci-release"),
        env = Map(
          "PGP_PASSPHRASE"    -> "${{ secrets.PGP_PASSPHRASE }}",
          "PGP_SECRET"        -> "${{ secrets.PGP_SECRET }}",
          "SONATYPE_PASSWORD" -> "${{ secrets.SONATYPE_PASSWORD }}",
          "SONATYPE_USERNAME" -> "${{ secrets.SONATYPE_USERNAME }}"
        )
      )
    ),
    githubWorkflowTargetTags ++= Seq("v*"),
    githubWorkflowPublishTargetBranches := Seq(RefPredicate.StartsWith(Ref.Tag("v"))),
    githubWorkflowBuild := Seq(WorkflowStep.Sbt(List("scalafmtCheckAll", "test", "scripted")))
  )
)

val circeVersion = "0.14.2"

lazy val root = (project in file("."))
  .enablePlugins(SbtPlugin, ParadoxSitePlugin, GhpagesPlugin)
  .settings(
    name                                          := "sbt-scraml",
    libraryDependencies += "com.commercetools.rmf" % "raml-model"       % "0.2.0-20220914193841",
    libraryDependencies += "org.scalameta"        %% "scalameta"        % "4.4.20",
    libraryDependencies += "org.scalameta"        %% "scalafmt-dynamic" % "3.0.0-RC6",
    libraryDependencies += "org.typelevel"        %% "cats-effect"      % "3.1.1",
    libraryDependencies += "org.scalatest"        %% "scalatest"        % "3.2.9" % Test,
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
