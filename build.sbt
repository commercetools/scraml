inThisBuild(List(
  organization := "com.commercetools",
  homepage := Some(url("https://github.com/commercetools/scraml")),
  licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
  developers := List(
    Developer(
      "commercetools/priceless-backend-team",
      "Priceless Team",
      "priceless-backend@commercetools.com",
      url("https://commercetools.com")
    )
  )
))

val circeVersion = "0.14.1"

lazy val root = (project in file("."))
  .enablePlugins(SbtPlugin)
  .settings(
    name := "sbt-scraml",
    libraryDependencies += "io.vrap.rmf" % "raml-model" % "0.2.0-20201204211458",
    libraryDependencies += "org.scalameta" %% "scalameta" % "4.4.20",
    libraryDependencies += "org.scalameta" %% "scalafmt-dynamic" % "3.0.0-RC6",
    libraryDependencies += "org.typelevel" %% "cats-effect" % "3.1.1",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % Test,
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
    resolvers ++= Seq(
      Resolver.jcenterRepo
    ),
    scriptedLaunchOpts := { scriptedLaunchOpts.value ++
      Seq("-Xmx2048M", "-Dplugin.version=" + version.value)
    },
    scriptedBufferLog := false
  )



