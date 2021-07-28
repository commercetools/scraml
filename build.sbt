ThisBuild / version := "0.1.1-SNAPSHOT"
ThisBuild / organization := "com.commercetools"
ThisBuild / homepage := Some(url("https://github.com/commercetools/scraml"))

lazy val root = (project in file("."))
  .enablePlugins(SbtPlugin)
  .settings(
    name := "sbt-scraml",
    libraryDependencies += "io.vrap.rmf" % "raml-model" % "0.2.0-20201204211458",
    libraryDependencies += "org.scalameta" %% "scalameta" % "4.4.20",
    libraryDependencies += "org.typelevel" %% "cats-effect" % "3.1.1",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % Test,
    pluginCrossBuild / sbtVersion := {
      scalaBinaryVersion.value match {
        case "2.12" => "1.5.0" // set minimum sbt version
      }
    },
    resolvers ++= Seq(
      Resolver.jcenterRepo
    ),
    scriptedLaunchOpts := { scriptedLaunchOpts.value ++
      Seq("-Xmx1024M", "-Dplugin.version=" + version.value)
    },
    scriptedBufferLog := false
  )



