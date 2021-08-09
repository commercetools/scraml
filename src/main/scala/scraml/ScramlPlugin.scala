package scraml

import cats.effect.unsafe.implicits.global
import sbt._
import sbt.Keys._

object ScramlPlugin extends AutoPlugin {
  override def trigger = allRequirements

  object autoImport {
    val ramlFile        = settingKey[Option[File]]("RAML file to be used by the sbt-scraml plugin")
    val basePackageName = settingKey[String]("base package name to be used for generated types")
    val jsonSupport = settingKey[Option[JsonSupport]](
      "if set, JSON support will be generated for the selected library"
    )
    val librarySupport = settingKey[Set[LibrarySupport]]("additional library support")
    val formatConfig =
      settingKey[Option[File]]("config to be used for formatting, no formatting if not set")
  }

  import autoImport._
  override lazy val globalSettings: Seq[Setting[_]] = Seq(
    ramlFile        := None,
    basePackageName := "scraml",
    jsonSupport     := None,
    librarySupport  := Set.empty,
    formatConfig    := None
  )

  override lazy val projectSettings: Seq[Setting[_]] = Seq(
    Compile / sourceGenerators += Def.task {
      val targetDir: File = (Compile / sourceManaged).value

      ramlFile.value
        .map { file =>
          val params = ModelGenParams(
            file,
            targetDir,
            basePackageName.value,
            jsonSupport.value,
            librarySupport.value,
            formatConfig.value
          )

          val generated = ModelGenRunner.run(DefaultModelGen)(params).unsafeRunSync()
          val s         = streams.value
          s.log.info(s"generated API model for $file in $targetDir")
          s.log.debug(generated.toString)
          generated.files.map(_.file)
        }
        .getOrElse(List.empty)
    }.taskValue
  )
}
