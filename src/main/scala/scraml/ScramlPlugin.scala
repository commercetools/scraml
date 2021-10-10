package scraml

import cats.effect.unsafe.implicits.global
import sbt._
import sbt.Keys._

object ScramlPlugin extends AutoPlugin {
  override def trigger = allRequirements

  object autoImport {
    val ramlFile = settingKey[Option[File]]("RAML file to be used by the sbt-scraml plugin")
    val scramlTargetDir = settingKey[Option[File]](
      "target dir to use for generation, otherwise 'Compile / sourceManaged' is used"
    )
    val basePackageName = settingKey[String]("base package name to be used for generated types")
    val librarySupport  = settingKey[Set[LibrarySupport]]("additional library support")
    val formatConfig =
      settingKey[Option[File]]("config to be used for formatting, no formatting if not set")

    val runScraml = taskKey[Seq[File]]("generate Scala from RAML definitions")
  }

  import autoImport._
  override lazy val globalSettings: Seq[Setting[_]] = Seq(
    ramlFile        := None,
    basePackageName := "scraml",
    librarySupport  := Set.empty,
    formatConfig    := None,
    scramlTargetDir := None
  )

  /// Here, the runScraml task is defined but not automatically added to
  /// the `sourceGenerators`.  This way, builds control when the code is
  /// generated.  One reason for doing this is to ensure that RAML definitions
  /// can be copied as needed before attempting to generate code.
  override lazy val projectSettings: Seq[Setting[_]] = Seq(
    runScraml := {
      val targetDir: File = scramlTargetDir.value.getOrElse((Compile / sourceManaged).value)
      // adapted from https://stackoverflow.com/questions/33897874/sbt-sourcegenerators-task-execute-only-if-a-file-changes
      val cachedGeneration = FileFunction.cached(
        streams.value.cacheDirectory / "scraml"
      ) { (_: Set[File]) =>
        ramlFile.value
          .map { file =>
            val params = ModelGenParams(
              file,
              targetDir,
              basePackageName.value,
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
          .toSet
      }

      ramlFile.value match {
        case Some(apiFile) =>
          val inputFiles = FileUtil.findFiles(apiFile.getParentFile).map(_.toFile).toSet
          cachedGeneration(inputFiles).toSeq
        case None => Seq.empty
      }
    }
  )
}
