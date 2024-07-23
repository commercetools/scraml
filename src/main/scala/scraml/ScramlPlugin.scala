package scraml

import cats.effect.unsafe.implicits.global
import sbt._
import sbt.Keys._
import sbt.internal.util.ManagedLogger

object ScramlPlugin extends AutoPlugin {
  override def trigger = allRequirements

  object autoImport {
    val ramlFile = settingKey[Option[File]]("RAML file to be used by the sbt-scraml plugin")
    val ramlDefinitions = settingKey[Seq[ModelDefinition]](
      "RAML definitions to be used for by the sbt-scraml plugin"
    )
    val ramlFieldMatchPolicy = settingKey[FieldMatchPolicy](
      "RAML policy for additional properties support (default: keep extra)"
    )
    val scramlTargetDir = settingKey[Option[File]](
      "target dir to use for generation, otherwise 'Compile / sourceManaged' is used"
    )
    val basePackageName = settingKey[String]("base package name to be used for generated types")
    val defaultTypes    = settingKey[DefaultTypes]("Scala types to use for RAML built-in types")
    val librarySupport  = settingKey[Set[LibrarySupport]]("additional library support")
    val formatConfig =
      settingKey[Option[File]]("config to be used for formatting, no formatting if not set")

    val runScraml = taskKey[Seq[File]]("generate Scala from RAML definitions")
    val defaultEnumVariant = settingKey[Option[String]](
      "value to used as default enum variant type name (fallback during encode/decode)"
    )
  }

  import autoImport._
  override lazy val globalSettings: Seq[Setting[_]] = Seq(
    ramlFile             := None,
    ramlDefinitions      := Seq.empty,
    ramlFieldMatchPolicy := FieldMatchPolicy.Default(),
    basePackageName      := "scraml",
    defaultTypes         := DefaultTypes(),
    librarySupport       := Set.empty,
    formatConfig         := None,
    scramlTargetDir      := None,
    defaultEnumVariant   := None
  )

  /// Here, the runScraml task is defined but not automatically added to
  /// the `sourceGenerators`.  This way, builds control when the code is
  /// generated.  One reason for doing this is to ensure that RAML definitions
  /// can be copied as needed before attempting to generate code.
  override lazy val projectSettings: Seq[Setting[_]] = Seq(
    runScraml := {
      val targetDir: File = scramlTargetDir.value.getOrElse((Compile / sourceManaged).value)
      val s               = streams.value
      val definitions = detectDuplicateBasePackages(s.log) {
        ramlFile.value
          .map { raml =>
            ModelDefinition(
              raml,
              basePackageName.value,
              Option(ramlFieldMatchPolicy.value),
              defaultTypes.value,
              librarySupport.value,
              None,
              formatConfig.value,
              defaultEnumVariant = defaultEnumVariant.value
            ) :: Nil
          }
          .getOrElse(ramlDefinitions.value)
      }

      // adapted from https://stackoverflow.com/questions/33897874/sbt-sourcegenerators-task-execute-only-if-a-file-changes
      val cachedGeneration = FileFunction.cached(
        streams.value.cacheDirectory / "scraml"
      ) { (_: Set[File]) =>
        definitions.flatMap { definition =>
          val params = definition.toModelGenParams(
            targetDir,
            ramlFieldMatchPolicy.value,
            defaultTypes.value,
            librarySupport.value,
            CrossVersion.partialVersion(scalaVersion.value),
            s.log
          )

          val generated = ModelGenRunner.run(DefaultModelGen)(params).unsafeRunSync()

          s.log.info(s"generated API model for ${definition.raml} in $targetDir")
          s.log.debug(generated.toString)
          generated.files.map(_.file)
        }.toSet
      }

      val allRamls = definitions
        .flatMap(definition => FileUtil.findFiles(definition.raml.getParentFile))
        .map(_.toFile)
        .toSet

      cachedGeneration(allRamls).toSeq
    }
  )

  private def detectDuplicateBasePackages(logger: ManagedLogger)(
      definitions: Seq[ModelDefinition]
  ): Seq[ModelDefinition] = {
    val packages = definitions.map(_.basePackage).sorted
    val duplicates = packages
      .combinations(2)
      .toList
      .filter {
        case Seq(a, b) if a == b =>
          logger.error(s"duplicate base packages detected: '$a' and '$b''")
          true
        case _ =>
          false
      }

    duplicates.headOption.foreach { case Seq(a, b) =>
      throw new IllegalArgumentException(s"duplicate base packages detected: '$a' and '$b''")
    }

    definitions
  }
}
