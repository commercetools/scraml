package scraml

import java.io.File

import sbt.internal.util.ManagedLogger

final case class ModelDefinition(
    raml: File,
    basePackage: String,
    defaultTypes: DefaultTypes = DefaultTypes(),
    librarySupport: Set[LibrarySupport] = Set.empty,
    defaultPackageAnnotation: Option[String] = None,
    formatConfig: Option[File] = None,
    generateDateCreated: Boolean = false
) {
  def toModelGenParams(
      targetDir: File,
      compilerVersion: Option[(Long, Long)],
      logger: ManagedLogger
  ): ModelGenParams =
    ModelGenParams(
      raml = raml,
      targetDir = targetDir,
      basePackage = basePackage,
      defaultTypes = defaultTypes,
      librarySupport = librarySupport,
      scalaVersion = compilerVersion,
      formatConfig = formatConfig,
      generateDateCreated = generateDateCreated,
      logger = Some(logger),
      defaultPackageAnnotation = defaultPackageAnnotation.map(_.toLowerCase())
    )

  def useDefaultLibrarySupport(default: Set[LibrarySupport]): ModelDefinition =
    if (librarySupport.isEmpty)
      copy(librarySupport = default)
    else
      this
}
