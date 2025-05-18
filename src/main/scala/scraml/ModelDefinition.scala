package scraml

import java.io.File

import sbt.internal.util.ManagedLogger

final case class ModelDefinition(
    raml: File,
    basePackage: String,
    fieldMatchPolicy: Option[FieldMatchPolicy] = None,
    defaultTypes: DefaultTypes = DefaultTypes(),
    librarySupport: Set[LibrarySupport] = Set.empty,
    defaultPackageAnnotation: Option[String] = None,
    formatConfig: Option[File] = None,
    generateDateCreated: Boolean = false,
    defaultEnumVariant: Option[String] = None,
    beanProperties: BeanProperties = BeanProperties()
) {
  def toModelGenParams(
      targetDir: File,
      fallbackFieldMatchPolicy: FieldMatchPolicy,
      fallbackDefaultTypes: DefaultTypes,
      fallbackLibrarySupport: Set[LibrarySupport],
      compilerVersion: Option[(Long, Long)],
      logger: ManagedLogger
  ): ModelGenParams = {
    val librariesToEnable =
      if (librarySupport.isEmpty)
        fallbackLibrarySupport
      else
        librarySupport

    val typesToUse =
      if (defaultTypes == DefaultTypes())
        fallbackDefaultTypes
      else
        defaultTypes

    ModelGenParams(
      raml = raml,
      targetDir = targetDir,
      basePackage = basePackage,
      fieldMatchPolicy = fieldMatchPolicy.getOrElse(fallbackFieldMatchPolicy),
      defaultTypes = typesToUse,
      librarySupport = librariesToEnable,
      scalaVersion = compilerVersion,
      formatConfig = formatConfig,
      generateDateCreated = generateDateCreated,
      logger = Some(logger),
      defaultPackageAnnotation = defaultPackageAnnotation.map(_.toLowerCase()),
      generateDefaultEnumVariant = defaultEnumVariant,
      beanProperties = beanProperties
    )
  }
}
