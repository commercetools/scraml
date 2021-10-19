package scraml

import cats.effect.IO
import cats.implicits.toTraverseOps
import io.vrap.rmf.raml.model.modules.Api
import io.vrap.rmf.raml.model.types._
import org.scalafmt.interfaces.Scalafmt
import java.io.File
import java.time.LocalDateTime
import scala.meta._

import _root_.io.vrap.rmf.raml.model.types.impl.ObjectTypeImpl

sealed trait GeneratedSource {
  def name: String
  def packageName: String
  def source: Tree
  def comment: Option[String]
  def companion: Option[Tree]
}

final case class TypeRef(
    scalaType: Type,
    packageName: Option[String] = None,
    defaultValue: Option[Term] = None
)

final case class GeneratedTypeSource(
    name: String,
    source: Tree,
    packageName: String,
    comment: Option[String],
    companion: Option[Tree]
) extends GeneratedSource

final case class GeneratedFile(source: GeneratedSource, file: File)
final case class GeneratedPackage(sources: List[GeneratedSource] = List.empty) {
  def withSource(source: GeneratedSource): GeneratedPackage = copy(sources = sources :+ source)
}

final case class GeneratedPackages(packages: Map[String, GeneratedPackage] = Map.empty) {
  def addSource(source: GeneratedSource): GeneratedPackages = copy(packages = {
    packages + (source.packageName -> packages
      .getOrElse(source.packageName, GeneratedPackage())
      .withSource(source))
  })
}

object DefaultModelGen extends ModelGen {
  import MetaUtil._
  import RMFUtil._

  /** This type is needed so that the `package object` can be generated. Since there is no
    * [[_root_.io.vrap.rmf.raml.model.types.ObjectType]] for the `package object`, an empty one will
    * suffice.
    */
  private class EmptyObjectType extends ObjectTypeImpl

  private def caseClassSource(implicit context: ModelGenContext): Defn.Class = {
    Defn.Class(
      mods = List(Mod.Final(), Mod.Case()),
      name = Type.Name(context.objectType.getName),
      tparams = Nil,
      ctor = Ctor.Primary(
        mods = Nil,
        name = Name.Anonymous(),
        paramss = List(context.typeParams)
      ),
      templ = Template(
        early = Nil,
        inits = initFromTypeOpt(context.scalaBaseType.map(_.scalaType)) ++ initFromTypeOpt(
          context.extendType
        ),
        self = Self(
          name = Name.Anonymous(),
          decltpe = None
        ),
        stats = Nil
      )
    )
  }

  private def initFromTypeOpt(aType: Option[Type]): List[Init] =
    aType.map(ref => List(Init(ref, Name(""), Nil))).getOrElse(Nil)

  private def companionObjectSource(typeName: String, stats: List[Stat] = Nil): Defn.Object = {
    Defn.Object(
      List(),
      Term.Name(typeName),
      Template(
        early = Nil,
        inits = Nil,
        self = Self(
          name = Name.Anonymous(),
          decltpe = None
        ),
        stats = stats
      )
    )
  }

  private def caseObjectSource(implicit context: ModelGenContext): Defn.Object = {
    Defn.Object(
      mods = List(Mod.Case()),
      Term.Name(context.objectType.getName),
      Template(
        early = Nil,
        inits = initFromTypeOpt(context.scalaBaseType.map(_.scalaType)) ++ initFromTypeOpt(
          context.extendType
        ),
        Self(Name(""), None),
        stats = Nil,
        derives = Nil
      )
    )
  }

  private def traitSource(implicit context: ModelGenContext): Defn.Trait = {
    val objectType = context.objectType
    val defs = context.typeProperties.flatMap { property =>
      context
        .scalaTypeRef(property.getType, !property.getRequired)
        .map { scalaType =>
          Decl.Def(
            Nil,
            Term.Name(property.getName),
            tparams = Nil,
            paramss = Nil,
            scalaType.scalaType
          )
        }
    }.toList

    val sealedModOpt: Option[Mod.Sealed] =
      if (context.isSealed) {
        Some(Mod.Sealed())
      } else None

    Defn.Trait(
      mods = sealedModOpt.toList,
      name = Type.Name(objectType.getName),
      tparams = Nil,
      ctor = Ctor.Primary(Nil, Name(""), Nil),
      templ = Template(
        early = Nil,
        inits = initFromTypeOpt(context.scalaBaseType.map(_.scalaType)) ++ initFromTypeOpt(
          context.extendType
        ),
        self = Self(Name(""), None),
        stats = defs,
        derives = Nil
      )
    )
  }
  import scala.collection.JavaConverters._

  private def enumTypeSource(
      stringType: StringType,
      params: ModelGenParams
  ): IO[GeneratedTypeSource] = for {
    packageName <- IO.fromOption(getPackageName(stringType))(
      new IllegalStateException("enum type should have package name")
    )
    enumInstances = stringType.getEnum.asScala.map { instance =>
      q"""
         case object ${Term.Name(instance.getValue.toString)} extends ${Init(
        Type.Name(stringType.getName),
        Name(""),
        Nil
      )}
       """
    }.toList
    enumTrait =
      q"""
         sealed trait ${Type.Name(stringType.getName)}
      """
    enumObject = companionObjectSource(stringType.getName, enumInstances)
    withLibs   = LibrarySupport.applyEnum(stringType)(enumTrait, enumObject)(params.allLibraries)
    comment =
      s"""/*
         |* Enum Type ${stringType.getName}
         |*/""".stripMargin
  } yield GeneratedTypeSource(
    stringType.getName,
    withLibs.defn,
    packageName,
    Some(comment),
    withLibs.companion
  )

  private def objectTypeSource(
      objectType: ObjectType,
      params: ModelGenParams,
      api: ApiContext
  ): IO[GeneratedTypeSource] =
    for {
      packageName <- IO.fromOption(getPackageName(objectType))(
        new IllegalStateException("object type should have package name")
      )
      apiBaseType = Option(objectType.asInstanceOf[AnyType].getType)
      extendType = getAnnotation(objectType)("scala-extends")
        .map(_.getValue.getValue.toString)
        .map(typeFromName)
      context = ModelGenContext(packageName, objectType, params, api, apiBaseType, extendType)

      discriminator = Option(objectType.getDiscriminator)
      isAbstract = getAnnotation(objectType)("abstract").exists(
        _.getValue.getValue.toString.toBoolean
      )

      source =
        discriminator match {
          case Some(_) =>
            LibrarySupport.applyTrait(
              traitSource(context),
              Some(companionObjectSource(objectType.getName))
            )(params.allLibraries, context)

          case None if isAbstract || context.getDirectSubTypes.nonEmpty =>
            LibrarySupport.applyTrait(
              traitSource(context),
              Some(companionObjectSource(objectType.getName))
            )(params.allLibraries, context)

          case None if context.isSingleton =>
            LibrarySupport.applyObject(caseObjectSource(context))(params.allLibraries, context)

          case None =>
            LibrarySupport.applyClass(
              caseClassSource(context),
              Some(companionObjectSource(objectType.getName))
            )(params.allLibraries, context)
        }
      docsUri = getAnnotation(objectType)("docs-uri").flatMap(annotation =>
        Option(annotation.getValue).map(_.getValue.toString)
      )
      dateCreated =
        if (params.generateDateCreated) s"date created: ${LocalDateTime.now()}" else ""
      comment =
        s"""/**
           |* generated by sbt-scraml, do not modify manually
           |* $dateCreated
           |* ${docsUri
          .map("see " + _)
          .orElse(Option(objectType.getDescription))
          .getOrElse(s"generated type for ${objectType.getName}")}
           |*/""".stripMargin
    } yield GeneratedTypeSource(
      objectType.getName,
      source.defn,
      packageName,
      Some(comment),
      source.companion
    )

  private def appendSource(
      file: File,
      source: GeneratedSource,
      formatConfig: Option[File],
      formatter: Scalafmt
  ): IO[GeneratedFile] = {
    val sourceString =
      s"${source.comment.map(_ + "\n").getOrElse("")}${source.source
        .toString()}\n${source.companion.map(_.toString() + "\n").getOrElse("")}\n"
    val formattedSource = formatConfig match {
      case Some(configFile) if configFile.exists() =>
        formatter.format(configFile.toPath, file.toPath, sourceString)
      case _ => sourceString
    }
    FileUtil.writeToFile(file, formattedSource, append = true).map(GeneratedFile(source, _))
  }

  private def basePackageFilePath(params: ModelGenParams) =
    s"${params.targetDir}/${params.basePackage.replace(".", File.separatorChar.toString)}"

  private def writePackages(
      generated: GeneratedPackages,
      params: ModelGenParams
  ): IO[List[GeneratedFile]] = {
    val scalafmt = Scalafmt.create(this.getClass.getClassLoader)
    val generate = generated.packages.map { case (name, generatedPackage) =>
      for {
        file <- IO {
          val packageFile = new File(s"${basePackageFilePath(params)}/$name.scala")
          packageFile.getParentFile.mkdirs()
          packageFile
        }
        packageStatement = Pkg(packageTerm(s"${params.basePackage}"), Nil).toString()
        fileWithPackage <- FileUtil.writeToFile(file, s"$packageStatement\n\n")
        files <- generatedPackage.sources
          .map(appendSource(fileWithPackage, _, params.formatConfig, scalafmt))
          .sequence
      } yield files
    }

    generate.toList.sequence.map(_.flatten)
  }

  private def generatePackages(api: ApiContext, params: ModelGenParams): IO[GeneratedPackages] =
    for {
      types <- api.getTypes.toList.map {
        case objectType: ObjectType => objectTypeSource(objectType, params, api).map(Some(_))
        case stringType: StringType if !Option(stringType.getEnum).forall(_.isEmpty) =>
          enumTypeSource(stringType, params).map(Some(_))
        case _ => IO(None)
      }.sequence
      packages = types.flatten.foldLeft(GeneratedPackages())(_ addSource _)
    } yield packages

  private def generatePackageObject(api: Api, params: ModelGenParams): IO[GeneratedFile] = for {
    packageObjectFile <- IO {
      val file = new File(new File(basePackageFilePath(params)), "package.scala")
      file.delete()
      file.getParentFile.mkdirs()
      file
    }
    packageParts = params.basePackage.split("\\.")
    objectName <- IO.fromOption(packageParts.lastOption)(
      new IllegalArgumentException("invalid package name")
    )
    packageName =
      if (packageParts.size <= 1) None else Some(packageParts.dropRight(1).mkString("."))
    packageObjectWithSource <- IO {
      val context =
        ModelGenContext(params.basePackage, new EmptyObjectType, params, ApiContext(api))
      val packageObject = LibrarySupport.applyPackageObject(
        q"""package object ${Term.Name(objectName)}"""
      )(params.allLibraries, context, api)

      val packageStatement = packageName.map(pkg => s"package $pkg\n").getOrElse("")
      (packageObject, s"$packageStatement${packageObject.toString}")
    }
    (theObject, source) = packageObjectWithSource
    _ <- FileUtil.writeToFile(packageObjectFile, source)
  } yield GeneratedFile(
    GeneratedTypeSource("package.scala", theObject, params.basePackage, None, None),
    packageObjectFile
  )

  override def generate(api: Api, params: ModelGenParams): IO[GeneratedModel] = for {
    _             <- FileUtil.deleteRecursively(new File(params.targetDir, params.basePackage))
    packageObject <- generatePackageObject(api, params)
    packages      <- generatePackages(ApiContext(api), params)
    files         <- writePackages(packages, params)
  } yield GeneratedModel(files, packageObject)
}
