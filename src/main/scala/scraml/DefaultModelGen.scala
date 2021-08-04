package scraml
import cats.effect.IO
import cats.implicits.toTraverseOps
import io.vrap.rmf.raml.model.modules.Api
import io.vrap.rmf.raml.model.types._

import java.io.{File, FileOutputStream}
import java.time.LocalDateTime
import scala.jdk.CollectionConverters._
import scala.meta._

sealed trait GeneratedSource {
  def name: String
  def packageName: String
  def source: Tree
  def comment: String
  def companion: Option[Tree]
}

final case class TypeRef(scalaType: Type, packageName: Option[String] = None, defaultValue: Option[Term] = None)

final case class ObjectTypeSource(name: String,
                                  source: Tree,
                                  packageName: String,
                                  comment: String,
                                  companion: Option[Tree]) extends GeneratedSource

final case class GeneratedFile(source: GeneratedSource, file: File)
final case class GeneratedPackage(sources: List[GeneratedSource] = List.empty) {
  def withSource(source: GeneratedSource): GeneratedPackage = copy(sources = sources :+ source)
}

final case class GeneratedPackages(packages: Map[String, GeneratedPackage] = Map.empty) {
  def addSource(source: GeneratedSource): GeneratedPackages = copy(packages = {
    packages + (source.packageName -> packages.getOrElse(source.packageName, GeneratedPackage()).withSource(source))
  })
}

object DefaultModelGen extends ModelGen {
  import RMFUtil.getAnnotation
  import MetaUtil._

  lazy val defaultArrayTypeName = "List"
  lazy val defaultAnyTypeName = "Any"

  lazy val dateTimeType: Type.Ref = typeFromName("java.time.LocalDateTime")
  lazy val dateOnlyType: Type.Ref = typeFromName("java.time.LocalDate")
  lazy val timeOnlyType: Type.Ref = typeFromName("java.time.LocalTime")

  private def writeToFile(file: File, content: String, append: Boolean = false): IO[File] = IO {
    val out = new FileOutputStream(file, append)
    out.write(content.getBytes("UTF-8"))
    out.flush()
    out.close()
    file
  }

  private def numberTypeString(numberType: NumberType): String = numberType.getFormat match {
    case NumberFormat.INT64 | NumberFormat.LONG => "Long"
    case NumberFormat.FLOAT => "Float"
    case NumberFormat.DOUBLE => "Double"
    case _ => "Int"
  }

  private def getPackageName(anyType: AnyType): Option[String] =
    getAnnotation(anyType)("package").map(_.getValue.getValue.toString.toLowerCase)

  private case class TypeRefDetails(baseType: Type, packageName: Option[String] = None, defaultValue: Option[Term] = None)

  private def scalaTypeRef(apiType: AnyType, optional: Boolean, typeName: Option[String] = None): Option[TypeRef] = {
    lazy val mappedType = apiType match {
      case _: BooleanType => TypeRefDetails(Type.Name("Boolean"))
      case _: IntegerType => TypeRefDetails(Type.Name("Int"))
      case number: NumberType => TypeRefDetails(Type.Name(numberTypeString(number)))
      case _: StringType => TypeRefDetails(Type.Name("String"))
      case array: ArrayType =>
        val arrayType = getAnnotation(array)("scala-array-type").map(_.getValue.getValue.toString).getOrElse(defaultArrayTypeName)
        val itemTypeOverride = getAnnotation(array)("scala-type").map(_.getValue.getValue.toString)
        // we do not need to be optional inside an collection, hence setting it to false
        TypeRefDetails(Type.Apply(typeFromName(arrayType), List(scalaTypeRef(array.getItems, false, itemTypeOverride).map(_.scalaType)).flatten), None, Some(Term.Select(packageTerm(arrayType), Term.Name("empty"))))
      case objectType: ObjectType if objectType.getName != "object" => TypeRefDetails(Type.Name(objectType.getName), getPackageName(objectType), None)
      case union: UnionType => TypeRefDetails(Type.Apply(Type.Name("Either"), union.getOneOf.asScala.flatMap(scalaTypeRef(_, optional)).map(_.scalaType).toList))
      case _: DateTimeType => TypeRefDetails(dateTimeType)
      case _: DateOnlyType => TypeRefDetails(dateOnlyType)
      case _: TimeOnlyType => TypeRefDetails(timeOnlyType)
      case _ => TypeRefDetails(Type.Name(defaultAnyTypeName))
    }

    val typeRef = typeName match {
      case Some(scalaTypeName) => TypeRefDetails(typeFromName(scalaTypeName), None, None)
      case None => mappedType
    }

    if(optional) {
      Some(TypeRef(Type.Apply(Type.Name("Option"), List(typeRef.baseType)), typeRef.packageName, Some(Term.Name("None"))))
    } else Some(TypeRef(typeRef.baseType, typeRef.packageName, typeRef.defaultValue))
  }

  /** map type refs from the 'asMap' annotation to real scala types */
  private def mapTypeToScala: String => String = {
    case "string" => "String"
    case "any" => defaultAnyTypeName
  }

  private def scalaProperty(prop: Property): Option[Term.Param] = {
    lazy val optional = !prop.getRequired
    val scalaTypeAnnotation = Option(prop.getAnnotation("scala-type")).map(_.getValue.getValue.toString)

    val typeRef = scalaTypeRef(prop.getType, optional, scalaTypeAnnotation)

    if(Option(prop.getPattern).isDefined) {
      prop.getName match {
        case "//" =>
          typeRef.map(ref => Term.Param(Nil, Term.Name("values"), Some(Type.Apply(Type.Name("Map"), List(Type.Name("String"), ref.scalaType))), None))
        case _ =>
          typeRef.map(ref => Term.Param(Nil, Term.Name("value"), Some(Type.Apply(Type.Name("Tuple2"), List(Type.Name("String"), ref.scalaType))), None))
      }
    } else typeRef.map(ref => Term.Param(Nil, Term.Name(prop.getName), Some(ref.scalaType), ref.defaultValue))
  }

  def discriminators(aType: AnyType): List[String] = aType match {
    case objectType: ObjectType => List(objectType.getDiscriminator) ++ Option(aType.getType).map(discriminators).getOrElse(List.empty)
    case _ => List.empty
  }

  private def typeProperties(objectType: ObjectType): Iterator[Property] =
    objectType.getAllProperties.asScala.iterator.filter(property => !discriminators(objectType).contains(property.getName))

  private def caseClassSource(context: ModelGenContext): Defn.Class = {
    val classParams = getAnnotation(context.objectType)("asMap").map(_.getValue) match {
      case Some(asMap: ObjectInstance) =>
        val properties = asMap.getValue.asScala
        val mapParam: Option[List[Term.Param]] = for {
          keyType <- properties.find(_.getName == "key").map(_.getValue.getValue.toString)
          valueType <- properties.find(_.getName == "value").map(_.getValue.getValue.toString)
        } yield List(Term.Param(Nil, Term.Name("values"), Some(Type.Apply(Type.Name("Map"), List(Type.Name(mapTypeToScala(keyType)), Type.Name(mapTypeToScala(valueType))))), None))
        mapParam.toList

      case _ =>
        List(typeProperties(context.objectType).flatMap(scalaProperty).toList)
    }

    Defn.Class(
      mods = List(Mod.Final(), Mod.Case()),
      name = Type.Name(context.objectType.getName),
      tparams = Nil,
      ctor = Ctor.Primary(
        mods = Nil,
        name = Name.Anonymous(),
        paramss = classParams
      ),
      templ = Template(
        early = Nil,
        inits = initFromTypeOpt(context.baseType.map(_.scalaType)) ++ initFromTypeOpt(context.extendType),
        self = Self(
          name = Name.Anonymous(),
          decltpe = None
        ),
        stats = Nil
      )
    )
  }

  private def getSubTypes(objectType: ObjectType): Iterator[AnyType] =
    objectType.getSubTypes.asScala.filter(_.getName != objectType.getName).iterator

  private def initFromTypeOpt(aType: Option[Type]): List[Init] = aType.map(ref => List(Init(ref, Name(""), Nil))).getOrElse(Nil)

  private def companionObjectSource(objectType: ObjectType): Defn.Object = {
    val typeName = objectType.getName
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
        stats = Nil
      )
    )
  }

  private def caseObjectSource(context: ModelGenContext): Defn.Object = {
    Defn.Object(
      mods = List(Mod.Case()),
      Term.Name(context.objectType.getName),
      Template(
        early = Nil,
        inits = initFromTypeOpt(context.baseType.map(_.scalaType)) ++ initFromTypeOpt(context.extendType), Self(Name(""), None),
        stats = Nil,
        derives = Nil
      )
    )
  }

  private def traitSource(context: ModelGenContext): Defn.Trait = {
    val objectType = context.objectType
    val defs = typeProperties(objectType).flatMap { property =>
      scalaTypeRef(property.getType, !property.getRequired).map { scalaType =>
        Decl.Def(Nil, Term.Name(property.getName), tparams = Nil, paramss = Nil, scalaType.scalaType)
      }
    }.toList

    val sealedModOpt: Option[Mod.Sealed] =
      if (getSubTypes(objectType).forall(getPackageName(_).contains(context.packageName))) {
        Some(Mod.Sealed())
      } else None

    Defn.Trait(
      mods = sealedModOpt.toList,
      name = Type.Name(objectType.getName),
      tparams = Nil,
      ctor = Ctor.Primary(Nil, Name(""), Nil),
      templ = Template(
        early = Nil,
        inits =  initFromTypeOpt(context.baseType.map(_.scalaType)) ++ initFromTypeOpt(context.extendType),
        self = Self(Name(""), None),
        stats = defs,
        derives = Nil
      )
    )
  }

  private def objectTypeSource(objectType: ObjectType, params: ModelGenParams): IO[ObjectTypeSource] = {
    for {
      packageName <- IO.fromOption(getPackageName(objectType))(new IllegalStateException("object type should have package name"))
      apiBaseType = Option(objectType.asInstanceOf[AnyType].getType)
      scalaBaseTypeRef = apiBaseType.flatMap(scalaTypeRef(_, false))
      discriminator = Option(objectType.getDiscriminator)
      isAbstract = getAnnotation(objectType)("abstract").exists(_.getValue.getValue.toString.toBoolean)
      isMapType = getAnnotation(objectType)("asMap").isDefined
      extendType = getAnnotation(objectType)("scala-extends").map(_.getValue.getValue.toString).map(typeFromName)
      context = ModelGenContext(packageName, objectType, params, scalaBaseTypeRef, extendType)
      source =
        discriminator match {
          case Some(_) | None if isAbstract || getSubTypes(objectType).nonEmpty =>
            LibrarySupport.applyTrait(traitSource(context), Some(companionObjectSource(objectType)))(params.librarySupport, context)

          case None if !isMapType && typeProperties(objectType).isEmpty =>
            LibrarySupport.applyObject(caseObjectSource(context))(params.librarySupport, context)

          case None =>
            LibrarySupport.applyClass(caseClassSource(context), Some(companionObjectSource(objectType)))(params.librarySupport, context)
        }
      docsUri = getAnnotation(objectType)("docs-uri").flatMap(annotation => Option(annotation.getValue).map(_.getValue.toString))
      comment =
        s"""/**
           |* generated by sbt-scraml, do not modify manually
           |*
           |*  date created: ${LocalDateTime.now()}
           |*
           |* ${docsUri.map("see " + _).orElse(Option(objectType.getDescription)).getOrElse(s"generated type for ${objectType.getName}")}
           |*/""".stripMargin
    } yield ObjectTypeSource(objectType.getName, source.defn, packageName, comment, source.companion)
  }

  private def appendSource(file: File, source: GeneratedSource): IO[GeneratedFile] =
    writeToFile(file, s"${source.comment}\n${source.source.toString()}\n${source.companion.map(_.toString()+ "\n").getOrElse("")}\n", append = true).map(GeneratedFile(source, _))

  private def writePackages(generated: GeneratedPackages, params: ModelGenParams): IO[GeneratedModel] = {
    val generate = generated.packages.map {
      case (name, generatedPackage) =>
        for {
          file <- IO {
            val packageFile = new File(s"${params.targetDir}/${params.basePackage.replace(".", File.separatorChar.toString)}/$name.scala")
            packageFile.getParentFile.mkdirs()
            packageFile
          }
          packageStatement = Pkg(packageTerm(s"${params.basePackage}"), Nil).toString()
          withPackage <- writeToFile(file, s"$packageStatement\n\n")
          files <- generatedPackage.sources.map(appendSource(withPackage, _)).sequence
        } yield files
    }

    generate.toList.sequence.map(_.flatten).map(GeneratedModel(_))
  }

  private def generatePackages(api: Api, params: ModelGenParams): IO[GeneratedPackages] = for {
    types <- api.getTypes.asScala.toList.map {
      case objectType: ObjectType => objectTypeSource(objectType, params).map(Some(_))
      case _ => IO(None)
    }.sequence
    packages = types.flatten.foldLeft(GeneratedPackages())(_ addSource _)
  } yield packages

  override def generate(api: Api, params: ModelGenParams): IO[GeneratedModel] = for {
    _ <- FileUtil.deleteRecursively(new File(params.targetDir, params.basePackage))
    packages <- generatePackages(api, params)
    model <- writePackages(packages, params)
  } yield model
}
