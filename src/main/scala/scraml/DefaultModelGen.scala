package scraml
import cats.effect.IO
import cats.implicits.toTraverseOps
import io.vrap.rmf.raml.model.modules.Api
import io.vrap.rmf.raml.model.types._

import java.io.{File, FileOutputStream}
import scala.jdk.CollectionConverters._
import scala.meta._

sealed trait GeneratedSource {
  def name: String
  def packageName: String
  def source: Tree
  def comment: String
}

final case class TypeRef(scalaType: Type, packageName: Option[String] = None, defaultValue: Option[Term] = None)

final case class ObjectTypeSource(name: String,
                                  source: Tree,
                                  packageName: String,
                                  comment: String) extends GeneratedSource

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
  lazy val defaultArrayTypeName = "List"
  lazy val defaultAnyTypeName = "Any"

  lazy val dateTimeType: Type.Select = Type.Select(Term.Select(Term.Name("java"), Term.Name("time")), Type.Name("LocalDateTime"))
  lazy val dateOnlyType: Type.Select = Type.Select(Term.Select(Term.Name("java"), Term.Name("time")), Type.Name("LocalDate"))
  lazy val timeOnlyType: Type.Select = Type.Select(Term.Select(Term.Name("java"), Term.Name("time")), Type.Name("LocalTime"))

  private def getAnnotation(from: AnyType)(name: String): Option[Annotation] = Option(from.getAnnotation(name))

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
        // we do not need to be optional inside an collection, hence setting it to false
        TypeRefDetails(Type.Apply(typeFromName(arrayType), List(scalaTypeRef(array.getItems, false).map(_.scalaType)).flatten), None, Some(Term.Select(packageTerm(arrayType), Term.Name("empty"))))
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

  private def caseClassSource(objectType: ObjectType, baseType: Option[TypeRef] = None): Defn.Class = {
    val params = getAnnotation(objectType)("asMap").map(_.getValue) match {
      case Some(asMap: ObjectInstance) =>
        val properties = asMap.getValue.asScala
        val mapParam: Option[List[Term.Param]] = for {
          keyType <- properties.find(_.getName == "key").map(_.getValue.getValue.toString)
          valueType <- properties.find(_.getName == "value").map(_.getValue.getValue.toString)
        } yield List(Term.Param(Nil, Term.Name("values"), Some(Type.Apply(Type.Name("Map"), List(Type.Name(mapTypeToScala(keyType)), Type.Name(mapTypeToScala(valueType))))), None))
        mapParam.toList

      case _ =>
        val discriminatorField = Option(objectType.getType()).flatMap {
          case objectType: ObjectType => Option(objectType.getDiscriminator)
          case _ => None
        }
        List(objectType.getAllProperties.asScala.filter(property => !discriminatorField.contains(property.getName)).flatMap(scalaProperty).toList)
    }

    Defn.Class(
      mods = List(Mod.Final(), Mod.Case()),
      name = Type.Name(objectType.getName),
      tparams = Nil,
      ctor = Ctor.Primary(
        mods = Nil,
        name = Name.Anonymous(),
        paramss = params
      ),
      templ = Template(
        early = Nil,
        inits = baseType.map(ref => List(Init(ref.scalaType, Name(""), Nil))).getOrElse(Nil),
        self = Self(
          name = Name.Anonymous(),
          decltpe = None
        ),
        stats = Nil
      )
    )
  }

  private def caseObjectSource(name: String, baseType: Option[TypeRef] = None): Defn.Object =
    Defn.Object(List(Mod.Case()), Term.Name(name), Template(Nil, inits = baseType.map(ref => List(Init(ref.scalaType, Name(""), Nil))).getOrElse(Nil), Self(Name(""), None), Nil, Nil))

  private def traitSource(objectType: ObjectType, baseType: Option[TypeRef] = None): Defn.Trait = {
    val defs = objectType.getAllProperties.asScala.filter(property => !Option(objectType.getDiscriminator).contains(property.getName)).flatMap { property =>
      scalaTypeRef(property.getType, !property.getRequired).map { scalaType =>
        Decl.Def(Nil, Term.Name(property.getName), tparams = Nil, paramss = Nil, scalaType.scalaType)
      }
    }.toList

    Defn.Trait(
      mods = Nil,
      name = Type.Name(objectType.getName),
      tparams = Nil,
      ctor = Ctor.Primary(Nil, Name(""), Nil),
      templ = Template(
        early = Nil,
        inits = baseType.map(ref => List(Init(ref.scalaType, Name(""), Nil))).getOrElse(Nil),
        self = Self(Name(""), None),
        stats = defs,
        derives = Nil
      )
    )
  }

  private def objectTypeSource(objectType: ObjectType): IO[ObjectTypeSource] = {
    for {
      packageName <- IO.fromOption(getPackageName(objectType))(new IllegalStateException("object type should have package name"))
      source: Tree = {
        val apiBaseType = Option(objectType.asInstanceOf[AnyType].getType)
        val scalaBaseTypeRef = apiBaseType.flatMap(scalaTypeRef(_, false))
        val discriminator = Option(objectType.getDiscriminator)
        val isAbstract = getAnnotation(objectType)("abstract").exists(_.getValue.getValue.toString.toBoolean)
        val isMapType = getAnnotation(objectType)("asMap").isDefined
        val hasSubTypes = objectType.getSubTypes.asScala.exists(_.getName != objectType.getName)

        discriminator match {
          case Some(_) => traitSource(objectType, scalaBaseTypeRef)
          case None if isAbstract || hasSubTypes => traitSource(objectType, scalaBaseTypeRef)
          case None if !isMapType && objectType.getAllProperties.isEmpty => caseObjectSource(objectType.getName, scalaBaseTypeRef)
          case None => caseClassSource(objectType, scalaBaseTypeRef)
        }
      }
      docsUri = getAnnotation(objectType)("docs-uri").flatMap(annotation => Option(annotation.getValue).map(_.getValue.toString))
      comment =
        s"""/**
           |* generated by sbt-scraml, do not modify manually
           |*
           |* ${docsUri.map("see " + _).orElse(Option(objectType.getDescription)).getOrElse(s"generated type for ${objectType.getName}")}
           |*/""".stripMargin
    } yield ObjectTypeSource(objectType.getName, source, packageName, comment)
  }

  private def appendSource(file: File, source: GeneratedSource): IO[GeneratedFile] =
    writeToFile(file, s"${source.comment}\n${source.source.toString()}\n", append = true).map(GeneratedFile(source, _))

  private def termSelect(parts: List[String], default: String): Term.Ref = parts match {
    case Nil => Term.Name(default)
    case first :: Nil => Term.Name(first)
    case first :: second :: Nil => Term.Select(Term.Name(second), Term.Name(first))
    case first :: remainder => Term.Select(termSelect(remainder, default), Term.Name(first))
  }

  private[scraml] def packageTerm(packageName: String): Term.Ref =
    termSelect(packageName.split("\\.").toList.reverse, packageName)

  private def typeFromNameParts(parts: List[String]): Type.Ref =
    parts match {
      case first :: Nil => Type.Name(first)
      case first :: second :: Nil => Type.Select(Term.Name(second), Type.Name(first))
      case first :: remainder => Type.Select(termSelect(remainder, first), Type.Name(first))
    }

  private[scraml] def typeFromName(fullQualifiedName: String): Type.Ref =
    typeFromNameParts(fullQualifiedName.split("\\.").toList.reverse)

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

  private def generatePackages(api: Api): IO[GeneratedPackages] = for {
    types <- api.getTypes.asScala.toList.map {
      case objectType: ObjectType => objectTypeSource(objectType).map(Some(_))
      case _ => IO(None)
    }.sequence
    packages = types.flatten.foldLeft(GeneratedPackages())(_ addSource _)
  } yield packages

  override def generate(api: Api, params: ModelGenParams): IO[GeneratedModel] = for {
    _ <- FileUtil.deleteRecursively(new File(params.targetDir, params.basePackage))
    packages <- generatePackages(api)
    model <- writePackages(packages, params)
  } yield model
}
