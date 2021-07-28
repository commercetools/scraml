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
}

final case class TypeRef(scalaType: Type, packageName: Option[String] = None)

final case class ObjectTypeSource(name: String,
                                  source: scala.meta.Defn.Class,
                                  packageName: String) extends GeneratedSource

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
  lazy val arrayType = "List"
  lazy val dateTimeType: Type.Select = Type.Select(Term.Select(Term.Name("java"), Term.Name("time")), Type.Name("LocalDateTime"))
  lazy val dateOnlyType: Type.Select = Type.Select(Term.Select(Term.Name("java"), Term.Name("time")), Type.Name("LocalDate"))
  lazy val timeOnlyType: Type.Select = Type.Select(Term.Select(Term.Name("java"), Term.Name("time")), Type.Name("LocalTime"))
  lazy val anyTypeName = "Any"

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

  private def scalaType(apiType: AnyType, optional: Boolean): Option[TypeRef] = {
    val (baseType, packageName) = apiType match {
      case _: BooleanType => (Type.Name("Boolean"), None)
      case _: IntegerType => (Type.Name("Int"), None)
      case number: NumberType => (Type.Name(numberTypeString(number)), None)
      case _: StringType => (Type.Name("String"), None)
      case array: ArrayType =>
        // we do not need to be optional inside an collection, hence setting it to false
        (Type.Apply(Type.Name(arrayType), List(scalaType(array.getItems, false).map(_.scalaType)).flatten), None)
      case objectType: ObjectType if objectType.getName != "object" => (Type.Name(objectType.getName), getPackageName(objectType))
      case union: UnionType => (Type.Apply(Type.Name("Either"), union.getOneOf.asScala.flatMap(scalaType(_, optional)).map(_.scalaType).toList), None)
      case _: DateTimeType => (dateTimeType, None)
      case _: DateOnlyType => (dateOnlyType, None)
      case _: TimeOnlyType => (timeOnlyType, None)
      case _ => (Type.Name(anyTypeName), None)
    }

    if(optional) {
      Some(TypeRef(Type.Apply(Type.Name("Option"), List(baseType)), packageName))
    } else Some(TypeRef(baseType, packageName))
  }

  /** map type refs from the 'asMap' annotation to real scala types */
  private def mapTypeToScala: String => String = {
    case "string" => "String"
    case "any" => anyTypeName
  }

  private def scalaProperty(prop: Property, objectType: ObjectType): Option[Term.Param] = {
    lazy val noneDefault = if(prop.getRequired) None else Some(Term.Name("None"))
    lazy val optional = !prop.getRequired

    getAnnotation(objectType)("asMap").map(_.getValue) match {
      case Some(asMap: ObjectInstance) =>
        val properties = asMap.getValue.asScala
        for {
          keyType <- properties.find(_.getName == "key").map(_.getValue.getValue.toString)
          valueType <- properties.find(_.getName == "value").map(_.getValue.getValue.toString)
        } yield Term.Param(Nil, Term.Name("values"), Some(Type.Apply(Type.Name("Map"), List(Type.Name(mapTypeToScala(keyType)), Type.Name(mapTypeToScala(valueType))))), None)

      case _ if Option(prop.getPattern).isDefined =>
        prop.getName match {
          case "//" =>
            Some(Term.Param(Nil, Term.Name("values"), scalaType(prop.getType, optional).map(theType => Type.Apply(Type.Name("Map"), List(Type.Name("String"), theType.scalaType))), noneDefault))
          case _ =>
            Some(Term.Param(Nil, Term.Name("value"), scalaType(prop.getType, optional).map(theType => Type.Apply(Type.Name("Tuple2"), List(Type.Name("String"), theType.scalaType))), noneDefault))
        }

      case _ => Some(Term.Param(Nil, Term.Name(prop.getName), scalaType(prop.getType, optional).map(_.scalaType), noneDefault))
    }
  }

  private def objectTypeSource(objectType: ObjectType): IO[ObjectTypeSource] = for {
    packageName <- IO.fromOption(getPackageName(objectType))(new IllegalStateException("object type should have package name"))
    params = List(objectType.getProperties.asScala.flatMap(scalaProperty(_, objectType)).toList)
    source = Defn.Class(
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
        inits = Nil,
        self = Self(
          name = Name.Anonymous(),
          decltpe = None
        ),
        stats = Nil
      )
    )
  } yield ObjectTypeSource(objectType.getName, source, packageName)

  private def appendSource(file: File, source: GeneratedSource): IO[GeneratedFile] =
    writeToFile(file, s"${source.source.toString()}\n", append = true).map(GeneratedFile(source, _))

  private def packageTerm(packageName: String): Term.Ref = {
    def select(parts: List[String]): Term.Ref = parts match {
      case first :: second :: Nil => Term.Select(Term.Name(first), Term.Name(second))
      case first :: remainder => Term.Select(select(remainder), Term.Name(first))
    }

    packageName.split("\\.").toList match {
      case first :: Nil => Term.Name(first)
      case moreThanOne => select(moreThanOne)
    }
  }

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
    _ <- FileUtil.deleteRecursively(params.targetDir)
    packages <- generatePackages(api)
    model <- writePackages(packages, params)
  } yield model
}
