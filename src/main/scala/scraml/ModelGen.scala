package scraml

import cats.effect.IO
import io.vrap.rmf.raml.model.modules.Api
import io.vrap.rmf.raml.model.types._
import scraml.MetaUtil.{packageTerm, typeFromName}
import scraml.RMFUtil.{getAnnotation, getPackageName}
import scraml.libs.{CirceJsonSupport, SphereJsonSupport}

import java.io.File
import scala.meta.{Defn, Member, Pkg, Stat, Term, Type}

sealed trait JsonSupport {
  def jsonType: String
}

case object Sphere extends JsonSupport {
  override def jsonType: String = "org.json4s.JsonAST.JValue"
}
case object Circe extends JsonSupport {
  override def jsonType: String = "io.circe.Json"
}

sealed trait CatsSupport
case object EqSupport   extends CatsSupport
case object ShowSupport extends CatsSupport

final case class ModelGenParams(
    raml: File,
    targetDir: File,
    basePackage: String,
    jsonSupport: Option[JsonSupport],
    librarySupport: Set[LibrarySupport],
    formatConfig: Option[File]
) {
  def allLibraries: List[LibrarySupport] = jsonSupport
    .map {
      case Sphere => List(SphereJsonSupport)
      case Circe  => List(CirceJsonSupport)
    }
    .getOrElse(Nil) ++ librarySupport.toList
}

final case class GeneratedModel(files: Seq[GeneratedFile]) {
  override def toString: String = {
    files
      .map(generatedFile => s"${generatedFile.source.name} (${generatedFile.file.getPath})")
      .mkString("\n")
  }
}

final case class ApiContext(private val api: Api) {
  import scala.jdk.CollectionConverters._

  def getTypes: Iterator[AnyType] = api.getTypes.asScala.iterator

  lazy val typesByName: Map[String, AnyType] = {
    api.getTypes.asScala.map(aType => (aType.getName, aType))
  }.toMap

  lazy val scalaExtends: Map[String, AnyType] =
    api.getTypes.asScala.flatMap { aType =>
      for {
        annotation <- getAnnotation(aType)("scala-extends")
        anyType    <- typesByName.get(annotation.getValue.getValue.toString)
      } yield (aType.getName, anyType)
    }.toMap
}

final case class ModelGenContext(
    packageName: String,
    objectType: ObjectType,
    params: ModelGenParams,
    api: ApiContext,
    apiBaseType: Option[AnyType] = None,
    extendType: Option[Type] = None
) {
  import scala.jdk.CollectionConverters._

  lazy val scalaBaseType: Option[TypeRef] =
    apiBaseType.flatMap(ModelGen.scalaTypeRef(_, false, None, anyTypeName))

  lazy val anyTypeName: String = params.jsonSupport.map(_.jsonType).getOrElse("Any")

  /** map type refs from the 'asMap' annotation to real scala types */
  def mapTypeToScala: String => Type.Ref = {
    case "string" => Type.Name("String")
    case "any"    => typeFromName(anyTypeName)
  }

  def getSubTypes: Iterator[AnyType] = {
    objectType.getSubTypes.asScala.filter(_.getName != objectType.getName).iterator ++
      api.scalaExtends
        .find { case (_, typeValue) =>
          typeValue.getName == objectType.getName
        }
        .flatMap(entry => api.typesByName.get(entry._1))
  }

  def typeProperties: Iterator[Property] = RMFUtil.typeProperties(objectType)

  lazy val isSealed: Boolean = getSubTypes.forall(getPackageName(_).contains(packageName))
}

final case class DefnWithCompanion[T <: Defn with Member](defn: T, companion: Option[Defn.Object])

trait LibrarySupport {
  def modifyClass(classDef: Defn.Class, companion: Option[Defn.Object])(
      context: ModelGenContext
  ): DefnWithCompanion[Defn.Class] =
    DefnWithCompanion(classDef, None)

  def modifyObject(objectDef: Defn.Object)(
      context: ModelGenContext
  ): DefnWithCompanion[Defn.Object] =
    DefnWithCompanion(objectDef, None)

  def modifyTrait(traitDef: Defn.Trait, companion: Option[Defn.Object])(
      context: ModelGenContext
  ): DefnWithCompanion[Defn.Trait] =
    DefnWithCompanion(traitDef, companion)

  def modifyPackageObject: Pkg.Object => Pkg.Object = identity
}

object LibrarySupport {
  def applyClass(
      defn: Defn.Class,
      companion: Option[Defn.Object]
  )(libs: List[LibrarySupport], context: ModelGenContext): DefnWithCompanion[Defn.Class] =
    libs.foldLeft(DefnWithCompanion(defn, companion)) { case (acc, lib) =>
      lib.modifyClass(acc.defn, acc.companion)(context)
    }
  def applyObject(
      defn: Defn.Object
  )(libs: List[LibrarySupport], context: ModelGenContext): DefnWithCompanion[Defn.Object] =
    libs.foldLeft(DefnWithCompanion(defn, None)) { case (acc, lib) =>
      lib.modifyObject(acc.defn)(context)
    }
  def applyTrait(
      defn: Defn.Trait,
      companion: Option[Defn.Object]
  )(libs: List[LibrarySupport], context: ModelGenContext): DefnWithCompanion[Defn.Trait] =
    libs.foldLeft(DefnWithCompanion(defn, companion)) { case (acc, lib) =>
      lib.modifyTrait(acc.defn, acc.companion)(context)
    }
  def applyPackageObject(packageObject: Pkg.Object)(libs: List[LibrarySupport]): Pkg.Object =
    libs.foldLeft(packageObject: Pkg.Object) { case (acc, lib) => lib.modifyPackageObject(acc) }

  def appendObjectStats(defn: Defn.Object, stats: List[Stat]): Defn.Object =
    defn.copy(templ = defn.templ.copy(stats = defn.templ.stats ++ stats))
  def appendPkgObjectStats(packageObject: Pkg.Object, stats: List[Stat]): Pkg.Object =
    packageObject.copy(templ = packageObject.templ.copy(stats = packageObject.templ.stats ++ stats))
  def appendClassStats(defn: Defn.Class, stats: List[Stat]): Defn.Class =
    defn.copy(templ = defn.templ.copy(stats = defn.templ.stats ++ stats))
  def appendTraitStats(defn: Defn.Trait, stats: List[Stat]): Defn.Trait =
    defn.copy(templ = defn.templ.copy(stats = defn.templ.stats ++ stats))
}

trait ModelGen {
  def generate(api: Api, params: ModelGenParams): IO[GeneratedModel]
}

object ModelGen {
  import scala.jdk.CollectionConverters._

  lazy val defaultArrayTypeName = "List"

  lazy val dateTimeType: Type.Ref = typeFromName("java.time.LocalDateTime")
  lazy val dateOnlyType: Type.Ref = typeFromName("java.time.LocalDate")
  lazy val timeOnlyType: Type.Ref = typeFromName("java.time.LocalTime")

  private def numberTypeString(numberType: NumberType): String = numberType.getFormat match {
    case NumberFormat.INT64 | NumberFormat.LONG => "Long"
    case NumberFormat.FLOAT                     => "Float"
    case NumberFormat.DOUBLE                    => "Double"
    case _                                      => "Int"
  }

  private case class TypeRefDetails(
      baseType: Type,
      packageName: Option[String] = None,
      defaultValue: Option[Term] = None
  )

  def scalaTypeRef(
      apiType: AnyType,
      optional: Boolean,
      typeName: Option[String] = None,
      defaultAnyTypeName: String
  ): Option[TypeRef] = {
    lazy val mappedType = apiType match {
      case _: BooleanType     => TypeRefDetails(Type.Name("Boolean"))
      case _: IntegerType     => TypeRefDetails(Type.Name("Int"))
      case number: NumberType => TypeRefDetails(Type.Name(numberTypeString(number)))
      case _: StringType      => TypeRefDetails(Type.Name("String"))
      case array: ArrayType =>
        val arrayType = getAnnotation(array)("scala-array-type")
          .map(_.getValue.getValue.toString)
          .getOrElse(defaultArrayTypeName)
        val itemTypeOverride = getAnnotation(array)("scala-type").map(_.getValue.getValue.toString)
        // we do not need to be optional inside an collection, hence setting it to false
        TypeRefDetails(
          Type.Apply(
            typeFromName(arrayType),
            List(
              scalaTypeRef(array.getItems, false, itemTypeOverride, defaultAnyTypeName).map(
                _.scalaType
              )
            ).flatten
          ),
          None,
          Some(Term.Select(packageTerm(arrayType), Term.Name("empty")))
        )
      case objectType: ObjectType if objectType.getName != "object" =>
        TypeRefDetails(Type.Name(objectType.getName), getPackageName(objectType), None)
      case union: UnionType =>
        TypeRefDetails(
          Type.Apply(
            Type.Name("Either"),
            union.getOneOf.asScala
              .flatMap(scalaTypeRef(_, optional, None, defaultAnyTypeName))
              .map(_.scalaType)
              .toList
          )
        )
      case _: DateTimeType => TypeRefDetails(dateTimeType)
      case _: DateOnlyType => TypeRefDetails(dateOnlyType)
      case _: TimeOnlyType => TypeRefDetails(timeOnlyType)
      case _               => TypeRefDetails(typeFromName(defaultAnyTypeName))
    }

    val typeRef = typeName match {
      case Some(scalaTypeName) => TypeRefDetails(typeFromName(scalaTypeName), None, None)
      case None                => mappedType
    }

    if (optional) {
      Some(
        TypeRef(
          Type.Apply(Type.Name("Option"), List(typeRef.baseType)),
          typeRef.packageName,
          Some(Term.Name("None"))
        )
      )
    } else Some(TypeRef(typeRef.baseType, typeRef.packageName, typeRef.defaultValue))
  }
}

object ModelGenRunner {
  def run(generator: ModelGen)(params: ModelGenParams): IO[GeneratedModel] = for {
    api   <- RMFUtil.readModel(params.raml)
    model <- generator.generate(api, params)
  } yield model
}
