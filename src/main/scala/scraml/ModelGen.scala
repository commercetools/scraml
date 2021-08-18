package scraml

import cats.effect.IO
import io.vrap.rmf.raml.model.modules.Api
import io.vrap.rmf.raml.model.types._
import scraml.MetaUtil.{packageTerm, typeFromName}
import scraml.RMFUtil.{getAnnotation, getPackageName}
import scraml.libs.{CirceJsonSupport, SphereJsonSupport}

import java.io.File
import scala.meta.{Decl, Defn, Member, Pkg, Stat, Term, Type}

sealed trait JsonSupport {
  def jsonType: String
}

case object Sphere extends JsonSupport {
  override def jsonType: String = "org.json4s.JsonAST.JValue"
}
case object Circe extends JsonSupport {
  override def jsonType: String = "io.circe.Json"
}

final case class ModelGenParams(
    raml: File,
    targetDir: File,
    basePackage: String,
    jsonSupport: Option[JsonSupport],
    librarySupport: Set[LibrarySupport],
    formatConfig: Option[File],
    generateDateCreated: Boolean = false
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

final case class MapTypeSpec(
    keyType: Type,
    valueType: Type,
    singleValue: Boolean = false,
    optional: Boolean = false
)

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

  def getSubTypes: Iterator[AnyType] = {
    objectType.getSubTypes.asScala.filter(_.getName != objectType.getName).iterator ++
      api.scalaExtends
        .find { case (_, typeValue) =>
          typeValue.getName == objectType.getName
        }
        .flatMap(entry => api.typesByName.get(entry._1))
  }

  lazy val typeProperties: Seq[Property] = RMFUtil.typeProperties(objectType).toSeq
  lazy val isSealed: Boolean = getSubTypes.forall(getPackageName(_).contains(packageName))
  lazy val isMapType: Option[MapTypeSpec] = ModelGen.isMapType(objectType, anyTypeName)
  lazy val isSingleton: Boolean           = isMapType.isEmpty && typeProperties.isEmpty

  def typeParams: List[Term.Param] = isMapType match {
    case Some(mapTypeSpec) =>
      val mapApply = Type.Apply(
        Type.Name("Map"),
        List(mapTypeSpec.keyType, mapTypeSpec.valueType)
      )

      val finalType = if (mapTypeSpec.optional) {
        Type.Apply(
          Type.Name("Option"),
          List(mapApply)
        )
      } else mapApply

      List(
        Term.Param(
          Nil,
          Term.Name("values"),
          Some(
            finalType
          ),
          if (mapTypeSpec.optional) Some(Term.Name("None")) else None
        )
      )
    case None => typeProperties.flatMap(ModelGen.scalaProperty(this)(_)).toList
  }
}

final case class DefnWithCompanion[T <: Defn with Member](defn: T, companion: Option[Defn.Object])

trait LibrarySupport {
  case object HasAnyProperties {
    def unapply(defn: Defn.Class): Boolean =
      defn.ctor.paramss.exists(_.nonEmpty)

    def unapply(defn: Defn.Trait): Boolean =
      defn.templ.stats.exists {
        case prop: Decl.Def if prop.paramss.isEmpty => true
        case _                                      => false
      }
  }

  abstract class HasProperties(names: Seq[String]) {
    final def unapply(defn: Defn.Class): Boolean =
      names.forall(n => defn.ctor.paramss.head.exists(_.name.value == n))
  }

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

  def modifyEnum(
      enumType: StringType
  )(enumTrait: Defn.Trait, companion: Option[Defn.Object]): DefnWithCompanion[Defn.Trait] =
    DefnWithCompanion(enumTrait, companion)

  final protected def generatePropertiesCode(defn: Defn.Class)(
      f: Term.Param => List[Stat]
  ): List[Stat] =
    defn.ctor.paramss.flatMap(_.flatMap(f))

  final protected def generatePropertiesCode(
      defn: Defn.Trait
  )(f: Decl.Def => List[Stat]): List[Stat] =
    defn.templ.stats
      .collect {
        case prop: Decl.Def if prop.paramss.isEmpty => prop
      }
      .flatMap(f)
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
  def applyEnum(enumType: StringType)(enumTrait: Defn.Trait, companion: Defn.Object)(
      libs: List[LibrarySupport]
  ): DefnWithCompanion[Defn.Trait] =
    libs.foldLeft(DefnWithCompanion(enumTrait, Some(companion))) { case (acc, lib) =>
      lib.modifyEnum(enumType)(acc.defn, acc.companion)
    }

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
      // only use enum type names on top-level defined enums
      // we would need to generate types for property types otherwise
      case _: StringType if apiType.eContainer().eClass().getName == "Property" =>
        TypeRefDetails(Type.Name("String"))
      case stringEnum: StringType if Option(stringEnum.getEnum).forall(!_.isEmpty) =>
        TypeRefDetails(Type.Name(stringEnum.getName))
      case _: StringType =>
        TypeRefDetails(Type.Name("String"))
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
              .flatMap(scalaTypeRef(_, false, None, defaultAnyTypeName))
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

  def scalaProperty(context: ModelGenContext)(prop: Property): Option[Term.Param] = {
    lazy val optional = !prop.getRequired
    val scalaTypeAnnotation =
      Option(prop.getAnnotation("scala-type")).map(_.getValue.getValue.toString)

    ModelGen
      .scalaTypeRef(prop.getType, optional, scalaTypeAnnotation, context.anyTypeName)
      .map(ref => Term.Param(Nil, Term.Name(prop.getName), Some(ref.scalaType), ref.defaultValue))
  }

  /** map type refs from the 'asMap' annotation to real scala types */
  private def mapTypeToScala(anyTypeName: String): String => Type.Ref = {
    case "string" => Type.Name("String")
    case "any"    => typeFromName(anyTypeName)
  }

  def isSingleton(objectType: ObjectType, anyTypeName: String): Boolean =
    ModelGen.isMapType(objectType, anyTypeName).isEmpty &&
      RMFUtil.typeProperties(objectType).isEmpty

  def isMapType(objectType: ObjectType, anyTypeName: String): Option[MapTypeSpec] = {
    getAnnotation(objectType)("asMap").map(_.getValue) match {
      case Some(asMap: ObjectInstance) =>
        val properties = asMap.getValue.asScala
        for {
          keyType   <- properties.find(_.getName == "key").map(_.getValue.getValue.toString)
          valueType <- properties.find(_.getName == "value").map(_.getValue.getValue.toString)
        } yield MapTypeSpec(
          mapTypeToScala(anyTypeName)(keyType),
          mapTypeToScala(anyTypeName)(valueType)
        )

      case _ =>
        RMFUtil
          .typeProperties(objectType)
          .filter(prop => Option(prop.getPattern).isDefined)
          .flatMap { prop =>
            lazy val isSingle = prop.getName != "//"
            lazy val optional = !prop.getRequired
            val scalaTypeAnnotation =
              Option(prop.getAnnotation("scala-type")).map(_.getValue.getValue.toString)

            val scalaMapRequired =
              Option(prop.getAnnotation("scala-map-required"))
                .forall(_.getValue.getValue.toString.toBoolean)

            ModelGen.scalaTypeRef(prop.getType, optional, scalaTypeAnnotation, anyTypeName).map {
              valueType =>
                MapTypeSpec(Type.Name("String"), valueType.scalaType, isSingle, !scalaMapRequired)
            }
          }
          .find(_ => true)
    }
  }

}

object ModelGenRunner {
  def run(generator: ModelGen)(params: ModelGenParams): IO[GeneratedModel] = for {
    api   <- RMFUtil.readModel(params.raml)
    model <- generator.generate(api, params)
  } yield model
}
