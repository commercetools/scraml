package scraml

import cats.effect.IO
import io.vrap.rmf.raml.model.modules.Api
import io.vrap.rmf.raml.model.types._
import scraml.MetaUtil.{packageTerm, typeFromName}
import scraml.RMFUtil.{getAnnotation, getPackageName, isEnumType}
import java.io.File
import scala.collection.immutable.TreeSet
import scala.meta._
import scala.reflect.ClassTag
import scala.util.Try

import sbt.internal.util.ManagedLogger

trait JsonSupport { self: LibrarySupport =>
  // Json support should usually be one of the first to be applied
  // but should still be customizable if you know what you are doing
  override def order: Double = 0.2

  def jsonType: String
}

final case class DefaultTypes(
    array: String = "scala.collection.immutable.List",
    boolean: String = "Boolean",
    date: String = "java.time.LocalDate",
    dateTime: String = "java.time.LocalDateTime",
    double: String = "Double",
    float: String = "Float",
    integer: String = "Int",
    long: String = "Long",
    map: String = "scala.collection.immutable.Map",
    number: String = "Float",
    string: String = "String",
    time: String = "java.time.LocalTime"
)

final case class ModelGenParams(
    raml: File,
    targetDir: File,
    basePackage: String,
    fieldMatchPolicy: FieldMatchPolicy,
    defaultTypes: DefaultTypes,
    librarySupport: Set[LibrarySupport],
    scalaVersion: Option[(Long, Long)] = Some((2, 13)),
    formatConfig: Option[File] = None,
    generateDateCreated: Boolean = false,
    logger: Option[ManagedLogger] = None,
    defaultPackageAnnotation: Option[String] = None,
    // if set the value of this option will be used as additional enum variant type name which will be used as fallback during encode/decode
    // this allows for forward compatible enums which can always be parsed as long as they are represented as string
    // note: enabling this retroactively might need changes on the usage side of the generation code
    generateDefaultEnumVariant: Option[String] = None,
    beanProperties: BeanProperties = BeanProperties()
) {
  lazy val allLibraries: List[LibrarySupport] = librarySupport.toList.sorted
  def dialect: Dialect = scalaVersion match {
    case Some((2, 12)) => dialects.Scala212
    case Some((2, 13)) => dialects.Scala213
    case Some((3, _))  => dialects.Scala3
    case _             => dialects.Scala213
  }
}

final case class GeneratedModel(sourceFiles: Seq[GeneratedFile], packageObject: GeneratedFile) {
  def files: Seq[GeneratedFile] = sourceFiles ++ List(packageObject)

  override def toString: String = {
    files
      .map(generatedFile => s"${generatedFile.source.name} (${generatedFile.file.getPath})")
      .mkString("\n")
  }
}

final case class PropertyOptionality(originally: Boolean, overridden: Boolean) {
  val isOptional = (originally && !overridden) || (!originally && overridden)
  val isRequired = !(originally || overridden)
}

object PropertyOptionality {
  def apply(aType: ObjectType, name: Name): PropertyOptionality = {
    val declarations = allDeclarations(aType, name.value).filterNot { case (_, property) =>
      property.getName == "//"
    }

    val optional      = declarations.headOption.exists(_._2.getRequired == false)
    val wasOverridden = declarations.drop(1).exists(_._2.getRequired ^ !optional)

    PropertyOptionality(originally = optional, overridden = wasOverridden)
  }

  private def allDeclarations(aType: ObjectType, name: String) = {
    RMFUtil.findAllDeclarations(aType, name) :::
      RMFUtil.findAllDeclarations(aType, MetaUtil.removeOverrideSuffix(name))
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
    mapType: Type.Ref,
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
  import RMFUtil.anyTypeOrdering

  lazy val scalaBaseType: Option[TypeRef] =
    apiBaseType.flatMap(scalaTypeRef(_, false))

  lazy val anyTypeName: String = params.allLibraries
    .collectFirst { case jsonSupport: JsonSupport =>
      jsonSupport.jsonType
    }
    .getOrElse("Any")

  lazy val scalaExtends: TreeSet[AnyType] = api.scalaExtends
    .foldLeft(TreeSet.empty[AnyType]) { case (acc, (key, typeValue)) =>
      if (typeValue.getName == objectType.getName)
        api.typesByName.get(key).map(acc + _).getOrElse(acc)
      else acc
    }

  lazy val getDirectSubTypes: Set[AnyType] =
    RMFUtil.subTypes(objectType) ++ scalaExtends

  lazy val leafTypes: TreeSet[AnyType] =
    RMFUtil.leafTypes(objectType)

  lazy val typeProperties: Seq[Property] = params.fieldMatchPolicy.namedProperties(objectType)
  lazy val isSealed: Boolean = getDirectSubTypes.forall(getPackageName(_).contains(packageName))
  lazy val isMapType: Option[MapTypeSpec] = ModelGen.isMapType(objectType, anyTypeName)(this)
  lazy val isSingleton: Boolean =
    isMapType.isEmpty && params.fieldMatchPolicy.isSingleton(objectType)(this)

  def findNamedProperty(name: String): Option[Property] =
    params.fieldMatchPolicy
      .namedProperties(objectType)
      .find(prop => name == prop.getName)

  def isLibraryEnabled[A <: LibrarySupport: ClassTag](): Boolean =
    params.allLibraries.exists(ls =>
      implicitly[ClassTag[A]].runtimeClass.isAssignableFrom(ls.getClass)
    )

  def scalaTypeRef(
      apiType: AnyType,
      optional: Boolean
  ): Option[TypeRef] =
    ModelGen.scalaTypeRef(apiType, optional, None, anyTypeName)(this)

  def scalaTypeRef(
      apiType: AnyType,
      optional: Boolean,
      typeName: Option[String] = None,
      defaultAnyTypeName: String
  ): Option[TypeRef] =
    ModelGen.scalaTypeRef(apiType, optional, typeName, defaultAnyTypeName)(this)

  def scalaTypeRefFromProperty(property: Property): Option[TypeRef] =
    scalaTypeRefFromProperty(property, !property.getRequired)

  def scalaTypeRefFromProperty(
      property: Property,
      optional: Boolean
  ): Option[TypeRef] = {
    val scalaTypeAnnotation = Option(
      property.getAnnotation("scala-type")
    ).map(_.getValue.getValue.toString)

    scalaTypeRef(
      property.getType,
      optional,
      typeName = scalaTypeAnnotation,
      defaultAnyTypeName = anyTypeName
    )
  }

  def typeParams(properties: Seq[Property]): List[Term.Param] = isMapType match {
    case Some(mapTypeSpec) =>
      val mapApply = Type.Apply(
        mapTypeSpec.mapType,
        Type.ArgClause(List(mapTypeSpec.keyType, mapTypeSpec.valueType))
      )

      val finalType = if (mapTypeSpec.optional) {
        Type.Apply(
          Type.Name("Option"),
          Type.ArgClause(List(mapApply))
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
    case None =>
      properties.flatMap(ModelGen.scalaProperty(_)(this.anyTypeName)(this)).toList
  }

  def warn(message: => String): Unit =
    params.logger.foreach {
      _.warn(message)
    }
}

final case class DefnWithCompanion[T <: Defn with Member](defn: T, companion: Option[Defn.Object])

trait LibrarySupport {
  // number between 0 and 1 to define the order of library support applications
  def order: Double = 0.5

  object HasAnyProperties {
    def unapply(defn: Defn.Class): Boolean =
      defn.ctor.paramClauses.exists(_.nonEmpty)

    def unapply(defn: Defn.Trait): Boolean =
      defn.templ.body.stats.exists {
        // a declaration without parameters is considered a property
        case prop: Decl.Def if prop.paramClauses.isEmpty => true
        case _                                           => false
      }
  }

  trait HasFacets {
    final def hasAnyFacets(anyType: AnyType): Boolean =
      anyType match {
        case at: ArrayType =>
          hasFacets(at)
        case it: IntegerType =>
          hasFacets(it)
        case nt: NumberType =>
          hasFacets(nt)
        case st: StringType =>
          hasFacets(st)
        case _ =>
          false
      }

    final def hasFacets(at: ArrayType): Boolean =
      (at.getMaxItems ne null) ||
        (at.getMinItems ne null) ||
        (at.getUniqueItems ne null) ||
        hasItemFacets(at)

    final def hasFacets(it: IntegerType): Boolean =
      (it.getMaximum ne null) ||
        (it.getMinimum ne null)

    final def hasFacets(nt: NumberType): Boolean =
      (nt.getMaximum ne null) ||
        (nt.getMinimum ne null)

    final def hasFacets(st: StringType): Boolean =
      (st.getPattern ne null) ||
        (st.getMaxLength ne null) ||
        (st.getMinLength ne null)

    final def hasItemFacets(at: ArrayType): Boolean =
      Option(at.getItems).exists {
        case nt: NumberType if hasFacets(nt) => true
        case st: StringType if hasFacets(st) => true
        case _                               => false
      }
  }

  abstract class HasProperties(names: Seq[String]) {
    final def unapply(defn: Defn.Class)(implicit
        context: ModelGenContext
    ): Boolean =
      names.forall { aName =>
        // the name has to be checked explicitly so that regular expression
        // properties do not accidentally match
        RMFUtil
          .findAllDeclarations(context.objectType, aName)
          .map(_._2.getName)
          .contains(aName)
      }
  }

  object NamedProperty {
    def unapply(param: Term.Param)(implicit
        context: ModelGenContext
    ): Option[(Term.Param, Property, String)] =
      tryToFind(param, param.name)

    def unapply(declaration: Decl.Def)(implicit
        context: ModelGenContext
    ): Option[(Decl.Def, Property, String)] =
      tryToFind(declaration, declaration.name)

    private def tryToFind[A](a: A, name: Name)(implicit
        context: ModelGenContext
    ) = {
      val unadornedName = propertyNameFrom(name.value)

      context.findNamedProperty(unadornedName).map { prop =>
        (a, prop, unadornedName)
      }
    }
  }

  def modifyAdditionalProperties(classDef: Defn.Class, companion: Option[Defn.Object])(implicit
      context: ModelGenContext
  ): DefnWithCompanion[Defn.Class] =
    DefnWithCompanion(classDef, companion)

  def modifyClass(classDef: Defn.Class, companion: Option[Defn.Object])(implicit
      context: ModelGenContext
  ): DefnWithCompanion[Defn.Class] =
    DefnWithCompanion(classDef, companion)

  def modifyObject(objectDef: Defn.Object)(implicit
      context: ModelGenContext
  ): DefnWithCompanion[Defn.Object] =
    DefnWithCompanion(objectDef, None)

  def modifyTrait(traitDef: Defn.Trait, companion: Option[Defn.Object])(implicit
      context: ModelGenContext
  ): DefnWithCompanion[Defn.Trait] =
    DefnWithCompanion(traitDef, companion)

  def modifyPackageObject(libs: List[LibrarySupport], api: Api)(implicit
      context: ModelGenContext
  ): Pkg.Object => Pkg.Object = identity

  def modifyEnum(
      enumType: StringType,
      params: ModelGenParams
  )(enumTrait: Defn.Trait, companion: Option[Defn.Object]): DefnWithCompanion[Defn.Trait] =
    DefnWithCompanion(enumTrait, companion)

  final protected def generatePropertiesCode[A](defn: Defn.Class)(
      f: Term.Param => List[A]
  ): List[A] =
    defn.ctor.paramClauses.headOption.fold(List.empty[A]) {
      _.flatMap(f)
    }

  final protected def generatePropertiesCode[A](defn: Defn.Trait)(
      f: Decl.Def => List[A]
  ): List[A] =
    defn.templ.body.stats
      .collect {
        case prop: Decl.Def if prop.paramClauses.isEmpty => prop
      }
      .flatMap(f)

  protected def propertyNameFrom(name: Name): String =
    propertyNameFrom(name.value)

  protected def propertyNameFrom(name: String): String =
    MetaUtil.removeOverrideSuffix(name)
}

object LibrarySupport {
  implicit val ordering: Ordering[LibrarySupport] =
    (x: LibrarySupport, y: LibrarySupport) => x.order.compare(y.order)

  /** Applies all LibrarySupport instances to the definition of each generated class's
    * ''AdditionalProperties'' type.
    */
  def applyAdditionalProperties(
      defn: Defn.Class,
      companion: Option[Defn.Object]
  )(libs: List[LibrarySupport], context: ModelGenContext): DefnWithCompanion[Defn.Class] =
    libs.foldLeft(DefnWithCompanion(defn, companion)) { case (acc, lib) =>
      lib.modifyAdditionalProperties(acc.defn, acc.companion)(context)
    }

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

  def applyPackageObject(
      packageObject: Pkg.Object
  )(libs: List[LibrarySupport], context: ModelGenContext, api: Api): Pkg.Object =
    libs.foldLeft(packageObject: Pkg.Object) { case (acc, lib) =>
      lib.modifyPackageObject(libs, api)(context)(acc)
    }

  def applyEnum(enumType: StringType, params: ModelGenParams)(
      enumTrait: Defn.Trait,
      companion: Defn.Object
  )(
      libs: List[LibrarySupport]
  ): DefnWithCompanion[Defn.Trait] =
    libs.foldLeft(DefnWithCompanion(enumTrait, Some(companion))) { case (acc, lib) =>
      lib.modifyEnum(enumType, params)(acc.defn, acc.companion)
    }

  def appendClassStats(defn: Defn.Class, stats: List[Stat]): Defn.Class =
    // The scala.meta.Dialect is constant for any given plugin execution and
    // therefore is not needed for AST manipulation.
    defn.copy(templ = defn.templ.copy(stats = defn.templ.body.stats ++ stats))

  def appendObjectStats(defn: Defn.Object, stats: List[Stat]): Defn.Object =
    // The scala.meta.Dialect is constant for any given plugin execution and
    // therefore is not needed for AST manipulation.
    defn.copy(templ = defn.templ.copy(stats = defn.templ.body.stats ++ stats))

  def appendPkgObjectStats(packageObject: Pkg.Object, stats: List[Stat]): Pkg.Object =
    // The scala.meta.Dialect is constant for any given plugin execution and
    // therefore is not needed for AST manipulation.
    packageObject.copy(templ =
      packageObject.templ.copy(stats = packageObject.templ.body.stats ++ stats)
    )

  def appendTraitStats(defn: Defn.Trait, stats: List[Stat]): Defn.Trait =
    // The scala.meta.Dialect is constant for any given plugin execution and
    // therefore is not needed for AST manipulation.
    defn.copy(templ = defn.templ.copy(stats = defn.templ.body.stats ++ stats))
}

trait ModelGen {
  def generate(api: Api, params: ModelGenParams): IO[GeneratedModel]
}

object ModelGen {
  import scala.jdk.CollectionConverters._

  private def numberTypeString(numberFormat: NumberFormat)(implicit
      context: ModelGenContext
  ): String = numberFormat match {
    case NumberFormat.INT64 | NumberFormat.LONG => context.params.defaultTypes.long
    case NumberFormat.FLOAT                     => context.params.defaultTypes.float
    case NumberFormat.DOUBLE                    => context.params.defaultTypes.double
    case _                                      => context.params.defaultTypes.integer
  }

  private case class TypeRefDetails(
      baseType: Type,
      packageName: Option[String] = None,
      defaultValue: Option[Term] = None,
      isCollection: Boolean = false
  ) {
    def addDefaultEnum(property: StringType): TypeRefDetails = {
      Option(property.getDefault).fold(this) { instance =>
        val enumType     = Term.Name(property.getName)
        val enumInstance = Term.Name(instance.getValue.toString)

        copy(defaultValue = Option(q"$enumType.$enumInstance"))
      }
    }

    def addDefaultValue(property: AnyType): TypeRefDetails =
      Option(property.getDefault).fold(this) { instance =>
        property match {
          case _: StringType =>
            val raw = instance.getValue.toString

            copy(defaultValue = Option(Lit.String(raw)))

          case int: IntegerType
              if NumberFormat.INT64 == int.getFormat || NumberFormat.LONG == int.getFormat =>
            val parsed = Try(instance.getValue.toString.toLong).map(Lit.Long(_)).toOption

            copy(defaultValue = parsed)

          case number: NumberType
              if NumberFormat.INT64 == number.getFormat || NumberFormat.LONG == number.getFormat =>
            val parsed = Try(instance.getValue.toString.toLong).map(Lit.Long(_)).toOption

            copy(defaultValue = parsed)

          case double: NumberType if NumberFormat.DOUBLE == double.getFormat =>
            val parsed = Try(instance.getValue.toString.toDouble).map(Lit.Double(_)).toOption

            copy(defaultValue = parsed)

          case _ =>
            copy(defaultValue = instance.getValue.toString.parse[Term].toOption)
        }
      }
  }

  def scalaTypeRef(
      apiType: AnyType,
      optional: Boolean,
      typeName: Option[String] = None,
      defaultAnyTypeName: String
  )(implicit context: ModelGenContext): Option[TypeRef] = {
    def overrideTypeOr(anyType: AnyType, default: => String) = {
      val typeOverride = getAnnotation(anyType)("scala-type")
        .map(_.getValue.getValue.toString)
        .map(typeFromName)
      TypeRefDetails(typeOverride.getOrElse(typeFromName(default)))
    }

    lazy val mappedType = apiType match {
      case boolean: BooleanType =>
        overrideTypeOr(boolean, context.params.defaultTypes.boolean).addDefaultValue(boolean)

      case integer: IntegerType =>
        overrideTypeOr(integer, numberTypeString(integer.getFormat)).addDefaultValue(integer)

      case number: NumberType =>
        overrideTypeOr(number, numberTypeString(number.getFormat)).addDefaultValue(number)

      // only use enum type names on top-level defined enums
      // we would need to generate types for property types otherwise
      case string: StringType
          if (string.getName eq null) ||
            (apiType.eContainer().eClass().getName == "Property" && string.getName == "string") =>
        TypeRefDetails(typeFromName(context.params.defaultTypes.string)).addDefaultValue(string)

      case stringEnum: StringType if isEnumType(stringEnum) =>
        TypeRefDetails(Type.Name(stringEnum.getName)).addDefaultEnum(stringEnum)

      case string: StringType =>
        overrideTypeOr(string, context.params.defaultTypes.string).addDefaultValue(string)

      case array: ArrayType =>
        val arrayType = getAnnotation(array)("scala-array-type")
          .map(_.getValue.getValue.toString)
          .getOrElse(context.params.defaultTypes.array)
        val itemTypeOverride = getAnnotation(array)("scala-type").map(_.getValue.getValue.toString)
        // we do not need to be optional inside an collection, hence setting it to false
        // also, arrays do not support default values
        TypeRefDetails(
          Type.Apply(
            typeFromName(arrayType),
            Type.ArgClause(
              scalaTypeRef(array.getItems, false, itemTypeOverride, defaultAnyTypeName)
                .map(
                  _.scalaType
                )
                .toList
            )
          ),
          None,
          Some(Term.Select(packageTerm(arrayType), Term.Name("empty"))),
          isCollection = true
        )

      case objectType: ObjectType if objectType.getName != "object" =>
        TypeRefDetails(Type.Name(objectType.getName), getPackageName(objectType), None)

      case union: UnionType =>
        TypeRefDetails(
          Type.Apply(
            Type.Name("Either"),
            Type.ArgClause(
              union.getOneOf.asScala
                .flatMap(scalaTypeRef(_, false, None, defaultAnyTypeName))
                .map(_.scalaType)
                .toList
            )
          )
        )

      case dateTime: DateTimeType =>
        overrideTypeOr(dateTime, context.params.defaultTypes.dateTime).addDefaultValue(dateTime)

      case date: DateOnlyType =>
        overrideTypeOr(date, context.params.defaultTypes.date).addDefaultValue(date)

      case time: TimeOnlyType =>
        overrideTypeOr(time, context.params.defaultTypes.time).addDefaultValue(time)

      case _ =>
        TypeRefDetails(typeFromName(defaultAnyTypeName))
    }

    val typeRefDetails = typeName match {
      case Some(scalaTypeName) => TypeRefDetails(typeFromName(scalaTypeName), None, None)
      case None                => mappedType
    }

    typeRefDetails match {
      case details: TypeRefDetails if !optional =>
        Some(TypeRef(details.baseType, details.packageName, details.defaultValue))

      case TypeRefDetails(baseType, packageName, Some(_), true) =>
        Some(
          TypeRef(
            Type.Apply(Type.Name("Option"), Type.ArgClause(List(baseType))),
            packageName,
            Some(q"None")
          )
        )

      case TypeRefDetails(baseType, packageName, Some(defaultValue), false) =>
        Some(
          TypeRef(
            Type.Apply(Type.Name("Option"), Type.ArgClause(List(baseType))),
            packageName,
            Some(q"Some($defaultValue)")
          )
        )

      case TypeRefDetails(baseType, packageName, None, _) =>
        Some(
          TypeRef(
            Type.Apply(Type.Name("Option"), Type.ArgClause(List(baseType))),
            packageName,
            Some(q"None")
          )
        )
    }
  }

  def scalaProperty(prop: TypedElement)(fallbackType: String)(implicit
      context: ModelGenContext
  ): Option[Term.Param] = {
    lazy val optional = !prop.getRequired
    val scalaTypeAnnotation =
      Option(prop.getAnnotation("scala-type")).map(_.getValue.getValue.toString)

    ModelGen
      .scalaTypeRef(prop.getType, optional, scalaTypeAnnotation, fallbackType)
      .map(ref => Term.Param(Nil, Term.Name(prop.getName), Some(ref.scalaType), ref.defaultValue))
  }

  /** map type refs from the 'asMap' annotation to real scala types */
  private def mapTypeToScala(anyTypeName: String, context: ModelGenContext): String => Type = {
    case "string" => Type.Name("String")
    case "any"    => typeFromName(anyTypeName)
    case arrayLike if arrayLike.endsWith("[]") =>
      val arrayType          = context.params.defaultTypes.array
      val itemType: Type.Ref = typeFromName(arrayLike.replaceFirst("\\[]", ""))

      Type.Apply(
        typeFromName(arrayType),
        Type.ArgClause(List(itemType))
      )
    case other =>
      context.api.typesByName
        .get(other)
        .flatMap(ModelGen.scalaTypeRef(_, false, None, anyTypeName)(context))
        .map(_.scalaType)
        .flatMap {
          case ref: Type.Ref =>
            Some(ref)
          case _ =>
            None
        }
        .getOrElse(typeFromName(other))
  }

  def isSingleton(objectType: ObjectType, anyTypeName: String)(implicit
      context: ModelGenContext
  ): Boolean =
    ModelGen.isMapType(objectType, anyTypeName).isEmpty &&
      context.params.fieldMatchPolicy.isSingleton(objectType)

  def isMapType(objectType: ObjectType, anyTypeName: String)(implicit
      context: ModelGenContext
  ): Option[MapTypeSpec] = {
    getAnnotation(objectType)("asMap").map(_.getValue) match {
      case Some(asMap: ObjectInstance) =>
        val properties = asMap.getValue.asScala
        for {
          keyType   <- properties.find(_.getName == "key").map(_.getValue.getValue.toString)
          valueType <- properties.find(_.getName == "value").map(_.getValue.getValue.toString)
        } yield MapTypeSpec(
          typeFromName(context.params.defaultTypes.map),
          mapTypeToScala(anyTypeName, context)(keyType),
          mapTypeToScala(anyTypeName, context)(valueType)
        )

      case _ => None
    }
  }
}

object ModelGenRunner {
  def run(generator: ModelGen)(params: ModelGenParams): IO[GeneratedModel] = for {
    api   <- RMFUtil.readModel(params.raml)
    model <- generator.generate(api, params)
  } yield model
}
