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
  lazy val defaultArrayTypeName = "List"
  lazy val defaultAnyTypeName = "Any"

  lazy val dateTimeType: Type.Ref = typeFromName("java.time.LocalDateTime")
  lazy val dateOnlyType: Type.Ref = typeFromName("java.time.LocalDate")
  lazy val timeOnlyType: Type.Ref = typeFromName("java.time.LocalTime")

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

  private def getDiscriminatorValueMod(objectType: ObjectType, params: ModelGenParams): Option[Mod.Annot] =
    params.jsonSupport match {
      case Some(Sphere) if Option(objectType.getDiscriminatorValue).isDefined =>
        Some(Mod.Annot(Init(typeFromName("io.sphere.json.annotations.JSONTypeHint"), Name(""), List(List(Lit.String(objectType.getDiscriminatorValue))))))
      case _ => None
    }

  private def typeProperties(objectType: ObjectType) =
    objectType.getAllProperties.asScala.filter(property => !discriminators(objectType).contains(property.getName))

  private def caseClassSource(objectType: ObjectType, params: ModelGenParams, baseType: Option[TypeRef] = None, extendType: Option[Type] = None): Defn.Class = {
    val classParams = getAnnotation(objectType)("asMap").map(_.getValue) match {
      case Some(asMap: ObjectInstance) =>
        val properties = asMap.getValue.asScala
        val mapParam: Option[List[Term.Param]] = for {
          keyType <- properties.find(_.getName == "key").map(_.getValue.getValue.toString)
          valueType <- properties.find(_.getName == "value").map(_.getValue.getValue.toString)
        } yield List(Term.Param(Nil, Term.Name("values"), Some(Type.Apply(Type.Name("Map"), List(Type.Name(mapTypeToScala(keyType)), Type.Name(mapTypeToScala(valueType))))), None))
        mapParam.toList

      case _ =>
        List(typeProperties(objectType).flatMap(scalaProperty).toList)
    }

    Defn.Class(
      mods = getDiscriminatorValueMod(objectType, params).toList ++ List(Mod.Final(), Mod.Case()),
      name = Type.Name(objectType.getName),
      tparams = Nil,
      ctor = Ctor.Primary(
        mods = Nil,
        name = Name.Anonymous(),
        paramss = classParams
      ),
      templ = Template(
        early = Nil,
        inits = initFromTypeOpt(baseType.map(_.scalaType)) ++ initFromTypeOpt(extendType),
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

  private def companionObjectSource(objectType: ObjectType, params: ModelGenParams): Defn.Object = {
    val typeName = objectType.getName
    val cats = params.catsSupport.toList.flatMap {
      case EqSupport =>
          q"""
            import cats.kernel.Eq
            implicit val ${Pat.Var(Term.Name(typeName + "Eq"))}: Eq[${Type.Name(typeName)}] =
              Eq.fromUniversalEquals
          """.stats
      case ShowSupport =>
          q"""
            import cats.Show
            implicit val ${Pat.Var(Term.Name(typeName + "Show"))}: Show[${Type.Name(typeName)}] = Show.show {
              instance =>
                val buffer = new StringBuilder($typeName)
                buffer.append(':')
                buffer.append('\n')

                0.until(instance.productArity).zip(instance.productElementNames).foreach {
                  case (index, name) =>
                    buffer.append('\t')
                    buffer.append(name)
                    buffer.append(": ")
                    buffer.append(instance.productElement(index))
                    buffer.append('\n')
                }

                buffer.toString()
            }""".stats
    }

    val json = params.jsonSupport.map {
      case Sphere =>
        if(getAnnotation(objectType)("scala-derive-json").exists(_.getValue.getValue.toString.toBoolean)) {
          q"""import io.sphere.json.generic._
            import io.sphere.json._

            implicit lazy val json: JSON[${Type.Name(typeName)}] = deriveJSON[${Type.Name(typeName)}]
          """.stats
        } else List.empty

      case other => throw new UnsupportedOperationException(s"$other support not implemented yet")
    }

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
        stats = json.getOrElse(List.empty) ++ cats
      )
    )
  }

  private def caseObjectSource(objectType: ObjectType, params: ModelGenParams, baseType: Option[TypeRef] = None, extendType: Option[Type] = None): Defn.Object =
    Defn.Object(
      mods = getDiscriminatorValueMod(objectType, params).toList ++ List(Mod.Case()),
      Term.Name(objectType.getName),
      Template(Nil, inits = initFromTypeOpt(baseType.map(_.scalaType)) ++ initFromTypeOpt(extendType), Self(Name(""), None), Nil, Nil)
    )

  private def traitSource(packageName: String,
                          objectType: ObjectType,
                          baseType: Option[TypeRef] = None,
                          params: ModelGenParams,
                          extendType: Option[Type] = None): Defn.Trait = {
    val defs = typeProperties(objectType).flatMap { property =>
      scalaTypeRef(property.getType, !property.getRequired).map { scalaType =>
        Decl.Def(Nil, Term.Name(property.getName), tparams = Nil, paramss = Nil, scalaType.scalaType)
      }
    }.toList

    val jsonMods = params.jsonSupport match {
      case Some(Sphere) if Option(objectType.getDiscriminator).isDefined =>
        List(Mod.Annot(Init(typeFromName("io.sphere.json.annotations.JSONTypeHintField"), Name(""), List(List(Lit.String(objectType.getDiscriminator))))))
      case _ => Nil
    }

    val sealedModOpt: Option[Mod.Sealed] =
      if (getSubTypes(objectType).forall(getPackageName(_).contains(packageName))) {
        Some(Mod.Sealed())
      } else None

    Defn.Trait(
      mods = jsonMods ++ sealedModOpt,
      name = Type.Name(objectType.getName),
      tparams = Nil,
      ctor = Ctor.Primary(Nil, Name(""), Nil),
      templ = Template(
        early = Nil,
        inits =  initFromTypeOpt(baseType.map(_.scalaType)) ++ initFromTypeOpt(extendType),
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
      (source, companion) =
        discriminator match {
          case Some(_) | None if isAbstract || getSubTypes(objectType).nonEmpty =>
            (traitSource(packageName, objectType, scalaBaseTypeRef, params, extendType), Some(companionObjectSource(objectType, params)))

          case None if !isMapType && typeProperties(objectType).isEmpty =>
            (caseObjectSource(objectType, params, scalaBaseTypeRef, extendType), None)

          case None =>
            (caseClassSource(objectType, params, scalaBaseTypeRef, extendType), Some(companionObjectSource(objectType, params)))
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
    } yield ObjectTypeSource(objectType.getName, source, packageName, comment, companion)
  }

  private def appendSource(file: File, source: GeneratedSource): IO[GeneratedFile] =
    writeToFile(file, s"${source.comment}\n${source.source.toString()}\n${source.companion.map(_.toString()+ "\n").getOrElse("")}\n", append = true).map(GeneratedFile(source, _))

  private def termSelect(parts: List[String], default: String): Term.Ref = parts match {
    case Nil => Term.Name(default)
    case first :: Nil => Term.Name(first)
    case first :: second :: Nil => Term.Select(Term.Name(second), Term.Name(first))
    case first :: remainder => Term.Select(termSelect(remainder, default), Term.Name(first))
  }

  private[scraml] def packageTerm(packageName: String): Term.Ref =
    termSelect(packageName.split("\\.").toList.reverse, packageName)

  private def typeFromNameParts(parts: List[String], default: String): Type.Ref =
    parts match {
      case Nil => Type.Name(default)
      case first :: Nil => Type.Name(first)
      case first :: second :: Nil => Type.Select(Term.Name(second), Type.Name(first))
      case first :: remainder => Type.Select(termSelect(remainder, first), Type.Name(first))
    }

  private[scraml] def typeFromName(fullQualifiedName: String): Type.Ref =
    typeFromNameParts(fullQualifiedName.split("\\.").toList.reverse, fullQualifiedName)

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
