package scraml

import cats.effect.IO
import io.vrap.rmf.raml.model.modules.Api
import io.vrap.rmf.raml.model.types.{AnyType, ObjectType}
import scraml.RMFUtil.getAnnotation
import scraml.libs.{CatsEqSupport, CatsShowSupport, SphereJsonSupport}

import java.io.File
import scala.meta.{Defn, Member, Stat, Type}

sealed trait JsonSupport
case object Sphere extends JsonSupport
case object Circe extends JsonSupport

sealed trait CatsSupport
case object EqSupport extends CatsSupport
case object ShowSupport extends CatsSupport

final case class ModelGenParams(raml: File,
                                targetDir: File,
                                basePackage: String,
                                jsonSupport: Option[JsonSupport] = None,
                                catsSupport: Set[CatsSupport] = Set.empty,
                                formatConfig: Option[File] = None) {
  def librarySupport: List[LibrarySupport] = jsonSupport.map {
    case Sphere => List(SphereJsonSupport)
    case Circe => Nil
  }.getOrElse(Nil) ++ catsSupport.map {
    case EqSupport => CatsEqSupport
    case ShowSupport => CatsShowSupport
  }
}

final case class GeneratedModel(files: Seq[GeneratedFile]) {
  override def toString: String = {
    files.map(generatedFile => s"${generatedFile.source.name} (${generatedFile.file.getPath})").mkString("\n")
  }
}

final case class ApiContext(private val api: Api) {
  import scala.jdk.CollectionConverters._

  def getTypes: Iterator[AnyType] = api.getTypes.asScala.iterator

  lazy val typesByName: Map[String, AnyType] = {
    api.getTypes.asScala.map(aType =>(aType.getName, aType))
  }.toMap

  lazy val scalaExtends: Map[String, AnyType] =
    api.getTypes.asScala.flatMap { aType =>
      for {
        annotation <- getAnnotation(aType)("scala-extends")
        anyType <- typesByName.get(annotation.getValue.getValue.toString)
      } yield (aType.getName, anyType)
    }.toMap
}

final case class ModelGenContext(packageName: String,
                                 objectType: ObjectType,
                                 params: ModelGenParams,
                                 api: ApiContext,
                                 baseType: Option[TypeRef] = None,
                                 extendType: Option[Type] = None)

final case class DefnWithCompanion[T <: Defn with Member](defn: T, companion: Option[Defn.Object])

trait LibrarySupport {
  def modifyClass(classDef: Defn.Class, companion: Option[Defn.Object])(context: ModelGenContext): DefnWithCompanion[Defn.Class] =
    DefnWithCompanion(classDef, None)

  def modifyObject(objectDef: Defn.Object)(context: ModelGenContext): DefnWithCompanion[Defn.Object] =
    DefnWithCompanion(objectDef, None)

  def modifyTrait(traitDef: Defn.Trait, companion: Option[Defn.Object])(context: ModelGenContext): DefnWithCompanion[Defn.Trait] =
    DefnWithCompanion(traitDef, companion)
}

object LibrarySupport {
  def applyClass(defn: Defn.Class, companion: Option[Defn.Object])(libs: List[LibrarySupport], context: ModelGenContext): DefnWithCompanion[Defn.Class] =
    libs.foldLeft(DefnWithCompanion(defn, companion)) { case (acc, lib) => lib.modifyClass(acc.defn, acc.companion)(context) }
  def applyObject(defn: Defn.Object)(libs: List[LibrarySupport], context: ModelGenContext): DefnWithCompanion[Defn.Object] =
    libs.foldLeft(DefnWithCompanion(defn, None)) { case (acc, lib) => lib.modifyObject(acc.defn)(context) }
  def applyTrait(defn: Defn.Trait, companion: Option[Defn.Object])(libs: List[LibrarySupport], context: ModelGenContext): DefnWithCompanion[Defn.Trait] =
    libs.foldLeft(DefnWithCompanion(defn, companion)) { case (acc, lib) => lib.modifyTrait(acc.defn, acc.companion)(context) }

  def appendObjectStats(defn: Defn.Object, stats: List[Stat]): Defn.Object =
    defn.copy(templ = defn.templ.copy(stats = defn.templ.stats ++ stats))
  def appendClassStats(defn: Defn.Class, stats: List[Stat]): Defn.Class =
    defn.copy(templ = defn.templ.copy(stats = defn.templ.stats ++ stats))
  def appendTraitStats(defn: Defn.Trait, stats: List[Stat]): Defn.Trait =
    defn.copy(templ = defn.templ.copy(stats = defn.templ.stats ++ stats))
}

trait ModelGen {
  def generate(api: Api, params: ModelGenParams): IO[GeneratedModel]
}

object ModelGenRunner {
  def run(generator: ModelGen)(params: ModelGenParams): IO[GeneratedModel] = for {
    api <- RMFUtil.readModel(params.raml)
    model <- generator.generate(api, params)
  } yield model
}
