package scraml.libs

import io.vrap.rmf.raml.model.types.StringType
import scraml.{DefnWithCompanion, LibrarySupport, ModelGenContext}

import scala.meta.{Defn, Import, Importee, Importer, Name, Pkg, Stat, Term}

case class PackageObjectExtensionSupport(
    wrapped: LibrarySupport,
    baseTypeFullQualifiedName: String,
    additionalImports: Seq[String]
) extends LibrarySupport {
  private def mkImports: List[Stat] =
    additionalImports.map { entry =>
      entry.split('.').toList match {
        case Nil =>
          throw new RuntimeException("configuration error: empty additional import")
        case topLevel :: Nil =>
          throw new RuntimeException(s"configuration error: cannot import $topLevel")
        case first :: sels :: Nil =>
          Import(
            Importer(
              Term.Name(first),
              Importee.Name(Name.Indeterminate(sels)) :: Nil
            ) :: Nil
          )
        case first :: second :: multiple if multiple.last == "_" =>
          Import(
            Importer(
              multiple.init.foldLeft(Term.Select(Term.Name(first), Term.Name(second))) { (a, b) =>
                Term.Select(a, Term.Name(b))
              },
              Importee.Wildcard() :: Nil
            ) :: Nil
          )
        case first :: second :: multiple =>
          Import(
            Importer(
              multiple.init.foldLeft(Term.Select(Term.Name(first), Term.Name(second))) { (a, b) =>
                Term.Select(a, Term.Name(b))
              },
              Importee.Name(Name(multiple.last)) :: Nil
            ) :: Nil
          )
      }
    }.toList

  override def modifyClass(classDef: Defn.Class, companion: Option[Defn.Object])(
      context: ModelGenContext
  ): DefnWithCompanion[Defn.Class] =
    wrapped.modifyClass(classDef, companion)(context)

  override def modifyObject(objectDef: Defn.Object)(
      context: ModelGenContext
  ): DefnWithCompanion[Defn.Object] =
    wrapped.modifyObject(objectDef)(context)

  override def modifyTrait(traitDef: Defn.Trait, companion: Option[Defn.Object])(
      context: ModelGenContext
  ): DefnWithCompanion[Defn.Trait] =
    wrapped.modifyTrait(traitDef, companion)(context)

  override def modifyPackageObject: Pkg.Object => Pkg.Object = packageObject =>
    wrapped.modifyPackageObject(LibrarySupport.appendPkgObjectStats(packageObject, mkImports))

  override def modifyEnum(
      enumType: StringType
  )(enumTrait: Defn.Trait, companion: Option[Defn.Object]): DefnWithCompanion[Defn.Trait] =
    wrapped.modifyEnum(enumType)(enumTrait, companion)
}
