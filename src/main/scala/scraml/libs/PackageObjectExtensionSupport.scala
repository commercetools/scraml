package scraml.libs

import io.vrap.rmf.raml.model.modules.Api
import scraml.LibrarySupport

import scala.meta.{Import, Importee, Importer, Name, Pkg, Stat, Term}

case class PackageObjectExtensionSupport(additionalImports: Seq[String]) extends LibrarySupport {
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

  override def modifyPackageObject(api: Api): Pkg.Object => Pkg.Object = packageObject =>
    LibrarySupport.appendPkgObjectStats(packageObject, mkImports)
}
