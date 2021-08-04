package scraml.libs

import scraml.LibrarySupport.appendObjectStats
import scraml.{DefnWithCompanion, LibrarySupport, ModelGenContext}

import scala.meta._

object CatsEqSupport extends LibrarySupport {
  private def eqStats(typeName: String): List[Stat] =
    q"""
      import cats.kernel.Eq
      implicit val ${Pat.Var(Term.Name(typeName + "Eq"))}: Eq[${Type.Name(typeName)}] =
        Eq.fromUniversalEquals
    """.stats

  override def modifyClass(classDef: Defn.Class, companion: Option[Defn.Object])(context: ModelGenContext): DefnWithCompanion[Defn.Class] =
    DefnWithCompanion(classDef, companion.map(appendObjectStats(_, eqStats(context.objectType.getName))))
}
