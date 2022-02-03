package scraml.libs

import scraml.LibrarySupport.appendObjectStats
import scraml.{DefnWithCompanion, LibrarySupport, ModelGenContext}

import scala.meta._

object CatsEqSupport extends LibrarySupport {
  object IsVersionedEntity extends HasProperties("id" :: "version" :: Nil)

  private def eqStats(classDef: Defn.Class)(implicit
      context: ModelGenContext
  ): List[Stat] =
    q"""
      import cats.kernel.Eq
      implicit val ${Pat.Var(Term.Name(classDef.name.value + "Eq"))}: Eq[${classDef.name}] =
        new Eq[${classDef.name}] {
          override def eqv(a: ${classDef.name}, b: ${classDef.name}): Boolean = {
            ${
      val checks = classDef match {
        case IsVersionedEntity() =>
          List[Term](q"a.id.equals(b.id)", q"a.version == b.version")
        case _ =>
          val checkAdditional = context.params.fieldMatchPolicy
            .additionalProperties(context.objectType)(context)
            .map { ap =>
              val propName = Term.Name(ap.propertyName)
              q"""a.$propName == b.$propName"""
            }

          generatePropertiesCode(classDef) { prop =>
            List[Term](q"""a.${Term.Name(prop.name.value)} == b.${Term.Name(prop.name.value)}""")
          } ::: checkAdditional.toList
      }

      checks match {
        case Nil         => q"""a.equals(b)"""
        case head :: Nil => head
        case head :: tail =>
          tail.foldLeft(head) { case (accum: Term, check: Term) =>
            q"""($accum) && ($check)"""
          }
      }
    }
        }
      }
    """.stats

  override def modifyClass(classDef: Defn.Class, companion: Option[Defn.Object])(implicit
      context: ModelGenContext
  ): DefnWithCompanion[Defn.Class] =
    DefnWithCompanion(
      classDef,
      companion.map(appendObjectStats(_, eqStats(classDef)))
    )
}
