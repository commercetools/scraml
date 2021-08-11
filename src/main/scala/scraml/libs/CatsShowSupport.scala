package scraml.libs

import scraml.LibrarySupport.appendObjectStats
import scraml.{DefnWithCompanion, LibrarySupport, ModelGenContext}
import scala.meta._

object CatsShowSupport extends LibrarySupport {
  private def showStats(classDef: Defn.Class): List[Stat] =
    q"""
      import cats.Show
      implicit val ${Pat.Var(Term.Name(classDef.name.value + "Show"))}: Show[${classDef.name}] = Show.show {
        instance =>
          val buffer = new StringBuilder(${classDef.name.value})
          buffer.append(':')
          buffer.append('\n')

          ..${
            generatePropertiesCode(classDef) {
              p =>
                List(
                  q"""buffer.append('\t')""",
                  q"""buffer.append(${p.name.value})""",
                  q"""buffer.append(": ")""",
                  q"""buffer.append(${Term.Name(p.name.value)})""",
                  q"""buffer.append('\n')"""
                )
            }
          }

          buffer.toString()
      }""".stats

  override def modifyClass(classDef: Defn.Class, companion: Option[Defn.Object])(context: ModelGenContext): DefnWithCompanion[Defn.Class] =
    DefnWithCompanion(
      classDef,
      companion.map(appendObjectStats(_, showStats(classDef)))
    )
}
