package scraml.libs

import scraml.LibrarySupport.appendObjectStats
import scraml.{DefnWithCompanion, LibrarySupport, ModelGenContext}

import scala.meta._

object CatsShowSupport extends LibrarySupport {
  private def showStats(typeName: String): List[Stat] =
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

  override def modifyClass(classDef: Defn.Class, companion: Option[Defn.Object])(context: ModelGenContext): DefnWithCompanion[Defn.Class] =
    DefnWithCompanion(classDef, companion.map(appendObjectStats(_, showStats(context.objectType.getName))))
}
