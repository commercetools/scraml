package scraml.libs

import scraml.LibrarySupport.appendObjectStats
import scraml.{DefnWithCompanion, LibrarySupport, ModelGenContext}

import scala.meta._

object MonocleLensSupport extends LibrarySupport {
  override def modifyClass(classDef: Defn.Class, companion: Option[Defn.Object])(context: ModelGenContext): DefnWithCompanion[Defn.Class] =
    DefnWithCompanion(classDef, companion.map(appendObjectStats(_, generateLenses(classDef))))

  private def generateLenses(classDef: Defn.Class): List[Stat] =
    List[Stat](q"""
      object Lenses {
        import monocle.Lens

        ..${
          generatePropertiesCode(classDef) {
            prop =>
              val classType = classDef.name
              val propType = prop.decltpe.get
              val propName = Term.Name(prop.name.value)

              List(
                q"""
                   val ${Pat.Var(propName)}: Lens[$classType, $propType] =
                     Lens[$classType, $propType](_.${propName}) {
                       a => s => s.copy(${propName} = a)
                     }
                """
              )
          }
        }
      }
      """
    )
}
