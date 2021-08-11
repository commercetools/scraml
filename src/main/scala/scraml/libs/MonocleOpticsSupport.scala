package scraml.libs

import scraml.LibrarySupport.appendObjectStats
import scraml.{DefnWithCompanion, LibrarySupport, ModelGenContext}

import scala.meta._

object MonocleOpticsSupport extends LibrarySupport {
  override def modifyClass(classDef: Defn.Class, companion: Option[Defn.Object])(context: ModelGenContext): DefnWithCompanion[Defn.Class] =
    DefnWithCompanion(classDef, companion.map(appendObjectStats(_, generateOptics(classDef))))

    override def modifyTrait(traitDef: Defn.Trait, companion: Option[Defn.Object])(context: ModelGenContext): DefnWithCompanion[Defn.Trait] =
      DefnWithCompanion(traitDef, companion.map(appendObjectStats(_, generateOptics(traitDef))))

  private def generateOptics(traitDef: Defn.Trait): List[Stat] =
    List[Stat](q"""
      object Optics {
        import monocle.Getter

        ..${
          generatePropertiesCode(traitDef) {
            prop =>
              val classType = traitDef.name
              val propType = prop.decltpe
              val propName = Term.Name(prop.name.value)

              List(q"""
                val ${Pat.Var(propName)}: Getter[$classType, $propType] =
                  Getter[$classType, $propType](_.$propName)
                """
             )
          }
        }
      }
      """
    )

  private def generateOptics(classDef: Defn.Class): List[Stat] =
    List[Stat](q"""
      object Optics {
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
