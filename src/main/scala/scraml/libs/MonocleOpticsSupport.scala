package scraml.libs

import scraml.LibrarySupport.appendObjectStats
import scraml.{DefnWithCompanion, LibrarySupport, ModelGenContext}

import scala.meta._

object MonocleOpticsSupport extends LibrarySupport {
  override def modifyClass(classDef: Defn.Class, companion: Option[Defn.Object])(implicit
      context: ModelGenContext
  ): DefnWithCompanion[Defn.Class] =
    classDef match {
      case HasAnyProperties() =>
        DefnWithCompanion(classDef, companion.map(appendObjectStats(_, generateOptics(classDef))))
      case _ => super.modifyClass(classDef, companion)(context)
    }

  override def modifyTrait(traitDef: Defn.Trait, companion: Option[Defn.Object])(implicit
      context: ModelGenContext
  ): DefnWithCompanion[Defn.Trait] =
    traitDef match {
      case HasAnyProperties() =>
        DefnWithCompanion(traitDef, companion.map(appendObjectStats(_, generateOptics(traitDef))))
      case _ => super.modifyTrait(traitDef, companion)(context)
    }

  private def generateOptics(traitDef: Defn.Trait): List[Stat] =
    List[Stat](
      q"""
      trait Optics {
        import monocle.Getter

        ..${generatePropertiesCode(traitDef) { prop =>
        val classType = traitDef.name
        val propType  = prop.decltpe
        val propName  = Term.Name(prop.name.value)

        List(q"""
                val ${Pat.Var(propName)}: Getter[$classType, $propType] =
                  Getter[$classType, $propType](_.$propName)
                """)
      }}
      }
      """,
      q"""object Optics extends Optics"""
    )

  private def generateOptics(classDef: Defn.Class)(implicit
      context: ModelGenContext
  ): List[Stat] =
    List[Stat](
      q"""
      trait Optics {
        import monocle.Lens

        ..${generatePropertiesCode(classDef) { prop =>
        val classType = classDef.name
        val propType  = prop.decltpe.get
        val propName  = Term.Name(prop.name.value)

        context.params.fieldMatchPolicy.additionalProperties(context.objectType) match {
          case Some(descriptor) =>
            List(
              q"""
                     val ${Pat.Var(propName)}: Lens[$classType, $propType] =
                       Lens[$classType, $propType](_.$propName) {
                         a => s => s.copy($propName = a)(
                           s.${Term.Name(descriptor.propertyName)}
                         )
                       }

                   """
            )

          case None =>
            List(
              q"""
                     val ${Pat.Var(propName)}: Lens[$classType, $propType] =
                       Lens[$classType, $propType](_.$propName) {
                         a => s => s.copy($propName = a)
                       }
                  """
            )
        }
      }}
      }
      """,
      q"""object Optics extends Optics"""
    )
}
