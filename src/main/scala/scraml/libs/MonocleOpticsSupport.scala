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
  ): List[Stat] = {
    val classType = classDef.name

    List[Stat](
      q"""
      trait Optics {
        import monocle.Lens

        ..${generatePropertiesCode(classDef) {
        case NamedProperty(param, _, declaredName) =>
          val propType  = param.decltpe.get
          val propName  = Term.Name(declaredName)
          val paramName = Term.Name(param.name.value)

          context.params.fieldMatchPolicy.additionalProperties(context.objectType) match {
            case Some(descriptor) =>
              List(
                q"""
                     val ${Pat.Var(propName)}: Lens[$classType, $propType] =
                       Lens[$classType, $propType](_.$paramName) {
                         a => s => s.copy($paramName = a)(
                           s.${Term.Name(descriptor.propertyName)}
                         )
                       }

                   """
              )

            case None =>
              List(
                q"""
                     val ${Pat.Var(propName)}: Lens[$classType, $propType] =
                       Lens[$classType, $propType](_.$paramName) {
                         a => s => s.copy($paramName = a)
                       }
                  """
              )
          }

        case _ =>
          List.empty
      }}

      ..${context.params.fieldMatchPolicy
        .additionalProperties(context.objectType)
        .map { ap =>
          val propName = Term.Name(ap.propertyName)

          q"""
                val ${Pat.Var(propName)}: Lens[$classType, Option[${ap.propertyType}]] =
                  Lens[$classType, Option[${ap.propertyType}]](_.$propName) {
                    a => s => s.copy()($propName = a)
                  }
           """
        }
        .toList}
      }
      """,
      q"""object Optics extends Optics"""
    )
  }
}
