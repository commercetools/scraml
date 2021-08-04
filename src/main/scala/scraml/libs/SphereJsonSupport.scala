package scraml.libs

import _root_.io.vrap.rmf.raml.model.types.ObjectType
import scraml.LibrarySupport.appendObjectStats
import scraml.MetaUtil.typeFromName
import scraml.RMFUtil.getAnnotation
import scraml.{DefnWithCompanion, LibrarySupport, ModelGenContext}

import scala.meta._

object SphereJsonSupport extends LibrarySupport {
  private def deriveObjectJson(typeName: String): List[Stat] =
    q"""import io.sphere.json.generic._
        implicit lazy val json = jsonProduct0(${Term.Name(typeName)})
     """.stats

  private def deriveJson(objectType: ObjectType): List[Stat] =
    if(getAnnotation(objectType)("scala-derive-json").exists(_.getValue.getValue.toString.toBoolean)) {
      q"""import io.sphere.json.generic._
          import io.sphere.json._

          implicit lazy val json: JSON[${Type.Name(objectType.getName)}] = deriveJSON[${Type.Name(objectType.getName)}]
          """.stats
    } else List.empty

  private def getDiscriminatorValueMod(objectType: ObjectType): Option[Mod.Annot] =
    if (Option(objectType.getDiscriminatorValue).isDefined) {
        Some(Mod.Annot(Init(typeFromName("io.sphere.json.annotations.JSONTypeHint"), Name(""), List(List(Lit.String(objectType.getDiscriminatorValue))))))
    } else None

  override def modifyClass(classDef: Defn.Class, companion: Option[Defn.Object])(context: ModelGenContext): DefnWithCompanion[Defn.Class] =
    DefnWithCompanion(
      defn = classDef.copy(mods = getDiscriminatorValueMod(context.objectType).toList ++ classDef.mods),
      companion = companion.map(appendObjectStats(_, deriveJson(context.objectType)))
    )

  override def modifyObject(objectDef: Defn.Object)(context: ModelGenContext): DefnWithCompanion[Defn.Object] =
    DefnWithCompanion(appendObjectStats(objectDef.copy(mods = getDiscriminatorValueMod(context.objectType).toList ++ objectDef.mods), deriveObjectJson(context.objectType.getName)), None)

  override def modifyTrait(traitDef: Defn.Trait, companion: Option[Defn.Object])(context: ModelGenContext): DefnWithCompanion[Defn.Trait] =
    DefnWithCompanion(
      defn = {
        val typeHintField = if (Option(context.objectType.getDiscriminator).isDefined) {
          List(Mod.Annot(Init(typeFromName("io.sphere.json.annotations.JSONTypeHintField"), Name(""), List(List(Lit.String(context.objectType.getDiscriminator))))))
        } else Nil

        traitDef.copy(mods = typeHintField ++ traitDef.mods)
      },
      companion = companion.map(appendObjectStats(_, deriveJson(context.objectType)))
    )
}
