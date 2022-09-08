package scraml.libs

import _root_.io.vrap.rmf.raml.model.types.ObjectType
import scraml.LibrarySupport.appendObjectStats
import scraml.MetaUtil.typeFromName
import scraml.RMFUtil.getAnnotation
import scraml.{DefnWithCompanion, JsonSupport, LibrarySupport, ModelGenContext}

import scala.meta._
import _root_.io.vrap.rmf.raml.model.types.StringType

object SphereJsonSupport extends LibrarySupport with JsonSupport {
  override def jsonType: String = "org.json4s.JsonAST.JValue"

  private def shouldDeriveJson(objectType: ObjectType): Boolean =
    getAnnotation(objectType)("scala-derive-json").exists(_.getValue.getValue.toString.toBoolean)

  private def deriveObjectJson(objectType: ObjectType): List[Stat] =
    if (shouldDeriveJson(objectType)) {
      q"""import io.sphere.json.generic._
          implicit lazy val json = jsonProduct0(${Term.Name(objectType.getName)})
      """.stats
    } else List.empty

  private def deriveJson(objectType: ObjectType): List[Stat] =
    if (shouldDeriveJson(objectType)) {
      q"""import io.sphere.json.generic._
          import io.sphere.json._

          implicit lazy val json: JSON[${Type.Name(objectType.getName)}] = deriveJSON[${Type.Name(
        objectType.getName
      )}]
      """.stats
    } else List.empty

  private def deriveWithFallback(context: ModelGenContext): List[Stat] =
    if (shouldDeriveJson(context.objectType)) {
      val subTypes = context.getDirectSubTypes.toList
      val matchTypes = Term.Match(
        Term.Name("value"),
        cases = subTypes.map(subType => {
          val caseName = subType.getName.toLowerCase
          val typeName = subType.getName
          Case(
            Pat.Typed(Pat.Var(Term.Name(caseName)), Type.Name(typeName)),
            None,
            Term.Apply(
              Term.Select(Term.Select(Term.Name(typeName), Term.Name("json")), Term.Name("write")),
              List(Term.Name(caseName))
            )
          ),
        }),
        mods = Nil
      )

      val read = subTypes.headOption match {
        case Some(firstSubType) =>
          val initRead: String =
            q"""
              ${Term.Name(firstSubType.getName)}.json.read(jval)
              """.toString
          subTypes.drop(1).foldLeft(initRead) { case (acc, next) =>
            val nextRead = q"""orElse(${Term.Name(next.getName)}.json.read(jval))""".toString
            s"$acc.$nextRead"
          }
        case _ => ""
      }

      val jsonStats = q"""
                  import io.sphere.json.generic._
                  import io.sphere.json._
                  import org.json4s._

                  implicit val json: JSON[${Type.Name(
        context.objectType.getName
      )}] = new JSON[${Type.Name(context.objectType.getName)}] {
                  override def read(jval: JsonAST.JValue): JValidation[${Type.Name(
        context.objectType.getName
      )}] = ${read.parse[Term].get}
                  override def write(value: ${Type
        .Name(context.objectType.getName)}): JsonAST.JValue = $matchTypes
                }""".stats
      jsonStats
    } else List.empty

  private def getDiscriminatorValueMod(objectType: ObjectType): Option[Mod.Annot] =
    if (Option(objectType.getDiscriminatorValue).isDefined) {
      Some(
        Mod.Annot(
          Init(
            typeFromName("io.sphere.json.annotations.JSONTypeHint"),
            Name(""),
            List(List(Lit.String(objectType.getDiscriminatorValue)))
          )
        )
      )
    } else None

  override def modifyClass(classDef: Defn.Class, companion: Option[Defn.Object])(implicit
      context: ModelGenContext
  ): DefnWithCompanion[Defn.Class] =
    DefnWithCompanion(
      defn =
        classDef.copy(mods = getDiscriminatorValueMod(context.objectType).toList ++ classDef.mods),
      companion = companion.map(appendObjectStats(_, deriveJson(context.objectType)))
    )

  override def modifyObject(
      objectDef: Defn.Object
  )(implicit context: ModelGenContext): DefnWithCompanion[Defn.Object] =
    DefnWithCompanion(
      appendObjectStats(
        objectDef.copy(mods =
          getDiscriminatorValueMod(context.objectType).toList ++ objectDef.mods
        ),
        deriveObjectJson(context.objectType)
      ),
      None
    )

  override def modifyTrait(traitDef: Defn.Trait, companion: Option[Defn.Object])(implicit
      context: ModelGenContext
  ): DefnWithCompanion[Defn.Trait] =
    DefnWithCompanion(
      defn = {
        val typeHintField = if (Option(context.objectType.getDiscriminator).isDefined) {
          List(
            Mod.Annot(
              Init(
                typeFromName("io.sphere.json.annotations.JSONTypeHintField"),
                Name(""),
                List(List(Lit.String(context.objectType.getDiscriminator)))
              )
            )
          )
        } else Nil

        traitDef.copy(mods = typeHintField ++ traitDef.mods)
      },
      companion = {
        val stats = if (Option(context.objectType.getDiscriminator).isDefined) {
          deriveJson(context.objectType)
        } else deriveWithFallback(context)

        companion.map(appendObjectStats(_, stats))
      }
    )

  override def modifyEnum(
      enumType: StringType
  )(enumTrait: Defn.Trait, companion: Option[Defn.Object]): DefnWithCompanion[Defn.Trait] = {
    import scala.jdk.CollectionConverters._

    val toJson = q"""
      implicit lazy val toJson: ToJSON[${Type
      .Name(enumType.getName())}] = ToJSON.stringWriter.contramap(_.toString)
    """

    val other = Case(
      Pat.Var(Term.Name("other")),
      None,
      q"""JSONParseError(s"not a instance of required enum: $$other").invalidNel"""
    )

    val matchEnumInstances = Term.PartialFunction(
      enumType
        .getEnum()
        .asScala
        .map(instance =>
          Case(
            Lit.String(instance.getValue().toString()),
            None,
            Term.Select(Term.Name(instance.getValue().toString()), Term.Name("valid"))
          )
        )
        .toList ++ List(other)
    )

    val fromJson = q"""
    implicit lazy val fromJson: FromJSON[${Type
      .Name(enumType.getName())}] = (jval: JsonAST.JValue) =>
      FromJSON.stringReader
        .read(jval)
        .andThen($matchEnumInstances)
    """

    val stats =
      q"""
        import io.sphere.json.ToJSON
        import io.sphere.json.FromJSON
        import io.sphere.json.JSONParseError

        import org.json4s._

        import cats.implicits.toContravariantOps
        import cats.data.Validated
        import cats.syntax.validated._
        $toJson
        $fromJson
       """.stats

    DefnWithCompanion(enumTrait, companion.map(appendObjectStats(_, stats)))
  }

}
