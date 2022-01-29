package scraml

import scala.meta._

import _root_.io.vrap.rmf.raml.model.types.ObjectType
import scraml.MetaUtil.typeFromName

/**
 * Defines the logic for supporting
 * [[https://github.com/raml-org/raml-spec/blob/master/versions/raml-10/raml-10.md#additional-properties additional properties]]
 * in
 * [[https://github.com/raml-org/raml-spec/blob/master/versions/raml-10/raml-10.md#object-type object types]]
 * which result in `case class` generation.
 */
protected[scraml] final case class AdditionalProperties(
  objectType: ObjectType
)(implicit private val context: ModelGenContext) {
  import context.params.fieldMatchPolicy

  val propertyName = "additionalProperties"

  private val spec = MapTypeSpec(
    typeFromName(context.params.defaultTypes.map),
    Type.Name("String"),
    typeFromName(context.anyTypeName),
    optional = true
  )

  def classDefinition(): Defn.Class =
    q"""
        final case class AdditionalProperties(
          private val underlying: ${spec.mapType}[${spec.keyType},${spec.valueType}]
        ) extends scala.Dynamic {
          override def toString(): String = underlying.mkString(", ")
          def selectDynamic(field: ${spec.keyType}): Option[${spec.valueType}] = underlying.get(field)
          def getOrElse[V >: ${spec.valueType}](key: ${spec.keyType}, default: => V): V = underlying.getOrElse(key, default)
          def isDefinedAt(key: ${spec.keyType}): Boolean = underlying.isDefinedAt(key)
          def isEmpty: Boolean = underlying.isEmpty
          def keySet: Set[${spec.keyType}] = underlying.keySet
          def keys: Iterable[${spec.keyType}] = underlying.keys
          def keysIterator: Iterator[${spec.keyType}] = underlying.keysIterator
          def nonEmpty: Boolean = !underlying.isEmpty
          def size: Int = underlying.size
          def values: Iterable[${spec.valueType}] = underlying.values
          def valuesIterator: Iterator[${spec.valueType}] = underlying.valuesIterator
        }
      """

  def companionDefinition(): Defn.Object =
    q"""
      object AdditionalProperties {
        import scala.util.matching.Regex
        val propertyNames: Seq[String] = Seq(
          ..${
            fieldMatchPolicy.namedProperties(objectType).map { property =>
                Lit.String(property.getName)
            }
    }
        )
        val allowedNames: Seq[Regex] = Seq(
          ..${
            fieldMatchPolicy.patternProperties(objectType).map { property =>
              val pattern = if (property.getName == "//")
                Lit.String("\"^.*$\"")
              else
                Lit.String(property.getPattern.toString)

              q"${pattern}.r"
            }
    }
        )
      }
     """

  def declareOwnerProperty(): Term.Param = {
    val tpe = typeFromName(s"${objectType.getDisplayName.getValue}.AdditionalProperties")
    val maybeOptional =
      if (spec.optional)
        Type.Apply(
          Type.Name("Option"),
          tpe :: Nil
        )
      else
        tpe

    Term.Param(
      Mod.ValParam() :: Nil,
      Term.Name(propertyName),
      Some(maybeOptional),
      Option(Term.Name("None")).filter(_ => spec.optional)
    )
  }

  def ownerCompanionSource(definition: DefnWithCompanion[Defn.Class]): List[Stat] =
    List[Stat](
      q"import scala.language.dynamics",
      definition.defn
    ) ::: definition.companion.toList
}
