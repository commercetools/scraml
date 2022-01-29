package scraml

import io.vrap.rmf.raml.model.types._


sealed trait FieldMatchPolicy {
  final def additionalProperties(objectType: ObjectType)(
    implicit context: ModelGenContext
  ): Option[AdditionalProperties] =
    if (areAdditionalPropertiesEnabled(objectType))
      Option(AdditionalProperties(objectType))
    else
      None

  def areAdditionalPropertiesEnabled(objectType: ObjectType)(
    implicit context: ModelGenContext
  ): Boolean =
    ModelGen.isMapType(objectType, context.anyTypeName).isEmpty &&
      Option(objectType.getAdditionalProperties).map(_.booleanValue())
        .getOrElse(true)

  def isPolicyFor(objectType: ObjectType): Boolean

  def isSingleton(objectType: ObjectType)(
    implicit context: ModelGenContext
  ): Boolean

  final def namedProperties(objectType: ObjectType): List[Property] =
    RMFUtil.typeProperties(objectType)
      .filterNot(isPatternProperty)
      .toList

  final def patternProperties(objectType: ObjectType): List[Property] =
    RMFUtil.typeProperties(objectType)
      .filter(isPatternProperty)
      .toList

  private def isPatternProperty(property: Property): Boolean =
    property.getName == "//" || Option(property.getPattern).isDefined
}

object FieldMatchPolicy {
  sealed trait SetBasedPolicy {
    this: FieldMatchPolicy =>

    val excluding: Set[String]

    override def isPolicyFor(objectType: ObjectType): Boolean =
      !Option(objectType.getDisplayName).map(_.getValue)
        .exists(excluding.contains)
  }

  final case class Exact(
    override val excluding: Set[String] = Set.empty
  ) extends FieldMatchPolicy with SetBasedPolicy {
    override def areAdditionalPropertiesEnabled(objectType: ObjectType)(
      implicit context: ModelGenContext
    ): Boolean =
      super.areAdditionalPropertiesEnabled(objectType) && !patternProperties(objectType).isEmpty

    override def isSingleton(objectType: ObjectType)(
      implicit context: ModelGenContext
    ): Boolean =
      !areAdditionalPropertiesEnabled(objectType) && namedProperties(objectType).isEmpty
  }

  object Exact {
    def unapply(policy: FieldMatchPolicy): Option[Exact] =
      policy match {
        case exact: Exact =>
          Some(exact)
        case _ =>
          None
      }
  }

  final case class IgnoreExtra(override val excluding: Set[String] = Set.empty)
    extends FieldMatchPolicy with SetBasedPolicy {
    override def areAdditionalPropertiesEnabled(objectType: ObjectType)(
        implicit context: ModelGenContext
    ): Boolean = false

    override def isSingleton(objectType: ObjectType)(
      implicit context: ModelGenContext
    ): Boolean =
      namedProperties(objectType).isEmpty
  }

  object IgnoreExtra {
    def unapply(policy: FieldMatchPolicy): Option[IgnoreExtra] =
      policy match {
        case ignore: IgnoreExtra =>
          Some(ignore)
        case _ =>
          None
      }
  }

  final case class KeepExtra(
    override val excluding: Set[String] = Set.empty
  ) extends FieldMatchPolicy with SetBasedPolicy {
    override def isSingleton(objectType: ObjectType)(
      implicit context: ModelGenContext
    ): Boolean = false
  }

  object KeepExtra {
    def unapply(policy: FieldMatchPolicy): Option[KeepExtra] =
      policy match {
        case keep: KeepExtra =>
          Some(keep)
        case _ =>
          None
      }
  }

  final case class MatchInOrder(policies: Seq[FieldMatchPolicy]) extends FieldMatchPolicy {
    override def isPolicyFor(objectType: ObjectType): Boolean =
      policies.find(_.isPolicyFor(objectType)).isDefined

    override def isSingleton(objectType: ObjectType)(
      implicit context: ModelGenContext
    ): Boolean =
      policyFor(objectType).isSingleton(objectType)

    override def areAdditionalPropertiesEnabled(objectType: ObjectType)(
      implicit context: ModelGenContext
    ): Boolean =
      policyFor(objectType).areAdditionalPropertiesEnabled(objectType)

    private def policyFor(objectType: ObjectType): FieldMatchPolicy =
      policies.find(_.isPolicyFor(objectType))
        .getOrElse(
          throw new RuntimeException("no field match policy found for: " + objectType.getName)
        )
  }

  def unapply(context: ModelGenContext): Option[FieldMatchPolicy] =
    context.params.fieldMatchPolicy match {
      case multi: MatchInOrder if multi.isPolicyFor(context.objectType) =>
        multi.policies.find(_.isPolicyFor(context.objectType))
      case exact: Exact if exact.isPolicyFor(context.objectType) =>
        Some(exact)
      case ignore: IgnoreExtra if ignore.isPolicyFor(context.objectType) =>
        Some(ignore)
      case keep: KeepExtra if keep.isPolicyFor(context.objectType) =>
        Some(keep)
      case _ =>
        None
    }
}