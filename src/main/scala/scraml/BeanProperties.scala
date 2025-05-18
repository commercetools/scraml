package scraml

final case class BeanProperties(
    anyVal: BeanProperties.AnyValSetting = BeanProperties.Unchanged,
    array: BeanProperties.ArraySetting = BeanProperties.Unchanged,
    evaluate: BeanProperties.EvaluationSetting = BeanProperties.Once,
    optional: BeanProperties.OptionalSetting = BeanProperties.Unchanged,
    scalaNumber: BeanProperties.ScalaNumberSetting = BeanProperties.Unchanged
)

object BeanProperties {
  sealed trait AnyValSetting

  sealed trait ArraySetting

  sealed trait EvaluationSetting

  sealed trait OptionalSetting

  sealed trait ScalaNumberSetting

  case object EveryInvocation extends EvaluationSetting

  case object Once extends EvaluationSetting

  case object Unchanged
      extends AnyValSetting
      with ArraySetting
      with OptionalSetting
      with ScalaNumberSetting

  case object UseJavaLangTypes extends AnyValSetting with ScalaNumberSetting

  case object UseJavaCollectionTypes extends ArraySetting

  case object UseJavaOptionalType extends OptionalSetting

  case object UseNullableReturnType extends OptionalSetting
}
