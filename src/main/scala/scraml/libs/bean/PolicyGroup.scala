package scraml.libs.bean

import cats.data.Ior

import scraml.BeanProperties._

final class PolicyGroup private ()(implicit
    private val anyVal: AnyValSetting,
    private val array: ArraySetting,
    private val optional: OptionalSetting,
    private val scalaNumber: ScalaNumberSetting
) {
  import Analyzer.Coordinate
  import GenPolicy.Default
  import cats.syntax.option._
  import cats.syntax.semigroup._

  def apply(implicit coordinate: Coordinate): GenPolicy = {
    val scalars = whenAnyVal().orElse(whenScalaNumber())

    whenOptional {
      whenRefined {
        whenArray(scalars)
          .orElse(scalars)
          .getOrElse(Default)
      }
        .orElse(whenArray(scalars).filter(_.hasMapping))
        .orElse(scalars)
    }
      .orElse {
        whenRefined {
          whenArray(scalars)
            .orElse(scalars)
            .getOrElse(Default)
        }
          .orElse(whenArray(scalars))
          .orElse(scalars)
      }
      .getOrElse(Default)
  }

  private def when[SettingT](predicate: Boolean)(
      f: SettingT => Option[GenPolicy]
  )(implicit
      setting: SettingT
  ): Option[GenPolicy] =
    if (predicate)
      f(setting)
    else
      none[GenPolicy]

  private def whenAnyVal()(implicit
      coordinate: Coordinate
  ): Option[GenPolicy] =
    when[AnyValSetting](coordinate.anyVal) {
      case UseJavaLangTypes =>
        AnyValUseJavaLangPolicy.some

      case Unchanged =>
        none[GenPolicy]
    }

  private def whenArray(mapper: Option[GenPolicy])(implicit
      coordinate: Coordinate
  ): Option[ContainerPolicy] =
    when[ArraySetting](coordinate.array) {
      case UseJavaCollectionTypes =>
        ContainerPolicy(
          containerTransformer = Ior.Right(JavaCollectionPolicy),
          mapper = mapper
        ).some

      case Unchanged =>
        ContainerPolicy(
          containerTransformer = Ior.Left(
            GenSignature.scalaCollection
          ),
          mapper = mapper
        ).some

      case _ =>
        none[ContainerPolicy]
    }
      .collectFirst { case container: ContainerPolicy =>
        container
      }

  private def whenOptional(mapper: Option[GenPolicy])(implicit
      coordinate: Coordinate
  ): Option[GenPolicy] =
    when[OptionalSetting](coordinate.array && coordinate.optional) {
      case Unchanged if mapper.isEmpty =>
        ContainerPolicy(
          containerTransformer = Ior.Left(
            GenSignature.scalaOptional |+| GenSignature.scalaCollection
          ),
          mapper = mapper
        ).some

      case _ =>
        none[GenPolicy]
    }
      .orElse {
        when[OptionalSetting](coordinate.optional) {
          case UseNullableReturnType =>
            NullableOptionPolicy(mapper = mapper).some

          case UseJavaOptionalType =>
            ContainerPolicy(
              containerTransformer = Ior.Right(JavaOptionalPolicy),
              mapper = mapper
            ).some

          case Unchanged =>
            ContainerPolicy(
              containerTransformer = Ior.Left(
                GenSignature.scalaOptional
              ),
              mapper = mapper
            ).some
        }
      }

  private def whenRefined(policy: => GenPolicy)(implicit
      coordinate: Coordinate
  ): Option[GenPolicy] =
    if (coordinate.refined)
      RefinedPolicy(next = policy).some
    else
      none[GenPolicy]

  private def whenScalaNumber()(implicit
      coordinate: Coordinate
  ): Option[GenPolicy] =
    when[ScalaNumberSetting](coordinate.scalaNumber) {
      case UseJavaLangTypes =>
        ScalaNumberUseJavaLangPolicy.some

      case Unchanged =>
        none[GenPolicy]
    }
}

object PolicyGroup {
  def apply(settings: scraml.BeanProperties): PolicyGroup =
    settings match {
      case scraml.BeanProperties(
            _,
            array,
            _,
            UseNullableReturnType,
            scalaNumber
          ) =>
        new PolicyGroup()(
          anyVal = UseJavaLangTypes,
          array = array,
          optional = UseNullableReturnType,
          scalaNumber = scalaNumber
        )

      case scraml.BeanProperties(
            anyVal,
            array,
            _,
            optional,
            scalaNumber
          ) =>
        new PolicyGroup()(
          anyVal = anyVal,
          array = array,
          optional = optional,
          scalaNumber = scalaNumber
        )
    }
}
