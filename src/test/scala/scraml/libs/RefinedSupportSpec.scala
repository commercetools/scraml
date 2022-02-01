package scraml.libs

import java.io.File

import cats.effect.unsafe.implicits.global
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import scraml.{DefaultModelGen, DefaultTypes, FieldMatchPolicy, ModelGenParams, ModelGenRunner}

class RefinedSupportSpec extends AnyWordSpec with Matchers with SourceCodeFormatting {
  "RefinedSupport" must {
    "modify case class refined property types (exact property matching)" in {
      val params = ModelGenParams(
        new File("src/sbt-test/sbt-scraml/refined/api/refined.raml"),
        new File("target/scraml-refined-test"),
        "scraml",
        FieldMatchPolicy.Exact(),
        DefaultTypes(),
        librarySupport = Set(CirceJsonSupport(), RefinedSupport),
        formatConfig = None,
        generateDateCreated = true
      )

      val generated = ModelGenRunner.run(DefaultModelGen)(params).unsafeRunSync()

      generated.files.nonEmpty should be(true)

      val theSource = generated.files
        .find(_.source.name == "DataType")
        .map(_.source.source.toString().stripTrailingSpaces)

      val theCompanion = generated.files
        .find(_.source.name == "DataType")
        .flatMap(_.source.companion)
        .map(_.toString().stripTrailingSpaces)

      theSource should be(
        Some(
          """final case class DataType(id: DataType.IdType, optionalCustomArrayTypeProp: DataType.OptionalCustomArrayTypePropType = None, foo: Option[String] = None, bar: DataType.BarType = None, numberProp: DataType.NumberPropType, customNumberProp: DataType.CustomNumberPropType, customArrayTypeProp: DataType.CustomArrayTypePropType, optionalStringArray: DataType.OptionalStringArrayType = None) extends BaseType"""
        )
      )

      theCompanion should be(
        Some(
          """object DataType {
            |  import io.circe._
            |  import io.circe.generic.semiauto._
            |  import io.circe.syntax._
            |  import io.circe.refined._
            |  implicit lazy val decoder: Decoder[DataType] = new Decoder[DataType] {
            |    def apply(c: HCursor): Decoder.Result[DataType] = {
            |      c.downField("id").as[String].flatMap { (_id: String) =>
            |        c.downField("optionalCustomArrayTypeProp").as[Option[Set[scala.math.BigDecimal]]].flatMap { (_optionalCustomArrayTypeProp: Option[Set[scala.math.BigDecimal]]) =>
            |          c.downField("foo").as[Option[String]].flatMap { (_foo: Option[String]) =>
            |            c.downField("bar").as[Option[String]].flatMap { (_bar: Option[String]) =>
            |              c.downField("numberProp").as[Float].flatMap { (_numberProp: Float) =>
            |                c.downField("customNumberProp").as[scala.math.BigDecimal].flatMap { (_customNumberProp: scala.math.BigDecimal) =>
            |                  c.downField("customArrayTypeProp").as[Vector[scala.math.BigDecimal]].flatMap { (_customArrayTypeProp: Vector[scala.math.BigDecimal]) =>
            |                    c.downField("optionalStringArray").as[Option[scala.collection.immutable.List[String]]].flatMap {
            |                      (_optionalStringArray: Option[scala.collection.immutable.List[String]]) => DataType.from(_id, _optionalCustomArrayTypeProp, _foo, _bar, _numberProp, _customNumberProp, _customArrayTypeProp, _optionalStringArray).swap.map(e => DecodingFailure(e.getMessage, Nil)).swap
            |                    }
            |                  }
            |                }
            |              }
            |            }
            |          }
            |        }
            |      }
            |    }
            |  }
			|  implicit lazy val encoder: Encoder[DataType] = deriveEncoder[DataType].mapJsonObject(_.+:("type" -> Json.fromString("data")))
            |  import eu.timepit.refined.api.Refined
            |  import eu.timepit.refined.boolean.And
            |  import eu.timepit.refined.collection._
            |  import eu.timepit.refined.numeric._
            |  import eu.timepit.refined.string._
            |  import shapeless.Witness
            |  type IdType = Refined[String, And[MinSize[Witness.`1`.T], And[MaxSize[Witness.`10`.T], MatchesRegex[Witness.`"^[A-z0-9-.]+$"`.T]]]]
            |  object IdType {
            |    import eu.timepit.refined.api._
            |    type ResultType = Refined[String, And[MinSize[Witness.`1`.T], And[MaxSize[Witness.`10`.T], MatchesRegex[Witness.`"^[A-z0-9-.]+$"`.T]]]]
            |    private val rt = RefinedType.apply[ResultType]
            |    def apply(candidate: String): Either[IllegalArgumentException, ResultType] = from(candidate)
            |    def from(candidate: String): Either[IllegalArgumentException, ResultType] = rt.refine(candidate).left.map(msg => new IllegalArgumentException(msg))
            |    def unapply(candidate: String): Option[ResultType] = from(candidate).toOption
            |    def unsafeFrom(candidate: String): ResultType = rt.unsafeRefine(candidate)
            |  }
            |  type OptionalCustomArrayTypePropItemPredicate = GreaterEqual[Witness.`0.0`.T]
            |  type OptionalCustomArrayTypePropType = Option[Refined[Set[scala.math.BigDecimal], And[MinSize[Witness.`1`.T], And[MaxSize[Witness.`100`.T], Forall[OptionalCustomArrayTypePropItemPredicate]]]]]
            |  object OptionalCustomArrayTypePropType {
            |    import eu.timepit.refined.api._
            |    type ResultType = Refined[Set[scala.math.BigDecimal], And[MinSize[Witness.`1`.T], And[MaxSize[Witness.`100`.T], Forall[OptionalCustomArrayTypePropItemPredicate]]]]
            |    private val rt = RefinedType.apply[ResultType]
            |    def apply(candidate: Set[scala.math.BigDecimal]): Either[IllegalArgumentException, Option[ResultType]] = from(Option(candidate))
            |    def apply(candidate: Option[Set[scala.math.BigDecimal]]): Either[IllegalArgumentException, Option[ResultType]] = from(candidate)
            |    def from(candidate: Option[Set[scala.math.BigDecimal]]): Either[IllegalArgumentException, Option[ResultType]] = candidate match {
            |      case Some(value) =>
            |        rt.refine(value).map(Some(_)).left.map(msg => new IllegalArgumentException(msg))
            |      case None =>
            |        Right(None)
            |    }
            |    def unapply(candidate: Option[Set[scala.math.BigDecimal]]): Option[ResultType] = from(candidate).fold(_ => None, a => a)
            |    def unsafeFrom(candidate: Option[Set[scala.math.BigDecimal]]): Option[ResultType] = candidate.map(rt.unsafeRefine)
            |  }
            |  type BarType = Option[Refined[String, MatchesRegex[Witness.`"^[A-z]+$"`.T]]]
            |  object BarType {
            |    import eu.timepit.refined.api._
            |    type ResultType = Refined[String, MatchesRegex[Witness.`"^[A-z]+$"`.T]]
            |    private val rt = RefinedType.apply[ResultType]
            |    def apply(candidate: String): Either[IllegalArgumentException, Option[ResultType]] = from(Option(candidate))
            |    def apply(candidate: Option[String]): Either[IllegalArgumentException, Option[ResultType]] = from(candidate)
            |    def from(candidate: Option[String]): Either[IllegalArgumentException, Option[ResultType]] = candidate match {
            |      case Some(value) =>
            |        rt.refine(value).map(Some(_)).left.map(msg => new IllegalArgumentException(msg))
            |      case None =>
            |        Right(None)
            |    }
            |    def unapply(candidate: Option[String]): Option[ResultType] = from(candidate).fold(_ => None, a => a)
            |    def unsafeFrom(candidate: Option[String]): Option[ResultType] = candidate.map(rt.unsafeRefine)
            |  }
            |  type NumberPropType = Refined[Float, Interval.Closed[Witness.`0`.T, Witness.`99.99999`.T]]
            |  object NumberPropType {
            |    import eu.timepit.refined.api._
            |    type ResultType = Refined[Float, Interval.Closed[Witness.`0`.T, Witness.`99.99999`.T]]
            |    private val rt = RefinedType.apply[ResultType]
            |    def apply(candidate: Float): Either[IllegalArgumentException, ResultType] = from(candidate)
            |    def from(candidate: Float): Either[IllegalArgumentException, ResultType] = rt.refine(candidate).left.map(msg => new IllegalArgumentException(msg))
            |    def unapply(candidate: Float): Option[ResultType] = from(candidate).toOption
            |    def unsafeFrom(candidate: Float): ResultType = rt.unsafeRefine(candidate)
            |  }
            |  type CustomNumberPropType = Refined[scala.math.BigDecimal, LessEqual[Witness.`99.99999`.T]]
            |  object CustomNumberPropType {
            |    import eu.timepit.refined.api._
            |    type ResultType = Refined[scala.math.BigDecimal, LessEqual[Witness.`99.99999`.T]]
            |    private val rt = RefinedType.apply[ResultType]
            |    def apply(candidate: scala.math.BigDecimal): Either[IllegalArgumentException, ResultType] = from(candidate)
            |    def from(candidate: scala.math.BigDecimal): Either[IllegalArgumentException, ResultType] = rt.refine(candidate).left.map(msg => new IllegalArgumentException(msg))
            |    def unapply(candidate: scala.math.BigDecimal): Option[ResultType] = from(candidate).toOption
            |    def unsafeFrom(candidate: scala.math.BigDecimal): ResultType = rt.unsafeRefine(candidate)
            |  }
            |  type CustomArrayTypePropItemPredicate = Interval.Closed[Witness.`1.23`.T, Witness.`4.56`.T]
            |  type CustomArrayTypePropType = Refined[Vector[scala.math.BigDecimal], And[MaxSize[Witness.`100`.T], Forall[CustomArrayTypePropItemPredicate]]]
            |  object CustomArrayTypePropType {
            |    import eu.timepit.refined.api._
            |    type ResultType = Refined[Vector[scala.math.BigDecimal], And[MaxSize[Witness.`100`.T], Forall[CustomArrayTypePropItemPredicate]]]
            |    private val rt = RefinedType.apply[ResultType]
            |    def apply(candidate: Vector[scala.math.BigDecimal]): Either[IllegalArgumentException, ResultType] = from(candidate)
            |    def from(candidate: Vector[scala.math.BigDecimal]): Either[IllegalArgumentException, ResultType] = rt.refine(candidate).left.map(msg => new IllegalArgumentException(msg))
            |    def unapply(candidate: Vector[scala.math.BigDecimal]): Option[ResultType] = from(candidate).toOption
            |    def unsafeFrom(candidate: Vector[scala.math.BigDecimal]): ResultType = rt.unsafeRefine(candidate)
            |  }
            |  type OptionalStringArrayItemPredicate = And[MinSize[Witness.`2`.T], And[MaxSize[Witness.`42`.T], MatchesRegex[Witness.`"^[A-z0-9]+$"`.T]]]
            |  type OptionalStringArrayType = Option[Refined[scala.collection.immutable.List[String], Forall[OptionalStringArrayItemPredicate]]]
            |  object OptionalStringArrayType {
            |    import eu.timepit.refined.api._
            |    type ResultType = Refined[scala.collection.immutable.List[String], Forall[OptionalStringArrayItemPredicate]]
            |    private val rt = RefinedType.apply[ResultType]
            |    def apply(candidate: scala.collection.immutable.List[String]): Either[IllegalArgumentException, Option[ResultType]] = from(Option(candidate))
            |    def apply(candidate: Option[scala.collection.immutable.List[String]]): Either[IllegalArgumentException, Option[ResultType]] = from(candidate)
            |    def from(candidate: Option[scala.collection.immutable.List[String]]): Either[IllegalArgumentException, Option[ResultType]] = candidate match {
            |      case Some(value) =>
            |        rt.refine(value).map(Some(_)).left.map(msg => new IllegalArgumentException(msg))
            |      case None =>
            |        Right(None)
            |    }
            |    def unapply(candidate: Option[scala.collection.immutable.List[String]]): Option[ResultType] = from(candidate).fold(_ => None, a => a)
            |    def unsafeFrom(candidate: Option[scala.collection.immutable.List[String]]): Option[ResultType] = candidate.map(rt.unsafeRefine)
            |  }
            |  def from(id: String, optionalCustomArrayTypeProp: Option[Set[scala.math.BigDecimal]] = None, foo: Option[String] = None, bar: Option[String] = None, numberProp: Float, customNumberProp: scala.math.BigDecimal, customArrayTypeProp: Vector[scala.math.BigDecimal] = Vector.empty, optionalStringArray: Option[scala.collection.immutable.List[String]] = None): Either[IllegalArgumentException, DataType] = {
            |    val _id = IdType.from(id)
            |    val _optionalCustomArrayTypeProp = OptionalCustomArrayTypePropType.from(optionalCustomArrayTypeProp)
            |    val _foo = Right(foo)
            |    val _bar = BarType.from(bar)
            |    val _numberProp = NumberPropType.from(numberProp)
            |    val _customNumberProp = CustomNumberPropType.from(customNumberProp)
            |    val _customArrayTypeProp = CustomArrayTypePropType.from(customArrayTypeProp)
            |    val _optionalStringArray = OptionalStringArrayType.from(optionalStringArray)
            |    _id.flatMap { (__id: IdType) =>
            |      _optionalCustomArrayTypeProp.flatMap { (__optionalCustomArrayTypeProp: OptionalCustomArrayTypePropType) =>
            |        _foo.flatMap { (__foo: Option[String]) =>
            |          _bar.flatMap { (__bar: BarType) =>
            |            _numberProp.flatMap { (__numberProp: NumberPropType) =>
            |              _customNumberProp.flatMap { (__customNumberProp: CustomNumberPropType) =>
            |                _customArrayTypeProp.flatMap { (__customArrayTypeProp: CustomArrayTypePropType) =>
            |                  _optionalStringArray.map {
            |                    (__optionalStringArray: OptionalStringArrayType) => DataType(__id, __optionalCustomArrayTypeProp, __foo, __bar, __numberProp, __customNumberProp, __customArrayTypeProp, __optionalStringArray)
            |                  }
            |                }
            |              }
            |            }
            |          }
            |        }
            |      }
            |    }
            |  }
            |}""".stripMargin.stripTrailingSpaces
        )
      )
    }

    "modify case class refined property types (ignore extra property matching)" in {
      val params = ModelGenParams(
        new File("src/sbt-test/sbt-scraml/refined/api/refined.raml"),
        new File("target/scraml-refined-test"),
        "scraml",
        FieldMatchPolicy.IgnoreExtra(),
        DefaultTypes(),
        librarySupport = Set(CirceJsonSupport(), RefinedSupport),
        formatConfig = None,
        generateDateCreated = true
      )

      val generated = ModelGenRunner.run(DefaultModelGen)(params).unsafeRunSync()

      generated.files.nonEmpty should be(true)

      val theSource = generated.files
        .find(_.source.name == "DataType")
        .map(_.source.source.toString().stripTrailingSpaces)

      val theCompanion = generated.files
        .find(_.source.name == "DataType")
        .flatMap(_.source.companion)
        .map(_.toString().stripTrailingSpaces)

      theSource.map(_.toString().stripTrailingSpaces) should be(
        Some(
          """final case class DataType(id: DataType.IdType, optionalCustomArrayTypeProp: DataType.OptionalCustomArrayTypePropType = None, foo: Option[String] = None, bar: DataType.BarType = None, numberProp: DataType.NumberPropType, customNumberProp: DataType.CustomNumberPropType, customArrayTypeProp: DataType.CustomArrayTypePropType, optionalStringArray: DataType.OptionalStringArrayType = None) extends BaseType"""
        )
      )

      theCompanion.map(_.toString().stripTrailingSpaces) should be(
        Some(
          """object DataType {
            |  import io.circe._
            |  import io.circe.generic.semiauto._
            |  import io.circe.syntax._
            |  import io.circe.refined._
            |  implicit lazy val decoder: Decoder[DataType] = new Decoder[DataType] {
            |    def apply(c: HCursor): Decoder.Result[DataType] = {
            |      c.downField("id").as[String].flatMap { (_id: String) =>
            |        c.downField("optionalCustomArrayTypeProp").as[Option[Set[scala.math.BigDecimal]]].flatMap { (_optionalCustomArrayTypeProp: Option[Set[scala.math.BigDecimal]]) =>
            |          c.downField("foo").as[Option[String]].flatMap { (_foo: Option[String]) =>
            |            c.downField("bar").as[Option[String]].flatMap { (_bar: Option[String]) =>
            |              c.downField("numberProp").as[Float].flatMap { (_numberProp: Float) =>
            |                c.downField("customNumberProp").as[scala.math.BigDecimal].flatMap { (_customNumberProp: scala.math.BigDecimal) =>
            |                  c.downField("customArrayTypeProp").as[Vector[scala.math.BigDecimal]].flatMap { (_customArrayTypeProp: Vector[scala.math.BigDecimal]) =>
            |                    c.downField("optionalStringArray").as[Option[scala.collection.immutable.List[String]]].flatMap {
            |                      (_optionalStringArray: Option[scala.collection.immutable.List[String]]) => DataType.from(_id, _optionalCustomArrayTypeProp, _foo, _bar, _numberProp, _customNumberProp, _customArrayTypeProp, _optionalStringArray).swap.map(e => DecodingFailure(e.getMessage, Nil)).swap
            |                    }
            |                  }
            |                }
            |              }
            |            }
            |          }
            |        }
            |      }
            |    }
            |  }
            |  implicit lazy val encoder: Encoder[DataType] = new Encoder[DataType] { final def apply(instance: DataType): Json = Json.obj("id" -> instance.id.asJson, "optionalCustomArrayTypeProp" -> instance.optionalCustomArrayTypeProp.asJson, "foo" -> instance.foo.asJson, "bar" -> instance.bar.asJson, "numberProp" -> instance.numberProp.asJson, "customNumberProp" -> instance.customNumberProp.asJson, "customArrayTypeProp" -> instance.customArrayTypeProp.asJson, "optionalStringArray" -> instance.optionalStringArray.asJson) }
            |  import eu.timepit.refined.api.Refined
            |  import eu.timepit.refined.boolean.And
            |  import eu.timepit.refined.collection._
            |  import eu.timepit.refined.numeric._
            |  import eu.timepit.refined.string._
            |  import shapeless.Witness
            |  type IdType = Refined[String, And[MinSize[Witness.`1`.T], And[MaxSize[Witness.`10`.T], MatchesRegex[Witness.`"^[A-z0-9-.]+$"`.T]]]]
            |  object IdType {
            |    import eu.timepit.refined.api._
            |    type ResultType = Refined[String, And[MinSize[Witness.`1`.T], And[MaxSize[Witness.`10`.T], MatchesRegex[Witness.`"^[A-z0-9-.]+$"`.T]]]]
            |    private val rt = RefinedType.apply[ResultType]
            |    def apply(candidate: String): Either[IllegalArgumentException, ResultType] = from(candidate)
            |    def from(candidate: String): Either[IllegalArgumentException, ResultType] = rt.refine(candidate).left.map(msg => new IllegalArgumentException(msg))
            |    def unapply(candidate: String): Option[ResultType] = from(candidate).toOption
            |    def unsafeFrom(candidate: String): ResultType = rt.unsafeRefine(candidate)
            |  }
            |  type OptionalCustomArrayTypePropItemPredicate = GreaterEqual[Witness.`0.0`.T]
            |  type OptionalCustomArrayTypePropType = Option[Refined[Set[scala.math.BigDecimal], And[MinSize[Witness.`1`.T], And[MaxSize[Witness.`100`.T], Forall[OptionalCustomArrayTypePropItemPredicate]]]]]
            |  object OptionalCustomArrayTypePropType {
            |    import eu.timepit.refined.api._
            |    type ResultType = Refined[Set[scala.math.BigDecimal], And[MinSize[Witness.`1`.T], And[MaxSize[Witness.`100`.T], Forall[OptionalCustomArrayTypePropItemPredicate]]]]
            |    private val rt = RefinedType.apply[ResultType]
            |    def apply(candidate: Set[scala.math.BigDecimal]): Either[IllegalArgumentException, Option[ResultType]] = from(Option(candidate))
            |    def apply(candidate: Option[Set[scala.math.BigDecimal]]): Either[IllegalArgumentException, Option[ResultType]] = from(candidate)
            |    def from(candidate: Option[Set[scala.math.BigDecimal]]): Either[IllegalArgumentException, Option[ResultType]] = candidate match {
            |      case Some(value) =>
            |        rt.refine(value).map(Some(_)).left.map(msg => new IllegalArgumentException(msg))
            |      case None =>
            |        Right(None)
            |    }
            |    def unapply(candidate: Option[Set[scala.math.BigDecimal]]): Option[ResultType] = from(candidate).fold(_ => None, a => a)
            |    def unsafeFrom(candidate: Option[Set[scala.math.BigDecimal]]): Option[ResultType] = candidate.map(rt.unsafeRefine)
            |  }
            |  type BarType = Option[Refined[String, MatchesRegex[Witness.`"^[A-z]+$"`.T]]]
            |  object BarType {
            |    import eu.timepit.refined.api._
            |    type ResultType = Refined[String, MatchesRegex[Witness.`"^[A-z]+$"`.T]]
            |    private val rt = RefinedType.apply[ResultType]
            |    def apply(candidate: String): Either[IllegalArgumentException, Option[ResultType]] = from(Option(candidate))
            |    def apply(candidate: Option[String]): Either[IllegalArgumentException, Option[ResultType]] = from(candidate)
            |    def from(candidate: Option[String]): Either[IllegalArgumentException, Option[ResultType]] = candidate match {
            |      case Some(value) =>
            |        rt.refine(value).map(Some(_)).left.map(msg => new IllegalArgumentException(msg))
            |      case None =>
            |        Right(None)
            |    }
            |    def unapply(candidate: Option[String]): Option[ResultType] = from(candidate).fold(_ => None, a => a)
            |    def unsafeFrom(candidate: Option[String]): Option[ResultType] = candidate.map(rt.unsafeRefine)
            |  }
            |  type NumberPropType = Refined[Float, Interval.Closed[Witness.`0`.T, Witness.`99.99999`.T]]
            |  object NumberPropType {
            |    import eu.timepit.refined.api._
            |    type ResultType = Refined[Float, Interval.Closed[Witness.`0`.T, Witness.`99.99999`.T]]
            |    private val rt = RefinedType.apply[ResultType]
            |    def apply(candidate: Float): Either[IllegalArgumentException, ResultType] = from(candidate)
            |    def from(candidate: Float): Either[IllegalArgumentException, ResultType] = rt.refine(candidate).left.map(msg => new IllegalArgumentException(msg))
            |    def unapply(candidate: Float): Option[ResultType] = from(candidate).toOption
            |    def unsafeFrom(candidate: Float): ResultType = rt.unsafeRefine(candidate)
            |  }
            |  type CustomNumberPropType = Refined[scala.math.BigDecimal, LessEqual[Witness.`99.99999`.T]]
            |  object CustomNumberPropType {
            |    import eu.timepit.refined.api._
            |    type ResultType = Refined[scala.math.BigDecimal, LessEqual[Witness.`99.99999`.T]]
            |    private val rt = RefinedType.apply[ResultType]
            |    def apply(candidate: scala.math.BigDecimal): Either[IllegalArgumentException, ResultType] = from(candidate)
            |    def from(candidate: scala.math.BigDecimal): Either[IllegalArgumentException, ResultType] = rt.refine(candidate).left.map(msg => new IllegalArgumentException(msg))
            |    def unapply(candidate: scala.math.BigDecimal): Option[ResultType] = from(candidate).toOption
            |    def unsafeFrom(candidate: scala.math.BigDecimal): ResultType = rt.unsafeRefine(candidate)
            |  }
            |  type CustomArrayTypePropItemPredicate = Interval.Closed[Witness.`1.23`.T, Witness.`4.56`.T]
            |  type CustomArrayTypePropType = Refined[Vector[scala.math.BigDecimal], And[MaxSize[Witness.`100`.T], Forall[CustomArrayTypePropItemPredicate]]]
            |  object CustomArrayTypePropType {
            |    import eu.timepit.refined.api._
            |    type ResultType = Refined[Vector[scala.math.BigDecimal], And[MaxSize[Witness.`100`.T], Forall[CustomArrayTypePropItemPredicate]]]
            |    private val rt = RefinedType.apply[ResultType]
            |    def apply(candidate: Vector[scala.math.BigDecimal]): Either[IllegalArgumentException, ResultType] = from(candidate)
            |    def from(candidate: Vector[scala.math.BigDecimal]): Either[IllegalArgumentException, ResultType] = rt.refine(candidate).left.map(msg => new IllegalArgumentException(msg))
            |    def unapply(candidate: Vector[scala.math.BigDecimal]): Option[ResultType] = from(candidate).toOption
            |    def unsafeFrom(candidate: Vector[scala.math.BigDecimal]): ResultType = rt.unsafeRefine(candidate)
            |  }
            |  type OptionalStringArrayItemPredicate = And[MinSize[Witness.`2`.T], And[MaxSize[Witness.`42`.T], MatchesRegex[Witness.`"^[A-z0-9]+$"`.T]]]
            |  type OptionalStringArrayType = Option[Refined[scala.collection.immutable.List[String], Forall[OptionalStringArrayItemPredicate]]]
            |  object OptionalStringArrayType {
            |    import eu.timepit.refined.api._
            |    type ResultType = Refined[scala.collection.immutable.List[String], Forall[OptionalStringArrayItemPredicate]]
            |    private val rt = RefinedType.apply[ResultType]
            |    def apply(candidate: scala.collection.immutable.List[String]): Either[IllegalArgumentException, Option[ResultType]] = from(Option(candidate))
            |    def apply(candidate: Option[scala.collection.immutable.List[String]]): Either[IllegalArgumentException, Option[ResultType]] = from(candidate)
            |    def from(candidate: Option[scala.collection.immutable.List[String]]): Either[IllegalArgumentException, Option[ResultType]] = candidate match {
            |      case Some(value) =>
            |        rt.refine(value).map(Some(_)).left.map(msg => new IllegalArgumentException(msg))
            |      case None =>
            |        Right(None)
            |    }
            |    def unapply(candidate: Option[scala.collection.immutable.List[String]]): Option[ResultType] = from(candidate).fold(_ => None, a => a)
            |    def unsafeFrom(candidate: Option[scala.collection.immutable.List[String]]): Option[ResultType] = candidate.map(rt.unsafeRefine)
            |  }
            |  def from(id: String, optionalCustomArrayTypeProp: Option[Set[scala.math.BigDecimal]] = None, foo: Option[String] = None, bar: Option[String] = None, numberProp: Float, customNumberProp: scala.math.BigDecimal, customArrayTypeProp: Vector[scala.math.BigDecimal] = Vector.empty, optionalStringArray: Option[scala.collection.immutable.List[String]] = None): Either[IllegalArgumentException, DataType] = {
            |    val _id = IdType.from(id)
            |    val _optionalCustomArrayTypeProp = OptionalCustomArrayTypePropType.from(optionalCustomArrayTypeProp)
            |    val _foo = Right(foo)
            |    val _bar = BarType.from(bar)
            |    val _numberProp = NumberPropType.from(numberProp)
            |    val _customNumberProp = CustomNumberPropType.from(customNumberProp)
            |    val _customArrayTypeProp = CustomArrayTypePropType.from(customArrayTypeProp)
            |    val _optionalStringArray = OptionalStringArrayType.from(optionalStringArray)
            |    _id.flatMap { (__id: IdType) =>
            |      _optionalCustomArrayTypeProp.flatMap { (__optionalCustomArrayTypeProp: OptionalCustomArrayTypePropType) =>
            |        _foo.flatMap { (__foo: Option[String]) =>
            |          _bar.flatMap { (__bar: BarType) =>
            |            _numberProp.flatMap { (__numberProp: NumberPropType) =>
            |              _customNumberProp.flatMap { (__customNumberProp: CustomNumberPropType) =>
            |                _customArrayTypeProp.flatMap { (__customArrayTypeProp: CustomArrayTypePropType) =>
            |                  _optionalStringArray.map {
            |                    (__optionalStringArray: OptionalStringArrayType) => DataType(__id, __optionalCustomArrayTypeProp, __foo, __bar, __numberProp, __customNumberProp, __customArrayTypeProp, __optionalStringArray)
            |                  }
            |                }
            |              }
            |            }
            |          }
            |        }
            |      }
            |    }
            |  }
            |}""".stripMargin.stripTrailingSpaces
        )
      )
    }

    "modify case class refined property types (keep extra property matching)" in {
      val params = ModelGenParams(
        new File("src/sbt-test/sbt-scraml/refined/api/refined.raml"),
        new File("target/scraml-refined-test"),
        "scraml",
        FieldMatchPolicy.KeepExtra(),
        DefaultTypes(),
        librarySupport = Set(CirceJsonSupport(), RefinedSupport),
        formatConfig = None,
        generateDateCreated = true
      )

      val generated = ModelGenRunner.run(DefaultModelGen)(params).unsafeRunSync()

      generated.files.nonEmpty should be(true)

      val theSource = generated.files
        .find(_.source.name == "DataType")
        .map(_.source.source.toString().stripTrailingSpaces)

      val theCompanion = generated.files
        .find(_.source.name == "DataType")
        .flatMap(_.source.companion)
        .map(_.toString().stripTrailingSpaces)

      theSource.map(_.toString().stripTrailingSpaces) should be(
        Some(
          """final case class DataType(id: DataType.IdType, optionalCustomArrayTypeProp: DataType.OptionalCustomArrayTypePropType = None, foo: Option[String] = None, bar: DataType.BarType = None, numberProp: DataType.NumberPropType, customNumberProp: DataType.CustomNumberPropType, customArrayTypeProp: DataType.CustomArrayTypePropType, optionalStringArray: DataType.OptionalStringArrayType = None)(val additionalProperties: Option[DataType.AdditionalProperties] = None) extends BaseType"""
        )
      )

      theCompanion.map(_.toString().stripTrailingSpaces) should be(
        Some(
          """object DataType {
            |  import scala.language.dynamics
            |  final case class AdditionalProperties(private val underlying: scala.collection.immutable.Map[String, io.circe.Json]) extends scala.Dynamic {
            |    override def toString(): String = underlying.mkString(", ")
            |    def selectDynamic(field: String): Option[io.circe.Json] = underlying.get(field)
            |    def getOrElse[V >: io.circe.Json](key: String, default: => V): V = underlying.getOrElse(key, default)
            |    def isDefinedAt(key: String): Boolean = underlying.isDefinedAt(key)
            |    def isEmpty: Boolean = underlying.isEmpty
            |    def keySet: Set[String] = underlying.keySet
            |    def keys: Iterable[String] = underlying.keys
            |    def keysIterator: Iterator[String] = underlying.keysIterator
            |    def nonEmpty: Boolean = !underlying.isEmpty
            |    def size: Int = underlying.size
            |    def values: Iterable[io.circe.Json] = underlying.values
            |    def valuesIterator: Iterator[io.circe.Json] = underlying.valuesIterator
            |  }
            |  object AdditionalProperties {
            |    import scala.util.matching.Regex
            |    val propertyNames: Seq[String] = Seq("id", "optionalCustomArrayTypeProp", "foo", "bar", "numberProp", "customNumberProp", "customArrayTypeProp", "optionalStringArray")
            |    val allowedNames: Seq[Regex] = Seq()
            |    import io.circe._
            |    import io.circe.generic.semiauto._
            |    implicit lazy val decoder: Decoder[AdditionalProperties] = new Decoder[AdditionalProperties] {
            |      final def apply(c: HCursor): Decoder.Result[AdditionalProperties] = {
            |        val parent = c.up
            |        val allKeys = parent.keys.fold(Set.empty[String])(_.toSet)
            |        val builder = scala.collection.immutable.Map.newBuilder[String, Json]
            |        val it = allKeys.filterNot(propertyNames.contains).iterator
            |        val entries = it.foldLeft(builder)({
            |          case (accum, key) =>
            |            parent.field(key).focus.fold(accum) {
            |              v => accum += key -> v
            |            }
            |        })
            |        Right(AdditionalProperties(builder.result()))
            |      }
            |    }
            |    def merge(into: Json, oap: Option[AdditionalProperties]): Json = {
            |      oap.fold(into)(merge(into, _))
            |    }
            |    def merge(into: Json, ap: AdditionalProperties): Json = {
            |      Json.fromFields(ap.underlying).deepMerge(into)
            |    }
            |  }
            |  import io.circe._
            |  import io.circe.generic.semiauto._
            |  import io.circe.syntax._
            |  import io.circe.refined._
            |  implicit lazy val decoder: Decoder[DataType] = new Decoder[DataType] {
            |    def apply(c: HCursor): Decoder.Result[DataType] = {
            |      c.downField("id").as[String].flatMap { (_id: String) =>
            |        c.downField("optionalCustomArrayTypeProp").as[Option[Set[scala.math.BigDecimal]]].flatMap { (_optionalCustomArrayTypeProp: Option[Set[scala.math.BigDecimal]]) =>
            |          c.downField("foo").as[Option[String]].flatMap { (_foo: Option[String]) =>
            |            c.downField("bar").as[Option[String]].flatMap { (_bar: Option[String]) =>
            |              c.downField("numberProp").as[Float].flatMap { (_numberProp: Float) =>
            |                c.downField("customNumberProp").as[scala.math.BigDecimal].flatMap { (_customNumberProp: scala.math.BigDecimal) =>
            |                  c.downField("customArrayTypeProp").as[Vector[scala.math.BigDecimal]].flatMap { (_customArrayTypeProp: Vector[scala.math.BigDecimal]) =>
            |                    c.downField("optionalStringArray").as[Option[scala.collection.immutable.List[String]]].flatMap { (_optionalStringArray: Option[scala.collection.immutable.List[String]]) =>
            |                      c.downField("additionalProperties").as[Option[DataType.AdditionalProperties]].flatMap {
            |                        (_additionalProperties: Option[DataType.AdditionalProperties]) => DataType.from(_id, _optionalCustomArrayTypeProp, _foo, _bar, _numberProp, _customNumberProp, _customArrayTypeProp, _optionalStringArray, _additionalProperties).swap.map(e => DecodingFailure(e.getMessage, Nil)).swap
            |                      }
            |                    }
            |                  }
            |                }
            |              }
            |            }
            |          }
            |        }
            |      }
            |    }
            |  }
            |  implicit lazy val encoder: Encoder[DataType] = new Encoder[DataType] { final def apply(instance: DataType): Json = AdditionalProperties.merge(Json.obj("type" -> Json.fromString("data"), "id" -> instance.id.asJson, "optionalCustomArrayTypeProp" -> instance.optionalCustomArrayTypeProp.asJson, "foo" -> instance.foo.asJson, "bar" -> instance.bar.asJson, "numberProp" -> instance.numberProp.asJson, "customNumberProp" -> instance.customNumberProp.asJson, "customArrayTypeProp" -> instance.customArrayTypeProp.asJson, "optionalStringArray" -> instance.optionalStringArray.asJson), instance.additionalProperties) }
            |  import eu.timepit.refined.api.Refined
            |  import eu.timepit.refined.boolean.And
            |  import eu.timepit.refined.collection._
            |  import eu.timepit.refined.numeric._
            |  import eu.timepit.refined.string._
            |  import shapeless.Witness
            |  type IdType = Refined[String, And[MinSize[Witness.`1`.T], And[MaxSize[Witness.`10`.T], MatchesRegex[Witness.`"^[A-z0-9-.]+$"`.T]]]]
            |  object IdType {
            |    import eu.timepit.refined.api._
            |    type ResultType = Refined[String, And[MinSize[Witness.`1`.T], And[MaxSize[Witness.`10`.T], MatchesRegex[Witness.`"^[A-z0-9-.]+$"`.T]]]]
            |    private val rt = RefinedType.apply[ResultType]
            |    def apply(candidate: String): Either[IllegalArgumentException, ResultType] = from(candidate)
            |    def from(candidate: String): Either[IllegalArgumentException, ResultType] = rt.refine(candidate).left.map(msg => new IllegalArgumentException(msg))
            |    def unapply(candidate: String): Option[ResultType] = from(candidate).toOption
            |    def unsafeFrom(candidate: String): ResultType = rt.unsafeRefine(candidate)
            |  }
            |  type OptionalCustomArrayTypePropItemPredicate = GreaterEqual[Witness.`0.0`.T]
            |  type OptionalCustomArrayTypePropType = Option[Refined[Set[scala.math.BigDecimal], And[MinSize[Witness.`1`.T], And[MaxSize[Witness.`100`.T], Forall[OptionalCustomArrayTypePropItemPredicate]]]]]
            |  object OptionalCustomArrayTypePropType {
            |    import eu.timepit.refined.api._
            |    type ResultType = Refined[Set[scala.math.BigDecimal], And[MinSize[Witness.`1`.T], And[MaxSize[Witness.`100`.T], Forall[OptionalCustomArrayTypePropItemPredicate]]]]
            |    private val rt = RefinedType.apply[ResultType]
            |    def apply(candidate: Set[scala.math.BigDecimal]): Either[IllegalArgumentException, Option[ResultType]] = from(Option(candidate))
            |    def apply(candidate: Option[Set[scala.math.BigDecimal]]): Either[IllegalArgumentException, Option[ResultType]] = from(candidate)
            |    def from(candidate: Option[Set[scala.math.BigDecimal]]): Either[IllegalArgumentException, Option[ResultType]] = candidate match {
            |      case Some(value) =>
            |        rt.refine(value).map(Some(_)).left.map(msg => new IllegalArgumentException(msg))
            |      case None =>
            |        Right(None)
            |    }
            |    def unapply(candidate: Option[Set[scala.math.BigDecimal]]): Option[ResultType] = from(candidate).fold(_ => None, a => a)
            |    def unsafeFrom(candidate: Option[Set[scala.math.BigDecimal]]): Option[ResultType] = candidate.map(rt.unsafeRefine)
            |  }
            |  type BarType = Option[Refined[String, MatchesRegex[Witness.`"^[A-z]+$"`.T]]]
            |  object BarType {
            |    import eu.timepit.refined.api._
            |    type ResultType = Refined[String, MatchesRegex[Witness.`"^[A-z]+$"`.T]]
            |    private val rt = RefinedType.apply[ResultType]
            |    def apply(candidate: String): Either[IllegalArgumentException, Option[ResultType]] = from(Option(candidate))
            |    def apply(candidate: Option[String]): Either[IllegalArgumentException, Option[ResultType]] = from(candidate)
            |    def from(candidate: Option[String]): Either[IllegalArgumentException, Option[ResultType]] = candidate match {
            |      case Some(value) =>
            |        rt.refine(value).map(Some(_)).left.map(msg => new IllegalArgumentException(msg))
            |      case None =>
            |        Right(None)
            |    }
            |    def unapply(candidate: Option[String]): Option[ResultType] = from(candidate).fold(_ => None, a => a)
            |    def unsafeFrom(candidate: Option[String]): Option[ResultType] = candidate.map(rt.unsafeRefine)
            |  }
            |  type NumberPropType = Refined[Float, Interval.Closed[Witness.`0`.T, Witness.`99.99999`.T]]
            |  object NumberPropType {
            |    import eu.timepit.refined.api._
            |    type ResultType = Refined[Float, Interval.Closed[Witness.`0`.T, Witness.`99.99999`.T]]
            |    private val rt = RefinedType.apply[ResultType]
            |    def apply(candidate: Float): Either[IllegalArgumentException, ResultType] = from(candidate)
            |    def from(candidate: Float): Either[IllegalArgumentException, ResultType] = rt.refine(candidate).left.map(msg => new IllegalArgumentException(msg))
            |    def unapply(candidate: Float): Option[ResultType] = from(candidate).toOption
            |    def unsafeFrom(candidate: Float): ResultType = rt.unsafeRefine(candidate)
            |  }
            |  type CustomNumberPropType = Refined[scala.math.BigDecimal, LessEqual[Witness.`99.99999`.T]]
            |  object CustomNumberPropType {
            |    import eu.timepit.refined.api._
            |    type ResultType = Refined[scala.math.BigDecimal, LessEqual[Witness.`99.99999`.T]]
            |    private val rt = RefinedType.apply[ResultType]
            |    def apply(candidate: scala.math.BigDecimal): Either[IllegalArgumentException, ResultType] = from(candidate)
            |    def from(candidate: scala.math.BigDecimal): Either[IllegalArgumentException, ResultType] = rt.refine(candidate).left.map(msg => new IllegalArgumentException(msg))
            |    def unapply(candidate: scala.math.BigDecimal): Option[ResultType] = from(candidate).toOption
            |    def unsafeFrom(candidate: scala.math.BigDecimal): ResultType = rt.unsafeRefine(candidate)
            |  }
            |  type CustomArrayTypePropItemPredicate = Interval.Closed[Witness.`1.23`.T, Witness.`4.56`.T]
            |  type CustomArrayTypePropType = Refined[Vector[scala.math.BigDecimal], And[MaxSize[Witness.`100`.T], Forall[CustomArrayTypePropItemPredicate]]]
            |  object CustomArrayTypePropType {
            |    import eu.timepit.refined.api._
            |    type ResultType = Refined[Vector[scala.math.BigDecimal], And[MaxSize[Witness.`100`.T], Forall[CustomArrayTypePropItemPredicate]]]
            |    private val rt = RefinedType.apply[ResultType]
            |    def apply(candidate: Vector[scala.math.BigDecimal]): Either[IllegalArgumentException, ResultType] = from(candidate)
            |    def from(candidate: Vector[scala.math.BigDecimal]): Either[IllegalArgumentException, ResultType] = rt.refine(candidate).left.map(msg => new IllegalArgumentException(msg))
            |    def unapply(candidate: Vector[scala.math.BigDecimal]): Option[ResultType] = from(candidate).toOption
            |    def unsafeFrom(candidate: Vector[scala.math.BigDecimal]): ResultType = rt.unsafeRefine(candidate)
            |  }
            |  type OptionalStringArrayItemPredicate = And[MinSize[Witness.`2`.T], And[MaxSize[Witness.`42`.T], MatchesRegex[Witness.`"^[A-z0-9]+$"`.T]]]
            |  type OptionalStringArrayType = Option[Refined[scala.collection.immutable.List[String], Forall[OptionalStringArrayItemPredicate]]]
            |  object OptionalStringArrayType {
            |    import eu.timepit.refined.api._
            |    type ResultType = Refined[scala.collection.immutable.List[String], Forall[OptionalStringArrayItemPredicate]]
            |    private val rt = RefinedType.apply[ResultType]
            |    def apply(candidate: scala.collection.immutable.List[String]): Either[IllegalArgumentException, Option[ResultType]] = from(Option(candidate))
            |    def apply(candidate: Option[scala.collection.immutable.List[String]]): Either[IllegalArgumentException, Option[ResultType]] = from(candidate)
            |    def from(candidate: Option[scala.collection.immutable.List[String]]): Either[IllegalArgumentException, Option[ResultType]] = candidate match {
            |      case Some(value) =>
            |        rt.refine(value).map(Some(_)).left.map(msg => new IllegalArgumentException(msg))
            |      case None =>
            |        Right(None)
            |    }
            |    def unapply(candidate: Option[scala.collection.immutable.List[String]]): Option[ResultType] = from(candidate).fold(_ => None, a => a)
            |    def unsafeFrom(candidate: Option[scala.collection.immutable.List[String]]): Option[ResultType] = candidate.map(rt.unsafeRefine)
            |  }
            |  def from(id: String, optionalCustomArrayTypeProp: Option[Set[scala.math.BigDecimal]] = None, foo: Option[String] = None, bar: Option[String] = None, numberProp: Float, customNumberProp: scala.math.BigDecimal, customArrayTypeProp: Vector[scala.math.BigDecimal] = Vector.empty, optionalStringArray: Option[scala.collection.immutable.List[String]] = None, additionalProperties: Option[DataType.AdditionalProperties] = None): Either[IllegalArgumentException, DataType] = {
            |    val _id = IdType.from(id)
            |    val _optionalCustomArrayTypeProp = OptionalCustomArrayTypePropType.from(optionalCustomArrayTypeProp)
            |    val _foo = Right(foo)
            |    val _bar = BarType.from(bar)
            |    val _numberProp = NumberPropType.from(numberProp)
            |    val _customNumberProp = CustomNumberPropType.from(customNumberProp)
            |    val _customArrayTypeProp = CustomArrayTypePropType.from(customArrayTypeProp)
            |    val _optionalStringArray = OptionalStringArrayType.from(optionalStringArray)
            |    _id.flatMap { (__id: IdType) =>
            |      _optionalCustomArrayTypeProp.flatMap { (__optionalCustomArrayTypeProp: OptionalCustomArrayTypePropType) =>
            |        _foo.flatMap { (__foo: Option[String]) =>
            |          _bar.flatMap { (__bar: BarType) =>
            |            _numberProp.flatMap { (__numberProp: NumberPropType) =>
            |              _customNumberProp.flatMap { (__customNumberProp: CustomNumberPropType) =>
            |                _customArrayTypeProp.flatMap { (__customArrayTypeProp: CustomArrayTypePropType) =>
            |                  _optionalStringArray.map {
            |                    (__optionalStringArray: OptionalStringArrayType) => DataType(__id, __optionalCustomArrayTypeProp, __foo, __bar, __numberProp, __customNumberProp, __customArrayTypeProp, __optionalStringArray)(additionalProperties)
            |                  }
            |                }
            |              }
            |            }
            |          }
            |        }
            |      }
            |    }
            |  }
            |}""".stripMargin.stripTrailingSpaces
        )
      )
    }

    "use base property declarations" in {
      val params = ModelGenParams(
        new File("src/sbt-test/sbt-scraml/refined/api/refined.raml"),
        new File("target/scraml-refined-test"),
        "scraml",
        FieldMatchPolicy.Exact(),
        DefaultTypes(),
        librarySupport = Set(CirceJsonSupport(), RefinedSupport),
        formatConfig = None,
        generateDateCreated = true
      )

      val generated = ModelGenRunner.run(DefaultModelGen)(params).unsafeRunSync()

      generated.files.nonEmpty should be(true)

      val theSource = generated.files
        .find(_.source.name == "ChildWithFacetsType")
        .map(_.source.source.toString().stripTrailingSpaces)

      val theCompanion = generated.files
        .find(_.source.name == "ChildWithFacetsType")
        .flatMap(_.source.companion)
        .map(_.toString().stripTrailingSpaces)

      theSource should be(
        Some(
          """final case class ChildWithFacetsType(id: String) extends BaseWithoutFacetsType"""
        )
      )

      theCompanion should be(
        Some(
          """object ChildWithFacetsType {
            |  import io.circe._
            |  import io.circe.generic.semiauto._
            |  import io.circe.syntax._
            |  import io.circe.refined._
            |  implicit lazy val decoder: Decoder[ChildWithFacetsType] = new Decoder[ChildWithFacetsType] {
            |    def apply(c: HCursor): Decoder.Result[ChildWithFacetsType] = {
            |      c.downField("id").as[String].flatMap {
            |        (_id: String) => ChildWithFacetsType.from(_id).swap.map(e => DecodingFailure(e.getMessage, Nil)).swap
            |      }
            |    }
            |  }
			|  implicit lazy val encoder: Encoder[ChildWithFacetsType] = deriveEncoder[ChildWithFacetsType].mapJsonObject(_.+:("type" -> Json.fromString("child")))
            |  import eu.timepit.refined.api.Refined
            |  import eu.timepit.refined.boolean.And
            |  import eu.timepit.refined.collection._
            |  import eu.timepit.refined.numeric._
            |  import eu.timepit.refined.string._
            |  import shapeless.Witness
            |  type IdType = Refined[String, MatchesRegex[Witness.`"^[A-z]+$"`.T]]
            |  object IdType {
            |    import eu.timepit.refined.api._
            |    type ResultType = Refined[String, MatchesRegex[Witness.`"^[A-z]+$"`.T]]
            |    private val rt = RefinedType.apply[ResultType]
            |    def apply(candidate: String): Either[IllegalArgumentException, ResultType] = from(candidate)
            |    def from(candidate: String): Either[IllegalArgumentException, ResultType] = rt.refine(candidate).left.map(msg => new IllegalArgumentException(msg))
            |    def unapply(candidate: String): Option[ResultType] = from(candidate).toOption
            |    def unsafeFrom(candidate: String): ResultType = rt.unsafeRefine(candidate)
            |  }
            |  def from(id: String): Either[IllegalArgumentException, ChildWithFacetsType] = {
            |    val _id = IdType.from(id)
            |    _id.map {
            |      (__id: IdType) => ChildWithFacetsType(__id.value)
            |    }
            |  }
            |}""".stripMargin.stripTrailingSpaces
        )
      )
    }

    "use inherited facets over ones in derived types" in {
      val params = ModelGenParams(
        new File("src/sbt-test/sbt-scraml/refined/api/refined.raml"),
        new File("target/scraml-refined-test"),
        "scraml",
        FieldMatchPolicy.Exact(),
        DefaultTypes(),
        librarySupport = Set(CirceJsonSupport(), RefinedSupport),
        formatConfig = None,
        generateDateCreated = true
      )

      val generated = ModelGenRunner.run(DefaultModelGen)(params).unsafeRunSync()

      generated.files.nonEmpty should be(true)

      val theBaseSource = generated.files
        .find(_.source.name == "BaseWithFacetsType")
        .map(_.source.source.toString().stripTrailingSpaces)

      val theChildSource = generated.files
        .find(_.source.name == "ChildOverridesAll")
        .map(_.source.source.toString().stripTrailingSpaces)

      val theChildCompanion = generated.files
        .find(_.source.name == "ChildOverridesAll")
        .flatMap(_.source.companion)
        .map(_.toString().stripTrailingSpaces)

      theBaseSource should be(
        Some(
          """sealed trait BaseWithFacetsType {
            |  def id: BaseWithFacetsType.IdType
            |  def count: BaseWithFacetsType.CountType
            |  def atMost100: BaseWithFacetsType.AtMost100Type
            |  def stringArray: BaseWithFacetsType.StringArrayType
            |}""".stripMargin
        )
      )

      theChildSource should be(
        Some(
          """final case class ChildOverridesAll(id: ChildOverridesAll.IdType, count: ChildOverridesAll.CountType, atMost100: ChildOverridesAll.AtMost100Type, stringArray: ChildOverridesAll.StringArrayType) extends BaseWithFacetsType"""
        )
      )

      theChildCompanion should be(
        Some(
          """object ChildOverridesAll {
            |  import io.circe._
            |  import io.circe.generic.semiauto._
            |  import io.circe.syntax._
            |  import io.circe.refined._
            |  implicit lazy val decoder: Decoder[ChildOverridesAll] = new Decoder[ChildOverridesAll] {
            |    def apply(c: HCursor): Decoder.Result[ChildOverridesAll] = {
            |      c.downField("id").as[String].flatMap { (_id: String) =>
            |        c.downField("count").as[Int].flatMap { (_count: Int) =>
            |          c.downField("atMost100").as[Float].flatMap { (_atMost100: Float) =>
            |            c.downField("stringArray").as[scala.collection.immutable.List[String]].flatMap {
            |              (_stringArray: scala.collection.immutable.List[String]) => ChildOverridesAll.from(_id, _count, _atMost100, _stringArray).swap.map(e => DecodingFailure(e.getMessage, Nil)).swap
            |            }
            |          }
            |        }
            |      }
            |    }
            |  }
			|  implicit lazy val encoder: Encoder[ChildOverridesAll] = deriveEncoder[ChildOverridesAll].mapJsonObject(_.+:("type" -> Json.fromString("overrides")))
            |  import eu.timepit.refined.api.Refined
            |  import eu.timepit.refined.boolean.And
            |  import eu.timepit.refined.collection._
            |  import eu.timepit.refined.numeric._
            |  import eu.timepit.refined.string._
            |  import shapeless.Witness
            |  type IdType = Refined[String, And[MinSize[Witness.`8`.T], And[MaxSize[Witness.`64`.T], MatchesRegex[Witness.`"^[A-z0-9-]*$"`.T]]]]
            |  object IdType {
            |    import eu.timepit.refined.api._
            |    type ResultType = Refined[String, And[MinSize[Witness.`8`.T], And[MaxSize[Witness.`64`.T], MatchesRegex[Witness.`"^[A-z0-9-]*$"`.T]]]]
            |    private val rt = RefinedType.apply[ResultType]
            |    def apply(candidate: String): Either[IllegalArgumentException, ResultType] = from(candidate)
            |    def from(candidate: String): Either[IllegalArgumentException, ResultType] = rt.refine(candidate).left.map(msg => new IllegalArgumentException(msg))
            |    def unapply(candidate: String): Option[ResultType] = from(candidate).toOption
            |    def unsafeFrom(candidate: String): ResultType = rt.unsafeRefine(candidate)
            |  }
            |  type CountType = Refined[Int, GreaterEqual[Witness.`0`.T]]
            |  object CountType {
            |    import eu.timepit.refined.api._
            |    type ResultType = Refined[Int, GreaterEqual[Witness.`0`.T]]
            |    private val rt = RefinedType.apply[ResultType]
            |    def apply(candidate: Int): Either[IllegalArgumentException, ResultType] = from(candidate)
            |    def from(candidate: Int): Either[IllegalArgumentException, ResultType] = rt.refine(candidate).left.map(msg => new IllegalArgumentException(msg))
            |    def unapply(candidate: Int): Option[ResultType] = from(candidate).toOption
            |    def unsafeFrom(candidate: Int): ResultType = rt.unsafeRefine(candidate)
            |  }
            |  type AtMost100Type = Refined[Float, LessEqual[Witness.`100.0`.T]]
            |  object AtMost100Type {
            |    import eu.timepit.refined.api._
            |    type ResultType = Refined[Float, LessEqual[Witness.`100.0`.T]]
            |    private val rt = RefinedType.apply[ResultType]
            |    def apply(candidate: Float): Either[IllegalArgumentException, ResultType] = from(candidate)
            |    def from(candidate: Float): Either[IllegalArgumentException, ResultType] = rt.refine(candidate).left.map(msg => new IllegalArgumentException(msg))
            |    def unapply(candidate: Float): Option[ResultType] = from(candidate).toOption
            |    def unsafeFrom(candidate: Float): ResultType = rt.unsafeRefine(candidate)
            |  }
            |  type StringArrayItemPredicate = And[MinSize[Witness.`2`.T], And[MaxSize[Witness.`99`.T], MatchesRegex[Witness.`"^[A-z0-9]+$"`.T]]]
            |  type StringArrayType = Refined[scala.collection.immutable.List[String], And[MinSize[Witness.`1`.T], And[MaxSize[Witness.`5`.T], Forall[StringArrayItemPredicate]]]]
            |  object StringArrayType {
            |    import eu.timepit.refined.api._
            |    type ResultType = Refined[scala.collection.immutable.List[String], And[MinSize[Witness.`1`.T], And[MaxSize[Witness.`5`.T], Forall[StringArrayItemPredicate]]]]
            |    private val rt = RefinedType.apply[ResultType]
            |    def apply(candidate: scala.collection.immutable.List[String]): Either[IllegalArgumentException, ResultType] = from(candidate)
            |    def from(candidate: scala.collection.immutable.List[String]): Either[IllegalArgumentException, ResultType] = rt.refine(candidate).left.map(msg => new IllegalArgumentException(msg))
            |    def unapply(candidate: scala.collection.immutable.List[String]): Option[ResultType] = from(candidate).toOption
            |    def unsafeFrom(candidate: scala.collection.immutable.List[String]): ResultType = rt.unsafeRefine(candidate)
            |  }
            |  def from(id: String, count: Int, atMost100: Float, stringArray: scala.collection.immutable.List[String] = scala.collection.immutable.List.empty): Either[IllegalArgumentException, ChildOverridesAll] = {
            |    val _id = IdType.from(id)
            |    val _count = CountType.from(count)
            |    val _atMost100 = AtMost100Type.from(atMost100)
            |    val _stringArray = StringArrayType.from(stringArray)
            |    _id.flatMap { (__id: IdType) =>
            |      _count.flatMap { (__count: CountType) =>
            |        _atMost100.flatMap { (__atMost100: AtMost100Type) =>
            |          _stringArray.map {
            |            (__stringArray: StringArrayType) => ChildOverridesAll(__id, __count, __atMost100, __stringArray)
            |          }
            |        }
            |      }
            |    }
            |  }
            |}""".stripMargin.stripTrailingSpaces
        )
      )
    }

    "use inherited facets" in {
      val params = ModelGenParams(
        new File("src/sbt-test/sbt-scraml/refined/api/refined.raml"),
        new File("target/scraml-refined-test"),
        "scraml",
        FieldMatchPolicy.Exact(),
        DefaultTypes(),
        librarySupport = Set(CirceJsonSupport(), RefinedSupport),
        formatConfig = None,
        generateDateCreated = true
      )

      val generated = ModelGenRunner.run(DefaultModelGen)(params).unsafeRunSync()

      generated.files.nonEmpty should be(true)

      val theBaseSource = generated.files
        .find(_.source.name == "BaseWithFacetsType")
        .map(_.source.source.toString().stripTrailingSpaces)

      val theChildSource = generated.files
        .find(_.source.name == "ChildInheritsAll")
        .map(_.source.source.toString().stripTrailingSpaces)

      val theChildCompanion = generated.files
        .find(_.source.name == "ChildInheritsAll")
        .flatMap(_.source.companion)
        .map(_.toString().stripTrailingSpaces)

      theBaseSource should be(
        Some(
          """sealed trait BaseWithFacetsType {
            |  def id: BaseWithFacetsType.IdType
            |  def count: BaseWithFacetsType.CountType
            |  def atMost100: BaseWithFacetsType.AtMost100Type
            |  def stringArray: BaseWithFacetsType.StringArrayType
            |}""".stripMargin.stripTrailingSpaces
        )
      )

      theChildSource should be(
        Some(
          """final case class ChildInheritsAll(id: ChildInheritsAll.IdType, count: ChildInheritsAll.CountType, atMost100: ChildInheritsAll.AtMost100Type, stringArray: ChildInheritsAll.StringArrayType) extends BaseWithFacetsType"""
        )
      )

      theChildCompanion should be(
        Some(
          """object ChildInheritsAll {
            |  import io.circe._
            |  import io.circe.generic.semiauto._
            |  import io.circe.syntax._
            |  import io.circe.refined._
            |  implicit lazy val decoder: Decoder[ChildInheritsAll] = new Decoder[ChildInheritsAll] {
            |    def apply(c: HCursor): Decoder.Result[ChildInheritsAll] = {
            |      c.downField("id").as[String].flatMap { (_id: String) =>
            |        c.downField("count").as[Int].flatMap { (_count: Int) =>
            |          c.downField("atMost100").as[Float].flatMap { (_atMost100: Float) =>
            |            c.downField("stringArray").as[scala.collection.immutable.List[String]].flatMap {
            |              (_stringArray: scala.collection.immutable.List[String]) => ChildInheritsAll.from(_id, _count, _atMost100, _stringArray).swap.map(e => DecodingFailure(e.getMessage, Nil)).swap
            |            }
            |          }
            |        }
            |      }
            |    }
            |  }
			|  implicit lazy val encoder: Encoder[ChildInheritsAll] = deriveEncoder[ChildInheritsAll].mapJsonObject(_.+:("type" -> Json.fromString("inherited")))
            |  import eu.timepit.refined.api.Refined
            |  import eu.timepit.refined.boolean.And
            |  import eu.timepit.refined.collection._
            |  import eu.timepit.refined.numeric._
            |  import eu.timepit.refined.string._
            |  import shapeless.Witness
            |  type IdType = Refined[String, And[MinSize[Witness.`8`.T], And[MaxSize[Witness.`64`.T], MatchesRegex[Witness.`"^[A-z0-9-]*$"`.T]]]]
            |  object IdType {
            |    import eu.timepit.refined.api._
            |    type ResultType = Refined[String, And[MinSize[Witness.`8`.T], And[MaxSize[Witness.`64`.T], MatchesRegex[Witness.`"^[A-z0-9-]*$"`.T]]]]
            |    private val rt = RefinedType.apply[ResultType]
            |    def apply(candidate: String): Either[IllegalArgumentException, ResultType] = from(candidate)
            |    def from(candidate: String): Either[IllegalArgumentException, ResultType] = rt.refine(candidate).left.map(msg => new IllegalArgumentException(msg))
            |    def unapply(candidate: String): Option[ResultType] = from(candidate).toOption
            |    def unsafeFrom(candidate: String): ResultType = rt.unsafeRefine(candidate)
            |  }
            |  type CountType = Refined[Int, GreaterEqual[Witness.`0`.T]]
            |  object CountType {
            |    import eu.timepit.refined.api._
            |    type ResultType = Refined[Int, GreaterEqual[Witness.`0`.T]]
            |    private val rt = RefinedType.apply[ResultType]
            |    def apply(candidate: Int): Either[IllegalArgumentException, ResultType] = from(candidate)
            |    def from(candidate: Int): Either[IllegalArgumentException, ResultType] = rt.refine(candidate).left.map(msg => new IllegalArgumentException(msg))
            |    def unapply(candidate: Int): Option[ResultType] = from(candidate).toOption
            |    def unsafeFrom(candidate: Int): ResultType = rt.unsafeRefine(candidate)
            |  }
            |  type AtMost100Type = Refined[Float, LessEqual[Witness.`100.0`.T]]
            |  object AtMost100Type {
            |    import eu.timepit.refined.api._
            |    type ResultType = Refined[Float, LessEqual[Witness.`100.0`.T]]
            |    private val rt = RefinedType.apply[ResultType]
            |    def apply(candidate: Float): Either[IllegalArgumentException, ResultType] = from(candidate)
            |    def from(candidate: Float): Either[IllegalArgumentException, ResultType] = rt.refine(candidate).left.map(msg => new IllegalArgumentException(msg))
            |    def unapply(candidate: Float): Option[ResultType] = from(candidate).toOption
            |    def unsafeFrom(candidate: Float): ResultType = rt.unsafeRefine(candidate)
            |  }
            |  type StringArrayItemPredicate = And[MinSize[Witness.`2`.T], And[MaxSize[Witness.`99`.T], MatchesRegex[Witness.`"^[A-z0-9]+$"`.T]]]
            |  type StringArrayType = Refined[scala.collection.immutable.List[String], And[MinSize[Witness.`1`.T], And[MaxSize[Witness.`5`.T], Forall[StringArrayItemPredicate]]]]
            |  object StringArrayType {
            |    import eu.timepit.refined.api._
            |    type ResultType = Refined[scala.collection.immutable.List[String], And[MinSize[Witness.`1`.T], And[MaxSize[Witness.`5`.T], Forall[StringArrayItemPredicate]]]]
            |    private val rt = RefinedType.apply[ResultType]
            |    def apply(candidate: scala.collection.immutable.List[String]): Either[IllegalArgumentException, ResultType] = from(candidate)
            |    def from(candidate: scala.collection.immutable.List[String]): Either[IllegalArgumentException, ResultType] = rt.refine(candidate).left.map(msg => new IllegalArgumentException(msg))
            |    def unapply(candidate: scala.collection.immutable.List[String]): Option[ResultType] = from(candidate).toOption
            |    def unsafeFrom(candidate: scala.collection.immutable.List[String]): ResultType = rt.unsafeRefine(candidate)
            |  }
            |  def from(id: String, count: Int, atMost100: Float, stringArray: scala.collection.immutable.List[String] = scala.collection.immutable.List.empty): Either[IllegalArgumentException, ChildInheritsAll] = {
            |    val _id = IdType.from(id)
            |    val _count = CountType.from(count)
            |    val _atMost100 = AtMost100Type.from(atMost100)
            |    val _stringArray = StringArrayType.from(stringArray)
            |    _id.flatMap { (__id: IdType) =>
            |      _count.flatMap { (__count: CountType) =>
            |        _atMost100.flatMap { (__atMost100: AtMost100Type) =>
            |          _stringArray.map {
            |            (__stringArray: StringArrayType) => ChildInheritsAll(__id, __count, __atMost100, __stringArray)
            |          }
            |        }
            |      }
            |    }
            |  }
            |}""".stripMargin.stripTrailingSpaces
        )
      )
    }
  }
}
