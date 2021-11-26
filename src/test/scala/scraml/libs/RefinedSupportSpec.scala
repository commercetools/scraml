package scraml.libs

import java.io.File

import cats.effect.unsafe.implicits.global
import org.scalatest.diagrams.Diagrams
import org.scalatest.wordspec.AnyWordSpec
import scraml.{DefaultModelGen, DefaultTypes, ModelGenParams, ModelGenRunner}

class RefinedSupportSpec extends AnyWordSpec with Diagrams {
  implicit class StripTrailingSpaces(private val content: String) {
    def stripTrailingSpaces: String =
      content.split('\n')
        .map(_.replaceFirst(" +$", ""))
        .mkString("\n")
  }

  "RefinedSupport" must {
    "modify case class refined property types" in {
      val params = ModelGenParams(
        new File("src/sbt-test/sbt-scraml/refined/api/refined.raml"),
        new File("target/scraml-refined-test"),
        "scraml",
        DefaultTypes(),
        librarySupport = Set(CirceJsonSupport(), RefinedSupport),
        formatConfig = None,
        generateDateCreated = true
      )

      val generated = ModelGenRunner.run(DefaultModelGen)(params).unsafeRunSync()

      assert(generated.files.nonEmpty)

      val theSource = generated.files
        .find(_.source.name == "DataType")
        .map(_.source.source.toString().stripTrailingSpaces)

      val theCompanion = generated.files
        .find(_.source.name == "DataType")
        .flatMap(_.source.companion)
        .map(_.toString().stripTrailingSpaces)

      assert(
        theSource.contains(
          """final case class DataType(id: DataType.IdType, optionalCustomArrayTypeProp: DataType.OptionalCustomArrayTypePropType = None, foo: Option[String] = None, bar: DataType.BarType = None, numberProp: DataType.NumberPropType, customNumberProp: DataType.CustomNumberPropType, customArrayTypeProp: DataType.CustomArrayTypePropType, optionalStringArray: DataType.OptionalStringArrayType = None) extends BaseType"""
        )
      )

      assert(
        theCompanion.contains(
          """object DataType {
            |  import eu.timepit.refined.api.Refined
            |  import eu.timepit.refined.boolean.And
            |  import eu.timepit.refined.collection._
            |  import eu.timepit.refined.numeric._
            |  import eu.timepit.refined.string._
            |  import shapeless.Witness
            |  import io.circe.refined._
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
            |  import io.circe._
            |  import io.circe.generic.semiauto._
            |  implicit lazy val decoder: Decoder[DataType] = deriveDecoder[DataType]
            |  implicit lazy val encoder: Encoder[DataType] = deriveEncoder[DataType].mapJsonObject(_.add("type", Json.fromString("data")))
            |}""".stripMargin
            .stripTrailingSpaces
        )
      )
    }

    "use base property declarations" in {
      val params = ModelGenParams(
        new File("src/sbt-test/sbt-scraml/refined/api/refined.raml"),
        new File("target/scraml-refined-test"),
        "scraml",
        DefaultTypes(),
        librarySupport = Set(CirceJsonSupport(), RefinedSupport),
        formatConfig = None,
        generateDateCreated = true
      )

      val generated = ModelGenRunner.run(DefaultModelGen)(params).unsafeRunSync()

      assert(generated.files.nonEmpty)

      val theSource = generated.files
        .find(_.source.name == "ChildWithFacetsType")
        .map(_.source.source.toString().stripTrailingSpaces)

      val theCompanion = generated.files
        .find(_.source.name == "ChildWithFacetsType")
        .flatMap(_.source.companion)
        .map(_.toString().stripTrailingSpaces)

      assert(
        theSource.contains(
          """final case class ChildWithFacetsType(id: String) extends BaseWithoutFacetsType"""
        )
      )

      assert(
        theCompanion.contains(
          """object ChildWithFacetsType {
            |  import eu.timepit.refined.api.Refined
            |  import eu.timepit.refined.boolean.And
            |  import eu.timepit.refined.collection._
            |  import eu.timepit.refined.numeric._
            |  import eu.timepit.refined.string._
            |  import shapeless.Witness
            |  import io.circe.refined._
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
            |  import io.circe._
            |  import io.circe.generic.semiauto._
            |  implicit lazy val decoder: Decoder[ChildWithFacetsType] = deriveDecoder[ChildWithFacetsType]
            |  implicit lazy val encoder: Encoder[ChildWithFacetsType] = deriveEncoder[ChildWithFacetsType].mapJsonObject(_.add("type", Json.fromString("child")))
            |}""".stripMargin
            .stripTrailingSpaces
        )
      )
    }

    "use inherited facets over ones in derived types" in {
      val params = ModelGenParams(
        new File("src/sbt-test/sbt-scraml/refined/api/refined.raml"),
        new File("target/scraml-refined-test"),
        "scraml",
        DefaultTypes(),
        librarySupport = Set(CirceJsonSupport(), RefinedSupport),
        formatConfig = None,
        generateDateCreated = true
      )

      val generated = ModelGenRunner.run(DefaultModelGen)(params).unsafeRunSync()

      assert(generated.files.nonEmpty)

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

      assert(
        theBaseSource.contains(
          """sealed trait BaseWithFacetsType {
            |  def id: BaseWithFacetsType.IdType
            |  def count: BaseWithFacetsType.CountType
            |  def atMost100: BaseWithFacetsType.AtMost100Type
            |  def stringArray: BaseWithFacetsType.StringArrayType
            |}""".stripMargin
        )
      )

      assert(
        theChildSource.contains(
          """final case class ChildOverridesAll(id: ChildOverridesAll.IdType, count: ChildOverridesAll.CountType, atMost100: ChildOverridesAll.AtMost100Type, stringArray: ChildOverridesAll.StringArrayType) extends BaseWithFacetsType"""
        )
      )

      assert(
        theChildCompanion.contains(
          """object ChildOverridesAll {
            |  import eu.timepit.refined.api.Refined
            |  import eu.timepit.refined.boolean.And
            |  import eu.timepit.refined.collection._
            |  import eu.timepit.refined.numeric._
            |  import eu.timepit.refined.string._
            |  import shapeless.Witness
            |  import io.circe.refined._
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
            |  import io.circe._
            |  import io.circe.generic.semiauto._
            |  implicit lazy val decoder: Decoder[ChildOverridesAll] = deriveDecoder[ChildOverridesAll]
            |  implicit lazy val encoder: Encoder[ChildOverridesAll] = deriveEncoder[ChildOverridesAll].mapJsonObject(_.add("type", Json.fromString("overrides")))
            |}""".stripMargin
            .stripTrailingSpaces
        )
      )
    }

    "use inherited facets" in {
      val params = ModelGenParams(
        new File("src/sbt-test/sbt-scraml/refined/api/refined.raml"),
        new File("target/scraml-refined-test"),
        "scraml",
        DefaultTypes(),
        librarySupport = Set(CirceJsonSupport(), RefinedSupport),
        formatConfig = None,
        generateDateCreated = true
      )

      val generated = ModelGenRunner.run(DefaultModelGen)(params).unsafeRunSync()

      assert(generated.files.nonEmpty)

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

      assert(
        theBaseSource.contains(
          """sealed trait BaseWithFacetsType {
            |  def id: BaseWithFacetsType.IdType
            |  def count: BaseWithFacetsType.CountType
            |  def atMost100: BaseWithFacetsType.AtMost100Type
            |  def stringArray: BaseWithFacetsType.StringArrayType
            |}""".stripMargin
            .stripTrailingSpaces
        )
      )

      assert(
        theChildSource.contains(
          """final case class ChildInheritsAll(id: ChildInheritsAll.IdType, count: ChildInheritsAll.CountType, atMost100: ChildInheritsAll.AtMost100Type, stringArray: ChildInheritsAll.StringArrayType) extends BaseWithFacetsType"""
        )
      )

      assert(
        theChildCompanion.contains(
          """object ChildInheritsAll {
            |  import eu.timepit.refined.api.Refined
            |  import eu.timepit.refined.boolean.And
            |  import eu.timepit.refined.collection._
            |  import eu.timepit.refined.numeric._
            |  import eu.timepit.refined.string._
            |  import shapeless.Witness
            |  import io.circe.refined._
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
            |  import io.circe._
            |  import io.circe.generic.semiauto._
            |  implicit lazy val decoder: Decoder[ChildInheritsAll] = deriveDecoder[ChildInheritsAll]
            |  implicit lazy val encoder: Encoder[ChildInheritsAll] = deriveEncoder[ChildInheritsAll].mapJsonObject(_.add("type", Json.fromString("inherited")))
            |}""".stripMargin
            .stripTrailingSpaces
        )
      )
    }
  }
}
