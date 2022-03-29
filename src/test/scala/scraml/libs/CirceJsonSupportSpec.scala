package scraml.libs

import java.io.File

import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scraml._

class CirceJsonSupportSpec extends AnyFlatSpec with Matchers with SourceCodeFormatting {
  "Circe JSON Support (exact property matching)" should "generate JSON derivation" in {
    val params = ModelGenParams(
      new File("src/sbt-test/sbt-scraml/json/api/json.raml"),
      new File("target/scraml-circe-json-test"),
      "scraml",
      FieldMatchPolicy.Exact(),
      DefaultTypes(
        float = "scala.math.BigDecimal",
        double = "scala.math.BigDecimal",
        number = "scala.math.BigDecimal",
        long = "scala.math.BigInt"
      ),
      librarySupport = Set(
        CirceJsonSupport(formats = Map("localDateTime" -> "io.circe.Decoder.decodeLocalDateTime"))
      ),
      None
    )

    val generated = ModelGenRunner.run(DefaultModelGen)(params).unsafeRunSync()

    generated.files match {
      case noDiscBase ::
          _ ::
          _ ::
          baseType ::
          intermediateType ::
          grandchildType ::
          dataType ::
          emptyBase ::
          noProps ::
          noSealedBase ::
          someEnum ::
          defaultProperty ::
          otherSub ::
          mapLike ::
          packageObject ::
          Nil =>
        noDiscBase.source.source.toString().stripTrailingSpaces should be(
          "sealed trait NoDiscriminatorBase"
        )
        noDiscBase.source.companion.map(_.toString().stripTrailingSpaces) should be(
          Some(s"""object NoDiscriminatorBase {
                  |  import io.circe.Decoder.Result
                  |  import io.circe._
                  |  implicit lazy val decoder: Decoder[NoDiscriminatorBase] = new Decoder[NoDiscriminatorBase] { override def apply(c: HCursor): Result[NoDiscriminatorBase] = NoDiscriminatorSub2.decoder.tryDecode(c).fold(_ => NoDiscriminatorSub1.decoder.tryDecode(c), Right(_)) }
                  |  implicit lazy val encoder: Encoder[NoDiscriminatorBase] = new Encoder[NoDiscriminatorBase] {
                  |    override def apply(nodiscriminatorbase: NoDiscriminatorBase): Json = nodiscriminatorbase match {
                  |      case nodiscriminatorsub1: NoDiscriminatorSub1 =>
                  |        NoDiscriminatorSub1.encoder(nodiscriminatorsub1)
                  |      case nodiscriminatorsub2: NoDiscriminatorSub2 =>
                  |        NoDiscriminatorSub2.encoder(nodiscriminatorsub2)
                  |    }
                  |  }
                  |}""".stripMargin.stripTrailingSpaces)
        )

        baseType.source.packageName should be("datatypes")
        baseType.source.source.toString().stripTrailingSpaces should be(
          "sealed trait BaseType extends Any { def id: String }"
        )
        baseType.source.companion.map(_.toString().stripTrailingSpaces) should be(
          Some(
            s"""object BaseType {
                                                                      |  import io.circe.Decoder.Result
                                                                      |  import io.circe._
                                                                      |  implicit lazy val decoder: Decoder[BaseType] = new Decoder[BaseType] {
                                                                      |    override def apply(c: HCursor): Result[BaseType] = c.downField("type").as[String] match {
                                                                      |      case Right("data") =>
                                                                      |        DataType.decoder(c)
                                                                      |      case Right("grandchild") =>
                                                                      |        GrandchildType.decoder(c)
                                                                      |      case other =>
                                                                      |        Left(DecodingFailure(s"unknown discriminator: $$other", c.history))
                                                                      |    }
                                                                      |  }
                                                                      |  implicit lazy val encoder: Encoder[BaseType] = new Encoder[BaseType] {
                                                                      |    override def apply(basetype: BaseType): Json = basetype match {
                                                                      |      case datatype: DataType =>
                                                                      |        DataType.encoder(datatype)
                                                                      |      case grandchildtype: GrandchildType =>
                                                                      |        GrandchildType.encoder(grandchildtype)
                                                                      |    }
                                                                      |  }
                                                                      |}""".stripMargin.stripTrailingSpaces
          )
        )

        baseType.source.name should be("BaseType")
        baseType.file.getPath should be("target/scraml-circe-json-test/scraml/datatypes.scala")

        dataType.source.packageName should be("datatypes")
        dataType.source.source.toString().stripTrailingSpaces should be(
          "final case class DataType(id: String, foo: Option[String] = None, customTypeProp: scala.math.BigDecimal, customArrayTypeProp: Vector[scala.math.BigDecimal] = Vector.empty) extends BaseType"
        )
        dataType.source.name should be("DataType")
        dataType.source.companion.map(_.toString().stripTrailingSpaces) should be(
          Some(
            s"""object DataType {
                                                                      |  import io.circe._
                                                                      |  import io.circe.generic.semiauto._
                                                                      |  import io.circe.syntax._
                                                                      |  import scraml.Formats._
                                                                      |  implicit lazy val decoder: Decoder[DataType] = deriveDecoder[DataType]
                                                                      |  implicit lazy val encoder: Encoder[DataType] = deriveEncoder[DataType].mapJsonObject(_.+:("type" -> Json.fromString("data")))
                                                                      |}""".stripMargin.stripTrailingSpaces
          )
        )

        emptyBase.source.source.toString().stripTrailingSpaces should be("sealed trait EmptyBase")
        emptyBase.source.companion.map(_.toString().stripTrailingSpaces) should be(
          Some(
            s"""object EmptyBase {
                                                                       |  import io.circe.Decoder.Result
                                                                       |  import io.circe._
                                                                       |  implicit lazy val decoder: Decoder[EmptyBase] = new Decoder[EmptyBase] {
                                                                       |    override def apply(c: HCursor): Result[EmptyBase] = c.downField("type").as[String] match {
                                                                       |      case Right("nope") =>
                                                                       |        NoProps.decoder(c)
                                                                       |      case other =>
                                                                       |        Left(DecodingFailure(s"unknown discriminator: $$other", c.history))
                                                                       |    }
                                                                       |  }
                                                                       |  implicit lazy val encoder: Encoder[EmptyBase] = new Encoder[EmptyBase] {
                                                                       |    override def apply(emptybase: EmptyBase): Json = emptybase match {
                                                                       |      case noprops: NoProps.type =>
                                                                       |        NoProps.encoder(noprops)
                                                                       |    }
                                                                       |  }
                                                                       |}""".stripMargin.stripTrailingSpaces
          )
        )

        noProps.source.source.toString().stripTrailingSpaces should be(
          s"""case object NoProps extends EmptyBase {
             |  import io.circe._
             |  import io.circe.generic.semiauto._
             |  import io.circe.Decoder.Result
             |  implicit lazy val decoder: Decoder[NoProps.type] = new Decoder[NoProps.type] {
             |    override def apply(c: HCursor): Result[NoProps.type] = c.downField("type").as[String] match {
             |      case Right("nope") =>
             |        Right(NoProps)
             |      case other =>
             |        Left(DecodingFailure(s"unknown type: $$other", c.history))
             |    }
             |  }
             |  implicit lazy val encoder: Encoder[NoProps.type] = new Encoder[NoProps.type] { override def apply(a: NoProps.type): Json = Json.obj("type" -> Json.fromString("nope")) }
             |}""".stripMargin.stripTrailingSpaces
        )

        noSealedBase.source.source.toString().stripTrailingSpaces should be("trait NoSealedBase")
        noSealedBase.source.companion.map(_.toString().stripTrailingSpaces) should be(
          Some(
            s"""object NoSealedBase {
                                                                          |  import io.circe.Decoder.Result
                                                                          |  import io.circe._
                                                                          |  implicit lazy val decoder: Decoder[NoSealedBase] = new Decoder[NoSealedBase] {
                                                                          |    override def apply(c: HCursor): Result[NoSealedBase] = c.downField("typeId").as[String] match {
                                                                          |      case Right("map-like") =>
                                                                          |        MapLike.decoder(c)
                                                                          |      case Right("other-sub") =>
                                                                          |        OtherSub.decoder(c)
                                                                          |      case other =>
                                                                          |        Left(DecodingFailure(s"unknown discriminator: $$other", c.history))
                                                                          |    }
                                                                          |  }
                                                                          |  implicit lazy val encoder: Encoder[NoSealedBase] = new Encoder[NoSealedBase] {
                                                                          |    override def apply(nosealedbase: NoSealedBase): Json = nosealedbase match {
                                                                          |      case maplike: MapLike =>
                                                                          |        MapLike.encoder(maplike)
                                                                          |      case othersub: OtherSub =>
                                                                          |        OtherSub.encoder(othersub)
                                                                          |    }
                                                                          |  }
                                                                          |}""".stripMargin.stripTrailingSpaces
          )
        )
        mapLike.source.source.toString().stripTrailingSpaces should be(
          s"""final case class MapLike(values: scala.collection.immutable.Map[String, String]) extends NoSealedBase""".stripMargin.stripTrailingSpaces
        )

        mapLike.source.companion.map(_.toString().stripTrailingSpaces) should be(
          Some(
            s"""object MapLike {
                                                                     |  import io.circe._
                                                                     |  import io.circe.syntax._
                                                                     |  import io.circe.generic.semiauto._
                                                                     |  import io.circe.Decoder.Result
                                                                     |  implicit lazy val decoder: Decoder[MapLike] = new Decoder[MapLike] { override def apply(c: HCursor): Result[MapLike] = c.as[scala.collection.immutable.Map[String, String]].map(MapLike.apply) }
                                                                     |  implicit lazy val encoder: Encoder[MapLike] = new Encoder[MapLike] { override def apply(a: MapLike): Json = a.values.asJson }
                                                                     |}""".stripMargin.stripTrailingSpaces
          )
        )

        someEnum.source.source.toString().stripTrailingSpaces should be(
          s"""sealed trait SomeEnum""".stripMargin.stripTrailingSpaces
        )

        someEnum.source.companion.map(_.toString().stripTrailingSpaces) should be(
          Some(
            s"""object SomeEnum {
                                                                      |  case object A extends SomeEnum
                                                                      |  case object B extends SomeEnum
                                                                      |  import io.circe._
                                                                      |  implicit lazy val encoder: Encoder[SomeEnum] = Encoder[String].contramap({
                                                                      |    case A => "A"
                                                                      |    case B => "B"
                                                                      |  })
                                                                      |  implicit lazy val decoder: Decoder[SomeEnum] = Decoder[String].emap({
                                                                      |    case "A" =>
                                                                      |      Right(A)
                                                                      |    case "B" =>
                                                                      |      Right(B)
                                                                      |    case other =>
                                                                      |      Left(s"invalid enum value: $$other")
                                                                      |  })
                                                                      |}""".stripMargin.stripTrailingSpaces
          )
        )

        otherSub.source.source.toString().stripTrailingSpaces should be(
          """final case class OtherSub(id: String) extends NoSealedBase"""
        )

        otherSub.source.companion.map(_.toString().stripTrailingSpaces) should be(
          Some(
            """object OtherSub {
            |  import io.circe._
            |  import io.circe.generic.semiauto._
            |  import io.circe.syntax._
            |  import scraml.Formats._
            |  implicit lazy val decoder: Decoder[OtherSub] = deriveDecoder[OtherSub]
            |  implicit lazy val encoder: Encoder[OtherSub] = deriveEncoder[OtherSub].mapJsonObject(_.+:("typeId" -> Json.fromString("other-sub")))
            |}""".stripMargin.stripTrailingSpaces
          )
        )

        packageObject.source.source.toString.stripTrailingSpaces should be(
          s"""package object scraml {
             |  import io.circe.Decoder.Result
             |  import io.circe.{ HCursor, Json, Decoder, Encoder }
             |  implicit def eitherEncoder[A, B](implicit aEncoder: Encoder[A], bEncoder: Encoder[B]): Encoder[Either[A, B]] = new Encoder[Either[A, B]] {
             |    override def apply(a: Either[A, B]): Json = a match {
             |      case Right(b) =>
             |        bEncoder(b)
             |      case Left(a) =>
             |        aEncoder(a)
             |    }
             |  }
             |  implicit def eitherDecoder[A, B](implicit aDecoder: Decoder[A], bDecoder: Decoder[B]): Decoder[Either[A, B]] = new Decoder[Either[A, B]] { override def apply(c: HCursor): Result[Either[A, B]] = aDecoder.either(bDecoder)(c) }
             |  object Formats { implicit lazy val localDateTime = io.circe.Decoder.decodeLocalDateTime }
             |}""".stripMargin.stripTrailingSpaces
        )

        intermediateType.source.source.toString().stripTrailingSpaces should be(
          "sealed trait IntermediateType extends BaseType { def id: String }"
        )
        intermediateType.source.companion.map(_.toString().stripTrailingSpaces) should be(
          Some(s"""object IntermediateType {
                  |  import io.circe.Decoder.Result
                  |  import io.circe._
                  |  implicit lazy val decoder: Decoder[IntermediateType] = new Decoder[IntermediateType] {
                  |    override def apply(c: HCursor): Result[IntermediateType] = c.downField("type").as[String] match {
                  |      case Right("grandchild") =>
                  |        GrandchildType.decoder(c)
                  |      case other =>
                  |        Left(DecodingFailure(s"unknown discriminator: $$other", c.history))
                  |    }
                  |  }
                  |  implicit lazy val encoder: Encoder[IntermediateType] = new Encoder[IntermediateType] {
                  |    override def apply(intermediatetype: IntermediateType): Json = intermediatetype match {
                  |      case grandchildtype: GrandchildType =>
                  |        GrandchildType.encoder(grandchildtype)
                  |    }
                  |  }
                  |}""".stripMargin.stripTrailingSpaces)
        )

        grandchildType.source.source.toString().stripTrailingSpaces should be(
          "final case class GrandchildType(id: String, foo: Option[String] = None, aDouble: scala.math.BigDecimal, aFloat: scala.math.BigDecimal, anInt: Int, aLong: scala.math.BigInt, customTypeProp: scala.math.BigDecimal, customArrayTypeProp: Vector[scala.math.BigDecimal] = Vector.empty) extends IntermediateType"
        )
        grandchildType.source.companion.map(_.toString().stripTrailingSpaces) should be(
          Some(
            s"""object GrandchildType {
              |  import io.circe._
              |  import io.circe.generic.semiauto._
              |  import io.circe.syntax._
              |  import scraml.Formats._
              |  implicit lazy val decoder: Decoder[GrandchildType] = deriveDecoder[GrandchildType]
              |  implicit lazy val encoder: Encoder[GrandchildType] = deriveEncoder[GrandchildType].mapJsonObject(_.+:("type" -> Json.fromString("grandchild")))
              |}""".stripMargin.stripTrailingSpaces
          )
        )

        defaultProperty.source.source.toString().stripTrailingSpaces should be(
          """final case class DefaultProperty(message: String = "this is a default message", limit: Option[Int] = Some(2), requiredEnum: SomeEnum = SomeEnum.B, optionalEnum: Option[SomeEnum] = Some(SomeEnum.A), constrained: String = "AA", longInteger: scala.math.BigInt = -9223372036854775808L, longNumber: Option[scala.math.BigInt] = Some(-9223372036854775808L))"""
        )
        defaultProperty.source.companion.map(_.toString().stripTrailingSpaces) should be(
          Some(
            s"""object DefaultProperty {
               |  import io.circe._
               |  import io.circe.generic.semiauto._
               |  import io.circe.syntax._
               |  import scraml.Formats._
               |  implicit lazy val decoder: Decoder[DefaultProperty] = new Decoder[DefaultProperty] {
               |    def apply(c: HCursor): Decoder.Result[DefaultProperty] = {
               |      c.getOrElse[String]("message")("this is a default message").flatMap { (_message: String) =>
               |        c.getOrElse[Option[Int]]("limit")(Some(2)).flatMap { (_limit: Option[Int]) =>
               |          c.getOrElse[SomeEnum]("requiredEnum")(SomeEnum.B).flatMap { (_requiredEnum: SomeEnum) =>
               |            c.getOrElse[Option[SomeEnum]]("optionalEnum")(Some(SomeEnum.A)).flatMap { (_optionalEnum: Option[SomeEnum]) =>
               |              c.getOrElse[String]("constrained")("AA").flatMap { (_constrained: String) =>
               |                c.getOrElse[scala.math.BigInt]("longInteger")(-9223372036854775808L).flatMap { (_longInteger: scala.math.BigInt) =>
               |                  c.getOrElse[Option[scala.math.BigInt]]("longNumber")(Some(-9223372036854775808L)).map {
               |                    (_longNumber: Option[scala.math.BigInt]) => DefaultProperty(_message, _limit, _requiredEnum, _optionalEnum, _constrained, _longInteger, _longNumber)
               |                  }
               |                }
               |              }
               |            }
               |          }
               |        }
               |      }
               |    }
               |  }
               |  implicit lazy val encoder: Encoder[DefaultProperty] = deriveEncoder[DefaultProperty]
               |}""".stripMargin.stripTrailingSpaces
          )
        )
    }
  }

  "Circe JSON Support (keep-extra property matching)" should "generate JSON derivation" in {
    val params = ModelGenParams(
      new File("src/sbt-test/sbt-scraml/json/api/json.raml"),
      new File("target/scraml-circe-json-test"),
      "scraml",
      FieldMatchPolicy.KeepExtra(),
      DefaultTypes(
        float = "scala.math.BigDecimal",
        double = "scala.math.BigDecimal",
        number = "scala.math.BigDecimal",
        long = "scala.math.BigInt"
      ),
      librarySupport = Set(
        CirceJsonSupport(formats = Map("localDateTime" -> "io.circe.Decoder.decodeLocalDateTime"))
      ),
      None
    )

    val generated = ModelGenRunner.run(DefaultModelGen)(params).unsafeRunSync()

    generated.files match {
      case noDiscBase ::
          _ ::
          _ ::
          baseType ::
          intermediateType ::
          grandchildType ::
          dataType ::
          emptyBase ::
          noProps ::
          noSealedBase ::
          someEnum ::
          defaultProperty ::
          otherSub ::
          mapLike ::
          packageObject ::
          Nil =>
        noDiscBase.source.source.toString().stripTrailingSpaces should be(
          "sealed trait NoDiscriminatorBase"
        )
        noDiscBase.source.companion.map(_.toString().stripTrailingSpaces) should be(
          Some(s"""object NoDiscriminatorBase {
                  |  import io.circe.Decoder.Result
                  |  import io.circe._
                  |  implicit lazy val decoder: Decoder[NoDiscriminatorBase] = new Decoder[NoDiscriminatorBase] { override def apply(c: HCursor): Result[NoDiscriminatorBase] = NoDiscriminatorSub2.decoder.tryDecode(c).fold(_ => NoDiscriminatorSub1.decoder.tryDecode(c), Right(_)) }
                  |  implicit lazy val encoder: Encoder[NoDiscriminatorBase] = new Encoder[NoDiscriminatorBase] {
                  |    override def apply(nodiscriminatorbase: NoDiscriminatorBase): Json = nodiscriminatorbase match {
                  |      case nodiscriminatorsub1: NoDiscriminatorSub1 =>
                  |        NoDiscriminatorSub1.encoder(nodiscriminatorsub1)
                  |      case nodiscriminatorsub2: NoDiscriminatorSub2 =>
                  |        NoDiscriminatorSub2.encoder(nodiscriminatorsub2)
                  |    }
                  |  }
                  |}""".stripMargin.stripTrailingSpaces)
        )

        baseType.source.packageName should be("datatypes")
        baseType.source.source.toString().stripTrailingSpaces should be(
          "sealed trait BaseType extends Any { def id: String }"
        )
        baseType.source.companion.map(_.toString().stripTrailingSpaces) should be(
          Some("""object BaseType {
               |  import io.circe.Decoder.Result
               |  import io.circe._
               |  implicit lazy val decoder: Decoder[BaseType] = new Decoder[BaseType] {
               |    override def apply(c: HCursor): Result[BaseType] = c.downField("type").as[String] match {
               |      case Right("data") =>
               |        DataType.decoder(c)
               |      case Right("grandchild") =>
               |        GrandchildType.decoder(c)
               |      case other =>
               |        Left(DecodingFailure(s"unknown discriminator: $other", c.history))
               |    }
               |  }
               |  implicit lazy val encoder: Encoder[BaseType] = new Encoder[BaseType] {
               |    override def apply(basetype: BaseType): Json = basetype match {
               |      case datatype: DataType =>
               |        DataType.encoder(datatype)
               |      case grandchildtype: GrandchildType =>
               |        GrandchildType.encoder(grandchildtype)
               |    }
               |  }
               |}""".stripMargin.stripTrailingSpaces)
        )

        baseType.source.name should be("BaseType")
        baseType.file.getPath should be("target/scraml-circe-json-test/scraml/datatypes.scala")

        dataType.source.packageName should be("datatypes")
        dataType.source.source.toString().stripTrailingSpaces should be(
          "final case class DataType(id: String, foo: Option[String] = None, customTypeProp: scala.math.BigDecimal, customArrayTypeProp: Vector[scala.math.BigDecimal] = Vector.empty)(val additionalProperties: Option[DataType.AdditionalProperties] = None) extends BaseType"
        )
        dataType.source.name should be("DataType")
        dataType.source.companion.map(_.toString().stripTrailingSpaces) should be(
          Some("""object DataType {
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
                 |    val propertyNames: Seq[String] = Seq("id", "foo", "customTypeProp", "customArrayTypeProp")
                 |    val allowedNames: Seq[Regex] = Seq()
                 |    import io.circe._
                 |    import io.circe.generic.semiauto._
                 |    implicit lazy val decoder: Decoder[Option[AdditionalProperties]] = new Decoder[Option[AdditionalProperties]] {
                 |      final def apply(c: HCursor): Decoder.Result[Option[AdditionalProperties]] = {
                 |        val allKeys = c.keys.fold(Set.empty[String])(_.toSet)
                 |        Right(Option(allKeys.filterNot(propertyNames.contains)).filterNot(_.isEmpty).map {
                 |          _.foldLeft(scala.collection.immutable.Map.newBuilder[String, Json])({
                 |            case (accum, key) =>
                 |              c.downField(key).focus.fold(accum) {
                 |                v => accum += key -> v
                 |              }
                 |          })
                 |        }.map(b => AdditionalProperties(b.result())))
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
                 |  import scraml.Formats._
                 |  implicit lazy val decoder: Decoder[DataType] = new Decoder[DataType] {
                 |    def apply(c: HCursor): Decoder.Result[DataType] = {
                 |      c.downField("id").as[String].flatMap { (_id: String) =>
                 |        c.downField("foo").as[Option[String]].flatMap { (_foo: Option[String]) =>
                 |          c.downField("customTypeProp").as[scala.math.BigDecimal].flatMap { (_customTypeProp: scala.math.BigDecimal) =>
                 |            c.downField("customArrayTypeProp").as[Vector[scala.math.BigDecimal]].flatMap { (_customArrayTypeProp: Vector[scala.math.BigDecimal]) =>
                 |              AdditionalProperties.decoder(c).map {
                 |                (_additionalProperties: Option[DataType.AdditionalProperties]) => DataType(_id, _foo, _customTypeProp, _customArrayTypeProp)(_additionalProperties)
                 |              }
                 |            }
                 |          }
                 |        }
                 |      }
                 |    }
                 |  }
                 |  implicit lazy val encoder: Encoder[DataType] = new Encoder[DataType] { final def apply(instance: DataType): Json = AdditionalProperties.merge(Json.obj("type" -> Json.fromString("data"), "id" -> instance.id.asJson, "foo" -> instance.foo.asJson, "customTypeProp" -> instance.customTypeProp.asJson, "customArrayTypeProp" -> instance.customArrayTypeProp.asJson), instance.additionalProperties) }
                 |}""".stripMargin.stripTrailingSpaces)
        )

        emptyBase.source.source.toString().stripTrailingSpaces should be("sealed trait EmptyBase")
        emptyBase.source.companion.map(_.toString().stripTrailingSpaces) should be(
          Some("""object EmptyBase {
                  |  import io.circe.Decoder.Result
                  |  import io.circe._
                  |  implicit lazy val decoder: Decoder[EmptyBase] = new Decoder[EmptyBase] {
                  |    override def apply(c: HCursor): Result[EmptyBase] = c.downField("type").as[String] match {
                  |      case Right("nope") =>
                  |        NoProps.decoder(c)
                  |      case other =>
                  |        Left(DecodingFailure(s"unknown discriminator: $other", c.history))
                  |    }
                  |  }
                  |  implicit lazy val encoder: Encoder[EmptyBase] = new Encoder[EmptyBase] {
                  |    override def apply(emptybase: EmptyBase): Json = emptybase match {
                  |      case noprops: NoProps =>
                  |        NoProps.encoder(noprops)
                  |    }
                  |  }
                  |}""".stripMargin.stripTrailingSpaces)
        )

        noProps.source.source.toString().stripTrailingSpaces should be(
          """final case class NoProps()(val additionalProperties: Option[NoProps.AdditionalProperties] = None) extends EmptyBase"""
        )

        noProps.source.companion.map(_.toString().stripTrailingSpaces) should be(
          Some(
            """object NoProps {
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
              |    val propertyNames: Seq[String] = Seq()
              |    val allowedNames: Seq[Regex] = Seq()
              |    import io.circe._
              |    import io.circe.generic.semiauto._
              |    implicit lazy val decoder: Decoder[Option[AdditionalProperties]] = new Decoder[Option[AdditionalProperties]] {
              |      final def apply(c: HCursor): Decoder.Result[Option[AdditionalProperties]] = {
              |        val allKeys = c.keys.fold(Set.empty[String])(_.toSet)
              |        Right(Option(allKeys.filterNot(propertyNames.contains)).filterNot(_.isEmpty).map {
              |          _.foldLeft(scala.collection.immutable.Map.newBuilder[String, Json])({
              |            case (accum, key) =>
              |              c.downField(key).focus.fold(accum) {
              |                v => accum += key -> v
              |              }
              |          })
              |        }.map(b => AdditionalProperties(b.result())))
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
              |  import scraml.Formats._
              |  implicit lazy val decoder: Decoder[NoProps] = new Decoder[NoProps] {
              |    def apply(c: HCursor): Decoder.Result[NoProps] = {
              |      AdditionalProperties.decoder(c).map {
              |        (_additionalProperties: Option[NoProps.AdditionalProperties]) => NoProps()(_additionalProperties)
              |      }
              |    }
              |  }
              |  implicit lazy val encoder: Encoder[NoProps] = new Encoder[NoProps] { final def apply(instance: NoProps): Json = AdditionalProperties.merge(Json.obj("type" -> Json.fromString("nope")), instance.additionalProperties) }
              |}""".stripMargin.stripTrailingSpaces
          )
        )

        noSealedBase.source.source.toString().stripTrailingSpaces should be("trait NoSealedBase")
        noSealedBase.source.companion.map(_.toString().stripTrailingSpaces) should be(
          Some(
            """object NoSealedBase {
               |  import io.circe.Decoder.Result
               |  import io.circe._
               |  implicit lazy val decoder: Decoder[NoSealedBase] = new Decoder[NoSealedBase] {
               |    override def apply(c: HCursor): Result[NoSealedBase] = c.downField("typeId").as[String] match {
               |      case Right("map-like") =>
               |        MapLike.decoder(c)
               |      case Right("other-sub") =>
               |        OtherSub.decoder(c)
               |      case other =>
               |        Left(DecodingFailure(s"unknown discriminator: $other", c.history))
               |    }
               |  }
               |  implicit lazy val encoder: Encoder[NoSealedBase] = new Encoder[NoSealedBase] {
               |    override def apply(nosealedbase: NoSealedBase): Json = nosealedbase match {
               |      case maplike: MapLike =>
               |        MapLike.encoder(maplike)
               |      case othersub: OtherSub =>
               |        OtherSub.encoder(othersub)
               |    }
               |  }
               |}""".stripMargin.stripTrailingSpaces
          )
        )
        mapLike.source.source.toString().stripTrailingSpaces should be(
          s"""final case class MapLike(values: scala.collection.immutable.Map[String, String]) extends NoSealedBase""".stripMargin.stripTrailingSpaces
        )

        mapLike.source.companion.map(_.toString().stripTrailingSpaces) should be(
          Some(
            """object MapLike {
              |  import io.circe._
              |  import io.circe.syntax._
              |  import io.circe.generic.semiauto._
              |  import io.circe.Decoder.Result
              |  implicit lazy val decoder: Decoder[MapLike] = new Decoder[MapLike] { override def apply(c: HCursor): Result[MapLike] = c.as[scala.collection.immutable.Map[String, String]].map(MapLike.apply) }
              |  implicit lazy val encoder: Encoder[MapLike] = new Encoder[MapLike] { override def apply(a: MapLike): Json = a.values.asJson }
              |}""".stripMargin.stripTrailingSpaces
          )
        )

        someEnum.source.source.toString().stripTrailingSpaces should be(
          s"""sealed trait SomeEnum""".stripMargin.stripTrailingSpaces
        )

        someEnum.source.companion.map(_.toString().stripTrailingSpaces) should be(
          Some(
            """object SomeEnum {
              |  case object A extends SomeEnum
              |  case object B extends SomeEnum
              |  import io.circe._
              |  implicit lazy val encoder: Encoder[SomeEnum] = Encoder[String].contramap({
              |    case A => "A"
              |    case B => "B"
              |  })
              |  implicit lazy val decoder: Decoder[SomeEnum] = Decoder[String].emap({
              |    case "A" =>
              |      Right(A)
              |    case "B" =>
              |      Right(B)
              |    case other =>
              |      Left(s"invalid enum value: $other")
              |  })
              |}""".stripMargin.stripTrailingSpaces
          )
        )

        otherSub.source.source.toString().stripTrailingSpaces should be(
          """final case class OtherSub(id: String)(val additionalProperties: Option[OtherSub.AdditionalProperties] = None) extends NoSealedBase"""
        )

        otherSub.source.companion.map(_.toString().stripTrailingSpaces) should be(
          Some(
            """object OtherSub {
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
              |    val propertyNames: Seq[String] = Seq("id")
              |    val allowedNames: Seq[Regex] = Seq()
              |    import io.circe._
              |    import io.circe.generic.semiauto._
              |    implicit lazy val decoder: Decoder[Option[AdditionalProperties]] = new Decoder[Option[AdditionalProperties]] {
              |      final def apply(c: HCursor): Decoder.Result[Option[AdditionalProperties]] = {
              |        val allKeys = c.keys.fold(Set.empty[String])(_.toSet)
              |        Right(Option(allKeys.filterNot(propertyNames.contains)).filterNot(_.isEmpty).map {
              |          _.foldLeft(scala.collection.immutable.Map.newBuilder[String, Json])({
              |            case (accum, key) =>
              |              c.downField(key).focus.fold(accum) {
              |                v => accum += key -> v
              |              }
              |          })
              |        }.map(b => AdditionalProperties(b.result())))
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
              |  import scraml.Formats._
              |  implicit lazy val decoder: Decoder[OtherSub] = new Decoder[OtherSub] {
              |    def apply(c: HCursor): Decoder.Result[OtherSub] = {
              |      c.downField("id").as[String].flatMap { (_id: String) =>
              |        AdditionalProperties.decoder(c).map {
              |          (_additionalProperties: Option[OtherSub.AdditionalProperties]) => OtherSub(_id)(_additionalProperties)
              |        }
              |      }
              |    }
              |  }
              |  implicit lazy val encoder: Encoder[OtherSub] = new Encoder[OtherSub] { final def apply(instance: OtherSub): Json = AdditionalProperties.merge(Json.obj("typeId" -> Json.fromString("other-sub"), "id" -> instance.id.asJson), instance.additionalProperties) }
              |}""".stripMargin.stripTrailingSpaces
          )
        )

        packageObject.source.source.toString.stripTrailingSpaces should be(
          """package object scraml {
            |  import io.circe.Decoder.Result
            |  import io.circe.{ HCursor, Json, Decoder, Encoder }
            |  implicit def eitherEncoder[A, B](implicit aEncoder: Encoder[A], bEncoder: Encoder[B]): Encoder[Either[A, B]] = new Encoder[Either[A, B]] {
            |    override def apply(a: Either[A, B]): Json = a match {
            |      case Right(b) =>
            |        bEncoder(b)
            |      case Left(a) =>
            |        aEncoder(a)
            |    }
            |  }
            |  implicit def eitherDecoder[A, B](implicit aDecoder: Decoder[A], bDecoder: Decoder[B]): Decoder[Either[A, B]] = new Decoder[Either[A, B]] { override def apply(c: HCursor): Result[Either[A, B]] = aDecoder.either(bDecoder)(c) }
            |  object Formats { implicit lazy val localDateTime = io.circe.Decoder.decodeLocalDateTime }
            |}""".stripMargin.stripTrailingSpaces
        )

        intermediateType.source.source.toString().stripTrailingSpaces should be(
          "sealed trait IntermediateType extends BaseType { def id: String }"
        )
        intermediateType.source.companion.map(_.toString().stripTrailingSpaces) should be(
          Some("""object IntermediateType {
                  |  import io.circe.Decoder.Result
                  |  import io.circe._
                  |  implicit lazy val decoder: Decoder[IntermediateType] = new Decoder[IntermediateType] {
                  |    override def apply(c: HCursor): Result[IntermediateType] = c.downField("type").as[String] match {
                  |      case Right("grandchild") =>
                  |        GrandchildType.decoder(c)
                  |      case other =>
                  |        Left(DecodingFailure(s"unknown discriminator: $other", c.history))
                  |    }
                  |  }
                  |  implicit lazy val encoder: Encoder[IntermediateType] = new Encoder[IntermediateType] {
                  |    override def apply(intermediatetype: IntermediateType): Json = intermediatetype match {
                  |      case grandchildtype: GrandchildType =>
                  |        GrandchildType.encoder(grandchildtype)
                  |    }
                  |  }
                  |}""".stripMargin.stripTrailingSpaces)
        )

        grandchildType.source.source.toString().stripTrailingSpaces should be(
          "final case class GrandchildType(id: String, foo: Option[String] = None, aDouble: scala.math.BigDecimal, aFloat: scala.math.BigDecimal, anInt: Int, aLong: scala.math.BigInt, customTypeProp: scala.math.BigDecimal, customArrayTypeProp: Vector[scala.math.BigDecimal] = Vector.empty)(val additionalProperties: Option[GrandchildType.AdditionalProperties] = None) extends IntermediateType"
        )
        grandchildType.source.companion.map(_.toString().stripTrailingSpaces) should be(
          Some(
            """object GrandchildType {
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
              |    val propertyNames: Seq[String] = Seq("id", "foo", "aDouble", "aFloat", "anInt", "aLong", "customTypeProp", "customArrayTypeProp")
              |    val allowedNames: Seq[Regex] = Seq()
              |    import io.circe._
              |    import io.circe.generic.semiauto._
              |    implicit lazy val decoder: Decoder[Option[AdditionalProperties]] = new Decoder[Option[AdditionalProperties]] {
              |      final def apply(c: HCursor): Decoder.Result[Option[AdditionalProperties]] = {
              |        val allKeys = c.keys.fold(Set.empty[String])(_.toSet)
              |        Right(Option(allKeys.filterNot(propertyNames.contains)).filterNot(_.isEmpty).map {
              |          _.foldLeft(scala.collection.immutable.Map.newBuilder[String, Json])({
              |            case (accum, key) =>
              |              c.downField(key).focus.fold(accum) {
              |                v => accum += key -> v
              |              }
              |          })
              |        }.map(b => AdditionalProperties(b.result())))
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
              |  import scraml.Formats._
              |  implicit lazy val decoder: Decoder[GrandchildType] = new Decoder[GrandchildType] {
              |    def apply(c: HCursor): Decoder.Result[GrandchildType] = {
              |      c.downField("id").as[String].flatMap { (_id: String) =>
              |        c.downField("foo").as[Option[String]].flatMap { (_foo: Option[String]) =>
              |          c.downField("aDouble").as[scala.math.BigDecimal].flatMap { (_aDouble: scala.math.BigDecimal) =>
              |            c.downField("aFloat").as[scala.math.BigDecimal].flatMap { (_aFloat: scala.math.BigDecimal) =>
              |              c.downField("anInt").as[Int].flatMap { (_anInt: Int) =>
              |                c.downField("aLong").as[scala.math.BigInt].flatMap { (_aLong: scala.math.BigInt) =>
              |                  c.downField("customTypeProp").as[scala.math.BigDecimal].flatMap { (_customTypeProp: scala.math.BigDecimal) =>
              |                    c.downField("customArrayTypeProp").as[Vector[scala.math.BigDecimal]].flatMap { (_customArrayTypeProp: Vector[scala.math.BigDecimal]) =>
              |                      AdditionalProperties.decoder(c).map {
              |                        (_additionalProperties: Option[GrandchildType.AdditionalProperties]) => GrandchildType(_id, _foo, _aDouble, _aFloat, _anInt, _aLong, _customTypeProp, _customArrayTypeProp)(_additionalProperties)
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
              |  implicit lazy val encoder: Encoder[GrandchildType] = new Encoder[GrandchildType] { final def apply(instance: GrandchildType): Json = AdditionalProperties.merge(Json.obj("type" -> Json.fromString("grandchild"), "id" -> instance.id.asJson, "foo" -> instance.foo.asJson, "aDouble" -> instance.aDouble.asJson, "aFloat" -> instance.aFloat.asJson, "anInt" -> instance.anInt.asJson, "aLong" -> instance.aLong.asJson, "customTypeProp" -> instance.customTypeProp.asJson, "customArrayTypeProp" -> instance.customArrayTypeProp.asJson), instance.additionalProperties) }
              |}""".stripMargin.stripTrailingSpaces
          )
        )

        defaultProperty.source.source.toString().stripTrailingSpaces should be(
          """final case class DefaultProperty(message: String = "this is a default message", limit: Option[Int] = Some(2), requiredEnum: SomeEnum = SomeEnum.B, optionalEnum: Option[SomeEnum] = Some(SomeEnum.A), constrained: String = "AA", longInteger: scala.math.BigInt = -9223372036854775808L, longNumber: Option[scala.math.BigInt] = Some(-9223372036854775808L))(val additionalProperties: Option[DefaultProperty.AdditionalProperties] = None)"""
        )
        defaultProperty.source.companion.map(_.toString().stripTrailingSpaces) should be(
          Some(
            s"""object DefaultProperty {
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
               |    val propertyNames: Seq[String] = Seq("message", "limit", "requiredEnum", "optionalEnum", "constrained", "longInteger", "longNumber")
               |    val allowedNames: Seq[Regex] = Seq()
               |    import io.circe._
               |    import io.circe.generic.semiauto._
               |    implicit lazy val decoder: Decoder[Option[AdditionalProperties]] = new Decoder[Option[AdditionalProperties]] {
               |      final def apply(c: HCursor): Decoder.Result[Option[AdditionalProperties]] = {
               |        val allKeys = c.keys.fold(Set.empty[String])(_.toSet)
               |        Right(Option(allKeys.filterNot(propertyNames.contains)).filterNot(_.isEmpty).map {
               |          _.foldLeft(scala.collection.immutable.Map.newBuilder[String, Json])({
               |            case (accum, key) =>
               |              c.downField(key).focus.fold(accum) {
               |                v => accum += key -> v
               |              }
               |          })
               |        }.map(b => AdditionalProperties(b.result())))
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
               |  import scraml.Formats._
               |  implicit lazy val decoder: Decoder[DefaultProperty] = new Decoder[DefaultProperty] {
               |    def apply(c: HCursor): Decoder.Result[DefaultProperty] = {
               |      c.getOrElse[String]("message")("this is a default message").flatMap { (_message: String) =>
               |        c.getOrElse[Option[Int]]("limit")(Some(2)).flatMap { (_limit: Option[Int]) =>
               |          c.getOrElse[SomeEnum]("requiredEnum")(SomeEnum.B).flatMap { (_requiredEnum: SomeEnum) =>
               |            c.getOrElse[Option[SomeEnum]]("optionalEnum")(Some(SomeEnum.A)).flatMap { (_optionalEnum: Option[SomeEnum]) =>
               |              c.getOrElse[String]("constrained")("AA").flatMap { (_constrained: String) =>
               |                c.getOrElse[scala.math.BigInt]("longInteger")(-9223372036854775808L).flatMap { (_longInteger: scala.math.BigInt) =>
               |                  c.getOrElse[Option[scala.math.BigInt]]("longNumber")(Some(-9223372036854775808L)).flatMap { (_longNumber: Option[scala.math.BigInt]) =>
               |                    AdditionalProperties.decoder(c).map {
               |                      (_additionalProperties: Option[DefaultProperty.AdditionalProperties]) => DefaultProperty(_message, _limit, _requiredEnum, _optionalEnum, _constrained, _longInteger, _longNumber)(_additionalProperties)
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
               |  implicit lazy val encoder: Encoder[DefaultProperty] = new Encoder[DefaultProperty] { final def apply(instance: DefaultProperty): Json = AdditionalProperties.merge(Json.obj("message" -> instance.message.asJson, "limit" -> instance.limit.asJson, "requiredEnum" -> instance.requiredEnum.asJson, "optionalEnum" -> instance.optionalEnum.asJson, "constrained" -> instance.constrained.asJson, "longInteger" -> instance.longInteger.asJson, "longNumber" -> instance.longNumber.asJson), instance.additionalProperties) }
               |}""".stripMargin.stripTrailingSpaces
          )
        )
    }
  }
}
