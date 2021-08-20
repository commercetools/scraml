package scraml

import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.File

class CirceJsonSupportSpec extends AnyFlatSpec with Matchers {
  "Circe JSON Support" should "generate JSON derivation" in {
    val params = ModelGenParams(
      new File("src/sbt-test/sbt-scraml/json/api/json.raml"),
      new File("target/scraml-circe-json-test"),
      "scraml",
      jsonSupport = Some(Circe()),
      librarySupport = Set.empty,
      None
    )

    val generated = ModelGenRunner.run(DefaultModelGen)(params).unsafeRunSync()

    generated.files match {
      case noDiscBase :: _ :: _ :: baseType :: _ :: _ :: dataType :: emptyBase :: noProps :: noSealedBase :: someEnum :: otherSub :: mapLike :: packageObject :: Nil =>
        noDiscBase.source.source.toString() should be(
          "sealed trait NoDiscriminatorBase { def toJson(): io.circe.Json }"
        )
        noDiscBase.source.companion.map(_.toString()) should be(
          Some("""object NoDiscriminatorBase {
                  |  import io.circe._
                  |  implicit val encodeAll: Encoder[NoDiscriminatorBase] = new Encoder[NoDiscriminatorBase] { override def apply(instance: NoDiscriminatorBase): Json = instance.toJson() }
                  |  implicit def decodeAll(implicit NoDiscriminatorSub1Decoder: Decoder[NoDiscriminatorSub1], NoDiscriminatorSub2Decoder: Decoder[NoDiscriminatorSub2]): Decoder[NoDiscriminatorBase] = new Decoder[NoDiscriminatorBase] {
                  |    override def apply(c: HCursor): Decoder.Result[NoDiscriminatorBase] = {
                  |      NoDiscriminatorSub1Decoder.tryDecode(c).fold(_ => NoDiscriminatorSub2Decoder.tryDecode(c), Right(_))
                  |    }
                  |  }
                  |}""".stripMargin)
        )

        baseType.source.packageName should be("datatypes")
        baseType.source.source.toString() should be(
          """sealed trait BaseType extends Any {
            |  def id: String
            |  def toJson(): io.circe.Json
            |}""".stripMargin
        )
        baseType.source.companion.map(_.toString()) should be(Some("""object BaseType {
                                                                      |  import io.circe.Decoder.Result
                                                                      |  import io.circe._
                                                                      |  implicit val encodeAll: Encoder[BaseType] = new Encoder[BaseType] { override def apply(instance: BaseType): Json = instance.toJson() }
                                                                      |  implicit def decodeAll(implicit DataTypeDecoder: Decoder[DataType], GrandchildTypeDecoder: Decoder[GrandchildType]): Decoder[BaseType] = new Decoder[BaseType] {
                                                                      |    override def apply(c: HCursor): Result[BaseType] = c.downField("type").as[String] match {
                                                                      |      case Right("data") =>
                                                                      |        DataTypeDecoder(c)
                                                                      |      case Right("grandchild") =>
                                                                      |        GrandchildTypeDecoder(c)
                                                                      |      case other =>
                                                                      |        Left(DecodingFailure(s"unknown discriminator: $other", c.history))
                                                                      |    }
                                                                      |  }
                                                                      |}""".stripMargin))

        baseType.source.name should be("BaseType")
        baseType.file.getPath should be("target/scraml-circe-json-test/scraml/datatypes.scala")

        dataType.source.packageName should be("datatypes")
        dataType.source.source.toString() should be(
          """final case class DataType(id: String, foo: Option[String] = None, customTypeProp: scala.math.BigDecimal, customArrayTypeProp: Vector[scala.math.BigDecimal] = Vector.empty) extends BaseType {
            |  import io.circe.{ Codec, Json }
            |  def toJson(): Json = implicitly[Codec[DataType]].apply(this)
            |}""".stripMargin
        )
        dataType.source.name should be("DataType")
        dataType.source.companion.map(_.toString()) should be(Some(s"""object DataType {
             |  import io.circe._
             |  import io.circe.generic.semiauto._
             |  implicit lazy val json: Codec[DataType] = Codec.from[DataType](deriveDecoder[DataType], deriveEncoder[DataType].mapJson(_.mapObject(_.add("type", Json.fromString("data")))))
             |}""".stripMargin))

        emptyBase.source.source.toString() should be(
          "sealed trait EmptyBase { def toJson(): io.circe.Json }"
        )
        emptyBase.source.companion.map(_.toString()) should be(Some("""object EmptyBase {
             |  import io.circe.Decoder.Result
             |  import io.circe._
             |  implicit val encodeAll: Encoder[EmptyBase] = new Encoder[EmptyBase] { override def apply(instance: EmptyBase): Json = instance.toJson() }
             |  implicit def decodeAll(): Decoder[EmptyBase] = new Decoder[EmptyBase] {
             |    override def apply(c: HCursor): Result[EmptyBase] = c.downField("type").as[String] match {
             |      case Right("nope") =>
             |        Right(NoProps)
             |      case other =>
             |        Left(DecodingFailure(s"unknown discriminator: $other", c.history))
             |    }
             |  }
             |}""".stripMargin))

        noProps.source.source.toString() should be(
          """case object NoProps extends EmptyBase {
             |  import io.circe.Json
             |  def toJson(): Json = Json.fromFields(Map("type" -> Json.fromString("nope")))
             |}""".stripMargin
        )

        noSealedBase.source.source.toString() should be(
          "trait NoSealedBase { def toJson(): io.circe.Json }"
        )
        noSealedBase.source.companion.map(_.toString()) should be(Some("""object NoSealedBase {
                                                                          |  import io.circe.Decoder.Result
                                                                          |  import io.circe._
                                                                          |  implicit val encodeAll: Encoder[NoSealedBase] = new Encoder[NoSealedBase] { override def apply(instance: NoSealedBase): Json = instance.toJson() }
                                                                          |  implicit def decodeAll(implicit MapLikeDecoder: Decoder[MapLike], OtherSubDecoder: Decoder[OtherSub]): Decoder[NoSealedBase] = new Decoder[NoSealedBase] {
                                                                          |    override def apply(c: HCursor): Result[NoSealedBase] = c.downField("type").as[String] match {
                                                                          |      case Right("map-like") =>
                                                                          |        MapLikeDecoder(c)
                                                                          |      case Right("other-sub") =>
                                                                          |        OtherSubDecoder(c)
                                                                          |      case other =>
                                                                          |        Left(DecodingFailure(s"unknown discriminator: $other", c.history))
                                                                          |    }
                                                                          |  }
                                                                          |}""".stripMargin))
        mapLike.source.source.toString() should be(
          s"""final case class MapLike(values: Map[String, String]) extends NoSealedBase {
             |  import io.circe.{ Codec, Json }
             |  def toJson(): Json = implicitly[Codec[MapLike]].apply(this)
             |}""".stripMargin
        )

        mapLike.source.companion.map(_.toString()) should be(Some(s"""object MapLike {
             |  import io.circe.syntax._
             |  import io.circe._
             |  import io.circe.Decoder.Result
             |  implicit lazy val json: Codec[MapLike] = new Codec[MapLike] {
             |    override def apply(a: MapLike): Json = a.values.asJson
             |    override def apply(c: HCursor): Result[MapLike] = c.as[Map[String, String]].map(MapLike.apply)
             |  }
             |}""".stripMargin))

        someEnum.source.source.toString() should be(
          s"""sealed trait SomeEnum""".stripMargin
        )

        someEnum.source.companion.map(_.toString()) should be(Some("""object SomeEnum {
                                                                      |  case object A extends SomeEnum
                                                                      |  case object B extends SomeEnum
                                                                      |  import io.circe._
                                                                      |  implicit val encode: Encoder[SomeEnum] = Encoder[String].contramap({
                                                                      |    case A => "A"
                                                                      |    case B => "B"
                                                                      |  })
                                                                      |  implicit val decode: Decoder[SomeEnum] = Decoder[String].emap({
                                                                      |    case "A" =>
                                                                      |      Right(A)
                                                                      |    case "B" =>
                                                                      |      Right(B)
                                                                      |    case other =>
                                                                      |      Left(s"invalid enum value: $other")
                                                                      |  })
                                                                      |}""".stripMargin))

        packageObject.source.source.toString should be(s"""package object scraml {
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
             |}""".stripMargin)
    }
  }
}
