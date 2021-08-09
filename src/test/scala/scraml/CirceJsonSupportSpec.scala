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
      jsonSupport = Some(Circe),
      librarySupport = Set.empty,
      None
    )

    val generated = ModelGenRunner.run(DefaultModelGen)(params).unsafeRunSync()

    generated.files match {
      case noDiscBase :: _ :: _ :: baseType :: dataType :: emptyBase :: noProps :: noSealedBase :: otherSub :: packageObject :: Nil =>
        noDiscBase.source.source.toString() should be("sealed trait NoDiscriminatorBase")
        noDiscBase.source.companion.map(_.toString()) should be(
          Some(s"""object NoDiscriminatorBase {
                  |  import io.circe.Decoder.Result
                  |  import io.circe._
                  |  implicit def decodeAll(implicit NoDiscriminatorSub1Decoder: Decoder[NoDiscriminatorSub1], NoDiscriminatorSub2Decoder: Decoder[NoDiscriminatorSub2]): Decoder[NoDiscriminatorBase] = new Decoder[NoDiscriminatorBase] { override def apply(c: HCursor): Result[NoDiscriminatorBase] = NoDiscriminatorSub1Decoder.tryDecode(c).fold(_ => NoDiscriminatorSub2Decoder.tryDecode(c), Right(_)) }
                  |  implicit def encodeAll(implicit NoDiscriminatorSub1Encoder: Encoder[NoDiscriminatorSub1], NoDiscriminatorSub2Encoder: Encoder[NoDiscriminatorSub2]): Encoder[NoDiscriminatorBase] = new Encoder[NoDiscriminatorBase] {
                  |    override def apply(nodiscriminatorbase: NoDiscriminatorBase): Json = nodiscriminatorbase match {
                  |      case nodiscriminatorsub1: NoDiscriminatorSub1 =>
                  |        NoDiscriminatorSub1Encoder(nodiscriminatorsub1)
                  |      case nodiscriminatorsub2: NoDiscriminatorSub2 =>
                  |        NoDiscriminatorSub2Encoder(nodiscriminatorsub2)
                  |    }
                  |  }
                  |}""".stripMargin)
        )

        baseType.source.packageName should be("datatypes")
        baseType.source.source.toString() should be(
          "sealed trait BaseType extends Any { def id: String }"
        )
        baseType.source.companion.map(_.toString()) should be(Some(s"""object BaseType {
                                                                      |  import io.circe.Decoder.Result
                                                                      |  import io.circe._
                                                                      |  implicit def decodeAll(implicit DataTypeDecoder: Decoder[DataType]): Decoder[BaseType] = new Decoder[BaseType] {
                                                                      |    override def apply(c: HCursor): Result[BaseType] = c.downField("type").as[String] match {
                                                                      |      case Right("data") =>
                                                                      |        DataTypeDecoder(c)
                                                                      |      case other =>
                                                                      |        Left(DecodingFailure(s"unknown discriminator: $$other", c.history))
                                                                      |    }
                                                                      |  }
                                                                      |  implicit def encodeAll(implicit DataTypeEncoder: Encoder[DataType]): Encoder[BaseType] = new Encoder[BaseType] {
                                                                      |    override def apply(basetype: BaseType): Json = basetype match {
                                                                      |      case datatype: DataType =>
                                                                      |        DataTypeEncoder(datatype).mapObject(_.add("type", JString("data")))
                                                                      |    }
                                                                      |  }
                                                                      |}""".stripMargin))

        baseType.source.name should be("BaseType")
        baseType.file.getPath should be("target/scraml-circe-json-test/scraml/datatypes.scala")

        dataType.source.packageName should be("datatypes")
        dataType.source.source.toString() should be(
          "final case class DataType(id: String, foo: Option[String] = None, customTypeProp: scala.math.BigDecimal, customArrayTypeProp: Vector[scala.math.BigDecimal] = Vector.empty) extends BaseType"
        )
        dataType.source.name should be("DataType")
        dataType.source.companion.map(_.toString()) should be(Some(s"""object DataType {
             |  import io.circe._
             |  import io.circe.generic.semiauto._
             |  implicit lazy val json: Codec[DataType] = deriveCodec[DataType]
             |}""".stripMargin))

        emptyBase.source.source.toString() should be("sealed trait EmptyBase")
        noProps.source.source.toString() should be(
          s"""case object NoProps extends EmptyBase""".stripMargin
        )

        noSealedBase.source.source.toString() should be("trait NoSealedBase")
        noSealedBase.source.companion.map(_.toString()) should be(Some(s"""object NoSealedBase {
                                                                          |  import io.circe.Decoder.Result
                                                                          |  import io.circe._
                                                                          |  implicit def decodeAll(implicit OtherSubDecoder: Decoder[OtherSub]): Decoder[NoSealedBase] = new Decoder[NoSealedBase] {
                                                                          |    override def apply(c: HCursor): Result[NoSealedBase] = c.downField("type").as[String] match {
                                                                          |      case Right("other-sub") =>
                                                                          |        OtherSubDecoder(c)
                                                                          |      case other =>
                                                                          |        Left(DecodingFailure(s"unknown discriminator: $$other", c.history))
                                                                          |    }
                                                                          |  }
                                                                          |  implicit def encodeAll(implicit OtherSubEncoder: Encoder[OtherSub]): Encoder[NoSealedBase] = new Encoder[NoSealedBase] {
                                                                          |    override def apply(nosealedbase: NoSealedBase): Json = nosealedbase match {
                                                                          |      case othersub: OtherSub =>
                                                                          |        OtherSubEncoder(othersub).mapObject(_.add("type", JString("other-sub")))
                                                                          |    }
                                                                          |  }
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
