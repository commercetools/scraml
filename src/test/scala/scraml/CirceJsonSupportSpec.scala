package scraml

import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.File

class CirceJsonSupportSpec extends AnyFlatSpec with Matchers {
  "Circe JSON Support" should "generate JSON derivation" in {
    val params = ModelGenParams(
      new File(getClass.getClassLoader.getResource("json/json.raml").toURI),
      new File("target/scraml-circe-json-test"),
      "scraml",
      jsonSupport = Some(Circe),
      librarySupport = Set.empty,
      None
    )

    val generated = ModelGenRunner.run(DefaultModelGen)(params).unsafeRunSync()

    generated.files match {
      case noDiscBase :: _ :: _ :: baseType :: dataType :: emptyBase :: noProps :: packageObject :: Nil =>
        noDiscBase.source.source.toString() should be("sealed trait NoDiscriminatorBase")
        noDiscBase.source.companion.map(_.toString()) should be(Some(
          s"""object NoDiscriminatorBase {
             |  import io.circe._
             |  import io.circe.generic.semiauto._
             |  implicit lazy val json: Codec[NoDiscriminatorBase] = deriveCodec[NoDiscriminatorBase]
             |}""".stripMargin))

        baseType.source.packageName should be("datatypes")
        baseType.source.source.toString() should be("sealed trait BaseType extends Any { def id: String }")
        baseType.source.companion.map(_.toString()) should be(Some(
          s"""object BaseType {
             |  import io.circe._
             |  import io.circe.generic.semiauto._
             |  implicit lazy val json: Codec[BaseType] = deriveCodec[BaseType]
             |}""".stripMargin))

        baseType.source.name should be("BaseType")
        baseType.file.getPath should be("target/scraml-sphere-json-test/scraml/datatypes.scala")

        dataType.source.packageName should be("datatypes")
        dataType.source.source.toString() should be("final case class DataType(id: String, foo: Option[String] = None, customTypeProp: scala.long.BigDecimal, customArrayTypeProp: Vector[scala.long.BigDecimal] = Vector.empty) extends BaseType")
        dataType.source.name should be("DataType")
        dataType.source.companion.map(_.toString()) should be(Some(
          s"""object DataType {
             |  import io.circe._
             |  import io.circe.generic.semiauto._
             |  implicit lazy val json: Codec[DataType] = deriveCodec[DataType]
             |}""".stripMargin))

        emptyBase.source.source.toString() should be("sealed trait EmptyBase")
        noProps.source.source.toString() should be(s"""case object NoProps extends EmptyBase""".stripMargin)

        packageObject.source.source.toString should be(
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
             |}""".stripMargin)
    }
  }
}
