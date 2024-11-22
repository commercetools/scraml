package scraml.libs

import cats.effect.unsafe.implicits.global
import org.scalatest.diagrams.Diagrams
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import scraml.{DefaultModelGen, DefaultTypes, FieldMatchPolicy, ModelGenParams, ModelGenRunner}
import java.io.File

final class TapirSupportSpec
    extends AnyWordSpec
    with Diagrams
    with Matchers
    with SourceCodeFormatting {
  "TapirSupport" must {
    "generate simple endpoints" in {
      val params = ModelGenParams(
        new File("src/sbt-test/sbt-scraml/tapir/api/tapir.raml"),
        new File("target/scraml-tapir-test"),
        "scraml",
        FieldMatchPolicy.Exact(),
        DefaultTypes(),
        librarySupport = Set(CirceJsonSupport(), TapirSupport("Endpoints")),
        formatConfig = None
      )

      val generated = ModelGenRunner.run(DefaultModelGen)(params).unsafeRunSync()

      generated.packageObject.source.source.toString should be(
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
        |  import sttp.tapir._
        |  import sttp.model._
        |  import sttp.tapir.CodecFormat.TextPlain
        |  import sttp.tapir.json.circe._
        |  import sttp.tapir.generic.auto._
        |  type |[+A1, +A2] = Either[A1, A2]
        |  private implicit def anySchema[T]: Schema[T] = Schema[T](SchemaType.SCoproduct(Nil, None)(_ => None), None)
        |  private implicit def eitherTapirCodecPlain[A, B](implicit aCodec: Codec.PlainCodec[A], bCodec: Codec.PlainCodec[B]): Codec.PlainCodec[Either[A, B]] = new Codec.PlainCodec[Either[A, B]] {
        |    override val format = TextPlain()
        |    override val schema = anySchema[Either[A, B]]
        |    override def rawDecode(l: String): DecodeResult[Either[A, B]] = {
        |      aCodec.rawDecode(l) match {
        |        case e: DecodeResult.Failure =>
        |          bCodec.rawDecode(l).map(Right(_))
        |        case other =>
        |          other.map(Left(_))
        |      }
        |    }
        |    override def encode(h: Either[A, B]): String = {
        |      h match {
        |        case Left(a) =>
        |          aCodec.encode(a)
        |        case Right(b) =>
        |          bCodec.encode(b)
        |      }
        |    }
        |  }
        |  private implicit val queryOptionalCollectionCodec: Codec[List[String], Option[scala.collection.immutable.List[String]], TextPlain] = new Codec[List[String], Option[scala.collection.immutable.List[String]], TextPlain] {
        |    override def rawDecode(l: List[String]): DecodeResult[Option[scala.collection.immutable.List[String]]] = DecodeResult.Value(Some(l.to[scala.collection.immutable.List]))
        |    override def encode(h: Option[scala.collection.immutable.List[String]]): List[String] = h.map(_.to[List]).getOrElse(Nil)
        |    override lazy val schema: Schema[Option[scala.collection.immutable.List[String]]] = Schema.binary
        |    override lazy val format: TextPlain = TextPlain()
        |  }
        |  object Endpoints {
        |    object Greeting {
        |      final case class GetGreetingParams(enum_type: SomeEnum, name: Option[String] = None)
        |      lazy val getGreeting = endpoint.get.in("greeting").in(query[SomeEnum]("enum_type") and query[Option[String]]("name")).mapInTo[GetGreetingParams].out(jsonBody[DataType])
        |    }
        |  }
        |}""".stripMargin
      )
    }

    "generate enumeration types" in {
      val params = ModelGenParams(
        new File("src/sbt-test/sbt-scraml/simple/api/simple.raml"),
        new File("target/scraml-tapir-test"),
        "scraml",
        FieldMatchPolicy.Exact(),
        DefaultTypes(),
        librarySupport = Set(CirceJsonSupport(), TapirSupport("Endpoints")),
        formatConfig = None,
        generateDefaultEnumVariant = None
      )

      val generated = ModelGenRunner.run(DefaultModelGen)(params).unsafeRunSync()
      val enumCompanion =
        generated.files.find(_.source.name == "SomeEnum").flatMap(_.source.companion)

      enumCompanion.map(_.toString.stripTrailingSpaces) should be(
        Some(
          """object SomeEnum {
            |  case object A extends SomeEnum
            |  case object B extends SomeEnum
            |  case object ENUM extends SomeEnum
            |  case object TYPE extends SomeEnum
            |  import io.circe._
            |  implicit lazy val encoder: Encoder[SomeEnum] = Encoder[String].contramap({
            |    case A => "A"
            |    case B => "B"
            |    case ENUM => "enum"
            |    case TYPE => "type"
            |  })
            |  implicit lazy val decoder: Decoder[SomeEnum] = Decoder[String].emap({
            |    case "A" =>
            |      Right(A)
            |    case "B" =>
            |      Right(B)
            |    case "enum" =>
            |      Right(ENUM)
            |    case "type" =>
            |      Right(TYPE)
            |    case other =>
            |      Left(s"invalid enum value: $other")
            |  })
            |  implicit lazy val tapirCodec: sttp.tapir.Codec.PlainCodec[SomeEnum] = sttp.tapir.Codec.string.mapDecode[SomeEnum]({
            |    case "A" =>
            |      sttp.tapir.DecodeResult.Value(A)
            |    case "B" =>
            |      sttp.tapir.DecodeResult.Value(B)
            |    case "enum" =>
            |      sttp.tapir.DecodeResult.Value(ENUM)
            |    case "type" =>
            |      sttp.tapir.DecodeResult.Value(TYPE)
            |    case other =>
            |      sttp.tapir.DecodeResult.InvalidValue(sttp.tapir.ValidationError[String](sttp.tapir.Validator.enumeration(List("A", "B", "enum", "type")), other) :: Nil)
            |  })({
            |    case A => "A"
            |    case B => "B"
            |    case ENUM => "enum"
            |    case TYPE => "type"
            |  })
            |}""".stripMargin
        )
      )
    }

    "generate enumeration types with default variant" in {
      val params = ModelGenParams(
        new File("src/sbt-test/sbt-scraml/simple/api/simple.raml"),
        new File("target/scraml-tapir-test"),
        "scraml",
        FieldMatchPolicy.Exact(),
        DefaultTypes(),
        librarySupport = Set(CirceJsonSupport(), TapirSupport("Endpoints")),
        formatConfig = None,
        generateDefaultEnumVariant = Some("Unknown")
      )

      val generated = ModelGenRunner.run(DefaultModelGen)(params).unsafeRunSync()
      val enumCompanion =
        generated.files.find(_.source.name == "SomeEnum").flatMap(_.source.companion)

      enumCompanion.map(_.toString.stripTrailingSpaces) should be(
        Some(
          """object SomeEnum {
            |  case object A extends SomeEnum
            |  case object B extends SomeEnum
            |  case object ENUM extends SomeEnum
            |  case object TYPE extends SomeEnum
            |  case class Unknown(value: String) extends SomeEnum
            |  import io.circe._
            |  implicit lazy val encoder: Encoder[SomeEnum] = Encoder[String].contramap({
            |    case A => "A"
            |    case B => "B"
            |    case ENUM => "enum"
            |    case TYPE => "type"
            |    case Unknown(value) => value
            |  })
            |  implicit lazy val decoder: Decoder[SomeEnum] = Decoder[String].emap({
            |    case "A" =>
            |      Right(A)
            |    case "B" =>
            |      Right(B)
            |    case "enum" =>
            |      Right(ENUM)
            |    case "type" =>
            |      Right(TYPE)
            |    case other =>
            |      Right(Unknown(other))
            |  })
            |  implicit lazy val tapirCodec: sttp.tapir.Codec.PlainCodec[SomeEnum] = sttp.tapir.Codec.string.mapDecode[SomeEnum]({
            |    case "A" =>
            |      sttp.tapir.DecodeResult.Value(A)
            |    case "B" =>
            |      sttp.tapir.DecodeResult.Value(B)
            |    case "enum" =>
            |      sttp.tapir.DecodeResult.Value(ENUM)
            |    case "type" =>
            |      sttp.tapir.DecodeResult.Value(TYPE)
            |    case other =>
            |      sttp.tapir.DecodeResult.Value(Unknown(other))
            |  })({
            |    case A => "A"
            |    case B => "B"
            |    case ENUM => "enum"
            |    case TYPE => "type"
            |    case Unknown(value) => value
            |  })
            |}""".stripMargin
        )
      )
    }

    "generate complex endpoints" in {
      val params = ModelGenParams(
        new File("src/sbt-test/sbt-scraml/tapir/api/tapir-complex.raml"),
        new File("target/scraml-tapir-complex-test"),
        "scraml",
        FieldMatchPolicy.Exact(),
        DefaultTypes(),
        librarySupport = Set(CirceJsonSupport(), RefinedSupport, TapirSupport("Endpoints")),
        formatConfig = None
      )

      val generated = ModelGenRunner.run(DefaultModelGen)(params).unsafeRunSync()

      generated.packageObject.source.source.toString should be(
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
          |  import sttp.tapir._
          |  import sttp.model._
          |  import sttp.tapir.CodecFormat.TextPlain
          |  import sttp.tapir.json.circe._
          |  import sttp.tapir.generic.auto._
          |  type |[+A1, +A2] = Either[A1, A2]
          |  private implicit def anySchema[T]: Schema[T] = Schema[T](SchemaType.SCoproduct(Nil, None)(_ => None), None)
          |  private implicit def eitherTapirCodecPlain[A, B](implicit aCodec: Codec.PlainCodec[A], bCodec: Codec.PlainCodec[B]): Codec.PlainCodec[Either[A, B]] = new Codec.PlainCodec[Either[A, B]] {
          |    override val format = TextPlain()
          |    override val schema = anySchema[Either[A, B]]
          |    override def rawDecode(l: String): DecodeResult[Either[A, B]] = {
          |      aCodec.rawDecode(l) match {
          |        case e: DecodeResult.Failure =>
          |          bCodec.rawDecode(l).map(Right(_))
          |        case other =>
          |          other.map(Left(_))
          |      }
          |    }
          |    override def encode(h: Either[A, B]): String = {
          |      h match {
          |        case Left(a) =>
          |          aCodec.encode(a)
          |        case Right(b) =>
          |          bCodec.encode(b)
          |      }
          |    }
          |  }
          |  private implicit val queryOptionalCollectionCodec: Codec[List[String], Option[scala.collection.immutable.List[String]], TextPlain] = new Codec[List[String], Option[scala.collection.immutable.List[String]], TextPlain] {
          |    override def rawDecode(l: List[String]): DecodeResult[Option[scala.collection.immutable.List[String]]] = DecodeResult.Value(Some(l.to[scala.collection.immutable.List]))
          |    override def encode(h: Option[scala.collection.immutable.List[String]]): List[String] = h.map(_.to[List]).getOrElse(Nil)
          |    override lazy val schema: Schema[Option[scala.collection.immutable.List[String]]] = Schema.binary
          |    override lazy val format: TextPlain = TextPlain()
          |  }
          |  object Endpoints {
          |    object Greeting {
          |      final case class GetGreetingByPreambleAndDelayParams(preamble: Preamble, delay: Int, name: Option[String] = None, repeat: Option[Int] = None, uppercase: Option[Boolean] = None)
          |      lazy val getGreetingByPreambleAndDelay = endpoint.get.in("greeting" / path[Preamble]("preamble") / path[Int]("delay")).in(query[Option[String]]("name") and query[Option[Int]]("repeat") and query[Option[Boolean]]("uppercase")).mapInTo[GetGreetingByPreambleAndDelayParams].out(jsonBody[DataType])
          |    }
          |    object Upload { lazy val postUpload = endpoint.post.in("upload").in(inputStreamBody) }
          |  }
          |}""".stripMargin.stripTrailingSpaces
      )
    }

    "generate ct api endpoints" in {
      val params = ModelGenParams(
        new File("src/sbt-test/sbt-scraml/ct-api/reference/api-specs/api/api.raml"),
        new File("target/scraml-tapir-ct-api-test"),
        "scraml",
        FieldMatchPolicy.Exact(),
        DefaultTypes(),
        librarySupport = Set(scraml.libs.CirceJsonSupport(), TapirSupport("Endpoints")),
        formatConfig = None
      )

      ModelGenRunner.run(DefaultModelGen)(params).unsafeRunSync()
    }
  }
}
