package scraml.libs

import org.scalatest.diagrams.Diagrams
import org.scalatest.wordspec.AnyWordSpec
import scraml.{DefaultModelGen, DefaultTypes, ModelGenParams, ModelGenRunner}
import java.io.File

import cats.effect.unsafe.implicits.global
import org.scalatest.matchers.should.Matchers

final class TapirSupportSpec extends AnyWordSpec with Diagrams with Matchers {
  "TapirSupport" must {
    "generate simple endpoints" in {
      val params = ModelGenParams(
        new File("src/sbt-test/sbt-scraml/simple/api/simple.raml"),
        new File("target/scraml-tapir-test"),
        "scraml",
        DefaultTypes(),
        librarySupport = Set(CirceJsonSupport(), TapirSupport("Endpoints")),
        formatConfig = None
      )

      val generated = ModelGenRunner.run(DefaultModelGen)(params).unsafeRunSync()

      generated.packageObject.source.source.toString should be(s"""package object scraml {
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
                                                                  |  type |[+A1, +A2] = Either[A1, A2]
                                                                  |  private implicit def anySchema[T]: Schema[T] = Schema[T](SchemaType.SCoproduct(Nil, None)(_ => None), None)
                                                                  |  private implicit val queryOptionalCollectionCodec: Codec[List[String], Option[scala.collection.immutable.List[String]], TextPlain] = new Codec[List[String], Option[scala.collection.immutable.List[String]], TextPlain] {
                                                                  |    override def rawDecode(l: List[String]): DecodeResult[Option[scala.collection.immutable.List[String]]] = DecodeResult.Value(Some(l.to[scala.collection.immutable.List]))
                                                                  |    override def encode(h: Option[scala.collection.immutable.List[String]]): List[String] = h.map(_.to[List]).getOrElse(Nil)
                                                                  |    override lazy val schema: Schema[Option[scala.collection.immutable.List[String]]] = Schema.binary
                                                                  |    override lazy val format: TextPlain = TextPlain()
                                                                  |  }
                                                                  |  object Endpoints {
                                                                  |    object Greeting {
                                                                  |      final case class GetGreetingParams(name: Option[String] = None)
                                                                  |      val getGreeting = endpoint.in("greeting").in(query[Option[String]]("name")).mapInTo[GetGreetingParams].get.out(jsonBody[DataType])
                                                                  |    }
                                                                  |  }
                                                                  |}""".stripMargin)
    }

    "generate ct api endpoints" in {
      val params = ModelGenParams(
        new File("src/sbt-test/sbt-scraml/ct-api/reference/api-specs/api/api.raml"),
        new File("target/scraml-tapir-ct-api-test"),
        "scraml",
        DefaultTypes(),
        librarySupport = Set(scraml.libs.CirceJsonSupport(), TapirSupport("Endpoints")),
        formatConfig = None
      )

      ModelGenRunner.run(DefaultModelGen)(params).unsafeRunSync()
    }
  }
}
