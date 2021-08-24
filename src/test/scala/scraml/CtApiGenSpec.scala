package scraml

import cats.effect.unsafe.implicits.global
import io.circe.Decoder.Result
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.File
import io.circe._
import scraml.libs.CirceJsonSupport

class CtApiGenSpec extends AnyFlatSpec with Matchers {
  "Default model gen" should "generate ct API" in {
    val params = ModelGenParams(
      new File("src/sbt-test/sbt-scraml/ct-api/reference/api-specs/api/api.raml"),
      new File("target/scraml-test-ct"),
      "scraml",
      librarySupport = Set(CirceJsonSupport),
      None
    )

    println(ModelGenRunner.run(DefaultModelGen)(params).unsafeRunSync())
  }
}
