package scraml.libs

import org.scalatest.diagrams.Diagrams
import org.scalatest.wordspec.AnyWordSpec
import scraml.{DefaultModelGen, ModelGenParams, ModelGenRunner}

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
        librarySupport = Set(TapirSupport("Endpoints")),
        formatConfig = None,
        generateDateCreated = true
      )

      val generated = ModelGenRunner.run(DefaultModelGen)(params).unsafeRunSync()

      generated.packageObject.source.source.toString should be(s"""package object scraml {
           |  object Endpoints {
           |    import sttp.tapir._
           |    val greeting = endpoint.in("greeting")
           |  }
           |}""".stripMargin)
    }

    "generate ct api endpoints" in {
      val params = ModelGenParams(
        new File("src/sbt-test/sbt-scraml/ct-api/reference/api-specs/api/api.raml"),
        new File("target/scraml-tapir-ct-api-test"),
        "scraml",
        librarySupport = Set(TapirSupport("Endpoints")),
        formatConfig = None,
        generateDateCreated = true
      )

      ModelGenRunner.run(DefaultModelGen)(params).unsafeRunSync()
    }
  }
}
