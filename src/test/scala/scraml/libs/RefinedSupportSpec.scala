package scraml.libs

import java.io.File

import cats.effect.unsafe.implicits.global
import org.scalatest.diagrams.Diagrams
import org.scalatest.wordspec.AnyWordSpec
import scraml.{DefaultModelGen, DefaultTypes, ModelGenParams, ModelGenRunner}

class RefinedSupportSpec extends AnyWordSpec with Diagrams {
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
        .map(_.source.source.toString())

      val theCompanion = generated.files
        .find(_.source.name == "DataType")
        .flatMap(_.source.companion)
        .map(_.toString())

      assert(theSource.exists(_.contains("DataType.IdType")))
      assert(theCompanion.exists(_.contains("import io.circe.refined")))
      assert(theCompanion.exists(_.contains("type IdType = Refined")))
    }
  }
}
