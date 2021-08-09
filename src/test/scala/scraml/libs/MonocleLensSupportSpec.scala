package scraml.libs

import java.io.File

import cats.effect.unsafe.implicits.global
import org.scalatest.diagrams.Diagrams
import org.scalatest.wordspec.AnyWordSpec
import scraml.{DefaultModelGen, ModelGenParams, ModelGenRunner}

final class MonocleLensSupportSpec extends AnyWordSpec with Diagrams {
  "MonocleLensSupport" must {
    "generate a 'Lenses' object within the companion for a case class" in {
      val params = ModelGenParams(
        new File("src/sbt-test/sbt-scraml/simple/api/simple.raml"),
        new File("target/scraml-monocle-test"),
        "scraml",
        jsonSupport = None,
        librarySupport = Set(MonocleLensSupport),
        formatConfig = None,
        generateDateCreated = true
      )

      val generated = ModelGenRunner.run(DefaultModelGen)(params).unsafeRunSync()

      assert(generated.files.nonEmpty)

      val theCompanion = generated.files
        .find(_.source.name == "DataType")
        .flatMap(_.source.companion)
        .map(_.toString())

      assert(
        theCompanion.contains(
          """object DataType {
            |  object Lenses {
            |    import monocle.Lens
            |    val id: Lens[DataType, String] = Lens[DataType, String](_.id) {
            |      a => s => s.copy(id = a)
            |    }
            |    val foo: Lens[DataType, Option[String]] = Lens[DataType, Option[String]](_.foo) {
            |      a => s => s.copy(foo = a)
            |    }
            |    val customTypeProp: Lens[DataType, scala.math.BigDecimal] = Lens[DataType, scala.math.BigDecimal](_.customTypeProp) {
            |      a => s => s.copy(customTypeProp = a)
            |    }
            |    val customArrayTypeProp: Lens[DataType, Vector[scala.math.BigDecimal]] = Lens[DataType, Vector[scala.math.BigDecimal]](_.customArrayTypeProp) {
            |      a => s => s.copy(customArrayTypeProp = a)
            |    }
            |  }
            |}""".stripMargin
        )
      )
    }
  }
}
