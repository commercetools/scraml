package scraml.libs

import java.io.File

import cats.effect.unsafe.implicits.global
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import scraml.{DefaultModelGen, DefaultTypes, FieldMatchPolicy, ModelGenParams, ModelGenRunner}

final class MonocleOpticsSupportSpec extends AnyWordSpec with Matchers with SourceCodeFormatting {
  "MonocleOpticsSupport" must {
    "generate an 'Optics' object within the companion for a case class" in {
      val params = ModelGenParams(
        new File("src/sbt-test/sbt-scraml/simple/api/simple.raml"),
        new File("target/scraml-monocle-test"),
        "scraml",
        FieldMatchPolicy.Exact(),
        DefaultTypes(),
        librarySupport = Set(MonocleOpticsSupport),
        formatConfig = None,
        generateDateCreated = true
      )

      val generated = ModelGenRunner.run(DefaultModelGen)(params).unsafeRunSync()

      generated.files.nonEmpty should be(true)

      val theCompanion = generated.files
        .find(_.source.name == "DataType")
        .flatMap(_.source.companion)
        .map(_.toString())

      theCompanion.map(_.toString().stripTrailingSpaces) should be(
        Some(
          """object DataType {
            |  trait Optics {
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
            |  object Optics extends Optics
            |}""".stripMargin
        )
      )
    }

    "generate an 'Optics' object within the companion for a trait" in {
      val params = ModelGenParams(
        new File("src/sbt-test/sbt-scraml/simple/api/simple.raml"),
        new File("target/scraml-monocle-test"),
        "scraml",
        FieldMatchPolicy.Exact(),
        DefaultTypes(),
        librarySupport = Set(MonocleOpticsSupport),
        formatConfig = None,
        generateDateCreated = true
      )

      val generated = ModelGenRunner.run(DefaultModelGen)(params).unsafeRunSync()

      generated.files.nonEmpty should be(true)

      val theCompanion = generated.files
        .find(_.source.name == "BaseType")
        .flatMap(_.source.companion)
        .map(_.toString())

      theCompanion.map(_.toString().stripTrailingSpaces) should be(
        Some(
          """object BaseType {
            |  trait Optics {
            |    import monocle.Getter
            |    val id: Getter[BaseType, String] = Getter[BaseType, String](_.id)
            |  }
            |  object Optics extends Optics
            |}""".stripMargin
        )
      )
    }
  }
}
