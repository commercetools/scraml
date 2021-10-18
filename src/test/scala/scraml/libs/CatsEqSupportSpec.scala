package scraml.libs

import java.io.File

import cats.effect.unsafe.implicits.global
import org.scalatest.diagrams.Diagrams
import org.scalatest.wordspec.AnyWordSpec
import scraml.{DefaultModelGen, DefaultTypes, ModelGenParams, ModelGenRunner}

final class CatsEqSupportSpec extends AnyWordSpec with Diagrams {
  "CatsEqSupport" must {
    "generate an implicit in the companion object when enabled" in {
      val params = ModelGenParams(
        new File("src/sbt-test/sbt-scraml/simple/api/simple.raml"),
        new File("target/scraml-cats-eq-test"),
        "scraml",
        DefaultTypes(),
        librarySupport = Set(CatsEqSupport),
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
        theCompanion === Some(
          """object DataType {
            |  import cats.kernel.Eq
            |  implicit val DataTypeEq: Eq[DataType] = new Eq[DataType] {
            |    override def eqv(a: DataType, b: DataType): Boolean = {
            |      a.id == b.id && a.foo == b.foo && a.customTypeProp == b.customTypeProp && a.customArrayTypeProp == b.customArrayTypeProp
            |    }
            |  }
            |}""".stripMargin
        )
      )
    }
  }
}
