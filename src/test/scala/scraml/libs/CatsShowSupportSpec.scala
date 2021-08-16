package scraml.libs

import java.io.File

import cats.effect.unsafe.implicits.global
import org.scalatest.diagrams.Diagrams
import org.scalatest.wordspec.AnyWordSpec
import scraml.{DefaultModelGen, ModelGenParams, ModelGenRunner}

final class CatsShowSupportSpec extends AnyWordSpec with Diagrams {
  "CatsShowSupport" must {
    "generate an implicit in the companion object when enabled" in {
      val params = ModelGenParams(
        new File("src/sbt-test/sbt-scraml/simple/api/simple.raml"),
        new File("target/scraml-cats-show-test"),
        "scraml",
        jsonSupport = None,
        librarySupport = Set(CatsShowSupport),
        formatConfig = None,
        generateDateCreated = false
      )

      val generated = ModelGenRunner.run(DefaultModelGen)(params).unsafeRunSync()

      assert(generated.files.nonEmpty)

      val theCompanion = generated.files
          .find(_.source.name == "DataType")
          .flatMap(_.source.companion)
          .map(_.toString())
          /// Scalameta puts a trailing space after "instance =>"
          .map(_.replaceAll(" \n", "\n"))

      assert(
        theCompanion === Some(
          """object DataType {
            |  import cats.Show
            |  implicit val DataTypeShow: Show[DataType] = Show.show { instance =>
            |    val buffer = new StringBuilder("DataType")
            |    buffer.append(':')
            |    buffer.append('\n')
            |    buffer.append('\t')
            |    buffer.append("id")
            |    buffer.append(": ")
            |    buffer.append(instance.id)
            |    buffer.append('\n')
            |    buffer.append('\t')
            |    buffer.append("foo")
            |    buffer.append(": ")
            |    buffer.append(instance.foo)
            |    buffer.append('\n')
            |    buffer.append('\t')
            |    buffer.append("customTypeProp")
            |    buffer.append(": ")
            |    buffer.append(instance.customTypeProp)
            |    buffer.append('\n')
            |    buffer.append('\t')
            |    buffer.append("customArrayTypeProp")
            |    buffer.append(": ")
            |    buffer.append(instance.customArrayTypeProp)
            |    buffer.append('\n')
            |    buffer.toString()
            |  }
            |}""".stripMargin
        )
      )
    }

    "not produce an instance for an object" in {
      val params = ModelGenParams(
        new File("src/sbt-test/sbt-scraml/simple/api/simple.raml"),
        new File("target/scraml-cats-show-test"),
        "scraml",
        jsonSupport = None,
        librarySupport = Set(CatsShowSupport),
        formatConfig = None
      )

      val generated = ModelGenRunner.run(DefaultModelGen)(params).unsafeRunSync()

      assert(generated.files.nonEmpty)

      val theCompanion = generated.files
          .find(_.source.name == "NoProps")
          .flatMap(_.source.companion)

      assert(theCompanion.isEmpty)
    }
  }
}
