package scraml.libs.bean

import scala.meta._
import org.scalatest.diagrams.Diagrams
import org.scalatest.wordspec.AnyWordSpec
import scraml.DefaultTypes
import scraml.libs.SourceCodeFormatting

class GenSignatureSpec extends AnyWordSpec with Diagrams with SourceCodeFormatting {
  import cats.syntax.semigroup._

  implicit val defaultTypes = DefaultTypes()
  lazy val intType          = "Int".parse[Type].get

  "The GenSignature type" must {
    "support Java Collection" in {
      val result   = GenSignature.javaCollection.signature(intType)
      val expected = "java.util.List[Int]"

      assert(result.syntax === expected)
    }

    "support Java Optional" in {
      val result   = GenSignature.javaOptional.signature(intType)
      val expected = "java.util.Optional[Int]"

      assert(result.syntax === expected)
    }

    "support configured Scala Collection" in {
      val result   = GenSignature.scalaCollection.signature(intType)
      val expected = s"${defaultTypes.array}[Int]"

      assert(result.syntax === expected)
    }

    "support Scala Option" in {
      val result   = GenSignature.scalaOptional.signature(intType)
      val expected = "Option[Int]"

      assert(result.syntax === expected)
    }

    "The GenSignature.Container type" must {
      "support composition" in {
        val result = (
          GenSignature.scalaOptional |+| GenSignature.scalaCollection
        ).signature(intType)
        val expected = s"Option[${defaultTypes.array}[Int]]"

        assert(result.syntax === expected)
      }
    }
  }
}
