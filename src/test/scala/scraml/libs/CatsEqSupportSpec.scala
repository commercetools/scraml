package scraml.libs

import java.io.File

import cats.effect.unsafe.implicits.global
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import scraml.{DefaultModelGen, DefaultTypes, FieldMatchPolicy, ModelGenParams, ModelGenRunner}

final class CatsEqSupportSpec extends AnyWordSpec with Matchers with SourceCodeFormatting {
  "CatsEqSupport" must {
    "generate an implicit in the companion object when enabled (ignore extra property matching)" in {
      val params = ModelGenParams(
        new File("src/sbt-test/sbt-scraml/simple/api/simple.raml"),
        new File("target/scraml-cats-eq-test"),
        "scraml",
        FieldMatchPolicy.IgnoreExtra(),
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
        .map(_.toString().stripTrailingSpaces)

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

    "generate an implicit in the companion object when enabled (keep extra property matching)" in {
      val params = ModelGenParams(
        new File("src/sbt-test/sbt-scraml/simple/api/simple.raml"),
        new File("target/scraml-cats-eq-test"),
        "scraml",
        FieldMatchPolicy.KeepExtra(),
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
        .map(_.toString().stripTrailingSpaces)

      assert(
        theCompanion === Some(
          """object DataType {
            |  import scala.language.dynamics
            |  final case class AdditionalProperties(private val underlying: scala.collection.immutable.Map[String, Any]) extends scala.Dynamic {
            |    override def toString(): String = underlying.mkString(", ")
            |    def selectDynamic(field: String): Option[Any] = underlying.get(field)
            |    def getOrElse[V >: Any](key: String, default: => V): V = underlying.getOrElse(key, default)
            |    def isDefinedAt(key: String): Boolean = underlying.isDefinedAt(key)
            |    def isEmpty: Boolean = underlying.isEmpty
            |    def keySet: Set[String] = underlying.keySet
            |    def keys: Iterable[String] = underlying.keys
            |    def keysIterator: Iterator[String] = underlying.keysIterator
            |    def nonEmpty: Boolean = !underlying.isEmpty
            |    def size: Int = underlying.size
            |    def values: Iterable[Any] = underlying.values
            |    def valuesIterator: Iterator[Any] = underlying.valuesIterator
            |  }
            |  object AdditionalProperties {
            |    import scala.util.matching.Regex
            |    val propertyNames: Seq[String] = Seq("id", "foo", "customTypeProp", "customArrayTypeProp")
            |    val allowedNames: Seq[Regex] = Seq()
            |  }
            |  import cats.kernel.Eq
            |  implicit val DataTypeEq: Eq[DataType] = new Eq[DataType] {
            |    override def eqv(a: DataType, b: DataType): Boolean = {
            |      a.id == b.id && a.foo == b.foo && a.customTypeProp == b.customTypeProp && a.customArrayTypeProp == b.customArrayTypeProp && a.additionalProperties == b.additionalProperties
            |    }
            |  }
            |}""".stripMargin
        )
      )
    }
  }
}
