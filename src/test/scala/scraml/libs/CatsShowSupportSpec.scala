package scraml.libs

import java.io.File

import cats.effect.unsafe.implicits.global
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import scraml.{DefaultModelGen, DefaultTypes, FieldMatchPolicy, ModelGenParams, ModelGenRunner}

final class CatsShowSupportSpec extends AnyWordSpec with Matchers with SourceCodeFormatting {
  "CatsShowSupport" must {
    "generate an implicit in the companion object when enabled (exact property matching)" in {
      val params = ModelGenParams(
        new File("src/sbt-test/sbt-scraml/cats/api/simple.raml"),
        new File("target/scraml-cats-show-test"),
        "scraml",
        FieldMatchPolicy.Exact(),
        DefaultTypes(),
        librarySupport = Set(CatsShowSupport),
        formatConfig = None,
        generateDateCreated = false
      )

      val generated = ModelGenRunner.run(DefaultModelGen)(params).unsafeRunSync()

      generated.files.nonEmpty should equal(true)

      val theCompanion = generated.files
        .find(_.source.name == "DataType")
        .flatMap(_.source.companion)
        .map(_.toString().stripTrailingSpaces)

      theCompanion should be(
        Some(
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

    "generate an implicit in the companion object when enabled (default property matching)" in {
      val params = ModelGenParams(
        new File("src/sbt-test/sbt-scraml/cats/api/simple.raml"),
        new File("target/scraml-cats-show-test"),
        "scraml",
        FieldMatchPolicy.Default(),
        DefaultTypes(),
        librarySupport = Set(CatsShowSupport),
        formatConfig = None,
        generateDateCreated = false
      )

      val generated = ModelGenRunner.run(DefaultModelGen)(params).unsafeRunSync()

      generated.files.nonEmpty should equal(true)

      val theCompanion = generated.files
        .find(_.source.name == "DataType")
        .flatMap(_.source.companion)
        .map(_.toString().stripTrailingSpaces)

      theCompanion should be(
        Some(
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
            |    buffer.append('\t')
            |    buffer.append("additionalProperties")
            |    buffer.append(": ")
            |    buffer.append(instance.additionalProperties)
            |    buffer.append('\n')
            |    buffer.toString()
            |  }
            |}""".stripMargin
        )
      )
    }

    "not produce an instance for an object" in {
      val params = ModelGenParams(
        new File("src/sbt-test/sbt-scraml/cats/api/simple.raml"),
        new File("target/scraml-cats-show-test"),
        "scraml",
        FieldMatchPolicy.Exact(),
        DefaultTypes(),
        librarySupport = Set(CatsShowSupport),
        formatConfig = None
      )

      val generated = ModelGenRunner.run(DefaultModelGen)(params).unsafeRunSync()

      generated.files.nonEmpty should equal(true)

      val theCompanion = generated.files
        .find(_.source.name == "NoProps")
        .flatMap(_.source.companion)

      theCompanion.isEmpty should equal(true)
    }

    "show the original property name if overridden" in {
      val params = ModelGenParams(
        new File("src/sbt-test/sbt-scraml/cats/api/simple.raml"),
        new File("target/scraml-cats-eq-test"),
        "scraml",
        FieldMatchPolicy.KeepExtra(),
        DefaultTypes(),
        librarySupport = Set(CatsShowSupport),
        formatConfig = None,
        generateDateCreated = true
      )

      val generated = ModelGenRunner.run(DefaultModelGen)(params).unsafeRunSync()

      assert(generated.files.nonEmpty)

      val theCompanion = generated.files
        .find(_.source.name == "DerivedWithRequired")
        .flatMap(_.source.companion)
        .map(_.toString().stripTrailingSpaces)

      assert(
        theCompanion === Some(
          """object DerivedWithRequired {
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
            |    val propertyNames: Seq[String] = Seq("id", "version")
            |    val allowedNames: Seq[Regex] = Seq()
            |  }
            |  import cats.Show
            |  implicit val DerivedWithRequiredShow: Show[DerivedWithRequired] = Show.show { instance =>
            |    val buffer = new StringBuilder("DerivedWithRequired")
            |    buffer.append(':')
            |    buffer.append('\n')
            |    buffer.append('\t')
            |    buffer.append("id")
            |    buffer.append(": ")
            |    buffer.append(instance.id$scraml)
            |    buffer.append('\n')
            |    buffer.append('\t')
            |    buffer.append("version")
            |    buffer.append(": ")
            |    buffer.append(instance.version)
            |    buffer.append('\n')
            |    buffer.append('\t')
            |    buffer.append("additionalProperties")
            |    buffer.append(": ")
            |    buffer.append(instance.additionalProperties)
            |    buffer.append('\n')
            |    buffer.toString()
            |  }
            |}""".stripMargin.stripTrailingSpaces
        )
      )
    }
  }
}
