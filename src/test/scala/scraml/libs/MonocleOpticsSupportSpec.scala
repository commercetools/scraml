package scraml.libs

import java.io.File

import cats.effect.unsafe.implicits.global
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import scraml.{DefaultModelGen, DefaultTypes, FieldMatchPolicy, ModelGenParams, ModelGenRunner}

final class MonocleOpticsSupportSpec extends AnyWordSpec with Matchers with SourceCodeFormatting {
  "MonocleOpticsSupport" must {
    "generate an 'Optics' object within the companion for a case class (exact property matching)" in {
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
        .map(_.toString().stripTrailingSpaces)

      theCompanion should be(
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

    "generate an 'Optics' object within the companion for a case class (default property matching)" in {
      val params = ModelGenParams(
        new File("src/sbt-test/sbt-scraml/simple/api/simple.raml"),
        new File("target/scraml-monocle-test"),
        "scraml",
        FieldMatchPolicy.Default(),
        DefaultTypes(),
        librarySupport = Set(MonocleOpticsSupport),
        formatConfig = None,
        generateDateCreated = true
      )

      val generated = ModelGenRunner.run(DefaultModelGen)(params).unsafeRunSync()

      generated.files.nonEmpty should be(true)

      val theSource = generated.files
        .find(_.source.name == "DataType")
        .map(_.source.source)
        .map(_.toString().stripTrailingSpaces)

      val theCompanion = generated.files
        .find(_.source.name == "DataType")
        .flatMap(_.source.companion)
        .map(_.toString().stripTrailingSpaces)

      theSource should be(
        Some(
          """final case class DataType(id: String, foo: Option[String] = None, customTypeProp: scala.math.BigDecimal, customArrayTypeProp: Vector[scala.math.BigDecimal] = Vector.empty)(val additionalProperties: Option[DataType.AdditionalProperties] = None) extends BaseType"""
        )
      )

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
            |  trait Optics {
            |    import monocle.Lens
            |    val id: Lens[DataType, String] = Lens[DataType, String](_.id) {
            |      a => s => s.copy(id = a)(s.additionalProperties)
            |    }
            |    val foo: Lens[DataType, Option[String]] = Lens[DataType, Option[String]](_.foo) {
            |      a => s => s.copy(foo = a)(s.additionalProperties)
            |    }
            |    val customTypeProp: Lens[DataType, scala.math.BigDecimal] = Lens[DataType, scala.math.BigDecimal](_.customTypeProp) {
            |      a => s => s.copy(customTypeProp = a)(s.additionalProperties)
            |    }
            |    val customArrayTypeProp: Lens[DataType, Vector[scala.math.BigDecimal]] = Lens[DataType, Vector[scala.math.BigDecimal]](_.customArrayTypeProp) {
            |      a => s => s.copy(customArrayTypeProp = a)(s.additionalProperties)
            |    }
            |    val additionalProperties: Lens[DataType, Option[DataType.AdditionalProperties]] = Lens[DataType, Option[DataType.AdditionalProperties]](_.additionalProperties) {
            |      a => s => s.copy()(additionalProperties = a)
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
        .map(_.toString().stripTrailingSpaces)

      theCompanion should be(
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

    "properly name 'Optics' for overridden properties" in {
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
        .find(_.source.name == "DerivedWithRequired")
        .flatMap(_.source.companion)
        .map(_.toString().stripTrailingSpaces)

      theCompanion should be(
        Some(
          """object DerivedWithRequired {
            |  trait Optics {
            |    import monocle.Lens
            |    val id: Lens[DerivedWithRequired, String] = Lens[DerivedWithRequired, String](_.id$scraml) {
            |      a => s => s.copy(id$scraml = a)
            |    }
            |  }
            |  object Optics extends Optics
            |}""".stripMargin.stripTrailingSpaces
        )
      )
    }
  }
}
