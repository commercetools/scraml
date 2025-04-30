package scraml.libs

import java.io.File
import cats.effect.unsafe.implicits.global
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import scraml._

class BeanPropertiesSupportSpec extends AnyWordSpec with Matchers with SourceCodeFormatting {
  "BeanPropertiesSupport" must {
    "add JavaBean compatible methods when enabled" in {
      val params = ModelGenParams(
        new File("src/sbt-test/sbt-scraml/simple/api/simple.raml"),
        new File("target/scraml-bean-properties-test"),
        "scraml",
        FieldMatchPolicy.Exact(),
        DefaultTypes(),
        librarySupport = Set(BeanPropertiesSupport),
        formatConfig = None,
        generateDateCreated = true
      )

      val generated = ModelGenRunner.run(DefaultModelGen)(params).unsafeRunSync()

      generated.files.nonEmpty should be(true)

      val theSource = generated.files
        .find(_.source.name == "DataType")
        .map(_.source.source.toString().stripTrailingSpaces)

      theSource should be(
        Some(
          """final case class DataType(id: String, foo: Option[String] = None, customTypeProp: scala.math.BigDecimal, customArrayTypeProp: Vector[scala.math.BigDecimal] = Vector.empty) extends BaseType {
            |  import scala.jdk.CollectionConverters._
            |  import scala.jdk.OptionConverters._
            |  lazy val getId: String = id
            |  lazy val getFoo: Option[String] = foo
            |  lazy val getCustomTypeProp: scala.math.BigDecimal = customTypeProp
            |  lazy val getCustomArrayTypeProp: Vector[scala.math.BigDecimal] = customArrayTypeProp
            |}""".stripMargin
        )
      )
    }

    "add JavaBean compatible methods when enabled and refined" in {
      val params = ModelGenParams(
        new File("src/sbt-test/sbt-scraml/refined/api/refined.raml"),
        new File("target/scraml-bean-properties-test"),
        "scraml",
        FieldMatchPolicy.Exact(),
        DefaultTypes(),
        librarySupport = Set(BeanPropertiesSupport, RefinedSupport),
        formatConfig = None,
        generateDateCreated = true
      )

      val generated = ModelGenRunner.run(DefaultModelGen)(params).unsafeRunSync()

      generated.files.nonEmpty should be(true)

      val theSource = generated.files
        .find(_.source.name == "DataType")
        .map(_.source.source.toString().stripTrailingSpaces)

      theSource should be(
        Some(
          """final case class DataType(id: DataType.IdType, optionalCustomArrayTypeProp: DataType.OptionalCustomArrayTypePropType = None, foo: Option[String] = None, bar: DataType.BarType = None, numberProp: DataType.NumberPropType, customNumberProp: DataType.CustomNumberPropType, customArrayTypeProp: DataType.CustomArrayTypePropType, optionalStringArray: DataType.OptionalStringArrayType = None) extends BaseType {
            |  import scala.jdk.CollectionConverters._
            |  import scala.jdk.OptionConverters._
            |  lazy val getId: String = id.value
            |  lazy val getOptionalCustomArrayTypeProp: Option[Set[scala.math.BigDecimal]] = optionalCustomArrayTypeProp.map(_.value)
            |  lazy val getFoo: Option[String] = foo
            |  lazy val getBar: Option[String] = bar.map(_.value)
            |  lazy val getNumberProp: Float = numberProp.value
            |  lazy val getCustomNumberProp: scala.math.BigDecimal = customNumberProp.value
            |  lazy val getCustomArrayTypeProp: Vector[scala.math.BigDecimal] = customArrayTypeProp.value
            |  lazy val getOptionalStringArray: Option[scala.collection.immutable.List[String]] = optionalStringArray.map(_.value)
            |}""".stripMargin
        )
      )
    }

    "add JavaBean compatible methods with conversions" in {
      import BeanProperties._

      val params = ModelGenParams(
        new File("src/sbt-test/sbt-scraml/simple/api/simple.raml"),
        new File("target/scraml-bean-properties-test"),
        "scraml",
        FieldMatchPolicy.Exact(),
        DefaultTypes(),
        librarySupport = Set(BeanPropertiesSupport),
        formatConfig = None,
        generateDateCreated = true,
        beanProperties = BeanProperties(
          array = UseJavaCollectionTypes,
          optional = UseJavaOptionalType,
          scalaNumber = UseJavaLangTypes
        )
      )

      val generated = ModelGenRunner.run(DefaultModelGen)(params).unsafeRunSync()

      generated.files.nonEmpty should be(true)

      val theSource = generated.files
        .find(_.source.name == "DataType")
        .map(_.source.source.toString().stripTrailingSpaces)

      theSource should be(
        Some(
          """final case class DataType(id: String, foo: Option[String] = None, customTypeProp: scala.math.BigDecimal, customArrayTypeProp: Vector[scala.math.BigDecimal] = Vector.empty) extends BaseType {
            |  import scala.jdk.CollectionConverters._
            |  import scala.jdk.OptionConverters._
            |  lazy val getId: String = id
            |  lazy val getFoo: java.util.Optional[String] = foo.toJava
            |  lazy val getCustomTypeProp: java.math.BigDecimal = customTypeProp.bigDecimal
            |  lazy val getCustomArrayTypeProp: Vector[scala.math.BigDecimal] = customArrayTypeProp
            |}""".stripMargin
        )
      )
    }

    "add JavaBean compatible methods with conversions and refined" in {
      import BeanProperties._

      val params = ModelGenParams(
        new File("src/sbt-test/sbt-scraml/refined/api/refined.raml"),
        new File("target/scraml-bean-properties-test"),
        "scraml",
        FieldMatchPolicy.Exact(),
        DefaultTypes(),
        librarySupport = Set(BeanPropertiesSupport, RefinedSupport),
        formatConfig = None,
        generateDateCreated = true,
        beanProperties = BeanProperties(
          anyVal = UseJavaLangTypes,
          array = UseJavaCollectionTypes,
          optional = UseJavaOptionalType,
          scalaNumber = UseJavaLangTypes
        )
      )

      val generated = ModelGenRunner.run(DefaultModelGen)(params).unsafeRunSync()

      generated.files.nonEmpty should be(true)

      val theSource = generated.files
        .find(_.source.name == "DataType")
        .map(_.source.source.toString().stripTrailingSpaces)

      theSource should be(
        Some(
          """final case class DataType(id: DataType.IdType, optionalCustomArrayTypeProp: DataType.OptionalCustomArrayTypePropType = None, foo: Option[String] = None, bar: DataType.BarType = None, numberProp: DataType.NumberPropType, customNumberProp: DataType.CustomNumberPropType, customArrayTypeProp: DataType.CustomArrayTypePropType, optionalStringArray: DataType.OptionalStringArrayType = None) extends BaseType {
            |  import scala.jdk.CollectionConverters._
            |  import scala.jdk.OptionConverters._
            |  lazy val getId: String = id.value
            |  lazy val getOptionalCustomArrayTypeProp: java.util.Optional[Set[scala.math.BigDecimal]] = optionalCustomArrayTypeProp.map(_.value).toJava
            |  lazy val getFoo: java.util.Optional[String] = foo.toJava
            |  lazy val getBar: java.util.Optional[String] = bar.map(_.value).toJava
            |  lazy val getNumberProp: java.lang.Float = numberProp.value.asInstanceOf[java.lang.Float]
            |  lazy val getCustomNumberProp: java.math.BigDecimal = customNumberProp.value.bigDecimal
            |  lazy val getCustomArrayTypeProp: Vector[scala.math.BigDecimal] = customArrayTypeProp.value
            |  lazy val getOptionalStringArray: java.util.Optional[java.util.List[String]] = optionalStringArray.map(_.value.asJava).toJava
            |}""".stripMargin
        )
      )
    }

    "add JavaBean compatible methods with 'nullable option'" in {
      import BeanProperties._

      val params = ModelGenParams(
        new File("src/sbt-test/sbt-scraml/refined/api/refined.raml"),
        new File("target/scraml-bean-properties-test"),
        "scraml",
        FieldMatchPolicy.Exact(),
        DefaultTypes(),
        librarySupport = Set(BeanPropertiesSupport, RefinedSupport),
        formatConfig = None,
        generateDateCreated = true,
        beanProperties = BeanProperties(
          anyVal = UseJavaLangTypes,
          array = UseJavaCollectionTypes,
          optional = UseNullableReturnType,
          scalaNumber = UseJavaLangTypes
        )
      )

      val generated = ModelGenRunner.run(DefaultModelGen)(params).unsafeRunSync()

      generated.files.nonEmpty should be(true)

      val theSource = generated.files
        .find(_.source.name == "DataType")
        .map(_.source.source.toString().stripTrailingSpaces)

      theSource should be(
        Some(
          """final case class DataType(id: DataType.IdType, optionalCustomArrayTypeProp: DataType.OptionalCustomArrayTypePropType = None, foo: Option[String] = None, bar: DataType.BarType = None, numberProp: DataType.NumberPropType, customNumberProp: DataType.CustomNumberPropType, customArrayTypeProp: DataType.CustomArrayTypePropType, optionalStringArray: DataType.OptionalStringArrayType = None) extends BaseType {
            |  import scala.jdk.CollectionConverters._
            |  import scala.jdk.OptionConverters._
            |  lazy val getId: String = id.value
            |  lazy val getOptionalCustomArrayTypeProp: Set[scala.math.BigDecimal] = optionalCustomArrayTypeProp.map(_.value).orNull
            |  lazy val getFoo: String = foo.orNull
            |  lazy val getBar: String = bar.map(_.value).orNull
            |  lazy val getNumberProp: java.lang.Float = numberProp.value.asInstanceOf[java.lang.Float]
            |  lazy val getCustomNumberProp: java.math.BigDecimal = customNumberProp.value.bigDecimal
            |  lazy val getCustomArrayTypeProp: Vector[scala.math.BigDecimal] = customArrayTypeProp.value
            |  lazy val getOptionalStringArray: java.util.List[String] = optionalStringArray.map(_.value.asJava).orNull
            |}""".stripMargin
        )
      )
    }
  }
}
