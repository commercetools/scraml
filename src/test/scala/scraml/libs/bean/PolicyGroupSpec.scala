package scraml.libs.bean

import scala.meta._

import org.scalatest.diagrams.Diagrams
import org.scalatest.wordspec.AnyWordSpec
import scraml.{BeanProperties, DefaultTypes}
import scraml.libs.SourceCodeFormatting

class PolicyGroupSpec extends AnyWordSpec with Diagrams with SourceCodeFormatting {
  import BeanProperties._
  import cats.syntax.semigroup._

  implicit val defaultTypes = DefaultTypes()
  private val analyzer      = new Analyzer(companion = None)

  "The PolicyGroup type" when {
    "configured with default settings" must {
      implicit val group = PolicyGroup(BeanProperties())

      "use the Default policy for AnyRef scalars" in {
        val code     = defineMethodFor("prop: String")
        val expected = "def getProp: String = prop"

        assert(code === expected)
      }

      "use the Default policy for AnyVal scalars" in {
        val code     = defineMethodFor("prop: Long")
        val expected = "def getProp: Long = prop"

        assert(code === expected)
      }

      "use Scala.Option for optional properties" in {
        val code     = defineMethodFor("prop: Option[Int]")
        val expected = "def getProp: Option[Int] = prop"

        assert(code === expected)
      }

      "use the configured 'array' type for array properties" in {
        val code     = defineMethodFor(s"prop: ${defaultTypes.array}[Int]")
        val expected = s"def getProp: ${defaultTypes.array}[Int] = prop"

        assert(code === expected)
      }

      "remove refined from scalar types" in {
        val code     = defineMethodFor("prop: Refined[Int, GreaterEqual[Witness.`0`.T]]")
        val expected = "def getProp: Int = prop.value"

        assert(code === expected)
      }

      "remove refined from array types" in {
        val code = defineMethodFor(
          s"prop: Refined[${defaultTypes.array}[Int], MaxSize[Witness.`10`.T]]"
        )

        val expected = s"def getProp: ${defaultTypes.array}[Int] = prop.value"

        assert(code === expected)
      }

      "support optional array types" in {
        val code = defineMethodFor(s"prop: Option[${defaultTypes.array}[Int]]")

        val expected =
          s"""def getProp
             |: Option[${defaultTypes.array}[Int]]
             | = prop""".stripMargin.stripAllNewlines

        assert(code === expected)
      }

      "support optional refined array types" in {
        val code = defineMethodFor(
          s"prop: Option[Refined[${defaultTypes.array}[Int], MaxSize[Witness.`10`.T]]]"
        )

        val expected =
          s"""def getProp
             |: Option[${defaultTypes.array}[Int]]
             | = prop.map(_.value)""".stripMargin.stripAllNewlines

        assert(code === expected)
      }
    }

    "configured with anyVal 'use java lang' settings" must {
      implicit val group = PolicyGroup(BeanProperties(anyVal = UseJavaLangTypes))

      "use the Default policy for AnyRef scalars" in {
        val code     = defineMethodFor("prop: String")
        val expected = "def getProp: String = prop"

        assert(code === expected)
      }

      "use the Default policy for AnyVal scalars" in {
        val code     = defineMethodFor("prop: Long")
        val expected = "def getProp: java.lang.Long = prop.asInstanceOf[AnyRef]"

        assert(code === expected)
      }

      "use Scala.Option for optional properties" in {
        val code = defineMethodFor("prop: Option[Int]")
        val expected =
          """def getProp:
            | Option[java.lang.Integer]
            | = prop.map(_.asInstanceOf[AnyRef])""".stripMargin.stripAllNewlines

        assert(code === expected)
      }

      "use the configured 'array' type for array properties" in {
        val code = defineMethodFor(s"prop: ${defaultTypes.array}[Int]")
        val expected =
          s"""def getProp:
             | ${defaultTypes.array}[java.lang.Integer]
             | = prop.map(_.asInstanceOf[AnyRef])""".stripMargin.stripAllNewlines

        assert(code === expected)
      }

      "support refined scalar types" in {
        val code     = defineMethodFor("prop: Refined[Int, GreaterEqual[Witness.`0`.T]]")
        val expected = "def getProp: java.lang.Integer = prop.value.asInstanceOf[AnyRef]"

        assert(code === expected)
      }

      "support refined array types" in {
        val code = defineMethodFor(
          s"prop: Refined[${defaultTypes.array}[Int], MaxSize[Witness.`10`.T]]"
        )

        val expected =
          s"""def getProp:
             | ${defaultTypes.array}[java.lang.Integer]
             | = prop.value.map(_.asInstanceOf[AnyRef])""".stripMargin.stripAllNewlines

        assert(code === expected)
      }

      "support optional array types" in {
        val code = defineMethodFor(s"prop: Option[${defaultTypes.array}[Int]]")

        val expected =
          s"""def getProp
             |: Option[${defaultTypes.array}[java.lang.Integer]]
             | = prop.map(_.map(_.asInstanceOf[AnyRef]))""".stripMargin.stripAllNewlines

        assert(code === expected)
      }

      "support optional refined array types" in {
        val code = defineMethodFor(
          s"prop: Option[Refined[${defaultTypes.array}[Int], MaxSize[Witness.`10`.T]]]"
        )

        val expected =
          s"""def getProp
             |: Option[${defaultTypes.array}[java.lang.Integer]]
             | = prop.map(_.value.map(_.asInstanceOf[AnyRef]))""".stripMargin.stripAllNewlines

        assert(code === expected)
      }
    }

    "configured with array, anyVal and optional 'use java' settings" must {
      implicit val group = PolicyGroup(
        BeanProperties(
          anyVal = UseJavaLangTypes,
          array = UseJavaCollectionTypes,
          optional = UseJavaOptionalType
        )
      )

      "use the Default policy for AnyRef scalars" in {
        val code     = defineMethodFor("prop: String")
        val expected = "def getProp: String = prop"

        assert(code === expected)
      }

      "use the Default policy for AnyVal scalars" in {
        val code     = defineMethodFor("prop: Long")
        val expected = "def getProp: java.lang.Long = prop.asInstanceOf[AnyRef]"

        assert(code === expected)
      }

      "use Scala.Option for optional properties" in {
        val code = defineMethodFor("prop: Option[Int]")
        val expected =
          """def getProp:
            | java.util.Optional[java.lang.Integer]
            | = prop.map(_.asInstanceOf[AnyRef])
            |.toJava""".stripMargin.stripAllNewlines

        assert(code === expected)
      }

      "use the java util 'array' type for array properties" in {
        val code = defineMethodFor(s"prop: ${defaultTypes.array}[Int]")
        val expected =
          s"""def getProp:
             | java.util.List[java.lang.Integer]
             | = prop.map(_.asInstanceOf[AnyRef])
             |.asJava""".stripMargin.stripAllNewlines

        assert(code === expected)
      }

      "support refined scalar types" in {
        val code     = defineMethodFor("prop: Refined[Int, GreaterEqual[Witness.`0`.T]]")
        val expected = "def getProp: java.lang.Integer = prop.value.asInstanceOf[AnyRef]"

        assert(code === expected)
      }

      "support refined array types" in {
        val code = defineMethodFor(
          s"prop: Refined[${defaultTypes.array}[Int], MaxSize[Witness.`10`.T]]"
        )

        val expected =
          s"""def getProp:
             | java.util.List[java.lang.Integer]
             | = prop.value.map(_.asInstanceOf[AnyRef])
             |.asJava""".stripMargin.stripAllNewlines

        assert(code === expected)
      }

      "support optional array types" in {
        val code = defineMethodFor(s"prop: Option[${defaultTypes.array}[Int]]")

        val expected =
          s"""def getProp
             |: java.util.Optional[java.util.List[java.lang.Integer]]
             | = prop.map(_.map(_.asInstanceOf[AnyRef]).asJava)
             |.toJava""".stripMargin.stripAllNewlines

        assert(code === expected)
      }

      "support optional refined array types" in {
        val code = defineMethodFor(
          s"prop: Option[Refined[${defaultTypes.array}[Int], MaxSize[Witness.`10`.T]]]"
        )

        val expected =
          s"""def getProp
             |: java.util.Optional[java.util.List[java.lang.Integer]]
             | = prop.map(_.value.map(_.asInstanceOf[AnyRef]).asJava)
             |.toJava""".stripMargin.stripAllNewlines

        assert(code === expected)
      }
    }
  }

  private def defineMethodFor(declaration: String)(implicit
      group: PolicyGroup
  ): String = {
    val parsed = s"def $declaration".parse[Stat]

    assert(parsed.toEither.isRight)

    val deconstructed    = analyzer.deconstruct(parsed.get.asInstanceOf[Decl.Def])
    val policy           = group(deconstructed.coordinate)
    val beanPropertyName = Term.Name("get" |+| deconstructed.name.value.capitalize)
    val resultType       = policy.signature(deconstructed.underlyingType)
    val body = policy.body(
      Term.Name(deconstructed.name.value),
      deconstructed.underlyingType
    )

    q"def $beanPropertyName: $resultType = $body".syntax
  }
}
