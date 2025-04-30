package scraml.libs.bean

import scala.meta._

import cats.data.Ior
import org.scalatest.diagrams.Diagrams
import org.scalatest.wordspec.AnyWordSpec
import scraml.DefaultTypes
import scraml.libs.SourceCodeFormatting

class GenPolicySpec extends AnyWordSpec with Diagrams with SourceCodeFormatting {
  import cats.syntax.option._
  import cats.syntax.semigroup._

  implicit val defaultTypes: DefaultTypes = DefaultTypes()
  private val analyzer                    = new Analyzer(companion = None)

  "The GenPolicy types" when {
    "given scalar property definitions" must {
      "generate methods with no transformations" in {
        val code     = defineMethodFor("anInt: Int")(GenPolicy.Default)
        val expected = "def getAnInt: Int = anInt"

        assert(code === expected)
      }

      "generate methods with anyVal transformations" in {
        val code = defineMethodFor("javaChar: Char")(
          AnyValUseJavaLangPolicy
        )

        val expected =
          """def getJavaChar
            |: java.lang.Character =
            | javaChar.asInstanceOf[java.lang.Character]""".stripMargin.stripAllNewlines

        assert(code === expected)
      }

      "generate methods with scalaNumber transformations" in {
        val code = defineMethodFor("big: scala.math.BigInt")(
          ScalaNumberUseJavaLangPolicy
        )

        val expected = "def getBig: java.math.BigInteger = big.bigInteger"

        assert(code === expected)
      }

      "generate refined methods" in {
        val code = defineMethodFor("anInt: Refined[Int, GreaterEqual[Witness.`1`.T]]")(
          RefinedPolicy(next = GenPolicy.Default)
        )

        val expected = "def getAnInt: Int = anInt.value"

        assert(code === expected)
      }
    }

    "given array property definitions" must {
      "generate methods with no transformations" in {
        val code = defineMethodFor(s"ints: ${defaultTypes.array}[Int]")(
          ContainerPolicy(
            containerTransformer = Ior.Left(GenSignature.scalaCollection)
          )
        )

        val expected = s"def getInts: ${defaultTypes.array}[Int] = ints"

        assert(code === expected)
      }

      "generate methods with scalaNumber transformations" in {
        val code = defineMethodFor(
          s"bigs: ${defaultTypes.array}[scala.math.BigInt]"
        )(
          ContainerPolicy(
            containerTransformer = Ior.Left(GenSignature.scalaCollection),
            mapper = ScalaNumberUseJavaLangPolicy.some
          )
        )

        val expected =
          s"""def getBigs
             |: ${defaultTypes.array}[java.math.BigInteger] =
             | bigs.map(_.bigInteger)""".stripMargin.stripAllNewlines

        assert(code === expected)
      }

      "generate refined methods" in {
        val code = defineMethodFor(
          s"ints: Refined[${defaultTypes.array}[Int], MaxSize[Witness.`10`.T]]"
        )(
          RefinedPolicy(
            next = ContainerPolicy(
              containerTransformer = Ior.Left(GenSignature.scalaCollection)
            )
          )
        )

        val expected = s"def getInts: ${defaultTypes.array}[Int] = ints.value"

        assert(code === expected)
      }

      "generate methods with Java and anyVal transformation" in {
        val code = defineMethodFor(
          s"javaFloats: ${defaultTypes.array}[Float]"
        )(
          ContainerPolicy(
            containerTransformer = Ior.Right(JavaCollectionPolicy),
            mapper = AnyValUseJavaLangPolicy.some
          )
        )

        val expected =
          """def getJavaFloats
            |: java.util.List[java.lang.Float] =
            | javaFloats.map(_.asInstanceOf[java.lang.Float]).asJava
            |""".stripMargin.stripAllNewlines

        assert(code === expected)
      }
    }

    "given optional property definitions" must {
      "generate methods with no transformations" in {
        val code = defineMethodFor("ints: Option[Int]")(
          ContainerPolicy(
            containerTransformer = Ior.Left(GenSignature.scalaOptional)
          )
        )

        val expected = "def getInts: Option[Int] = ints"

        assert(code === expected)
      }

      "generate refined methods" in {
        val code = defineMethodFor(
          "ints: Option[Refined[Int, Less[Witness.`10`.T]]]"
        )(
          ContainerPolicy(
            containerTransformer = Ior.Left(GenSignature.scalaOptional),
            mapper = RefinedPolicy(next = GenPolicy.Default).some
          )
        )

        val expected = "def getInts: Option[Int] = ints.map(_.value)"

        assert(code === expected)
      }

      "generate methods with anyVal transformations" in {
        val code = defineMethodFor("javaFloat: Option[Float]")(
          ContainerPolicy(
            containerTransformer = Ior.Left(GenSignature.scalaOptional),
            mapper = AnyValUseJavaLangPolicy.some
          )
        )

        val expected =
          """def getJavaFloat
            |: Option[java.lang.Float] =
            | javaFloat.map(_.asInstanceOf[java.lang.Float])""".stripMargin.stripAllNewlines

        assert(code === expected)
      }

      "generate methods with Java Optional transformation" in {
        val code = defineMethodFor("javaFloat: Option[Float]")(
          ContainerPolicy(
            containerTransformer = Ior.Right(JavaOptionalPolicy),
            mapper = AnyValUseJavaLangPolicy.some
          )
        )

        val expected =
          """def getJavaFloat
            |: java.util.Optional[java.lang.Float] =
            | javaFloat.map(_.asInstanceOf[java.lang.Float]).toJava
            |""".stripMargin.stripAllNewlines

        assert(code === expected)
      }
    }

    "given optional property definitions (nullable)" must {
      "generate methods with no transformations" in {
        val code = defineMethodFor("ints: Option[Int]")(
          NullableOptionPolicy(
            mapper = AnyValUseJavaLangPolicy.some
          )
        )

        val expected =
          """def getInts
            |: java.lang.Integer =
            | ints.map(_.asInstanceOf[java.lang.Integer])
            |.orNull""".stripMargin.stripAllNewlines

        assert(code === expected)
      }

      "generate refined methods" in {
        val code = defineMethodFor(
          "anInt: Option[Refined[Int, Less[Witness.`10`.T]]]"
        )(
          NullableOptionPolicy(
            mapper = RefinedPolicy(
              next = AnyValUseJavaLangPolicy
            ).some
          )
        )

        val expected =
          """def getAnInt
            |: java.lang.Integer =
            | anInt.map(_.value.asInstanceOf[java.lang.Integer])
            |.orNull""".stripMargin.stripAllNewlines

        assert(code === expected)
      }

      "generate methods with anyVal transformations" in {
        val code = defineMethodFor("javaFloat: Option[Float]")(
          NullableOptionPolicy(
            mapper = AnyValUseJavaLangPolicy.some
          )
        )

        val expected =
          """def getJavaFloat
            |: java.lang.Float =
            | javaFloat.map(_.asInstanceOf[java.lang.Float])
            |.orNull""".stripMargin.stripAllNewlines

        assert(code === expected)
      }

      "generate methods with scalaNumber transformations" in {
        val code = defineMethodFor("javaFloat: Option[BigDecimal]")(
          NullableOptionPolicy(
            mapper = ScalaNumberUseJavaLangPolicy.some
          )
        )

        val expected =
          """def getJavaFloat
            |: java.math.BigDecimal =
            | javaFloat.map(_.bigDecimal)
            |.orNull""".stripMargin.stripAllNewlines

        assert(code === expected)
      }

      "generate methods with Java Collection transformation" in {
        val code = defineMethodFor(
          s"javaFloats: ${defaultTypes.array}[Float]"
        )(
          ContainerPolicy(
            containerTransformer = Ior.Right(JavaCollectionPolicy),
            mapper = AnyValUseJavaLangPolicy.some
          )
        )

        val expected =
          """def getJavaFloats
            |: java.util.List[java.lang.Float] =
            | javaFloats.map(_.asInstanceOf[java.lang.Float]).asJava
            |""".stripMargin.stripAllNewlines

        assert(code === expected)
      }
    }

    "given optional array property definitions" must {
      "generate methods with no transformations" in {
        val code = defineMethodFor(
          s"ints: Option[${defaultTypes.array}[Int]]"
        )(
          ContainerPolicy(
            containerTransformer = Ior.Both(
              GenSignature.scalaOptional,
              ContainerPolicy(
                containerTransformer = Ior.Left(GenSignature.scalaCollection)
              )
            )
          )
        )

        val expected = s"def getInts: Option[${defaultTypes.array}[Int]] = ints"

        assert(code === expected)
      }

      "generate refined methods" in {
        val code = defineMethodFor(
          s"ints: Option[Refined[${defaultTypes.array}[Int], MaxSize[Witness.`10`.T]]]"
        )(
          ContainerPolicy(
            containerTransformer = Ior.Left(GenSignature.scalaOptional),
            mapper = RefinedPolicy(
              next = ContainerPolicy(
                containerTransformer = Ior.Left(GenSignature.scalaCollection)
              )
            ).some
          )
        )

        val expected = s"def getInts: Option[${defaultTypes.array}[Int]] = ints.map(_.value)"

        assert(code === expected)
      }

      "generate methods with anyVal transformations" in {
        val code = defineMethodFor(
          s"ints: Option[Refined[${defaultTypes.array}[Int], MaxSize[Witness.`10`.T]]]"
        )(
          ContainerPolicy(
            containerTransformer = Ior.Left(GenSignature.scalaOptional),
            mapper = RefinedPolicy(
              next = ContainerPolicy(
                containerTransformer = Ior.Left(GenSignature.scalaCollection),
                mapper = AnyValUseJavaLangPolicy.some
              )
            ).some
          )
        )

        val expected =
          s"""def getInts
             |: Option[${defaultTypes.array}[java.lang.Integer]] =
             | ints.map(
             |_.value.map(_.asInstanceOf[java.lang.Integer])
             |)""".stripMargin.stripAllNewlines

        assert(code === expected)
      }

      "generate methods with Java and anyVal transformation" in {
        val code = defineMethodFor(
          s"javaFloats: Option[${defaultTypes.array}[Float]]"
        )(
          ContainerPolicy(
            containerTransformer = Ior.Right(JavaOptionalPolicy),
            mapper = ContainerPolicy(
              containerTransformer = Ior.Right(JavaCollectionPolicy),
              mapper = AnyValUseJavaLangPolicy.some
            ).some
          )
        )

        val expected =
          """def getJavaFloats
            |: java.util.Optional[java.util.List[java.lang.Float]] =
            | javaFloats.map(_.map(_.asInstanceOf[java.lang.Float]).asJava)
            |.toJava
            |""".stripMargin.stripAllNewlines

        assert(code === expected)
      }
    }
  }

  private def defineMethodFor(declaration: String)(policy: GenPolicy): String = {
    val parsed = s"def $declaration".parse[Stat]

    assert(parsed.toEither.isRight)

    val deconstructed    = analyzer.deconstruct(parsed.get.asInstanceOf[Decl.Def])
    val beanPropertyName = Term.Name("get" |+| deconstructed.name.value.capitalize)
    val resultType       = policy.signature(deconstructed.underlyingType)
    val body = policy.body(
      Term.Name(deconstructed.name.value),
      deconstructed.underlyingType
    )

    q"def $beanPropertyName: $resultType = $body".syntax
  }
}
