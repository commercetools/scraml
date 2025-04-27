package scraml.libs.bean

import scala.meta._

import org.scalatest.diagrams.Diagrams
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import scraml.DefaultTypes
import scraml.libs.SourceCodeFormatting

class AnalyzerSpec extends AnyWordSpec with Diagrams with Matchers with SourceCodeFormatting {
  implicit val defaultTypes: DefaultTypes = DefaultTypes()
  private val analyzer                    = new Analyzer(companion = None)

  "The property Analyzer" when {
    "inspecting trait property definitions" must {
      "support scalar AnyRef properties" in {
        val parsed = "def prop: String".parse[Stat]

        assert(parsed.toEither.isRight)
        parsed.toEither.foreach { case stat: Decl.Def =>
          val result = analyzer.deconstruct(stat)

          assert(result.coordinate.anyVal === false)
          assert(result.coordinate.array === false)
          assert(result.coordinate.optional === false)
          assert(result.coordinate.refined === false)
          assert(result.coordinate.scalaNumber === false)
          assert(result.name.value === "prop")
          assert(result.underlyingType.syntax === "String")
        }
      }

      "support scalar AnyVal properties" in {
        val parsed = "def prop: Double".parse[Stat]

        assert(parsed.toEither.isRight)
        parsed.toEither.foreach { case stat: Decl.Def =>
          val result = analyzer.deconstruct(stat)

          assert(result.coordinate.anyVal === true)
          assert(result.coordinate.array === false)
          assert(result.coordinate.optional === false)
          assert(result.coordinate.refined === false)
          assert(result.coordinate.scalaNumber === false)
          assert(result.name.value === "prop")
          assert(result.underlyingType.syntax === "Double")
        }
      }

      "support collection AnyRef properties" in {
        val parsed = s"def prop: ${defaultTypes.array}[String]".parse[Stat]

        assert(parsed.toEither.isRight)
        parsed.toEither.foreach { case stat: Decl.Def =>
          val result = analyzer.deconstruct(stat)

          assert(result.coordinate.anyVal === false)
          assert(result.coordinate.array === true)
          assert(result.coordinate.optional === false)
          assert(result.coordinate.refined === false)
          assert(result.coordinate.scalaNumber === false)
          assert(result.name.value === "prop")
          assert(result.underlyingType.syntax === "String")
        }
      }

      "support collection AnyVal properties" in {
        val parsed = s"def prop: ${defaultTypes.array}[Long]".parse[Stat]

        assert(parsed.toEither.isRight)
        parsed.toEither.foreach { case stat: Decl.Def =>
          val result = analyzer.deconstruct(stat)

          assert(result.coordinate.anyVal === true)
          assert(result.coordinate.array === true)
          assert(result.coordinate.optional === false)
          assert(result.coordinate.refined === false)
          assert(result.coordinate.scalaNumber === false)
          assert(result.name.value === "prop")
          assert(result.underlyingType.syntax === "Long")
        }
      }

      "support optional AnyRef properties" in {
        val parsed = "def prop: Option[String]".parse[Stat]

        assert(parsed.toEither.isRight)
        parsed.toEither.foreach { case stat: Decl.Def =>
          val result = analyzer.deconstruct(stat)

          assert(result.coordinate.anyVal === false)
          assert(result.coordinate.array === false)
          assert(result.coordinate.optional === true)
          assert(result.coordinate.refined === false)
          assert(result.coordinate.scalaNumber === false)
          assert(result.name.value === "prop")
          assert(result.underlyingType.syntax === "String")
        }
      }

      "support optional AnyVal properties" in {
        val parsed = "def prop: Option[Int]".parse[Stat]

        assert(parsed.toEither.isRight)
        parsed.toEither.foreach { case stat: Decl.Def =>
          val result = analyzer.deconstruct(stat)

          assert(result.coordinate.anyVal === true)
          assert(result.coordinate.array === false)
          assert(result.coordinate.optional === true)
          assert(result.coordinate.refined === false)
          assert(result.coordinate.scalaNumber === false)
          assert(result.name.value === "prop")
          assert(result.underlyingType.syntax === "Int")
        }
      }

      "support optional collection AnyRef properties" in {
        val parsed = s"def prop: Option[${defaultTypes.array}[String]]".parse[Stat]

        assert(parsed.toEither.isRight)
        parsed.toEither.foreach { case stat: Decl.Def =>
          val result = analyzer.deconstruct(stat)

          assert(result.coordinate.anyVal === false)
          assert(result.coordinate.array === true)
          assert(result.coordinate.optional === true)
          assert(result.coordinate.refined === false)
          assert(result.coordinate.scalaNumber === false)
          assert(result.name.value === "prop")
          assert(result.underlyingType.syntax === "String")
        }
      }

      "support optional collection AnyVal properties" in {
        val parsed = s"def prop: Option[${defaultTypes.array}[Byte]]".parse[Stat]

        assert(parsed.toEither.isRight)
        parsed.toEither.foreach { case stat: Decl.Def =>
          val result = analyzer.deconstruct(stat)

          assert(result.coordinate.anyVal === true)
          assert(result.coordinate.array === true)
          assert(result.coordinate.optional === true)
          assert(result.coordinate.refined === false)
          assert(result.coordinate.scalaNumber === false)
          assert(result.name.value === "prop")
          assert(result.underlyingType.syntax === "Byte")
        }
      }

      "support optional collection ScalaNumber properties" in {
        val parsed = s"def prop: Option[${defaultTypes.array}[scala.math.BigDecimal]]".parse[Stat]

        assert(parsed.toEither.isRight)
        parsed.toEither.foreach { case stat: Decl.Def =>
          val result = analyzer.deconstruct(stat)

          assert(result.coordinate.anyVal === false)
          assert(result.coordinate.array === true)
          assert(result.coordinate.optional === true)
          assert(result.coordinate.refined === false)
          assert(result.coordinate.scalaNumber === true)
          assert(result.name.value === "prop")
          assert(result.underlyingType.syntax === "scala.math.BigDecimal")
        }
      }

      "support optional refined AnyRef properties" in {
        val parsed = "def prop: Option[Refined[String, MinSize[Witness.`1`.T]]]".parse[Stat]

        assert(parsed.toEither.isRight)
        parsed.toEither.foreach { case stat: Decl.Def =>
          val result = analyzer.deconstruct(stat)

          assert(result.coordinate.anyVal === false)
          assert(result.coordinate.array === false)
          assert(result.coordinate.optional === true)
          assert(result.coordinate.refined === true)
          assert(result.coordinate.scalaNumber === false)
          assert(result.name.value === "prop")
          assert(result.underlyingType.syntax === "String")
        }
      }

      "support optional refined AnyVal properties" in {
        val parsed = "def prop: Option[Refined[Int, Interval.Closed[Witness.`0`.T, Witness.`9`.T]]]"
          .parse[Stat]

        assert(parsed.toEither.isRight)
        parsed.toEither.foreach { case stat: Decl.Def =>
          val result = analyzer.deconstruct(stat)

          assert(result.coordinate.anyVal === true)
          assert(result.coordinate.array === false)
          assert(result.coordinate.optional === true)
          assert(result.coordinate.refined === true)
          assert(result.coordinate.scalaNumber === false)
          assert(result.name.value === "prop")
          assert(result.underlyingType.syntax === "Int")
        }
      }

      "support optional refined collection AnyVal properties" in {
        val parsed =
          s"def prop: Option[Refined[${defaultTypes.array}[Short], MaxSize[Witness.`5`.T]]]"
            .parse[Stat]

        assert(parsed.toEither.isRight)
        parsed.toEither.foreach { case stat: Decl.Def =>
          val result = analyzer.deconstruct(stat)

          assert(result.coordinate.anyVal === true)
          assert(result.coordinate.array === true)
          assert(result.coordinate.optional === true)
          assert(result.coordinate.refined === true)
          assert(result.coordinate.scalaNumber === false)
          assert(result.name.value === "prop")
          assert(result.underlyingType.syntax === "Short")
        }
      }

      "support optional refined collection AnyRef properties" in {
        val parsed =
          s"def prop: Option[Refined[${defaultTypes.array}[Json], MaxSize[Witness.`5`.T]]]"
            .parse[Stat]

        assert(parsed.toEither.isRight)
        parsed.toEither.foreach { case stat: Decl.Def =>
          val result = analyzer.deconstruct(stat)

          assert(result.coordinate.anyVal === false)
          assert(result.coordinate.array === true)
          assert(result.coordinate.optional === true)
          assert(result.coordinate.refined === true)
          assert(result.coordinate.scalaNumber === false)
          assert(result.name.value === "prop")
          assert(result.underlyingType.syntax === "Json")
        }
      }

      "support refined AnyRef properties" in {
        val parsed = "def prop: Refined[Json, MaxSize[Witness.`5`.T]]".parse[Stat]

        assert(parsed.toEither.isRight)
        parsed.toEither.foreach { case stat: Decl.Def =>
          val result = analyzer.deconstruct(stat)

          assert(result.coordinate.anyVal === false)
          assert(result.coordinate.array === false)
          assert(result.coordinate.optional === false)
          assert(result.coordinate.refined === true)
          assert(result.coordinate.scalaNumber === false)
          assert(result.name.value === "prop")
          assert(result.underlyingType.syntax === "Json")
        }
      }

      "support refined AnyVal properties" in {
        val parsed = "def prop: Refined[Float, MaxSize[Witness.`5`.T]]".parse[Stat]

        assert(parsed.toEither.isRight)
        parsed.toEither.foreach { case stat: Decl.Def =>
          val result = analyzer.deconstruct(stat)

          assert(result.coordinate.anyVal === true)
          assert(result.coordinate.array === false)
          assert(result.coordinate.optional === false)
          assert(result.coordinate.refined === true)
          assert(result.coordinate.scalaNumber === false)
          assert(result.name.value === "prop")
          assert(result.underlyingType.syntax === "Float")
        }
      }

      "support refined collection AnyRef properties" in {
        val parsed = s"def prop: Refined[${defaultTypes.array}[Json], MaxSize[Witness.`5`.T]]"
          .parse[Stat]

        assert(parsed.toEither.isRight)
        parsed.toEither.foreach { case stat: Decl.Def =>
          val result = analyzer.deconstruct(stat)

          assert(result.coordinate.anyVal === false)
          assert(result.coordinate.array === true)
          assert(result.coordinate.optional === false)
          assert(result.coordinate.refined === true)
          assert(result.coordinate.scalaNumber === false)
          assert(result.name.value === "prop")
          assert(result.underlyingType.syntax === "Json")
        }
      }

      "support refined collection AnyVal properties" in {
        val parsed =
          s"def prop: Refined[${defaultTypes.array}[Float], MaxSize[Witness.`5`.T]]"
            .parse[Stat]

        assert(parsed.toEither.isRight)
        parsed.toEither.foreach { case stat: Decl.Def =>
          val result = analyzer.deconstruct(stat)

          assert(result.coordinate.anyVal === true)
          assert(result.coordinate.array === true)
          assert(result.coordinate.optional === false)
          assert(result.coordinate.refined === true)
          assert(result.coordinate.scalaNumber === false)
          assert(result.name.value === "prop")
          assert(result.underlyingType.syntax === "Float")
        }
      }

      "gracefully handle Either properties" in {
        val parsed = "def prop: Either[Int, Long]".parse[Stat]

        assert(parsed.toEither.isRight)
        parsed.toEither.foreach { case stat: Decl.Def =>
          val result = analyzer.deconstruct(stat)

          assert(result.coordinate.anyVal === false)
          assert(result.coordinate.array === false)
          assert(result.coordinate.optional === false)
          assert(result.coordinate.refined === false)
          assert(result.coordinate.scalaNumber === false)
          assert(result.name.value === "prop")
          assert(result.underlyingType.syntax === "Either[Int, Long]")
        }
      }

      "gracefully handle Map properties" in {
        val parsed = "def prop: Map[String, Json]".parse[Stat]

        assert(parsed.toEither.isRight)
        parsed.toEither.foreach { case stat: Decl.Def =>
          val result = analyzer.deconstruct(stat)

          assert(result.coordinate.anyVal === false)
          assert(result.coordinate.array === false)
          assert(result.coordinate.optional === false)
          assert(result.coordinate.refined === false)
          assert(result.coordinate.scalaNumber === false)
          assert(result.name.value === "prop")
          assert(result.underlyingType.syntax === "Map[String, Json]")
        }
      }
    }

    "inspecting class property definitions" must {
      "support scalar AnyRef properties" in {
        val parsed = defineClassWith("prop: String")

        assert(parsed.toEither.isRight)
        parsed.toEither.foreach { param =>
          val result = analyzer.deconstruct(param)

          assert(result.coordinate.anyVal === false)
          assert(result.coordinate.array === false)
          assert(result.coordinate.optional === false)
          assert(result.coordinate.refined === false)
          assert(result.coordinate.scalaNumber === false)
          assert(result.name.value === "prop")
          assert(result.underlyingType.syntax === "String")
        }
      }

      "support scalar AnyVal properties" in {
        val parsed = defineClassWith("prop: Int")

        assert(parsed.toEither.isRight)
        parsed.toEither.foreach { param =>
          val result = analyzer.deconstruct(param)

          assert(result.coordinate.anyVal === true)
          assert(result.coordinate.array === false)
          assert(result.coordinate.optional === false)
          assert(result.coordinate.refined === false)
          assert(result.coordinate.scalaNumber === false)
          assert(result.name.value === "prop")
          assert(result.underlyingType.syntax === "Int")
        }
      }

      "support collection AnyRef properties" in {
        val parsed = defineClassWith(s"prop: ${defaultTypes.array}[String]")

        assert(parsed.toEither.isRight)
        parsed.toEither.foreach { param =>
          val result = analyzer.deconstruct(param)

          assert(result.coordinate.anyVal === false)
          assert(result.coordinate.array === true)
          assert(result.coordinate.optional === false)
          assert(result.coordinate.refined === false)
          assert(result.coordinate.scalaNumber === false)
          assert(result.name.value === "prop")
          assert(result.underlyingType.syntax === "String")
        }
      }

      "support collection AnyVal properties" in {
        val parsed = defineClassWith(s"prop: ${defaultTypes.array}[Long]")

        assert(parsed.toEither.isRight)
        parsed.toEither.foreach { param =>
          val result = analyzer.deconstruct(param)

          assert(result.coordinate.anyVal === true)
          assert(result.coordinate.array === true)
          assert(result.coordinate.optional === false)
          assert(result.coordinate.refined === false)
          assert(result.coordinate.scalaNumber === false)
          assert(result.name.value === "prop")
          assert(result.underlyingType.syntax === "Long")
        }
      }

      "support optional AnyRef properties" in {
        val parsed = defineClassWith("prop: Option[String]")

        assert(parsed.toEither.isRight)
        parsed.toEither.foreach { param =>
          val result = analyzer.deconstruct(param)

          assert(result.coordinate.anyVal === false)
          assert(result.coordinate.array === false)
          assert(result.coordinate.optional === true)
          assert(result.coordinate.refined === false)
          assert(result.coordinate.scalaNumber === false)
          assert(result.name.value === "prop")
          assert(result.underlyingType.syntax === "String")
        }
      }

      "support optional AnyVal properties" in {
        val parsed = defineClassWith("prop: Option[Int]")

        assert(parsed.toEither.isRight)
        parsed.toEither.foreach { param =>
          val result = analyzer.deconstruct(param)

          assert(result.coordinate.anyVal === true)
          assert(result.coordinate.array === false)
          assert(result.coordinate.optional === true)
          assert(result.coordinate.refined === false)
          assert(result.coordinate.scalaNumber === false)
          assert(result.name.value === "prop")
          assert(result.underlyingType.syntax === "Int")
        }
      }

      "support optional collection AnyRef properties" in {
        val parsed = defineClassWith(s"prop: Option[${defaultTypes.array}[String]]")

        assert(parsed.toEither.isRight)
        parsed.toEither.foreach { param =>
          val result = analyzer.deconstruct(param)

          assert(result.coordinate.anyVal === false)
          assert(result.coordinate.array === true)
          assert(result.coordinate.optional === true)
          assert(result.coordinate.refined === false)
          assert(result.coordinate.scalaNumber === false)
          assert(result.name.value === "prop")
          assert(result.underlyingType.syntax === "String")
        }
      }

      "support optional collection AnyVal properties" in {
        val parsed = defineClassWith(s"prop: Option[${defaultTypes.array}[Byte]]")

        assert(parsed.toEither.isRight)
        parsed.toEither.foreach { param =>
          val result = analyzer.deconstruct(param)

          assert(result.coordinate.anyVal === true)
          assert(result.coordinate.array === true)
          assert(result.coordinate.optional === true)
          assert(result.coordinate.refined === false)
          assert(result.coordinate.scalaNumber === false)
          assert(result.name.value === "prop")
          assert(result.underlyingType.syntax === "Byte")
        }
      }

      "support optional collection ScalaNumber properties" in {
        val parsed = defineClassWith(s"prop: Option[${defaultTypes.array}[BigInt]]")

        assert(parsed.toEither.isRight)
        parsed.toEither.foreach { param =>
          val result = analyzer.deconstruct(param)

          assert(result.coordinate.anyVal === false)
          assert(result.coordinate.array === true)
          assert(result.coordinate.optional === true)
          assert(result.coordinate.refined === false)
          assert(result.coordinate.scalaNumber === true)
          assert(result.name.value === "prop")
          assert(result.underlyingType.syntax === "BigInt")
        }
      }

      "support optional refined AnyRef properties" in {
        val parsed = defineClassWith("prop: Option[Refined[String, MinSize[Witness.`1`.T]]]")

        assert(parsed.toEither.isRight)
        parsed.toEither.foreach { param =>
          val result = analyzer.deconstruct(param)

          assert(result.coordinate.anyVal === false)
          assert(result.coordinate.array === false)
          assert(result.coordinate.optional === true)
          assert(result.coordinate.refined === true)
          assert(result.coordinate.scalaNumber === false)
          assert(result.name.value === "prop")
          assert(result.underlyingType.syntax === "String")
        }
      }

      "support optional refined AnyVal properties" in {
        val parsed = defineClassWith(
          "prop: Option[Refined[Int, Interval.Closed[Witness.`0`.T, Witness.`9`.T]]]"
        )

        assert(parsed.toEither.isRight)
        parsed.toEither.foreach { param =>
          val result = analyzer.deconstruct(param)

          assert(result.coordinate.anyVal === true)
          assert(result.coordinate.array === false)
          assert(result.coordinate.optional === true)
          assert(result.coordinate.refined === true)
          assert(result.coordinate.scalaNumber === false)
          assert(result.name.value === "prop")
          assert(result.underlyingType.syntax === "Int")
        }
      }

      "support optional refined collection AnyVal properties" in {
        val parsed = defineClassWith(
          s"prop: Option[Refined[${defaultTypes.array}[Short], MaxSize[Witness.`5`.T]]]"
        )

        assert(parsed.toEither.isRight)
        parsed.toEither.foreach { param =>
          val result = analyzer.deconstruct(param)

          assert(result.coordinate.anyVal === true)
          assert(result.coordinate.array === true)
          assert(result.coordinate.optional === true)
          assert(result.coordinate.refined === true)
          assert(result.coordinate.scalaNumber === false)
          assert(result.name.value === "prop")
          assert(result.underlyingType.syntax === "Short")
        }
      }

      "support optional refined collection AnyRef properties" in {
        val parsed = defineClassWith(
          s"prop: Option[Refined[${defaultTypes.array}[Json], MaxSize[Witness.`5`.T]]]"
        )

        assert(parsed.toEither.isRight)
        parsed.toEither.foreach { param =>
          val result = analyzer.deconstruct(param)

          assert(result.coordinate.anyVal === false)
          assert(result.coordinate.array === true)
          assert(result.coordinate.optional === true)
          assert(result.coordinate.refined === true)
          assert(result.coordinate.scalaNumber === false)
          assert(result.name.value === "prop")
          assert(result.underlyingType.syntax === "Json")
        }
      }

      "support refined AnyRef properties" in {
        val parsed = defineClassWith("prop: Refined[Json, MaxSize[Witness.`5`.T]]")

        assert(parsed.toEither.isRight)
        parsed.toEither.foreach { param =>
          val result = analyzer.deconstruct(param)

          assert(result.coordinate.anyVal === false)
          assert(result.coordinate.array === false)
          assert(result.coordinate.optional === false)
          assert(result.coordinate.refined === true)
          assert(result.coordinate.scalaNumber === false)
          assert(result.name.value === "prop")
          assert(result.underlyingType.syntax === "Json")
        }
      }

      "support refined AnyVal properties" in {
        val parsed = defineClassWith("prop: Refined[Float, MaxSize[Witness.`5`.T]]")

        assert(parsed.toEither.isRight)
        parsed.toEither.foreach { param =>
          val result = analyzer.deconstruct(param)

          assert(result.coordinate.anyVal === true)
          assert(result.coordinate.array === false)
          assert(result.coordinate.optional === false)
          assert(result.coordinate.refined === true)
          assert(result.coordinate.scalaNumber === false)
          assert(result.name.value === "prop")
          assert(result.underlyingType.syntax === "Float")
        }
      }

      "support refined collection AnyRef properties" in {
        val parsed = defineClassWith(
          s"prop: Refined[${defaultTypes.array}[Json], MaxSize[Witness.`5`.T]]"
        )

        assert(parsed.toEither.isRight)
        parsed.toEither.foreach { param =>
          val result = analyzer.deconstruct(param)

          assert(result.coordinate.anyVal === false)
          assert(result.coordinate.array === true)
          assert(result.coordinate.optional === false)
          assert(result.coordinate.refined === true)
          assert(result.coordinate.scalaNumber === false)
          assert(result.name.value === "prop")
          assert(result.underlyingType.syntax === "Json")
        }
      }

      "support refined collection AnyVal properties" in {
        val parsed = defineClassWith(
          s"prop: Refined[${defaultTypes.array}[Float], MaxSize[Witness.`5`.T]]"
        )

        assert(parsed.toEither.isRight)
        parsed.toEither.foreach { param =>
          val result = analyzer.deconstruct(param)

          assert(result.coordinate.anyVal === true)
          assert(result.coordinate.array === true)
          assert(result.coordinate.optional === false)
          assert(result.coordinate.refined === true)
          assert(result.coordinate.scalaNumber === false)
          assert(result.name.value === "prop")
          assert(result.underlyingType.syntax === "Float")
        }
      }

      "gracefully handle Either properties" in {
        val parsed = defineClassWith("prop: Either[Int, Long]")

        assert(parsed.toEither.isRight)
        parsed.toEither.foreach { param =>
          val result = analyzer.deconstruct(param)

          assert(result.coordinate.anyVal === false)
          assert(result.coordinate.array === false)
          assert(result.coordinate.optional === false)
          assert(result.coordinate.refined === false)
          assert(result.coordinate.scalaNumber === false)
          assert(result.name.value === "prop")
          assert(result.underlyingType.syntax === "Either[Int, Long]")
        }
      }

      "gracefully handle Map properties" in {
        val parsed = defineClassWith("prop: Map[String, Json]")

        assert(parsed.toEither.isRight)
        parsed.toEither.foreach { param =>
          val result = analyzer.deconstruct(param)

          assert(result.coordinate.anyVal === false)
          assert(result.coordinate.array === false)
          assert(result.coordinate.optional === false)
          assert(result.coordinate.refined === false)
          assert(result.coordinate.scalaNumber === false)
          assert(result.name.value === "prop")
          assert(result.underlyingType.syntax === "Map[String, Json]")
        }
      }
    }
  }

  private def defineClassWith(property: String): Parsed[Term.Param] =
    s"""class Sample(
      |$property
      |)
      |""".stripMargin
      .parse[Source] match {
      case Parsed.Success(
            Source(
              List(
                Defn.Class.After_4_6_0(
                  _,
                  _,
                  _,
                  Ctor.Primary.After_4_6_0(
                    _,
                    _,
                    Seq(Term.ParamClause(List(param), _))
                  ),
                  _
                )
              )
            )
          ) =>
        Parsed.Success(param)

      case other: Parsed.Error =>
        other
    }
}
