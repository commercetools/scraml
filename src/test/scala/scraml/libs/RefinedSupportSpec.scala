package scraml.libs

import java.io.File

import cats.effect.unsafe.implicits.global
import org.scalatest.diagrams.Diagrams
import org.scalatest.wordspec.AnyWordSpec
import scraml.{DefaultModelGen, DefaultTypes, ModelGenParams, ModelGenRunner}

class RefinedSupportSpec extends AnyWordSpec with Diagrams {
  "RefinedSupport" must {
    "modify case class refined property types" in {
      val params = ModelGenParams(
        new File("src/sbt-test/sbt-scraml/refined/api/refined.raml"),
        new File("target/scraml-refined-test"),
        "scraml",
        DefaultTypes(),
        librarySupport = Set(CirceJsonSupport(), RefinedSupport),
        formatConfig = None,
        generateDateCreated = true
      )

      val generated = ModelGenRunner.run(DefaultModelGen)(params).unsafeRunSync()

      assert(generated.files.nonEmpty)

      val theSource = generated.files
        .find(_.source.name == "DataType")
        .map(_.source.source.toString())

      val theCompanion = generated.files
        .find(_.source.name == "DataType")
        .flatMap(_.source.companion)
        .map(_.toString())

      assert(
        theSource.contains(
          """final case class DataType(id: DataType.IdType, optionalCustomArrayTypeProp: DataType.OptionalCustomArrayTypePropType = None, foo: Option[String] = None, bar: DataType.BarType = None, numberProp: DataType.NumberPropType, customNumberProp: DataType.CustomNumberPropType, customArrayTypeProp: DataType.CustomArrayTypePropType, optionalStringArray: DataType.OptionalStringArrayType = None) extends BaseType"""
        )
      )

      assert(
        theCompanion.contains(
          """object DataType {
            |  import eu.timepit.refined.api.Refined
            |  import eu.timepit.refined.boolean.And
            |  import eu.timepit.refined.collection._
            |  import eu.timepit.refined.numeric._
            |  import eu.timepit.refined.string._
            |  import shapeless.Witness
            |  import io.circe.refined._
            |  type IdType = Refined[String, And[MinSize[Witness.`1`.T], And[MaxSize[Witness.`10`.T], MatchesRegex[Witness.`"^[A-z0-9-.]+$"`.T]]]]
            |  type OptionalCustomArrayTypePropItemPredicate = GreaterEqual[Witness.`0.0`.T]
            |  type OptionalCustomArrayTypePropType = Option[Refined[Set[scala.math.BigDecimal], And[MinSize[Witness.`1`.T], And[MaxSize[Witness.`100`.T], Forall[OptionalCustomArrayTypePropItemPredicate]]]]]
            |  type BarType = Option[Refined[String, MatchesRegex[Witness.`"^[A-z]+$"`.T]]]
            |  type NumberPropType = Refined[Float, Interval.Closed[Witness.`0`.T, Witness.`99.99999`.T]]
            |  type CustomNumberPropType = Refined[scala.math.BigDecimal, LessEqual[Witness.`99.99999`.T]]
            |  type CustomArrayTypePropItemPredicate = Interval.Closed[Witness.`1.23`.T, Witness.`4.56`.T]
            |  type CustomArrayTypePropType = Refined[Vector[scala.math.BigDecimal], And[MaxSize[Witness.`100`.T], Forall[CustomArrayTypePropItemPredicate]]]
            |  type OptionalStringArrayItemPredicate = And[MinSize[Witness.`2`.T], And[MaxSize[Witness.`42`.T], MatchesRegex[Witness.`"^[A-z0-9]+$"`.T]]]
            |  type OptionalStringArrayType = Option[Refined[scala.collection.immutable.List[String], Forall[OptionalStringArrayItemPredicate]]]
            |  import io.circe._
            |  import io.circe.generic.semiauto._
            |  implicit lazy val decoder: Decoder[DataType] = deriveDecoder[DataType]
            |  implicit lazy val encoder: Encoder[DataType] = deriveEncoder[DataType].mapJsonObject(_.add("type", Json.fromString("data")))
            |}""".stripMargin
        )
      )
    }
  }
}
