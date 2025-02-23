package scraml

import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scraml.libs.SphereJsonSupport

import java.io.File

class SphereJsonSupportSpec extends AnyFlatSpec with Matchers {
  "Sphere JSON Support" should "generate JSON derivation" in {
    val params = ModelGenParams(
      new File("src/sbt-test/sbt-scraml/json/api/json.raml"),
      new File("target/scraml-sphere-json-test"),
      "scraml",
      FieldMatchPolicy.Exact(),
      DefaultTypes(),
      librarySupport = Set(SphereJsonSupport),
      None
    )

    val generated = ModelGenRunner.run(DefaultModelGen)(params).unsafeRunSync()

    generated.files match {
      case noDiscBase :: _ :: _ :: baseType :: _ :: _ :: dataType :: emptyBase :: noProps :: _ :: someEnum :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: Nil =>
        noDiscBase.source.source.toString() should be("sealed trait NoDiscriminatorBase")
        noDiscBase.source.companion.map(_.toString()) should be(
          Some(s"""object NoDiscriminatorBase {
             |  import io.sphere.json.generic._
             |  import io.sphere.json._
             |  import org.json4s._
             |  implicit val json: JSON[NoDiscriminatorBase] = new JSON[NoDiscriminatorBase] {
             |    override def read(jval: JsonAST.JValue): JValidation[NoDiscriminatorBase] = NoDiscriminatorSub1.json.read(jval).orElse(NoDiscriminatorSub2.json.read(jval))
             |    override def write(value: NoDiscriminatorBase): JsonAST.JValue = value match {
             |      case nodiscriminatorsub1: NoDiscriminatorSub1 =>
             |        NoDiscriminatorSub1.json.write(nodiscriminatorsub1)
             |      case nodiscriminatorsub2: NoDiscriminatorSub2 =>
             |        NoDiscriminatorSub2.json.write(nodiscriminatorsub2)
             |    }
             |  }
             |}""".stripMargin)
        )

        someEnum.source.source.toString() should be("sealed trait SomeEnum")
        someEnum.source.companion.map(_.toString()) should be(Some(s"""object SomeEnum {
            |  case object A extends SomeEnum
            |  case object B extends SomeEnum
            |  case object enum extends SomeEnum
            |  case object `type` extends SomeEnum
            |  import io.sphere.json.ToJSON
            |  import io.sphere.json.FromJSON
            |  import io.sphere.json.JSONParseError
            |  import org.json4s._
            |  import cats.implicits.toContravariantOps
            |  import cats.data.Validated
            |  import cats.syntax.validated._
            |  implicit lazy val toJson: ToJSON[SomeEnum] = ToJSON.stringWriter.contramap(_.toString)
            |  implicit lazy val fromJson: FromJSON[SomeEnum] = (jval: JsonAST.JValue) => FromJSON.stringReader.read(jval).andThen {
            |    case "A" =>
            |      A.valid
            |    case "B" =>
            |      B.valid
            |    case "enum" =>
            |      enum.valid
            |    case "type" =>
            |      `type`.valid
            |    case other =>
            |      JSONParseError(s"not a instance of required enum: $$other").invalidNel
            |  }
            |}""".stripMargin))

        baseType.source.packageName should be("datatypes")
        baseType.source.source.toString() should be(
          "@io.sphere.json.annotations.JSONTypeHintField(\"type\") sealed trait BaseType extends Any { def id: String }"
        )
        baseType.source.companion.map(_.toString()) should be(Some(s"""object BaseType {
             |  import io.sphere.json.generic._
             |  import io.sphere.json._
             |  implicit lazy val json: JSON[BaseType] = deriveJSON[BaseType]
             |}""".stripMargin))

        baseType.source.name should be("BaseType")
        baseType.file.getPath should be("target/scraml-sphere-json-test/scraml/datatypes.scala")

        dataType.source.packageName should be("datatypes")
        dataType.source.source.toString() should be(
          "@io.sphere.json.annotations.JSONTypeHint(\"data\") final case class DataType(id: String, foo: Option[String] = None, customTypeProp: scala.math.BigDecimal, customArrayTypeProp: Vector[scala.math.BigDecimal] = Vector.empty) extends BaseType"
        )
        dataType.source.name should be("DataType")
        dataType.source.companion.map(_.toString()) should be(Some(s"""object DataType {
             |  import io.sphere.json.generic._
             |  import io.sphere.json._
             |  implicit lazy val json: JSON[DataType] = deriveJSON[DataType]
             |}""".stripMargin))

        emptyBase.source.source.toString() should be(
          "@io.sphere.json.annotations.JSONTypeHintField(\"type\") sealed trait EmptyBase"
        )
        noProps.source.source.toString() should be(
          s"""@io.sphere.json.annotations.JSONTypeHint(\"nope\") case object NoProps extends EmptyBase {
                                                      |  import io.sphere.json.generic._
                                                      |  implicit lazy val json = jsonProduct0(NoProps)
                                                      |}""".stripMargin
        )
    }
  }
}
