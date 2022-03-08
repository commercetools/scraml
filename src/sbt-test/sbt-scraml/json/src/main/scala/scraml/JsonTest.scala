package scraml

import io.circe._
import io.circe.parser._
import io.circe.syntax._

object JsonTest extends App {

  checkIntermediate()
  checkAdditional()
  checkDefault()


  def checkIntermediate(): Unit = {
    val grandchild = GrandchildType(
      id = "someid",
      foo = None,
      aDouble = 1.23,
      aFloat = 4.56F,
      anInt = 5,
      aLong = 6L,
      customTypeProp = scala.math.BigDecimal(42),
      customArrayTypeProp = Vector(scala.math.BigDecimal(99))
    )()

    assert(grandchild.asJson == IntermediateType.encoder(grandchild))
    assert(grandchild.additionalProperties.isEmpty)
  }

  def checkAdditional(): Unit = {
    val grandchild = GrandchildType(
      id = "someid",
      foo = None,
      aDouble = 1.23,
      aFloat = 4.56F,
      anInt = 5,
      aLong = 6L,
      customTypeProp = scala.math.BigDecimal(42),
      customArrayTypeProp = Vector(scala.math.BigDecimal(99))
    )(
      additionalProperties = Some(
        GrandchildType.AdditionalProperties(
          Map[String, Json](
            "extraInt" -> Json.fromInt(54321),
            "extraObj" -> Json.fromFields(
              Map[String, Json](
                "objField1" -> Json.fromBoolean(true),
                "objField2" -> Json.fromString("hello, world!"),
                "objField3" -> Json.arr(Json.fromInt(1), Json.fromInt(2))
              )
            )
          )
        )
      )
    )

    assert(grandchild.asJson == IntermediateType.encoder(grandchild))
    assert(!grandchild.additionalProperties.isEmpty)
    assert(grandchild.asJson.toString.contains("extraInt"))
    assert(grandchild.asJson.toString.contains("extraObj"))
    assert(grandchild.asJson.toString.contains("objField1"))
    assert(grandchild.asJson.toString.contains("objField2"))
    assert(grandchild.asJson.toString.contains("objField3"))
    assert(
      parse(grandchild.asJson.toString) == Right(grandchild.asJson),
      s"JSON did not round trip:\nparse: ${parse(grandchild.asJson.toString)}"
    )
  }

  def checkDefault(): Unit = {
    val default = DefaultProperty()()

    assert(default.message ne null)
    assert(default.message != "")
    assert(default.asJson == DefaultProperty.encoder(default))
    assert(default.asJson.toString.contains("message"))
    assert(default.asJson.toString.contains(default.message))
    assert(default.asJson.toString.contains(default.limit.fold("<<missing>>")(_.toString)))
  }
}

