package scraml

import io.circe._
import io.circe.parser._
import io.circe.syntax._

object RefinedTest extends App {
  checkNoAdditional()
  checkAdditional()
  checkDefault()


  def checkNoAdditional(): Unit = {
    /// Because `SomeMapType` had explicit pattern properties and nothing else,
    /// all of its properties are in `SomeMapType.AdditionalProperties`.
    val someMapType = SomeMapType()(
      Some(
        SomeMapType.AdditionalProperties(
          Map(
            "a" -> Json.fromInt(1),
            "b" -> Json.fromString("two")
          )
        )
      )
    )

    val noPropsWithout = NoProps.from()
    val dataTypeWithout = DataType.from(
      id = "valid-id",
      optionalCustomArrayTypeProp = None,
      foo = Some("hello, world!"),
      bar = None,
      numberProp = 0.99F,
      customNumberProp = BigDecimal(1.23),
      customArrayTypeProp = Vector(BigDecimal(4.56))
    )

    assert(noPropsWithout.isRight)
    assert(dataTypeWithout.isRight)
    assert(
      parse(someMapType.asJson.toString) == Right(someMapType.asJson),
      s"JSON did not round trip:\nparse: ${parse(someMapType.asJson.toString)}"
    )

    assert(
      noPropsWithout.flatMap(np => parse(np.asJson.toString)) == noPropsWithout.map(_.asJson),
      s"JSON did not round trip:\nparse: ${noPropsWithout.flatMap(np => parse(np.asJson.toString))}"
    )

    assert(
      dataTypeWithout.flatMap(np => parse(np.asJson.toString)) == dataTypeWithout.map(_.asJson),
      s"JSON did not round trip:\nparse: ${dataTypeWithout.flatMap(np => parse(np.asJson.toString))}"
    )
  }

  def checkAdditional(): Unit = {
    val noPropsWith = NoProps.from(
      Some(
        NoProps.AdditionalProperties(
          Map(
            "extra" -> Json.fromString("added to instance")
          )
        )
      )
    )

    val dataTypeWith = DataType.from(
      id = "valid-id",
      optionalCustomArrayTypeProp = None,
      foo = Some("hello, world!"),
      bar = None,
      numberProp = 0.99F,
      customNumberProp = BigDecimal(1.23),
      customArrayTypeProp = Vector(BigDecimal(4.56)),
      additionalProperties = Some(
        DataType.AdditionalProperties(
          Map(
            "extra" -> Json.fromString("added to instance")
          )
        )
      )
    )

    assert(
      noPropsWith.flatMap(np => parse(np.asJson.toString)) == noPropsWith.map(_.asJson),
      s"JSON did not round trip:\nparse: ${noPropsWith.flatMap(np => parse(np.asJson.toString))}"
    )

    assert(
      dataTypeWith.flatMap(np => parse(np.asJson.toString)) == dataTypeWith.map(_.asJson),
      s"JSON did not round trip:\nparse: ${dataTypeWith.flatMap(np => parse(np.asJson.toString))}"
    )
  }

  def checkDefault(): Unit = {
    val default = DefaultProperty()

    assert(default.message == DefaultProperty.MessageType.default)
    assert(default.limit == DefaultProperty.LimitType.default)
    assert(default.limit.isDefined)
    assert(default.longInteger.value == Long.MinValue)
    assert(default.longNumber.exists(_.value == Long.MinValue))
    assert(default.asJson == DefaultProperty.encoder(default))
    assert(default.asJson.toString.contains("message"))
    assert(default.asJson.toString.contains(default.message.value))
    assert(default.asJson.toString.contains(default.limit.fold("<<missing>>")(_.value.toString)))
  }
}

