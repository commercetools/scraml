package scraml

import io.circe._
import io.circe.syntax._

object JsonTest extends App {
  val grandchild = GrandchildType(
    id = "someid",
    foo = None,
    aDouble = 1.23,
    aFloat = 4.56F,
    anInt = 5,
    aLong = 6L,
    customTypeProp = scala.math.BigDecimal(42),
    customArrayTypeProp = Vector(scala.math.BigDecimal(99))
  )

  assert(grandchild.asJson == IntermediateType.encoder(grandchild))
}

