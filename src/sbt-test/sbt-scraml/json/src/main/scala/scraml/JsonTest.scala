package scraml

import io.circe._
import io.circe.syntax._

object JsonTest extends App {
  val grandchild = GrandchildType(id = "someid", customTypeProp = scala.math.BigDecimal(42))

  assert(grandchild.asJson == IntermediateType.encoder(grandchild))
}

