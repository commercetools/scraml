package scraml

import io.circe._
import io.circe.syntax._

object JsonTest extends App {
  val grandchild = GrandchildType(id = "someid", customTypeProp = scala.math.BigDecimal(42))
  val imCodec = implicitly[Codec[IntermediateType]]

  println(grandchild.asJson)
  println(im(grandchild))
}

