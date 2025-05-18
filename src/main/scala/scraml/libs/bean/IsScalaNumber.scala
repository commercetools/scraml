package scraml.libs.bean

import scala.meta._

object IsScalaNumber {
  private val types = List(
    classOf[BigDecimal],
    classOf[BigInt]
  ).flatMap { theType =>
    theType.getName :: theType.getSimpleName :: Nil
  }.toSet

  def apply(name: Type): Boolean = apply(name.syntax)

  def apply(name: Type.Name): Boolean = apply(name.value)

  def apply(name: String): Boolean = types.contains(name)

  def unapply(candidate: AnyRef): Boolean =
    candidate match {
      case Type.Name(value) =>
        apply(value)

      case tpe: Type =>
        apply(tpe.syntax)

      case value: String =>
        apply(value)

      case _ =>
        false
    }
}
