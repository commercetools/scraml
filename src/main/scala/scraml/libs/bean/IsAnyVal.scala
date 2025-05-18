package scraml.libs.bean

import scala.meta.{Name, Type}

object IsAnyVal {
  // Even though Unit is technically an AnyVal, it is excluded here as it
  // does not qualify as a valid Java Bean result type.
  private val types = Set(
    classOf[Boolean],
    classOf[Byte],
    classOf[Char],
    classOf[Short],
    classOf[Int],
    classOf[Long],
    classOf[Float],
    classOf[Double]
  ).map(_.getSimpleName.capitalize)

  def apply(name: Type): Boolean = apply(name.syntax)

  def apply(name: Type.Name): Boolean = apply(name.value)

  def apply(name: String): Boolean = types.contains(name.capitalize)

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
