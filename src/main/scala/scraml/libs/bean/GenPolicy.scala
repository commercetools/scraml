package scraml.libs.bean

import scala.annotation.unused
import scala.meta._

import cats.data.Ior
import scraml.DefaultTypes
import scraml.MetaUtil.typeFromName

sealed trait GenPolicy extends GenSignature {
  def body(instance: Term, underlying: Type): Term
}

object GenPolicy {
  object Default extends GenPolicy {
    override def body(instance: Term, @unused underlying: Type): Term = instance

    override def signature(underlying: Type)(implicit
        @unused defaultTypes: DefaultTypes
    ): Type = underlying
  }
}

object AnyValUseJavaLangPolicy extends GenPolicy {
  private val translationTable = Map(
    classOf[Boolean] -> "Boolean",
    classOf[Byte]    -> "Byte",
    classOf[Char]    -> "Character",
    classOf[Short]   -> "Short",
    classOf[Int]     -> "Integer",
    classOf[Long]    -> "Long",
    classOf[Float]   -> "Float",
    classOf[Double]  -> "Double"
  ).map { case (k, v) =>
    k.getSimpleName.capitalize -> typeFromName(s"java.lang.$v")
  }

  override def body(instance: Term, underlying: Type): Term =
    q"$instance.asInstanceOf[${translationTable(underlying.syntax.capitalize)}]"

  override def signature(underlying: Type)(implicit
      defaultTypes: DefaultTypes
  ): Type =
    translationTable(underlying.syntax.capitalize)
}

final case class ContainerPolicy(
    private val containerTransformer: Ior[GenSignature.Container, GenPolicy],
    private val mapper: Option[GenPolicy] = None
) extends GenPolicy {
  val hasTransformation = mapper.isDefined || containerTransformer.isRight

  override def body(instance: Term, underlying: Type): Term = {
    val mapped = mapper.fold(instance) { policy =>
      val placeholder = Term.Placeholder()

      q"$instance.map(${policy.body(placeholder, underlying)})"
    }

    containerTransformer.toOption
      .fold(mapped)(_.body(mapped, underlying))
  }

  override def signature(underlying: Type)(implicit
      defaultTypes: DefaultTypes
  ): Type = {
    val mapped = mapper.fold(underlying)(_.signature(underlying))

    containerTransformer
      .fold(
        _.signature(mapped),
        _.signature(mapped),
        (container, suffixer) => container.signature(suffixer.signature(mapped))
      )
  }
}

case object JavaCollectionPolicy extends GenPolicy {
  private val container = GenSignature.javaCollection

  override def body(instance: Term, @unused underlying: Type): Term =
    q"$instance.asJava"

  override def signature(underlying: Type)(implicit
      defaultTypes: DefaultTypes
  ): Type =
    container.signature(underlying)
}

case object JavaOptionalPolicy extends GenPolicy {
  private val container = GenSignature.javaOptional

  override def body(instance: Term, @unused underlying: Type): Term =
    q"$instance.toJava"

  override def signature(underlying: Type)(implicit
      defaultTypes: DefaultTypes
  ): Type =
    container.signature(underlying)
}

final case class NullableOptionPolicy(
    private val mapper: Option[GenPolicy]
) extends GenPolicy {
  override def body(instance: Term, underlying: Type): Term = {
    val mapped = mapper.fold(instance) { policy =>
      val placeholder = Term.Placeholder()

      q"$instance.map(${policy.body(placeholder, underlying)})"
    }

    q"$mapped.orNull"
  }

  override def signature(underlying: Type)(implicit
      defaultTypes: DefaultTypes
  ): Type =
    mapper.fold(underlying)(_.signature(underlying))
}

final case class RefinedPolicy(private val next: GenPolicy) extends GenPolicy {
  override def body(instance: Term, underlying: Type): Term =
    next.body(q"$instance.value", underlying)

  override def signature(underlying: Type)(implicit
      defaultTypes: DefaultTypes
  ): Type = next.signature(underlying)
}

object ScalaNumberUseJavaLangPolicy extends GenPolicy {
  private val translationTable = Map(
    classOf[BigDecimal].getName       -> "BigDecimal",
    classOf[BigDecimal].getSimpleName -> "BigDecimal",
    classOf[BigInt].getName           -> "BigInteger",
    classOf[BigInt].getSimpleName     -> "BigInteger"
  ).map { case (k, v) =>
    k -> typeFromName(s"java.math.$v")
  }

  override def body(instance: Term, underlying: Type): Term =
    if (underlying.syntax.endsWith("BigInt"))
      q"$instance.bigInteger"
    else
      q"$instance.bigDecimal"

  override def signature(underlying: Type)(implicit
      defaultTypes: DefaultTypes
  ): Type =
    translationTable(underlying.syntax)
}
