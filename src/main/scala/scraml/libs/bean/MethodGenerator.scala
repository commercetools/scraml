package scraml.libs.bean

import scala.meta._
import scraml.{BeanProperties, DefaultTypes, ModelGenContext}

sealed abstract class MethodGenerator(
    private val companion: Option[Defn.Object],
    private val group: PolicyGroup
)(implicit context: ModelGenContext) {
  import cats.syntax.semigroup._

  implicit private val defaultTypes: DefaultTypes = context.params.defaultTypes
  private val analyzer                            = new Analyzer(companion)
  private val policyLookup = Analyzer.Coordinate.permutations
    .map(c => c -> group(c))
    .toMap

  def apply(property: Decl.Def): Stat = {
    val deconstructed    = analyzer.deconstruct(property)
    val policy           = policyLookup(deconstructed.coordinate)
    val resultType: Type = policy.signature(deconstructed.underlyingType)
    val methodName       = beanName(deconstructed.name, resultType)

    q"def $methodName: $resultType"
  }

  def apply(property: Term.Param): Stat = {
    val deconstructed    = analyzer.deconstruct(property)
    val policy           = policyLookup(deconstructed.coordinate)
    val resultType: Type = policy.signature(deconstructed.underlyingType)
    val methodName       = beanName(deconstructed.name, resultType)
    val body: Term = policy.body(
      Term.Name(deconstructed.name.value),
      deconstructed.underlyingType
    )

    define(methodName, resultType, body)
  }

  protected def define(name: Term.Name, resultType: Type, body: Term): Stat

  private def beanName(name: Name, resultType: Type): Term.Name = {
    val renderedTypeName = resultType.syntax
    val prefix =
      if (IsAnyVal(renderedTypeName) && renderedTypeName.equalsIgnoreCase("boolean"))
        "is"
      else
        "get"

    Term.Name(prefix |+| name.value.capitalize)
  }
}

object MethodGenerator {
  import BeanProperties._

  def create(companion: Option[Defn.Object])(implicit
      context: ModelGenContext
  ): MethodGenerator =
    context.params.beanProperties.evaluate match {
      case EveryInvocation =>
        new DefMethodGenerator(
          companion,
          PolicyGroup(context.params.beanProperties)
        )

      case Once =>
        new LazyValMethodGenerator(
          companion,
          PolicyGroup(context.params.beanProperties)
        )
    }
}

final class DefMethodGenerator(
    private val companion: Option[Defn.Object],
    private val group: PolicyGroup
)(implicit context: ModelGenContext)
    extends MethodGenerator(companion, group) {
  override protected def define(methodName: Term.Name, resultType: Type, body: Term): Stat =
    q"def $methodName: $resultType = $body"
}

final class LazyValMethodGenerator(
    private val companion: Option[Defn.Object],
    private val group: PolicyGroup
)(implicit context: ModelGenContext)
    extends MethodGenerator(companion, group) {
  override protected def define(methodName: Term.Name, resultType: Type, body: Term): Stat =
    q"lazy val ${Pat.Var(methodName)}: $resultType = $body"
}
