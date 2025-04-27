package scraml.libs.bean

import scala.meta._
import scraml.DefaultTypes

final class Analyzer(private val companion: Option[Defn.Object]) {
  import Analyzer._
  import cats.syntax.eq._

  private val companionTypes = companion.toList
    .flatMap(_.templ.exprs)
    .collect { case keep: Defn.Type =>
      keep.name.value -> keep.body
    }
    .toMap

  def deconstruct(property: Decl.Def)(implicit
      defaultTypes: DefaultTypes
  ): DeconstructedProperty = {
    inspect(property.name, property.decltpe)
  }

  def deconstruct(property: Term.Param)(implicit
      defaultTypes: DefaultTypes
  ): DeconstructedProperty = {
    (property.name, property.decltpe) match {
      case (name, Some(expected: Type)) =>
        inspect(Term.Name(name.value), expected)

      case (_, Some(other)) =>
        throw new IllegalArgumentException(
          s"""internal plugin error -- unsupported class property AST definition
             |detected.
             |
             |  property : ${other.syntax}
             |""".stripMargin
        )

      case _ =>
        throw new RuntimeException(
          s"""internal plugin error -- unable to parse class property AST
             |definition.
             |
             |  property : ${property.syntax}
             |""".stripMargin
        )
    }
  }

  private def inspect(name: Term.Name, decl: Type)(implicit
      defaultTypes: DefaultTypes
  ): DeconstructedProperty = {
    val property = Decl.Def(
      mods = Nil,
      name = name,
      paramClauseGroups = Nil,
      decltpe = resolveTypeAlias(decl)
    )

    property match {
      case q"def $name: Option[Refined[$col[${underlying: Type}], $_]]"
          if col.syntax === defaultTypes.array =>
        DeconstructedProperty(
          Coordinate(
            anyVal = IsAnyVal(underlying),
            array = true,
            optional = true,
            refined = true,
            scalaNumber = IsScalaNumber(underlying)
          ),
          name,
          underlying
        )

      case q"def $name: Option[Refined[${underlying: Type}, $_]]" =>
        DeconstructedProperty(
          Coordinate(
            anyVal = IsAnyVal(underlying),
            optional = true,
            refined = true,
            scalaNumber = IsScalaNumber(underlying)
          ),
          name,
          underlying
        )

      case q"def $name: Option[$col[${underlying: Type}]]" if col.syntax === defaultTypes.array =>
        DeconstructedProperty(
          Coordinate(
            anyVal = IsAnyVal(underlying),
            array = true,
            optional = true,
            scalaNumber = IsScalaNumber(underlying)
          ),
          name,
          underlying
        )

      case q"def $name: Option[${underlying: Type}]" =>
        DeconstructedProperty(
          Coordinate(
            anyVal = IsAnyVal(underlying),
            optional = true,
            scalaNumber = IsScalaNumber(underlying)
          ),
          name,
          underlying
        )

      case q"def $name: Refined[$col[${underlying: Type}], $_]"
          if col.syntax === defaultTypes.array =>
        DeconstructedProperty(
          Coordinate(
            anyVal = IsAnyVal(underlying),
            array = true,
            refined = true,
            scalaNumber = IsScalaNumber(underlying)
          ),
          name,
          underlying
        )

      case q"def $name: Refined[$underlying, $_]" =>
        DeconstructedProperty(
          Coordinate(
            anyVal = IsAnyVal(underlying),
            refined = true,
            scalaNumber = IsScalaNumber(underlying)
          ),
          name,
          underlying
        )

      case q"def $name: ${col}[${underlying: Type}]" if col.syntax === defaultTypes.array =>
        DeconstructedProperty(
          Coordinate(
            anyVal = IsAnyVal(underlying),
            array = true,
            scalaNumber = IsScalaNumber(underlying)
          ),
          name,
          underlying
        )

      case q"def $name: $underlying" =>
        DeconstructedProperty(
          Coordinate(
            anyVal = IsAnyVal(underlying),
            scalaNumber = IsScalaNumber(underlying)
          ),
          name,
          underlying
        )

      case unsupported =>
        throw new IllegalArgumentException(
          s"""internal plugin error -- unsupported property AST definition
             |detected.
             |
             |  property : ${unsupported.syntax}
             |""".stripMargin
        )
    }
  }

  private def resolveTypeAlias(declaration: Type): Type =
    companionTypes.getOrElse(
      declaration.syntax.replaceFirst("^.*\\.", ""),
      declaration
    )
}

object Analyzer {
  final case class Coordinate(
      anyVal: Boolean = false,
      array: Boolean = false,
      optional: Boolean = false,
      refined: Boolean = false,
      scalaNumber: Boolean = false
  )

  object Coordinate {
    private val booleans = Set(false, true)

    private val parameters = for {
      anyVal      <- booleans
      array       <- booleans
      optional    <- booleans
      refined     <- booleans
      scalaNumber <- booleans
    } yield (anyVal, array, optional, refined, scalaNumber)

    private val invalidCombinations = Set(
      // A specific property cannot be both an AnyVal and a ScalaNumber.
      Coordinate(anyVal = true, scalaNumber = true)
    )

    lazy val permutations: Set[Coordinate] = parameters.map((apply _).tupled) -- invalidCombinations
  }

  final case class DeconstructedProperty(
      coordinate: Coordinate,
      name: Name,
      underlyingType: Type
  )
}
