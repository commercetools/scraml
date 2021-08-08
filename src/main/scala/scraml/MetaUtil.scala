package scraml

import scala.meta.{Term, Type}

object MetaUtil {
  def termSelect(parts: List[String], default: String): Term.Ref = parts match {
    case Nil                    => Term.Name(default)
    case first :: Nil           => Term.Name(first)
    case first :: second :: Nil => Term.Select(Term.Name(second), Term.Name(first))
    case first :: remainder     => Term.Select(termSelect(remainder, default), Term.Name(first))
  }

  def packageTerm(packageName: String): Term.Ref =
    termSelect(packageName.split("\\.").toList.reverse, packageName)

  def typeFromNameParts(parts: List[String], default: String): Type.Ref =
    parts match {
      case Nil                    => Type.Name(default)
      case first :: Nil           => Type.Name(first)
      case first :: second :: Nil => Type.Select(Term.Name(second), Type.Name(first))
      case first :: remainder     => Type.Select(termSelect(remainder, first), Type.Name(first))
    }

  def typeFromName(fullQualifiedName: String): Type.Ref =
    typeFromNameParts(fullQualifiedName.split("\\.").toList.reverse, fullQualifiedName)
}
