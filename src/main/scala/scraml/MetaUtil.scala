package scraml

import scala.meta.{Name, Term, Type}

object MetaUtil {
  private val overrideSuffix = "$scraml"

  def addOverrideSuffix(name: Name): Name =
    Name(addOverrideSuffix(name.value))

  def addOverrideSuffix(name: String): String =
    removeOverrideSuffix(name) + overrideSuffix

  def hasOverrideSuffix(name: Name): Boolean =
    hasOverrideSuffix(name.value)

  def hasOverrideSuffix(name: String): Boolean =
    name.endsWith(overrideSuffix)

  def removeOverrideSuffix(name: Name): Name =
    Name(name.value.stripSuffix(overrideSuffix))

  def removeOverrideSuffix(name: String): String =
    name.stripSuffix(overrideSuffix)

  def termSelect(parts: List[String], default: String): Term.Ref = parts match {
    case Nil                    => Term.Name(default)
    case first :: Nil           => Term.Name(first)
    case first :: second :: Nil => Term.Select(Term.Name(second), Term.Name(first))
    case first :: remainder     => Term.Select(termSelect(remainder, default), Term.Name(first))
  }

  def packageTerm(packageName: String): Term.Ref =
    termSelect(packageName.split("\\.").toList.reverse, packageName)

  def termFromName(fullyQualifiedName: String): Term.Ref =
    termSelect(fullyQualifiedName.split('.').toList.reverse, fullyQualifiedName)

  def typeFromNameParts(parts: List[String], default: String): Type.Ref =
    parts match {
      case Nil                    => Type.Name(default)
      case first :: Nil           => Type.Name(first)
      case first :: second :: Nil => Type.Select(Term.Name(second), Type.Name(first))
      case first :: remainder     => Type.Select(termSelect(remainder, first), Type.Name(first))
    }

  def typeFromName(fullQualifiedName: String): Type.Ref =
    typeFromNameParts(fullQualifiedName.split("\\.").toList.reverse, fullQualifiedName)

  def isTypeApply(scalaType: Type, typeName: String): Boolean = scalaType match {
    case typeApply: Type.Apply =>
      typeApply.tpe match {
        case Type.Name(name) if name == typeName => true
        case _                                   => false
      }
    case _ => false
  }
}
