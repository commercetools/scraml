package scraml.libs.bean

import scala.annotation.unused
import scala.meta._

import cats.Semigroup
import scraml.DefaultTypes

trait GenSignature {
  def signature(underlying: Type)(implicit
      defaultTypes: DefaultTypes
  ): Type
}

object GenSignature {
  sealed trait Container extends GenSignature

  object Container {
    implicit val containerSemigroup: Semigroup[Container] =
      new Semigroup[Container] {
        override def combine(a: Container, b: Container): Container =
          new Container {
            override def signature(underlying: Type)(implicit
                defaultTypes: DefaultTypes
            ): Type =
              a.signature(b.signature(underlying))
          }
      }
  }

  val javaCollection: Container = new Container {
    override def signature(underlying: Type)(implicit
        defaultTypes: DefaultTypes
    ): Type =
      parseType(s"java.util.List[${underlying.syntax}]")
  }

  val javaOptional: Container = new Container {
    override def signature(underlying: Type)(implicit
        defaultTypes: DefaultTypes
    ): Type =
      parseType(s"java.util.Optional[${underlying.syntax}]")
  }

  val scalaCollection: Container = new Container {
    override def signature(underlying: Type)(implicit
        defaultTypes: DefaultTypes
    ): Type =
      parseType(s"${defaultTypes.array}[${underlying.syntax}]")
  }

  val scalaOptional: Container = new Container {
    override def signature(underlying: Type)(implicit
        @unused defaultTypes: DefaultTypes
    ): Type =
      parseType(s"Option[${underlying.syntax}]")
  }

  private def parseType(definition: String): Type =
    definition.parse[Type].get
}
