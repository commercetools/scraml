package scraml

import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.File
import scala.meta.Term

class DefaultModelGenSpec extends AnyFlatSpec with Matchers {
  "Default model gen" should "generate data type from API spec" in {
    val params = ModelGenParams(
      new File(getClass.getClassLoader.getResource("simple.raml").toURI),
      new File("target/scraml-test"),
      "scraml"
    )

    val generated = ModelGenRunner.run(DefaultModelGen)(params).unsafeRunSync()

    generated.files match {
      case baseType :: dataType :: Nil =>
        baseType.source.packageName should be("datatypes")
        baseType.source.source.toString() should be("sealed trait BaseType extends Any { def id: String }")
        baseType.source.name should be("BaseType")
        baseType.file.getPath should be("target/scraml-test/scraml/datatypes.scala")

        dataType.source.packageName should be("datatypes")
        dataType.source.source.toString() should be("final case class DataType(id: String, foo: Option[String] = None, customTypeProp: scala.long.BigDecimal, customArrayTypeProp: Vector[scala.long.BigDecimal] = Vector.empty) extends BaseType")
        dataType.source.name should be("DataType")
        dataType.file.getPath should be("target/scraml-test/scraml/datatypes.scala")

      case _ => fail()
    }
  }

  it should "create a package from string" in {
    DefaultModelGen.packageTerm("a").toString() should be("a")
    DefaultModelGen.packageTerm("a.b").toString() should be("a.b")
    DefaultModelGen.packageTerm("a.b.c").toString() should be("a.b.c")
    DefaultModelGen.packageTerm("a.b.c.d.e").toString() should be(
      Term.Select(Term.Select(Term.Select(Term.Select(Term.Name("a"), Term.Name("b")), Term.Name("c")), Term.Name("d")), Term.Name("e")).toString()
    )
  }

  it should "create a type from string" in {
    DefaultModelGen.typeFromName("a").toString() should be("a")
    DefaultModelGen.typeFromName("a.b").toString() should be("a.b")
    DefaultModelGen.typeFromName("a.b.c").toString() should be("a.b.c")
    DefaultModelGen.typeFromName("a.b.c.d.e").toString() should be("a.b.c.d.e")
  }
}
