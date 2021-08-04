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
      case baseType :: dataType :: emptyBase :: noProps :: Nil =>
        baseType.source.packageName should be("datatypes")
        baseType.source.source.toString() should be("sealed trait BaseType extends Any { def id: String }")
        baseType.source.companion.map(_.toString()) should be(Some(
          s"""object BaseType""".stripMargin))

        baseType.source.name should be("BaseType")
        baseType.file.getPath should be("target/scraml-test/scraml/datatypes.scala")

        dataType.source.packageName should be("datatypes")
        dataType.source.source.toString() should be("final case class DataType(id: String, foo: Option[String] = None, customTypeProp: scala.long.BigDecimal, customArrayTypeProp: Vector[scala.long.BigDecimal] = Vector.empty) extends BaseType")
        dataType.source.name should be("DataType")
        dataType.file.getPath should be("target/scraml-test/scraml/datatypes.scala")

        emptyBase.source.source.toString() should be("sealed trait EmptyBase")
        noProps.source.source.toString() should be(s"""case object NoProps extends EmptyBase""".stripMargin)
      case _ => fail()
    }
  }

  it should "create a package from string" in {
    MetaUtil.packageTerm("a").toString() should be("a")
    MetaUtil.packageTerm("a.b").toString() should be("a.b")
    MetaUtil.packageTerm("a.b.c").toString() should be("a.b.c")
    MetaUtil.packageTerm("a.b.c.d.e").toString() should be(
      Term.Select(Term.Select(Term.Select(Term.Select(Term.Name("a"), Term.Name("b")), Term.Name("c")), Term.Name("d")), Term.Name("e")).toString()
    )
  }

  it should "create a type from string" in {
    MetaUtil.typeFromName("a").toString() should be("a")
    MetaUtil.typeFromName("a.b").toString() should be("a.b")
    MetaUtil.typeFromName("a.b.c").toString() should be("a.b.c")
    MetaUtil.typeFromName("a.b.c.d.e").toString() should be("a.b.c.d.e")
  }
}
