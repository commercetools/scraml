package scraml

import io.vrap.rmf.raml.model.modules.impl.{ApiImpl, ModulesFactoryImpl}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scraml.libs.PackageObjectExtensionSupport

class PackageObjectExtensionSupportSpec extends AnyFlatSpec with Matchers {
  "Package Object Extension" should "add imports" in {
    val packageObject = {
      import scala.meta._
      q"""package object ${Term.Name("test")}"""
    }

    PackageObjectExtensionSupport(additionalImports = Seq("scala.mutable._"))
      .modifyPackageObject(new ModulesFactoryImpl().createApi())(
        packageObject
      )
      .templ
      .stats
      .mkString should be("import scala.mutable._")
  }
}
