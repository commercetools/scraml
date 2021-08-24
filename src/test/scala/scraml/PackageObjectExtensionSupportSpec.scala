package scraml

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scraml.libs.PackageObjectExtensionSupport

import scala.meta._

class PackageObjectExtensionSupportSpec extends AnyFlatSpec with Matchers {
  "Package Object Extension" should "add imports" in {
    val packageObject: Pkg.Object = q"""package object ${Term.Name("test")}"""

    PackageObjectExtensionSupport(additionalImports = Seq("scala.mutable._"))
      .modifyPackageObject(
        packageObject
      )
      .templ
      .stats
      .mkString should be("import scala.mutable._")
  }
}
