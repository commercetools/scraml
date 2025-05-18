package scraml

import java.io.File
import scala.jdk.CollectionConverters._
import cats.effect.unsafe.implicits.global
import io.vrap.rmf.raml.model.types.ObjectType
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scraml.libs.PackageObjectExtensionSupport

final class PackageObjectExtensionSupportSpec extends AnyFlatSpec with Matchers {
  "Package Object Extension" should "add imports" in {
    val ramlFile =
      new File(getClass.getClassLoader.getResource("scala-extends/scala-extends.raml").toURI)
    val api = RMFUtil.readModel(ramlFile).unsafeRunSync()

    val packageObject = {
      import scala.meta._

      q"""package object ${Term.Name("test")}"""
    }

    PackageObjectExtensionSupport(additionalImports = Seq("scala.mutable._"))
      .modifyPackageObject(List.empty, api)(
        ModelGenContext(
          "test",
          api.getTypes.asScala.collectFirst { case ot: ObjectType =>
            ot
          }.get,
          ModelGenParams(
            new File("src/sbt-test/sbt-scraml/cats/api/simple.raml"),
            new File("target/scraml-pkg-extension"),
            "scraml",
            FieldMatchPolicy.IgnoreExtra(),
            DefaultTypes(),
            librarySupport = Set.empty,
            formatConfig = None
          ),
          ApiContext(api)
        )
      )(packageObject)
      .templ
      .exprs
      .mkString should be("import scala.mutable._")
  }
}
