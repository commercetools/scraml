package scraml

import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.File

class DefaultModelGenSpec extends AnyFlatSpec with Matchers {
  "Default model gen" should "generate data type from API spec" in {
    val params = ModelGenParams(
      new File(getClass.getClassLoader.getResource("simple.raml").toURI),
      new File("target/scraml-test"),
      "scraml"
    )

    ModelGenRunner.run(DefaultModelGen)(params).unsafeRunSync()
  }
}
