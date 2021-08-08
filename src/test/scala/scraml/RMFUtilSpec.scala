package scraml

import cats.effect.unsafe.implicits.global
import io.vrap.rmf.raml.model.types.ObjectType
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.jdk.CollectionConverters._
import java.io.File

class RMFUtilSpec extends AnyFlatSpec with Matchers {
  "RMF util" should "get all subtypes" in {
    val ramlFile =
      new File(getClass.getClassLoader.getResource("scala-extends/scala-extends.raml").toURI)
    val api = RMFUtil.readModel(ramlFile).unsafeRunSync()

    api.getTypes.asScala.find(_.getName == "A") match {
      case Some(baseType: ObjectType) =>
        val context = ModelGenContext(
          "test",
          baseType,
          ModelGenParams(ramlFile, new File("target"), "base", None, Set.empty, None),
          ApiContext(api)
        )
        context.getSubTypes.toList
          .map(_.getName) should be(List("B", "C"))
      case _ => fail("type for test not found")
    }
  }
}
