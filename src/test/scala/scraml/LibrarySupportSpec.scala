package scraml

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.File

final case class TestLib(override val order: Double) extends LibrarySupport

class LibrarySupportSpec extends AnyFlatSpec with Matchers {
  "LibrarySupportSpec" should "establish ordering on libs" in {
    ModelGenParams(
      new File("doesnotexist.raml"),
      new File("target"),
      "scraml",
      DefaultTypes(),
      Set(
        TestLib(0.1),
        TestLib(0.3),
        TestLib(0.25)
      ),
      None
    ).allLibraries should be(
      List(
        TestLib(0.1),
        TestLib(0.25),
        TestLib(0.3)
      )
    )
  }

  "json support" should "came first per default" in {
    val jsonSupportWithDefaultOrder = new LibrarySupport with JsonSupport {
      override def jsonType: String = "Any"
    }

    val someLibrary = new LibrarySupport {}

    (jsonSupportWithDefaultOrder.order < someLibrary.order) should be(true)
  }
}
