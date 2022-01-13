package scraml.libs

import org.scalatest.Suite

trait SourceCodeFormatting {
  this: Suite =>

  implicit class StripTrailingSpaces(private val content: String) {
    def stripTrailingSpaces: String =
      content
        .split('\n')
        .map(_.replaceFirst(" +$", ""))
        .mkString("\n")
  }
}
