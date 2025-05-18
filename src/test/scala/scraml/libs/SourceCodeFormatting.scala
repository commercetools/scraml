package scraml.libs

import org.scalatest.Suite

trait SourceCodeFormatting {
  this: Suite =>

  implicit class StripTrailingSpaces(private val content: String) {
    def stripAllNewlines: String = content.replaceAllLiterally("\n", "")

    def stripTrailingSpaces: String =
      content
        .split('\n')
        .map(_.replaceFirst(" +$", ""))
        .mkString("\n")
  }
}
