package scraml

import cats.effect.IO
import cats.implicits._

import java.io.{File, FileOutputStream}
import java.nio.file.{Files, Paths}
import scala.collection.JavaConverters._

object FileUtil {
  def deleteRecursively(path: File): IO[List[Either[Throwable, Boolean]]] =
    if (path.exists()) {
      Files
        .walk(Paths.get(path.getAbsolutePath))
        .iterator()
        .asScala
        .map(path => IO(path.toFile.delete()).attempt)
        .toList
        .reverse
        .sequence
    } else IO.pure(List.empty)

  def writeToFile(file: File, content: String, append: Boolean = false): IO[File] = IO {
    val out = new FileOutputStream(file, append)
    out.write(content.getBytes("UTF-8"))
    out.flush()
    out.close()
    file
  }
}
