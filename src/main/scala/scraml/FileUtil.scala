package scraml

import cats.effect.IO
import cats.implicits._

import java.io.{File, FileOutputStream}
import java.nio.file.{Files, Path, Paths}
import scala.collection.JavaConverters._

object FileUtil {
  def findFiles(parent: File): Iterator[Path] =
    Files
      .walk(Paths.get(parent.getAbsolutePath))
      .iterator()
      .asScala

  def deleteRecursively(path: File): IO[List[Either[Throwable, Boolean]]] =
    if (path.exists()) {
      findFiles(path)
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
