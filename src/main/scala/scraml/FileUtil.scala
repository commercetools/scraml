package scraml

import cats.effect.IO
import cats.implicits._

import java.io.File
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
}
