package scraml

import cats.effect.IO
import io.vrap.rmf.raml.model.RamlModelBuilder
import io.vrap.rmf.raml.model.modules.Api
import io.vrap.rmf.raml.model.types.{Annotation, AnyType}
import org.eclipse.emf.common.util.URI

import java.io.File

object RMFUtil {
  def getPackageName(anyType: AnyType): Option[String] =
    getAnnotation(anyType)("package").map(_.getValue.getValue.toString.toLowerCase)

  def getAnnotation(from: AnyType)(name: String): Option[Annotation] = Option(from.getAnnotation(name))

  def readModel(apiPath: File): IO[Api] = for {
    model <- IO {
      new RamlModelBuilder().buildApi(URI.createFileURI(apiPath.getAbsolutePath))
    }

    api <-
      if(model.getValidationResults.isEmpty) {
        IO.pure(model.getRootObject)
      } else IO.raiseError(new IllegalArgumentException(s"error while reading model: ${model.getValidationResults}"))
  } yield api
}
