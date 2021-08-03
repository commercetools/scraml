package scraml

import cats.effect.IO
import io.vrap.rmf.raml.model.modules.Api

import java.io.File

sealed trait JsonSupport
case object Sphere extends JsonSupport
case object Circe extends JsonSupport

sealed trait CatsSupport
case object EqSupport extends CatsSupport
case object ShowSupport extends CatsSupport

final case class ModelGenParams(raml: File,
                                targetDir: File,
                                basePackage: String,
                                jsonSupport: Option[JsonSupport] = None,
                                catsSupport: Set[CatsSupport] = Set.empty)

final case class GeneratedModel(files: Seq[GeneratedFile]) {
  override def toString: String = {
    files.map(generatedFile => s"${generatedFile.source.name} (${generatedFile.file.getPath})").mkString("\n")
  }
}

trait ModelGen {
  def generate(api: Api, params: ModelGenParams): IO[GeneratedModel]
}

object ModelGenRunner {
  import io.vrap.rmf.raml.model.RamlModelBuilder
  import org.eclipse.emf.common.util.URI

  def readModel(apiPath: File): IO[Api] = for {
    model <- IO {
      new RamlModelBuilder().buildApi(URI.createFileURI(apiPath.getAbsolutePath))
    }

    api <-
      if(model.getValidationResults.isEmpty) {
        IO.pure(model.getRootObject)
      } else IO.raiseError(new IllegalArgumentException(s"error while reading model: ${model.getValidationResults}"))
  } yield api

  def run(generator: ModelGen)(params: ModelGenParams): IO[GeneratedModel] = for {
    api <- readModel(params.raml)
    model <- generator.generate(api, params)
  } yield model
}
