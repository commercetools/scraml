package scraml

import io.vrap.rmf.raml.model.types.{Annotation, AnyType}

object RMFUtil {
  def getAnnotation(from: AnyType)(name: String): Option[Annotation] = Option(from.getAnnotation(name))
}
