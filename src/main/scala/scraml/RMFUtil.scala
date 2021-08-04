package scraml

import io.vrap.rmf.raml.model.types.{Annotation, AnyType, ObjectType}
import scala.jdk.CollectionConverters._

object RMFUtil {
  def getAnnotation(from: AnyType)(name: String): Option[Annotation] = Option(from.getAnnotation(name))
  def getSubTypes(objectType: ObjectType): Iterator[AnyType] =
    objectType.getSubTypes.asScala.filter(_.getName != objectType.getName).iterator
}
