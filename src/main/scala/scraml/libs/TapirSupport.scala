package scraml.libs

import io.vrap.rmf.raml.model.modules.Api
import scraml.LibrarySupport

import scala.jdk.CollectionConverters._
import scala.meta._

final class TapirSupport(endpointsObjectName: String) extends LibrarySupport {
  override def modifyPackageObject(api: Api): Pkg.Object => Pkg.Object = packageObject => {
    val resourceStats: List[Stat] =
      api.getResources.asScala.map { resource =>
        val resourceName = {
          if (resource.getResourcePathName.trim.isEmpty) {
            resource.getRelativeUri.getTemplate
          } else if (resource.getResourcePathName.startsWith("/"))
            resource.getResourcePathName.drop(1)
          else resource.getResourcePathName
        }

        q"""
            val ${Pat.Var(Term.Name(resourceName))} = endpoint.in($resourceName)
         """
      }.toList

    val endpointStats = q"""import sttp.tapir._

                           ..${resourceStats}
                        """.stats

    val endpointsObject = Defn.Object(
      mods = Nil,
      Term.Name(endpointsObjectName),
      Template(
        early = Nil,
        inits = Nil,
        Self(Name(""), None),
        stats = endpointStats,
        derives = Nil
      )
    )

    LibrarySupport.appendPkgObjectStats(packageObject, List(endpointsObject))
  }
}

object TapirSupport {
  def apply(endpointsObjectName: String) = new TapirSupport(endpointsObjectName)
}
