package scraml.libs

import _root_.io.vrap.rmf.raml.model.resources.Resource
import io.vrap.rmf.raml.model.modules.Api
import scraml.{LibrarySupport, ModelGen}

import scala.jdk.CollectionConverters._
import scala.meta._
import scala.util.matching.Regex

final class TapirSupport(endpointsObjectName: String) extends LibrarySupport {
  private def upperCaseFirst(string: String): String =
    string.take(1).toUpperCase ++ string.drop(1)

  private def removeLeadingSlash(uri: String): String =
    uri.replaceFirst("/", "")

  // match "someprefix{variable}" strings as ("prefix", "variable") pairs to identify path params
  private val pathParam: Regex = "(.*)\\{([0-9a-zA-Z- _]+)\\}.*".r

  private def pathParts(uri: String): Iterator[String] = uri
    .split("/")
    .iterator
    .filterNot(_ == "/")

  private def resourceEndpoints(resource: Resource): List[Stat] = {
    val templateWithoutSlash = removeLeadingSlash(resource
      .getFullUri
      .getTemplate)

    // treat dashes as camel case separator (in the name only)
    val resourceNamePathParts = pathParts(templateWithoutSlash.replaceAll("-", "/")).toList

    val resourceName = resourceNamePathParts
      .zipWithIndex
      .map {
        // if the first path part is a variable, it is not part of a resource locator, hence we drop it in the name
        case (pathParam(_, _), index)  if (index == 0 && resourceNamePathParts.length > 1) => ""
        // skip the variable prefix on first level
        case (pathParam(_, name), index) => if (index == 0) upperCaseFirst(name) else "By" + upperCaseFirst(name)
        // camel case literals
        case (literal, _) => upperCaseFirst(literal)
      }.mkString

    resource.getMethods.asScala.flatMap { method =>
      val pathMatcher = {
        import meta._

        val pathMatcherString =
          pathParts(templateWithoutSlash)
            .map {
              // "prefix=value" path parts would be harder to express with tapirs `paths` so we are catching the full part and drop / add the prefix
              case pathParam(prefix, name) => s"""path[String]("$name")""".concat(if(prefix.isEmpty) "" else s""".map(_.drop(${prefix.length}))("$prefix".concat)""")
              case literal => s""""$literal""""
            }.mkString(" / ")

        pathMatcherString.parse[Term].get
      }

      val resourceMethodName = s"${method.getMethodName}$resourceName"
      val paramsTypeName = Type.Name(s"${upperCaseFirst(resourceMethodName)}Params")
      val pathParams = resource.getFullUriParameters.asScala.map { uriParam =>
        Term.Param(
          Nil,
          Term.Name(uriParam.getName),
          Some(
            Type.Name("String")
          ),
          None
        )
      }.toList

      val queryParams: List[Term.Param] = method.getQueryParameters.asScala.flatMap { queryParam =>
        if (Option(queryParam.getPattern).isDefined) None
        else ModelGen.scalaProperty(queryParam)(fallbackType = "String")
      }.toList

      val queryMatcher = queryParams.foldLeft("queryParams") {
        case (acc, queryParam) => acc ++ queryParam.decltpe.map(theType => s""" and query[$theType]("${queryParam.name.value}")""").getOrElse("")
      }.parse[Term].get

      val paramTypeDef: Defn.Class =
        q"""
            final case class $paramsTypeName(..$pathParams, allParams: QueryParams, ..$queryParams)
         """

      val endpointValue: Term.Select =
        q"""
           endpoint
            .in($pathMatcher)
            .in($queryMatcher)
            .mapInTo[$paramsTypeName]
            .${Term.Name(method.getMethodName)}
         """

      val endpointValueDef: Defn.Val =
        q"""
          val ${Pat.Var(Term.Name(resourceMethodName))} = $endpointValue
        """

      List(paramTypeDef, endpointValueDef) ++ resource.getResources.asScala.flatMap(resourceEndpoints).toList
    }.toList
  }

  override def modifyPackageObject(api: Api): Pkg.Object => Pkg.Object = packageObject => {
    val resourceStats: List[Stat] =
      api
        .getResources
        .asScala
        .flatMap(resourceEndpoints)
        .toList

    val endpointStats = q"""import sttp.tapir._
                            import sttp.model._
                            import sttp.tapir.CodecFormat.TextPlain

                            // TODO: why to we need that?
                            implicit val queryOptionalListCodec: Codec[List[String], Option[List[String]], TextPlain] = new Codec[List[String], Option[List[String]], TextPlain] {
                              override def rawDecode(l: List[String]): DecodeResult[Option[List[String]]] = DecodeResult.Value(Some(l))
                              override def encode(h: Option[List[String]]): List[String] = h.getOrElse(List.empty)
                              override lazy val schema: Schema[Option[List[String]]] = Schema.binary
                              override lazy val format: TextPlain = TextPlain()
                            }

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
