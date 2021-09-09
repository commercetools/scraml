package scraml.libs

import io.vrap.rmf.raml.model.resources.Resource
import io.vrap.rmf.raml.model.modules.Api
import io.vrap.rmf.raml.model.resources.Method
import io.vrap.rmf.raml.model.responses.Body
import scraml.LibrarySupport.appendObjectStats
import scraml.{DefnWithCompanion, JsonSupport, LibrarySupport, MetaUtil, ModelGen, ModelGenContext}

import scala.jdk.CollectionConverters._
import scala.meta._
import scala.util.matching.Regex

final case class ResourceDefinitions(
    baseResourceName: Option[String],
    paramTypeDef: Defn.Class,
    endpointValueDef: Defn.Val
)

final class TapirSupport(endpointsObjectName: String) extends LibrarySupport {
  private def upperCaseFirst(string: String): String =
    string.take(1).toUpperCase.concat(string.drop(1))

  private def removeLeadingSlash(uri: String): String =
    uri.replaceFirst("/", "")

  // match "someprefix{variable}" strings as ("prefix", "variable") pairs to identify path params
  private val pathParam: Regex = "(.*)\\{([0-9a-zA-Z- _]+)\\}.*".r

  private def pathParts(uri: String): Iterator[String] = uri
    .split("/")
    .iterator
    .filterNot(_ == "/")

  private def resourceNameFromTemplate(uriTemplate: String): String = {
    val resourceNamePathParts = pathParts(uriTemplate).toList

    resourceNamePathParts.zipWithIndex.map {
      // if the first path part is a variable, it is not part of a resource locator, hence we drop it in the name
      case (pathParam(_, _), index) if (index == 0 && resourceNamePathParts.length > 1) => ""
      // skip the variable prefix on first level
      case (pathParam(_, name), index) =>
        if (index == 0) upperCaseFirst(name) else "By" + upperCaseFirst(name)
      // camel case literals
      case (literal, _) => upperCaseFirst(literal)
    }.mkString
  }

  private def pathMatcher(template: String): Term = {
    import meta._

    val pathMatcherString =
      pathParts(template)
        .map {
          // "prefix=value" path parts would be harder to express with tapirs `paths` so we are catching the full part and drop / add the prefix
          case pathParam(prefix, name) =>
            s"""path[String]("$name")""".concat(
              if (prefix.isEmpty) "" else s""".map(_.drop(${prefix.length}))("$prefix".concat)"""
            )
          case literal => s""""$literal""""
        }
        .mkString(" / ")

    pathMatcherString.parse[Term].get
  }

  private def queryParamsFromMethod(method: Method): List[Term.Param] =
    method.getQueryParameters.asScala.flatMap { queryParam =>
      if (Option(queryParam.getPattern).isDefined) None
      else ModelGen.scalaProperty(queryParam)(fallbackType = "String")
    }.toList

  private def queryMatcher(queryParams: List[Term.Param]): Term = queryParams
    .foldLeft("queryParams") { case (acc, queryParam) =>
      acc ++ queryParam.decltpe
        .map(theType => s""" and query[$theType]("${queryParam.name.value}")""")
        .getOrElse("")
    }
    .parse[Term]
    .get

  private def pathParamsFromResource(resource: Resource): List[Term.Param] =
    resource.getFullUriParameters.asScala.map { uriParam =>
      Term.Param(
        Nil,
        Term.Name(uriParam.getName),
        Some(
          Type.Name("String")
        ),
        None
      )
    }.toList

  private def baseResourceName(template: String): Option[String] = pathParts(template)
    .find {
      case pathParam(_, _) => false
      case _               => true
    }
    .map(_.split("-").map(upperCaseFirst).mkString)

  private def bodyMappings(
      statusCode: Int,
      bodies: Seq[Body],
      jsonSupport: JsonSupport
  ): List[Term.Apply] =
    if (bodies.isEmpty) {
      List(q"""oneOfMapping(StatusCode($statusCode), emptyOutput)""")
    } else
      bodies.flatMap { body =>
        ModelGen.scalaTypeRef(body.getType, false, None, jsonSupport.jsonType).map { typeRef =>
          val mappingFunction = if (MetaUtil.isTypeApply(typeRef.scalaType, "Either")) {
            "oneOfMappingFromMatchType"
          } else "oneOfMapping"

          q"""${Term.Name(
            mappingFunction
          )}(StatusCode(${statusCode}), jsonBody[${typeRef.scalaType}])"""
        }
      }.toList

  private def resourceEndpointsDefinitions(
      resource: Resource,
      jsonSupport: JsonSupport
  ): List[ResourceDefinitions] = {
    val templateWithoutSlash = removeLeadingSlash(resource.getFullUri.getTemplate)
    // treat dashes as camel case separator (in the name only)
    val fullResourceName = resourceNameFromTemplate(templateWithoutSlash.replaceAll("-", "/"))

    val resourceMethodStats = resource.getMethods.asScala.flatMap { method =>
      val resourceMethodName            = s"${method.getMethodName}$fullResourceName"
      val pathParams                    = pathParamsFromResource(resource)
      val queryParams: List[Term.Param] = queryParamsFromMethod(method)

      val paramsTypeName = Type.Name(s"${upperCaseFirst(resourceMethodName)}Params")
      val paramTypeDef: Defn.Class =
        q"""
            final case class $paramsTypeName(..$pathParams, allParams: QueryParams, ..$queryParams)
         """

      val endpointMethodValue: Term =
        q"""
           endpoint
            .in(${pathMatcher(templateWithoutSlash)})
            .in(${queryMatcher(queryParams)})
            .mapInTo[$paramsTypeName]
            .${Term.Name(method.getMethodName)}
         """

      val (successMappings, errorMappings) =
        method.getResponses.asScala.foldLeft((List.empty[Term.Apply], List.empty[Term.Apply])) {
          // successes
          case ((successes, errors), response) if (response.getStatusCode.toInt / 100) == 2 =>
            (
              successes ++ bodyMappings(
                response.getStatusCode.toInt,
                response.getBodies.asScala,
                jsonSupport
              ),
              errors
            )
          // everything else is an error
          case ((successes, errors), response) =>
            (
              successes,
              errors ++ bodyMappings(
                response.getStatusCode.toInt,
                response.getBodies.asScala,
                jsonSupport
              )
            )
        }

      val endpointWithOut =
        if (successMappings.nonEmpty)
          q"""
          $endpointMethodValue.out(oneOf(..$successMappings))
         """
        else endpointMethodValue

      val endpointWithErrorOut =
        if (errorMappings.nonEmpty)
          q"""
           $endpointWithOut.errorOut(oneOf(..$errorMappings))
         """
        else endpointWithOut

      val endpointValueDef: Defn.Val =
        q"""
          val ${Pat.Var(Term.Name(resourceMethodName))} = $endpointWithErrorOut
        """
      List(
        ResourceDefinitions(baseResourceName(templateWithoutSlash), paramTypeDef, endpointValueDef)
      )
    }.toList

    resourceMethodStats ++ resource.getResources.asScala
      .flatMap(resourceEndpointsDefinitions(_, jsonSupport))
      .toList
  }

  private val anySchema =
    q"""
       private implicit def anySchema[T]: Schema[T] = Schema[T](
        SchemaType.SCoproduct(Nil, None)(_ => None),
        None
      )
     """

  // Either body types have issues with type erasure
  // see https://tapir.softwaremill.com/en/latest/endpoint/statuscodes.html#one-of-mapping-and-type-erasure
  // for those we are going for a `oneOfMappingFromMatchType` which can not de derived for circes Json type and others
  // so we need to provide instances
  private def matchJsonType(typeName: String) =
    q"""
      implicit lazy val matchType: sttp.tapir.typelevel.MatchType[${MetaUtil
      .typeFromName(typeName)}] = {
        case _: ${MetaUtil.typeFromName(typeName)} => true
        case _ => false
      }
     """

  // the query matchers were not happy with Option[List[String]] types
  // TODO: why do we need that in the first place, could be simplified with an already defined codec?
  private val queryListParamEncoder: Defn.Val =
    q"""
        private implicit val queryOptionalListCodec: Codec[List[String], Option[List[String]], TextPlain] = new Codec[List[String], Option[List[String]], TextPlain] {
          override def rawDecode(l: List[String]): DecodeResult[Option[List[String]]] = DecodeResult.Value(Some(l))
          override def encode(h: Option[List[String]]): List[String] = h.getOrElse(List.empty)
          override lazy val schema: Schema[Option[List[String]]] = Schema.binary
          override lazy val format: TextPlain = TextPlain()
        }
     """

  override def modifyClass(classDef: Defn.Class, companion: Option[Defn.Object])(
      context: ModelGenContext
  ): DefnWithCompanion[Defn.Class] =
    DefnWithCompanion(
      classDef,
      companion = companion.map(
        appendObjectStats(
          _,
          List(matchJsonType(classDef.name.value))
        )
      )
    )

  override def modifyPackageObject(libs: List[LibrarySupport], api: Api): Pkg.Object => Pkg.Object =
    packageObject => {
      val circeJsonSupport: JsonSupport = libs
        .collectFirst { case CirceJsonSupport =>
          CirceJsonSupport
        }
        .getOrElse(throw new IllegalStateException("tapir support requires circe support"))

      val resourceDefinitions: List[ResourceDefinitions] =
        api.getResources.asScala
          .flatMap(resourceEndpointsDefinitions(_, circeJsonSupport))
          .toList

      // note: we group endpoint definitions and have them in different sub objects
      // putting everything in the endpoint object can create errors like:
      // "Method too large: de/commercetools/api/package$Endpoints$.<clinit> ()V"
      // when compiling big APIs
      val endpointsGrouped =
        resourceDefinitions.groupBy(_.baseResourceName).toList.sortBy(_._1.getOrElse("")).flatMap {
          case (None, resources) =>
            resources.map(_.paramTypeDef) ++ resources.map(_.endpointValueDef)
          case (Some(baseName), resources) =>
            List(
              Defn.Object(
                mods = Nil,
                Term.Name(baseName),
                Template(
                  early = Nil,
                  inits = Nil,
                  Self(Name(""), None),
                  stats = resources.map(_.paramTypeDef) ++ resources.map(_.endpointValueDef),
                  derives = Nil
                )
              )
            )
        }

      val endpointsObject = Defn.Object(
        mods = Nil,
        Term.Name(endpointsObjectName),
        Template(
          early = Nil,
          inits = Nil,
          Self(Name(""), None),
          stats = endpointsGrouped,
          derives = Nil
        )
      )

      val tapirImports =
        q"""
        import sttp.tapir._
        import sttp.model._
        import sttp.tapir.CodecFormat.TextPlain
        import sttp.tapir.json.circe._

        $anySchema
        ${matchJsonType(circeJsonSupport.jsonType)}
        $queryListParamEncoder
       """.stats

      LibrarySupport.appendPkgObjectStats(packageObject, tapirImports ++ List(endpointsObject))
    }
}

object TapirSupport {
  def apply(endpointsObjectName: String) = new TapirSupport(endpointsObjectName)
}
