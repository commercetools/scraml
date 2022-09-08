package scraml.libs

import io.vrap.rmf.raml.model.modules.Api
import io.vrap.rmf.raml.model.resources.Resource
import io.vrap.rmf.raml.model.responses.Body
import io.vrap.rmf.raml.model.types.{StringType, TypedElement}
import scraml.{DefnWithCompanion, JsonSupport, LibrarySupport, ModelGen, ModelGenContext}
import scala.collection.immutable.TreeSet
import scala.jdk.CollectionConverters._
import scala.meta._
import scala.util.matching.Regex

final case class ResourceDefinitions(
    baseResourceName: Option[String],
    paramTypeDef: Option[Defn.Class],
    endpointValueDef: Defn.Val
)

final class TapirSupport(endpointsObjectName: String) extends LibrarySupport {
  import scraml.LibrarySupport._

  private val jsonContentType = "application/json"

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

  private def paramTypes(elements: Seq[TypedElement])(implicit
      context: ModelGenContext
  ): List[Term.Param] =
    elements.flatMap { queryParam =>
      if (Option(queryParam.getPattern).isDefined) None
      else ModelGen.scalaProperty(queryParam)(fallbackType = "String")
    }.toList

  private def paramMatcher(params: List[Term.Param])(function: String): Option[Term] =
    params
      .flatMap(param => param.decltpe.map(theType => q"""
        ${Term.Name(function)}[$theType](${param.name.value})
       """))
      .reduceOption[Term] { case (left, right) =>
        q""" $left and $right """
      }

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

  case class BodyWithMediaType(mediaType: String, bodyType: Type)

  implicit val bodyOrder: Ordering[BodyWithMediaType] = new Ordering[BodyWithMediaType] {
    override def compare(x: BodyWithMediaType, y: BodyWithMediaType): Int =
      s"${x.mediaType}${x.bodyType.toString}".compare(s"${y.mediaType}${y.bodyType.toString()}")
  }

  private def bodyTypes(
      bodies: Iterable[Body],
      jsonSupport: JsonSupport,
      optional: Boolean
  )(implicit context: ModelGenContext): List[BodyWithMediaType] =
    bodies.flatMap { body =>
      context
        .scalaTypeRef(body.getType, optional, None, jsonSupport.jsonType)
        .map(_.scalaType)
        .map(BodyWithMediaType(body.getContentType, _))
    }.toList

  private[libs] def resourceEndpointsDefinitions(
      resource: Resource,
      jsonSupport: JsonSupport
  )(implicit context: ModelGenContext): List[ResourceDefinitions] = {
    val templateWithoutSlash = removeLeadingSlash(resource.getFullUri.getTemplate)
    // treat dashes as camel case separator (in the name only)
    val fullResourceName = resourceNameFromTemplate(templateWithoutSlash.replaceAll("-", "/"))

    val resourceMethodStats = resource.getMethods.asScala.flatMap { method =>
      val resourceMethodName             = s"${method.getMethodName}$fullResourceName"
      val pathParams: List[Term.Param]   = pathParamsFromResource(resource)
      val queryParams: List[Term.Param]  = paramTypes(method.getQueryParameters.asScala)
      val headerParams: List[Term.Param] = paramTypes(method.getHeaders.asScala)
      val hasParams = pathParams.nonEmpty || queryParams.nonEmpty || headerParams.nonEmpty

      val endpointWithMethod =
        q"""
          endpoint.${Term.Name(method.getMethodName)}
        """

      val endpointWithHeaderMatcher: Term = paramMatcher(headerParams)("header").map(matcher => q"""
           $endpointWithMethod
             .in($matcher)
         """) getOrElse (endpointWithMethod)

      val endpointWithPathMatcher: Term =
        q"""
           $endpointWithHeaderMatcher
             .in(${pathMatcher(templateWithoutSlash)})
         """

      val endpointWithQueryMatcher: Term = paramMatcher(queryParams)("query")
        .map(matcher => q"""
           $endpointWithPathMatcher.in($matcher)
         """)
        .getOrElse(endpointWithPathMatcher)

      val paramsTypeName = Type.Name(s"${upperCaseFirst(resourceMethodName)}Params")

      val paramTypeDef: Option[Defn.Class] =
        if (hasParams) Some(q"""
            final case class $paramsTypeName(..$headerParams, ..$pathParams, ..$queryParams)
         """) else None

      val endpointWithInputMappedAndMethod: Term =
        if (hasParams) {
          q"""
           $endpointWithQueryMatcher
            .mapInTo[$paramsTypeName]
         """
        } else endpointWithQueryMatcher

      val endpointWithInputBody: Term = method.getBodies.asScala.headOption
        .flatMap(body =>
          context
            .scalaTypeRef(body.getType, optional = false, None, jsonSupport.jsonType)
            .map((_, body))
        )
        .map { case (typeRef, body) =>
          val inputBody: Term = body.getContentType.toLowerCase match {
            case json if json.contains(jsonContentType) || json.contains("application/graphql") =>
              q"""
                 jsonBody[${typeRef.scalaType}]
               """
            case form if form.contains("application/x-www-form-urlencoded") =>
              q"""
                 formBody[Map[String, String]]
               """
            case other =>
              throw new IllegalArgumentException(s"unsupported input media type: $other")
          }

          q"""
             $endpointWithInputMappedAndMethod.in($inputBody)
           """
        }
        .getOrElse(endpointWithInputMappedAndMethod)

      val (successTypes, errorTypes) =
        method.getResponses.asScala.foldLeft(
          (List.empty[BodyWithMediaType], List.empty[BodyWithMediaType])
        ) {
          // successes
          case ((successes, errors), response) if (response.getStatusCode.toInt / 100) == 2 =>
            (
              successes ++ bodyTypes(
                response.getBodies.asScala,
                jsonSupport,
                optional = false
              ),
              errors
            )
          // everything else is an error
          case ((successes, errors), response) =>
            (
              successes,
              errors ++ bodyTypes(
                response.getBodies.asScala,
                jsonSupport,
                optional = true // allow empty bodies on error
              )
            )
        }

      require(
        successTypes.forall(
          _.mediaType.toLowerCase.contains(jsonContentType) ||
            errorTypes.forall(_.mediaType.toLowerCase.contains(jsonContentType))
        ),
        s"only json return types supported right now, buf found $successTypes, $errorTypes"
      )

      def unionType(types: Iterable[Type]): Option[Type] = types.reduceOption[Type] { case (a, b) =>
        Type.ApplyInfix(a, Type.Name("|"), b)
      }

      val endpointWithOut =
        unionType(TreeSet(successTypes: _*).map(_.bodyType))
          .map(theType => q"""
        $endpointWithInputBody.out(jsonBody[$theType])
         """)
          .getOrElse(endpointWithInputBody)

      val endpointWithErrorOut =
        unionType(TreeSet(errorTypes: _*).map(_.bodyType))
          .map(theType => q"""
        $endpointWithOut.errorOut(jsonBody[$theType])
         """)
          .getOrElse(endpointWithOut)

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

  private def tapirDefinitions(context: ModelGenContext) = new TapirPackageDefinitions(context)

  override def modifyPackageObject(libs: List[LibrarySupport], api: Api)(implicit
      context: ModelGenContext
  ): Pkg.Object => Pkg.Object =
    packageObject => {
      val circeJsonSupport: JsonSupport = libs
        .collectFirst { case instance: CirceJsonSupport =>
          instance
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
      val endpointsGrouped: List[Stat] =
        resourceDefinitions.groupBy(_.baseResourceName).toList.sortBy(_._1.getOrElse("")).flatMap {
          case (None, resources) =>
            resources.flatMap(_.paramTypeDef) ++ resources.map(_.endpointValueDef)
          case (Some(baseName), resources) =>
            List(
              Defn.Object(
                mods = Nil,
                Term.Name(baseName),
                Template(
                  early = Nil,
                  inits = Nil,
                  Self(Name(""), None),
                  stats = resources.flatMap(_.paramTypeDef) ++ resources.map(_.endpointValueDef),
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
        import sttp.tapir.generic.auto._

        type |[+A1, +A2] = Either[A1, A2]

        ..${tapirDefinitions(context)()}

       """.stats

      LibrarySupport.appendPkgObjectStats(packageObject, tapirImports ++ List(endpointsObject))
    }

  override def modifyEnum(enumType: StringType)(
      enumTrait: Defn.Trait,
      companion: Option[Defn.Object]
  ): DefnWithCompanion[Defn.Trait] = {
    val enumValuesAsList =
      q"""
         List(
         ..${enumType.getEnum.asScala.toList.map { enum =>
        Lit.String(enum.getValue.toString)
      }})
       """

    val enumTypeName = Type.Name(enumType.getName)
    val tapirEnumCodec: List[Stat] =
      q"""
      implicit lazy val tapirCodec: sttp.tapir.Codec.PlainCodec[$enumTypeName] =
      sttp.tapir.Codec.string.mapDecode[$enumTypeName](
        ${Term.PartialFunction(
        enumType.getEnum.asScala.toList.map { enum =>
          Case(
            Lit.String(enum.getValue.toString),
            None,
            q"sttp.tapir.DecodeResult.Value(${Term.Name(enum.getValue.toString)})"
          )
        }
          ++ List[Case](
            Case(
              Pat.Var(Term.Name("other")),
              None,
              q"""
                  sttp.tapir.DecodeResult.InvalidValue(
                    sttp.tapir.ValidationError[String](
                      sttp.tapir.Validator.enumeration(
                        $enumValuesAsList
                      ),
                      other
                    ) :: Nil
                  )
                 """
            )
          )
      )}
      )(
        ${Term.PartialFunction(
        enumType.getEnum.asScala.toList.map { enum =>
          Case(
            Term.Name(enum.getValue.toString),
            None,
            Lit.String(enum.getValue.toString)
          )
        }
      )}
      )
       """ :: Nil

    super.modifyEnum(enumType)(enumTrait, companion.map(appendObjectStats(_, tapirEnumCodec)))
  }
}

object TapirSupport {
  def apply(endpointsObjectName: String) = new TapirSupport(endpointsObjectName)
}
