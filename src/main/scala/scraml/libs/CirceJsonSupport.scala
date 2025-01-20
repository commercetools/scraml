package scraml.libs

import io.vrap.rmf.raml.model.modules.Api
import io.vrap.rmf.raml.model.types.*
import scraml.LibrarySupport.*
import scraml.MetaUtil.*
import scraml.RMFUtil.{getAnnotation, isEnumType}
import scraml.*

import scala.util.Try

object CirceJsonSupport {
  def apply(formats: Map[String, String] = Map.empty, imports: Seq[String] = Seq.empty) =
    new CirceJsonSupport(formats, imports)
}

class CirceJsonSupport(formats: Map[String, String], imports: Seq[String])
    extends LibrarySupport
    with JsonSupport {
  import FieldMatchPolicy.IgnoreExtra

  import scala.collection.JavaConverters.*
  import scala.meta.*

  object HasAnyDefaults {
    def unapply(context: ModelGenContext): Boolean =
      Option(context.objectType.getAllProperties)
        .map(_.asScala.toList)
        .getOrElse(Nil)
        .exists(_.getType.getDefault ne null)
  }

  object HasAnyOverrides {
    def unapply(context: ModelGenContext): Option[ModelGenContext] =
      Option(context).filter {
        _.typeProperties
          .exists { property =>
            PropertyOptionality(context.objectType, Name(property.getName)).overridden
          }
      }
  }

  object HasRefinements extends HasFacets {
    def apply(context: ModelGenContext, classDef: Defn.Class): Boolean =
      context.isLibraryEnabled[RefinedSupport.type]() &&
        classDef.ctor.paramClauses.flatten
          .map(p => propertyNameFrom(p.name.value))
          .flatMap(name => RMFUtil.findAllDeclarations(context.objectType, name).map(_._2))
          .exists { prop =>
            Option(prop.getType()).exists(hasAnyFacets)
          }
  }

  override def jsonType: String = "io.circe.Json"

  private val decoderChunkThreshold = 50

  private val eitherCodec: List[Stat] =
    q"""
      import io.circe.Decoder.Result
      import io.circe.{HCursor, Json, Decoder, Encoder}

      implicit def eitherEncoder[A, B](implicit aEncoder: Encoder[A], bEncoder: Encoder[B]): Encoder[Either[A, B]] = new Encoder[Either[A, B]] {
        override def apply(a: Either[A, B]): Json = a match {
          case Right(b) =>
            bEncoder(b)
          case Left(a) =>
            aEncoder(a)
        }
      }

      implicit def eitherDecoder[A, B](implicit aDecoder: Decoder[A], bDecoder: Decoder[B]): Decoder[Either[A, B]] = new Decoder[Either[A, B]] {
        override def apply(c: HCursor): Result[Either[A, B]] =
          aDecoder.either(bDecoder)(c)
      }
     """.stats

  private def shouldDeriveJson(objectType: ObjectType): Boolean =
    getAnnotation(objectType)("scala-derive-json").forall(_.getValue.getValue.toString.toBoolean)

  private def discriminator(aType: ObjectType): Option[String] =
    RMFUtil.discriminators(aType) match {
      case Nil           => None
      case single :: Nil => Some(single)
      case _ =>
        throw new IllegalArgumentException(
          "only single discriminator should be defined in type hierarchy"
        )
    }

  private def discriminatorValue(aType: ObjectType): Option[String] =
    Option(aType.getDiscriminatorValue)

  private def discriminatorAndValue(aType: ObjectType): Option[(String, String)] =
    discriminator(aType).zip(discriminatorValue(aType)).headOption

  private def typeEncoder(implicit context: ModelGenContext): Defn.Val = {
    val subTypes = context.leafTypes.toList
    val typeName = context.objectType.getName

    Defn.Val(
      List(Mod.Implicit(), Mod.Lazy()),
      List(Pat.Var(Term.Name("encoder"))),
      Some(Type.Apply(Type.Name("Encoder"), List(Type.Name(typeName)))),
      Term.NewAnonymous(
        Template(
          Nil,
          List(
            Init(Type.Apply(Type.Name("Encoder"), List(Type.Name(typeName))), Name(""), Nil)
          ),
          Self(Name(""), None),
          List(
            Defn.Def(
              List(Mod.Override()),
              Term.Name("apply"),
              Nil,
              List(
                List(
                  Term.Param(
                    Nil,
                    Term.Name(typeName.toLowerCase),
                    Some(Type.Name(typeName)),
                    None
                  )
                )
              ),
              Some(Type.Name("Json")),
              Term.Match(
                Term.Name(typeName.toLowerCase),
                cases = subTypes.map { case subType: ObjectType =>
                  Case(
                    Pat.Typed(
                      Pat.Var(Term.Name("x")),
                      if (ModelGen.isSingleton(subType, context.anyTypeName))
                        Type.Singleton(Term.Name(subType.getName))
                      else Type.Name(subType.getName)
                    ),
                    None,
                    Term.Apply(
                      packageTerm(s"${subType.getName}.encoder"),
                      List(Term.Name("x"))
                    )
                  )
                },
                Nil
              )
            )
          ),
          Nil
        )
      )
    )
  }

  private def getDiscriminatorMapType(objectType: ObjectType): Option[Map[String, List[String]]] = {
    getAnnotation(objectType)("discriminator-map").map(_.getValue) match {
      case Some(discriminatorMap: ObjectInstance) =>
        Some(
          discriminatorMap.getValue.asScala
            .groupBy(_.getName)
            .map { case (key, values) =>
              key -> values.map(v => v.getValue.getValue.toString).toList
            }
        )
      case _ => None
    }
  }

  private def typeDecoder(context: ModelGenContext): Defn.Val = {
    val subTypes         = context.leafTypes.toList
    val typeName         = context.objectType.getName
    val discriminatorOpt = discriminator(context.objectType)
    val discriminatorMap = getDiscriminatorMapType(context.objectType).getOrElse(Map.empty)
    validateDiscriminatorsMutualExclusion(discriminatorOpt, discriminatorMap, typeName)
    if (discriminatorMap.nonEmpty) validateDiscriminatorMap(discriminatorMap, subTypes, typeName)
    val decode: Term = if (discriminatorOpt.isEmpty) {
      if (discriminatorMap.nonEmpty)
        extractDecodeForDiscriminatorMap(context, subTypes, typeName, discriminatorMap)
      else
        extractDecodeWithoutDiscriminator(context, subTypes, typeName)
    } else
      Term.Match(
        Term.ApplyType(
          Term.Select(
            Term.Apply(
              Term.Select(Term.Name("c"), Term.Name("downField")),
              discriminatorOpt.map(Lit.String(_)).toList
            ),
            Term.Name("as")
          ),
          List(Type.Name("String"))
        ),
        subTypes.flatMap { case subType: ObjectType =>
          discriminatorValue(subType).map { _ =>
            Case(
              Pat
                .Extract(
                  Term.Name("Right"),
                  List(Term.Select(Term.Name(subType.getName), Term.Name("jsonTypeHint")))
                ),
              None,
              Term.Apply(
                Term.Select(Term.Name(subType.getName), Term.Name("decoder")),
                List(Term.Name("c"))
              )
            )
          }
        } ++ List(
          Case(
            Pat.Var(Term.Name("other")),
            None,
            Term.Apply(
              Term.Name("Left"),
              List(
                Term.Apply(
                  Term.Name("DecodingFailure"),
                  List(
                    Term.Interpolate(
                      Term.Name("s"),
                      List(Lit.String("unknown discriminator: "), Lit.String("")),
                      List(Term.Name("other"))
                    ),
                    Term.Select(Term.Name("c"), Term.Name("history"))
                  )
                )
              )
            )
          )
        ),
        Nil
      )

    Defn.Val(
      List(Mod.Implicit(), Mod.Lazy()),
      List(Pat.Var(Term.Name("decoder"))),
      Some(Type.Apply(Type.Name("Decoder"), List(Type.Name(typeName)))),
      Term.NewAnonymous(
        Template(
          Nil,
          List(
            Init(Type.Apply(Type.Name("Decoder"), List(Type.Name(typeName))), Name(""), Nil)
          ),
          Self(Name(""), None),
          List(
            Defn.Def(
              List(Mod.Override()),
              Term.Name("apply"),
              Nil,
              List(List(Term.Param(Nil, Term.Name("c"), Some(Type.Name("HCursor")), None))),
              Some(Type.Apply(Type.Name("Result"), List(Type.Name(typeName)))),
              decode
            )
          ),
          Nil
        )
      )
    )
  }

  private def validateDiscriminatorsMutualExclusion(
      discriminatorOpt: Option[String],
      discriminatorMap: Map[String, List[String]],
      typeName: String
  ): Unit = {
    if (discriminatorOpt.nonEmpty && discriminatorMap.nonEmpty) {
      throw new IllegalArgumentException(
        s"Either discriminator type or key-base-discriminator type should be defined in type $typeName, cannot define both."
      )
    }
  }

  private def validateDiscriminatorMap(
      discriminatorMap: Map[String, List[String]],
      subTypes: Seq[AnyType],
      typeName: String
  ): Unit =
    if (
      subTypes.isEmpty || Some(discriminatorMap.values.toList.flatten).exists { toBeDerivedValues =>
        val toBeDerivedTypes = subTypes.map(_.getName).toList
        toBeDerivedTypes.length != toBeDerivedValues.length || toBeDerivedTypes
          .diff(toBeDerivedValues)
          .nonEmpty
      }
    ) {
      throw new IllegalArgumentException(
        s"Discriminator map derivation cannot be executed. " +
          s"Type $typeName does not have subtypes or mapping of types does not cover all cases."
      )
    }

  private def extractDecodeWithoutDiscriminator(
      context: ModelGenContext,
      subTypes: List[AnyType],
      typeName: String
  ) = {
    // Sort by number of properties so that the first type tried has the most
    // properties, the next has the same or less, etc.  Since there is no
    // discriminator, this ensures types which have a subset of the fields
    // other types have are tried later.
    val sortedByProperties = subTypes
      .collect { case obj: ObjectType =>
        obj
      }
      .sortBy(RMFUtil.typePropertiesWithoutDiscriminator(_).size)
      .reverse

    fromObjectTypes(context, typeName, sortedByProperties)
  }

  private def fromObjectTypes(
      context: ModelGenContext,
      typeName: String,
      sortedByProperties: List[ObjectType]
  ) = {
    sortedByProperties match {
      case Nil => noTypeDecodingFailure(typeName)
      case single :: Nil =>
        q"""
            ${packageTerm(s"${single.getName}.decoder")}.tryDecode(c)
            """
      case head :: tail if tail.length < decoderChunkThreshold =>
        val initRead: String =
          q"""
            ${packageTerm(s"${head.getName}.decoder")}.tryDecode(c)
            """.toString
        val decoderCalls = tail.foldLeft(initRead) { case (acc, next) =>
          val nextRead =
            q"""fold(_ => ${packageTerm(
              s"${next.getName}.decoder"
            )}.tryDecode(c), Right(_))""".toString
          s"$acc.$nextRead"
        }
        decoderCalls.parse[Term].get
      // For situations where there are a large number of leaf types, a
      // "chunked" type decoder is generated so that we do not have a
      // stack overflow in `scala.meta`.
      case large =>
        chunkedTypeDecoder(context, large)
    }
  }

  private def noTypeDecodingFailure(typeName: String) = {
    q"""
           Left(DecodingFailure("No concrete types exist for: " + $typeName))
           """
  }

  private def extractDecodeForDiscriminatorMap(
      context: ModelGenContext,
      subTypes: List[AnyType],
      typeName: String,
      discriminatorMap: Map[String, List[String]]
  ): Term = {
    def header(withBody: Term) = {
      q"""
         c.value.asObject.toRight(DecodingFailure("Expected object", c.history)).flatMap { obj =>
           $withBody
         }"""
    }

    val sortedByProperties = subTypes
      .collect { case obj: ObjectType =>
        obj
      }
      .sortBy(RMFUtil.typePropertiesWithoutDiscriminator(_).size)
      .reverse

    header(withBody =
      Term.Match(
        Term.Name("obj"),
        discriminatorMap.map { case (key, values) =>
          Case(
            pat = Pat.Wildcard(),
            cond = Some(
              Term.Apply(
                Term.Select(Term.Name("obj"), Term.Name("contains")),
                List(Lit.String(key))
              )
            ),
            body = {
              val keys = sortedByProperties.filter(objType => values.contains(objType.getName))
              fromObjectTypes(context, typeName, keys)
            }
          )
        }.toList ++ List(
          Case(
            pat = Pat.Var(Term.Name("other")),
            cond = Option.empty[Term],
            body = Term.Apply(
              Term.Name("Left"),
              List(
                Term.Apply(
                  Term.Name("DecodingFailure"),
                  List(
                    Term.Interpolate(
                      Term.Name("s"),
                      List(Lit.String("unknown discriminator: "), Lit.String("")),
                      List(
                        Term.Apply(
                          fun = Term.Select(
                            Term.Select(
                              Term.Select(Term.Name("other"), Term.Name("keys")),
                              Term.Name("headOption")
                            ),
                            Term.Name("getOrElse")
                          ),
                          args = List(Lit.String("unknown_value"))
                        )
                      )
                    ),
                    Term.Select(Term.Name("c"), Term.Name("history"))
                  )
                )
              )
            )
          )
        ),
        Nil
      )
    )
  }

  private def chunkedTypeDecoder(context: ModelGenContext, leaves: List[ObjectType]): Term.Block = {
    val localDefs: List[Defn.Def] = leaves
      .grouped(decoderChunkThreshold)
      .zipWithIndex
      .map {
        case (last :: Nil, index) =>
          q"""
          @scala.inline
          def ${Term.Name("chunk" + index)}() = ${Term.Name(last.getName)}.decoder.tryDecode(c)
          """
        case (head :: tail, index) =>
          q"""
          @scala.inline
          def ${Term.Name("chunk" + index)}() = ${tail.foldLeft(
            q"""${Term.Name(head.getName)}.decoder.tryDecode(c)"""
          ) { case (accum, decoder) =>
            q"""
                $accum.fold( _ => ${Term.Name(decoder.getName)}.decoder.tryDecode(c), Right(_) )
                """
          }}
          """
        case (Nil, index) =>
          q"""
          @scala.inline
          def ${Term.Name("chunk" + index)}() =
            Left(DecodingFailure("unknown payload: " + ${context.objectType.getName}))
          """
      }
      .toList

    Term.Block(
      localDefs :+ localDefs.map(d => q"${d.name}()").reduce { (a, b) =>
        q"$a.fold(_ => $b, Right(_))"
      }
    )
  }

  private def deriveJsonTypeSwitch(context: ModelGenContext): List[Stat] =
    if (shouldDeriveJson(context.objectType)) {
      val subTypes: List[AnyType] = context.getDirectSubTypes.toList

      subTypes match {
        case Nil => List.empty
        case _ =>
          val jsonStats =
            q"""
          import io.circe.Decoder.Result
          import io.circe._

          ${typeDecoder(context)}

          ${typeEncoder(context)}
        """.stats
          jsonStats
      }
    } else List.empty // should not derive

  private def deriveJson(classDef: Defn.Class)(implicit
      context: ModelGenContext
  ): List[Stat] = {
    import context.objectType

    if (shouldDeriveJson(objectType)) {
      val discriminatorTuple = discriminator(objectType).map(name =>
        q"""${Lit.String(name)} -> Json.fromString(jsonTypeHint)"""
      )

      lazy val pairs = generatePropertiesCode[Term](classDef) {
        case NamedProperty(param, _, declaredName) =>
          List(
            q"""
          ${Lit.String(declaredName)} ->
            instance.${Term.Name(param.name.value)}.asJson
          """
          )
      }

      val encoderDef: Defn.Val = context match {
        case FieldMatchPolicy(policy) if policy.areAdditionalPropertiesEnabled(objectType) =>
          val additionalPropName = policy
            .additionalProperties(objectType)
            .map(ap => Term.Name(ap.propertyName))

          q"""
            implicit lazy val encoder: Encoder[${Type.Name(objectType.getName)}] =
              new Encoder[${Type.Name(objectType.getName)}] {
                final def apply(instance: ${Type.Name(objectType.getName)}): Json =
                  AdditionalProperties.merge(
                    Json.obj(
                      ..${discriminatorTuple.toList ::: pairs}
                    ),
                    instance.${additionalPropName.get}
                  )
              }
           """

        case FieldMatchPolicy(IgnoreExtra(_)) | HasAnyOverrides(_) =>
          discriminatorTuple match {
            case Some(tuple) =>
              q"""
            implicit lazy val encoder: Encoder[${Type.Name(objectType.getName)}] =
              new Encoder[${Type.Name(objectType.getName)}] {
                final def apply(instance: ${Type.Name(objectType.getName)}): Json =
                    Json.obj($tuple, ..$pairs)
              }
          """

            case None =>
              q"""
            implicit lazy val encoder: Encoder[${Type.Name(objectType.getName)}] =
              new Encoder[${Type.Name(objectType.getName)}] {
                final def apply(instance: ${Type.Name(objectType.getName)}): Json =
                    Json.obj(..$pairs)
              }
          """
          }

        case _ =>
          discriminatorTuple match {
            case Some(tuple) =>
              q"""
               implicit lazy val encoder: Encoder[${Type.Name(objectType.getName)}] =
                 deriveEncoder[${Type.Name(objectType.getName)}].mapJsonObject(
                   _.+:($tuple)
               )
              """

            case None =>
              q"""
               implicit lazy val encoder: Encoder[${Type.Name(objectType.getName)}] =
                 deriveEncoder[${Type.Name(objectType.getName)}]
              """
          }
      }

      lazy val packageObjectRef = packageTerm(context.params.basePackage)

      val importStats: List[Stat] = q"""import io.circe._
          import io.circe.generic.semiauto._
          import io.circe.syntax._
          """.stats ++ formats.headOption.map(_ => q"import $packageObjectRef.Formats._") ++ imports
        .map(pkg => s"import $pkg".parse[Stat].get)

      val jsonTypeHint = discriminatorValue(objectType).map(discriminatorValue => {
        q"""
           val jsonTypeHint = $discriminatorValue
         """
      })

      importStats ++
        q"""
          ..${jsonTypeHint}
          ..${deriveJsonClassDecoder(context, classDef)}
          $encoderDef
         """.stats
    } else List.empty
  }

  private def deriveJsonClassDecoder(context: ModelGenContext, classDef: Defn.Class): List[Stat] = {
    import context.objectType

    val objectTypeName = Type.Name(objectType.getName)

    def defaultValueFor(param: Term.Param): Option[Term] = {
      val property     = Option(objectType.getProperty(param.name.value))
      val propertyType = property.map(_.getType)
      val isRequired   = property.fold(true)(_.getRequired)

      propertyType
        .flatMap(pt => Option(pt.getDefault))
        .flatMap { instance =>
          propertyType match {
            case Some(stringEnum: StringType) if isEnumType(stringEnum) =>
              val enumType     = Term.Name(stringEnum.getName)
              val enumInstance = Term.Name(stringEnum.getDefault.getValue.toString.toUpperCase)

              if (isRequired)
                Option(q"$enumType.$enumInstance")
              else
                Option(q"""Some($enumType.$enumInstance)""")

            case Some(_: StringType) if isRequired =>
              val raw = instance.getValue.toString

              Option(Lit.String(raw))

            case Some(_: StringType) =>
              val raw = instance.getValue.toString

              Option(q"""Some(${Lit.String(raw)})""")

            case Some(int: IntegerType)
                if isRequired &&
                  (NumberFormat.INT64 == int.getFormat || NumberFormat.LONG == int.getFormat) =>
              Try(instance.getValue.toString.toLong).map(Lit.Long(_)).toOption

            case Some(int: IntegerType)
                if NumberFormat.INT64 == int.getFormat || NumberFormat.LONG == int.getFormat =>
              Try(instance.getValue.toString.toLong).toOption.map { value =>
                q"""Some(${Lit.Long(value)})"""
              }

            case Some(number: NumberType)
                if isRequired &&
                  (NumberFormat.INT64 == number.getFormat || NumberFormat.LONG == number.getFormat) =>
              Try(instance.getValue.toString.toLong).map(Lit.Long(_)).toOption

            case Some(number: NumberType)
                if NumberFormat.INT64 == number.getFormat || NumberFormat.LONG == number.getFormat =>
              Try(instance.getValue.toString.toLong).toOption.map { value =>
                q"""Some(${Lit.Long(value)})"""
              }

            case Some(double: NumberType)
                if isRequired && NumberFormat.DOUBLE == double.getFormat =>
              Try(instance.getValue.toString.toLong).map(Lit.Double(_)).toOption

            case Some(double: NumberType) if NumberFormat.DOUBLE == double.getFormat =>
              Try(instance.getValue.toString.toLong).toOption.map { value =>
                q"""Some(${Lit.Double(value)})"""
              }

            case Some(_) if isRequired =>
              instance.getValue.toString.parse[Term].toOption

            case Some(_) =>
              instance.getValue.toString.parse[Term].toOption.map { term =>
                q"""Some($term)"""
              }

            case None =>
              None
          }
        }
    }

    def genFlatMaps(args: List[Term.Param])(
        genLastProperty: (Lit.String, Term.Name, Type, Term.Name, Option[Term]) => Term.Apply
    ): Term.Apply =
      args match {
        case last :: Nil =>
          val fieldName     = Lit.String(propertyNameFrom(last.name))
          val paramName     = Term.Name("_" + last.name.value)
          val companionName = Term.Name(classDef.name.value)

          genLastProperty(
            fieldName,
            paramName,
            last.decltpe.get,
            companionName,
            defaultValueFor(last)
          )

        case head :: tail =>
          val fieldName = Lit.String(propertyNameFrom(head.name))
          val paramName = Term.Name("_" + head.name.value)

          defaultValueFor(head).fold(
            q"""
              c.downField($fieldName).as[${head.decltpe.get}].flatMap {
                $paramName: ${head.decltpe.get} => ${genFlatMaps(tail)(genLastProperty)}
              }
           """
          ) { value =>
            q"""
              c.getOrElse[${head.decltpe.get}]($fieldName)($value).flatMap {
                $paramName: ${head.decltpe.get} => ${genFlatMaps(tail)(genLastProperty)}
              }
           """
          }
      }

    (context, context.params.fieldMatchPolicy.additionalProperties(objectType)(context)) match {
      case (FieldMatchPolicy(_), Some(additional)) if HasRefinements(context, classDef) =>
        q"""
        import io.circe.refined._

        implicit lazy val decoder: Decoder[$objectTypeName] =
          new Decoder[$objectTypeName] {
            def apply(c: HCursor): Decoder.Result[$objectTypeName] = {
              ${genFlatMaps(classDef.ctor.paramss.flatten) { case (_, _, _, companionName, _) =>
          val additionalParamName = Term.Name("_" + additional.propertyName)

          q"""
                AdditionalProperties.decoder(c).flatMap {
                  $additionalParamName: Option[${additional.propertyType}] =>
                    $companionName.from( ..${generatePropertiesCode(classDef) { prop =>
            Term.Name("_" + prop.name.value) :: Nil
          }.collect { case t: Term =>
            t
          }},
            $additionalParamName
            ).swap.map(e => DecodingFailure(e.getMessage, Nil)).swap
            }
           """
        }}
        }
        }
       """.stats

      case (FieldMatchPolicy(_), None) if HasRefinements(context, classDef) =>
        q"""
        import io.circe.refined._

        implicit lazy val decoder: Decoder[$objectTypeName] =
          new Decoder[$objectTypeName] {
            def apply(c: HCursor): Decoder.Result[$objectTypeName] = {
              ${genFlatMaps(classDef.ctor.paramss.flatten) {
          case (fieldName, paramName, paramType, companionName, Some(default)) =>
            q"""
              c.getOrElse[$paramType]($fieldName)($default).flatMap {
                $paramName: $paramType =>
                  $companionName.from(..${generatePropertiesCode(classDef) { prop =>
              Term.Name("_" + prop.name.value) :: Nil
            }.collect { case t: Term =>
              t
            }}).swap.map(e => DecodingFailure(e.getMessage, Nil)).swap
            }
           """

          case (fieldName, paramName, paramType, companionName, None) =>
            q"""
              c.downField($fieldName).as[$paramType].flatMap {
                $paramName: $paramType =>
                  $companionName.from(..${generatePropertiesCode(classDef) { prop =>
              Term.Name("_" + prop.name.value) :: Nil
            }.collect { case t: Term =>
              t
            }}).swap.map(e => DecodingFailure(e.getMessage, Nil)).swap
            }
           """
        }}
        }
        }
       """.stats

      case (FieldMatchPolicy(_), Some(additional)) =>
        List(
          q"""
          implicit lazy val decoder: Decoder[$objectTypeName] =
          new Decoder[$objectTypeName] {
            def apply(c: HCursor): Decoder.Result[$objectTypeName] = {
              ${genFlatMaps(classDef.ctor.paramss.flatten) { case (_, _, _, companionName, _) =>
            val additionalParamName = Term.Name("_" + additional.propertyName)

            q"""
                AdditionalProperties.decoder(c).map {
                  $additionalParamName: Option[${additional.propertyType}] =>
                    $companionName(..${generatePropertiesCode(classDef) { prop =>
              Term.Name("_" + prop.name.value) :: Nil
            }.collect { case t: Term =>
              t
            }}
              )($additionalParamName)
            }
           """
          }}
        }
        }
       """
        )

      case (FieldMatchPolicy(IgnoreExtra(_)) | HasAnyDefaults() | HasAnyOverrides(_), None) =>
        List(
          q"""
        implicit lazy val decoder: Decoder[$objectTypeName] =
          new Decoder[$objectTypeName] {
            def apply(c: HCursor): Decoder.Result[$objectTypeName] = {
              ${genFlatMaps(classDef.ctor.paramss.flatten) {
            case (fieldName, paramName, paramType, companionName, Some(default)) =>
              q"""
              c.getOrElse[$paramType]($fieldName)($default).map {
                $paramName: $paramType =>
                    $companionName(..${generatePropertiesCode(classDef) { prop =>
                Term.Name("_" + prop.name.value) :: Nil
              }.collect { case t: Term =>
                t
              }})
            }
           """

            case (fieldName, paramName, paramType, companionName, None) =>
              q"""
              c.downField($fieldName).as[$paramType].map {
                $paramName: $paramType =>
                    $companionName(..${generatePropertiesCode(classDef) { prop =>
                Term.Name("_" + prop.name.value) :: Nil
              }.collect { case t: Term =>
                t
              }})
            }
           """

          }}
        }
        }
       """
        )

      case _ =>
        List(
          q"""
          implicit lazy val decoder: Decoder[$objectTypeName] = deriveDecoder[$objectTypeName]
         """
        )
    }
  }

  private def mapTypeCodec(context: ModelGenContext): List[Stat] =
    if (shouldDeriveJson(context.objectType)) {
      context.isMapType match {
        case Some(mapType) =>
          val mapApply = Type.Apply(mapType.mapType, List(mapType.keyType, mapType.valueType))

          val decodeType: Type.Apply = if (mapType.optional) {
            Type.Apply(Type.Name("Option"), List(mapApply))
          } else mapApply

          val jsonTypeHint = discriminatorValue(context.objectType).map(discriminatorValue => {
            q"""
               val jsonTypeHint = $discriminatorValue
             """
          })

          q"""
        import io.circe._
        import io.circe.syntax._
        import io.circe.generic.semiauto._
        import io.circe.Decoder.Result

        implicit lazy val decoder: Decoder[${Type.Name(
            context.objectType.getName
          )}] = new Decoder[${Type.Name(context.objectType.getName)}] {
            override def apply(c: HCursor): Result[${Type.Name(context.objectType.getName)}] =
              c.as[$decodeType].map(${Term.Name(
            context.objectType.getName
          )}.apply)
        }
        implicit lazy val encoder: Encoder[${Type.Name(
            context.objectType.getName
          )}] = new Encoder[${Type.Name(context.objectType.getName)}] {
            override def apply(a: ${Type.Name(context.objectType.getName)}): Json =
              a.values.asJson
        }

        ..${jsonTypeHint}
         """.stats

        case None => List.empty
      }

    } else List.empty

  override def modifyAdditionalProperties(classDef: Defn.Class, companion: Option[Defn.Object])(
      implicit context: ModelGenContext
  ): DefnWithCompanion[Defn.Class] = {
    val mapType = termFromName(context.params.defaultTypes.map)

    DefnWithCompanion(
      classDef,
      companion = companion.map(
        appendObjectStats(
          _,
          q"""
            import io.circe._
            import io.circe.generic.semiauto._

            implicit lazy val decoder: Decoder[Option[${classDef.name}]] = new Decoder[Option[${classDef.name}]] {
              final def apply(c: HCursor): Decoder.Result[Option[${classDef.name}]] = {
                val allKeys = c.keys.fold(Set.empty[String])(_.toSet)

                Right(
                  Option(allKeys.filterNot(propertyNames.contains))
                    .filterNot(_.isEmpty)
                    .map {
                      _.foldLeft($mapType.newBuilder[String, Json]) {
                        case (accum, key) =>
                          c.downField(key).focus.fold(accum) {
                            v => accum += (key -> v)
                          }
                      }
                    }
                    .map(b => AdditionalProperties(b.result()))
                )
              }
            }

            def merge(into: Json, oap: Option[AdditionalProperties]): Json = {
              oap.fold(into)(merge(into, _))
            }

            def merge(into: Json, ap: AdditionalProperties): Json = {
              Json.fromFields(ap.underlying).deepMerge(into)
            }
           """.stats
        )
      )
    )
  }

  override def modifyClass(classDef: Defn.Class, companion: Option[Defn.Object])(implicit
      context: ModelGenContext
  ): DefnWithCompanion[Defn.Class] =
    DefnWithCompanion(
      classDef,
      companion = companion.map(
        appendObjectStats(
          _,
          context.isMapType.fold(deriveJson(classDef))(_ => mapTypeCodec(context))
        )
      )
    )

  override def modifyTrait(traitDef: Defn.Trait, companion: Option[Defn.Object])(implicit
      context: ModelGenContext
  ): DefnWithCompanion[Defn.Trait] =
    DefnWithCompanion(
      defn = traitDef,
      companion = companion.map(appendObjectStats(_, deriveJsonTypeSwitch(context)))
    )

  override def modifyObject(objectDef: Defn.Object)(implicit
      context: ModelGenContext
  ): DefnWithCompanion[Defn.Object] =
    discriminatorAndValue(context.objectType)
      .map { case (discriminatorPropertyName, discriminatorValueString) =>
        DefnWithCompanion(
          appendObjectStats(
            objectDef,
            q"""
        import io.circe._
        import io.circe.generic.semiauto._
        import io.circe.Decoder.Result

        val jsonTypeHint = $discriminatorValueString

        implicit lazy val decoder: Decoder[${Type.Singleton(
              Term.Name(context.objectType.getName)
            )}] = new Decoder[${Type.Singleton(Term.Name(context.objectType.getName))}] {
          override def apply(c: HCursor): Result[${Type.Singleton(
              Term.Name(context.objectType.getName)
            )}] = c.downField($discriminatorPropertyName).as[String] match {
            case Right(jsonTypeHint) =>
              Right(${Term.Name(context.objectType.getName)})
            case other =>
              Left(DecodingFailure(s"unknown type: $$other", c.history))
          }
        }
        implicit lazy val encoder: Encoder[${Type.Singleton(
              Term.Name(context.objectType.getName)
            )}] = new Encoder[${Type.Singleton(Term.Name(context.objectType.getName))}] {
          override def apply(a: ${Type
              .Singleton(Term.Name(context.objectType.getName))}): Json = Json.obj($discriminatorPropertyName -> Json.fromString(jsonTypeHint))
        }
         """.stats
          ),
          None
        )
      }
      .getOrElse(DefnWithCompanion(objectDef, None))

  override def modifyPackageObject(
      libs: List[LibrarySupport],
      api: Api
  )(implicit context: ModelGenContext): Pkg.Object => Pkg.Object = {
    val formatStats: List[Stat] = formats.map { case (alias, fullQualifiedName) =>
      q"""implicit lazy val ${Pat.Var(Term.Name(alias))} = ${packageTerm(fullQualifiedName)}"""
    }.toList

    val formatsObject = formatStats.headOption.map { _ =>
      q"""
         object Formats {
           ..$formatStats
         }
       """
    }

    appendPkgObjectStats(_, eitherCodec ++ formatsObject.toList)
  }

  override def modifyEnum(
      enumType: StringType,
      params: ModelGenParams
  )(enumTrait: Defn.Trait, companion: Option[Defn.Object]): DefnWithCompanion[Defn.Trait] = {
    val enumDecode = Defn.Val(
      List(Mod.Implicit(), Mod.Lazy()),
      List(Pat.Var(Term.Name("decoder"))),
      Some(Type.Apply(Type.Name("Decoder"), List(Type.Name(enumType.getName)))),
      Term.Apply(
        Term.Select(
          Term.ApplyType(Term.Name("Decoder"), List(Type.Name("String"))),
          Term.Name("emap")
        ),
        List(
          Term.PartialFunction(
            enumType.getEnum.asScala
              .map(instance =>
                Case(
                  Lit.String(instance.getValue.toString),
                  None,
                  Term.Apply(
                    Term.Name("Right"),
                    List(Term.Name(instance.getValue.toString.toUpperCase))
                  )
                )
              )
              .toList ++ (params.generateDefaultEnumVariant match {
              case Some(name) =>
                List(
                  Case(
                    Pat.Var(Term.Name("other")),
                    None,
                    Term.Apply(
                      Term.Name("Right"),
                      List(Term.Apply(Term.Name(name), List(Term.Name("other"))))
                    )
                  )
                )
              case None =>
                List(
                  Case(
                    Pat.Var(Term.Name("other")),
                    None,
                    Term.Apply(
                      Term.Name("Left"),
                      List(
                        Term.Interpolate(
                          Term.Name("s"),
                          List(Lit.String("invalid enum value: "), Lit.String("")),
                          List(Term.Name("other"))
                        )
                      )
                    )
                  )
                )
            })
          )
        )
      )
    )

    val enumEncode = Defn.Val(
      List(Mod.Implicit(), Mod.Lazy()),
      List(Pat.Var(Term.Name("encoder"))),
      Some(Type.Apply(Type.Name("Encoder"), List(Type.Name(enumType.getName)))),
      Term.Apply(
        Term.Select(
          Term.ApplyType(Term.Name("Encoder"), List(Type.Name("String"))),
          Term.Name("contramap")
        ),
        List(
          Term.PartialFunction(
            enumType.getEnum.asScala
              .map(instance =>
                Case(
                  Term.Name(instance.getValue.toString.toUpperCase),
                  None,
                  Lit.String(instance.getValue.toString)
                )
              )
              .toList ++ (params.generateDefaultEnumVariant match {
              case Some(name) =>
                List(
                  Case(
                    Pat.Extract(Term.Name(name), List(Pat.Var(Term.Name("value")))),
                    None,
                    Term.Name("value")
                  )
                )
              case None => Nil
            })
          )
        )
      )
    )

    val stats =
      q"""
        import io.circe._
        $enumEncode
        $enumDecode
       """.stats

    DefnWithCompanion(enumTrait, companion.map(appendObjectStats(_, stats)))
  }
}
