package scraml.libs

import scraml.LibrarySupport._
import scraml.MetaUtil.packageTerm
import scraml.RMFUtil.getAnnotation
import scraml.{DefnWithCompanion, JsonSupport, LibrarySupport, ModelGen, ModelGenContext, RMFUtil}
import io.vrap.rmf.raml.model.types.{AnyType, ObjectType, StringType}

object CirceJsonSupport extends LibrarySupport with JsonSupport {
  override def jsonType: String = "io.circe.Json"

  import scala.meta._
  import scala.collection.JavaConverters._

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

  private def typeEncoder(context: ModelGenContext): Defn.Val = {
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
                subTypes.map { case subType: ObjectType =>
                  Case(
                    Pat.Typed(
                      Pat.Var(Term.Name(subType.getName.toLowerCase)),
                      if (ModelGen.isSingleton(subType, context.anyTypeName))
                        Type.Singleton(Term.Name(subType.getName))
                      else Type.Name(subType.getName)
                    ),
                    None,
                    Term.Apply(
                      packageTerm(s"${subType.getName}.encoder"),
                      List(Term.Name(subType.getName.toLowerCase))
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

  private def typeDecoder(firstSubType: AnyType, context: ModelGenContext): Defn.Val = {
    val subTypes         = context.leafTypes.toList
    val typeName         = context.objectType.getName
    val discriminatorOpt = discriminator(context.objectType)
    val decode: Term = if (discriminatorOpt.isEmpty) {
      // Sort by number of properties so that the first type tried has the most
      // properties, the next has the same or less, etc.  Since there is no
      // discriminator, this ensures types which have a subset of the fields
      // other types have are tried later.
      val sortedByProperties = subTypes
        .collect { case obj: ObjectType =>
          obj
        }
        .sortBy(RMFUtil.typeProperties(_).size)
        .reverse

      sortedByProperties match {
        case Nil =>
          q"""
             Left(DecodingFailure("no concrete types exist for: " + $typeName))
             """
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
            val nextRead = q"""fold(_ => ${packageTerm(
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
          discriminatorValue(subType).map { discriminatorValueString =>
            Case(
              Pat
                .Extract(Term.Name("Right"), List(Lit.String(discriminatorValueString))),
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

      subTypes.headOption
        .map { firstSubType =>
          val jsonStats =
            q"""
          import io.circe.Decoder.Result
          import io.circe._

          ${typeDecoder(firstSubType, context)}

          ${typeEncoder(context)}
        """.stats
          jsonStats
        }
        .getOrElse(List.empty) // no subtypes
    } else List.empty          // should not derive

  private def deriveJson(objectType: ObjectType): List[Stat] =
    if (shouldDeriveJson(objectType)) {
      val encoderDef: Defn.Val = discriminator(objectType)
        .flatMap(_ => discriminatorValue(objectType))
        .map { discriminatorValueString =>
          Defn.Val(
            List(Mod.Implicit(), Mod.Lazy()),
            List(Pat.Var(Term.Name("encoder"))),
            Some(Type.Apply(Type.Name("Encoder"), List(Type.Name(objectType.getName)))),
            rhs = Term.Apply(
              Term.Select(
                Term.ApplyType(Term.Name("deriveEncoder"), List(Type.Name(objectType.getName))),
                Term.Name("mapJsonObject")
              ),
              List(
                Term.Apply(
                  Term.Select(Term.Placeholder(), Term.Name("add")),
                  List(
                    Lit.String("type"),
                    Term.Apply(
                      Term.Select(Term.Name("Json"), Term.Name("fromString")),
                      List(Lit.String(discriminatorValueString))
                    )
                  )
                )
              )
            )
          )
        }
        .getOrElse(
          q"""
           implicit lazy val encoder: Encoder[${Type
            .Name(objectType.getName)}] = deriveEncoder[${Type.Name(
            objectType.getName
          )}]
         """
        )

      q"""import io.circe._
          import io.circe.generic.semiauto._

          implicit lazy val decoder: Decoder[${Type.Name(
        objectType.getName
      )}] = deriveDecoder[${Type.Name(
        objectType.getName
      )}]
      """.stats ++ List(encoderDef)
    } else List.empty

  private def mapTypeCodec(context: ModelGenContext): List[Stat] =
    if (shouldDeriveJson(context.objectType)) {
      context.isMapType match {
        case Some(mapType) =>
          val mapApply = Type.Apply(Type.Name("Map"), List(mapType.keyType, mapType.valueType))

          val decodeType: Type.Apply = if (mapType.optional) {
            Type.Apply(Type.Name("Option"), List(mapApply))
          } else mapApply

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
         """.stats

        case None => List.empty
      }

    } else List.empty

  override def modifyClass(classDef: Defn.Class, companion: Option[Defn.Object])(
      context: ModelGenContext
  ): DefnWithCompanion[Defn.Class] =
    DefnWithCompanion(
      classDef,
      companion = companion.map(
        appendObjectStats(
          _,
          if (context.isMapType.isDefined) mapTypeCodec(context) else deriveJson(context.objectType)
        )
      )
    )

  override def modifyTrait(traitDef: Defn.Trait, companion: Option[Defn.Object])(
      context: ModelGenContext
  ): DefnWithCompanion[Defn.Trait] =
    DefnWithCompanion(
      defn = traitDef,
      companion = companion.map(appendObjectStats(_, deriveJsonTypeSwitch(context)))
    )

  override def modifyObject(objectDef: Defn.Object)(
      context: ModelGenContext
  ): DefnWithCompanion[Defn.Object] =
    discriminatorValue(context.objectType)
      .map { discriminatorValueString =>
        DefnWithCompanion(
          appendObjectStats(
            objectDef,
            q"""
        import io.circe._
        import io.circe.generic.semiauto._
        import io.circe.Decoder.Result

        implicit lazy val decoder: Decoder[${Type.Singleton(
              Term.Name(context.objectType.getName)
            )}] = new Decoder[${Type.Singleton(Term.Name(context.objectType.getName))}] {
          override def apply(c: HCursor): Result[${Type.Singleton(
              Term.Name(context.objectType.getName)
            )}] = c.downField("type").as[String] match {
            case Right(${discriminatorValueString}) =>
              Right(${Term.Name(context.objectType.getName)})
            case other =>
              Left(DecodingFailure(s"unknown type: $$other", c.history))
          }
        }
        implicit lazy val encoder: Encoder[${Type.Singleton(
              Term.Name(context.objectType.getName)
            )}] = new Encoder[${Type.Singleton(Term.Name(context.objectType.getName))}] {
          override def apply(a: ${Type.Singleton(
              Term.Name(context.objectType.getName)
            )}): Json = Json.obj("type" -> Json.fromString(${discriminatorValueString}))
        }
         """.stats
          ),
          None
        )
      }
      .getOrElse(DefnWithCompanion(objectDef, None))

  override def modifyPackageObject: Pkg.Object => Pkg.Object =
    appendPkgObjectStats(_, eitherCodec)

  override def modifyEnum(
      enumType: StringType
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
                  Term.Apply(Term.Name("Right"), List(Term.Name(instance.getValue.toString)))
                )
              )
              .toList ++ List(
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
                  Term.Name(instance.getValue.toString),
                  None,
                  Lit.String(instance.getValue.toString)
                )
              )
              .toList
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
