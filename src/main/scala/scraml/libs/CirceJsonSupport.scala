package scraml.libs

import io.vrap.rmf.raml.model.modules.Api
import scraml.LibrarySupport._
import scraml.MetaUtil.packageTerm
import scraml.ModelGen.isSingleton
import scraml.RMFUtil.getAnnotation
import scraml.{DefnWithCompanion, LibrarySupport, ModelGenContext, RMFUtil}
import io.vrap.rmf.raml.model.types.{AnyType, ObjectType, StringType}

object CirceJsonSupport extends LibrarySupport {
  import scala.meta._
  import scala.collection.JavaConverters._

  private def shouldDeriveJson(objectType: ObjectType): Boolean =
    getAnnotation(objectType)("scala-derive-json").forall(_.getValue.getValue.toString.toBoolean)

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

  private def discriminator(aType: ObjectType): Option[String] =
    RMFUtil.discriminators(aType) match {
      case Nil           => None
      case single :: Nil => Some(single)
      case _ =>
        throw new IllegalArgumentException(
          "only single discriminator should be defined in type hierarchy"
        )
    }

  private def discriminatorValue(aType: ObjectType): String =
    Option(aType.getDiscriminatorValue).getOrElse(aType.getName.toLowerCase)

  private def typeEncoder(context: ModelGenContext): Defn.Def = {
    val subTypes         = context.getSubTypes
    val typeName         = context.objectType.getName
    val discriminatorOpt = discriminator(context.objectType)

    Defn.Def(
      List(Mod.Implicit()),
      Term.Name("encodeAll"),
      Nil,
      paramss = List(
        subTypes.flatMap {
          case subType: ObjectType
            if !isSingleton(subType, context.anyTypeName) => // filter out case objects
            Some(
              Term.Param(
                List(Mod.Implicit()),
                Term.Name(s"${subType.getName}Encoder"),
                Some(Type.Apply(Type.Name("Encoder"), List(Type.Name(subType.getName)))),
                None
              )
            )
          case _ => None
        }
      ),
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
                subTypes.map {
                  case subType: ObjectType
                    if discriminatorOpt.isDefined && !isSingleton(subType, context.anyTypeName) =>
                    Case(
                      Pat.Typed(
                        Pat.Var(Term.Name(subType.getName.toLowerCase)),
                        Type.Name(subType.getName)
                      ),
                      None,
                      Term.Apply(
                        Term.Select(
                          Term.Apply(
                            Term.Name(s"${subType.getName}Encoder"),
                            List(Term.Name(subType.getName.toLowerCase))
                          ),
                          Term.Name("mapObject")
                        ),
                        List(
                          Term.Apply(
                            Term.Select(Term.Placeholder(), Term.Name("add")),
                            List(
                              Lit.String(discriminatorOpt.getOrElse("type")),
                              Term.Apply(
                                packageTerm("Json.fromString"),
                                List(Lit.String(discriminatorValue(subType)))
                              )
                            )
                          )
                        )
                      )
                    )
                  case subType: ObjectType
                    if discriminatorOpt.isEmpty && !isSingleton(subType, context.anyTypeName) =>
                    Case(
                      Pat.Typed(
                        Pat.Var(Term.Name(subType.getName.toLowerCase)),
                        Type.Name(subType.getName)
                      ),
                      None,
                      Term.Apply(
                        Term.Name(s"${subType.getName}Encoder"),
                        List(Term.Name(subType.getName.toLowerCase))
                      )
                    )
                  case subType: ObjectType => // case object
                    Case(
                      Term.Name(subType.getName),
                      None,
                      Term.Apply(
                        Term.Select(Term.Name("Json"), Term.Name("obj")),
                        List(
                          Term.ApplyInfix(
                            Lit.String(
                              discriminatorOpt.getOrElse("type")
                            ),
                            Term.Name("->"),
                            Nil,
                            List(
                              Term.Apply(
                                Term.Select(Term.Name("Json"), Term.Name("fromString")),
                                List(
                                  Lit.String(discriminatorValue(subType))
                                )
                              )
                            )
                          )
                        )
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

  private def typeDecoder(firstSubType: AnyType, context: ModelGenContext): Defn.Def = {
    val subTypes         = context.getSubTypes
    val typeName         = context.objectType.getName
    val discriminatorOpt = discriminator(context.objectType)
    val decode: Term = if (discriminatorOpt.isEmpty) {
      val initRead: String =
        q"""
            ${Term.Name(s"${firstSubType.getName}Decoder")}.tryDecode(c)
            """.toString

      val trySubtypes = subTypes.drop(1).foldLeft(initRead) { case (acc, next) =>
        val nextRead = q"""fold(_ => ${Term
          .Name(s"${next.getName}Decoder")}.tryDecode(c), Right(_))""".toString
        s"$acc.$nextRead"
      }

      trySubtypes.parse[Term].get
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
        subTypes.map {
          case subType: ObjectType if !isSingleton(subType, context.anyTypeName) =>
            Case(
              Pat
                .Extract(Term.Name("Right"), List(Lit.String(discriminatorValue(subType)))),
              None,
              Term.Apply(Term.Name(s"${subType.getName}Decoder"), List(Term.Name("c")))
            )
          case subType: ObjectType => // case object
            Case(
              Pat
                .Extract(Term.Name("Right"), List(Lit.String(discriminatorValue(subType)))),
              None,
              Term.Apply(Term.Name("Right"), List(Term.Name(subType.getName)))
            )
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

    Defn.Def(
      List(Mod.Implicit()),
      Term.Name("decodeAll"),
      Nil,
      List(
        subTypes.flatMap {
          case subType: ObjectType if !isSingleton(subType, context.anyTypeName) =>
            Some(
              Term.Param(
                List(Mod.Implicit()),
                Term.Name(s"${subType.getName}Decoder"),
                Some(Type.Apply(Type.Name("Decoder"), List(Type.Name(subType.getName)))),
                None
              )
            )
          case _ => None
        }
      ),
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

  private def deriveJsonTypeSwitch(context: ModelGenContext): List[Stat] =
    if (shouldDeriveJson(context.objectType)) {
      val subTypes: List[AnyType] = context.getSubTypes

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
      q"""import io.circe._
          import io.circe.generic.semiauto._

          implicit lazy val json: Codec[${Type.Name(objectType.getName)}] = deriveCodec[${Type.Name(
        objectType.getName
      )}]
      """.stats
    } else List.empty

  private def mapTypeCodec(context: ModelGenContext): List[Stat] =
    if (shouldDeriveJson(context.objectType)) {
      context.isMapType match {
        case Some(mapType) =>
          val mapApply = Type.Apply(Type.Name("Map"), List(mapType.keyType, mapType.valueType))

          val decodeType: Type.Apply = if (mapType.optional) {
            Type.Apply(Type.Name("Option"), List(mapApply))
          } else mapApply

          q"""import io.circe.syntax._
          import io.circe._
          import io.circe.Decoder.Result

          implicit lazy val json: Codec[${Type.Name(context.objectType.getName)}] = new Codec[${Type
            .Name(context.objectType.getName)}] {
            override def apply(a: ${Type.Name(context.objectType.getName)}): Json =
              a.values.asJson
            override def apply(c: HCursor): Result[${Type.Name(context.objectType.getName)}] =
              c.as[$decodeType].map(${Term.Name(
            context.objectType.getName
          )}.apply)
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

  override def modifyPackageObject(api: Api): Pkg.Object => Pkg.Object =
    appendPkgObjectStats(_, eitherCodec)

  override def modifyEnum(
                           enumType: StringType
                         )(enumTrait: Defn.Trait, companion: Option[Defn.Object]): DefnWithCompanion[Defn.Trait] = {
    val enumDecode = Defn.Val(
      List(Mod.Implicit()),
      List(Pat.Var(Term.Name("decode"))),
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
      List(Mod.Implicit()),
      List(Pat.Var(Term.Name("encode"))),
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
