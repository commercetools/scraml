package scraml.libs

import _root_.io.vrap.rmf.raml.model.types.ObjectType
import scraml.LibrarySupport._
import scraml.RMFUtil.getAnnotation
import scraml.{DefnWithCompanion, LibrarySupport, ModelGenContext}

import scala.meta._

object CirceJsonSupport extends LibrarySupport {
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

  private def deriveJsonTypeSwitch(context: ModelGenContext): List[Stat] =
    if (shouldDeriveJson(context.objectType)) {
      val typeName = context.objectType.getName
      val subTypes = context.getSubTypes.toList

      subTypes.headOption
        .map { firstSubType =>
          val encodeAll =
            Defn.Def(
              List(Mod.Implicit()),
              Term.Name("encodeAll"),
              Nil,
              List(
                subTypes.map(subType =>
                  Term.Param(
                    List(Mod.Implicit()),
                    Term.Name(s"${subType.getName}Encoder"),
                    Some(Type.Apply(Type.Name("Encoder"), List(Type.Name(subType.getName)))),
                    None
                  )
                )
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
                        subTypes.map { case subType: ObjectType =>
                          if (Option(context.objectType.getDiscriminator).isDefined)
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
                                      Lit.String(context.objectType.getDiscriminator),
                                      Term.Apply(
                                        Term.Name("JString"),
                                        List(Lit.String(subType.getDiscriminatorValue))
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          else
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
                        },
                        Nil
                      )
                    )
                  ),
                  Nil
                )
              )
            )

          val decode: Term = if (Option(context.objectType.getDiscriminator).isEmpty) {
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
                    List(Lit.String(context.objectType.getDiscriminator))
                  ),
                  Term.Name("as")
                ),
                List(Type.Name("String"))
              ),
              subTypes.map { case subType: ObjectType =>
                Case(
                  Pat.Extract(Term.Name("Right"), List(Lit.String(subType.getDiscriminatorValue))),
                  None,
                  Term.Apply(Term.Name(s"${subType.getName}Decoder"), List(Term.Name("c")))
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

          val decodeAll =
            Defn.Def(
              List(Mod.Implicit()),
              Term.Name("decodeAll"),
              Nil,
              List(
                subTypes.map(subType =>
                  Term.Param(
                    List(Mod.Implicit()),
                    Term.Name(s"${subType.getName}Decoder"),
                    Some(Type.Apply(Type.Name("Decoder"), List(Type.Name(subType.getName)))),
                    None
                  )
                )
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

          val jsonStats =
            q"""
          import io.circe.Decoder.Result
          import io.circe._

          $decodeAll

          $encodeAll
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

  override def modifyClass(classDef: Defn.Class, companion: Option[Defn.Object])(
      context: ModelGenContext
  ): DefnWithCompanion[Defn.Class] =
    DefnWithCompanion(
      classDef,
      companion = companion.map(appendObjectStats(_, deriveJson(context.objectType)))
    )

  override def modifyTrait(traitDef: Defn.Trait, companion: Option[Defn.Object])(
      context: ModelGenContext
  ): DefnWithCompanion[Defn.Trait] =
    DefnWithCompanion(
      defn = traitDef,
      companion = companion.map(appendObjectStats(_, deriveJsonTypeSwitch(context)))
    )

  override def modifyPackageObject: Pkg.Object => Pkg.Object =
    appendPkgObjectStats(_, eitherCodec)
}
