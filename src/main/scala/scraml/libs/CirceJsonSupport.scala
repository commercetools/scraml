package scraml.libs

import scala.annotation.tailrec
import scala.collection.immutable.TreeSet

import scraml.LibrarySupport._
import scraml.ModelGen.isSingleton
import scraml.RMFUtil.getAnnotation
import scraml.{DefnWithCompanion, LibrarySupport, ModelGenContext, RMFUtil}
import io.vrap.rmf.raml.model.types.{AnyType, ObjectType, Property, StringType}

object CirceJsonSupport extends LibrarySupport {
  import scala.meta._
  import scala.collection.JavaConverters._

  object IgnoreThisType {
    def unapply(context: ModelGenContext): Boolean =
      !getAnnotation(context.objectType)("scala-derive-json")
        .forall(_.getValue.getValue.toString.toBoolean)
  }

  sealed trait TypeInspection {
    @tailrec
    final protected def rootType(theType: AnyType): AnyType =
      if (theType.getType ne null)
        rootType(theType.getType)
      else
        theType
  }

  object UsesDiscriminator extends TypeInspection {
    def unapply(context: ModelGenContext): Option[String] =
      rootType(context.objectType) match {
        case obj: ObjectType =>
          Option(obj.getDiscriminator)
        case _ =>
          None
      }
  }

  object UsesFieldMap extends TypeInspection {
    def unapply(context: ModelGenContext): Option[Seq[Property]] =
      rootType(context.objectType) match {
        case obj: ObjectType if obj.getDiscriminator eq null =>
          Some(context.typeProperties)
        case _ =>
          None
      }
  }

  implicit val anyTypeOrdering: Ordering[AnyType] = new Ordering[AnyType] {
    override def compare(x: AnyType, y: AnyType): Int =
      x.getName.compareTo(y.getName)
  }

  private def expandLeafTypes(context: ModelGenContext): AnyType => TreeSet[AnyType] = {
    case subType: ObjectType if context.getSubTypes(subType).nonEmpty =>
      context
        .getSubTypes(subType)
        .foldLeft(TreeSet.empty[AnyType]) { case (accum, child) =>
          accum ++ expandLeafTypes(context)(child)
        }
    case other =>
      TreeSet(other)
  }

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

  private def declareToJson(): List[Stat] =
    List[Stat](q"def toJson(): io.circe.Json")

  private def defineToJson(context: ModelGenContext): List[Stat] =
    List[Stat](
      q"import io.circe.{ Codec, Json }",
      q"""
        def toJson(): Json = implicitly[Codec[${Type.Name(context.objectType.getName)}]].apply(this)
      """
    )

  private def deriveJsonTypeSwitch(context: ModelGenContext): List[Stat] = {
    val typeName = context.objectType.getName
    val subTypes =
      (TreeSet.empty[AnyType] ++ context.getSubTypes.flatMap(expandLeafTypes(context))).toList

    subTypes.headOption
      .map { firstSubType =>
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
            subTypes.map {
              case subType: ObjectType if !isSingleton(subType, context.anyTypeName) =>
                Case(
                  Pat
                    .Extract(Term.Name("Right"), List(Lit.String(subType.getDiscriminatorValue))),
                  None,
                  Term.Apply(Term.Name(s"${subType.getName}Decoder"), List(Term.Name("c")))
                )
              case subType: ObjectType => // case object
                Case(
                  Pat
                    .Extract(Term.Name("Right"), List(Lit.String(subType.getDiscriminatorValue))),
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

        val decodeAll =
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

        q"""
              import io.circe.Decoder.Result
              import io.circe._

              implicit val encodeAll: Encoder[${Type.Name(typeName)}] = new Encoder[${Type.Name(
          typeName
        )}] {
                override def apply(instance: ${Type.Name(typeName)}): Json = instance.toJson()
              }

              $decodeAll
            """.stats
      }
      .getOrElse(List.empty) // no subtypes
  }

  private def deriveJsonFieldMap(context: ModelGenContext): List[Stat] = {
    def mkDecoderTermName(base: String) = Term.Name(base + "Decoder")

    val typeName = Type.Name(context.objectType.getName)
    val subTypes = (TreeSet.empty[AnyType] ++ context.getSubTypes.flatMap(expandLeafTypes(context)))
      .collect { case obj: ObjectType =>
        obj
      }
      .toList
      .sortBy(RMFUtil.typeProperties(_).size)
      .reverse

    q"""
       import io.circe._

      implicit val encodeAll: Encoder[$typeName] = new Encoder[$typeName] {
        override def apply(instance: $typeName): Json = instance.toJson()
      }

      implicit def decodeAll(..${subTypes match {
      case Nil =>
        throw new RuntimeException(s"no subtypes found: ${context.objectType.getName}")
      case one :: Nil =>
        List(
          Term.Param(
            mods = List(Mod.Implicit()),
            name = mkDecoderTermName(one.getName),
            decltpe = Some(Type.Apply(Type.Name("Decoder"), List(Type.Name(one.getName)))),
            default = None
          )
        )
      case multiple =>
        multiple.map { child =>
          Term.Param(
            mods = List(Mod.Implicit()),
            name = mkDecoderTermName(child.getName),
            decltpe = Some(Type.Apply(Type.Name("Decoder"), List(Type.Name(child.getName)))),
            default = None
          )
        }
    }}
      ): Decoder[${typeName}] = new Decoder[${typeName}] {
        override def apply(c: HCursor): Decoder.Result[${typeName}] = ${subTypes match {
      case Nil =>
        throw new RuntimeException(s"no subtypes found: ${context.objectType.getName}")
      case one :: Nil =>
        q"""${mkDecoderTermName(one.getName)}.tryDecode(c)"""
      case multiple =>
        val groups = multiple.grouped(20).zipWithIndex
        val localDefs: List[Defn.Def] = groups.map {
          case (last :: Nil, index) =>
            q"""def ${Term.Name("chunk" + index)}() = ${mkDecoderTermName(
              last.getName
            )}.tryDecode(c)"""
          case (head :: tail, index) =>
            q"""
                 def ${Term.Name("chunk" + index)}() = ${tail.foldLeft(q"""${mkDecoderTermName(
              head.getName
            )}.tryDecode(c)""") { case (accum, decoder) =>
              q"""
                          $accum.fold(
                            _ => ${mkDecoderTermName(decoder.getName)}.tryDecode(c),
                            Right(_)
                          )
                        """
            }}
                 """
          case (Nil, index) =>
            q"""def ${Term.Name(
              "chunk" + index
            )}() = Left(DecodingFailure("unknown payload: " + ${context.objectType.getName}))"""
        }.toList

        q"""
                 ..$localDefs
                 ${localDefs.map(d => q"${d.name}()").reduce { (a, b) =>
          q"$a.fold(_ => $b, Right(_))"
        }}
               """
    }}
      }
     """.stats
  }

  private def deriveJsonNoProps(context: ModelGenContext): List[Stat] = {
    def mkDecoderTermName(base: String) = Term.Name(base + "Decoder")

    val typeName = Type.Name(context.objectType.getName)
    val subTypes =
      (TreeSet.empty[AnyType] ++ context.getSubTypes.flatMap(expandLeafTypes(context))).collect {
        case obj: ObjectType =>
          obj
      }.toList

    q"""
      import io.circe._

      implicit val encodeAll: Encoder[$typeName] = new Encoder[$typeName] {
        override def apply(instance: $typeName): Json = instance.toJson()
      }

      implicit def decodeAll(..${subTypes match {
      case Nil =>
        throw new RuntimeException(s"no subtypes found: ${context.objectType.getName}")
      case one :: Nil =>
        List(
          Term.Param(
            mods = List(Mod.Implicit()),
            name = mkDecoderTermName(one.getName),
            decltpe = Some(Type.Apply(Type.Name("Decoder"), List(Type.Name(one.getName)))),
            default = None
          )
        )
      case multiple =>
        multiple.map { child =>
          Term.Param(
            mods = List(Mod.Implicit()),
            name = mkDecoderTermName(child.getName),
            decltpe = Some(Type.Apply(Type.Name("Decoder"), List(Type.Name(child.getName)))),
            default = None
          )
        }
    }}
      ): Decoder[${typeName}] = new Decoder[${typeName}] {
        override def apply(c: HCursor): Decoder.Result[${typeName}] = ${subTypes match {
      case Nil =>
        throw new RuntimeException(s"no subtypes found: ${context.objectType.getName}")
      case one :: Nil =>
        Term.Block(
          List(
            q"""${mkDecoderTermName(one.getName)}.tryDecode(c)"""
          )
        )
      case head :: tail =>
        Term.Block(
          List(
            tail.foldLeft(q"""${mkDecoderTermName(head.getName)}.tryDecode(c)""") {
              case (accum, decoder) =>
                q"""
                        $accum.fold(
                          _ => ${mkDecoderTermName(decoder.getName)}.tryDecode(c),
                          Right(_)
                        )
                      """
            }
          )
        )
    }}
      }
     """.stats
  }

  private def deriveJson(objectType: ObjectType): List[Stat] =
    q"""import io.circe._
        import io.circe.generic.semiauto._

        implicit lazy val json: Codec[${Type.Name(objectType.getName)}] = deriveCodec[${Type.Name(
      objectType.getName
    )}]
    """.stats

  private def mapTypeCodec(context: ModelGenContext): List[Stat] =
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

  override def modifyClass(classDef: Defn.Class, companion: Option[Defn.Object])(
      context: ModelGenContext
  ): DefnWithCompanion[Defn.Class] =
    context match {
      case IgnoreThisType() =>
        super.modifyClass(classDef, companion)(context)
      case _ =>
        DefnWithCompanion(
          appendClassStats(classDef, defineToJson(context)),
          companion = companion.map(
            appendObjectStats(
              _,
              if (context.isMapType.isDefined) mapTypeCodec(context)
              else deriveJson(context.objectType)
            )
          )
        )
    }

  override def modifyObject(objectDef: Defn.Object)(
      context: ModelGenContext
  ): DefnWithCompanion[Defn.Object] =
    context match {
      case IgnoreThisType() =>
        super.modifyObject(objectDef)(context)
      case UsesDiscriminator(property) =>
        DefnWithCompanion(
          appendObjectStats(
            objectDef,
            List(
              q"import io.circe.Json",
              q"""
                def toJson(): Json =
                  Json.fromFields(Map($property -> Json.fromString(${Option(
                context.objectType.getDiscriminatorValue
              ).getOrElse(context.objectType.getName)})))
              """
            )
          ),
          None
        )
      case _ =>
        DefnWithCompanion(
          appendObjectStats(
            objectDef,
            List(
              q"import io.circe.Json",
              q"""
                def toJson(): Json =
                  Json.fromFields(Map("type", Json.fromString(${context.objectType.getName})))
              """
            )
          ),
          None
        )
    }

  override def modifyTrait(traitDef: Defn.Trait, companion: Option[Defn.Object])(
      context: ModelGenContext
  ): DefnWithCompanion[Defn.Trait] =
    context match {
      case IgnoreThisType() =>
        super.modifyTrait(traitDef, companion)(context)
      case UsesDiscriminator(_) =>
        DefnWithCompanion(
          defn = appendTraitStats(traitDef, declareToJson()),
          companion = companion.map(appendObjectStats(_, deriveJsonTypeSwitch(context)))
        )
      case UsesFieldMap(fields) if fields.isEmpty =>
        DefnWithCompanion(
          defn = appendTraitStats(traitDef, declareToJson()),
          companion = companion.map(appendObjectStats(_, deriveJsonNoProps(context)))
        )
      case UsesFieldMap(_) =>
        DefnWithCompanion(
          defn = appendTraitStats(traitDef, declareToJson()),
          companion = companion.map(appendObjectStats(_, deriveJsonFieldMap(context)))
        )
    }

  override def modifyPackageObject: Pkg.Object => Pkg.Object =
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
