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
    if(shouldDeriveJson(context.objectType)) {
      val typeName = context.objectType.getName
      val subTypes = context.getSubTypes.toList

      val encodeAll =
        Defn.Def(List(Mod.Implicit()), Term.Name("encodeAll"),
        tparams = List(Type.Param(Nil, Type.Name(typeName), Nil, Type.Bounds(None, None), Nil, Nil)) ++ subTypes.map { subType =>
          Type.Param(Nil, Type.Name(subType.getName), Nil, Type.Bounds(None, Some(Type.Name(typeName))), Nil, Nil)
        },
        paramss = List(subTypes.map { subType =>
          Term.Param(List(Mod.Implicit()), Term.Name(s"${subType.getName}Encoder"), Some(Type.Apply(Type.Name("Encoder"), List(Type.Name("A")))), None)
        }),
        decltpe = Some(Type.Apply(Type.Name("Encoder"), List(Type.Name(typeName)))),
        body = {
          val matchTypes = Term.Match(
            Term.Name("base"),
            cases = subTypes.map( subType => {
              val caseName = subType.getName.toLowerCase
              val typeName = subType.getName
              Case(Pat.Typed(Pat.Var(Term.Name(caseName)), Type.Name(typeName)), None, Term.Apply(Term.Name(s"${typeName}Encoder"), List(Term.Name(caseName))))
            }),
            mods = Nil
          )

          Term.NewAnonymous(
            Template(
              early = Nil,
              inits = List(Init(Type.Apply(Type.Name("Encoder"), List(Type.Name(typeName))), Name(""), Nil)),
              self = Self(Name(""), None),
              stats =
                List(
                  Defn.Def(
                    mods = List(Mod.Override()),
                    name = Term.Name("apply"), Nil,
                    paramss = List(List(Term.Param(Nil, Term.Name("base"), Some(Type.Name(typeName)), None))),
                    decltpe = Some(Type.Name("Json")),
                    body = matchTypes)
                ),
              derives = Nil
            )
          )
        })

      val decodeAll =
        Defn.Def(List(Mod.Implicit()), Term.Name("decodeAll"),
          List(Type.Param(Nil, Type.Name(typeName), Nil, Type.Bounds(None, None), Nil, Nil), Type.Param(Nil, Type.Name("A"), Nil, Type.Bounds(None, Some(Type.Name(typeName))), Nil, Nil), Type.Param(Nil, Type.Name("B"), Nil, Type.Bounds(None, Some(Type.Name(typeName))), Nil, Nil)), List(List(Term.Param(List(Mod.Implicit()), Term.Name("aDecoder"), Some(Type.Apply(Type.Name("Decoder"), List(Type.Name("A")))), None), Term.Param(List(Mod.Implicit()), Term.Name("bDecoder"), Some(Type.Apply(Type.Name("Decoder"), List(Type.Name("B")))), None))), Some(Type.Apply(Type.Name("Decoder"), List(Type.Name(typeName)))), Term.NewAnonymous(Template(Nil, List(Init(Type.Apply(Type.Name("Decoder"), List(Type.Name(typeName))), Name(""), Nil)), Self(Name(""), None), List(Defn.Def(List(Mod.Override()), Term.Name("apply"), Nil, List(List(Term.Param(Nil, Term.Name("c"), Some(Type.Name("HCursor")), None))), Some(Type.Apply(Type.Name("Result"), List(Type.Name(typeName)))), Term.Apply(Term.Apply(Term.Select(Term.Name("aDecoder"), Term.Name("or")), List(Term.Name("bDecoder"))), List(Term.Name("c"))))), Nil)))


      val jsonStats =
        q"""
          import io.circe.Decoder.Result
          import io.circe.{HCursor, Json, Decoder, Encoder}

          implicit def decodeAll(implicit cartReference: Decoder[CartReference], customerRef: Decoder[CustomerReference]): Decoder[Reference] = new Decoder[Reference] {
            override def apply(c: HCursor): Result[Reference] =
              cartReference.tryDecode(c).fold(_ => customerRef.tryDecode(c), Right(_))
          }

          implicit def encodeAll(implicit cartReference: Encoder[CartReference], customerRef: Encoder[CustomerReference]): Encoder[Reference] = new Encoder[Reference] {
            override def apply(a: Reference): Json = a match {
              case a: CartReference => cartReference(a)
              case b: CustomerReference => customerRef(b)
            }
          }
        """.stats
      jsonStats
    } else List.empty

  private def deriveJson(objectType: ObjectType): List[Stat] =
    if(shouldDeriveJson(objectType)) {
      q"""import io.circe._
          import io.circe.generic.semiauto._

          implicit lazy val json: Codec[${Type.Name(objectType.getName)}] = deriveCodec[${Type.Name(objectType.getName)}]
      """.stats
    } else List.empty

  override def modifyClass(classDef: Defn.Class, companion: Option[Defn.Object])(context: ModelGenContext): DefnWithCompanion[Defn.Class] =
    DefnWithCompanion(
      classDef,
      companion = companion.map(appendObjectStats(_, deriveJson(context.objectType)))
    )

  override def modifyTrait(traitDef: Defn.Trait, companion: Option[Defn.Object])(context: ModelGenContext): DefnWithCompanion[Defn.Trait] =
    DefnWithCompanion(
      defn = traitDef,
      companion = {
        val jsonStats = if (!context.isSealed || Option(context.objectType.getDiscriminator).isEmpty) {
          deriveJsonTypeSwitch(context)
        } else deriveJson(context.objectType)

        companion.map(appendObjectStats(_, jsonStats))
      }
    )

  override def modifyPackageObject: Pkg.Object => Pkg.Object =
    appendPkgObjectStats(_, eitherCodec)
}
