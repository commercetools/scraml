package scraml.libs

import scala.meta._

import scraml.{MetaUtil, ModelGenContext}

final class TapirPackageDefinitions(private val context: ModelGenContext) {
  private trait CommonDefinitions {
    protected def eitherCodecPlain =
      q"""
      private implicit def eitherTapirCodecPlain[A, B](
        implicit aCodec: Codec.PlainCodec[A],
        bCodec: Codec.PlainCodec[B]
      )
      : Codec.PlainCodec[Either[A, B]] =
        new Codec.PlainCodec[Either[A, B]] {
          override val format = TextPlain()
          override val schema = anySchema[Either[A, B]]
          override def rawDecode(l: String): DecodeResult[Either[A, B]] = {
            aCodec.rawDecode(l) match {
              case e: DecodeResult.Failure => bCodec.rawDecode(l).map(Right(_))
              case other => other.map(Left(_))
            }
          }
          override def encode(h: Either[A, B]): String = {
            h match {
              case Left(a) => aCodec.encode(a)
              case Right(b) => bCodec.encode(b)
            }
          }
        }
       """
  }

  // Scala v2.12 specific tapir definitions
  private object Scala212 extends CommonDefinitions {
    def apply(): List[Stat] =
      q"""
          $anySchema
          $eitherCodecPlain
          $queryCollectionParamEncoder
       """.stats

    private def anySchema =
      q"""
        private implicit def anySchema[T]: Schema[T] = Schema[T](
          SchemaType.SCoproduct(Nil, None)(_ => None),
          None
        )
      """

    // the query matchers were not happy with Option[List[String]] types
    // TODO: why do we need that in the first place, could be simplified with an already defined codec?
    private def queryCollectionParamEncoder: Defn.Val = {
      val arrayType = MetaUtil.typeFromName(context.params.defaultTypes.array)

      q"""
      private implicit val queryOptionalCollectionCodec: Codec[List[String], Option[$arrayType[String]], TextPlain] = new Codec[List[String], Option[$arrayType[String]], TextPlain] {
        override def rawDecode(l: List[String]): DecodeResult[Option[$arrayType[String]]] = DecodeResult.Value(Some(l.to[$arrayType]))
        override def encode(h: Option[$arrayType[String]]): List[String] = h.map(_.to[List]).getOrElse(Nil)
        override lazy val schema: Schema[Option[$arrayType[String]]] = Schema.binary
        override lazy val format: TextPlain = TextPlain()
      }
    """
    }
  }

  // Scala v2.13 specific tapir definitions
  private object Scala213 extends CommonDefinitions {
    def apply(): List[Stat] =
      q"""
          $anySchema
          $eitherCodecPlain
          $queryCollectionParamEncoder
       """.stats

    private def anySchema =
      q"""
        private implicit def anySchema[T]: Schema[T] = Schema[T](
          SchemaType.SCoproduct(Nil, None)(_ => None),
          None
        )
      """

    // the query matchers were not happy with Option[List[String]] types
    // TODO: why do we need that in the first place, could be simplified with an already defined codec?
    private def queryCollectionParamEncoder: Defn.Val = {
      val arrayType = MetaUtil.typeFromName(context.params.defaultTypes.array)
      val arrayTerm = MetaUtil.termFromName(context.params.defaultTypes.array)

      q"""
      private implicit val queryOptionalCollectionCodec: Codec[List[String], Option[$arrayType[String]], TextPlain] = new Codec[List[String], Option[$arrayType[String]], TextPlain] {
        override def rawDecode(l: List[String]): DecodeResult[Option[$arrayType[String]]] = DecodeResult.Value(Some(l.to($arrayTerm)))
        override def encode(h: Option[$arrayType[String]]): List[String] = h.map(_.to(List)).getOrElse(Nil)
        override lazy val schema: Schema[Option[$arrayType[String]]] = Schema.binary
        override lazy val format: TextPlain = TextPlain()
      }
    """
    }
  }

  def apply(): List[Stat] = context.params.scalaVersion match {
    case Some((2, 12)) => Scala212()
    case _             => Scala213()
  }
}
