package scraml.libs

import java.io.File

import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scraml._

class CirceJsonSupportSpec extends AnyFlatSpec with Matchers {
  "Circe JSON Support (exact property matching)" should "generate JSON derivation" in {
    val params = ModelGenParams(
      new File("src/sbt-test/sbt-scraml/json/api/json.raml"),
      new File("target/scraml-circe-json-test"),
      "scraml",
      FieldMatchPolicy.Exact(),
      DefaultTypes(
        float = "scala.math.BigDecimal",
        double = "scala.math.BigDecimal",
        number = "scala.math.BigDecimal",
        long = "scala.math.BigInt"
      ),
      librarySupport = Set(
        CirceJsonSupport(formats = Map("localDateTime" -> "io.circe.Decoder.decodeLocalDateTime"))
      ),
      None
    )

    val generated = ModelGenRunner.run(DefaultModelGen)(params).unsafeRunSync()

    generated.files match {
      case noDiscBase ::
          _ ::
          _ ::
          baseType ::
          intermediateType ::
          grandchildType ::
          dataType ::
          emptyBase ::
          noProps ::
          noSealedBase ::
          someEnum ::
          otherSub ::
          mapLike ::
          packageObject ::
          Nil =>
        noDiscBase.source.source.toString() should be("sealed trait NoDiscriminatorBase")
        noDiscBase.source.companion.map(_.toString()) should be(
          Some(s"""object NoDiscriminatorBase {
                  |  import io.circe.Decoder.Result
                  |  import io.circe._
                  |  implicit lazy val decoder: Decoder[NoDiscriminatorBase] = new Decoder[NoDiscriminatorBase] { override def apply(c: HCursor): Result[NoDiscriminatorBase] = NoDiscriminatorSub2.decoder.tryDecode(c).fold(_ => NoDiscriminatorSub1.decoder.tryDecode(c), Right(_)) }
                  |  implicit lazy val encoder: Encoder[NoDiscriminatorBase] = new Encoder[NoDiscriminatorBase] {
                  |    override def apply(nodiscriminatorbase: NoDiscriminatorBase): Json = nodiscriminatorbase match {
                  |      case nodiscriminatorsub1: NoDiscriminatorSub1 =>
                  |        NoDiscriminatorSub1.encoder(nodiscriminatorsub1)
                  |      case nodiscriminatorsub2: NoDiscriminatorSub2 =>
                  |        NoDiscriminatorSub2.encoder(nodiscriminatorsub2)
                  |    }
                  |  }
                  |}""".stripMargin)
        )

        baseType.source.packageName should be("datatypes")
        baseType.source.source.toString() should be(
          "sealed trait BaseType extends Any { def id: String }"
        )
        baseType.source.companion.map(_.toString()) should be(Some(s"""object BaseType {
                                                                      |  import io.circe.Decoder.Result
                                                                      |  import io.circe._
                                                                      |  implicit lazy val decoder: Decoder[BaseType] = new Decoder[BaseType] {
                                                                      |    override def apply(c: HCursor): Result[BaseType] = c.downField("type").as[String] match {
                                                                      |      case Right("data") =>
                                                                      |        DataType.decoder(c)
                                                                      |      case Right("grandchild") =>
                                                                      |        GrandchildType.decoder(c)
                                                                      |      case other =>
                                                                      |        Left(DecodingFailure(s"unknown discriminator: $$other", c.history))
                                                                      |    }
                                                                      |  }
                                                                      |  implicit lazy val encoder: Encoder[BaseType] = new Encoder[BaseType] {
                                                                      |    override def apply(basetype: BaseType): Json = basetype match {
                                                                      |      case datatype: DataType =>
                                                                      |        DataType.encoder(datatype)
                                                                      |      case grandchildtype: GrandchildType =>
                                                                      |        GrandchildType.encoder(grandchildtype)
                                                                      |    }
                                                                      |  }
                                                                      |}""".stripMargin))

        baseType.source.name should be("BaseType")
        baseType.file.getPath should be("target/scraml-circe-json-test/scraml/datatypes.scala")

        dataType.source.packageName should be("datatypes")
        dataType.source.source.toString() should be(
          "final case class DataType(id: String, foo: Option[String] = None, customTypeProp: scala.math.BigDecimal, customArrayTypeProp: Vector[scala.math.BigDecimal] = Vector.empty) extends BaseType"
        )
        dataType.source.name should be("DataType")
        dataType.source.companion.map(_.toString()) should be(Some(s"""object DataType {
                                                                      |  import io.circe._
                                                                      |  import io.circe.generic.semiauto._
                                                                      |  import scraml.Formats._
                                                                      |  implicit lazy val decoder: Decoder[DataType] = deriveDecoder[DataType]
                                                                      |  implicit lazy val encoder: Encoder[DataType] = deriveEncoder[DataType].mapJsonObject(_ +: "type" -> Json.fromString("data"))
                                                                      |}""".stripMargin))

        emptyBase.source.source.toString() should be("sealed trait EmptyBase")
        emptyBase.source.companion.map(_.toString()) should be(Some(s"""object EmptyBase {
                                                                       |  import io.circe.Decoder.Result
                                                                       |  import io.circe._
                                                                       |  implicit lazy val decoder: Decoder[EmptyBase] = new Decoder[EmptyBase] {
                                                                       |    override def apply(c: HCursor): Result[EmptyBase] = c.downField("type").as[String] match {
                                                                       |      case Right("nope") =>
                                                                       |        NoProps.decoder(c)
                                                                       |      case other =>
                                                                       |        Left(DecodingFailure(s"unknown discriminator: $$other", c.history))
                                                                       |    }
                                                                       |  }
                                                                       |  implicit lazy val encoder: Encoder[EmptyBase] = new Encoder[EmptyBase] {
                                                                       |    override def apply(emptybase: EmptyBase): Json = emptybase match {
                                                                       |      case noprops: NoProps.type =>
                                                                       |        NoProps.encoder(noprops)
                                                                       |    }
                                                                       |  }
                                                                       |}""".stripMargin))

        noProps.source.source.toString() should be(
          s"""case object NoProps extends EmptyBase {
             |  import io.circe._
             |  import io.circe.generic.semiauto._
             |  import io.circe.Decoder.Result
             |  implicit lazy val decoder: Decoder[NoProps.type] = new Decoder[NoProps.type] {
             |    override def apply(c: HCursor): Result[NoProps.type] = c.downField("type").as[String] match {
             |      case Right("nope") =>
             |        Right(NoProps)
             |      case other =>
             |        Left(DecodingFailure(s"unknown type: $$other", c.history))
             |    }
             |  }
             |  implicit lazy val encoder: Encoder[NoProps.type] = new Encoder[NoProps.type] { override def apply(a: NoProps.type): Json = Json.obj("type" -> Json.fromString("nope")) }
             |}""".stripMargin
        )

        noSealedBase.source.source.toString() should be("trait NoSealedBase")
        noSealedBase.source.companion.map(_.toString()) should be(Some(s"""object NoSealedBase {
                                                                          |  import io.circe.Decoder.Result
                                                                          |  import io.circe._
                                                                          |  implicit lazy val decoder: Decoder[NoSealedBase] = new Decoder[NoSealedBase] {
                                                                          |    override def apply(c: HCursor): Result[NoSealedBase] = c.downField("typeId").as[String] match {
                                                                          |      case Right("map-like") =>
                                                                          |        MapLike.decoder(c)
                                                                          |      case Right("other-sub") =>
                                                                          |        OtherSub.decoder(c)
                                                                          |      case other =>
                                                                          |        Left(DecodingFailure(s"unknown discriminator: $$other", c.history))
                                                                          |    }
                                                                          |  }
                                                                          |  implicit lazy val encoder: Encoder[NoSealedBase] = new Encoder[NoSealedBase] {
                                                                          |    override def apply(nosealedbase: NoSealedBase): Json = nosealedbase match {
                                                                          |      case maplike: MapLike =>
                                                                          |        MapLike.encoder(maplike)
                                                                          |      case othersub: OtherSub =>
                                                                          |        OtherSub.encoder(othersub)
                                                                          |    }
                                                                          |  }
                                                                          |}""".stripMargin))
        mapLike.source.source.toString() should be(
          s"""final case class MapLike(values: scala.collection.immutable.Map[String, String]) extends NoSealedBase""".stripMargin
        )

        mapLike.source.companion.map(_.toString()) should be(Some(s"""object MapLike {
                                                                     |  import io.circe._
                                                                     |  import io.circe.syntax._
                                                                     |  import io.circe.generic.semiauto._
                                                                     |  import io.circe.Decoder.Result
                                                                     |  implicit lazy val decoder: Decoder[MapLike] = new Decoder[MapLike] { override def apply(c: HCursor): Result[MapLike] = c.as[scala.collection.immutable.Map[String, String]].map(MapLike.apply) }
                                                                     |  implicit lazy val encoder: Encoder[MapLike] = new Encoder[MapLike] { override def apply(a: MapLike): Json = a.values.asJson }
                                                                     |}""".stripMargin))

        someEnum.source.source.toString() should be(
          s"""sealed trait SomeEnum""".stripMargin
        )

        someEnum.source.companion.map(_.toString()) should be(Some(s"""object SomeEnum {
                                                                      |  case object A extends SomeEnum
                                                                      |  case object B extends SomeEnum
                                                                      |  import io.circe._
                                                                      |  implicit lazy val encoder: Encoder[SomeEnum] = Encoder[String].contramap({
                                                                      |    case A => "A"
                                                                      |    case B => "B"
                                                                      |  })
                                                                      |  implicit lazy val decoder: Decoder[SomeEnum] = Decoder[String].emap({
                                                                      |    case "A" =>
                                                                      |      Right(A)
                                                                      |    case "B" =>
                                                                      |      Right(B)
                                                                      |    case other =>
                                                                      |      Left(s"invalid enum value: $$other")
                                                                      |  })
                                                                      |}""".stripMargin))

        otherSub.source.source.toString() should be(
          """final case class OtherSub(id: String) extends NoSealedBase"""
        )

        otherSub.source.companion.map(_.toString()) should be(
          Some(
            """object OtherSub {
            |  import io.circe._
            |  import io.circe.generic.semiauto._
            |  import scraml.Formats._
            |  implicit lazy val decoder: Decoder[OtherSub] = deriveDecoder[OtherSub]
            |  implicit lazy val encoder: Encoder[OtherSub] = deriveEncoder[OtherSub].mapJsonObject(_ +: "typeId" -> Json.fromString("other-sub"))
            |}""".stripMargin
          )
        )

        packageObject.source.source.toString should be(s"""package object scraml {
             |  import io.circe.Decoder.Result
             |  import io.circe.{ HCursor, Json, Decoder, Encoder }
             |  implicit def eitherEncoder[A, B](implicit aEncoder: Encoder[A], bEncoder: Encoder[B]): Encoder[Either[A, B]] = new Encoder[Either[A, B]] {
             |    override def apply(a: Either[A, B]): Json = a match {
             |      case Right(b) =>
             |        bEncoder(b)
             |      case Left(a) =>
             |        aEncoder(a)
             |    }
             |  }
             |  implicit def eitherDecoder[A, B](implicit aDecoder: Decoder[A], bDecoder: Decoder[B]): Decoder[Either[A, B]] = new Decoder[Either[A, B]] { override def apply(c: HCursor): Result[Either[A, B]] = aDecoder.either(bDecoder)(c) }
             |  object Formats { implicit lazy val localDateTime = io.circe.Decoder.decodeLocalDateTime }
             |}""".stripMargin)

        intermediateType.source.source.toString() should be(
          "sealed trait IntermediateType extends BaseType { def id: String }"
        )
        intermediateType.source.companion.map(_.toString()) should be(
          Some(s"""object IntermediateType {
                  |  import io.circe.Decoder.Result
                  |  import io.circe._
                  |  implicit lazy val decoder: Decoder[IntermediateType] = new Decoder[IntermediateType] {
                  |    override def apply(c: HCursor): Result[IntermediateType] = c.downField("type").as[String] match {
                  |      case Right("grandchild") =>
                  |        GrandchildType.decoder(c)
                  |      case other =>
                  |        Left(DecodingFailure(s"unknown discriminator: $$other", c.history))
                  |    }
                  |  }
                  |  implicit lazy val encoder: Encoder[IntermediateType] = new Encoder[IntermediateType] {
                  |    override def apply(intermediatetype: IntermediateType): Json = intermediatetype match {
                  |      case grandchildtype: GrandchildType =>
                  |        GrandchildType.encoder(grandchildtype)
                  |    }
                  |  }
                  |}""".stripMargin)
        )

        grandchildType.source.source.toString() should be(
          "final case class GrandchildType(id: String, foo: Option[String] = None, aDouble: scala.math.BigDecimal, aFloat: scala.math.BigDecimal, anInt: Int, aLong: scala.math.BigInt, customTypeProp: scala.math.BigDecimal, customArrayTypeProp: Vector[scala.math.BigDecimal] = Vector.empty) extends IntermediateType"
        )
        grandchildType.source.companion.map(_.toString()) should be(Some(s"""object GrandchildType {
                                                                            |  import io.circe._
                                                                            |  import io.circe.generic.semiauto._
                                                                            |  import scraml.Formats._
                                                                            |  implicit lazy val decoder: Decoder[GrandchildType] = deriveDecoder[GrandchildType]
                                                                            |  implicit lazy val encoder: Encoder[GrandchildType] = deriveEncoder[GrandchildType].mapJsonObject(_ +: "type" -> Json.fromString("grandchild"))
                                                                            |}""".stripMargin))
    }
  }

  "Circe JSON Support (keep-extra property matching)" should "generate JSON derivation" in {
    val params = ModelGenParams(
      new File("src/sbt-test/sbt-scraml/json/api/json.raml"),
      new File("target/scraml-circe-json-test"),
      "scraml",
      FieldMatchPolicy.KeepExtra(),
      DefaultTypes(
        float = "scala.math.BigDecimal",
        double = "scala.math.BigDecimal",
        number = "scala.math.BigDecimal",
        long = "scala.math.BigInt"
      ),
      librarySupport = Set(
        CirceJsonSupport(formats = Map("localDateTime" -> "io.circe.Decoder.decodeLocalDateTime"))
      ),
      None
    )

    val generated = ModelGenRunner.run(DefaultModelGen)(params).unsafeRunSync()

    generated.files match {
      case noDiscBase ::
        _ ::
        _ ::
        baseType ::
        intermediateType ::
        grandchildType ::
        dataType ::
        emptyBase ::
        noProps ::
        noSealedBase ::
        someEnum ::
        otherSub ::
        mapLike ::
        packageObject ::
        Nil =>
        noDiscBase.source.source.toString() should be("sealed trait NoDiscriminatorBase")
        noDiscBase.source.companion.map(_.toString()) should be(
          Some(s"""object NoDiscriminatorBase {
                  |  import io.circe.Decoder.Result
                  |  import io.circe._
                  |  implicit lazy val decoder: Decoder[NoDiscriminatorBase] = new Decoder[NoDiscriminatorBase] { override def apply(c: HCursor): Result[NoDiscriminatorBase] = NoDiscriminatorSub2.decoder.tryDecode(c).fold(_ => NoDiscriminatorSub1.decoder.tryDecode(c), Right(_)) }
                  |  implicit lazy val encoder: Encoder[NoDiscriminatorBase] = new Encoder[NoDiscriminatorBase] {
                  |    override def apply(nodiscriminatorbase: NoDiscriminatorBase): Json = nodiscriminatorbase match {
                  |      case nodiscriminatorsub1: NoDiscriminatorSub1 =>
                  |        NoDiscriminatorSub1.encoder(nodiscriminatorsub1)
                  |      case nodiscriminatorsub2: NoDiscriminatorSub2 =>
                  |        NoDiscriminatorSub2.encoder(nodiscriminatorsub2)
                  |    }
                  |  }
                  |}""".stripMargin)
        )

        baseType.source.packageName should be("datatypes")
        baseType.source.source.toString() should be(
          "sealed trait BaseType extends Any { def id: String }"
        )
        baseType.source.companion.map(_.toString()) should be(Some(s"""object BaseType {
                                                                      |  import io.circe.Decoder.Result
                                                                      |  import io.circe._
                                                                      |  implicit lazy val decoder: Decoder[BaseType] = new Decoder[BaseType] {
                                                                      |    override def apply(c: HCursor): Result[BaseType] = c.downField("type").as[String] match {
                                                                      |      case Right("data") =>
                                                                      |        DataType.decoder(c)
                                                                      |      case Right("grandchild") =>
                                                                      |        GrandchildType.decoder(c)
                                                                      |      case other =>
                                                                      |        Left(DecodingFailure(s"unknown discriminator: $$other", c.history))
                                                                      |    }
                                                                      |  }
                                                                      |  implicit lazy val encoder: Encoder[BaseType] = new Encoder[BaseType] {
                                                                      |    override def apply(basetype: BaseType): Json = basetype match {
                                                                      |      case datatype: DataType =>
                                                                      |        DataType.encoder(datatype)
                                                                      |      case grandchildtype: GrandchildType =>
                                                                      |        GrandchildType.encoder(grandchildtype)
                                                                      |    }
                                                                      |  }
                                                                      |}""".stripMargin))

        baseType.source.name should be("BaseType")
        baseType.file.getPath should be("target/scraml-circe-json-test/scraml/datatypes.scala")

        dataType.source.packageName should be("datatypes")
        dataType.source.source.toString() should be(
          "final case class DataType(id: String, foo: Option[String] = None, customTypeProp: scala.math.BigDecimal, customArrayTypeProp: Vector[scala.math.BigDecimal] = Vector.empty)(val additionalProperties: Option[DataType.AdditionalProperties] = None) extends BaseType"
        )
        dataType.source.name should be("DataType")
        dataType.source.companion.map(_.toString()) should be(Some(s"""object DataType {
                                                                      |  import scala.language.dynamics
                                                                      |  final case class AdditionalProperties(private val underlying: scala.collection.immutable.Map[String, io.circe.Json]) extends scala.Dynamic {
                                                                      |    override def toString(): String = underlying.mkString(", ")
                                                                      |    def selectDynamic(field: String): Option[io.circe.Json] = underlying.get(field)
                                                                      |    def getOrElse[V >: io.circe.Json](key: String, default: => V): V = underlying.getOrElse(key, default)
                                                                      |    def isDefinedAt(key: String): Boolean = underlying.isDefinedAt(key)
                                                                      |    def isEmpty: Boolean = underlying.isEmpty
                                                                      |    def keySet: Set[String] = underlying.keySet
                                                                      |    def keys: Iterable[String] = underlying.keys
                                                                      |    def keysIterator: Iterator[String] = underlying.keysIterator
                                                                      |    def nonEmpty: Boolean = !underlying.isEmpty
                                                                      |    def size: Int = underlying.size
                                                                      |    def values: Iterable[io.circe.Json] = underlying.values
                                                                      |    def valuesIterator: Iterator[io.circe.Json] = underlying.valuesIterator
                                                                      |  }
                                                                      |  object AdditionalProperties {
                                                                      |    import scala.util.matching.Regex
                                                                      |    val propertyNames: Seq[String] = Seq("id", "foo", "customTypeProp", "customArrayTypeProp")
                                                                      |    val allowedNames: Seq[Regex] = Seq()
                                                                      |    import io.circe._
                                                                      |    import io.circe.generic.semiauto._
                                                                      |    implicit lazy val decoder: Decoder[AdditionalProperties] = new Decoder[AdditionalProperties] {
                                                                      |      final def apply(c: HCursor): Decoder.Result[AdditionalProperties] = {
                                                                      |        val parent = c.up
                                                                      |        val allKeys = parent.keys.fold(Set.empty[String])(_.toSet)
                                                                      |        val builder = scala.collection.immutable.Map.newBuilder[String, Json]
                                                                      |        val it = allKeys.filterNot(propertyNames.contains).iterator
                                                                      |        val entries = it.foldLeft(builder)({
                                                                      |          case (accum, key) =>
                                                                      |            parent.field(key).focus.fold(accum) {
                                                                      |              v => accum += key -> v
                                                                      |            }
                                                                      |        })
                                                                      |        Right(AdditionalProperties(builder.result()))
                                                                      |      }
                                                                      |    }
                                                                      |    def addFields(into: Json, ap: AdditionalProperties): Json = {
                                                                      |      Json.fromFields(ap.underlying).deepMerge(into)
                                                                      |    }
                                                                      |  }
                                                                      |  import io.circe._
                                                                      |  import io.circe.generic.semiauto._
                                                                      |  import scraml.Formats._
                                                                      |  implicit lazy val decoder: Decoder[DataType] = deriveDecoder[DataType]
                                                                      |  implicit lazy val encoder: Encoder[DataType] = new Encoder[DataType] { final def apply(instance: DataType): Json = AdditionalProperties.addFields(Json.obj("type" -> Json.fromString("data"), "id" -> Encoder[String].apply(instance.id), "foo" -> Encoder[Option[String]].apply(instance.foo), "customTypeProp" -> Encoder[scala.math.BigDecimal].apply(instance.customTypeProp), "customArrayTypeProp" -> Encoder[Vector[scala.math.BigDecimal]].apply(instance.customArrayTypeProp)), instance.additionalProperties) }
                                                                      |}""".stripMargin))

        emptyBase.source.source.toString() should be("sealed trait EmptyBase")
        emptyBase.source.companion.map(_.toString()) should be(Some(s"""object EmptyBase {
                                                                       |  import io.circe.Decoder.Result
                                                                       |  import io.circe._
                                                                       |  implicit lazy val decoder: Decoder[EmptyBase] = new Decoder[EmptyBase] {
                                                                       |    override def apply(c: HCursor): Result[EmptyBase] = c.downField("type").as[String] match {
                                                                       |      case Right("nope") =>
                                                                       |        NoProps.decoder(c)
                                                                       |      case other =>
                                                                       |        Left(DecodingFailure(s"unknown discriminator: $$other", c.history))
                                                                       |    }
                                                                       |  }
                                                                       |  implicit lazy val encoder: Encoder[EmptyBase] = new Encoder[EmptyBase] {
                                                                       |    override def apply(emptybase: EmptyBase): Json = emptybase match {
                                                                       |      case noprops: NoProps =>
                                                                       |        NoProps.encoder(noprops)
                                                                       |    }
                                                                       |  }
                                                                       |}""".stripMargin))

        noProps.source.source.toString() should be(
          """final case class NoProps()(val additionalProperties: Option[NoProps.AdditionalProperties] = None) extends EmptyBase"""
        )

        noProps.source.companion.map(_.toString()) should be(
          Some(
            """object NoProps {
              |  import scala.language.dynamics
              |  final case class AdditionalProperties(private val underlying: scala.collection.immutable.Map[String, io.circe.Json]) extends scala.Dynamic {
              |    override def toString(): String = underlying.mkString(", ")
              |    def selectDynamic(field: String): Option[io.circe.Json] = underlying.get(field)
              |    def getOrElse[V >: io.circe.Json](key: String, default: => V): V = underlying.getOrElse(key, default)
              |    def isDefinedAt(key: String): Boolean = underlying.isDefinedAt(key)
              |    def isEmpty: Boolean = underlying.isEmpty
              |    def keySet: Set[String] = underlying.keySet
              |    def keys: Iterable[String] = underlying.keys
              |    def keysIterator: Iterator[String] = underlying.keysIterator
              |    def nonEmpty: Boolean = !underlying.isEmpty
              |    def size: Int = underlying.size
              |    def values: Iterable[io.circe.Json] = underlying.values
              |    def valuesIterator: Iterator[io.circe.Json] = underlying.valuesIterator
              |  }
              |  object AdditionalProperties {
              |    import scala.util.matching.Regex
              |    val propertyNames: Seq[String] = Seq()
              |    val allowedNames: Seq[Regex] = Seq()
              |    import io.circe._
              |    import io.circe.generic.semiauto._
              |    implicit lazy val decoder: Decoder[AdditionalProperties] = new Decoder[AdditionalProperties] {
              |      final def apply(c: HCursor): Decoder.Result[AdditionalProperties] = {
              |        val parent = c.up
              |        val allKeys = parent.keys.fold(Set.empty[String])(_.toSet)
              |        val builder = scala.collection.immutable.Map.newBuilder[String, Json]
              |        val it = allKeys.filterNot(propertyNames.contains).iterator
              |        val entries = it.foldLeft(builder)({
              |          case (accum, key) =>
              |            parent.field(key).focus.fold(accum) {
              |              v => accum += key -> v
              |            }
              |        })
              |        Right(AdditionalProperties(builder.result()))
              |      }
              |    }
              |    def addFields(into: Json, ap: AdditionalProperties): Json = {
              |      Json.fromFields(ap.underlying).deepMerge(into)
              |    }
              |  }
              |  import io.circe._
              |  import io.circe.generic.semiauto._
              |  import scraml.Formats._
              |  implicit lazy val decoder: Decoder[NoProps] = deriveDecoder[NoProps]
              |  implicit lazy val encoder: Encoder[NoProps] = new Encoder[NoProps] { final def apply(instance: NoProps): Json = AdditionalProperties.addFields(Json.obj("type" -> Json.fromString("nope")), instance.additionalProperties) }
              |}""".stripMargin
          )
        )

        noSealedBase.source.source.toString() should be("trait NoSealedBase")
        noSealedBase.source.companion.map(_.toString()) should be(Some(s"""object NoSealedBase {
                                                                          |  import io.circe.Decoder.Result
                                                                          |  import io.circe._
                                                                          |  implicit lazy val decoder: Decoder[NoSealedBase] = new Decoder[NoSealedBase] {
                                                                          |    override def apply(c: HCursor): Result[NoSealedBase] = c.downField("typeId").as[String] match {
                                                                          |      case Right("map-like") =>
                                                                          |        MapLike.decoder(c)
                                                                          |      case Right("other-sub") =>
                                                                          |        OtherSub.decoder(c)
                                                                          |      case other =>
                                                                          |        Left(DecodingFailure(s"unknown discriminator: $$other", c.history))
                                                                          |    }
                                                                          |  }
                                                                          |  implicit lazy val encoder: Encoder[NoSealedBase] = new Encoder[NoSealedBase] {
                                                                          |    override def apply(nosealedbase: NoSealedBase): Json = nosealedbase match {
                                                                          |      case maplike: MapLike =>
                                                                          |        MapLike.encoder(maplike)
                                                                          |      case othersub: OtherSub =>
                                                                          |        OtherSub.encoder(othersub)
                                                                          |    }
                                                                          |  }
                                                                          |}""".stripMargin))
        mapLike.source.source.toString() should be(
          s"""final case class MapLike(values: scala.collection.immutable.Map[String, String]) extends NoSealedBase""".stripMargin
        )

        mapLike.source.companion.map(_.toString()) should be(Some(s"""object MapLike {
                                                                     |  import io.circe._
                                                                     |  import io.circe.syntax._
                                                                     |  import io.circe.generic.semiauto._
                                                                     |  import io.circe.Decoder.Result
                                                                     |  implicit lazy val decoder: Decoder[MapLike] = new Decoder[MapLike] { override def apply(c: HCursor): Result[MapLike] = c.as[scala.collection.immutable.Map[String, String]].map(MapLike.apply) }
                                                                     |  implicit lazy val encoder: Encoder[MapLike] = new Encoder[MapLike] { override def apply(a: MapLike): Json = a.values.asJson }
                                                                     |}""".stripMargin))

        someEnum.source.source.toString() should be(
          s"""sealed trait SomeEnum""".stripMargin
        )

        someEnum.source.companion.map(_.toString()) should be(Some(s"""object SomeEnum {
                                                                      |  case object A extends SomeEnum
                                                                      |  case object B extends SomeEnum
                                                                      |  import io.circe._
                                                                      |  implicit lazy val encoder: Encoder[SomeEnum] = Encoder[String].contramap({
                                                                      |    case A => "A"
                                                                      |    case B => "B"
                                                                      |  })
                                                                      |  implicit lazy val decoder: Decoder[SomeEnum] = Decoder[String].emap({
                                                                      |    case "A" =>
                                                                      |      Right(A)
                                                                      |    case "B" =>
                                                                      |      Right(B)
                                                                      |    case other =>
                                                                      |      Left(s"invalid enum value: $$other")
                                                                      |  })
                                                                      |}""".stripMargin))

        otherSub.source.source.toString() should be(
          """final case class OtherSub(id: String)(val additionalProperties: Option[OtherSub.AdditionalProperties] = None) extends NoSealedBase"""
        )

        otherSub.source.companion.map(_.toString()) should be(
          Some(
            """object OtherSub {
              |  import scala.language.dynamics
              |  final case class AdditionalProperties(private val underlying: scala.collection.immutable.Map[String, io.circe.Json]) extends scala.Dynamic {
              |    override def toString(): String = underlying.mkString(", ")
              |    def selectDynamic(field: String): Option[io.circe.Json] = underlying.get(field)
              |    def getOrElse[V >: io.circe.Json](key: String, default: => V): V = underlying.getOrElse(key, default)
              |    def isDefinedAt(key: String): Boolean = underlying.isDefinedAt(key)
              |    def isEmpty: Boolean = underlying.isEmpty
              |    def keySet: Set[String] = underlying.keySet
              |    def keys: Iterable[String] = underlying.keys
              |    def keysIterator: Iterator[String] = underlying.keysIterator
              |    def nonEmpty: Boolean = !underlying.isEmpty
              |    def size: Int = underlying.size
              |    def values: Iterable[io.circe.Json] = underlying.values
              |    def valuesIterator: Iterator[io.circe.Json] = underlying.valuesIterator
              |  }
              |  object AdditionalProperties {
              |    import scala.util.matching.Regex
              |    val propertyNames: Seq[String] = Seq("id")
              |    val allowedNames: Seq[Regex] = Seq()
              |    import io.circe._
              |    import io.circe.generic.semiauto._
              |    implicit lazy val decoder: Decoder[AdditionalProperties] = new Decoder[AdditionalProperties] {
              |      final def apply(c: HCursor): Decoder.Result[AdditionalProperties] = {
              |        val parent = c.up
              |        val allKeys = parent.keys.fold(Set.empty[String])(_.toSet)
              |        val builder = scala.collection.immutable.Map.newBuilder[String, Json]
              |        val it = allKeys.filterNot(propertyNames.contains).iterator
              |        val entries = it.foldLeft(builder)({
              |          case (accum, key) =>
              |            parent.field(key).focus.fold(accum) {
              |              v => accum += key -> v
              |            }
              |        })
              |        Right(AdditionalProperties(builder.result()))
              |      }
              |    }
              |    def addFields(into: Json, ap: AdditionalProperties): Json = {
              |      Json.fromFields(ap.underlying).deepMerge(into)
              |    }
              |  }
              |  import io.circe._
              |  import io.circe.generic.semiauto._
              |  import scraml.Formats._
              |  implicit lazy val decoder: Decoder[OtherSub] = deriveDecoder[OtherSub]
              |  implicit lazy val encoder: Encoder[OtherSub] = new Encoder[OtherSub] { final def apply(instance: OtherSub): Json = AdditionalProperties.addFields(Json.obj("typeId" -> Json.fromString("other-sub"), "id" -> Encoder[String].apply(instance.id)), instance.additionalProperties) }
              |}""".stripMargin
          )
        )

        packageObject.source.source.toString should be(s"""package object scraml {
                                                          |  import io.circe.Decoder.Result
                                                          |  import io.circe.{ HCursor, Json, Decoder, Encoder }
                                                          |  implicit def eitherEncoder[A, B](implicit aEncoder: Encoder[A], bEncoder: Encoder[B]): Encoder[Either[A, B]] = new Encoder[Either[A, B]] {
                                                          |    override def apply(a: Either[A, B]): Json = a match {
                                                          |      case Right(b) =>
                                                          |        bEncoder(b)
                                                          |      case Left(a) =>
                                                          |        aEncoder(a)
                                                          |    }
                                                          |  }
                                                          |  implicit def eitherDecoder[A, B](implicit aDecoder: Decoder[A], bDecoder: Decoder[B]): Decoder[Either[A, B]] = new Decoder[Either[A, B]] { override def apply(c: HCursor): Result[Either[A, B]] = aDecoder.either(bDecoder)(c) }
                                                          |  object Formats { implicit lazy val localDateTime = io.circe.Decoder.decodeLocalDateTime }
                                                          |}""".stripMargin)

        intermediateType.source.source.toString() should be(
          "sealed trait IntermediateType extends BaseType { def id: String }"
        )
        intermediateType.source.companion.map(_.toString()) should be(
          Some(s"""object IntermediateType {
                  |  import io.circe.Decoder.Result
                  |  import io.circe._
                  |  implicit lazy val decoder: Decoder[IntermediateType] = new Decoder[IntermediateType] {
                  |    override def apply(c: HCursor): Result[IntermediateType] = c.downField("type").as[String] match {
                  |      case Right("grandchild") =>
                  |        GrandchildType.decoder(c)
                  |      case other =>
                  |        Left(DecodingFailure(s"unknown discriminator: $$other", c.history))
                  |    }
                  |  }
                  |  implicit lazy val encoder: Encoder[IntermediateType] = new Encoder[IntermediateType] {
                  |    override def apply(intermediatetype: IntermediateType): Json = intermediatetype match {
                  |      case grandchildtype: GrandchildType =>
                  |        GrandchildType.encoder(grandchildtype)
                  |    }
                  |  }
                  |}""".stripMargin)
        )

        grandchildType.source.source.toString() should be(
          "final case class GrandchildType(id: String, foo: Option[String] = None, aDouble: scala.math.BigDecimal, aFloat: scala.math.BigDecimal, anInt: Int, aLong: scala.math.BigInt, customTypeProp: scala.math.BigDecimal, customArrayTypeProp: Vector[scala.math.BigDecimal] = Vector.empty)(val additionalProperties: Option[GrandchildType.AdditionalProperties] = None) extends IntermediateType"
        )
        grandchildType.source.companion.map(_.toString()) should be(Some(s"""object GrandchildType {
                                                                            |  import scala.language.dynamics
                                                                            |  final case class AdditionalProperties(private val underlying: scala.collection.immutable.Map[String, io.circe.Json]) extends scala.Dynamic {
                                                                            |    override def toString(): String = underlying.mkString(", ")
                                                                            |    def selectDynamic(field: String): Option[io.circe.Json] = underlying.get(field)
                                                                            |    def getOrElse[V >: io.circe.Json](key: String, default: => V): V = underlying.getOrElse(key, default)
                                                                            |    def isDefinedAt(key: String): Boolean = underlying.isDefinedAt(key)
                                                                            |    def isEmpty: Boolean = underlying.isEmpty
                                                                            |    def keySet: Set[String] = underlying.keySet
                                                                            |    def keys: Iterable[String] = underlying.keys
                                                                            |    def keysIterator: Iterator[String] = underlying.keysIterator
                                                                            |    def nonEmpty: Boolean = !underlying.isEmpty
                                                                            |    def size: Int = underlying.size
                                                                            |    def values: Iterable[io.circe.Json] = underlying.values
                                                                            |    def valuesIterator: Iterator[io.circe.Json] = underlying.valuesIterator
                                                                            |  }
                                                                            |  object AdditionalProperties {
                                                                            |    import scala.util.matching.Regex
                                                                            |    val propertyNames: Seq[String] = Seq("id", "foo", "aDouble", "aFloat", "anInt", "aLong", "customTypeProp", "customArrayTypeProp")
                                                                            |    val allowedNames: Seq[Regex] = Seq()
                                                                            |    import io.circe._
                                                                            |    import io.circe.generic.semiauto._
                                                                            |    implicit lazy val decoder: Decoder[AdditionalProperties] = deriveDecoder[AdditionalProperties]
                                                                            |    implicit lazy val encoder: Encoder[AdditionalProperties] = deriveEncoder[AdditionalProperties]
                                                                            |  }
                                                                            |  import io.circe._
                                                                            |  import io.circe.generic.semiauto._
                                                                            |  import scraml.Formats._
                                                                            |  implicit lazy val decoder: Decoder[GrandchildType] = deriveDecoder[GrandchildType]
                                                                            |  implicit lazy val encoder: Encoder[GrandchildType] = deriveEncoder[GrandchildType].mapJsonObject(_.add("type", Json.fromString("grandchild")))
                                                                            |}""".stripMargin))
    }
  }
}
