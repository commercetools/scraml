package scraml.libs

import scala.meta._

import _root_.io.vrap.rmf.raml.model.types._
import _root_.io.vrap.rmf.raml.model.values.RegExp
import scraml.{DefnWithCompanion, LibrarySupport, MetaUtil, ModelGenContext}

object RefinedSupport extends LibrarySupport {
  import LibrarySupport.appendObjectStats

  sealed trait TypeNaming {
    protected def mkDeclType(objectType: ObjectType, propertyName: String): Type =
      Type.Select(
        Term.Name(objectType.getName),
        mkTypeName(propertyName)
      )

    protected def mkItemTypeName(propertyName: String): Type.Name =
      Type.Name(propertyName.capitalize + "ItemPredicate")

    protected def mkTypeName(propertyName: String): Type.Name =
      Type.Name(propertyName.capitalize + "Type")
  }

  sealed abstract class RefinedPropertyMatching[A] extends TypeNaming {
    final def hasFacets(at: ArrayType): Boolean =
      (at.getMaxItems ne null) ||
        (at.getMinItems ne null) ||
        (at.getUniqueItems ne null) ||
        hasItemFacets(at)

    final def hasFacets(nt: NumberType): Boolean =
      (nt.getMaximum ne null) ||
        (nt.getMinimum ne null)

    final def hasFacets(st: StringType): Boolean =
      (st.getPattern ne null) ||
        (st.getMaxLength ne null) ||
        (st.getMinLength ne null)

    final def hasItemFacets(at: ArrayType): Boolean =
      Option(at.getItems).exists {
        case nt: NumberType if hasFacets(nt) => true
        case st: StringType if hasFacets(st) => true
        case _                               => false
      }

    final def unapply(declaration: Decl.Def)(implicit
        context: ModelGenContext
    ): Option[A] = {
      val property = Option(context.objectType.getProperty(declaration.name.value))

      dispatch(
        declaration.name,
        property.map(_.getType),
        property.fold(true)(_.getRequired)
      )
    }

    final def unapply(param: Term.Param)(implicit
        context: ModelGenContext
    ): Option[A] = {
      val property = Option(context.objectType.getProperty(param.name.value))

      dispatch(
        param.name,
        property.map(_.getType),
        property.fold(true)(_.getRequired)
      )
    }

    protected def array(name: Name, descriptor: ArrayType, optional: Boolean)(implicit
        context: ModelGenContext
    ): Option[A] = None

    protected def number(name: Name, descriptor: NumberType, optional: Boolean)(implicit
        context: ModelGenContext
    ): Option[A] = None

    protected def string(name: Name, descriptor: StringType, optional: Boolean)(implicit
        context: ModelGenContext
    ): Option[A] = None

    protected def numberBounds(
        min: Option[BigDecimal],
        max: Option[BigDecimal]
    ): List[Type.Apply] = {
      (min, max) match {
        case (Some(lower), Some(upper)) if upper < lower =>
          throw new RuntimeException("invalid min/max number bounds detected")
        case (Some(lower), Some(upper)) =>
          predicateType("Interval.Closed", lower, upper) :: Nil
        case (Some(lower), None) =>
          predicateType("GreaterEqual", lower) :: Nil
        case (None, Some(upper)) =>
          predicateType("LessEqual", upper) :: Nil
        case _ =>
          Nil
      }
    }

    protected def collectionBounds(min: Option[Integer], max: Option[Integer]): List[Type.Apply] = {
      (min, max) match {
        case (Some(lower), Some(upper)) if upper < lower =>
          throw new RuntimeException("invalid min/max string bounds detected")
        case (Some(lower), Some(upper)) =>
          predicateType("MinSize", lower) ::
            predicateType("MaxSize", upper) ::
            Nil
        case (None, Some(upper)) =>
          predicateType("MaxSize", upper) :: Nil
        case (Some(lower), None) =>
          predicateType("MinSize", lower) :: Nil
        case _ =>
          Nil
      }
    }

    protected def pattern(regex: Option[RegExp]): List[Type.Apply] =
      regex.toList.map { re =>
        predicateType("MatchesRegex", "\"" + re.toString + "\"")
      }

    protected def predicateType(name: String, constants: AnyRef*): Type.Apply =
      Type.Apply(
        MetaUtil.typeFromName(name),
        constants.toList.map { constant =>
          Type.Select(
            Term.Select(
              Term.Name("Witness"),
              Term.Name(constant.toString)
            ),
            Type.Name("T")
          )
        }
      )

    private def dispatch(name: Name, propertyType: Option[AnyType], required: Boolean)(implicit
        context: ModelGenContext
    ): Option[A] = {
      propertyType match {
        case Some(at: ArrayType) if hasFacets(at) =>
          array(name, at, !required)
        case Some(nt: NumberType) if hasFacets(nt) =>
          number(name, nt, !required)
        case Some(st: StringType) if hasFacets(st) =>
          string(name, st, !required)
        case _ =>
          None
      }
    }
  }

  object RefinedPropertyDeclaration extends RefinedPropertyMatching[(Type, Option[Term])] {
    override protected def array(name: Name, descriptor: ArrayType, optional: Boolean)(implicit
        context: ModelGenContext
    ): Option[(Type, Option[Term])] = {
      Some(
        mkDeclType(context.objectType, name.value) -> defaultValue(optional) {
          q"None"
        }
      )
    }

    override protected def number(name: Name, descriptor: NumberType, optional: Boolean)(implicit
        context: ModelGenContext
    ): Option[(Type, Option[Term])] =
      Some(
        mkDeclType(context.objectType, name.value) -> defaultValue(optional) {
          q"None"
        }
      )

    override protected def string(name: Name, descriptor: StringType, optional: Boolean)(implicit
        context: ModelGenContext
    ): Option[(Type, Option[Term])] =
      Some(
        mkDeclType(context.objectType, name.value) -> defaultValue(optional) {
          q"None"
        }
      )

    private def defaultValue(optional: Boolean)(
        value: => Term
    ): Option[Term] =
      if (optional)
        Some(value)
      else
        None
  }

  object RefinedPropertyItemPredicates extends RefinedPropertyMatching[List[Type.Apply]] {
    def apply(declaration: Decl.Def)(implicit
        context: ModelGenContext
    ): Option[List[Type.Apply]] =
      unapply(declaration)

    def apply(param: Term.Param)(implicit
        context: ModelGenContext
    ): Option[List[Type.Apply]] =
      unapply(param)

    override protected def array(name: Name, descriptor: ArrayType, optional: Boolean)(implicit
        context: ModelGenContext
    ): Option[List[Type.Apply]] = {
      Option(descriptor.getItems) match {
        case Some(nt: NumberType) if hasFacets(nt) =>
          Some(
            numberBounds(
              Option(nt.getMinimum).map(BigDecimal(_)),
              Option(nt.getMaximum).map(BigDecimal(_))
            )
          )

        case Some(st: StringType) if hasFacets(st) =>
          Some(
            collectionBounds(Option(st.getMinLength), Option(st.getMaxLength)) :::
              pattern(Option(st.getPattern))
          )

        case _ =>
          None
      }
    }
  }

  object RefinedPropertyPredicates extends RefinedPropertyMatching[List[Type.Apply]] {
    override protected def array(name: Name, descriptor: ArrayType, optional: Boolean)(implicit
        context: ModelGenContext
    ): Option[List[Type.Apply]] = {
      Option(descriptor.getItems) match {
        case Some(nt: NumberType) if hasFacets(nt) =>
          Some(
            collectionBounds(Option(descriptor.getMinItems), Option(descriptor.getMaxItems)) :::
              forall(mkItemTypeName(name.value))
          )

        case Some(st: StringType) if hasFacets(st) =>
          Some(
            collectionBounds(Option(descriptor.getMinItems), Option(descriptor.getMaxItems)) :::
              forall(mkItemTypeName(name.value))
          )

        case _ =>
          Some(
            collectionBounds(Option(descriptor.getMinItems), Option(descriptor.getMaxItems))
          )
      }
    }

    override protected def number(name: Name, descriptor: NumberType, optional: Boolean)(implicit
        context: ModelGenContext
    ): Option[List[Type.Apply]] = {
      Some(
        numberBounds(
          Option(descriptor.getMinimum).map(BigDecimal(_)),
          Option(descriptor.getMaximum).map(BigDecimal(_))
        )
      )
    }

    override protected def string(name: Name, descriptor: StringType, optional: Boolean)(implicit
        context: ModelGenContext
    ): Option[List[Type.Apply]] = {
      Some(
        collectionBounds(Option(descriptor.getMinLength), Option(descriptor.getMaxLength)) :::
          pattern(Option(descriptor.getPattern))
      )
    }

    private def forall(itemPredicateName: Name): List[Type.Apply] = {
      Type.Apply(
        Type.Name("Forall"),
        Type.Name(itemPredicateName.value) :: Nil
      ) :: Nil
    }
  }

  object RefinedPropertyType
      extends RefinedPropertyMatching[(Type.Name, Option[Type.Name], Boolean)] {
    override protected def array(name: Name, descriptor: ArrayType, optional: Boolean)(implicit
        context: ModelGenContext
    ): Option[(Type.Name, Option[Type.Name], Boolean)] = {
      Option(descriptor.getItems) match {
        case Some(nt: NumberType) if hasFacets(nt) =>
          Some((mkTypeName(name.value), Some(mkItemTypeName(name.value)), optional))

        case Some(st: StringType) if hasFacets(st) =>
          Some((mkTypeName(name.value), Some(mkItemTypeName(name.value)), optional))

        case _ =>
          Some((mkTypeName(name.value), None, optional))
      }
    }

    override protected def number(name: Name, descriptor: NumberType, optional: Boolean)(implicit
        context: ModelGenContext
    ): Option[(Type.Name, Option[Type.Name], Boolean)] =
      Some((mkTypeName(name.value), None, optional))

    override protected def string(name: Name, descriptor: StringType, optional: Boolean)(implicit
        context: ModelGenContext
    ): Option[(Type.Name, Option[Type.Name], Boolean)] =
      Some((mkTypeName(name.value), None, optional))
  }

  override val order: Double = 0.1

  override def modifyClass(classDef: Defn.Class, companion: Option[Defn.Object])(implicit
      context: ModelGenContext
  ): DefnWithCompanion[Defn.Class] =
    (classDef, companion) match {
      case (HasAnyProperties(), Some(c)) =>
        DefnWithCompanion(
          declareRefinements(classDef)(context),
          Some(defineRefinements(classDef, c)(context))
        )
      case _ =>
        super.modifyClass(classDef, companion)(context)
    }

  override def modifyTrait(traitDef: Defn.Trait, companion: Option[Defn.Object])(implicit
      context: ModelGenContext
  ): DefnWithCompanion[Defn.Trait] =
    (traitDef, companion) match {
      case (HasAnyProperties(), Some(c)) =>
        DefnWithCompanion(
          declareRefinements(traitDef)(context),
          Some(defineRefinements(traitDef, c)(context))
        )
      case _ =>
        super.modifyTrait(traitDef, companion)(context)
    }

  private def declareRefinements(classDef: Defn.Class)(implicit
      context: ModelGenContext
  ): Defn.Class = {
    val declarations = classDef.ctor.paramss match {
      case primary :: extra =>
        primary.map {
          case prop @ RefinedPropertyDeclaration((typeName, default)) =>
            prop.copy(decltpe = Some(typeName), default = default)
          case unrefined =>
            unrefined
        } :: extra
      case other =>
        other
    }

    classDef.copy(ctor = classDef.ctor.copy(paramss = declarations))
  }

  private def declareRefinements(traitDef: Defn.Trait)(implicit
      context: ModelGenContext
  ): Defn.Trait = {
    val adapted = generatePropertiesCode(traitDef) {
      case prop @ RefinedPropertyDeclaration((typeName, _)) =>
        List(prop.copy(decltpe = typeName))
      case other =>
        List(other)
    }

    traitDef.copy(templ = traitDef.templ.copy(stats = adapted))
  }

  private def defineRefinements(classDef: Defn.Class, companion: Defn.Object)(implicit
      context: ModelGenContext
  ): Defn.Object = {
    val types = generatePropertiesCode(classDef) {
      // Properties which have regular expressions as their name cannot
      // be resolved, so are skipped.
      case prop if context.objectType.getProperty(prop.name.value) ne null =>
        val propDef = context.objectType.getProperty(prop.name.value)
        val scalaTypeAnnotation = Option(
          propDef.getAnnotation("scala-type")
        ).map(_.getValue.getValue.toString)

        val originalType = context
          .scalaTypeRef(
            propDef.getType,
            optional = false,
            typeName = scalaTypeAnnotation,
            defaultAnyTypeName = context.anyTypeName
          )
          .get
          .scalaType

        prop match {
          case RefinedPropertyType(typeName, Some(itemName), optional) if optional =>
            val itemPredicates = RefinedPropertyItemPredicates(prop) match {
              case Some(one :: Nil) =>
                q"type $itemName = $one" :: Nil

              case Some(head :: tail) =>
                q"type $itemName = ${reduce(head, tail)}" :: Nil

              case _ =>
                Nil
            }

            itemPredicates ++ List[Stat](
              q"""
                type $typeName = Option[Refined[$originalType,${predicates(prop)}]]
                """,
              refinedTypeObject(typeName, originalType, predicates(prop), true)
            )

          case RefinedPropertyType(typeName, Some(itemName), _) =>
            val itemPredicates = RefinedPropertyItemPredicates(prop) match {
              case Some(one :: Nil) =>
                q"type $itemName = $one" :: Nil

              case Some(head :: tail) =>
                q"type $itemName = ${reduce(head, tail)}" :: Nil

              case _ =>
                Nil
            }

            itemPredicates ++ List[Stat](
              q"type $typeName = Refined[$originalType,${predicates(prop)}]",
              refinedTypeObject(typeName, originalType, predicates(prop), false)
            )

          case RefinedPropertyType(typeName, None, optional) if optional =>
            List[Stat](
              q"""
                type $typeName = Option[Refined[$originalType,${predicates(prop)}]]
                """,
              refinedTypeObject(typeName, originalType, predicates(prop), true)
            )

          case RefinedPropertyType(typeName, None, _) =>
            List[Stat](
              q"type $typeName = Refined[$originalType,${predicates(prop)}]",
              refinedTypeObject(typeName, originalType, predicates(prop), false)
            )

          case _ =>
            List.empty[Stat]
        }

      case _ =>
        List.empty[Stat]
    }

    appendObjectStats(companion, preface ++ types)
  }

  private def defineRefinements(traitDef: Defn.Trait, companion: Defn.Object)(implicit
      context: ModelGenContext
  ): Defn.Object = {
    val types = generatePropertiesCode(traitDef) {
      // Properties which have regular expressions as their name cannot
      // be resolved, so are skipped.
      case prop if context.objectType.getProperty(prop.name.value) ne null =>
        val propDef = context.objectType.getProperty(prop.name.value)
        val scalaTypeAnnotation = Option(
          propDef.getAnnotation("scala-type")
        ).map(_.getValue.getValue.toString)

        val originalType = context
          .scalaTypeRef(
            propDef.getType,
            optional = false,
            typeName = scalaTypeAnnotation,
            defaultAnyTypeName = context.anyTypeName
          )
          .get
          .scalaType

        prop match {
          case RefinedPropertyType(typeName, Some(itemName), optional) if optional =>
            val itemPredicates = RefinedPropertyItemPredicates(prop) match {
              case Some(one :: Nil) =>
                q"type $itemName = $one" :: Nil

              case Some(head :: tail) =>
                q"type $itemName = ${reduce(head, tail)}" :: Nil

              case _ =>
                Nil
            }

            itemPredicates ++ List[Stat](
              q"""
                type $typeName = Option[Refined[$originalType,${predicates(prop)}]]
                """
            )

          case RefinedPropertyType(typeName, Some(itemName), _) =>
            val itemPredicates = RefinedPropertyItemPredicates(prop) match {
              case Some(one :: Nil) =>
                q"type $itemName = $one" :: Nil

              case Some(head :: tail) =>
                q"type $itemName = ${reduce(head, tail)}" :: Nil

              case _ =>
                Nil
            }

            itemPredicates ++ List[Stat](
              q"type $typeName = Refined[$originalType,${predicates(prop)}]"
            )

          case RefinedPropertyType(typeName, _, optional) if optional =>
            List[Stat](
              q"""
                type $typeName = Option[Refined[$originalType,${predicates(prop)}]]
                """
            )

          case RefinedPropertyType(typeName, _, _) =>
            List[Stat](
              q"type $typeName = Refined[$originalType,${predicates(prop)}]"
            )

          case _ =>
            List.empty[Stat]
        }

      case _ =>
        List.empty[Stat]
    }

    appendObjectStats(companion, preface ++ types)
  }

  private def predicates(prop: Decl.Def)(implicit
      context: ModelGenContext
  ): Type = {
    prop match {
      case RefinedPropertyPredicates(one :: Nil) =>
        one
      case RefinedPropertyPredicates(head :: tail) =>
        reduce(head, tail)
      case _ =>
        throw new RuntimeException("no predicates found for: " + prop.name.value)
    }
  }

  private def predicates(prop: Term.Param)(implicit
      context: ModelGenContext
  ): Type = {
    prop match {
      case RefinedPropertyPredicates(one :: Nil) =>
        one
      case RefinedPropertyPredicates(head :: tail) =>
        reduce(head, tail)
      case _ =>
        throw new RuntimeException("no predicates found for: " + prop.name.value)
    }
  }

  private def preface(implicit context: ModelGenContext) =
    q"""
        import eu.timepit.refined.api.Refined
        import eu.timepit.refined.boolean.And
        import eu.timepit.refined.collection._
        import eu.timepit.refined.numeric._
        import eu.timepit.refined.string._
        import shapeless.Witness

          ..${if (context.isLibraryEnabled[CirceJsonSupport]())
      q"import io.circe.refined._" :: Nil
    else
      Nil}
       """.stats

  private def reduce(lhs: Type, rhs: List[Type]): Type = rhs match {
    case last :: Nil =>
      q"val dummy: And[$lhs, $last]".decltpe
    case head :: tail =>
      q"val dummy: And[$lhs, ${reduce(head, tail)}]".decltpe
    case Nil =>
      throw new RuntimeException("logic error: unable to reduce an empty list")
  }

  private def refinedTypeObject(
      typeName : Type.Name,
      originalType: Type,
      predicates: Type,
      optional: Boolean
  ): Defn.Object = {
    /// These "companion-like" object definitions are inspired by the refined
    /// `RefTypeOps` class.
    if (optional)
      q"""
      object ${Term.Name(typeName.value)} {
        import eu.timepit.refined.api._

        type ResultType = Refined[$originalType,$predicates]

        private val rt = RefinedType.apply[ResultType]

        def apply(candidate: $originalType): Either[IllegalArgumentException, Option[ResultType]] =
          from(Option(candidate))

        def apply(candidate: Option[$originalType]): Either[IllegalArgumentException, Option[ResultType]] =
          from(candidate)

        def from(candidate: Option[$originalType]): Either[IllegalArgumentException, Option[ResultType]] =
          candidate match {
            case Some(value) =>
              rt.refine(value).map(Some(_)).left.map(msg => new IllegalArgumentException(msg))
            case None =>
              Right(None)
          }

        def unapply(candidate: Option[$originalType]): Option[ResultType] =
          from(candidate).fold(_ => None, a => a)

        def unsafeFrom(candidate: Option[$originalType]): Option[ResultType] =
          candidate.map(rt.unsafeRefine)
      }
     """
    else
      q"""
      object ${Term.Name(typeName.value)} {
        import eu.timepit.refined.api._

        type ResultType = Refined[$originalType,$predicates]

        private val rt = RefinedType.apply[ResultType]

        def apply(candidate: $originalType): Either[IllegalArgumentException, ResultType] =
          from(candidate)

        def from(candidate: $originalType): Either[IllegalArgumentException, ResultType] =
          rt.refine(candidate).left.map(msg => new IllegalArgumentException(msg))

        def unapply(candidate: $originalType): Option[ResultType] =
          from(candidate).toOption

        def unsafeFrom(candidate: $originalType): ResultType =
          rt.unsafeRefine(candidate)
      }
     """
  }
}
