package scraml.libs

import scala.meta._

import _root_.io.vrap.rmf.raml.model.types._
import _root_.io.vrap.rmf.raml.model.values.RegExp
import scraml.{DefnWithCompanion, LibrarySupport, MetaUtil, ModelGenContext, RMFUtil}

object RefinedSupport extends LibrarySupport {
  import LibrarySupport.appendObjectStats

  // Refined support must be after Json support but before
  // most all others.
  override val order: Double = 0.3

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

  sealed trait HasFacets {
    final def hasAnyFacets(anyType: AnyType): Boolean =
      anyType match {
        case at: ArrayType =>
          hasFacets(at)
        case nt: NumberType =>
          hasFacets(nt)
        case st: StringType =>
          hasFacets(st)
        case _ =>
          false
      }

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
  }

  object DetectMultipleFacetDefinitions extends HasFacets {
    final def apply(declaration: Member)(implicit
        context: ModelGenContext
    ): Option[String] = {
      detect(context.objectType, declaration.name.value)
    }

    private def detect(objectType: ObjectType, name: String)(implicit
        context: ModelGenContext
    ): Option[String] = {
      val declarations = RMFUtil
        .findAllDeclarations(objectType, name)
        .filter { case (definingType, prop) =>
          hasAnyFacets(prop.getType())
        }

      declarations match {
        case _ :: Nil =>
          None

        case head :: tail if different(head, tail) =>
          Some(
            s"""multiple facet definitions detected for '$name':
               |   using      : ${head._1.getName()}
               |   additional : ${tail.map(_._1.getName()).mkString(", ")}
               |""".stripMargin
          )

        case _ =>
          None
      }
    }

    private def different(
        first: (ObjectType, Property),
        children: List[(ObjectType, Property)]
    ): Boolean = {
      children.foldLeft(false) { case (accum, (_, prop)) =>
        val isDifferent = (first._2.getType(), prop.getType()) match {
          case (original: ArrayType, current: ArrayType) =>
            val itemDifference = (Option(original.getItems()), Option(current.getItems())) match {
              case (Some(originalItems: NumberType), Some(currentItems: NumberType)) =>
                diff(originalItems, currentItems)

              case (Some(originalItems: StringType), Some(currentItems: StringType)) =>
                diff(originalItems, currentItems)

              case _ =>
                false
            }

            Option(original.getMaxItems()) != Option(current.getMaxItems()) ||
            Option(original.getMinItems()) != Option(current.getMinItems()) ||
            Option(original.getUniqueItems()) != Option(current.getUniqueItems()) ||
            itemDifference

          case (original: NumberType, current: NumberType) =>
            diff(original, current)

          case (original: StringType, current: StringType) =>
            diff(original, current)

          case _ =>
            false
        }

        accum || isDifferent
      }
    }

    private def diff(original: NumberType, current: NumberType): Boolean =
      Option(original.getMaximum()) != Option(current.getMaximum()) ||
        Option(original.getMinimum()) != Option(current.getMinimum())

    private def diff(original: StringType, current: StringType): Boolean =
      Option(original.getMaxLength()) != Option(current.getMaxLength()) ||
        Option(original.getMinLength()) != Option(current.getMinLength()) ||
        Option(original.getPattern()) != Option(current.getPattern())
  }

  sealed abstract class RefinedPropertyMatching[A] extends TypeNaming with HasFacets {
    final def unapply(declaration: Decl.Def)(implicit
        context: ModelGenContext
    ): Option[A] = {
      dispatch(declaration.name)
    }

    final def unapply(param: Term.Param)(implicit
        context: ModelGenContext
    ): Option[A] = {
      dispatch(param.name)
    }

    protected def array(
        definingType: ObjectType,
        name: Name,
        descriptor: ArrayType,
        optional: Boolean
    )(implicit context: ModelGenContext): Option[A] = None

    protected def number(
        definingType: ObjectType,
        name: Name,
        descriptor: NumberType,
        optional: Boolean
    )(implicit context: ModelGenContext): Option[A] = None

    protected def string(
        definingType: ObjectType,
        name: Name,
        descriptor: StringType,
        optional: Boolean
    )(implicit context: ModelGenContext): Option[A] = None

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

    protected def propertyDefinition(
        aType: ObjectType,
        name: Name
    ): Option[(ObjectType, Property)] =
      RMFUtil.findAllDeclarations(aType, name.value).headOption

    private def dispatch(name: Name)(implicit
        context: ModelGenContext
    ): Option[A] = {
      val definition = propertyDefinition(context.objectType, name).map { case (ot, prop) =>
        (ot, prop.getType(), prop.getRequired)
      }

      definition match {
        case Some((obj, at: ArrayType, required)) =>
          array(obj, name, at, !required)
        case Some((obj, nt: NumberType, required)) =>
          number(obj, name, nt, !required)
        case Some((obj, st: StringType, required)) =>
          string(obj, name, st, !required)
        case _ =>
          None
      }
    }
  }

  object RefinedPropertyConstructorUse extends RefinedPropertyMatching[Term] {
    override protected def propertyDefinition(
        objectType: ObjectType,
        name: Name
    ): Option[(ObjectType, Property)] = {
      super.propertyDefinition(objectType, Name(propertyName(name)))
    }

    override protected def array(
        definingType: ObjectType,
        name: Name,
        descriptor: ArrayType,
        optional: Boolean
    )(implicit context: ModelGenContext): Option[Term] =
      constructorArg(name, optional, sourceIsFaceted(name))

    override protected def number(
        definingType: ObjectType,
        name: Name,
        descriptor: NumberType,
        optional: Boolean
    )(implicit context: ModelGenContext): Option[Term] =
      constructorArg(name, optional, sourceIsFaceted(name))

    override protected def string(
        definingType: ObjectType,
        name: Name,
        descriptor: StringType,
        optional: Boolean
    )(implicit context: ModelGenContext): Option[Term] =
      constructorArg(name, optional, sourceIsFaceted(name))

    private def constructorArg(name: Name, optional: Boolean, faceted: Boolean)(implicit
        context: ModelGenContext
    ): Option[Term] = {
      // strip leading underscores added in generated variable names
      val nameToUse = propertyName(name)

      if (!optional && faceted && !originalWasFaceted(nameToUse))
        Some(q"${Term.Name(name.value)}.value")
      else
        None
    }

    private def originalWasFaceted(name: String)(implicit
        context: ModelGenContext
    ): Boolean =
      propertyDefinition(context.objectType, Name(name))
        .map(_._2)
        .exists { prop =>
          hasAnyFacets(prop.getType)
        }

    private def propertyName(name: Name): String =
      name.value
        .stripPrefix("_")
        .stripPrefix("_")

    private def sourceIsFaceted(name: Name)(implicit
        context: ModelGenContext
    ): Boolean =
      Option(context.objectType.getProperty(propertyName(name))).exists(p =>
        hasAnyFacets(p.getType())
      )
  }

  object RefinedPropertyDeclaration extends RefinedPropertyMatching[(Type, Option[Term])] {
    override protected def propertyDefinition(
        aType: ObjectType,
        name: Name
    ): Option[(ObjectType, Property)] = {
      super.propertyDefinition(aType, name).filter { case (_, prop) =>
        hasAnyFacets(prop.getType())
      }
    }

    override protected def array(
        definingType: ObjectType,
        name: Name,
        descriptor: ArrayType,
        optional: Boolean
    )(implicit context: ModelGenContext): Option[(Type, Option[Term])] = {
      Some(
        mkDeclType(context.objectType, name.value) -> defaultValue(optional) {
          q"None"
        }
      )
    }

    override protected def number(
        definingType: ObjectType,
        name: Name,
        descriptor: NumberType,
        optional: Boolean
    )(implicit context: ModelGenContext): Option[(Type, Option[Term])] =
      Some(
        mkDeclType(context.objectType, name.value) -> defaultValue(optional) {
          q"None"
        }
      )

    override protected def string(
        definingType: ObjectType,
        name: Name,
        descriptor: StringType,
        optional: Boolean
    )(implicit context: ModelGenContext): Option[(Type, Option[Term])] =
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
    override protected def propertyDefinition(
        aType: ObjectType,
        name: Name
    ): Option[(ObjectType, Property)] = {
      RMFUtil.findAllDeclarations(aType, name.value).find { case (_, prop) =>
        hasAnyFacets(prop.getType())
      }
    }

    def apply(declaration: Decl.Def)(implicit
        context: ModelGenContext
    ): Option[List[Type.Apply]] =
      unapply(declaration)

    def apply(param: Term.Param)(implicit
        context: ModelGenContext
    ): Option[List[Type.Apply]] =
      unapply(param)

    override protected def array(
        definingType: ObjectType,
        name: Name,
        descriptor: ArrayType,
        optional: Boolean
    )(implicit context: ModelGenContext): Option[List[Type.Apply]] = {
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
    override protected def propertyDefinition(
        aType: ObjectType,
        name: Name
    ): Option[(ObjectType, Property)] = {
      RMFUtil.findAllDeclarations(aType, name.value).find { case (_, prop) =>
        hasAnyFacets(prop.getType())
      }
    }

    override protected def array(
        definingType: ObjectType,
        name: Name,
        descriptor: ArrayType,
        optional: Boolean
    )(implicit context: ModelGenContext): Option[List[Type.Apply]] = {
      val min = Option(descriptor.getMinItems)
      val max = Option(descriptor.getMaxItems)

      Option(descriptor.getItems) match {
        case Some(nt: NumberType) if hasFacets(nt) =>
          Some(
            collectionBounds(min, max) :::
              forall(mkItemTypeName(name.value))
          )

        case Some(st: StringType) if hasFacets(st) =>
          Some(
            collectionBounds(min, max) :::
              forall(mkItemTypeName(name.value))
          )

        case _ =>
          Some(
            collectionBounds(Option(descriptor.getMinItems), Option(descriptor.getMaxItems))
          )
      }
    }

    override protected def number(
        definingType: ObjectType,
        name: Name,
        descriptor: NumberType,
        optional: Boolean
    )(implicit context: ModelGenContext): Option[List[Type.Apply]] = {
      val min = Option(descriptor.getMinimum)
      val max = Option(descriptor.getMaximum)

      Some(numberBounds(min.map(BigDecimal(_)), max.map(BigDecimal(_))))
    }

    override protected def string(
        definingType: ObjectType,
        name: Name,
        descriptor: StringType,
        optional: Boolean
    )(implicit context: ModelGenContext): Option[List[Type.Apply]] = {
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
    override protected def propertyDefinition(
        aType: ObjectType,
        name: Name
    ): Option[(ObjectType, Property)] = {
      RMFUtil.findAllDeclarations(aType, name.value).find { case (_, prop) =>
        hasAnyFacets(prop.getType())
      }
    }

    override protected def array(
        definingType: ObjectType,
        name: Name,
        descriptor: ArrayType,
        optional: Boolean
    )(implicit context: ModelGenContext): Option[(Type.Name, Option[Type.Name], Boolean)] = {
      Option(descriptor.getItems) match {
        case Some(nt: NumberType) if hasFacets(nt) =>
          Some((mkTypeName(name.value), Some(mkItemTypeName(name.value)), optional))

        case Some(st: StringType) if hasFacets(st) =>
          Some((mkTypeName(name.value), Some(mkItemTypeName(name.value)), optional))

        case _ =>
          Some((mkTypeName(name.value), None, optional))
      }
    }

    override protected def number(
        definingType: ObjectType,
        name: Name,
        descriptor: NumberType,
        optional: Boolean
    )(implicit context: ModelGenContext): Option[(Type.Name, Option[Type.Name], Boolean)] =
      Some((mkTypeName(name.value), None, optional))

    override protected def string(
        definingType: ObjectType,
        name: Name,
        descriptor: StringType,
        optional: Boolean
    )(implicit context: ModelGenContext): Option[(Type.Name, Option[Type.Name], Boolean)] =
      Some((mkTypeName(name.value), None, optional))
  }

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
        val originalType = determineOriginalType(prop.name.value)

        warnWhenMultipleFacets(prop)

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

    val from =
      List[Stat](
        q"""
          def from( ..${classDef.ctor.paramss.flatten})
          : Either[IllegalArgumentException, ${classDef.name}] = {
            ..${generatePropertiesCode(classDef) {
          case prop if context.objectType.getProperty(prop.name.value) ne null =>
            val invocation = prop match {
              case RefinedPropertyType(typeName, _, optional) if optional =>
                q"${Term.Name(typeName.value)}.from(${Term.Name(prop.name.value)})"

              case RefinedPropertyType(typeName, _, _) =>
                q"${Term.Name(typeName.value)}.from(${Term.Name(prop.name.value)})"

              case unrefined =>
                q"Right(${Term.Name(unrefined.name.value)})"
            }

            List[Stat](
              q"val ${Pat.Var(Term.Name("_" + prop.name.value))} = $invocation"
            )

          case _ =>
            List[Stat](
              q"val _values = Right(values)"
            )
        }}

          ${
          def genFlatmaps(terms: List[Term.Param], remaining: List[Term.Param]): Term.Apply =
            remaining match {
              case last :: Nil =>
                val termName  = Term.Name(last.name.value)
                val paramName = Term.Name("_" + last.name.value)

                q"""
                   $termName.map { $paramName: ${last.decltpe.get} =>
                   ${Term.Name(context.objectType.getName)}( ..${terms.map {
                  case RefinedPropertyConstructorUse(term) =>
                    term
                  case p =>
                    Term.Name(p.name.value)
                }})
                   }
                 """
              case head :: tail =>
                val termName  = Term.Name(head.name.value)
                val paramName = Term.Name("_" + head.name.value)

                q"""
                   $termName.flatMap { $paramName: ${head.decltpe.get} =>
                     ${genFlatmaps(terms, tail)}
                   }
                 """
            }

          val terms = classDef.ctor.paramss.flatten.map {
            case prop: Term.Param if context.objectType.getProperty(prop.name.value) ne null =>
              prop match {
                case RefinedPropertyType(typeName, _, _) =>
                  prop.copy(
                    name = Name("_" + prop.name.value),
                    decltpe = Some(q"val foo: $typeName".decltpe)
                  )

                case unrefined =>
                  unrefined.copy(name = Term.Name("_" + unrefined.name.value))
              }

            case map: Term.Param =>
              map.copy(name = Name("_values"))
          }

          genFlatmaps(
            terms.map(p => p.copy(name = Term.Name("_" + p.name.value))),
            terms
          )
        }
      }
       """
      )

    appendObjectStats(companion, preface ++ types ++ from)
  }

  private def defineRefinements(traitDef: Defn.Trait, companion: Defn.Object)(implicit
      context: ModelGenContext
  ): Defn.Object = {
    val types = generatePropertiesCode(traitDef) {
      // Properties which have regular expressions as their name cannot
      // be resolved, so are skipped.
      case prop if context.objectType.getProperty(prop.name.value) ne null =>
        val originalType = determineOriginalType(prop.name.value)

        warnWhenMultipleFacets(prop)

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

  private def determineOriginalType(name: String)(implicit
      context: ModelGenContext
  ): Type = {
    val propDef = context.objectType.getProperty(name)
    val scalaTypeAnnotation = Option(
      propDef.getAnnotation("scala-type")
    ).map(_.getValue.getValue.toString)

    context
      .scalaTypeRef(
        propDef.getType,
        optional = false,
        typeName = scalaTypeAnnotation,
        defaultAnyTypeName = context.anyTypeName
      )
      .get
      .scalaType
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
      typeName: Type.Name,
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

  private def warnWhenMultipleFacets(declaration: Member)(implicit
      context: ModelGenContext
  ): Unit =
    DetectMultipleFacetDefinitions(declaration).foreach {
      context.warn(_)
    }
}
