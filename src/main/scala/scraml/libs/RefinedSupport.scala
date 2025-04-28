package scraml.libs

import scala.meta._

import _root_.io.vrap.rmf.raml.model.types._
import _root_.io.vrap.rmf.raml.model.values.RegExp
import scraml._

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
      Type.Name(clean(propertyName) + "ItemPredicate")

    protected def mkTypeName(propertyName: String): Type.Name =
      Type.Name(clean(propertyName) + "Type")

    private def clean(propertyName: String): String =
      propertyNameFrom(propertyName.stripPrefix("_").capitalize)
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
        .findAllDeclarations(objectType, propertyNameFrom(name))
        .filter { case (_, prop) =>
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

    final def unapply(pat: Pat)(implicit
        context: ModelGenContext
    ): Option[A] = pat match {
      case Pat.Var(name) =>
        dispatch(name)

      case _ =>
        None
    }

    protected def array(
        definingType: ObjectType,
        name: Name,
        descriptor: ArrayType,
        optionality: PropertyOptionality
    )(implicit context: ModelGenContext): Option[A] = None

    protected def integer(
        definingType: ObjectType,
        name: Name,
        descriptor: IntegerType,
        optionality: PropertyOptionality
    )(implicit context: ModelGenContext): Option[A] = None

    protected def number(
        definingType: ObjectType,
        name: Name,
        descriptor: NumberType,
        optionality: PropertyOptionality
    )(implicit context: ModelGenContext): Option[A] = None

    protected def string(
        definingType: ObjectType,
        name: Name,
        descriptor: StringType,
        optionality: PropertyOptionality
    )(implicit context: ModelGenContext): Option[A] = None

    protected def numberBounds(
        min: Option[BigDecimal],
        max: Option[BigDecimal]
    ): List[Type.Apply] = {
      def toLiteral(number: BigDecimal): Lit = number match {
        case n if n.isValidInt  => Lit.Int(number.toInt)
        case n if n.isValidLong => Lit.Long(number.toInt)
        case n                  => Lit.Double(n.toDouble)
      }
      (min, max) match {
        case (Some(lower), Some(upper)) if upper < lower =>
          throw new RuntimeException("invalid min/max number bounds detected")
        case (Some(lower), Some(upper)) =>
          predicateType("Interval.Closed", toLiteral(lower), toLiteral(upper)) :: Nil
        case (Some(lower), None) =>
          predicateType("GreaterEqual", toLiteral(lower)) :: Nil
        case (None, Some(upper)) =>
          predicateType("LessEqual", toLiteral(upper)) :: Nil
        case _ =>
          Nil
      }
    }

    protected def collectionBounds(min: Option[Integer], max: Option[Integer]): List[Type.Apply] = {
      (min, max) match {
        case (Some(lower), Some(upper)) if upper < lower =>
          throw new RuntimeException("invalid min/max string bounds detected")
        case (Some(lower), Some(upper)) =>
          predicateType("MinSize", Lit.Int(lower.toInt)) ::
            predicateType("MaxSize", Lit.Int(upper.toInt)) ::
            Nil
        case (None, Some(upper)) =>
          predicateType("MaxSize", Lit.Int(upper.toInt)) :: Nil
        case (Some(lower), None) =>
          predicateType("MinSize", Lit.Int(lower.toInt)) :: Nil
        case _ =>
          Nil
      }
    }

    protected def pattern(regex: Option[RegExp]): List[Type.Apply] =
      regex.toList.map { re =>
        predicateType("MatchesRegex", Lit.String(re.toString))
      }

    protected def predicateType(name: String, constants: Lit*): Type.Apply =
      Type.Apply(
        MetaUtil.typeFromName(name),
        Type.ArgClause(constants.toList)
      )

    protected def propertyDefinition(
        aType: ObjectType,
        name: Name
    ): Option[(ObjectType, Property, Name)] =
      RMFUtil
        .findAllDeclarations(aType, propertyNameFrom(name))
        .headOption
        .map { case (declaringType, property) =>
          (declaringType, property, name)
        }

    private def dispatch(name: Name)(implicit
        context: ModelGenContext
    ): Option[A] = {
      val definition = propertyDefinition(context.objectType, name)
        .map { case (ot, prop, maybeRenamed) =>
          (ot, prop.getType(), PropertyOptionality(context.objectType, maybeRenamed))
        }

      definition match {
        case Some((obj, at: ArrayType, optionality)) =>
          array(obj, name, at, optionality)

        case Some((obj, it: IntegerType, optionality)) =>
          integer(obj, name, it, optionality)

        case Some((obj, nt: NumberType, optionality)) =>
          number(obj, name, nt, optionality)

        case Some((obj, st: StringType, optionality)) if !RMFUtil.isEnumType(st) =>
          string(obj, name, st, optionality)

        case _ =>
          None
      }
    }
  }

  object RefinedPropertyConstructorUse extends RefinedPropertyMatching[Term] {
    override protected def propertyDefinition(
        objectType: ObjectType,
        name: Name
    ): Option[(ObjectType, Property, Name)] = {
      val unadorned = propertyNameFrom(name.value)
        .stripPrefix("__")

      super.propertyDefinition(objectType, Name(unadorned))
    }

    override protected def array(
        definingType: ObjectType,
        name: Name,
        descriptor: ArrayType,
        optionality: PropertyOptionality
    )(implicit context: ModelGenContext): Option[Term] =
      constructorArg(name, optionality)

    override protected def integer(
        definingType: ObjectType,
        name: Name,
        descriptor: IntegerType,
        optionality: PropertyOptionality
    )(implicit context: ModelGenContext): Option[Term] =
      constructorArg(name, optionality)

    override protected def number(
        definingType: ObjectType,
        name: Name,
        descriptor: NumberType,
        optionality: PropertyOptionality
    )(implicit context: ModelGenContext): Option[Term] =
      constructorArg(name, optionality)

    override protected def string(
        definingType: ObjectType,
        name: Name,
        descriptor: StringType,
        optionality: PropertyOptionality
    )(implicit context: ModelGenContext): Option[Term] =
      constructorArg(name, optionality)

    private def constructorArg(name: Name, optionality: PropertyOptionality)(implicit
        context: ModelGenContext
    ): Option[Term] = {
      // strip leading underscores added in generated variable names
      val nameToUse = propertyNameFrom(name.value)
        .stripPrefix("__")

      // this is a truth table used to identify what Term to create
      (sourceIsFaceted(nameToUse), originalWasFaceted(nameToUse), optionality.isOptional) match {
        case (true, false, true) =>
          Some(q"${Term.Name(name.value)}.map(_.value)")

        case (true, false, false) =>
          Some(q"${Term.Name(name.value)}.value")

        case _ =>
          None
      }
    }

    private def originalWasFaceted(name: String)(implicit
        context: ModelGenContext
    ): Boolean =
      propertyDefinition(context.objectType, Name(name))
        .map(_._2)
        .exists { prop =>
          hasAnyFacets(prop.getType)
        }

    private def sourceIsFaceted(name: String)(implicit
        context: ModelGenContext
    ): Boolean =
      Option(context.objectType.getProperty(name)).exists { p =>
        hasAnyFacets(p.getType())
      }
  }

  object RefinedPropertyDeclaration extends RefinedPropertyMatching[(Type, Option[Term])] {
    override protected def propertyDefinition(
        aType: ObjectType,
        name: Name
    ): Option[(ObjectType, Property, Name)] = {
      super.propertyDefinition(aType, name).filter { case (_, prop, _) =>
        hasAnyFacets(prop.getType())
      }
    }

    override protected def array(
        definingType: ObjectType,
        name: Name,
        descriptor: ArrayType,
        optionality: PropertyOptionality
    )(implicit context: ModelGenContext): Option[(Type, Option[Term])] = {
      Some(
        mkDeclType(context.objectType, name.value) -> defaultValue(descriptor, optionality) {
          None
        }
      )
    }

    override protected def integer(
        definingType: ObjectType,
        name: Name,
        descriptor: IntegerType,
        optionality: PropertyOptionality
    )(implicit context: ModelGenContext): Option[(Type, Option[Term])] = {
      Some(
        mkDeclType(context.objectType, name.value) -> defaultValue(descriptor, optionality) {
          Some(
            q"${Term.Name(context.objectType.getName)}.${Term.Name(mkTypeName(name.value).value)}.default"
          )
        }
      )
    }

    override protected def number(
        definingType: ObjectType,
        name: Name,
        descriptor: NumberType,
        optionality: PropertyOptionality
    )(implicit context: ModelGenContext): Option[(Type, Option[Term])] = {
      Some(
        mkDeclType(context.objectType, name.value) -> defaultValue(descriptor, optionality) {
          Some(
            q"${Term.Name(context.objectType.getName)}.${Term.Name(mkTypeName(name.value).value)}.default"
          )
        }
      )
    }

    override protected def string(
        definingType: ObjectType,
        name: Name,
        descriptor: StringType,
        optionality: PropertyOptionality
    )(implicit context: ModelGenContext): Option[(Type, Option[Term])] = {
      Some(
        mkDeclType(context.objectType, name.value) -> defaultValue(descriptor, optionality) {
          Some(
            q"${Term.Name(context.objectType.getName)}.${Term.Name(mkTypeName(name.value).value)}.default"
          )
        }
      )
    }

    private def defaultValue(descriptor: AnyType, optionality: PropertyOptionality)(
        value: => Option[Term]
    ): Option[Term] =
      Option(descriptor.getDefault)
        .flatMap(_ => value)
        .orElse(Some(q"None").filter(_ => optionality.isOptional))
  }

  object RefinedPropertyItemPredicates extends RefinedPropertyMatching[List[Type.Apply]] {
    override protected def propertyDefinition(
        aType: ObjectType,
        name: Name
    ): Option[(ObjectType, Property, Name)] =
      RMFUtil
        .findAllDeclarations(aType, propertyNameFrom(name))
        .find { case (_, prop) =>
          hasAnyFacets(prop.getType())
        }
        .map { case (dt, p) => (dt, p, name) }

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
        optionality: PropertyOptionality
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
    ): Option[(ObjectType, Property, Name)] =
      RMFUtil
        .findAllDeclarations(aType, propertyNameFrom(name))
        .find { case (_, prop) =>
          hasAnyFacets(prop.getType())
        }
        .map { case (dt, p) => (dt, p, name) }

    override protected def array(
        definingType: ObjectType,
        name: Name,
        descriptor: ArrayType,
        optionality: PropertyOptionality
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

    override protected def integer(
        definingType: ObjectType,
        name: Name,
        descriptor: IntegerType,
        optionality: PropertyOptionality
    )(implicit context: ModelGenContext): Option[List[Type.Apply]] = {
      val min = Option(descriptor.getMinimum)
      val max = Option(descriptor.getMaximum)

      Some(numberBounds(min.map(BigDecimal(_)), max.map(BigDecimal(_))))
    }

    override protected def number(
        definingType: ObjectType,
        name: Name,
        descriptor: NumberType,
        optionality: PropertyOptionality
    )(implicit context: ModelGenContext): Option[List[Type.Apply]] = {
      val min = Option(descriptor.getMinimum)
      val max = Option(descriptor.getMaximum)

      Some(numberBounds(min.map(BigDecimal(_)), max.map(BigDecimal(_))))
    }

    override protected def string(
        definingType: ObjectType,
        name: Name,
        descriptor: StringType,
        optionality: PropertyOptionality
    )(implicit context: ModelGenContext): Option[List[Type.Apply]] = {
      Some(
        collectionBounds(Option(descriptor.getMinLength), Option(descriptor.getMaxLength)) :::
          pattern(Option(descriptor.getPattern))
      )
    }

    private def forall(itemPredicateName: Name): List[Type.Apply] = {
      Type.Apply(
        Type.Name("Forall"),
        Type.ArgClause(Type.Name(itemPredicateName.value) :: Nil)
      ) :: Nil
    }
  }

  object RefinedPropertyType
      extends RefinedPropertyMatching[(Type.Name, Option[Type.Name], Boolean)] {
    override protected def propertyDefinition(
        aType: ObjectType,
        name: Name
    ): Option[(ObjectType, Property, Name)] =
      RMFUtil
        .findAllDeclarations(aType, propertyNameFrom(name))
        .find { case (_, prop) =>
          hasAnyFacets(prop.getType())
        }
        .map { case (dt, p) => (dt, p, name) }

    override protected def array(
        definingType: ObjectType,
        name: Name,
        descriptor: ArrayType,
        optionality: PropertyOptionality
    )(implicit context: ModelGenContext): Option[(Type.Name, Option[Type.Name], Boolean)] = {
      Option(descriptor.getItems) match {
        case Some(nt: NumberType) if hasFacets(nt) =>
          Some((mkTypeName(name.value), Some(mkItemTypeName(name.value)), optionality.isOptional))

        case Some(st: StringType) if hasFacets(st) =>
          Some((mkTypeName(name.value), Some(mkItemTypeName(name.value)), optionality.isOptional))

        case _ =>
          Some((mkTypeName(name.value), None, optionality.isOptional))
      }
    }

    override protected def integer(
        definingType: ObjectType,
        name: Name,
        descriptor: IntegerType,
        optionality: PropertyOptionality
    )(implicit context: ModelGenContext): Option[(Type.Name, Option[Type.Name], Boolean)] =
      Some((mkTypeName(name.value), None, optionality.isOptional))

    override protected def number(
        definingType: ObjectType,
        name: Name,
        descriptor: NumberType,
        optionality: PropertyOptionality
    )(implicit context: ModelGenContext): Option[(Type.Name, Option[Type.Name], Boolean)] =
      Some((mkTypeName(name.value), None, optionality.isOptional))

    override protected def string(
        definingType: ObjectType,
        name: Name,
        descriptor: StringType,
        optionality: PropertyOptionality
    )(implicit context: ModelGenContext): Option[(Type.Name, Option[Type.Name], Boolean)] =
      Some((mkTypeName(name.value), None, optionality.isOptional))
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
    val declarations = classDef.ctor.paramClauses.map { clause =>
      clause.copy(values = clause.values.map {
        case prop @ RefinedPropertyDeclaration((typeName, default)) =>
          prop.copy(decltpe = Some(typeName), default = default)
        case unrefined =>
          unrefined
      })
    }

    val vals = classDef.templ.body.stats.map {
      case candidate @ Defn.Val(
            _,
            List(RefinedPropertyType(typeName, _, _)),
            Some(Type.Apply.After_4_6_0(tpe, _)),
            _
          ) =>
        val fullyQualifiedName = Type.Select(
          Term.Name(context.objectType.getName),
          typeName
        )

        candidate.copy(decltpe = Option(Type.Apply(tpe, Type.ArgClause(List(fullyQualifiedName)))))

      case other =>
        other
    }

    classDef.copy(
      ctor = classDef.ctor.copy(paramss = declarations.map(_.values).toList),
      templ = classDef.templ.copy(stats = vals)
    )
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
      case NamedProperty(param, prop, declaredName) =>
        val originalType = determineOriginalType(declaredName)

        warnWhenMultipleFacets(param)

        param match {
          case RefinedPropertyType(typeName, Some(itemName), optional) if optional =>
            val itemPredicates = RefinedPropertyItemPredicates(param) match {
              case Some(one :: Nil) =>
                q"type $itemName = $one" :: Nil

              case Some(head :: tail) =>
                q"type $itemName = ${reduce(head, tail)}" :: Nil

              case _ =>
                Nil
            }

            itemPredicates ++ List[Stat](
              q"""
                type $typeName = Option[Refined[$originalType,${predicates(param)}]]
                """,
              refinedTypeObject(
                typeName,
                originalType,
                param,
                optional = true,
                collection = true
              )
            )

          case RefinedPropertyType(typeName, Some(itemName), _) =>
            val itemPredicates = RefinedPropertyItemPredicates(param) match {
              case Some(one :: Nil) =>
                q"type $itemName = $one" :: Nil

              case Some(head :: tail) =>
                q"type $itemName = ${reduce(head, tail)}" :: Nil

              case _ =>
                Nil
            }

            itemPredicates ++ List[Stat](
              q"type $typeName = Refined[$originalType,${predicates(param)}]",
              refinedTypeObject(
                typeName,
                originalType,
                param,
                optional = false,
                collection = true
              )
            )

          case RefinedPropertyType(typeName, None, optional) if optional =>
            List[Stat](
              q"""
                type $typeName = Option[Refined[$originalType,${predicates(param)}]]
                """,
              refinedTypeObject(
                typeName,
                originalType,
                param,
                optional = true,
                collection = false
              )
            )

          case RefinedPropertyType(typeName, None, _) =>
            List[Stat](
              q"type $typeName = Refined[$originalType,${predicates(param)}]",
              refinedTypeObject(
                typeName,
                originalType,
                param,
                optional = false,
                collection = false
              )
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
          def from( ..${classDef.ctor.paramClauses.map(_.values).toList.flatten.map { param =>
            param.copy(
              mods = Nil,
              name = Name(propertyNameFrom(param.name))
            )
          }})
          : Either[IllegalArgumentException, ${classDef.name}] = {
            ..${generatePropertiesCode(classDef) {
            case NamedProperty(param, _, declaredName) =>
              val invocation = param match {
                case RefinedPropertyType(typeName, _, _) =>
                  q"${Term.Name(typeName.value)}.from(${Term.Name(propertyNameFrom(param.name))})"

                case unrefined =>
                  q"Right(${Term.Name(propertyNameFrom(unrefined.name))})"
              }

              List[Stat](
                q"val ${Pat.Var(Term.Name("_" + declaredName))} = $invocation"
              )

            case _ =>
              List[Stat](
                q"val _values = Right(values)"
              )
          }}

          ${def genFlatmaps(
              terms: List[Term.Param],
              remaining: List[Term.Param],
              additionalProperties: Option[AdditionalProperties]
          ): Term.Apply =
            remaining match {
              case Nil =>
                q"""
                  Right(
                   ${Term.Name(context.objectType.getName)}()(
                    ..${additionalProperties.map { ap =>
                    Term.Name(ap.propertyName)
                  }.toList}
                   )
                 )
                 """

              case last :: Nil if additionalProperties.isEmpty =>
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

              case last :: Nil =>
                val termName  = Term.Name(propertyNameFrom(last.name))
                val paramName = Term.Name("_" + propertyNameFrom(last.name))

                q"""
                   $termName.map { $paramName: ${last.decltpe.get} =>
                   ${Term.Name(context.objectType.getName)}( ..${terms.map {
                    case RefinedPropertyConstructorUse(term) =>
                      term
                    case p =>
                      Term.Name(p.name.value)
                  }})(..${additionalProperties.map { ap =>
                    Term.Name(ap.propertyName)
                  }.toList}
                 )
                 }
                 """

              case head :: tail =>
                val termName  = Term.Name(propertyNameFrom(head.name))
                val paramName = Term.Name("_" + propertyNameFrom(head.name))

                q"""
                   $termName.flatMap { $paramName: ${head.decltpe.get} =>
                     ${genFlatmaps(terms, tail, additionalProperties)}
                   }
                 """
            }

          val primary = generatePropertiesCode(classDef) {
            case NamedProperty(param, _, declaredName) =>
              param match {
                case RefinedPropertyType(typeName, _, _) =>
                  param.copy(
                    name = Name("_" + declaredName),
                    decltpe = Some(q"val foo: $typeName".decltpe)
                  ) :: Nil

                case unrefined =>
                  unrefined.copy(name = Term.Name("_" + unrefined.name.value)) :: Nil
              }

            case map: Term.Param =>
              map.copy(name = Name("_values")) :: Nil
          }

          val additional = context.params.fieldMatchPolicy
            .additionalProperties(context.objectType)(context)

          genFlatmaps(
            primary.map(p => p.copy(name = Term.Name("_" + propertyNameFrom(p.name)))),
            primary,
            additional
          )}
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
      case NamedProperty(param, prop, declaredName) =>
        val originalType = determineOriginalType(param.name.value)

        warnWhenMultipleFacets(param)

        param match {
          case RefinedPropertyType(typeName, Some(itemName), optional) if optional =>
            val itemPredicates = RefinedPropertyItemPredicates(param) match {
              case Some(one :: Nil) =>
                q"type $itemName = $one" :: Nil

              case Some(head :: tail) =>
                q"type $itemName = ${reduce(head, tail)}" :: Nil

              case _ =>
                Nil
            }

            itemPredicates ++ List[Stat](
              q"""
                type $typeName = Option[Refined[$originalType,${predicates(param)}]]
                """
            )

          case RefinedPropertyType(typeName, Some(itemName), _) =>
            val itemPredicates = RefinedPropertyItemPredicates(param) match {
              case Some(one :: Nil) =>
                q"type $itemName = $one" :: Nil

              case Some(head :: tail) =>
                q"type $itemName = ${reduce(head, tail)}" :: Nil

              case _ =>
                Nil
            }

            itemPredicates ++ List[Stat](
              q"type $typeName = Refined[$originalType,${predicates(param)}]"
            )

          case RefinedPropertyType(typeName, _, optional) if optional =>
            List[Stat](
              q"""
                type $typeName = Option[Refined[$originalType,${predicates(param)}]]
                """
            )

          case RefinedPropertyType(typeName, _, _) =>
            List[Stat](
              q"type $typeName = Refined[$originalType,${predicates(param)}]"
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

    context.scalaTypeRefFromProperty(propDef, optional = false).get.scalaType
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

  private def preface(implicit context: ModelGenContext): List[Stat] =
    q"""
        import eu.timepit.refined.api.Refined
        import eu.timepit.refined.boolean.And
        import eu.timepit.refined.collection._
        import eu.timepit.refined.numeric._
        import eu.timepit.refined.string._

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
      param: Term.Param,
      optional: Boolean,
      collection: Boolean
  )(implicit context: ModelGenContext): Defn.Object = {
    /// These "companion-like" object definitions are inspired by the refined
    /// `RefTypeOps` class.
    param.default match {
      case Some(term) if !collection && !optional =>
        q"""
        object ${Term.Name(typeName.value)} {
          import eu.timepit.refined.api._

          type ResultType = Refined[$originalType,${predicates(param)}]

          private val rt = RefinedType.apply[ResultType]

          lazy val default: ResultType = unsafeFrom($term)

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

      case Some(term) if !collection && optional =>
        q"""
        object ${Term.Name(typeName.value)} {
          import eu.timepit.refined.api._

          type ResultType = Refined[$originalType,${predicates(param)}]

          private val rt = RefinedType.apply[ResultType]

          lazy val default: Option[ResultType] = unsafeFrom($term)

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

      case _ if optional =>
        q"""
        object ${Term.Name(typeName.value)} {
          import eu.timepit.refined.api._

          type ResultType = Refined[$originalType,${predicates(param)}]

          private val rt = RefinedType.apply[ResultType]

          lazy val default: Option[ResultType] = None

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

      case _ =>
        q"""
      object ${Term.Name(typeName.value)} {
        import eu.timepit.refined.api._

        type ResultType = Refined[$originalType,${predicates(param)}]

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

  private def warnWhenMultipleFacets(declaration: Member)(implicit
      context: ModelGenContext
  ): Unit =
    DetectMultipleFacetDefinitions(declaration).foreach {
      context.warn(_)
    }
}
