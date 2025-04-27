package scraml.libs

import scala.meta._

import scraml.LibrarySupport.{appendClassStats, appendTraitStats}
import scraml.{DefnWithCompanion, LibrarySupport, ModelGenContext}

object BeanPropertiesSupport extends LibrarySupport {
  import cats.syntax.applicative._

  override def order: Double = 1.0.min(RefinedSupport.order + 0.05)

  override def modifyClass(classDef: Defn.Class, companion: Option[Defn.Object])(implicit
      context: ModelGenContext
  ): DefnWithCompanion[Defn.Class] =
    classDef match {
      case HasAnyProperties() =>
        DefnWithCompanion(
          appendClassStats(classDef, generateBeanProperties(classDef, companion)),
          companion
        )

      case _ => super.modifyClass(classDef, companion)(context)
    }

  override def modifyTrait(traitDef: Defn.Trait, companion: Option[Defn.Object])(implicit
      context: ModelGenContext
  ): DefnWithCompanion[Defn.Trait] =
    traitDef match {
      case HasAnyProperties() =>
        DefnWithCompanion(
          appendTraitStats(traitDef, generateBeanProperties(traitDef, companion)),
          companion
        )

      case _ => super.modifyTrait(traitDef, companion)(context)
    }

  private def generateBeanProperties(
      classDef: Defn.Class,
      companion: Option[Defn.Object]
  )(implicit
      context: ModelGenContext
  ): List[Stat] = {
    val beanMethods = bean.MethodGenerator.create(companion)

    q"""
         import scala.jdk.CollectionConverters._
         import scala.jdk.OptionConverters._

         ..${generatePropertiesCode(classDef)(beanMethods(_).pure[List])}
       """.stats
  }

  private def generateBeanProperties(
      traitDef: Defn.Trait,
      companion: Option[Defn.Object]
  )(implicit
      context: ModelGenContext
  ): List[Stat] = {
    val beanMethods = bean.MethodGenerator.create(companion)

    generatePropertiesCode(traitDef)(beanMethods(_).pure[List])
  }
}
