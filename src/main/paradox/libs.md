# Library Support

SCRAML allows generating supporting types for a number of libraries:

## cats

@@snip [build.sbt](../../../src/sbt-test/sbt-scraml/cats/build.sbt) {}

## circe

@@snip [build.sbt](../../../src/sbt-test/sbt-scraml/json/build.sbt) {}

## monocle

@@snip [build.sbt](../../../src/sbt-test/sbt-scraml/cats/build.sbt) {}

## refined (with cats and circe)

@@snip [build.sbt](../../../src/sbt-test/sbt-scraml/refined/build.sbt) {}

## Sphere JSON

@@snip [build.sbt](../../../src/sbt-test/sbt-scraml/ct-api-sphere/build.sbt) {}

## tapir

The [tapir](https://github.com/softwaremill/tapir) support generating endpoint values:

### Example tapir build.sbt

@@snip [build.sbt](../../../examples/build.sbt) {}

### Interpreting tapir endpoints

Usage of the generated `Endpoints.Greeting.getGreeting` type from the previous @ref:[api](api.md) example:

@@snip [build.sbt](../../../examples/src/main/scala/examples/GreetingExample.scala) {}

## bean properties

Unlike the above library support components, the bean properties library support instructs `scraml` to emit JavaBeans method definitions and does not require use of an external library.  Instead, it can enable `scraml` generated types to have methods Java-based libraries often expect.

@@snip [build.sbt](../../../src/sbt-test/sbt-scraml/ct-api-beans/build.sbt) {}

Including `scraml.libs.BeanPropertiesSupport` in the `librarySupport` definition enables bean property generation.  Further customization is specified in the optional `scraml.BeanProperties` value.

Each `scraml.BeanProperties` configuration option is detailed here.

### anyVal

The `anyVal` configuration setting determines what, if any, transformations are applied to Scala `AnyVal` types (such as `Int`, `Double`, etc.).

#### `BeanProperties.UseJavaLangTypes`

When enabled, Scala `AnyVal` types are converted into their `java.lang` equivalent.

| Scala Type | Bean Definition Type |
| :--- | :--- |
| `Byte` | `java.lang.Byte` |
| `Char` | `java.lang.Character` |
| `Double` | `java.lang.Double` |
| `Float` | `java.lang.Float` |
| `Int` | `java.lang.Integer` |
| `Long` | `java.lang.Long` |
| `Short` | `java.lang.Short` |

#### `BeanProperties.Unchanged`

No transformations are applied to the Scala type.

### array

The `array` configuration setting determines what, if any, transformation is applied to the configured `DefaultTypes.array` type.

#### `BeanProperties.UseJavaCollectionTypes`

When enabled, convert the `DefaultTypes.array` type to `java.lang.List`.  Only `Seq`-based types are currently supported.

#### `BeanProperties.Unchanged`

No transformations are applied to the Scala type.

### evaluate

The `evaluate` configuration setting determines whether *each* bean method will be evaluated every time it is invoked or only the first time.

#### `BeanProperties.EveryInvocation`

Evaluate each time the property is used (`def`).

#### `BeanProperties.Once`

Evaluate only the first time the property is used (`lazy val`).

### optional

The `optional` configuration setting determines what, if any, transformation is applied to `scala.Option` properties.

#### `BeanProperties.UseJavaOptionalType`

When enabled, convert `scala.Option` to `java.lang.Optional`.

#### `BeanProperties.UseNullableReturnType`

When enabled, convert **all** `scala.Option` properties to return the equivalent `AnyRef` representation.  When the original property `isEmpty`, `null` is returned.

NOTE: This option automatically enables `BeanProperties.UseJavaLangTypes` for `anyVal`.

#### `BeanProperties.Unchanged`

No transformations are applied to the Scala type.

### scalaNumber

The `scalaNumber` configuration setting determines what, if any, transformation is applied to `scala.math` properties.

| Scala Type | Bean Definition Type |
| :--- | :--- |
| `BigDecimal` | `java.math.BigDecimal` |
| `BigInt` | `java.math.BigInt` |

