# Setup

## Add the dependency to your build

`project/plugins.sbt`:

@@@vars
```scala
 addSbtPlugin("com.commercetools" % "sbt-scraml" % "$version$")
```
@@@

## Configure your build

For projects which have one [RAML API specification](https://raml.org/) root document, each SCRAML setting can be specified individually, with `ramlFile` being the only required setting for `runScraml`.

`build.sbt`:

@@snip [build.sbt](../../../src/sbt-test/sbt-scraml/simple/build.sbt) {}


Projects which have two or more [RAML API specification](https://raml.org/) root documents must use the `ramlDefinitions` setting and **not** `ramlFile`.  If both are set, `ramlFile` will take precedence and `ramlDefinitions` will be ignored.

`build.sbt`:

@@snip [build.sbt](../../../src/sbt-test/sbt-scraml/two-specifications/build.sbt) {}

Notes:

* Each `basePackage` **must** be unique across all `scraml.ModelDefinition`s.
* The `defaultTypes` can be set globally and will be used for each `scraml.ModelDefinition` which has a `defaultTypes` with all default values.
* The `libraryDefinitions` can be set globally and will be used for each `scraml.ModelDefinition` which has an empty `Set`.

## Tuning Code Generation

A project can tune what type of code is generated both by the `LibrarySupport` enabled as well as specifying a `FieldMatchPolicy`.  The former is documented in the @ref:[Library Support](libs.md) page and will not be covered here.

A `FieldMatchPolicy` determines whether or not `additionalProperties` are supported for a RAML type definition as well as what Circe will do in the presence of them (if Circe is enabled).  There are four distinct policies available and a `MatchInOrder` policy which allows the specification of one or more `FieldMatchPolicy` instances applied in the order given.

To select a `FieldMatchPolicy`, set the project-wide `ramlFieldMatchPolicy` or specify the `ramlFieldMatchPolicy` `Option` in each `scraml.ModelDefinition` as desired.  If a `scraml.ModelDefinition` does not specify a `ramlFieldMatchPolicy`, the project-wide `ramlFieldMatchPolicy` setting is used.

All policies which support `additionalProperties` for a given [RAML Object Type](https://github.com/raml-org/raml-spec/blob/master/versions/raml-10/raml-10.md/#object-type) have an additional `case class` constructor argument list generated of the form:

```scala
final case class Generated(...)(
  val additionalProperties: Option[Generated.AdditionalProperties] = None
)
```

Where `Generated` is the name of the Scala class created by `scraml`.

Types resulting in `case class` generation which do **not** have `additionalProperties` enabled will **not** have the additional constructor argument list.  For example:

```scala
final case class NoAdditionalAllowed(...)
```

Note that `trait`s and `object`s will not have `additionalProperties` generated for them.

Also note that use of the custom `asMap` annotation initiates an alternate code generation flow, bypassing the `FieldMatchPolicy` all together.

### Default

This is the policy `scraml` will use if no other is specified.  It adheres to the [RAML Additional Properties](https://github.com/raml-org/raml-spec/blob/master/versions/raml-10/raml-10.md/#additional-properties) specification as closely as possible by enabling `additionalProperties` support unless the `additionalProperties` facet is set to `false` for a [RAML Object Type](https://github.com/raml-org/raml-spec/blob/master/versions/raml-10/raml-10.md/#object-type).

If Circe support is enabled, named properties will be used in the generated `Decoder` and any additional properties are kept in the `additionalProperties` property for [RAML Object Types](https://github.com/raml-org/raml-spec/blob/master/versions/raml-10/raml-10.md/#object-type) which have `additionalProperties` enabled.  Additional properties will not cause the `Decoder` to fail for types which have `additionalProperties` disabled.

Note that `Default` will match **all** [RAML Object Types](https://github.com/raml-org/raml-spec/blob/master/versions/raml-10/raml-10.md/#object-type).  Since it is the default used and is the fallback policy for `MatchInOrder`, there is no need to explicitly configure this policy.

### Exact

The `Exact` policy only generates `additionalProperties` support when one or more pattern properties are present in a [RAML Object Type](https://github.com/raml-org/raml-spec/blob/master/versions/raml-10/raml-10.md/#object-type).

If Circe support is enabled, a derived `Decoder` is generated unless `additionalProperties` are allowed.  Additional properties present for types which disallow `additionalProperties` will cause the `Decoder` to fail.

### IgnoreExtra

The `IgnoreExtra` policy never generates `additionalProperties` support.

If Circe support is enabled, only named properties for a [RAML Object Type](https://github.com/raml-org/raml-spec/blob/master/versions/raml-10/raml-10.md/#object-type) will be used in the generated `Decoder`.  Additional properties will not cause the `Decoder` to fail.

### KeepExtra

The `KeepExtra` policy always generates `additionalProperties` support.  This also implies there will be no singleton types generated as there could always be a possibility of `additionalProperties` being present on a per-instance basis.

If Circe support is enabled, named properties will be used in the generated `Decoder`.  Additional properties will not cause the `Decoder` to fail.

### MatchInOrder

The `MatchInOrder` policy is a model of the composite pattern and allows a project to specify in which order each policy will be applied.  In order for policies specified after others to be considered, `Exact`, `IgnoreExtra`, and `KeepExtra` have an `excluding: Set[String]` constructor argument (defaulting to `Set.empty`).  If a [RAML Object Type](https://github.com/raml-org/raml-spec/blob/master/versions/raml-10/raml-10.md/#object-type) has a `displayName` which matches a policy's `excluding` configuration, that policy will not be applied and the next one will be attempted.  If no policy matches, the `Default` policy is used.

`build.sbt`:

@@snip [build.sbt](../../../src/sbt-test/sbt-scraml/refined/build.sbt) {}

