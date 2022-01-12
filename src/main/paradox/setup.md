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

