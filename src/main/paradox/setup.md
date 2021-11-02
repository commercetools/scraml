# Setup

## Add the dependency to your build

`project/plugins.sbt`:

@@@vars
```scala
 addSbtPlugin("com.commercetools" % "sbt-scraml" % "$version$")
```
@@@

## Configure your build

`build.sbt`:

@@snip [build.sbt](../../../src/sbt-test/sbt-scraml/simple/build.sbt) {}