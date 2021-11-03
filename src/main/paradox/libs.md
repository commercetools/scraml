# Library Support

SCRAML allows to generate supporting types for a number of libraries:

## cats

@@snip [build.sbt](../../../src/sbt-test/sbt-scraml/cats/build.sbt) {}

## circe

@@snip [build.sbt](../../../src/sbt-test/sbt-scraml/json/build.sbt) {}

## refined (with cats and circe)

@@snip [build.sbt](../../../src/sbt-test/sbt-scraml/refined/build.sbt) {}

## Sphere JSON

@@snip [build.sbt](../../../src/sbt-test/sbt-scraml/ct-api-sphere/build.sbt) {}

## tapir

the [tapir](https://github.com/softwaremill/tapir) support generating endpoint values:

### Example tapir build.sbt

@@snip [build.sbt](../../../examples/build.sbt) {}

### Interpreting tapir endpoints

usage of the generated `Endpoints.Greeting.getGreeting` type from the previous @ref:[api](api.md) example:

@@snip [build.sbt](../../../examples/src/main/scala/examples/GreetingExample.scala) {}
