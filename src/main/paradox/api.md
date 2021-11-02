## Specify an API

SCRAML uses https://github.com/commercetools/rest-modeling-framework which is based on the [RAML API specification](https://raml.org/)

## Example API 

in `api/simple.raml`:

@@snip [simple.raml](../../../src/sbt-test/sbt-scraml/simple/api/simple.raml) {}

Note the use of annotations:

@@snip [simple.raml](../../../src/sbt-test/sbt-scraml/simple/api/annotations.raml) {}

and types aliases (which will be used for the generated type names):

@@snip [simple.raml](../../../src/sbt-test/sbt-scraml/simple/api/types.raml) {}
