# SCRAML

`sbt` plugin that generates code from RAML files at build time.

Built on top of the [commercetools/rest-modeling-framework](https://github.com/commercetools/rest-modeling-framework)
and [scalameta](https://scalameta.org/).

## How to use

there is a [test project](src/sbt-test/sbt-scraml/simple) that you can use as a starting point. 

## Running Tests

```shell
sbt scripted # runs all tests in src/sbt-test
sbt scripted sbt-scraml/simple # runs a specific test
```
