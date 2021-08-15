# SCRAML

[![latest release](https://shields.io/badge/-Maven_Central-lightgrey?style=flat&logo=scala)](https://search.maven.org/search?q=g:com.commercetools%20AND%20a:sbt-scraml)


`sbt` plugin that generates code from RAML files at build time.

Built on top of the [commercetools/rest-modeling-framework](https://github.com/commercetools/rest-modeling-framework)
and [scalameta](https://scalameta.org/).

## How to use

there is a [test project](src/sbt-test/sbt-scraml/simple) that you can use as a starting point. 

## Running Tests

```shell
sbt scripted # runs all tests in src/sbt-test
sbt 'scripted sbt-scraml/simple' # runs a specific test (note use of quotes)
```
