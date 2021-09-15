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

## Building the examples project

The steps needed to build the `examples` project depend on whether or not you want to use a `SNAPSHOT` build or a published one.


### Snapshot builds

First, publish a local copy of `sbt-scraml`:

```shell
sbt clean test publishLocal
```

Identify the snapshot version from the `sbt` output.  It will look something like `0.8.5+9-294bb63c+20210915-1554-SNAPSHOT` (expect completely different values of course)

Then, building `examples` can be done thusly:

```shell
cd ./examples
sbt -Dplugin.version=0.8.5+9-294bb63c+20210915-1554-SNAPSHOT compile
```


### Release builds

Here, you will want to identify which published version to build with.  Search [Maven Central](https://search.maven.org/artifact/com.commercetools/sbt-scraml) for the version you would like to use.

Next, run:


```shell
cd ./examples
sbt -Dplugin.version=9.9.9 compile    # where '9.9.9' is the version found above
```

