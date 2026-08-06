## Parsing with Scala meta for debugging etc.

This can be helpful to understand how to map source code to the types of the scala meta model:

```sh
sbt console
...
scala> import scala.meta._
scala> s"""implicit lazy val decoder: Decoder[SomeEnum] = Decoder[String].emap({
     |   case "A" => Right(A)
     |   case "B" => Right(B)
     |   case other => Right(Unknown(other))
     | })""".parse[Stat].get.structure
res4: String = Defn.Val(List(Mod.Implicit(), Mod.Lazy()), List(Pat.Var(Term.Name("decoder"))), Some(Type.Apply(Type.Name("Decoder"), List(Type.Name("SomeEnum")))) ...
```

also see https://scalameta.org/docs/trees/guide.html

## Cross-building

The build itself runs on sbt 2, and the plugin is published for both sbt versions:

| Scala          | targets  | artifact                |
| -------------- | -------- | ----------------------- |
| 3.8.4 (default)| sbt 2.0.5| `sbt-scraml_sbt2_3`     |
| 2.12.20        | sbt 1    | `sbt-scraml_2.12_1.0`   |

Prefix a command with `+` to run it for both, or `++<version>` to select one:

```shell
sbt +test                     # both
sbt '++ 2.12.20; test'        # sbt 1 build only
```

Note that sources must compile under **both** Scala 2.12 and Scala 3, so they are
limited to the syntax the older compiler accepts.

## Running Tests

```shell
sbt scripted # runs all tests in src/sbt-test
sbt 'scripted sbt-scraml/simple' # runs a specific test (note use of quotes)
sbt +scripted # runs them against both sbt 1 and sbt 2
```

The scripted tests deliberately do not pin `sbt.version`; each cross-build leg runs
them on the sbt version given by `scriptedSbt` in `build.sbt`.

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