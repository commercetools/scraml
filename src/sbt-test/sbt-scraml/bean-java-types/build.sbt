val refinedVersion = "0.11.3"

lazy val root = (project in file("."))
  .settings(
    scalaVersion := "2.13.16",
    crossScalaVersions ++= Seq("3.3.4"),
    name := "scraml-bean-java-types-test",
    version := "0.1",
    ramlFile := Some(file("api/bean.raml")),
    ramlFieldMatchPolicy := scraml.FieldMatchPolicy.Exact(),
    librarySupport := Set(
      scraml.libs.BeanPropertiesSupport,
      scraml.libs.RefinedSupport
      ),
    beanProperties := scraml.BeanProperties(
      anyVal = scraml.BeanProperties.UseJavaLangTypes,
      array = scraml.BeanProperties.UseJavaCollectionTypes,
      optional = scraml.BeanProperties.UseJavaOptionalType,
      scalaNumber = scraml.BeanProperties.UseJavaLangTypes
    ),
    Compile / sourceGenerators += runScraml,
    libraryDependencies ++= Seq(
      "eu.timepit" %% "refined",
      "eu.timepit" %% "refined-cats"
    ).map(_ % refinedVersion),
  )

