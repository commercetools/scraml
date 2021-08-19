sys.props.get("plugin.version").orElse(sys.env.get("PLUGIN_VERSION")) match {
  case Some(x) => addSbtPlugin("com.commercetools" % "sbt-scraml" % x)
  case _ => sys.error("""|The system property 'plugin.version' is not defined.
                         |Specify this property using the scriptedLaunchOpts -D.""".stripMargin)
}
