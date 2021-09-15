sys.props.get("plugin.version") match {
    case Some(ver) => addSbtPlugin("com.commercetools" % "sbt-scraml" % ver)
    case _ => sys.error("""|The system property 'plugin.version' is not defined.
                           |Specify this property using the scriptedLaunchOpts -D.""".stripMargin)
}
