logLevel := Level.Warn

//addSbtPlugin("org.allenai.plugins" % "allenai-sbt-plugins" % "1.1.12")

// The Play plugin
addSbtPlugin("com.typesafe.play" % "sbt-plugin" % "2.5.10")

// this has some issues with AI2's sbt plugins
addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.8.2")
