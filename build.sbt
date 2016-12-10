import sbt._
import sbt.Keys._

import org.allenai.plugins.CoreDependencies._
import org.allenai.plugins.StylePlugin

val cogcompNLPVersion = "3.0.83"
val cogcompPipelineVersion = "0.1.25"
val ccgGroupId = "edu.illinois.cs.cogcomp"

lazy val commonSettings = Seq(
  version := "1.0",
  scalaVersion := "2.11.8",
  javaOptions ++= List("-Xmx11g")
)

// TODO(danm): This is used enough projects to be in allenai/sbt-plugins CoreDependencies.
def nlpstack(component: String) = ("org.allenai.nlpstack" %% s"nlpstack-$component" % "1.6")
  .exclude("commons-logging", "commons-logging")

def textualEntailment(component: String) =
  "org.allenai.textual-entailment" %% component % "1.0.6"

val sprayVersion = "1.3.3"
def sprayModule(id: String): ModuleID = "io.spray" %% s"spray-$id" % sprayVersion
val sprayClient = sprayModule("client")

lazy val root = (project in file("."))
  .enablePlugins(StylePlugin).
  settings(commonSettings: _*).
  settings(
    name := "text-ilp",
    libraryDependencies ++= Seq(
      allenAiCommon,
      //  allenAiTestkit % "test",
      "org.allenai.common" %% "common-cache" % "1.4.6",
      "commons-io" % "commons-io" % "2.4",
      "net.sf.opencsv" % "opencsv" % "2.1",
      "com.typesafe.play" % "play-json_2.11" % "2.5.9",
      "org.rogach" %% "scallop" % "2.0.5",
      "com.google.inject" % "guice" % "4.0",
      "net.debasishg" %% "redisclient" % "3.0",
      "com.medallia.word2vec" % "Word2VecJava" % "0.10.3",
      ccgGroupId % "illinois-core-utilities" % cogcompNLPVersion withSources,
      ccgGroupId % "illinois-nlp-pipeline" % cogcompPipelineVersion withSources,
      ccgGroupId % "illinois-quantifier" % "2.0.8" withSources,
      ccgGroupId % "saul-examples_2.11" % "0.5.5",
      ccgGroupId % "scip-jni" % "3.1.1",
      nlpstack("chunk") exclude("edu.stanford.nlp", "stanford-corenlp"),
      nlpstack("lemmatize") exclude("edu.stanford.nlp", "stanford-corenlp"),
      nlpstack("tokenize") exclude("edu.stanford.nlp", "stanford-corenlp"),
      nlpstack("postag") exclude("edu.stanford.nlp", "stanford-corenlp"),
      nlpstack("core") exclude("edu.stanford.nlp", "stanford-corenlp"),
      textualEntailment("interface"),
      textualEntailment("service"),
      sprayClient
    ),
    resolvers ++= Seq(
      Resolver.mavenLocal,
      "CogcompSoftware" at "http://cogcomp.cs.illinois.edu/m2repo/"
    ),
    // Make sure SCIP libraries are locatable.
    javaOptions += s"-Djava.library.path=lib",
    envVars ++= Map(
      "LD_LIBRARY_PATH" -> "lib",
      "DYLD_LIBRARY_PATH" -> "lib"
    ),
    includeFilter in unmanagedJars := "*.jar" || "*.so" || "*.dylib",
    fork := true
)

lazy val viz = (project in file("viz")).
  settings(commonSettings: _*).
  dependsOn(root).
  aggregate(root).
  enablePlugins(PlayScala).
  settings(
    name:= "text-ilp-visualization",
    libraryDependencies ++= Seq(
      filters,
      "org.scalatestplus.play" %% "scalatestplus-play" % "1.5.1" % Test,
      "com.typesafe.play" % "play_2.11" % "2.5.10",
      "org.webjars" %% "webjars-play" % "2.4.0-1",
      "org.webjars" % "bootstrap" % "3.3.7",
      "org.webjars" % "jquery" % "3.1.1",
      "org.webjars" % "headjs" % "1.0.3"
    ),
    resolvers ++= Seq("scalaz-bintray" at "http://dl.bintray.com/scalaz/releases")
  )