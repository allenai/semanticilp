import sbt._
import sbt.Keys._

import org.allenai.plugins.CoreDependencies._

import org.allenai.plugins.StylePlugin

name := "TextILP"
version := "1.0"

scalaVersion := "2.11.5"

lazy val root = (project in file("."))
  .enablePlugins(StylePlugin)

val cogcompNLPVersion = "3.0.83"
val cogcompPipelineVersion = "0.1.25"
val ccgGroupId = "edu.illinois.cs.cogcomp"

resolvers ++= Seq(
  Resolver.mavenLocal,
  "CogcompSoftware" at "http://cogcomp.cs.illinois.edu/m2repo/"
)

javaOptions ++= List("-Xmx11g")

libraryDependencies ++= Seq(
//  allenAiCommon,
//  allenAiTestkit % "test",
  "org.allenai.common" %% "common-cache" % "1.4.6",
  "commons-io" % "commons-io" % "2.4",
  "net.sf.opencsv" % "opencsv" % "2.1",
  "com.typesafe.play" % "play-json_2.11" % "2.5.9",
  "org.rogach" %% "scallop" % "2.0.5",
  ccgGroupId % "illinois-core-utilities" % cogcompNLPVersion withSources,
  ccgGroupId % "illinois-nlp-pipeline" % cogcompPipelineVersion withSources,
  ccgGroupId % "illinois-quantifier" % "2.0.8" withSources,
  ccgGroupId % "saul-examples_2.11" % "0.5.5"
)