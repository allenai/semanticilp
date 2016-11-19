import sbt._
import sbt.Keys._

import org.allenai.plugins.CoreDependencies._

import org.allenai.plugins.StylePlugin

name := "TextILP"
version := "1.0"

scalaVersion := "2.11.5"

lazy val root = (project in file("."))
  .enablePlugins(StylePlugin)

val cogcompNLPVersion = "3.0.71"
val cogcompPipelineVersion = "0.1.25"
val ccgGroupId = "edu.illinois.cs.cogcomp"

resolvers ++= Seq(
  Resolver.mavenLocal,
  "CogcompSoftware" at "http://cogcomp.cs.illinois.edu/m2repo/"
)

libraryDependencies ++= Seq(
  allenAiCommon,
  "commons-io" % "commons-io" % "2.4",
  "net.sf.opencsv" % "opencsv" % "2.1",
  allenAiTestkit % "test",
  "com.typesafe.play" % "play-json_2.11" % "2.5.9",
  ccgGroupId % "illinois-core-utilities" % cogcompNLPVersion withSources,
  ccgGroupId % "illinois-nlp-pipeline" % cogcompPipelineVersion withSources
)