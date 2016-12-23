package org.allenai.ari.solvers.textilp.utils

import java.io.File

object Constants {
  val squadTrainingDataFile = new File("/Users/daniel/Desktop/squad-train-v1.1.json")
  val squadDevDataFile = new File("/Users/daniel/Desktop/squad-dev-v1.1.json")
  val queryLink = "http://aristo-docker-swarm.dev.allenai.org:8080/ask?text="  // "http://aristo-dev.dev.ai2:8080/ask?text="
  val useRedisCachingForAnnotation = false
  val useRedisCachingForElasticSearch = false
  val pipelineAnnotationCache = ""

  // for elastic search
  val indexNames = Map(
    "barrons" -> "Barrons 4th Grade Study Guide",
    "websentences" -> "Web",
    "ck12biov44" -> "CK-12 8th Grade Biology Textbook",
    "waterloo" -> "Waterloo Corpus",
    "wikipedia" -> "Wikipedia",
    "simplewikipedia" -> "Simple Wikipedia"
  )

  // clusterSettings
  val clusterName = "aristo-es"
  val hostIp = "aristo-es1.dev.ai2"
  val hostPort = 9300

}
