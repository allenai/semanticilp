package org.allenai.ari.solvers.textilp.utils

import java.io.File

import redis.clients.jedis.Protocol

object Constants {
  val squadTrainingDataFile = new File("other/questionSets/squad-train-v1.1.json")
  val squadDevDataFile = new File("other/questionSets/squad-dev-v1.1.json")
  val queryLink = "http://aristo-docker-swarm.dev.allenai.org:8080/ask?text=" // "http://aristo-dev.dev.ai2:8080/ask?text="

  val useRedisCachingForAnnotation = true
  val useRedisCachingForElasticSearch = false

  val redisServer = "tableilp-light.dev.ai2" //  //"tableilp16c1.dev.ai2" "localhost"
  val redisPort = Protocol.DEFAULT_PORT
  val timeout = 20000

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
