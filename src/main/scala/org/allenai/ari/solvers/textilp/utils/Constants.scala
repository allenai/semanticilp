package org.allenai.ari.solvers.textilp.utils

import java.io.File
import java.net.URLEncoder

import redis.clients.jedis.Protocol

case class Elastic(clusterName: String, hostIp: String, hostPort: Int, indexName: Map[String, String])

object Constants {
  val squadTrainingDataFile = new File("other/questionSets/squad-train-v1.1.json")
  val squadDevDataFile = new File("other/questionSets/squad-dev-v1.1.json")
  val queryLink = "http://aristo-docker-swarm.dev.allenai.org:8080/ask?text=" // "http://aristo-dev.dev.ai2:8080/ask?text="

  def pipelineServer(text: String, views: String) =
    s"http://austen.cs.illinois.edu:8080/annotate?text=${URLEncoder.encode(text, "UTF-8")}&views=${URLEncoder.encode(views, "UTF-8")}"

  val useRedisCachingForAnnotation = true
  val useRedisCachingForElasticSearch = false

  val redisServer = "tableilp-light.dev.ai2" // "localhost" //  //  //"tableilp16c1.dev.ai2" "localhost"
  val redisPort = Protocol.DEFAULT_PORT
  val timeout = 20000

  val pipelineAnnotationCache = ""

  // for elastic search
  val remoteElastic = Elastic(
    clusterName = "aristo-es", hostIp = "aristo-es1.dev.ai2", hostPort = 9300,
    Map(
      "barrons" -> "Barrons 4th Grade Study Guide",
      "websentences" -> "Web",
      "ck12biov44" -> "CK-12 8th Grade Biology Textbook",
      "waterloo" -> "Waterloo Corpus",
      "wikipedia" -> "Wikipedia",
      "simplewikipedia" -> "Simple Wikipedia"
    //    "waterloofiltered1" -> "waterloofiltered1",
    //    "waterloofiltered2" -> "waterloofiltered2",
    //    "quizlet-qna" -> "quizlet-qna",
    //    "quizlet-termdef" -> "quizlet-termdef",
    //    "studystack-qna" -> "studystack-qna",
    //    "virginiaflashcard-sentences" -> "virginiaflashcard-sentences",
    //    "ck12-flexbook-gr3-sentences" -> "ck12-flexbook-gr3-sentences",
    //    "ck12-flexbook-gr4-sentences" -> "ck12-flexbook-gr4-sentences",
    //    "ck12-flexbook-gr5-sentences" -> "ck12-flexbook-gr5-sentences",
    //    "ck12-flexbook-gr3-qna" -> "ck12-flexbook-gr3-qna",
    //    "barrons-2016-09-21" -> "barrons-2016-09-21",
    //    "busc-2016-11-17" -> "busc-2016-11-17",
    //    "ck12-flexbook-gr3-qna-2016-10-19" -> "ck12-flexbook-gr3-qna-2016-10-19"
    )
  )

  val localElastic = Elastic(
    clusterName = "danielk", hostIp = "localhost", hostPort = 9300,
    Map("tables-to-sentences2-2017-06-20" -> "Barrons 4th Grade Study Guide")
  )

  val elasticBeingUsed = remoteElastic

  // vivek's questions
  val vivekPredictonsFile = "other/vivekPredictions/vivek-predictions.tsv"
  val vivekTestParagraphs = "other/vivekPredictions/test-documents.tsv"

}
