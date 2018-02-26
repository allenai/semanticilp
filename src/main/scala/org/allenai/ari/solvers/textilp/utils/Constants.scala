package org.allenai.ari.solvers.textilp.utils

import java.io.File
import java.net.URLEncoder

import org.allenai.ari.solvers.textilp.solvers.TextILPModel
import redis.clients.jedis.Protocol

import scala.io.Source

case class Elastic(clusterName: String, hostIp: String, hostPort: Int, indexName: Map[String, String])

object Constants {
  // this is the variable that decides which model to use
  val textILPModel: TextILPModel = TextILPModel.EnsembleMinimal

  // annotator servers
  val sahandPort = "8081"
  val sahandServer = "http://cogcomp.dev.ai2"

  val cogcompAnnotatorPort = "5800"
  val cogcompAnnotatorServer = "http://cogcomp.dev.ai2" // "http://austen.cs.illinois.edu" // "http://cogcomp.dev.ai2"

  val externalAnnotatorsPort = "8009"
  val externalAnnotatorsServer = "http://cogcomp.dev.ai2" // "http://bronte.cs.illinois.edu"

  // whether to extract curator or not
  val useCurator = textILPModel match {
    case TextILPModel.EnsembleFull | TextILPModel.StackedForProcesses | TextILPModel.StackedForScience => true
    case TextILPModel.EnsembleMinimal | TextILPModel.StackedForProcessesMinimal | TextILPModel.StackedForScienceMinimal => false
  }

  // this is the link solvers send calls when evaluating against AI2 contempo
  val queryLink = "http://aristo-docker-swarm.dev.allenai.org:8080/ask?text=" // "http://aristo-dev.dev.ai2:8080/ask?text="

  // for elastic search
  val remoteElastic = Elastic(
    clusterName = "aristo-es", hostIp = "aristo-es.dev.ai2", hostPort = 9300,
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

  val multiRCSentencesElastic = Elastic(
    clusterName = "elasticsearch", hostIp = "localhost", hostPort = 9300,
    Map("nultircsentences-2017-12-15" -> "nultircsentences-2017-12-15")
  )

  // this is the variable which decides which instance of elasticsearch to use
  val elasticBeingUsed = remoteElastic

  // vivek's questions
  val vivekPredictonsFile = "other/vivekPredictions/vivek-predictions.tsv"
  val vivekTestParagraphsFile = "other/vivekPredictions/test-documents.tsv"
  lazy val vivekTestParagraphs = Source.fromFile(new File(Constants.vivekTestParagraphsFile)).getLines().toSet

  val vivekQuestions = Source.fromFile(new File(Constants.vivekPredictonsFile)).getLines().toList.drop(1).map { line =>
    val split = line.split("\t")
    val pid = split(1)
    val question = split(2)
    (pid, question)
  }
  val (vivekTestQuestionsAndPars, vivekTrainQuestionsAndPars) = vivekQuestions.partition(x => Constants.vivekTestParagraphs.contains(x._1))
  val vivekTestQuestions = vivekTestQuestionsAndPars.map(_._2).toSet
  val vivekTrainQuestions = vivekTrainQuestionsAndPars.map(_._2).toSet
}
