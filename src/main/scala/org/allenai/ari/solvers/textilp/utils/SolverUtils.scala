package org.allenai.ari.solvers.textilp.utils

import java.io.File
import java.net.{InetSocketAddress, URLEncoder}

import edu.illinois.cs.cogcomp.core.datastructures.ViewNames
import edu.illinois.cs.cogcomp.core.datastructures.textannotation.TextAnnotation
import org.allenai.ari.solvers.textilp.alignment.KeywordTokenizer
import org.allenai.ari.solvers.textilp.{Entity, EntityRelationResult}
import org.allenai.common.cache.JsonQueryCache
import org.elasticsearch.action.search.SearchType
import org.elasticsearch.client.transport.TransportClient
import org.elasticsearch.common.settings.Settings
import org.elasticsearch.common.transport.InetSocketTransportAddress
import org.elasticsearch.index.query.QueryBuilders
import org.elasticsearch.search.SearchHit
import play.api.libs.json._
import redis.clients.jedis.Protocol

import scala.collection.JavaConverters._
import spray.json.DefaultJsonProtocol._

import scala.io.Source

object SolverUtils {
  def handleQuestionWithManyCandidates(onlyQuestion: String, candidates: Seq[String], solver: String): Seq[(String, Double)] = {
    candidates.grouped(6).foldRight(Seq[(String, Double)]()) { (smallGroupOfCandidates, combinedScoreMap) =>
      assert(smallGroupOfCandidates.size <= 6)
      val allOptions = smallGroupOfCandidates.zipWithIndex.map { case (opt, idx) => s" (${(idx + 'A').toChar}) $opt " }.mkString
      val smallQuestion = onlyQuestion + allOptions
      combinedScoreMap ++ evaluateASingleQuestion(smallQuestion, solver)
    }
  }

  /** query question against existing remote solvers
    * The question can have at most 6 options, A to F: "question text (A) option1 (B) option2 .... "
    */
  def evaluateASingleQuestion(q: String, solver: String): Seq[(String, Double)] = {
    val charset = "UTF-8"
    val query = Constants.queryLink + URLEncoder.encode(q, charset) + "&solvers=" + solver
    val html = Source.fromURL(query)
    val jsonString = html.mkString
    val json = Json.parse(jsonString)
    val perOptionResponses = (json \ "response" \ "success" \\ "answers").head.as[JsArray]
    perOptionResponses.value.map { perOptionResponse =>
      val confidence = (perOptionResponse \ "confidence").as[JsNumber].value.toDouble
      val selection = (perOptionResponse \ "selection" \ "multipleChoice" \ "key").as[String]
      val focus = (perOptionResponse \ "selection" \ "multipleChoice" \ "focus").as[String]
      focus -> confidence
    }
  }

  def sortedAnswerToSolverResponse(question: String, options: Seq[String],
                                   snippet: String,
                                   sortedCandidates: Seq[(String, Double)]): (Seq[Int], EntityRelationResult) = {
    val maxScore = sortedCandidates.head._2
    val (selectedAnswers, _) = sortedCandidates.filter(_._2 == maxScore).unzip

    val selectedIndices = question.zipWithIndex.collect{ case (option, idx) if selectedAnswers.toSet.contains(option) => idx }

    val questionString = "Question: " + question
    val choiceString = "|Options: " + options.zipWithIndex.map { case (ans, key) => s" (${key + 1}) " + ans }.mkString(" ")
    val paragraphString = "|Paragraph: " + snippet
    val fullText = questionString + paragraphString + choiceString
    val entities = selectedAnswers.map { str =>
      val begin = choiceString.indexOf(str) + paragraphString.length + questionString.length
      val end = begin + str.length
      Entity("  ", str, Seq((begin, end)))
    }
    selectedIndices -> EntityRelationResult(fullText, entities, Seq.empty, sortedCandidates.toString)
  }

  def getCandidateAnswer(contextTA: TextAnnotation): Set[String] = {
    val nounPhrases = contextTA.getView(ViewNames.SHALLOW_PARSE).getConstituents.asScala.
      filter { ch => ch.getLabel.contains("N") || ch.getLabel.contains("J") || ch.getLabel.contains("V") }.map(_.getSurfaceForm)
    val quotationExtractionPattern = "([\"'])(?:(?=(\\\\?))\\2.)*?\\1".r
    val stringsInsideQuotationMark = quotationExtractionPattern.findAllIn(contextTA.text)
    val ners = contextTA.getView(ViewNames.NER_CONLL).getConstituents.asScala.map(_.getSurfaceForm)
    val ners_onto = contextTA.getView(ViewNames.NER_ONTONOTES).getConstituents.asScala.map(_.getSurfaceForm)
    val quant = contextTA.getView(ViewNames.QUANTITIES).getConstituents.asScala.map(_.getSurfaceForm)
    val p = "-?\\d+".r // regex for finding all the numbers
    val numbers = p.findAllIn(contextTA.text)
    (nounPhrases ++ quant ++ ners ++ ners_onto ++ numbers ++ stringsInsideQuotationMark).toSet
  }

  lazy val esClient = {
    val settings = Settings.builder()
      .put("cluster.name", Constants.clusterName)
      .put("client.transport.sniff", false)
      .put("sniffOnConnectionFault", false)
      .build()
    val host = Constants.hostIp
    val address = new InetSocketTransportAddress(new InetSocketAddress(host, 9300))
    println(s"Created Elastic Search Client in cluster ${Constants.clusterName}")
    val clientBuilder = TransportClient.builder().settings(settings)
    clientBuilder.build().addTransportAddress(address)
  }

  def extractPatagraphGivenQuestionAndFocusSet(question: String, focusSet: Seq[String], topK: Int): Seq[String] = {
    focusSet.flatMap(f => extractParagraphGivenQuestionAndFocusWord(question, f, 200))
  }

  def extractPatagraphGivenQuestionAndFocusSet3(question: String, focusSet: Seq[String], topK: Int): Seq[String] = {
    val sortedSet = focusSet.flatMap(f => extractParagraphGivenQuestionAndFocusWord3(question, f, 200)).sortBy(-_._2)
    (if(sortedSet.size > topK) {
      sortedSet.take(topK)
    }
    else {
      sortedSet
    }).map{_._1}
  }

  def getLuceneHitFields(hit: SearchHit): Map[String, AnyRef] = {
    hit.sourceAsMap().asScala.toMap
  }

  def extractParagraphGivenQuestionAndFocusWord(question: String, focus: String, topK: Int): Set[String] = {
    val hits = extractParagraphGivenQuestionAndFocusWord3(question, focus, topK)
    hits.sortBy(-_._2).map { _._1 }.toSet
  }

  def extractParagraphGivenQuestionAndFocusWord2(question: String, focus: String, topK: Int): Set[String] = {
    val hits = extractParagraphGivenQuestionAndFocusWord3(question, focus, 200)
    val sortedHits = hits.sortBy(-_._2)
    val selectedOutput = if(sortedHits.length > topK) {
      sortedHits.slice(0, topK)
    }
    else {
      sortedHits
    }
    selectedOutput.map {_._1}.toSet
  }

  lazy val elasticWebredisCache = if (Constants.useRedisCachingForElasticSearch) {
    JsonQueryCache[String]("elastic:", "localhost", Protocol.DEFAULT_PORT, Protocol.DEFAULT_TIMEOUT)
  } else {
    // use the dummy client, which always returns None for any query (and not using any Redis)
    DummyRedisClient
  }

  def extractParagraphGivenQuestionAndFocusWord3(question: String, focus: String, searchHitSize: Int): Seq[(String, Double)] = {
    val cacheKey = "elasticWebParagraph:" + question + "//focus:" + focus + "//topK:" + searchHitSize
    val cacheResult = if(Constants.useRedisCachingForAnnotation) {
      elasticWebredisCache.get(cacheKey)
    }
    else {
      None
    }
    if (cacheResult.isEmpty) {
      val questionWords = KeywordTokenizer.Default.stemmedKeywordTokenize(question)
      val focusWords = KeywordTokenizer.Default.stemmedKeywordTokenize(focus)
      val searchStr = s"$question $focus"
      val response = esClient.prepareSearch(Constants.indexNames.keys.toSeq: _*)
        // NOTE: DFS_QUERY_THEN_FETCH guarantees that multi-index queries return accurate scoring
        // results, do not modify
        .setSearchType(SearchType.DFS_QUERY_THEN_FETCH)
        .setQuery(QueryBuilders.matchQuery("text", searchStr))
        .setFrom(0).setSize(searchHitSize).setExplain(true)
        .execute()
        .actionGet()
      // Filter hits that don't overlap with both question and focus words.
      def getLuceneHitFields(hit: SearchHit): Map[String, AnyRef] = {
        hit.sourceAsMap().asScala.toMap
      }
      val hits = response.getHits.getHits.filter { x =>
        val hitWordsSet = KeywordTokenizer.Default.stemmedKeywordTokenize(x.sourceAsString).toSet
        (hitWordsSet.intersect(questionWords.toSet).nonEmpty
          && hitWordsSet.intersect(focusWords.toSet).nonEmpty)
      }
      val results = hits.map{h: SearchHit => getLuceneHitFields(h)("text").toString -> h.score().toDouble }
      val cacheValue = JsArray(results.map{ case (key, value) =>  JsArray(Seq(JsString(key), JsNumber(value)))})
      elasticWebredisCache.put(cacheKey, cacheValue.toString())
      results
    } else {
      val jsonString = cacheResult.get
      val json = Json.parse(jsonString)
      json.as[JsArray].value.map{resultTuple =>
        val tupleValues = resultTuple.as[JsArray]
        val key = tupleValues.head.as[JsString].value
        val score = tupleValues(1).as[JsNumber].value
        key -> score.toDouble
      }
    }
  }

  lazy val omnibusTrain = loadQuestions("Omnibus-Gr04-NDMC-Train.tsv")
  lazy val omnibusTest = loadQuestions("Omnibus-Gr04-NDMC-Test.tsv")
  lazy val omnibusDev: Seq[(String, Seq[String], String)] = loadQuestions("Omnibus-Gr04-NDMC-Dev.tsv")
  lazy val publicTrain = loadQuestions("Public-Feb2016-Elementary-NDMC-Train2.tsv")
  lazy val publicTest = loadQuestions("Public-Feb2016-Elementary-NDMC-Test.tsv")
  lazy val publicDev = loadQuestions("Public-Feb2016-Elementary-NDMC-Dev.tsv")
  lazy val regentsTrain = loadQuestions("Regents-Gr04-NDMC-Train.tsv")
  lazy val small = loadQuestions("small.tsv")

  def loadQuestions(fileName: String): Seq[(String, Seq[String], String)] = {
    Source.fromFile(new File("other/questionSets/" + fileName)).getLines().toList.map{ line =>
      val split = line.split("\t")
      val question = split(0)
      val answer = split(1)
      val questionSplit = question.split("\\([A-Z]\\)")
      (questionSplit.head, questionSplit.tail.toSeq, answer)
    }
  }

  def assignCredit(predict: Seq[Int], gold: Int, maxOpts: Int): Double = {
    //println("predict: " + predict + " / gold: " + gold)
    require(!(predict.contains(-1) && predict.length > 1))
    if(predict.contains(-1) || predict.isEmpty) { // no answer; give partial credits
      1 / maxOpts.toDouble
    }
    else if(predict.contains(gold)) {
      1 / predict.length.toDouble
    }
    else {
      0.0
    }
  }

  import sys.process._
  def assignCreditSquad(predict: String, golds: Seq[String]): (Double, Double, Double) = {
    val output = s"python other/evaluateAnswers.py '$predict' ${golds.mkString("'", "' '", "'")} ".!!
    val outputSplit = output.split("\t")
    val exactMatch = outputSplit(0).toDouble
    val f1 = outputSplit(1).toDouble
    (exactMatch, f1, 1.0)
  }

  def printMemoryDetails() = {
    val mb = 1024*1024

    //Getting the runtime reference from system
    val runtime = Runtime.getRuntime

    println("##### Heap utilization statistics [MB] #####")

    //Print used memory
    println("Used Memory:" + (runtime.totalMemory() - runtime.freeMemory()) / mb)

    //Print free memory
    println("Free Memory:" + runtime.freeMemory() / mb)

    //Print total available memory
    println("Total Memory:" + runtime.totalMemory() / mb)

    //Print Maximum available memory
    println("Max Memory:" + runtime.maxMemory() / mb)
  }

}
