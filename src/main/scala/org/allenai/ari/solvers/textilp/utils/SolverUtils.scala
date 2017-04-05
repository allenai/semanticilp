package org.allenai.ari.solvers.textilp.utils

import java.io.File
import java.net.{ InetSocketAddress, URLEncoder }
import java.util

import edu.illinois.cs.cogcomp.McTest.MCTestBaseline
import edu.illinois.cs.cogcomp.core.datastructures.ViewNames
import edu.illinois.cs.cogcomp.core.datastructures.textannotation.{ Constituent, TextAnnotation }
import org.allenai.ari.solvers.textilp.alignment.KeywordTokenizer
import org.allenai.ari.solvers.textilp.solvers.TextIlpParams
import org.allenai.ari.solvers.textilp.{ Entity, EntityRelationResult, Paragraph, Question }
import org.allenai.common.cache.JsonQueryCache
import org.apache.commons.codec.digest.DigestUtils
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

  val params = TextIlpParams(
    activeQuestionTermWeight = 0.33,
    alignmentScoreDiscount = 0.0, // not used
    questionCellOffset = -0.4, // tuned
    paragraphAnswerOffset = -0.4, // tuned
    firstOrderDependencyEdgeAlignments = 0.0,
    activeSentencesDiscount = -2.5, // tuned
    activeParagraphConstituentsWeight = 0.0, // tuned
    minQuestionTermsAligned = 1,
    maxQuestionTermsAligned = 3,
    minQuestionTermsAlignedRatio = 0.1,
    maxQuestionTermsAlignedRatio = 0.65,
    maxActiveSentences = 2,
    longerThan1TokenAnsPenalty = 0.0,
    longerThan2TokenAnsPenalty = 0.0,
    longerThan3TokenAnsPenalty = 0.02,

    // Answer Options: sparsity
    moreThan1AlignmentAnsPenalty = -0.3,
    moreThan2AlignmentAnsPenalty = -0.5,
    moreThan3AlignmentAnsPenalty = -0.7,

    meteorExactMatchMinScoreValue = 0.3,
    meteorExactMatchMinScoreDiff = 0.12,

    exactMatchMinScoreValue = 0.76,
    exactMatchMinScoreDiff = 0.15,
    exactMatchSoftWeight = 0.0,

    minQuestionToParagraphAlignmentScore = 0.0,
    minParagraphToQuestionAlignmentScore = 0.00,

    // Question: sparsity
    moreThan1AlignmentToQuestionTermPenalty = -0.3,
    moreThan2AlignmentToQuestionTermPenalty = -0.4,
    moreThan3AlignmentToQuestionTermPenalty = -0.5,

    // Paragraph: proximity inducing
    activeDist1WordsAlignmentBoost = 0.0,
    activeDist2WordsAlignmentBoost = 0.0,
    activeDist3WordsAlignmentBoost = 0.0,

    // Paragraph: sparsity
    maxNumberOfWordsAlignedPerSentence = 8,
    maxAlignmentToRepeatedWordsInParagraph = 3,
    moreThan1AlignmentToParagraphTokenPenalty = 0.0,
    moreThan2AlignmentToParagraphTokenPenalty = 0.0,
    moreThan3AlignmentToParagraphTokenPenalty = 0.0,

    // Paragraph: intra-sentence alignment
    coreferenceWeight = 0.0,
    intraSentenceAlignmentScoreDiscount = 0.0,
    entailmentWeight = 0.0,
    srlAlignmentWeight = 0.0,
    scieneTermBoost = 0.1
  )

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
    //    println("query: " + query)
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
    val trimmedSelectedSet = selectedAnswers.map(_.trim).toSet
    val trimmedOptions = options.map(_.trim)
    val selectedIndices = trimmedOptions.zipWithIndex.collect { case (option, idx) if trimmedSelectedSet.contains(option) => idx }
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

  def extractPatagraphGivenQuestionAndFocusSet3(question: String, focusSet: Seq[String], topK: Int,
    staticCache: Boolean = true): Seq[String] = {
    val sortedSet = focusSet.flatMap { f =>
      if (staticCache) {
        staticCacheLucene(question, f, 200)
      } else {
        extractParagraphGivenQuestionAndFocusWord3(question, f, 200)
      }
    }.sortBy(-_._2)
    (if (sortedSet.size > topK) {
      sortedSet.take(topK)
    } else {
      sortedSet
    }).map { _._1 }
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
    val selectedOutput = if (sortedHits.length > topK) {
      sortedHits.slice(0, topK)
    } else {
      sortedHits
    }
    selectedOutput.map { _._1 }.toSet
  }

  lazy val elasticWebRedisCache = if (Constants.useRedisCachingForElasticSearch) {
    JsonQueryCache[String]("elastic:", Constants.redisServer, Constants.redisPort, timeoutMillis = Constants.timeout)
  } else {
    // use the dummy client, which always returns None for any query (and not using any Redis)
    DummyRedisClient
  }

  def extractParagraphGivenQuestionAndFocusWord3(question: String, focus: String, searchHitSize: Int): Seq[(String, Double)] = {
    val cacheKey = "elasticWebParagraph:" + question + "//focus:" + focus + "//topK:" + searchHitSize
    val cacheResult = if (Constants.useRedisCachingForElasticSearch) {
      elasticWebRedisCache.get(cacheKey)
    } else {
      None
    }
    if (cacheResult.isEmpty) {
      val results = extract(question, focus, searchHitSize)
      val cacheValue = JsArray(results.map { case (key, value) => JsArray(Seq(JsString(key), JsNumber(value))) })
      if (Constants.useRedisCachingForAnnotation) {
        elasticWebRedisCache.put(cacheKey, cacheValue.toString())
      }
      results
    } else {
      val jsonString = cacheResult.get
      val json = Json.parse(jsonString)
      json.as[JsArray].value.map { resultTuple =>
        val tupleValues = resultTuple.as[JsArray]
        val key = tupleValues.head.as[JsString].value
        val score = tupleValues(1).as[JsNumber].value
        key -> score.toDouble
      }
    }
  }

  def extract(question: String, focus: String, searchHitSize: Int): Seq[(String, Double)] = {
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
    hits.map { h: SearchHit => getLuceneHitFields(h)("text").toString -> h.score().toDouble }
  }

  lazy val omnibusTrain = loadQuestions("Omnibus-Gr04-NDMC-Train.tsv")
  lazy val omnibusTest = loadQuestions("Omnibus-Gr04-NDMC-Test.tsv")
  lazy val omnibusDev: Seq[(String, Seq[String], String)] = loadQuestions("Omnibus-Gr04-NDMC-Dev.tsv")
  lazy val publicTrain = loadQuestions("Public-Feb2016-Elementary-NDMC-Train2.tsv")
  lazy val publicTest = loadQuestions("Public-Feb2016-Elementary-NDMC-Test.tsv")
  lazy val publicDev = loadQuestions("Public-Feb2016-Elementary-NDMC-Dev.tsv")
  lazy val regentsTrain = loadQuestions("Regents-Gr04-NDMC-Train.tsv")
  lazy val regentsTest = loadQuestions("Regents-Gr04-NDMC-Test.tsv")
  lazy val regentsPerturbed = loadQuestions("regents-train-perturbed.tsv")
  lazy val small = loadQuestions("small.tsv")

  def loadQuestions(fileName: String): Seq[(String, Seq[String], String)] = {
    Source.fromFile(new File("other/questionSets/" + fileName)).getLines().toList.map { line =>
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
    if (predict.contains(-1) || predict.isEmpty) { // no answer; give partial credits
      1 / maxOpts.toDouble
    } else if (predict.contains(gold)) {
      1 / predict.length.toDouble
    } else {
      0.0
    }
  }

  import sys.process._
  def assignCreditSquad(predict: String, golds: Seq[String]): (Double, Double, Double) = {
    // val command = s"python other/evaluateAnswers.py '$predict' ${golds.mkString("'", "' '", "'")} "
    val command = Seq("python", "other/evaluateAnswers.py", s"'$predict'") ++ golds.map(str => s"'$str'")
    val output = command.!!
    val outputSplit = output.split("\t")
    val exactMatch = outputSplit(0).toDouble
    val f1 = outputSplit(1).toDouble
    (exactMatch, f1, 1.0)
  }

  val articles = "\\b(a|an|the)\\b".r
  val moreThanOneSpace = "\\s{2,}".r
  def assignCreditSquadScalaVersion(predict: String, golds: Seq[String]): ((Double, Double, Double), Double) = {
    def normalizeString(s: String) = {
      def lowerCase(s: String): String = s.toLowerCase
      def noPunctuation(s: String): String = s.replaceAll("[\\Q][(){},.;!?<>%\\E]", "")
      def noArticles(s: String): String = articles.replaceAllIn(s, " ")
      def removeExtraWhitespace(s: String): String = moreThanOneSpace.replaceAllIn(s, " ")
      removeExtraWhitespace(noArticles(noPunctuation(lowerCase(s.trim)).trim).trim)
    }

    def exactMatch(p: String, g: String): Double = if (normalizeString(p) == normalizeString(g)) 1.0 else 0.0

    def f1Score(p: String, g: String): (Double, Double, Double) = {
      val pN = normalizeString(p)
      val qN = normalizeString(g)
      val pNormalized = pN.split("\\s")
      val gNormalized = qN.split("\\s")
      //      println("pNormalized: " + pNormalized.toSeq)
      //      println("gNormalized: " + gNormalized.toSeq)
      val pWordFreqMap = pNormalized.groupBy(a => a).map { case (k, v) => k -> v.length }
      val gWordFreqMap = gNormalized.groupBy(a => a).map { case (k, v) => k -> v.length }
      val numSame = pNormalized.toSet.intersect(gNormalized.toSet).toList.map(i => scala.math.min(pWordFreqMap(i), gWordFreqMap(i))).sum
      //      println("numSame: " + numSame)
      if (numSame == 0) {
        (0.0, 0.0, 0.0)
      } else {
        val Pre = numSame.toDouble / pNormalized.length
        val Rec = numSame.toDouble / gNormalized.length
        //        println("Pre: " + Pre)
        //        println("Rec: " + Rec)
        val f1 = 2 * Pre * Rec / (Pre + Rec)
        (f1, Pre, Rec)
      }
    }

    val bestF1PR = golds.map(g => f1Score(predict, g)).maxBy(_._1)
    val bestEM = golds.map(g => exactMatch(predict, g)).max
    bestF1PR -> bestEM
  }

  def printMemoryDetails() = {
    val mb = 1024 * 1024

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

  // sentence similarity stuff
  //        val documentList = trainReader.instances.flatMap{_.paragraphs.map{_.context}}
  //        val tfIdf = new TfIdf(documentList)
  MCTestBaseline.readStopWords()
  val stopwords = MCTestBaseline.stopWords.asScala.toSet
  assert(stopwords.size > 20)
  def getSimilarity(seq1: Seq[Constituent], seq2: Seq[Constituent], document: String): Double = {
    //          MCTestBaseline.ScoreAnswers(normalize(seq1).toArray, normalize(seq2).mkString(" "), Array(""), MCTestBaseline.stopWords).head
    val set2Normalized = normalize(seq2)
    val set1Normalized = normalize(seq1)
    //          normalize(seq1).intersect(set2Normalized).size.toDouble / set1Normalized.size
    normalize(seq1).intersect(normalize(seq2)).size
    //          normalize(seq1).intersect(normalize(seq2)).map(w => tfIdf.score(w, document)).sum
  }

  def normalize(seq: Seq[Constituent]) = seq.map(_.getLabel.trim).toSet.diff(stopwords)

  def getSentenceScores(p: Paragraph, q: Question): Seq[(Int, Double)] = {
    val questionLemmaCons = q.qTAOpt.get.getView(ViewNames.LEMMA).getConstituents.asScala.toList
    val lemmaCons = p.contextTAOpt.get.getView(ViewNames.LEMMA).getConstituents.asScala.toList
    //          println("goldAnswerSenId:  " + goldAnswerSenId)
    lemmaCons.groupBy(_.getSentenceId).map {
      case (id, consList) =>
        // calculate similarity between the constituents and the question
        id -> getSimilarity(consList, questionLemmaCons, p.context)
    }.toSeq.sortBy(-_._2)
  }

  /** saves the result of query on disk as text file */
  val staticCache = "other/elasticStaticCache/"
  def staticCacheLucene(question: String, focus: String, searchHitSize: Int): Seq[(String, Double)] = {
    val stringKey = "elasticWebParagraph:" + question + "-focus:" + focus + "-topK:" + searchHitSize + Constants.indexNames.keySet
    val cacheKey = (util.Arrays.toString(DigestUtils.sha1(stringKey)) + ".txt").replaceAll("\\s", "")
    //println("CacheKey: " + cacheKey)
    val f = new File(staticCache + cacheKey)

    def callLuceneServer: Seq[(String, Double)] = {
      //println("extracting the knowledge from remote server. . . ")
      // danielk: commenting for now, since I wanna save some time
      /*
      val results = extract(question, focus, searchHitSize)
      val cacheValue = JsArray(results.map { case (key, value) => JsArray(Seq(JsString(key), JsNumber(value))) })

      import java.io._
      val pw = new PrintWriter(f)
      pw.write(cacheValue.toString())
      pw.close()
      results
      */
      Seq.empty
    }

    if (f.exists()) {
      //println("fetching knowledge from cache . . . .")
      val soutceFile = Source.fromFile(f)
      val jsonString = soutceFile.getLines().mkString("")
      soutceFile.close()
      val json = Json.parse(jsonString)
      val a = json.as[JsArray].value.map { resultTuple =>
        val tupleValues = resultTuple.as[JsArray]
        val key = tupleValues.head.as[JsString].value
        val score = tupleValues(1).as[JsNumber].value
        key -> score.toDouble
      }
      val b = if (a.isEmpty) {
        //println("Because the document is empty, we're calling the server . . . ")
        callLuceneServer
      } else {
        a
      }
      if (b.isEmpty) {
        //println("nope! Still empty . . .")
      }
      b
    } else {
      callLuceneServer
    }
  }

  lazy val scienceTermsMap = {
    val f = Source.fromFile(new File("other/topicalities.tsv"))
    val map = f.getLines().toList.drop(1).map { l =>
      val split = l.split("\t")
      split(3) -> split(0).toDouble
    }
    f.close()
    map
  }.toMap

  def myLuceneSolver(question: String, focusSet: Seq[String]): Seq[(Double, Int)] = {
    println("questin: " + question + "  options: " + focusSet)
    val scores = focusSet.map(f => myLuceneSolverPerFocus(question, f, 200)).zipWithIndex
    if (scores.isEmpty) {
      val maxScore = scores.maxBy(_._1)
      scores.filter { _._1 == maxScore._1 }
    } else {
      println("Empty lucene response ..... ")
      Seq.empty
    }
  }

  def myLuceneSolverPerFocus(question: String, focus: String, searchHitSize: Int): Double = {
    val cacheKey = (util.Arrays.toString(DigestUtils.sha1("elasticWebParagraph:" +
      question + "-focus:" + focus + "-topK:" + searchHitSize)) + ".txt").replaceAll("\\s", "")
    println("cache key: " + focus)
    println("question: " + question)
    val f = new File(staticCache + cacheKey)

    def extractAndSaveKnowledge: Double = {
      println("extracting the knowledge from remote server. . . ")
      val results = extract(question, focus, searchHitSize)
      val cacheValue = JsArray(results.map { case (key, value) => JsArray(Seq(JsString(key), JsNumber(value))) })

      import java.io._
      val pw = new PrintWriter(f)
      pw.write(cacheValue.toString())
      pw.close()
      if (!results.isEmpty) results.unzip._2.max else -100.0
    }

    if (f.exists()) {
      val soutceFile = Source.fromFile(f)
      val jsonString = soutceFile.getLines().mkString("")
      soutceFile.close()
      val json = Json.parse(jsonString)
      val a = json.as[JsArray].value.map { resultTuple =>
        val tupleValues = resultTuple.as[JsArray]
        val key = tupleValues.head.as[JsString].value
        val score = tupleValues(1).as[JsNumber].value
        key -> score.toDouble
      }
      if (!a.isEmpty) a.unzip._2.max else {
        extractAndSaveKnowledge
      }
    } else {
      extractAndSaveKnowledge
    }
  }
}
