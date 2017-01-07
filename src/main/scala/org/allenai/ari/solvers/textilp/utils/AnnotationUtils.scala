package org.allenai.ari.solvers.textilp.utils

import java.util.Properties
import java.util.regex.Pattern

import edu.illinois.cs.cogcomp.core.datastructures.ViewNames
import edu.illinois.cs.cogcomp.core.datastructures.textannotation.{Constituent, TextAnnotation}
import edu.illinois.cs.cogcomp.core.utilities.SerializationHelper
import edu.illinois.cs.cogcomp.core.utilities.configuration.{Configurator, ResourceManager}
import edu.illinois.cs.cogcomp.curator.CuratorFactory
import edu.illinois.cs.cogcomp.pipeline.common.PipelineConfigurator
import edu.illinois.cs.cogcomp.pipeline.common.PipelineConfigurator._
import edu.illinois.cs.cogcomp.pipeline.main.PipelineFactory
import edu.illinois.cs.cogcomp.saulexamples.nlp.QuestionTypeClassification.QuestionTypeAnnotator
import org.allenai.ari.solvers.textilp.utils.WikiUtils.WikiDataProperties
import org.allenai.ari.solvers.textilp.{Paragraph, Question, TopicGroup}
import org.allenai.common.cache.JsonQueryCache
import redis.clients.jedis.{JedisPool, Protocol}
import spray.json.DefaultJsonProtocol._

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/** a dummy redis client for when redis is not supposed to be used */
object DummyRedisClient extends JsonQueryCache[String]("", new JedisPool()) {
  override def get(key: String) = None
  override def put(key: String, value: String): Unit = {}
}

sealed trait AnnotationLevel {}

// just tokenization
case object Tokenization extends AnnotationLevel
// Tokens, NER, POS, Chunk
case object Basic extends AnnotationLevel
// ???
case object Intermediate extends AnnotationLevel
// All possible annotations
case object Advanced extends AnnotationLevel

class AnnotationUtils {

  // redis cache for annotations
  lazy val synchronizedRedisClient = if (Constants.useRedisCachingForAnnotation) {
    JsonQueryCache[String]("", "localhost", Protocol.DEFAULT_PORT, Protocol.DEFAULT_TIMEOUT)
  } else {
    // use the dummy client, which always returns None for any query (and not using any Redis)
    DummyRedisClient
  }

  val viewsToDisable = Set(USE_SRL_NOM, USE_SRL_VERB, USE_STANFORD_DEP, USE_STANFORD_PARSE)
  val viewsToAdd = Seq(ViewNames.POS, ViewNames.LEMMA, ViewNames.NER_CONLL, ViewNames.NER_ONTONOTES,
    ViewNames.SHALLOW_PARSE/*, ViewNames.QUANTITIES*/)

  lazy val pipelineService = {
    println("Starting to build the pipeline service . . . ")
    val settings = new Properties()
    settings.setProperty("cacheDirectory", "annotation-cache-textilp")
    //settings.setProperty("disableCache", Configurator.TRUE)
    viewsToDisable.foreach{ key => settings.setProperty(key.value, Configurator.FALSE) }
    val rm = new ResourceManager(settings)
    //viewsToDisable.foreach { v => settings.setProperty(v.key, Configurator.FALSE) }
    val config = new PipelineConfigurator().getConfig(rm)
    val service = PipelineFactory.buildPipeline(config)
    println("Done with building the pipeline service . . . ")
    service
  }

  lazy val curatorService = {
    //val settings = new Properties()
    //settings.setProperty("cacheDirectory", "annotation-cache-textilp")
    //settings.setProperty("disableCache", Configurator.TRUE)
    //viewsToDisable.foreach{ key => settings.setProperty(key.value, Configurator.FALSE) }
    //val rm = new ResourceManager(settings)
    //viewsToDisable.foreach { v => settings.setProperty(v.key, Configurator.FALSE) }
    //val config = new CuratorConfigurator().getConfig(rm)
    CuratorFactory.buildCuratorClient()
  }

  lazy val questionTypeClassification = new QuestionTypeAnnotator()


  def annotate(string: String): TextAnnotation = {
    val cacheKey = "*TextAnnotations:" + viewsToDisable.mkString("*") + viewsToAdd.mkString("*") + string
    val redisAnnotation = synchronizedRedisClient.get(cacheKey)
    if (redisAnnotation.isDefined) {
      SerializationHelper.deserializeFromJson(redisAnnotation.get)
    } else {
      //val textAnnotation = pipelineService.createAnnotatedTextAnnotation("", "", string)
      val textAnnotation = pipelineService.createBasicTextAnnotation("", "", string)
      viewsToAdd.foreach{ vu => pipelineService.addView(textAnnotation, vu) }
      //if (withQuantifier) quantifierAnnotator.addView(textAnnotation)
      synchronizedRedisClient.put(cacheKey, SerializationHelper.serializeToJson(textAnnotation))
      textAnnotation
    }
  }

  //lazy val quantifierAnnotator = new Quantifier()

  def annotateInstance(tg: TopicGroup): TopicGroup = {
    val annotatedParagraphs = tg.paragraphs.map { p =>
      val annotatedContext = pipelineService.createBasicTextAnnotation("", "", p.context)
      val annotatedQuestions = p.questions.map { q =>
        val ta = pipelineService.createBasicTextAnnotation("", "", q.questionText)
        Question(q.questionText, q.questionId, q.answers, Some(ta))
      }
      Paragraph(p.context, annotatedQuestions, Some(annotatedContext))
    }
    TopicGroup(tg.title, annotatedParagraphs)
  }

  // wikifier annotations for question candidate generation


//  val wikifierAnnotations = {
//    val lines = Source.fromFile("other/squadDev_wikifier.txt").getLines().mkString
//
//    lines.split("\n\n").map{ jsonString =>
//      println(jsonString)
//      val ta = SerializationHelper.deserializeFromJson(jsonString)
//      println(ta.getAvailableViews)
//    }
//    1.0
//  }

  import redis.clients.jedis.Protocol
  import spray.json.DefaultJsonProtocol._
  lazy val wikifierRedis = JsonQueryCache[String]("", "localhost", Protocol.DEFAULT_PORT, Protocol.DEFAULT_TIMEOUT)

  def processSQuADWithWikifierAndPutRedis(reader: SQuADReader) = {
    reader.instances.zipWithIndex.foreach {
      case (ins, idx) =>
        println("Idx: " + idx + " / ratio: " + idx * 1.0 / reader.instances.size)
        ins.paragraphs.foreach { p =>
          val ta = curatorService.createBasicTextAnnotation("", "", p.context)
          curatorService.addView(ta, ViewNames.WIKIFIER)
          val json = SerializationHelper.serializeToJson(ta)
          wikifierRedis.put(ta.getText, json)
          p.questions.foreach { q =>
            val ta = curatorService.createBasicTextAnnotation("", "", q.questionText)
            curatorService.addView(ta, ViewNames.WIKIFIER)
            val json = SerializationHelper.serializeToJson(ta)
            wikifierRedis.put(ta.getText, json)
          }
        }
    }
  }

  def processSQuADWithWikifier(reader: SQuADReader) = {
    import java.io._
    val pw = new PrintWriter(new File("squadTrain_wikifier.txt" ))
    reader.instances.zipWithIndex.foreach {
      case (ins, idx) =>
        println("Idx: " + idx + " / ratio: " + idx * 1.0 / reader.instances.size)
        ins.paragraphs.foreach { p =>
          val ta = curatorService.createBasicTextAnnotation("", "", p.context)
          curatorService.addView(ta, ViewNames.WIKIFIER)
          val json = SerializationHelper.serializeToJson(ta)
          pw.write(json + "\n\n")
          p.questions.foreach { q =>
            val ta = curatorService.createBasicTextAnnotation("", "", q.questionText)
            curatorService.addView(ta, ViewNames.WIKIFIER)
            val json = SerializationHelper.serializeToJson(ta)
            pw.write(json + "\n\n")
          }
        }
    }
    pw.close()
  }


  // processes questions and paragraphs together (hence "jointly")
  def processSQuADWithWikifierJointly(reader: SQuADReader) = {
    import java.io._
    val pw = new PrintWriter(new File("squadTrain_wikifier.txt" ))
    reader.instances.zipWithIndex.foreach {
      case (ins, idx) =>
        println("Idx: " + idx + " / ratio: " + idx * 1.0 / reader.instances.size)
        ins.paragraphs.foreach { p =>

          val jointText = (p.context + p.questions.map {_.questionText}).mkString(" ")

          val ta = curatorService.createBasicTextAnnotation("", "", p.context)
          curatorService.addView(ta, ViewNames.WIKIFIER)
          val json = SerializationHelper.serializeToJson(ta)
          pw.write(json + "\n\n")
          p.questions.foreach { q =>
            val ta = curatorService.createBasicTextAnnotation("", "", q.questionText)
            curatorService.addView(ta, ViewNames.WIKIFIER)
            val json = SerializationHelper.serializeToJson(ta)
            pw.write(json + "\n\n")
          }
        }
    }
    pw.close()
  }

  def verifyWikifierAnnotationsOnDisk(reader: SQuADReader) = {
    reader.instances.zipWithIndex.foreach {
      case (ins, idx) =>
        println("Idx: " + idx + " / ratio: " + idx * 1.0 / reader.instances.size)
        ins.paragraphs.foreach { p =>
          //val ta = curatorService.createBasicTextAnnotation("", "", p.context)
          //curatorService.addView(ta, ViewNames.WIKIFIER)
          //val json = SerializationHelper.serializeToJson(ta)
          //wikifierRedis.put(p.context, json)
          val redisOutput = wikifierRedis.get(p.context)
          require(redisOutput.isDefined)
          p.questions.foreach { q =>
            //val ta = curatorService.createBasicTextAnnotation("", "", q.questionText)
            //curatorService.addView(ta, ViewNames.WIKIFIER)
            //val json = SerializationHelper.serializeToJson(ta)
            //wikifierRedis.put(ta.getText, json)
            val redisOutput = wikifierRedis.get(q.questionText)
            require(redisOutput.isDefined)
          }
        }
    }
  }

  import scala.collection.JavaConverters._

  val questionTerms = Set("which", "what", "where", "who", "whom", "how", "when", "why")
  val numberTriggers = Set("age")
  val tobe = Set("is", "are", "am", "do", "does", "did", "was", "were")

  def containsOffset(c: Constituent, offset: Int): Boolean = {
    c.getEndCharOffset >= offset && c.getStartCharOffset <= offset
  }

  def getTargetPhrase(question: Question, paragraph: Paragraph): Seq[String] = {
    require(question.qTAOpt.isDefined, throw new Exception("the annotation for this question doest not exist"))
    require(paragraph.contextTAOpt.isDefined, throw new Exception("the annotation for this paragraph doest not exist"))
    println(question.questionText)
    println(paragraph.context)
    val (shallowParseCons, paragraphQuantitiesCons, paragraphNerConsConll, paragraphNerConsOnto,
    wikiMentionsInText, wikiMentionsInQuestion, paragraphTokens) = extractVariables(question, paragraph)
//    println("wikiMentionsInText: " + wikiMentionsInText.map(a => a.getSurfaceForm + ":" + a.getLabel).mkString("/") )
//    println("wikiMentionsInQuestion: " + wikiMentionsInQuestion.map(a => a.getSurfaceForm + ":" + a.getLabel).mkString("/") )

    def questionPhraseCondition(a: Constituent): Boolean = {
      val splitSet = a.getSurfaceForm.toLowerCase.split(" ").toSet
      val fullText = a.getTextAnnotation.text.toLowerCase
      if(splitSet.contains("who")) {
        // who should not come with "those who" or "how many"
        if (fullText.contains("how many") || fullText.contains("those who")) {
          false
        }
        else {
          true
        }
      }
      else if(splitSet.contains("when")) {
        if (fullText.contains("what ") || fullText.contains("which") || fullText.contains("which") || fullText.contains("how") || fullText.contains("why")) {
          false
        }
        else {
          true
        }
      }
      else {
        splitSet.intersect(questionTerms).nonEmpty
      }
    }

    val questionConstituentOpt = shallowParseCons.zipWithIndex.collectFirst{ case (a, idx) if questionPhraseCondition(a) => a -> idx }

    println(shallowParseCons.map(a =>
      s"( ${a.getSurfaceForm} -> ${a.getLabel} )").mkString(" ")
    )
    println(questionConstituentOpt)
    val candidates = ArrayBuffer[Constituent]()
    val triggerTerm = if(questionConstituentOpt.isDefined) {
      val tailingTerms = questionConstituentOpt.get._1.getSurfaceForm.split(" ").tail
      val idx = questionConstituentOpt.get._2
      val triggerTerm = if(tailingTerms.nonEmpty) {
        tailingTerms.mkString(" ")
      }
      else {
        println("trigger terms was empty so we replaced it with the next constituents")
        if (shallowParseCons.length > idx + 1)
          shallowParseCons(idx + 1).getSurfaceForm
        else
          "" // example when this doesn't work: There were multiple students from Notre Dame who entered the Pro Football Hall of Fame, how many?
      }

      val wikiTriggerTermOpt = wikiMentionsInQuestion.find{ _.getStartSpan > questionConstituentOpt.get._1.getStartSpan }

      println("trigger term:  " + triggerTerm)
      val wikiTrigger = if(wikiTriggerTermOpt.isDefined) dropWikiURL(wikiTriggerTermOpt.get.getLabel) else triggerTerm
      println("wiki trigger: " + wikiTrigger)

      // general number trigger
      if(questionConstituentOpt.get._1.getSurfaceForm.contains(" age ") ||
        question.questionText.toLowerCase.contains("how many") ||
        question.questionText.toLowerCase.contains("how much") ||
        question.questionText.toLowerCase.contains("how large")
      ){
        println("general number trigger! ")
        numberExtractor(paragraphQuantitiesCons, paragraphNerConsOnto, candidates)
      }

//      // time trigger
//      if(questionConstituentOpt.get._1.getSurfaceForm.contains("time")
//      ){
//        println("time trigger! ")
//        candidates.++=:(paragraphQuantitiesCons.filter(_.getLabel.contains("TIME")))
//      }

      // date trigger
      if(questionConstituentOpt.get._1.getSurfaceForm.contains(" date ") ||
        questionConstituentOpt.get._1.getSurfaceForm.toLowerCase.trim.contains("when") ||
        questionConstituentOpt.get._1.getSurfaceForm.toLowerCase.trim.contains("what year") ||
        questionConstituentOpt.get._1.getSurfaceForm.toLowerCase.trim.contains("what time")){
        println("date trigger! ")
        println("time trigger! ")
        dateExtractor(paragraphQuantitiesCons, paragraphNerConsOnto, candidates)
      }

      // language trigger
      if(questionConstituentOpt.get._1.getSurfaceForm.toLowerCase.trim.contains("language")){
        println("language trigger! ")
        languageExtractor(paragraphNerConsOnto, candidates)
      }

      // nationality trigger
      if(questionConstituentOpt.get._1.getSurfaceForm.toLowerCase.trim.contains("nationality")){
        println("nationality trigger! ")
        nationalityExtractor(paragraphNerConsOnto, candidates)
      }

      // percent trigger
      if(questionConstituentOpt.get._1.getSurfaceForm.toLowerCase.trim.contains("percent")){
        println("percent trigger! ")
        percentageExtractor(paragraphNerConsOnto, candidates)
      }

      // money trigger
      if(questionConstituentOpt.get._1.getSurfaceForm.toLowerCase.trim.contains("money") ||
        questionConstituentOpt.get._1.getSurfaceForm.toLowerCase.trim.contains("currency")){
        println("money trigger! ")
        moneyExtractor(paragraphNerConsOnto, candidates)
      }

      // person trigger
      if(( questionConstituentOpt.get._1.getSurfaceForm.toLowerCase.trim.contains("who") &&
        !question.questionText.contains("how many") ) ||
        questionConstituentOpt.get._1.getSurfaceForm.toLowerCase.trim.contains("which individual") ||
        questionConstituentOpt.get._1.getSurfaceForm.toLowerCase.trim.contains("which person") ||
        questionConstituentOpt.get._1.getSurfaceForm.toLowerCase.trim.contains("what individual") ||
        questionConstituentOpt.get._1.getSurfaceForm.toLowerCase.trim.contains("what person") ){
        println("person trigger! ")
        personExtractor(question, paragraph, candidates)

        // organization
        if(candidates.isEmpty) {
          candidates.++=:(paragraphNerConsConll.filter(_.getLabel.contains("ORG")))
        }
      }

      // which trigger
      if((questionConstituentOpt.get._1.getSurfaceForm.toLowerCase.trim.contains("which") ||
        questionConstituentOpt.get._1.getSurfaceForm.toLowerCase.trim.contains("what")) &&
        candidates.nonEmpty ) {
        println("which trigger . . .")

        // which location
        if(triggerTerm.contains("location")) {
          locationExtractor(paragraphNerConsConll, paragraphNerConsOnto, candidates)
        }

        // institute
        if(triggerTerm.contains("institute") || triggerTerm.contains("company")) {
          instituteExtractor(paragraphNerConsConll, paragraphNerConsOnto, candidates)
        }

        // which entity
        if(triggerTerm.contains("entity")) {
          entityExtractor(paragraphNerConsConll, paragraphNerConsOnto, candidates)
        }

        // to-be triggers
        if(triggerTerm.split(" ").toSet.intersect(tobe).nonEmpty) {
          println("tobe trigger  . . . ")
        }

        // WikiData
        if(candidates.isEmpty) {
          extractGeneralWikiSubsetMentions(wikiMentionsInText, candidates, wikiTrigger)
        }
        else {
          println("Candidates are not empty: " + candidates.length)
        }
      }

      /*
      if(questionConstituent.get.getSurfaceForm.contains(" age ")){
        println("age trigger! ")
      }
      var triggerTerm = questionConstituent.get.getSurfaceForm.split(" ").tail.mkString(" ")
      println("trigger term:  " + triggerTerm)
      val triggerWordCategories = WikiUtils.extractRelevantCategories(triggerTerm)
      println("triggerWordCategories: " + triggerWordCategories)
      val wikiMentionCategories = wikiMentionsInText.map{ c => c -> WikiUtils.extractCategoryOfWikipage(c.getSurfaceForm) }
      println("Paragraph categories: " + wikiMentionCategories.mkString("\n"))
      val wikiMentions = wikiMentionCategories.filter{case (c, categories) => categories.toSet.intersect(triggerWordCategories.toSet).nonEmpty}
      println("Mentions with intersections" + wikiMentions.map(_._1))
      */
      triggerTerm
    }
    else {
      ""
    }
    val targetSet = Set(triggerTerm, "the" + triggerTerm.trim)
    postProcessCandidates(candidates, targetSet)
  }

  def candidateGenerationWithQuestionTypeClassification(question: Question, paragraph: Paragraph): Set[String] = {
    val candidates = ArrayBuffer[Constituent]()
    require(question.qTAOpt.isDefined, throw new Exception("the annotation for this question doest not exist"))
    require(paragraph.contextTAOpt.isDefined, throw new Exception("the annotation for this paragraph doest not exist"))
    println(question.questionText)
    println(paragraph.context)
    val pTA = paragraph.contextTAOpt.get
    val qTA = question.qTAOpt.get
    questionTypeClassification.addView(qTA)
    require(qTA.getAvailableViews.contains(questionTypeClassification.finalViewName))
    val fineType = qTA.getView(questionTypeClassification.finalViewName).getConstituents.get(0).getLabel
    val fineScore = qTA.getView(questionTypeClassification.finalViewName).getConstituents.get(0).getConstituentScore
    val coarseType = qTA.getView(questionTypeClassification.finalViewName).getConstituents.get(1).getLabel
    val coarseScore = qTA.getView(questionTypeClassification.finalViewName).getConstituents.get(1).getConstituentScore
    println("fileType: " + fineType  + s" ($fineScore) / coarseType: " + coarseType + s" ($coarseScore)")

    val (shallowParseCons, paragraphQuantitiesCons, paragraphNerConsConll, paragraphNerConsOnto,
    wikiMentionsInText, wikiMentionsInQuestion, paragraphTokens) = extractVariables(question, paragraph)

    fineType.toString match {
      case "LOC:city" =>  if (fineScore > -2.0) cityExtractor(candidates, wikiMentionsInText)
      case "LOC:country" => if (fineScore > -2.5) countryExtractor(candidates, wikiMentionsInText)
      case "LOC:mount" =>
        if (fineScore > -1.2) {
          extractMountain(candidates, paragraphNerConsOnto, wikiMentionsInText)
        }
      case "LOC:state" =>
        if (fineScore > -0.4) {
          stateExtractor(candidates, wikiMentionsInText)
        }
      case "LOC:other" =>
        if(fineScore> 0.0) {
          locationExtractor(paragraphNerConsConll, paragraphNerConsOnto, candidates)
          stateExtractor(candidates, wikiMentionsInText)
          extractMountain(candidates, paragraphNerConsOnto, wikiMentionsInText)
        }
      case "NUM:count" =>
        if( fineScore > -1.3 ) numberExtractor(paragraphQuantitiesCons, paragraphNerConsOnto, candidates)
      case "NUM:dist" =>
        if (fineScore > -1.5) numberExtractor(paragraphQuantitiesCons, paragraphNerConsOnto, candidates)
      case "NUM:weight" =>
        if (fineScore > -1) numberExtractor(paragraphQuantitiesCons, paragraphNerConsOnto, candidates)
      case "NUM:speed" =>
        if (fineScore > -2.5) numberExtractor(paragraphQuantitiesCons, paragraphNerConsOnto, candidates)
      case "NUM:period" =>
        if (fineScore > -1.5) numberExtractor(paragraphQuantitiesCons, paragraphNerConsOnto, candidates)
      case "NUM:perc" =>
        if (fineScore > -1.5) percentageExtractor(paragraphQuantitiesCons, candidates)
      case "NUM:date" =>
        if (fineScore > -1.5) dateExtractor(paragraphQuantitiesCons, paragraphNerConsOnto, candidates)
      case "NUM:money" =>
        if (fineScore > 0.0) moneyExtractor(paragraphQuantitiesCons, candidates)
      case "NUM:other" =>
        if (fineScore > -0.5) numberExtractor(paragraphQuantitiesCons, paragraphNerConsOnto, candidates)
      case "ABBR:exp" => if(fineScore > -0.5) Set.empty /*TODO*/ else Set.empty
      case "DESC:reason" => if(fineScore > 0.1) Set.empty /*TODO*/ else Set.empty
      case "ENTY:color" =>
        if(fineScore > -1.25) wikiDataInstanceOfExtractor(candidates, wikiMentionsInText, WikiDataProperties.color)
      case "ENTY:food" =>
        if(fineScore > 2.5) wikiDataInstanceOfExtractor(candidates, wikiMentionsInText, WikiDataProperties.food)
      case "ENTY:other" =>
        if(fineScore > 1.0) entityExtractor(paragraphNerConsConll, paragraphNerConsOnto, candidates)
      case _ =>
    }

    if(candidates.isEmpty) {
      coarseType match {
        case "ABBR" =>
        case "DESC" =>
        case "ENTY" | "HUM" =>
          if (coarseScore > 0.5) {
            entityExtractor(paragraphNerConsConll, paragraphNerConsOnto, candidates)
            wikiDataInstanceOfExtractor(candidates, wikiMentionsInText, WikiDataProperties.color)
            wikiDataInstanceOfExtractor(candidates, wikiMentionsInText, WikiDataProperties.food)
            wikiDataInstanceOfExtractor(candidates, wikiMentionsInText, WikiDataProperties.person)
            wikiDataInstanceOfExtractor(candidates, wikiMentionsInText, WikiDataProperties.country)
          }
        case "NUM" =>
          if (coarseScore > 0.5) {
            numberExtractor(paragraphQuantitiesCons, paragraphNerConsOnto, candidates)
            percentageExtractor(paragraphQuantitiesCons, candidates)
            dateExtractor(paragraphQuantitiesCons, paragraphNerConsOnto, candidates)
            moneyExtractor(paragraphQuantitiesCons, candidates)
            candidates.++=:(paragraphNerConsOnto.filter(_.getLabel.contains("ORDINAL")))
          }
        case "LOC" =>
          if (coarseScore > -0.5) {
            cityExtractor(candidates, wikiMentionsInText)
            countryExtractor(candidates, wikiMentionsInText)
            extractMountain(candidates, paragraphNerConsOnto, wikiMentionsInText)
            stateExtractor(candidates, wikiMentionsInText)
          }
      }
    }

    candidates.map(_.getSurfaceForm.trim).toSet
  }

  def getCandidateAnswer(contextTA: TextAnnotation): Set[String] = {
    val candidates = ArrayBuffer[Constituent]()
    println(contextTA.getText)
    val pTA = contextTA
    val nounPhrases = contextTA.getView(ViewNames.SHALLOW_PARSE).getConstituents.asScala.
      filter { ch => ch.getLabel.contains("N") || ch.getLabel.contains("J") || ch.getLabel.contains("V") }.map(_.getSurfaceForm)
    val quotationExtractionPattern = "([\"'])(?:(?=(\\\\?))\\2.)*?\\1".r
    val stringsInsideQuotationMark = quotationExtractionPattern.findAllIn(contextTA.text)
    val paragraphNerConsConll = contextTA.getView(ViewNames.NER_CONLL).getConstituents.asScala.toList
    val paragraphNerConsOnto = contextTA.getView(ViewNames.NER_ONTONOTES).getConstituents.asScala.toList
    val paragraphWikiAnnotationOpt = wikifierRedis.get(contextTA.getText)
    val wikiMentionsInText = SerializationHelper.deserializeFromJson(paragraphWikiAnnotationOpt.get).getView(ViewNames.WIKIFIER).getConstituents.asScala.toList
    val quant = if(contextTA.hasView(ViewNames.QUANTITIES)) {
      contextTA.getView(ViewNames.QUANTITIES).getConstituents.asScala.map(_.getSurfaceForm)
    }
    else {
      Seq.empty
    }
    val paragraphQuantitiesCons = List.empty //paragraph.contextTAOpt.get.getView(ViewNames.QUANTITIES).getConstituents.asScala.toList
    val p = "-?\\d+".r // regex for finding all the numbers
    val numbers = p.findAllIn(contextTA.text)
    extractMountain(candidates, paragraphNerConsOnto, wikiMentionsInText)
    cityExtractor(candidates, wikiMentionsInText)
    countryExtractor(candidates, wikiMentionsInText)
    stateExtractor(candidates, wikiMentionsInText)
    numberExtractor(paragraphQuantitiesCons, paragraphNerConsOnto, candidates)
    percentageExtractor(paragraphQuantitiesCons, candidates)
    dateExtractor(paragraphQuantitiesCons, paragraphNerConsOnto, candidates)
    moneyExtractor(paragraphQuantitiesCons, candidates)
    candidates.++=:(paragraphNerConsOnto.filter(_.getLabel.contains("ORDINAL")))
    entityExtractor(paragraphNerConsConll, paragraphNerConsOnto, candidates)
    wikiDataInstanceOfExtractor(candidates, wikiMentionsInText, WikiDataProperties.color)
    wikiDataInstanceOfExtractor(candidates, wikiMentionsInText, WikiDataProperties.food)
    wikiDataInstanceOfExtractor(candidates, wikiMentionsInText, WikiDataProperties.person)
    wikiDataInstanceOfExtractor(candidates, wikiMentionsInText, WikiDataProperties.country)
    candidates.++(nounPhrases ++ quant ++ paragraphNerConsConll ++ paragraphNerConsOnto ++ numbers ++ stringsInsideQuotationMark)
    candidates.map(_.getSurfaceForm.trim).toSet
  }

  def postProcessCandidates(candidates: ArrayBuffer[Constituent], targetSet: Set[String]): Seq[String] = {
    val candidatesSet = candidates.map(_.getSurfaceForm.trim).toSet
    // println("candidatesSet= " + candidatesSet)
    def setContainsIt(str: String): Boolean = {
      candidatesSet.diff(Set(str)).exists { s => s.contains(str) }
    }
    val uniqueCandidateSet = candidatesSet.collect { case a if !setContainsIt(a) => a }
    val uniqueCandidateSetWithoutQuestionTarget = uniqueCandidateSet.diff(targetSet)
    println("uniqueCandidateSetWithoutQuestionTarget: " + uniqueCandidateSetWithoutQuestionTarget)
    uniqueCandidateSetWithoutQuestionTarget.toSeq
  }


  def extractMountain(candidates: ArrayBuffer[Constituent], paragraphNerConsOnto: List[Constituent], wikiMentionsInText: List[Constituent]): Unit = {
    // WikiData
    wikiDataInstanceOfExtractor(candidates, wikiMentionsInText, WikiDataProperties.mountain)

    // NER
    def ontonotesLocationFilter(in: String): Boolean = in.contains("GPE")
    val ontonotesCandidates = paragraphNerConsOnto.filter(a => ontonotesLocationFilter(a.getLabel))
    candidates.++=:(ontonotesCandidates)
  }

  def stateExtractor(candidates: ArrayBuffer[Constituent], wikiMentionsInText: List[Constituent]): Unit = {
    wikiDataInstanceOfExtractor(candidates, wikiMentionsInText, WikiDataProperties.state)
  }

  def countryExtractor(candidates: ArrayBuffer[Constituent], wikiMentionsInText: List[Constituent]): Unit = {
    wikiDataInstanceOfExtractor(candidates, wikiMentionsInText, WikiDataProperties.country)
  }

  def cityExtractor(candidates: ArrayBuffer[Constituent], wikiMentionsInText: List[Constituent]): Unit = {
    wikiDataInstanceOfExtractor(candidates, wikiMentionsInText, WikiDataProperties.city)
  }

  def extractGeneralWikiSubsetMentions(wikiMentionsInText: List[Constituent], candidates: ArrayBuffer[Constituent], wikiTrigger: String): Unit = {
    println("using Wiki mentions . . .  ")
    //println(wikiMentionsInText)
    val wikiMentions = wikiMentionsInText.filter { c =>
      WikiUtils.wikiAskQuery(dropWikiURL(c.getLabel), wikiTrigger, WikiDataProperties.instanceOf, 5) ||
        WikiUtils.wikiAskQuery(dropWikiURL(c.getLabel), wikiTrigger, WikiDataProperties.subclassOf, 5)
    }
    candidates.++=:(wikiMentions)
  }

  def entityExtractor(paragraphNerConsConll: List[Constituent], paragraphNerConsOnto: List[Constituent], candidates: ArrayBuffer[Constituent]): Unit = {
    // NER-Conll
    def conllLocationFilter(in: String): Boolean = in.contains("ORG")
    candidates.++=:(paragraphNerConsConll.filter(a => conllLocationFilter(a.getLabel)))
    println("surface: " + paragraphNerConsConll)
    println("labels: " + paragraphNerConsConll.map(_.getLabel))
    println("conll filtered: " + paragraphNerConsConll.filter(a => conllLocationFilter(a.getLabel)))

    // NER-Ontonotes
    def ontonotesLocationFilter(in: String): Boolean = in.contains("ORG")
    val ontonotesCandidates = paragraphNerConsOnto.filter(a => ontonotesLocationFilter(a.getLabel))
    candidates.++=:(ontonotesCandidates)
    println(paragraphNerConsOnto)
  }

  def instituteExtractor(paragraphNerConsConll: List[Constituent], paragraphNerConsOnto: List[Constituent], candidates: ArrayBuffer[Constituent]): Unit = {
    // NER-Conll
    def conllLocationFilter(in: String): Boolean = in.contains("ORG")
    candidates.++=:(paragraphNerConsConll.filter(a => conllLocationFilter(a.getLabel)))
    println("surface: " + paragraphNerConsConll)
    println("labels: " + paragraphNerConsConll.map(_.getLabel))
    println("conll filtered: " + paragraphNerConsConll.filter(a => conllLocationFilter(a.getLabel)))

    // NER-Ontonotes
    def ontonotesLocationFilter(in: String): Boolean = in.contains("ORG")
    val ontonotesCandidates = paragraphNerConsOnto.filter(a => ontonotesLocationFilter(a.getLabel))
    candidates.++=:(ontonotesCandidates)
    println(paragraphNerConsOnto)
  }

  def locationExtractor(paragraphNerConsConll: List[Constituent], paragraphNerConsOnto: List[Constituent], candidates: ArrayBuffer[Constituent]): Unit = {
    // NER-Conll
    def conllLocationFilter(in: String): Boolean = in.contains("LOC") || in.contains("ORG")
    candidates.++=:(paragraphNerConsConll.filter(a => conllLocationFilter(a.getLabel)))
    println("surface: " + paragraphNerConsConll)
    println("labels: " + paragraphNerConsConll.map(_.getLabel))
    println("conll filtered: " + paragraphNerConsConll.filter(a => conllLocationFilter(a.getLabel)))

    // NER-Ontonotes
    def ontonotesLocationFilter(in: String): Boolean = in.contains("LOC") || in.contains("ORG") || in.contains("GPE") || in.contains("FAC")
    val ontonotesCandidates = paragraphNerConsOnto.filter(a => ontonotesLocationFilter(a.getLabel))
    candidates.++=:(ontonotesCandidates)
  }

  def numberExtractor(paragraphQuantitiesCons: List[Constituent], paragraphNerConsOnto: List[Constituent], candidates: ArrayBuffer[Constituent]): Unit = {
    candidates.++=:(paragraphQuantitiesCons.filter(_.getLabel.contains("Number")))

    // NER-Ontonotes
    candidates.++=:(paragraphNerConsOnto.filter(_.getLabel.contains("CARDINAL")))
    candidates.++=:(paragraphNerConsOnto.filter(_.getLabel.contains("QUANTITY")))
  }

  def personExtractor(question: Question, paragraph: Paragraph, candidates: ArrayBuffer[Constituent]): Unit = {
    val (shallowParseCons, paragraphQuantitiesCons, paragraphNerConsConll, paragraphNerConsOnto,
    wikiMentionsInText, wikiMentionsInQuestion, paragraphTokens) = extractVariables(question, paragraph)
    // NER-Conll
    candidates.++=:(paragraphNerConsConll.filter(_.getLabel.contains("PER")))
    println("surface: " + paragraphNerConsConll)
    println("labels: " + paragraphNerConsConll.map(_.getLabel))
    println("conll filtered: " + paragraphNerConsConll.filter(_.getLabel.contains("PER")))

    // NER-Ontonotes
    val ontonotesCandidates = paragraphNerConsOnto.filter(_.getLabel.contains("PERSON"))
    candidates.++=:(ontonotesCandidates)
    println(paragraphNerConsOnto)

    // add constituents with prefix
    val prefixSet = Seq("Father", "Rev.")
    val constituentsWithPrefix = prefixSet.flatMap { prefix =>
      ontonotesCandidates.map { c =>
        val target = prefix + " " + c.getSurfaceForm
        Pattern.quote(target).r.findAllIn(paragraph.context) -> target
      }
    }.filter {
      _._1.nonEmpty
    }.map { case (a, t) =>
      println(s"(s, e) = (${a.start}, ${a.end})  /  text length :" + paragraph.contextTAOpt.get.text.length)
      val charStart = a.start
      val charEnd = a.end - 1
      val consStart = paragraphTokens.find {
        containsOffset(_, charStart)
      }.getOrElse(throw new Exception("didn't find the token"))
      val consEnd = paragraphTokens.find {
        containsOffset(_, charEnd)
      }.getOrElse(throw new Exception("didn't find the token"))
      new Constituent(t, "candidates", paragraph.contextTAOpt.get, consStart.getStartSpan, consEnd.getEndSpan)
    }

    println("constituentsWithPrefix = " + constituentsWithPrefix)
    candidates.++=:(constituentsWithPrefix)

    // WikiData
    if (candidates.isEmpty) {
      wikiDataInstanceOfExtractor(candidates, wikiMentionsInText, WikiDataProperties.person)
    }
  }

  def wikiDataInstanceOfExtractor(candidates: ArrayBuffer[Constituent], wikiMentionsInText: List[Constituent], targetType: String): Unit = {
    val wikiMentions = wikiMentionsInText.filter { c =>
      WikiUtils.wikiAskQuery(dropWikiURL(c.getLabel), targetType, WikiDataProperties.instanceOf, 5)
    }
    candidates.++=:(wikiMentions)
  }

  def moneyExtractor(paragraphNerConsOnto: List[Constituent], candidates: ArrayBuffer[Constituent]): Unit = {
    // NER-Ontonotes
    candidates.++=:(paragraphNerConsOnto.filter(_.getLabel.contains("MONEY")))
  }

  def percentageExtractor(paragraphNerConsOnto: List[Constituent], candidates: ArrayBuffer[Constituent]): Unit = {
    // NER-Ontonotes
    candidates.++=:(paragraphNerConsOnto.filter(_.getLabel.contains("PERCENT")))
  }

  def nationalityExtractor(paragraphNerConsOnto: List[Constituent], candidates: ArrayBuffer[Constituent]): Unit = {
    // NER-Ontonotes
    candidates.++=:(paragraphNerConsOnto.filter(_.getLabel.contains("NORP")))
  }

  def languageExtractor(paragraphNerConsOnto: List[Constituent], candidates: ArrayBuffer[Constituent]): Unit = {
    // NER-Conll
    candidates.++=:(paragraphNerConsOnto.filter(_.getLabel.contains("LANGUAGE")))
  }

  def dateExtractor(paragraphQuantitiesCons: List[Constituent], paragraphNerConsOnto: List[Constituent], candidates: ArrayBuffer[Constituent]): Unit = {
    candidates.++=:(paragraphQuantitiesCons.filter(_.getLabel.contains("TIME")))
    candidates.++=:(paragraphQuantitiesCons.filter(_.getLabel.contains("Date")))

    // NER-ontonotes
    candidates.++=:(paragraphNerConsOnto.filter(_.getLabel.contains("DATE")))
  }

  def extractVariables(question: Question, paragraph: Paragraph): (List[Constituent], List[Constituent], List[Constituent],
    List[Constituent], List[Constituent], List[Constituent], List[Constituent]) = {
    val paragraphTokenView = paragraph.contextTAOpt.get.getView(ViewNames.TOKENS)
    val paragraphTokens = paragraphTokenView.getConstituents.asScala.toList
    val shallowParseCons = question.qTAOpt.get.getView(ViewNames.SHALLOW_PARSE).getConstituents.asScala.toList
    val paragraphQuantitiesCons = List.empty //paragraph.contextTAOpt.get.getView(ViewNames.QUANTITIES).getConstituents.asScala.toList
    val paragraphNerConsConll = paragraph.contextTAOpt.get.getView(ViewNames.NER_CONLL).getConstituents.asScala.toList
    val paragraphNerConsOnto = paragraph.contextTAOpt.get.getView(ViewNames.NER_ONTONOTES).getConstituents.asScala.toList
    val questionWikiAnnotationOpt = wikifierRedis.get(question.questionText)
    val paragraphWikiAnnotationOpt = wikifierRedis.get(paragraph.context)
    require(questionWikiAnnotationOpt.isDefined, throw new Exception("The wiki annotation for question is not defined"))
    require(paragraphWikiAnnotationOpt.isDefined, throw new Exception("The wiki annotation for paragraph is not defined"))
    val wikiMentionsInText = SerializationHelper.deserializeFromJson(paragraphWikiAnnotationOpt.get).getView(ViewNames.WIKIFIER).getConstituents.asScala.toList
    val wikiMentionsInQuestion = SerializationHelper.deserializeFromJson(questionWikiAnnotationOpt.get).getView(ViewNames.WIKIFIER).getConstituents.asScala.toList
    (shallowParseCons, paragraphQuantitiesCons, paragraphNerConsConll, paragraphNerConsOnto, wikiMentionsInText, wikiMentionsInQuestion, paragraphTokens)
  }

  def dropWikiURL(url: String): String = url.replace("http://en.wikipedia.org/wiki/", "")

}
