package org.allenai.ari.solvers.squad

import java.io.File
import java.util
import java.util.regex.Pattern

import edu.illinois.cs.cogcomp.core.datastructures.{IntPair, Pair, ViewNames}
import edu.illinois.cs.cogcomp.core.datastructures.textannotation.{Constituent, TextAnnotation, View}
import edu.illinois.cs.cogcomp.core.datastructures.trees.Tree
import edu.illinois.cs.cogcomp.core.utilities.SerializationHelper
import edu.illinois.cs.cogcomp.nlp.utilities.ParseUtils
import edu.illinois.cs.cogcomp.saulexamples.nlp.QuestionTypeClassification.QuestionTypeAnnotator
import org.allenai.ari.solvers.textilp.{Paragraph, Question}
import org.allenai.ari.solvers.textilp.utils.WikiUtils
import org.allenai.ari.solvers.textilp.utils.WikiUtils.WikiDataProperties
import org.allenai.common.cache.JsonQueryCache

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object CandidateGeneration {
  import scala.collection.JavaConverters._

  val questionTerms = Set("which", "what", "where", "who", "whom", "how", "when", "why")
  val numberTriggers = Set("age")
  val tobe = Set("is", "are", "am", "do", "does", "did", "was", "were")
  val personNamePrefix = Set("Father", "Rev.")

  lazy val questionTypeClassification = new QuestionTypeAnnotator()

  import redis.clients.jedis.Protocol
  import spray.json.DefaultJsonProtocol._
  lazy val wikifierRedis = JsonQueryCache[String]("", "localhost", Protocol.DEFAULT_PORT, Protocol.DEFAULT_TIMEOUT)

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

    val (questionConstituentOpt, triggerTerm, wikiTriggerTermOpt) = extractQuestionKeyQuestionTerms(shallowParseCons, wikiMentionsInQuestion)

    val candidates = ArrayBuffer[Constituent]()
    if(questionConstituentOpt.isDefined) {
      val wikiTrigger = if(wikiTriggerTermOpt.isDefined) dropWikiURL(wikiTriggerTermOpt.get.getLabel) else triggerTerm
      println("wiki trigger: " + wikiTrigger)

      // general number trigger
      if(questionConstituentOpt.get._1.getSurfaceForm.contains(" age ") ||
        questionNumberTrigger(question.questionText.toLowerCase)
      ){
        println("general number trigger! ")
        numberExtractor(paragraphQuantitiesCons, paragraphNerConsOnto, candidates)
      }

      // date trigger
      if(questionDateTrigger(questionConstituentOpt.get._1.getSurfaceForm.toLowerCase.trim)){
        println("date trigger! ")
        println("time trigger! ")
        dateExtractor(paragraphQuantitiesCons, paragraphNerConsOnto, candidates)
      }

      // language trigger
      if(questionLanguageTrigger(questionConstituentOpt.get._1.getSurfaceForm.toLowerCase.trim)){
        println("language trigger! ")
        languageExtractor(paragraphNerConsOnto, candidates)
      }

      // nationality trigger
      if(questionNationalityTrigger(questionConstituentOpt.get._1.getSurfaceForm.toLowerCase.trim)){
        println("nationality trigger! ")
        nationalityExtractor(paragraphNerConsOnto, candidates)
      }

      // percent trigger
      if(questionPercentTrigger(questionConstituentOpt.get._1.getSurfaceForm.toLowerCase.trim)){
        println("percent trigger! ")
        percentageExtractor(paragraphNerConsOnto, candidates)
      }

      // money trigger
      if(questionCurrencyTrigger(questionConstituentOpt.get._1.getSurfaceForm.toLowerCase.trim)){
        println("money trigger! ")
        moneyExtractor(paragraphNerConsOnto, candidates)
      }

      // person trigger
      if(questionPersonTrigger(questionConstituentOpt.get._1.getSurfaceForm.toLowerCase.trim)){
        println("person trigger! ")
        personExtractor(question, paragraph, candidates)

        // organization
        if(candidates.isEmpty) {
          candidates.++=:(paragraphNerConsConll.filter(a => conllOrg(a.getLabel)))
        }
      }

      // which trigger
      if(questionWhichWhatTrigger(questionConstituentOpt.get._1.getSurfaceForm.toLowerCase.trim) && candidates.nonEmpty ) {
        println("which trigger . . .")

        // which location
        if(questionLocationTrigger(triggerTerm)) {
          locationExtractor(paragraphNerConsConll, paragraphNerConsOnto, candidates)
        }

        // institute
        if( questionOrgTrigger(triggerTerm) ) {
          instituteExtractor(paragraphNerConsConll, paragraphNerConsOnto, candidates)
        }

        // which entity
        if( questionEntTrigger(triggerTerm)) {
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
    }
    val targetSet = Set(triggerTerm, "the" + triggerTerm.trim)
    postProcessCandidates(candidates, targetSet)
  }

  def extractQuestionKeyQuestionTerms(shallowParseCons: List[Constituent], wikiMentionsInQuestion: List[Constituent]): (Option[(Constituent, Int)], String, Option[Constituent]) = {
    val questionConstituentOpt: Option[(Constituent, Int)] = extractQuestionTypeInformation(shallowParseCons)

//    println(shallowParseCons.map(a =>
//      s"( ${a.getSurfaceForm} -> ${a.getLabel} )").mkString(" ")
//    )
//    println(questionConstituentOpt)

    val (triggerTerm, wikiTriggerTermOpt) = if (questionConstituentOpt.isDefined) {
      val tailingTerms = questionConstituentOpt.get._1.getSurfaceForm.split(" ").tail
      val idx = questionConstituentOpt.get._2
      val wikiTriggerTermOpt = wikiMentionsInQuestion.find {
        _.getStartSpan > questionConstituentOpt.get._1.getStartSpan
      }
      val tTerm = if (tailingTerms.nonEmpty) {
        tailingTerms.mkString(" ")
      }
      else {
//        println("trigger terms was empty so we replaced it with the next constituents")
        if (shallowParseCons.length > idx + 1)
          shallowParseCons(idx + 1).getSurfaceForm
        else
          "" // example when this doesn't work: There were multiple students from Notre Dame who entered the Pro Football Hall of Fame, how many?
      }
      tTerm -> wikiTriggerTermOpt
    }
    else {
      "" -> None
    }
    //println("trigger term: " + triggerTerm)
    (questionConstituentOpt, triggerTerm, wikiTriggerTermOpt)
  }

  def extractQuestionTypeInformation(shallowParseCons: List[Constituent]): Option[(Constituent, Int)] = {
    def questionPhraseCondition(a: Constituent): Boolean = {
      val splitSet = a.getSurfaceForm.toLowerCase.split(" ").toSet
      val fullText = a.getTextAnnotation.text.toLowerCase
      if (splitSet.contains("who")) {
        // who should not come with "those who" or "how many"
        if (fullText.contains("how many") || fullText.contains("those who")) {
          false
        }
        else {
          true
        }
      }
      else if (splitSet.contains("when")) {
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

    val questionConstituentOpt = shallowParseCons.zipWithIndex.collectFirst { case (a, idx) if questionPhraseCondition(a) => a -> idx }
    questionConstituentOpt
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
            candidates.++=:(paragraphNerConsOnto.filter(a => ontoOrdinal(a.getLabel)))
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
    val pTA = contextTA
    val nounPhrases = contextTA.getView(ViewNames.SHALLOW_PARSE).getConstituents.asScala.
      filter { ch => ch.getLabel.contains("N") || ch.getLabel.contains("J") || ch.getLabel.contains("V") }
    val quotationExtractionPattern = "([\"'])(?:(?=(\\\\?))\\2.)*?\\1".r
    val stringsInsideQuotationMark = quotationExtractionPattern.findAllIn(contextTA.text)
    val paragraphNerConsConll = contextTA.getView(ViewNames.NER_CONLL).getConstituents.asScala.toList
    val paragraphNerConsOnto = contextTA.getView(ViewNames.NER_ONTONOTES).getConstituents.asScala.toList
    val paragraphWikiAnnotationOpt = wikifierRedis.get(contextTA.getText)
    val wikiMentionsInText = SerializationHelper.deserializeFromJson(paragraphWikiAnnotationOpt.get).getView(ViewNames.WIKIFIER).getConstituents.asScala.toList
    val quant = if(false) {///if(contextTA.hasView(ViewNames.QUANTITIES)) {
      contextTA.getView(ViewNames.QUANTITIES).getConstituents.asScala
    }
    else {
      Seq.empty
    }
    val posAndChunkPatternCandidates = TextAnnotationPatternExtractor.extractPatterns(contextTA)
    val parseTreeCandidates = generateCandidates(contextTA)
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
    candidates.++=:(paragraphNerConsOnto.filter(a => ontoOrdinal(a.getLabel)))
    entityExtractor(paragraphNerConsConll, paragraphNerConsOnto, candidates)
    wikiDataInstanceOfExtractor(candidates, wikiMentionsInText, WikiDataProperties.color)
    wikiDataInstanceOfExtractor(candidates, wikiMentionsInText, WikiDataProperties.food)
    wikiDataInstanceOfExtractor(candidates, wikiMentionsInText, WikiDataProperties.person)
    wikiDataInstanceOfExtractor(candidates, wikiMentionsInText, WikiDataProperties.country)
    candidates.++=:(nounPhrases ++ quant ++ paragraphNerConsConll ++ paragraphNerConsOnto ++ parseTreeCandidates)
    (candidates.map(_.getSurfaceForm.trim) ++ numbers ++ stringsInsideQuotationMark ++ posAndChunkPatternCandidates).toSet
  }

  def extractPOSPatterns(vu: View): Unit = {
    val cons = vu.getConstituents.asScala
  }

  def extractChunkPatterns(): Unit = {

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
    val ontonotesCandidates = paragraphNerConsOnto.filter(a => ontoGPE(a.getLabel))
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
    val wikiMentions = wikiMentionsInText.filter { c => isSubsetWithWikiData(wikiTrigger, c.getLabel) }
    candidates.++=:(wikiMentions)
  }

  def isSubsetWithWikiData(wikiTrigger: String, candidateTerm: String): Boolean = {
    WikiUtils.wikiAskQuery(dropWikiURL(candidateTerm), wikiTrigger, WikiDataProperties.instanceOf, 5) ||
      WikiUtils.wikiAskQuery(dropWikiURL(candidateTerm), wikiTrigger, WikiDataProperties.subclassOf, 5)
  }

  def entityExtractor(paragraphNerConsConll: List[Constituent], paragraphNerConsOnto: List[Constituent], candidates: ArrayBuffer[Constituent]): Unit = {
    // NER-Conll
    candidates.++=:(paragraphNerConsConll.filter(a => conllOrg(a.getLabel)))

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
    candidates.++=:(paragraphNerConsConll.filter(a => conllLocationFilter(a.getLabel)))
    println("surface: " + paragraphNerConsConll)
    println("labels: " + paragraphNerConsConll.map(_.getLabel))
    println("conll filtered: " + paragraphNerConsConll.filter(a => conllLocationFilter(a.getLabel)))

    // NER-Ontonotes
    val ontonotesCandidates = paragraphNerConsOnto.filter(a => ontonotesLocationFilter(a.getLabel))
    candidates.++=:(ontonotesCandidates)
  }

  def numberExtractor(paragraphQuantitiesCons: List[Constituent], paragraphNerConsOnto: List[Constituent], candidates: ArrayBuffer[Constituent]): Unit = {
    // quantifier
    candidates.++=:(paragraphQuantitiesCons.filter(a => quantifierNumber(a.getLabel)))
    // NER-Ontonotes
    candidates.++=:(paragraphNerConsOnto.filter(a => ontoQuantOrCard(a.getLabel)))
  }

  def personExtractor(question: Question, paragraph: Paragraph, candidates: ArrayBuffer[Constituent]): Unit = {
    val (shallowParseCons, paragraphQuantitiesCons, paragraphNerConsConll, paragraphNerConsOnto,
    wikiMentionsInText, wikiMentionsInQuestion, paragraphTokens) = extractVariables(question, paragraph)
    // NER-Conll
    candidates.++=:(paragraphNerConsConll.filter(a => conllPerson(a.getLabel)))
    println("surface: " + paragraphNerConsConll)
    println("labels: " + paragraphNerConsConll.map(_.getLabel))
    println("conll filtered: " + paragraphNerConsConll.filter(a => conllPerson(a.getLabel)))

    // NER-Ontonotes
    val ontonotesCandidates = paragraphNerConsOnto.filter(a => ontoPerson(a.getLabel))
    candidates.++=:(ontonotesCandidates)
    println(paragraphNerConsOnto)

    // add constituents with prefix
    val constituentsWithPrefix = personNamePrefix.flatMap { prefix =>
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
    candidates.++=:(paragraphNerConsOnto.filter(a => ontoMoney(a.getLabel)))
  }

  def percentageExtractor(paragraphNerConsOnto: List[Constituent], candidates: ArrayBuffer[Constituent]): Unit = {
    // NER-Ontonotes
    candidates.++=:(paragraphNerConsOnto.filter(a => ontoPercent(a.getLabel)))
  }

  def nationalityExtractor(paragraphNerConsOnto: List[Constituent], candidates: ArrayBuffer[Constituent]): Unit = {
    // NER-Ontonotes
    candidates.++=:(paragraphNerConsOnto.filter(a => ontoNationality(a.getLabel)))
  }

  def languageExtractor(paragraphNerConsOnto: List[Constituent], candidates: ArrayBuffer[Constituent]): Unit = {
    // NER-Ontonotes
    candidates.++=:(paragraphNerConsOnto.filter(a => ontoLanguage(a.getLabel)))
  }

  def dateExtractor(paragraphQuantitiesCons: List[Constituent], paragraphNerConsOnto: List[Constituent], candidates: ArrayBuffer[Constituent]): Unit = {
    // quantifier
    candidates.++=:(paragraphQuantitiesCons.filter(a => quantifierDate(a.getLabel)))
    // NER-ontonotes
    candidates.++=:(paragraphNerConsOnto.filter(a => ontoDate(a.getLabel)))
  }

  def isItRankingQuestion(question: String): Boolean = {
    val p1 = "Where.*did.*rank.*\\?".r
    val p2 = "Where.*does.*rank.*\\?".r
    p1.findAllIn(question).nonEmpty || p2.findAllIn(question).nonEmpty
  }

  def conllPerson(str: String): Boolean = str.contains("PER")
  def conllLocation(str: String): Boolean = str.contains("LOC")
  def conllOrg(str: String): Boolean = str.contains("ORG")
  def conllGpe(str: String): Boolean = str.contains("GPE")
  def conllLocationFilter(in: String): Boolean = in.contains("LOC") || in.contains("ORG")

  def ontoPerson(str: String): Boolean = str.contains("PERSON")
  def ontoMoney(str: String): Boolean = str.contains("MONEY")
  def ontoPercent(str: String): Boolean = str.contains("PERCENT")
  def ontoNationality(str: String): Boolean = str.contains("NORP")
  def ontoOrdinal(str: String): Boolean = str.contains("ORDINAL")
  def ontoDate(str: String): Boolean = str.contains("DATE")
  def ontoOrg(str: String): Boolean = str.contains("ORG")
  def ontoGPE(str: String): Boolean = str.contains("GPE")
  def ontoLanguage(str: String): Boolean = str.contains("LANGUAGE")
  def ontoQuantOrCard(str: String): Boolean = str.contains("CARDINAL") || str.contains("QUANTITY")
  def ontonotesLocationFilter(in: String): Boolean = in.contains("LOC") || in.contains("ORG") || in.contains("GPE") || in.contains("FAC")

  def quantifierDate(str: String): Boolean = str.contains("TIME") || str.contains("Date")
  def quantifierNumber(str: String): Boolean = str.contains("Number")

  def questionNumberTrigger(str: String): Boolean = str.contains("how many") || str.contains("how much") || str.contains("how large") || str.contains(" age ")
  def questionDateTrigger(str: String): Boolean = str.contains(" date ") || str.contains("when ") || str.contains("what year") || str.contains("what time")
  def questionLanguageTrigger(str: String): Boolean = str.contains(" language ")
  def questionNationalityTrigger(str: String): Boolean = str.contains(" nationality ")
  def questionPercentTrigger(str: String): Boolean = str.contains(" percent ")
  def questionCurrencyTrigger(str: String): Boolean = str.contains(" money ") || str.contains(" currency ")
  def questionPersonTrigger(str: String): Boolean = ( str.contains("who ") && !str.contains("how many ")) ||
    str.contains("which individual") || str.contains("which person") ||
    str.contains("what individual") || str.contains("what person") || str.contains("to whom ")
  def questionWhichWhatTrigger(str: String) = str.contains("which") || str.contains("what") ||
    str.contains("for whos ") || str.contains("for whose ")
  def questionLocationTrigger(str: String) = str.contains("location")
  def questionOrgTrigger(str: String) = str.contains("institute") || str.contains("company")
  def questionEntTrigger(str: String) = str.contains("entity")


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

  def getNewConstituent(ta: TextAnnotation, start: Int, end: Int): Constituent = {
    val newConstituent = new Constituent("", 1.0, "CANDIDATES", ta, start, end)
    //new Relation("ChildOf", predicateClone, newConstituent, 1.0)
    newConstituent
  }

  // generates candiates using parse tree
  def generateCandidates(ta: TextAnnotation): ArrayBuffer[Constituent] = {
    val candidates = ArrayBuffer[Constituent]()
    ta.sentences().asScala.indices.foreach { sentenceId =>
      val tree = ParseUtils.getParseTree(ViewNames.PARSE_STANFORD, ta, sentenceId)
      val currentNode = ParseUtils.getSpanLabeledTree(tree)
      val sentenceStart = ta.getSentence(sentenceId).getStartSpan
      val sentenceEnd = ta.getSentence(sentenceId).getEndSpan
//      val currentNode = spanLabeledTree
//      println("spanLabeledTree = " + spanLabeledTree)
//      println("currentNode = " + currentNode)=
      def getCandidatesRecursively(currentNode: Tree[Pair[String, IntPair]]): Unit = {
//        println("Rec currentNode: " + currentNode)
        if (!currentNode.isLeaf) {
          val leaves = currentNode.getChildren.asScala
          leaves.foreach { sibling =>
            val siblingNode = sibling.getLabel
            // do not take the predicate as the argument
            val siblingSpan = siblingNode.getSecond
            val siblingLabel = siblingNode.getFirst
            val start = siblingSpan.getFirst + sentenceStart
            val end = siblingSpan.getSecond + sentenceStart
            candidates.+=(getNewConstituent(ta, start, end))
            if (siblingLabel.startsWith("PP")) {
              for (child <- sibling.getChildren.asScala) {
                val candidateStart = child.getLabel.getSecond.getFirst + sentenceStart
                val candidateEnd = child.getLabel.getSecond.getSecond + sentenceStart
                candidates.+=(getNewConstituent(ta, candidateStart, candidateEnd))
              }
            }
            getCandidatesRecursively(sibling)
          }
        }
        else {
          // do nothing
        }
      }
      getCandidatesRecursively(currentNode)
      //println(candidates.toSet.mkString("\n"))
    }
    candidates
  }

  def dropWikiURL(url: String): String = url.replace("http://en.wikipedia.org/wiki/", "")

  val stopwordsSet = Source.fromFile(new File("other/stopwords.txt")).getLines().toSet


}
