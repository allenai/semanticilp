package org.allenai.ari.solvers.squad

import edu.illinois.cs.cogcomp.core.datastructures.ViewNames
import edu.illinois.cs.cogcomp.core.datastructures.textannotation.{ TextAnnotation, View }
import edu.illinois.cs.cogcomp.core.utilities.SerializationHelper
import edu.illinois.cs.cogcomp.lbjava.learn.{ SparseAveragedPerceptron, SparseNetworkLearner }
import edu.illinois.cs.cogcomp.saul.classifier.Learnable
import edu.illinois.cs.cogcomp.saul.datamodel.DataModel
import org.allenai.ari.solvers.textilp.{ Paragraph, QPPair, Question }
import org.allenai.ari.solvers.textilp.utils.{ AnnotationUtils, Constants, SolverUtils }

import scala.collection.JavaConverters._
import CandidateGeneration._
import SquadClassifierUtils._
import edu.illinois.cs.cogcomp.saul.datamodel.property.Property
import edu.illinois.cs.cogcomp.saul.datamodel.property.features.discrete.{ DiscreteCollectionProperty, DiscreteProperty }
import me.tongfei.progressbar.ProgressBar

import scala.collection.immutable.IndexedSeq

object SquadSolverDataModel extends DataModel {

  val pair = node[QPPair]

  val beginTokenLabel = property(pair) { qp: QPPair => qp.beginTokenIdx == getBeginTokenIndex(qp.question, qp.paragraph) }
  val endTokenLabel = property(pair) { qp: QPPair => qp.endTokenIdx == getEndTokenIndex(qp.question, qp.paragraph) }
  val insideTokenLabel = property(pair) { qp: QPPair =>
    val begin = getBeginTokenIndex(qp.question, qp.paragraph)
    val end = getEndTokenIndex(qp.question, qp.paragraph)
    qp.beginTokenIdx >= begin && qp.beginTokenIdx <= end
  }

  val pairTokenLabel = property(pair) { qp: QPPair =>
    val longestAns = qp.question.answers.maxBy(_.answerText.length)
    val endToken = qp.paragraph.contextTAOpt.get.getTokenIdFromCharacterOffset(longestAns.answerStart + longestAns.answerText.length - 1)
    val beginToken = qp.paragraph.contextTAOpt.get.getTokenIdFromCharacterOffset(longestAns.answerStart)
    (qp.beginTokenIdx == beginToken) && (qp.endTokenIdx == endToken)
  }

  val sentenceIdLabel = property(pair) { qp: QPPair =>
    val indices = qp.question.answers.map { ans =>
      val charStart = ans.answerStart
      val c = qp.paragraph.contextTAOpt.get.getView(ViewNames.TOKENS).getConstituents.asScala.toList.
        filter(c => c.getStartCharOffset <= charStart + 1 && c.getEndCharOffset >= charStart + 1)
      require(c.nonEmpty,
        s"ans: $ans \n / paragraph: ${qp.paragraph.context} \n /  p.getView(ViewNames.TOKENS): ${qp.paragraph.contextTAOpt.get.getView(ViewNames.TOKENS).
          getConstituents.asScala.map(c => c.getSurfaceForm + ": " + c.getStartCharOffset + ", " + c.getEndCharOffset).mkString("/")}")
      c.head.getSentenceId
    }.toSet
    require(indices.size == 1)
    indices(0)
  }

  val candidateLemma = (begin: Boolean) => property(pair) { qp: QPPair => getPLemmaLabel(qp, begin) }

  val questionContainsString = (str: String) => property(pair) { qp: QPPair =>
    qp.question.questionText.toLowerCase.contains(str)
  }
  val whTypes = questionTerms.map(questionContainsString)

  val toBeQuestion = property(pair) { qp: QPPair =>
    tobe.exists(qp.question.questionText.contains)
  }

  val questionKeyTerms = (begin: Boolean) => property(pair) { qp: QPPair =>
    val shallowParseCons = qp.question.qTAOpt.get.getView(ViewNames.SHALLOW_PARSE).getConstituents.asScala.toList
    val questionWikiAnnotationOpt = wikifierRedis.get(qp.question.questionText)
    val wikiMentionsInQuestion = SerializationHelper.deserializeFromJson(questionWikiAnnotationOpt.get).getView(ViewNames.WIKIFIER).getConstituents.asScala.toList
    val paragraphWikiAnnotationOpt = wikifierRedis.get(qp.paragraph.context)
    val wikiViewOfInParagraph = SerializationHelper.deserializeFromJson(paragraphWikiAnnotationOpt.get).getView(ViewNames.WIKIFIER)
    val paragraphWikiLabel = wikiViewOfInParagraph.getConstituentsCoveringToken(if (begin) qp.beginTokenIdx else qp.endTokenIdx).asScala.headOption.map { _.getLabel }.getOrElse("")
    val (questionConstituentOpt, triggerTerm, wikiTriggerTermOpt) = extractQuestionKeyQuestionTerms(shallowParseCons, wikiMentionsInQuestion)
    val wikiTrigger = if (wikiTriggerTermOpt.isDefined) dropWikiURL(wikiTriggerTermOpt.get.getLabel) else triggerTerm
    val wikiDataCandidates = isSubsetWithWikiData(wikiTrigger, paragraphWikiLabel)
    questionConstituentOpt.map(_._1).toString + wikiDataCandidates
  }

  val questionConstituentConjOnto = (begin: Boolean) => property(pair) { qp: QPPair =>
    val shallowParseCons = qp.question.qTAOpt.get.getView(ViewNames.SHALLOW_PARSE).getConstituents.asScala.toList
    val (questionConstituentOpt, triggerTerm, wikiTriggerTermOpt) = extractQuestionKeyQuestionTerms(shallowParseCons, List.empty)
    questionConstituentOpt.map(_._1).toString + getPOntoLabel(qp, begin)
  }

  val questionTriggerTermConjOnto = (begin: Boolean) => property(pair) { qp: QPPair =>
    val shallowParseCons = qp.question.qTAOpt.get.getView(ViewNames.SHALLOW_PARSE).getConstituents.asScala.toList
    val (questionConstituentOpt, triggerTerm, wikiTriggerTermOpt) = extractQuestionKeyQuestionTerms(shallowParseCons, List.empty)
    triggerTerm + getPOntoLabel(qp, begin)
  }

  val rankingQuestionAndOrdinal = (begin: Boolean) => property(pair) { qp: QPPair =>
    isItRankingQuestion(qp.question.questionText.toLowerCase) && ontoOrdinal(getPOntoLabel(qp, begin))
  }

  val rankingQuestionAndNumber = (begin: Boolean) => property(pair) { qp: QPPair =>
    isItRankingQuestion(qp.question.questionText.toLowerCase) && ontoQuantOrCard(getPOntoLabel(qp, begin))
  }

  val numberQuestionAndCandidateIsNumber = (begin: Boolean) => property(pair) { qp: QPPair =>
    questionNumberTrigger(qp.question.questionText.toLowerCase) && ontoQuantOrCard(getPOntoLabel(qp, begin))
  }

  val dateQuestionAndCandidateIsDate = (begin: Boolean) => property(pair) { qp: QPPair =>
    questionDateTrigger(qp.question.questionText.toLowerCase) && ontoDate(getPOntoLabel(qp, begin))
  }

  val personTriggerAndPersonNameOnto = (begin: Boolean) => property(pair) { qp: QPPair =>
    //    println("getPOntoLabel: " + getPOntoLabel(qp, begin) + " ontoPerson: " + ontoPerson(getPOntoLabel(qp, begin)) +
    //      "  questionPersonTrigger: " + questionPersonTrigger(qp.question.questionText.toLowerCase))
    //questionPersonTrigger(qp.question.questionText.toLowerCase) && ontoPerson(getPOntoLabel(qp, begin))
    questionPersonTrigger(qp.question.questionText.toLowerCase).toString + getPOntoLabel(qp, begin)
  }

  val personTriggerAndPersonNameConll = (begin: Boolean) => property(pair) { qp: QPPair =>
    questionPersonTrigger(qp.question.questionText.toLowerCase) && conllPerson(getPConllLabel(qp, begin))
  }

  val personTriggerAndPersonNamePrefix = (begin: Boolean) => property(pair) { qp: QPPair =>
    questionPersonTrigger(qp.question.questionText.toLowerCase) && personNamePrefix.contains(getTokenSurface(qp, begin))
  }

  //  val personTriggerAndPersonNameWikiData = (begin: Boolean) => property(pair) { qp: QPPair =>
  //    questionPersonTrigger(qp.question.questionText.toLowerCase) &&
  //      WikiUtils.wikiAskQuery(dropWikiURL(getPWikiLabel(qp, begin)), WikiDataProperties.person, WikiDataProperties.instanceOf, 5)
  //  }

  val currencyTriggerAndOntoCurrencyLabel = (begin: Boolean) => property(pair) { qp: QPPair =>
    questionCurrencyTrigger(qp.question.questionText.toLowerCase) && ontoMoney(getPOntoLabel(qp, begin))
  }

  val percentTriggerAndOntoPercentLabel = (begin: Boolean) => property(pair) { qp: QPPair =>
    questionPercentTrigger(qp.question.questionText.toLowerCase) && ontoPercent(getPOntoLabel(qp, begin))
  }

  val nationalityTriggerAndOntoNationalityLabel = (begin: Boolean) => property(pair) { qp: QPPair =>
    questionNationalityTrigger(qp.question.questionText.toLowerCase) && ontoNationality(getPOntoLabel(qp, begin))
  }

  val languageTriggerAndOntoLanguageLabel = (begin: Boolean) => property(pair) { qp: QPPair =>
    questionLanguageTrigger(qp.question.questionText.toLowerCase) && ontoLanguage(getPOntoLabel(qp, begin))
  }

  val firstNPConjWithOntoLabel = (begin: Boolean) => property(pair) { qp: QPPair =>
    getKthTargetLabelInQuestionShallowParse(qp, 0, "NP") + getPOntoLabel(qp, begin)
  }

  val secondNPConjWithOntoLabel = (begin: Boolean) => property(pair) { qp: QPPair =>
    getKthTargetLabelInQuestionShallowParse(qp, 1, "NP") + getPOntoLabel(qp, begin)
  }

  val firstVPConjWithOntoLabel = (begin: Boolean) => property(pair) { qp: QPPair =>
    getKthTargetLabelInQuestionShallowParse(qp, 0, "VP") + getPOntoLabel(qp, begin)
  }

  val secondVPConjWithOntoLabel = (begin: Boolean) => property(pair) { qp: QPPair =>
    getKthTargetLabelInQuestionShallowParse(qp, 1, "VP") + getPOntoLabel(qp, begin)
  }

  val whichWhatTriggerAndOntoLocationLabel = (begin: Boolean) => property(pair) { qp: QPPair =>
    questionWhichWhatTrigger(qp.question.questionText.toLowerCase) &&
      questionLocationTrigger(qp.question.questionText.toLowerCase) &&
      ontonotesLocationFilter(getPOntoLabel(qp, begin))
  }

  val whichWhatTriggerAndConllLocationLabel = (begin: Boolean) => property(pair) { qp: QPPair =>
    questionWhichWhatTrigger(qp.question.questionText.toLowerCase) &&
      questionLocationTrigger(qp.question.questionText.toLowerCase) &&
      conllLocationFilter(getPOntoLabel(qp, begin))
  }

  val whichWhatTriggerAndConllInstituteLabel = (begin: Boolean) => property(pair) { qp: QPPair =>
    questionWhichWhatTrigger(qp.question.questionText.toLowerCase).toString +
      questionOrgTrigger(qp.question.questionText).toString + getPOntoLabel(qp, begin)
  }

  val whichWhatTriggerAndEntityTriggerAndOntoLabels = (begin: Boolean) => property(pair) { qp: QPPair =>
    questionWhichWhatTrigger(qp.question.questionText.toLowerCase).toString +
      questionEntTrigger(qp.question.questionText).toString + getPOntoLabel(qp, begin)
  }

  val whichWhatTriggerAndIsTobeAndEntityLabel = (begin: Boolean) => property(pair) { qp: QPPair =>
    questionWhichWhatTrigger(qp.question.questionText.toLowerCase).toString +
      tobe.exists(qp.question.questionText.contains).toString +
      getPOntoLabel(qp, begin)
  }

  // manually extracted type
  // TODO: comment this
  val questionTypeConjWithAnnotations = (begin: Boolean) => property(pair) { qp: QPPair =>
    val questionType = extractQuestionTypeInformation(getQShallowParseView(qp).getConstituents.asScala.toList).map(_._1)
    def conjWithType = (str: String) => str + questionType
    List(conjWithType(getPLemmaLabel(qp, begin)), conjWithType(getPPOSLabel(qp, begin)),
      conjWithType(getPChunkLabel(qp, begin)), conjWithType(getPOntoLabel(qp, begin)),
      conjWithType(getPConllLabel(qp, begin)))
  }

  val questionTypeConjWithGivebLabel = (begin: Boolean, f: (QPPair, Boolean) => String) => property(pair) { qp: QPPair =>
    val questionType = extractQuestionTypeInformation(getQShallowParseView(qp).getConstituents.asScala.toList).map(_._1)
    f(qp, begin) + questionType
    //    List(conjWithType(getPLemmaLabel(qp, begin)), conjWithType(getPPOSLabel(qp, begin)),
    //      conjWithType(getPChunkLabel(qp, begin)), conjWithType(getPOntoLabel(qp, begin)),
    //      conjWithType(getPConllLabel(qp, begin)))
  }

  val questionTypeConjWithAnnotations2 = (begin: Boolean) => List(
    questionTypeConjWithGivebLabel(begin, getPLemmaLabel),
    questionTypeConjWithGivebLabel(begin, getPPOSLabel),
    questionTypeConjWithGivebLabel(begin, getPChunkLabel),
    questionTypeConjWithGivebLabel(begin, getPOntoLabel),
    questionTypeConjWithGivebLabel(begin, getPConllLabel)
  )

  // f:(QPPair, Boolean) => String

  val typeClassifierViewName = "QUESTION_TYPE"
  //  val questionType = (begin: Boolean) =>  property(pair) { qp: QPPair =>
  //    val fineType = qp.question.qTAOpt.get.getView(typeClassifierViewName).getConstituents.get(0).getLabel
  //    val coarseType = qp.question.qTAOpt.get.getView(typeClassifierViewName).getConstituents.get(1).getLabel
  //    val thresholdedLabelsFine = Seq(-1.0, 0.5).map(th => questionTypeWithScoreThresholded(th, fine = true, qp))
  //    val thresholdedLabelsCoarse = Seq(-1.0, 0.5).map(th => questionTypeWithScoreThresholded(th, fine = false, qp))
  //    val typeInfo = List(fineType, coarseType) ++ thresholdedLabelsFine ++ thresholdedLabelsCoarse
  //    val tokenLabelInfo = List(getPLemmaLabel(qp, begin), getPPOSLabel(qp, begin),
  //      getPChunkLabel(qp, begin), getPOntoLabel(qp, begin), getPConllLabel(qp, begin))
  //    typeInfo.flatMap{ typ => tokenLabelInfo.map{ tok => typ + tok } }
  //  }

  //  def questionTypeWithScoreThresholded(th: Double, fine: Boolean, qp: QPPair): String = {
  //    val score = getQTA(qp).getView(typeClassifierViewName).getConstituents.get(if(fine) 0 else 1).getConstituentScore
  //    (score > th).toString
  //  }

  //  val questionTypeClassifierConjWithAnnotations = (begin: Boolean) => property(pair) { qp: QPPair =>
  //    val questionType = extractQuestionTypeInformation(getQShallowParseView(qp).getConstituents.asScala.toList).map(_._1)
  //    def conjWithType = (str: String) => str + questionType
  //    List(conjWithType(getPLemmaLabel(qp, begin)), conjWithType(getPPOSLabel(qp, begin)),
  //      conjWithType(getPChunkLabel(qp, begin)), conjWithType(getPOntoLabel(qp, begin)),
  //      conjWithType(getPConllLabel(qp, begin)))
  //  }

  val questionTypeClassifierConjGivenLabel = (begin: Boolean, f: (QPPair, Boolean) => String) => property(pair) { qp: QPPair =>
    val fineType = qp.question.qTAOpt.get.getView(typeClassifierViewName).getConstituents.get(0).getLabel
    val coarseType = qp.question.qTAOpt.get.getView(typeClassifierViewName).getConstituents.get(1).getLabel
    val fineScore = qp.question.qTAOpt.get.getView(typeClassifierViewName).getConstituents.get(0).getConstituentScore
    val coarseScore = qp.question.qTAOpt.get.getView(typeClassifierViewName).getConstituents.get(1).getConstituentScore
    val fineScoreType = Seq(-1.0, 0.0, 0.5).map(s => if (fineScore > s) "1" else "0").mkString
    val coarseScoreType = Seq(-1.0, 0.0, 0.5).map(s => if (coarseScore > s) "1" else "0").mkString
    List(f(qp, begin) + fineType + fineScoreType, f(qp, begin) + coarseType + coarseScoreType)
  }

  val questionTypeClassifierConjWithAnnotations2 = (begin: Boolean) => List(
    questionTypeClassifierConjGivenLabel(begin, getPLemmaLabel),
    questionTypeClassifierConjGivenLabel(begin, getPPOSLabel),
    questionTypeClassifierConjGivenLabel(begin, getPChunkLabel),
    questionTypeClassifierConjGivenLabel(begin, getPOntoLabel),
    questionTypeClassifierConjGivenLabel(begin, getPConllLabel)
  )

  val slidingWindowOfLemmaWithTh = (begin: Boolean, size: Int, th: Int) => property(pair) { qp: QPPair =>
    val center = if (begin) qp.beginTokenIdx else qp.endTokenIdx
    val min = scala.math.min(0, center - size)
    val max = scala.math.min(qp.paragraph.contextTAOpt.get.getTokens.length - 1, center + size)
    val paragraphsLemmaTokens = getPLemmaView(qp).getConstituentsCoveringSpan(min, max).asScala.map(_.getLabel).toSet
    val questionLemmaTokens = getQLemmaView(qp).getConstituents.asScala.map(_.getLabel).toSet
    paragraphsLemmaTokens.intersect(questionLemmaTokens).size > th
  }

  val slidingWindowOfLemmaSize = (begin: Boolean, size: Int) => property(pair) { qp: QPPair =>
    val center = if (begin) qp.beginTokenIdx else qp.endTokenIdx
    val ta = qp.paragraph.contextTAOpt.get
    val min = scala.math.max(0, center - size)
    val max = scala.math.min(ta.getTokens.length - 1, center + size)
    val paragraphsLemmaTokens = getPLemmaView(qp).getConstituentsCoveringSpan(min, max).asScala.map(_.getLabel).toSet
    val questionLemmaTokens = getQLemmaView(qp).getConstituents.asScala.map(_.getLabel).toSet
    paragraphsLemmaTokens.intersect(questionLemmaTokens).size.toString
  }

  val slidingWindowOfLemmaSizeWithinSentence = (begin: Boolean, size: Int) => property(pair) { qp: QPPair =>
    val center = if (begin) qp.beginTokenIdx else qp.endTokenIdx
    val ta = qp.paragraph.contextTAOpt.get
    val sentenceId = ta.getSentenceId(center)
    val filteredIndices = ta.getTokens.indices.filter(ta.getSentenceId(_) == sentenceId)
    val min = scala.math.max(center - size, filteredIndices.min)
    val max = scala.math.min(center + size, filteredIndices.max)
    val paragraphsLemmaTokens = getPLemmaView(qp).getConstituentsCoveringSpan(min, max).asScala.map(_.getLabel).toSet
    val questionLemmaTokens = getQLemmaView(qp).getConstituents.asScala.map(_.getLabel).toSet
    paragraphsLemmaTokens.intersect(questionLemmaTokens).size.toString
  }

  val slidingWindowOfLemmaSizeWithinSentenceSpan = (size: Int) => property(pair) { qp: QPPair =>
    val ta = qp.paragraph.contextTAOpt.get
    val sentenceId = ta.getSentenceId(qp.beginTokenIdx)
    val filteredIndices = ta.getTokens.indices.filter(ta.getSentenceId(_) == sentenceId)
    val min = scala.math.max(qp.beginTokenIdx - size, filteredIndices.min)
    val max = scala.math.min(qp.endTokenIdx + size, filteredIndices.max)
    val paragraphsLemmaTokens = getPLemmaView(qp).getConstituentsCoveringSpan(min, max).asScala.map(_.getLabel).toSet
    val questionLemmaTokens = getQLemmaView(qp).getConstituents.asScala.map(_.getLabel).toSet
    paragraphsLemmaTokens.intersect(questionLemmaTokens).size.toString
  }

  val questionOverlapWithinCurrentSentence = (begin: Boolean, size: Int) => property(pair) { qp: QPPair =>
    val center = if (begin) qp.beginTokenIdx else qp.endTokenIdx
    val ta = qp.paragraph.contextTAOpt.get
    val sentenceId = ta.getSentenceId(center)
    val filteredIndices = ta.getTokens.indices.filter(ta.getSentenceId(_) == sentenceId)
    val paragraphsLemmaTokens = getPLemmaView(qp).getConstituentsCoveringSpan(filteredIndices.min, filteredIndices.max).asScala.map(_.getLabel).toSet
    val questionLemmaTokens = getQLemmaView(qp).getConstituents.asScala.map(_.getLabel).toSet
    paragraphsLemmaTokens.intersect(questionLemmaTokens).size.toString
  }

  /*  val slidingWindowWithLemma = {
      (begin: Boolean) =>
        List(
          slidingWindowOfLemmaWithTh(begin, 4, 1),
          slidingWindowOfLemmaWithTh(begin, 4, 2),
          slidingWindowOfLemmaWithTh(begin, 4, 3),
          slidingWindowOfLemmaWithTh(begin, 4, 4),
          slidingWindowOfLemmaWithTh(begin, 4, 5),
          slidingWindowOfLemmaWithTh(begin, 4, 6),
          slidingWindowOfLemmaWithTh(begin, 4, 7)
        )
    }*/

  val lengthOfSpan = property(pair) { qp: QPPair => List((qp.endTokenIdx - qp.beginTokenIdx).toString) }

  val posLabelsOfSpan = property(pair) { qp: QPPair => getPPOSSpanLabel(qp).toList }

  val posLabelsOfSpanConjWithLength = property(pair) { qp: QPPair =>
    val length = qp.endTokenIdx - qp.beginTokenIdx
    getPPOSSpanLabel(qp).sortBy(identity).map(_ + length).toList
  }

  val lemmaLabelsOfSpanConjWithLength = property(pair) { qp: QPPair =>
    val length = qp.endTokenIdx - qp.beginTokenIdx
    getPPOSSpanLabel(qp).sortBy(i => i).map(_ + length).toList
  }

  val ontoLabelsOfSpan = property(pair) { qp: QPPair => getPOntoSpanLabel(qp).toList }

  val conllLabelsOfSpan = property(pair) { qp: QPPair => getPOntoSpanLabel(qp).toList }

  val lemmaLabelsOfSpan = property(pair) { qp: QPPair =>
    //getPLemmaLabel(qp, begin = true)
    getPLemmaSpanLabel(qp).toList.distinct.sortBy(i => i).toString
  }

  val conj2 = (p1: DiscreteProperty[QPPair], p2: DiscreteProperty[QPPair]) => property(pair) { qp: QPPair =>
    p1(qp) + p2(qp)
  }

  val conj2Collection = (p1: DiscreteCollectionProperty[QPPair], p2: DiscreteProperty[QPPair]) => property(pair) { qp: QPPair =>
    p1(qp) + p2(qp)
  }

  val bigramForward = (p: DiscreteProperty[QPPair]) => property(pair) { qp: QPPair =>
    val tokenSize = qp.paragraph.contextTAOpt.get.getTokens.length
    val beginNew = if (qp.beginTokenIdx + 1 < tokenSize) qp.beginTokenIdx + 1 else qp.beginTokenIdx
    val endNew = if (qp.endTokenIdx + 1 < tokenSize) qp.endTokenIdx + 1 else qp.endTokenIdx
    val qpF = QPPair(qp.question, qp.paragraph, beginNew, endNew)
    p(qp) + p(qpF)
  }

  val bigramBackward = (p: DiscreteProperty[QPPair]) => property(pair) { qp: QPPair =>
    val beginNew = if (qp.beginTokenIdx - 1 >= 0) qp.beginTokenIdx - 1 else 0
    val endNew = if (qp.endTokenIdx - 1 >= 0) qp.endTokenIdx - 1 else 0
    val qpB = QPPair(qp.question, qp.paragraph, beginNew, endNew)
    p(qp) + p(qpB)
  }

  val bigramFeatures = { (begin: Boolean) =>
    questionTypeClassifierConjWithAnnotations2(begin) ++
      questionTypeConjWithAnnotations2(begin) ++
      Seq(
        whichWhatTriggerAndIsTobeAndEntityLabel(begin),
        candidateLemma(begin),
        numberQuestionAndCandidateIsNumber(begin),
        dateQuestionAndCandidateIsDate(begin),
        personTriggerAndPersonNameOnto(begin),
        personTriggerAndPersonNameConll(begin),
        nationalityTriggerAndOntoNationalityLabel(begin),
        whichWhatTriggerAndConllInstituteLabel(begin),
        whichWhatTriggerAndEntityTriggerAndOntoLabels(begin),
        questionConstituentConjOnto(begin),
        questionTriggerTermConjOnto(begin),
        firstNPConjWithOntoLabel(begin),
        secondNPConjWithOntoLabel(begin),
        firstVPConjWithOntoLabel(begin),
        secondVPConjWithOntoLabel(begin)
      ).flatMap(p => Seq(bigramForward(p), bigramBackward(p)))
  }

  val trigramForward = (p: DiscreteProperty[QPPair]) => property(pair) { qp: QPPair =>
    val tokenSize = qp.paragraph.contextTAOpt.get.getTokens.length
    val beginNew2 = if (qp.beginTokenIdx + 1 < tokenSize) qp.beginTokenIdx + 1 else qp.beginTokenIdx
    val endNew2 = if (qp.endTokenIdx + 1 < tokenSize) qp.endTokenIdx + 1 else qp.endTokenIdx
    val beginNew3 = if (qp.beginTokenIdx + 2 < tokenSize) qp.beginTokenIdx + 2 else qp.beginTokenIdx
    val endNew3 = if (qp.endTokenIdx + 2 < tokenSize) qp.endTokenIdx + 2 else qp.endTokenIdx
    val qpF2 = QPPair(qp.question, qp.paragraph, beginNew2, endNew2)
    val qpF3 = QPPair(qp.question, qp.paragraph, beginNew3, endNew3)
    p(qp) + p(qpF2) + p(qpF3)
  }

  val conjWithSlidingWindow = { (begin: Boolean) =>
    questionTypeConjWithAnnotations2(begin) ++ Seq(
      whichWhatTriggerAndIsTobeAndEntityLabel(begin),
      candidateLemma(begin),
      numberQuestionAndCandidateIsNumber(begin),
      dateQuestionAndCandidateIsDate(begin),
      personTriggerAndPersonNameOnto(begin),
      personTriggerAndPersonNameConll(begin),
      nationalityTriggerAndOntoNationalityLabel(begin),
      whichWhatTriggerAndConllInstituteLabel(begin),
      whichWhatTriggerAndEntityTriggerAndOntoLabels(begin),
      questionConstituentConjOnto(begin),
      questionTriggerTermConjOnto(begin),
      firstNPConjWithOntoLabel(begin),
      secondNPConjWithOntoLabel(begin),
      firstVPConjWithOntoLabel(begin),
      secondVPConjWithOntoLabel(begin)
    ).map(p => conj2(p, questionOverlapWithinCurrentSentence(begin, 5)))
  }

  val bigramFeaturesWithContextSimilarity = { (begin: Boolean) =>
    conjWithSlidingWindow(begin).flatMap(p => Seq(bigramForward(p), bigramBackward(p)))
  }

  val propertyList = (begin: Boolean) => {
    bigramFeaturesWithContextSimilarity(begin) ++
      bigramFeatures(begin) ++
      conjWithSlidingWindow(begin) ++
      questionTypeClassifierConjWithAnnotations2(begin) ++
      questionTypeConjWithAnnotations2(begin) ++
      List(
        slidingWindowOfLemmaSize(begin, 5),
        questionOverlapWithinCurrentSentence(begin, 5),
        slidingWindowOfLemmaSizeWithinSentence(begin, 5),
        whichWhatTriggerAndIsTobeAndEntityLabel(begin),
        candidateLemma(begin),
        numberQuestionAndCandidateIsNumber(begin),
        dateQuestionAndCandidateIsDate(begin),
        personTriggerAndPersonNameOnto(begin),
        personTriggerAndPersonNameConll(begin),
        nationalityTriggerAndOntoNationalityLabel(begin),
        whichWhatTriggerAndConllInstituteLabel(begin),
        whichWhatTriggerAndEntityTriggerAndOntoLabels(begin),
        questionConstituentConjOnto(begin),
        questionTriggerTermConjOnto(begin),
        firstNPConjWithOntoLabel(begin),
        secondNPConjWithOntoLabel(begin),
        firstVPConjWithOntoLabel(begin),
        secondVPConjWithOntoLabel(begin)
      //    questionKeyTerms(begin)//,
      //    personTriggerAndPersonNameWikiData(begin),
      //    personTriggerAndPersonNamePrefix(begin)//,
      //    rankingQuestionAndOrdinal(begin)//,
      //    rankingQuestionAndNumber(begin)//,
      //    currencyTriggerAndOntoCurrencyLabel(begin)//,
      //    percentTriggerAndOntoPercentLabel(begin)//,
      //    nationalityTriggerAndOntoNationalityLabel(begin)
      //    languageTriggerAndOntoLanguageLabel(begin)
      //    whichWhatTriggerAndOntoLocationLabel(begin)//,
      //    whichWhatTriggerAndConllLocationLabel(begin)//,
      )
  }

  // often longer questions need longer answers; this can be reflected in the context similarity, or directly in a length feature
  // try removing the stopwords

  val beginFeatures: List[Property[QPPair]] = propertyList(true)

  val endFeatures: List[Property[QPPair]] = propertyList(false)

  val pairFeatures = {
    List(
      lengthOfSpan,
      posLabelsOfSpan,
      ontoLabelsOfSpan,
      conllLabelsOfSpan,
      lemmaLabelsOfSpan
    )
  }
  // slidingWindowOfLemmaSizeWithinSentence(begin, 5),
  //val pairFeaturesWithContext = pairFeatures.map(p => conj2Collection(p, slidingWindowOfLemmaSizeWithinSentenceSpan(4)))

}

class SquadClassifier(cType: String = "begin") extends Learnable[QPPair](SquadSolverDataModel.pair) {
  import SquadSolverDataModel._
  def label = cType match {
    case "begin" => beginTokenLabel
    case "end" => endTokenLabel
    case "pair" => pairTokenLabel
    case "inside" => insideTokenLabel
    case "sentId" => sentenceIdLabel
    case _ => throw new Exception("Unknown classifier type")
  }
  def cFeatures = cType match {
    case "begin" => beginFeatures
    case "end" => endFeatures
    case "pair" => List(lemmaLabelsOfSpan) // lengthOfSpan  pairFeatures //++ pairFeaturesWithContext ++   beginFeatures ++ endFeatures
    case "inside" => beginFeatures
    case "sentId" => beginFeatures
    case _ => throw new Exception("Unknown classifier type")
  }
  override def feature = using(cFeatures)
  override lazy val classifier = new SparseNetworkLearner {
    val p = new SparseAveragedPerceptron.Parameters()
    p.learningRate = .1
    p.thickness = 4
    baseLTU = new SparseAveragedPerceptron(p)
  }
}

object SquadClassifierUtils {
  val beginClassifier = new SquadClassifier("begin")
  beginClassifier.modelSuffix = "begin"
  val endClassifier = new SquadClassifier("end")
  endClassifier.modelSuffix = "end"
  val pairClassifier = new SquadClassifier("pair")
  pairClassifier.modelSuffix = "pair"
  val insideClassifier = new SquadClassifier("inside")
  insideClassifier.modelSuffix = "inside"
  val sentenceIdClassifier = new SquadClassifier("sentId")
  sentenceIdClassifier.modelSuffix = "sentId"

  private lazy val annotationUtils = new AnnotationUtils()
  private lazy val trainReader = new SQuADReader(Constants.squadTrainingDataFile, Some(annotationUtils.pipelineService), annotationUtils)
  private lazy val devReader = new SQuADReader(Constants.squadDevDataFile, Some(annotationUtils.pipelineService), annotationUtils)

  lazy val ((trainInstances, trainQPPairs), (devInstances, devQPPairs)) = {
    println("trainReader length: " + trainReader.instances.length)
    //    println("devReader length: " + devReader.instances.length)
    def getInstances(i: Int, j: Int, sQuADReader: SQuADReader): (Seq[QPPair], Seq[(Question, Paragraph)]) = {
      val qAndpPairs = sQuADReader.instances.slice(i, j).flatMap { ii => ii.paragraphs.flatMap { p => p.questions.map(q => (q, p)) } } //.filter(_._1.questionText.toLowerCase.contains("who"))
      val trainableInstances = qAndpPairs.flatMap {
        case (q, p) =>
          val inds = p.contextTAOpt.get.getTokens.indices
          val begin = getBeginTokenIndex(q, p)
          val end = getEndTokenIndex(q, p)
          //        println("q: " + q.questionText)
          //        println("a: " + q.answers)
          //        println("p: " + p.context)
          val extraInds = (1 to 0).flatMap(_ => List(begin, end))
          val (positive, negative) = (inds ++ extraInds).map { i =>
            val qp = QPPair(q, p, i, i)
            //          SquadSolverDataModel.personTriggerAndPersonNameOnto(true)(qp)
            //          println("i: " + i + " / " + SquadSolverDataModel.beginTokenLabel(qp) + "  /   " + SquadSolverDataModel.endTokenLabel(qp) + "  /   " + p.contextTAOpt.get.getToken(i))
            //          println("------------")
            //          println("Person trigger: " + SquadSolverDataModel.personTriggerAndPersonNameOnto(true)(qp) +
            //            " / overlap: " + SquadSolverDataModel.slidingWindowOfLemmaSize(true, 5)(qp)  +
            //            " / overlapWithinSentence: " + SquadSolverDataModel.questionOverlapWithinCurrentSentence(true, 5)(qp)  +
            //            " / overlapWithSentence: " + SquadSolverDataModel.slidingWindowOfLemmaSizeWithinSentence(true, 5)(qp)  +
            //            " / label: " + SquadSolverDataModel.insideTokenLabel(qp) +
            //            " / token: " + p.contextTAOpt.get.getToken(i)
            //          )
            qp
          }.partition { SquadSolverDataModel.insideTokenLabel(_) == "true" }
          positive ++ scala.util.Random.shuffle(negative).take(positive.length * 2)
      }
      trainableInstances -> qAndpPairs
    }
    getInstances(0, 28, trainReader) -> getInstances(28, 30, trainReader)
  }

  lazy val (trainInstancesForSentenceIdClassifier, devInstancesSentenceIdClassifier) = {
    println("trainReader length: " + trainReader.instances.length)
    println("devReader length: " + devReader.instances.length)
    def getInstances(sQuADReader: SQuADReader): Seq[QPPair] = {
      val qAndpPairs = sQuADReader.instances.flatMap { ii => ii.paragraphs.flatMap { p => p.questions.map(q => (q, p)) } }
      val a = qAndpPairs.flatMap {
        case (q, p) =>
          (0 until p.contextTAOpt.get.getNumberOfSentences).map { i =>
            QPPair(q, p, 0, 0, sentenceIdOpt = Some(i))
          }
      }
      println("getInstances: " + a.size)
      a
    }
    getInstances(trainReader) -> getInstances(devReader)
  }

  def populateInstances(): Unit = {
    println(trainInstances.length)
    println(devInstances.length)
    SquadSolverDataModel.pair.populate(trainInstances)
    //SquadSolverDataModel.pair.populate(testInstances, train = false)
  }

  // TA
  def getQTA(qp: QPPair): TextAnnotation = qp.question.qTAOpt.get
  def getPTA(qp: QPPair): TextAnnotation = qp.paragraph.contextTAOpt.get

  // P Views
  def getPOntoView(qp: QPPair): View = getPTA(qp).getView(ViewNames.NER_ONTONOTES)
  def getPConllView(qp: QPPair): View = getPTA(qp).getView(ViewNames.NER_CONLL)
  //  def getPWikiView(qp: QPPair): View = getPTA(qp).getView(ViewNames.WIKIFIER)
  def getPPOSView(qp: QPPair): View = getPTA(qp).getView(ViewNames.POS)
  def getPLemmaView(qp: QPPair): View = getPTA(qp).getView(ViewNames.LEMMA)
  def getPShallowParseView(qp: QPPair): View = getPTA(qp).getView(ViewNames.SHALLOW_PARSE)

  // Q Views
  def getQLemmaView(qp: QPPair): View = getQTA(qp).getView(ViewNames.LEMMA)
  def getQShallowParseView(qp: QPPair): View = getQTA(qp).getView(ViewNames.SHALLOW_PARSE)

  // P labels
  def getPLemmaLabel(qp: QPPair, begin: Boolean): String = getLabel(qp, getPLemmaView(qp), begin)
  def getPPOSLabel(qp: QPPair, begin: Boolean): String = getLabel(qp, getPPOSView(qp), begin)
  def getPOntoLabel(qp: QPPair, begin: Boolean): String = getLabel(qp, getPOntoView(qp), begin)
  def getPConllLabel(qp: QPPair, begin: Boolean): String = getLabel(qp, getPConllView(qp), begin)
  //def getPWikiLabel(qp: QPPair, begin: Boolean): String = getLabel(qp, getPWikiView(qp), begin)
  def getPChunkLabel(qp: QPPair, begin: Boolean): String = getLabel(qp, getPShallowParseView(qp), begin)

  def getPLemmaSpanLabel(qp: QPPair): Seq[String] = getLabels(getPLemmaView(qp), qp.beginTokenIdx, qp.endTokenIdx)
  def getPPOSSpanLabel(qp: QPPair): Seq[String] = getLabels(getPPOSView(qp), qp.beginTokenIdx, qp.endTokenIdx)
  def getPOntoSpanLabel(qp: QPPair): Seq[String] = getLabels(getPOntoView(qp), qp.beginTokenIdx, qp.endTokenIdx)
  def getPConllSpanLabel(qp: QPPair): Seq[String] = getLabels(getPConllView(qp), qp.beginTokenIdx, qp.endTokenIdx)

  def getKthTargetLabelInQuestionShallowParse(qp: QPPair, k: Int, targetLabel: String): String = {
    val surfaceStrings = getQShallowParseView(qp).getConstituents.asScala.filter(_.getLabel == targetLabel).map(_.getSurfaceForm)
    if (surfaceStrings.length > k) surfaceStrings(k) else ""
  }

  def getTokenSurface(qp: QPPair, begin: Boolean): String = {
    qp.paragraph.contextTAOpt.get.getToken(if (begin) qp.beginTokenIdx else qp.endTokenIdx)
  }

  def getLabel(qp: QPPair, vu: View, begin: Boolean): String = {
    vu.getConstituentsCoveringToken(if (begin) qp.beginTokenIdx else qp.endTokenIdx).asScala.headOption.map(_.getLabel).getOrElse("")
  }

  def getLabels(vu: View, beginIdx: Int, endIdx: Int): Seq[String] = {
    vu.getConstituentsCoveringSpan(beginIdx, endIdx).asScala.map(_.getLabel)
  }

  def decodeQuestionsWithBeginEnd(train: Boolean = true, size: Int = 10, k: Int = 5) = {
    (if (train) trainInstances else devInstances).take(size).foreach { ins =>
      beginEndDecoder(ins.question, ins.paragraph, k)
    }
  }

  def decodeQuestionsWithInside(train: Boolean = true, size: Int = 10, k: Int = 5) = {
    (if (train) trainInstances else devInstances).take(size).foreach { ins =>
      insideDecoder(ins.question, ins.paragraph, k, 0.0)
    }
  }

  def getBeginTokenIndex(question: Question, paragraph: Paragraph): Int = {
    val longestAns = question.answers.maxBy(_.answerText.length)
    //println("B: Longest Ans: " + longestAns)
    val tokId = paragraph.contextTAOpt.get.getTokenIdFromCharacterOffset(longestAns.answerStart)
    //println("TokId: " + tokId + "  /  " + paragraph.contextTAOpt.get.getToken(tokId))
    tokId
  }

  def getEndTokenIndex(question: Question, paragraph: Paragraph): Int = {
    val longestAns = question.answers.maxBy(_.answerText.length)
    paragraph.contextTAOpt.get.getTokenIdFromCharacterOffset(longestAns.answerStart + longestAns.answerText.length - 1)
  }

  def beginEndDecoder(q: Question, p: Paragraph, k: Int): Seq[(Int, Int)] = {
    println(q.questionText)
    println(p.context)
    val inds = p.contextTAOpt.get.getTokens.indices
    val scoresPerIndex = inds.map { i =>
      val qp = QPPair(q, p, i, i)
      val beginScore = beginClassifier.classifier.scores(qp).get("true")
      val endScore = endClassifier.classifier.scores(qp).get("true")
      (i, (beginScore, endScore))
    }.toMap

    // choose top-K pairs with the highest scores
    val scoresPerIndexPairs = for {
      j <- inds
      i <- 0 until j
    } yield (i, j, scoresPerIndex(i)._1 + scoresPerIndex(j)._2)

    extractTopKSpans(k, scoresPerIndexPairs, p.contextTAOpt.get)
  }

  def insideDecoder(q: Question, p: Paragraph, k: Int, threshold: Double): Seq[(Int, Int)] = {
    //println(q.questionText)
    //println(p.context)
    val inds = p.contextTAOpt.get.getTokens.indices
    val scoresPerIndex = inds.map { i =>
      val qp = QPPair(q, p, i, i)
      insideClassifier.classifier.scores(qp).get("true") - threshold
    }

    // choose top-K pairs with the highest scores
    val scoresPerIndexPairs = for {
      i <- inds
      j <- i to inds.end
    } yield (i, j, scoresPerIndex.slice(i, j).sum)

    extractTopKSpans(k, scoresPerIndexPairs, p.contextTAOpt.get)
  }

  def pairDecoder(q: Question, p: Paragraph, k: Int): Seq[(Int, Int)] = {
    println(q.questionText)
    println(p.context)
    val inds = p.contextTAOpt.get.getTokens.indices
    val scoresPerIndex = inds.map { i =>
      val qp = QPPair(q, p, i, i)
      pairClassifier.classifier.scores(qp).get("true")
    }

    // choose top-K pairs with the highest scores
    val scoresPerIndexPairs = for {
      i <- inds
      j <- i to inds.end
      qp = QPPair(q, p, i, i)
      score = pairClassifier.classifier.scores(qp).get("true")
    } yield (i, j, score)

    extractTopKSpans(k, scoresPerIndexPairs, p.contextTAOpt.get)
  }

  def extractTopKSpans(k: Int, scoresPerIndexPairs: IndexedSeq[(Int, Int, Double)], paragraphTA: TextAnnotation): Seq[(Int, Int)] = {
    val sortedScores = scoresPerIndexPairs.sortBy(-_._3) // biggest scores at the beginning

    val selectedSpans = sortedScores.take(k)
    //    selectedSpans.foreach { case (i, j, score) =>
    //      val answer = paragraphTA.getTokensInSpan(i, j).mkString(" ")
    //      println("ans: " + answer + " / score: " + score)
    //    }
    selectedSpans.map(a => (a._1, a._2))
  }

  // maximizes F-alpha
  def findFMaximizingThreshold(thresholdRange: Seq[Double], train: Boolean = true): Unit = {
    thresholdRange.foreach { th =>
      val pairs = if (train) trainQPPairs else devQPPairs
      val pb = new ProgressBar("Test", pairs.length); // name, initial max
      pb.start()
      val (exact, f1, ones) = pairs.map {
        case (q, p) =>
          val selectedSpans = insideDecoder(q, p, 1, th)
          assert(selectedSpans.length == 1)
          val predictedSpan = p.contextTAOpt.get.getTokensInSpan(selectedSpans.head._1, selectedSpans.head._2).mkString(" ")
          pb.step()
          SolverUtils.assignCreditSquad(predictedSpan, q.answers.map(_.answerText))
      }.unzip3
      pb.stop()
      val avgF1 = f1.sum / ones.sum
      val avgExact = exact.sum / ones.sum
      val totalCount = ones.sum
      println(s"Th: $th / Instance count: $totalCount / avgF1: $avgF1 / avgExact: $avgExact")
    }
  }

  def evaluateF1OfTopKScores(th: Double, train: Boolean = true, k: Int): Unit = {
    val pairs = if (train) trainQPPairs else devQPPairs
    val pb = new ProgressBar("Test", pairs.length); // name, initial max
    pb.start()
    val (exact, f1, ones) = pairs.map {
      case (q, p) =>
        val selectedSpans = insideDecoder(q, p, k, th)
        assert(selectedSpans.length == k)
        val (exactSet, f1Set, _) = selectedSpans.map { span =>
          val predictedSpan = p.contextTAOpt.get.getTokensInSpan(span._1, span._2).mkString(" ")
          SolverUtils.assignCreditSquad(predictedSpan, q.answers.map(_.answerText))
        }.unzip3
        pb.step()
        (exactSet.max, f1Set.max, 1.0)
    }.unzip3
    pb.stop()
    val avgF1 = f1.sum / ones.sum
    val avgExact = exact.sum / ones.sum
    val totalCount = ones.sum
    println(s"Th: $th / Instance count: $totalCount / avgF1: $avgF1 / avgExact: $avgExact")
  }

}