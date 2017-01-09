package org.allenai.ari.solvers.squad

import edu.illinois.cs.cogcomp.core.datastructures.ViewNames
import edu.illinois.cs.cogcomp.core.datastructures.textannotation.{TextAnnotation, View}
import edu.illinois.cs.cogcomp.core.utilities.SerializationHelper
import edu.illinois.cs.cogcomp.lbjava.learn.{SparseAveragedPerceptron, SparseNetworkLearner}
import edu.illinois.cs.cogcomp.saul.classifier.Learnable
import edu.illinois.cs.cogcomp.saul.datamodel.DataModel
import org.allenai.ari.solvers.textilp.QPPair
import org.allenai.ari.solvers.textilp.utils.WikiUtils.WikiDataProperties
import org.allenai.ari.solvers.textilp.utils.{AnnotationUtils, Constants, SQuADReader, WikiUtils}

import scala.collection.JavaConverters._
import CandidateGeneration._

object SquadSolverDataModel extends DataModel{

  val pair = node[QPPair]

  val beginTokenLabel = property(pair) { qp: QPPair =>
    val longestAns = qp.question.answers.maxBy(_.answerText.length)
    val beginToken = qp.paragraph.contextTAOpt.get.getTokenIdFromCharacterOffset(longestAns.answerStart)
    qp.beginTokenIdx == beginToken
  }

  val endTokenLabel = property(pair) { qp: QPPair =>
    val longestAns = qp.question.answers.maxBy(_.answerText.length)
    val endToken = qp.paragraph.contextTAOpt.get.getTokenIdFromCharacterOffset(longestAns.answerStart + longestAns.answerText.length - 1)
    qp.endTokenIdx == endToken
  }

  val pairTokenLabel = property(pair) { qp: QPPair =>
    val longestAns = qp.question.answers.maxBy(_.answerText.length)
    val endToken = qp.paragraph.contextTAOpt.get.getTokenIdFromCharacterOffset(longestAns.answerStart + longestAns.answerText.length - 1)
    val beginToken = qp.paragraph.contextTAOpt.get.getTokenIdFromCharacterOffset(longestAns.answerStart)
    (qp.beginTokenIdx == beginToken) && (qp.endTokenIdx == endToken)
  }

  val questionUnigrams = property(pair) { qp: QPPair =>
    qp.question.qTAOpt.get.getView(ViewNames.LEMMA).getConstituents.asScala.map(_.getSurfaceForm).toList
  }

  def candidateLemma = (begin: Boolean) => property(pair) { qp: QPPair =>
    val pTokens = qp.paragraph.contextTAOpt.get.getView(ViewNames.LEMMA).getConstituents.asScala.map(_.getSurfaceForm).toList
    if(begin) pTokens(qp.beginTokenIdx) else pTokens(qp.endTokenIdx)
  }
  val beginLemma = candidateLemma(true)
  val endLemma = candidateLemma(false)

  def questionContainsString = (str: String) => property(pair) { qp: QPPair =>
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
    val (questionConstituentOpt, triggerTerm, wikiTriggerTermOpt) = extractQuestionKeyQuestionTerms(shallowParseCons, wikiMentionsInQuestion)
    val wikiTrigger = if(wikiTriggerTermOpt.isDefined) dropWikiURL(wikiTriggerTermOpt.get.getLabel) else triggerTerm
    val wikiDataCandidates = isSubsetWithWikiData(wikiTrigger, getPWikiLabel(qp, begin))
    questionConstituentOpt.map(_._1).toString + wikiDataCandidates
  }

  val rankingQuestionAndOrdinal = (begin: Boolean) => property(pair) { qp: QPPair =>
    isItRankingQuestion(qp.question.questionText) && ontoOrdinal(getPOntoLabel(qp, begin))
  }

  val rankingQuestionAndNumber = (begin: Boolean) => property(pair) { qp: QPPair =>
    isItRankingQuestion(qp.question.questionText) && ontoQuantOrCard(getPOntoLabel(qp, begin))
  }

  val numberQuestionAndCandidateIsNumber = (begin: Boolean) => property(pair) { qp: QPPair =>
    questionNumberTrigger(qp.question.questionText.toLowerCase) && ontoQuantOrCard(getPOntoLabel(qp, begin))
  }

  val dateQuestionAndCandidateIsDate = (begin: Boolean) => property(pair) { qp: QPPair =>
    questionDateTrigger(qp.question.questionText.toLowerCase) && ontoDate(getPOntoLabel(qp, begin))
  }

  val personTriggerAndPersonNameOnto = (begin: Boolean) => property(pair) { qp: QPPair =>
    questionPersonTrigger(qp.question.questionText) && ontoPerson(getPOntoLabel(qp, begin))
  }

  val personTriggerAndPersonNameConll = (begin: Boolean) => property(pair) { qp: QPPair =>
    questionPersonTrigger(qp.question.questionText) && conllPerson(getPConllLabel(qp, begin))
  }

  val personTriggerAndPersonNamePrefix = (begin: Boolean) => property(pair) { qp: QPPair =>
    questionPersonTrigger(qp.question.questionText) && personNamePrefix.contains(getTokenSurface(qp, begin))
  }

  val personTriggerAndPersonNameWikiData = (begin: Boolean) => property(pair) { qp: QPPair =>
    questionPersonTrigger(qp.question.questionText) &&
      WikiUtils.wikiAskQuery(dropWikiURL(getPWikiLabel(qp, begin)), WikiDataProperties.person, WikiDataProperties.instanceOf, 5)
  }

  val currencyTriggerAndOntoCurrencyLabel = (begin: Boolean) => property(pair) { qp: QPPair =>
    questionCurrencyTrigger(qp.question.questionText) && ontoMoney(getPOntoLabel(qp, begin))
  }

  val percentTriggerAndOntoPercentLabel = (begin: Boolean) => property(pair) { qp: QPPair =>
    questionPercentTrigger(qp.question.questionText) && ontoPercent(getPOntoLabel(qp, begin))
  }

  val nationalityTriggerAndOntoNationalityLabel = (begin: Boolean) => property(pair) { qp: QPPair =>
    questionNationalityTrigger(qp.question.questionText) && ontoNationality(getPOntoLabel(qp, begin))
  }

  val languageTriggerAndOntoLanguageLabel = (begin: Boolean) => property(pair) { qp: QPPair =>
    questionLanguageTrigger(qp.question.questionText) && ontoLanguage(getPOntoLabel(qp, begin))
  }

  val whichWhatTriggerAndOntoLocationLabel = (begin: Boolean) => property(pair) { qp: QPPair =>
    questionWhichWhatTrigger(qp.question.questionText) &&
      questionLocationTrigger(qp.question.questionText) &&
      ontonotesLocationFilter(getPOntoLabel(qp, begin))
  }

  val whichWhatTriggerAndConllLocationLabel = (begin: Boolean) => property(pair) { qp: QPPair =>
    questionWhichWhatTrigger(qp.question.questionText) &&
      questionLocationTrigger(qp.question.questionText) &&
      conllLocationFilter(getPOntoLabel(qp, begin))
  }

  val whichWhatTriggerAndConllInstituteLabel = (begin: Boolean) => property(pair) { qp: QPPair =>
    questionWhichWhatTrigger(qp.question.questionText).toString +
      questionOrgTrigger(qp.question.questionText).toString + getPOntoLabel(qp, begin)
  }

  val whichWhatTriggerAndEntityTriggerAndOntoLabels = (begin: Boolean) => property(pair) { qp: QPPair =>
    questionWhichWhatTrigger(qp.question.questionText).toString +
      questionEntTrigger(qp.question.questionText).toString + getPOntoLabel(qp, begin)
  }

  val whichWhatTriggerAndIsTobeAndEntityLabel = (begin: Boolean) => property(pair) { qp: QPPair =>
    questionWhichWhatTrigger(qp.question.questionText).toString +
      tobe.exists(qp.question.questionText.contains).toString +
      getPOntoLabel(qp, begin)
  }

  // manually extracted type
  val questionTypeConjWithAnnotations = (begin: Boolean) => property(pair) { qp: QPPair =>
    val questionType = extractQuestionTypeInformation(getQShallowParseView(qp).getConstituents.asScala.toList).map(_._1)
    def conjWithType = (str: String) => str + questionType
    List(conjWithType(getPLemmaLabel(qp, begin)), conjWithType(getPPOSLabel(qp, begin)),
      conjWithType(getPChunkLabel(qp, begin)), conjWithType(getPOntoLabel(qp, begin)),
      conjWithType(getPConllLabel(qp, begin)))
  }

  val typeClassifierViewName = "QUESTION_TYPE"
  val questionType = (begin: Boolean) =>  property(pair) { qp: QPPair =>
    val fineType = qp.question.qTAOpt.get.getView(typeClassifierViewName).getConstituents.get(0).getLabel
    val coarseType = qp.question.qTAOpt.get.getView(typeClassifierViewName).getConstituents.get(1).getLabel
    val thresholdedLabelsFine = Seq(-1.0, 0.5).map(th => questionTypeWithScoreThresholded(th, fine = true, qp))
    val thresholdedLabelsCoarse = Seq(-1.0, 0.5).map(th => questionTypeWithScoreThresholded(th, fine = false, qp))
    val typeInfo = List(fineType, coarseType) ++ thresholdedLabelsFine ++ thresholdedLabelsCoarse
    val tokenLabelInfo = List(getPLemmaLabel(qp, begin), getPPOSLabel(qp, begin),
      getPChunkLabel(qp, begin), getPOntoLabel(qp, begin), getPConllLabel(qp, begin))
    typeInfo.flatMap{ typ => tokenLabelInfo.map{ tok => typ + tok } }
  }

  def questionTypeWithScoreThresholded(th: Double, fine: Boolean, qp: QPPair): String = {
    val score = getQTA(qp).getView(typeClassifierViewName).getConstituents.get(if(fine) 0 else 1).getConstituentScore
    (score > th).toString
  }

  val questionTypeClassifierConjWithAnnotations = (begin: Boolean) => property(pair) { qp: QPPair =>
    val questionType = extractQuestionTypeInformation(getQShallowParseView(qp).getConstituents.asScala.toList).map(_._1)
    def conjWithType = (str: String) => str + questionType
    List(conjWithType(getPLemmaLabel(qp, begin)), conjWithType(getPPOSLabel(qp, begin)),
      conjWithType(getPChunkLabel(qp, begin)), conjWithType(getPOntoLabel(qp, begin)),
      conjWithType(getPConllLabel(qp, begin)))
  }

  // TA
  def getQTA(qp: QPPair): TextAnnotation = qp.question.qTAOpt.get
  def getPTA(qp: QPPair): TextAnnotation = qp.paragraph.contextTAOpt.get

  // P Views
  def getPOntoView(qp: QPPair): View = getPTA(qp).getView(ViewNames.NER_ONTONOTES)
  def getPConllView(qp: QPPair): View = getPTA(qp).getView(ViewNames.NER_CONLL)
  def getPWikiView(qp: QPPair): View = getPTA(qp).getView(ViewNames.WIKIFIER)
  def getPPOSView(qp: QPPair): View = getPTA(qp).getView(ViewNames.POS)
  def getPLemmaView(qp: QPPair): View = getPTA(qp).getView(ViewNames.LEMMA)
  def getPShallowParseView(qp: QPPair): View = getPTA(qp).getView(ViewNames.SHALLOW_PARSE)

  // Q Views
  def getQShallowParseView(qp: QPPair): View = getQTA(qp).getView(ViewNames.SHALLOW_PARSE)

  // P labels
  def getPLemmaLabel(qp: QPPair, begin: Boolean): String = getLabel(qp, getPLemmaView(qp), begin)
  def getPPOSLabel(qp: QPPair, begin: Boolean): String = getLabel(qp, getPPOSView(qp), begin)
  def getPOntoLabel(qp: QPPair, begin: Boolean): String = getLabel(qp, getPOntoView(qp), begin)
  def getPConllLabel(qp: QPPair, begin: Boolean): String = getLabel(qp, getPConllView(qp), begin)
  def getPWikiLabel(qp: QPPair, begin: Boolean): String = getLabel(qp, getPWikiView(qp), begin)
  def getPChunkLabel(qp: QPPair, begin: Boolean): String = getLabel(qp, getPShallowParseView(qp), begin)

  def getTokenSurface(qp: QPPair, begin: Boolean): String = {
    qp.paragraph.contextTAOpt.get.getToken(if(begin) qp.beginTokenIdx else qp.endTokenIdx)
  }

  def getLabel(qp: QPPair, vu: View, begin: Boolean): String = {
    vu.getConstituentsCoveringToken(if (begin) qp.beginTokenIdx else qp.endTokenIdx).asScala.head.getLabel.mkString(" ")
  }
}


class SquadClassifier(cType: String = "begin") extends Learnable[QPPair](SquadSolverDataModel.pair) {
  import SquadSolverDataModel._
  def label = cType match {
    case "begin" => beginTokenLabel
    case "end" => endTokenLabel
    case "pair" => pairTokenLabel
    case _ => throw new Exception("Unknown classifier type")
  }
  def cFeatures = cType match {
    case "begin" => List(beginLemma)
    case "end" => List(endLemma)
    case "pair" => List(beginLemma, endLemma)
    case _ => throw new Exception("Unknown classifier type")
  }
  override def feature = using(questionUnigrams +: cFeatures)
  override lazy val classifier = new SparseNetworkLearner {
    val p = new SparseAveragedPerceptron.Parameters()
    p.learningRate = .1
    p.thickness = 4
    baseLTU = new SparseAveragedPerceptron(p)
  }
}

object SquadClassifierUtils {
  val beginClassifier = new SquadClassifier("begin")
  val endClassifier = new SquadClassifier("end")
  val pairClassifier = new SquadClassifier("pair")

  private lazy val annotationUtils = new AnnotationUtils()
  private lazy val trainReader = new SQuADReader(Constants.squadTrainingDataFile, Some(annotationUtils.pipelineService), annotationUtils)

  def populateInstances(): Unit =  {
    def getInstances(i: Int, j: Int): Seq[QPPair] = {
      val qAndpPairs = trainReader.instances.slice(i, j).flatMap { i => i.paragraphs.slice(0, 5).flatMap { p => p.questions.slice(0, 10).map(q => (q, p)) } }.take(1000)
      qAndpPairs.flatMap { case (q, p) => p.contextTAOpt.get.getTokens.indices.map { i => QPPair(q, p, i, i) } }
    }
    SquadSolverDataModel.pair.populate(getInstances(0, 10))
    SquadSolverDataModel.pair.populate(getInstances(11, 20), train = false)
  }
}