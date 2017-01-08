package org.allenai.ari.solvers.squad

import edu.illinois.cs.cogcomp.core.datastructures.ViewNames
import edu.illinois.cs.cogcomp.lbjava.learn.{SparseAveragedPerceptron, SparseNetworkLearner}
import edu.illinois.cs.cogcomp.saul.classifier.Learnable
import edu.illinois.cs.cogcomp.saul.datamodel.DataModel
import org.allenai.ari.solvers.textilp.QPPair
import org.allenai.ari.solvers.textilp.utils.{AnnotationUtils, Constants, SQuADReader}
import org.rogach.scallop.{ScallopConf, ScallopOption}

import scala.collection.JavaConverters._

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