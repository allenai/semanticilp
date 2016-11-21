package org.allenai.ari.solvers.textilp

import edu.illinois.cs.cogcomp.core.datastructures.ViewNames
import edu.illinois.cs.cogcomp.core.datastructures.textannotation.TextAnnotation
import org.allenai.ari.solvers.textilp.utils.{AnnotationUtils, Constants, SQuADReader}
import org.rogach.scallop._

import scala.collection.JavaConverters._

object ExperimentsApp {

  class ArgumentParser(args: Array[String]) extends ScallopConf(args) {
    val experimentType: ScallopOption[Int] = opt[Int]("type", descr = "Experiment type", required = true)
    verify()
  }

/*  def evaluateSquadWithAristo(inputData: Seq[TopicGroup]) = {

  }
*/

  def testQuantifier(): Unit = {
    val ta = AnnotationUtils.pipelineService.createAnnotatedTextAnnotation("", "",
      "The annual NFL Experience was held at the Moscone Center in San Francisco. In addition, \"Super Bowl City\" opened on January 30 at Justin Herman Plaza on The Embarcadero, featuring games and activities that will highlight the Bay Area's technology, culinary creations, and cultural diversity. More than 1 million people are expected to attend the festivities in San Francisco during Super Bowl Week. San Francisco mayor Ed Lee said of the highly visible homeless presence in this area \"they are going to have to leave\". San Francisco city supervisor Jane Kim unsuccessfully lobbied for the NFL to reimburse San Francisco for city services in the amount of $5 million.")
    AnnotationUtils.quantifierAnnotator.addView(ta)
    println(ta)
    println(ta.getAvailableViews)
  }

  def testPipelineAnnotation(): Unit = {
    val ta = AnnotationUtils.pipelineService.createAnnotatedTextAnnotation("", "",
      "this is a sample senrence that needs to be update with 20 pipelines in Illinois. ")
    println(ta)
    println(ta.getAvailableViews)
  }

  //TODO if "the" is among the candidate answrs, drop it and make it another candidate
  //TODO capture aphabetical numbers too, like "six"
  def generateCandiateAnswers(reader: SQuADReader): Unit = {
    var found: Int = 0
    var notFound: Int = 0
    reader.instances.foreach{ ins =>
      ins.paragraphs.foreach{ p =>
        p.contextTAOpt match {
          case None => throw new Exception("The instance does not contain annotation . . . ")
          case Some(annotation) =>
            val candidateAnswers = getCandidateAnswer(annotation)
            p.questions.foreach{ q =>
              val goldAnswers = q.answers.map(_.answerText)
              if(goldAnswers.exists(candidateAnswers.contains)) {
                println(" --> found ")
                found = found + 1
              }
              else{
                notFound = notFound + 1
                println(" --> not found ")
                println("Question: " + q)
                println("CandidateAnswers: " + candidateAnswers)
                println("context = " + p.context)
              }
            }
        }
      }
    }
    println("found: " + found + "\nnot-found: " + notFound)
  }

  def getCandidateAnswer(contextTA: TextAnnotation): Set[String] = {
    val nounPhrases = contextTA.getView(ViewNames.SHALLOW_PARSE).getConstituents.asScala.filter(_.getLabel.contains("N")).map(_.getSurfaceForm)
    val ners = contextTA.getView(ViewNames.NER_CONLL).getConstituents.asScala.map(_.getSurfaceForm)
    val quant = contextTA.getView(ViewNames.QUANTITIES).getConstituents.asScala.map(_.getSurfaceForm)
    val p = "-?\\d+".r // regex for finding all the numbers
    val numbers = p.findAllIn(contextTA.text)
    (nounPhrases ++ quant ++ ners ++ numbers).toSet
  }

  def main(args: Array[String]): Unit = {
    val parser = new ArgumentParser(args)
    parser.experimentType() match {
      case 1 =>
        val trainReader = new SQuADReader(Constants.squadTrainingDataFile, Some(AnnotationUtils.pipelineService))
        //val devReader = new SQuADReader(Constants.squadDevDataFile, Some(AnnotationUtils.pipelineService))
      generateCandiateAnswers(trainReader)
      case 2 => testQuantifier()
      case 3 => testPipelineAnnotation()
    }
  }
}
