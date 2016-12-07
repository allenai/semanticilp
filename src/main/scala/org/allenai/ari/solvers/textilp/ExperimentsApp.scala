package org.allenai.ari.solvers.textilp

import java.net.URLEncoder

import edu.illinois.cs.cogcomp.core.datastructures.ViewNames
import edu.illinois.cs.cogcomp.core.datastructures.textannotation.TextAnnotation
import org.allenai.ari.solvers.textilp.utils.{AnnotationUtils, Constants, SQuADReader}
import org.rogach.scallop._
import play.api.libs.json.{JsArray, JsNumber, Json}

import scala.collection.JavaConverters._
import scala.io.Source

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
    reader.instances.zipWithIndex.foreach{ case (ins, idx) =>
      println("Idx: " + idx + " / ratio: " + idx * 1.0 / reader.instances.size)
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
    val nounPhrases = contextTA.getView(ViewNames.SHALLOW_PARSE).getConstituents.asScala.
      filter{ch => ch.getLabel.contains("N") || ch.getLabel.contains("J") || ch.getLabel.contains("V") }.map(_.getSurfaceForm)
    val quotationExtractionPattern = "([\"'])(?:(?=(\\\\?))\\2.)*?\\1".r
    val stringsInsideQuotationMark = quotationExtractionPattern.findAllIn(contextTA.text)
    val ners = contextTA.getView(ViewNames.NER_CONLL).getConstituents.asScala.map(_.getSurfaceForm)
    val ners_onto = contextTA.getView(ViewNames.NER_ONTONOTES).getConstituents.asScala.map(_.getSurfaceForm)
    val quant = contextTA.getView(ViewNames.QUANTITIES).getConstituents.asScala.map(_.getSurfaceForm)
    val p = "-?\\d+".r // regex for finding all the numbers
    val numbers = p.findAllIn(contextTA.text)
    (nounPhrases ++ quant ++ ners ++ ners_onto ++ numbers ++ stringsInsideQuotationMark).toSet
  }

  /** query question against existing remote solvers
    * The question can have at most 6 options, A to F: "question text (A) option1 (B) option2 .... "
    * */
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

  def handleQuestionWithManyCandidates(onlyQuestion: String, candidates: Set[String], solver: String): Seq[(String, Double)] = {
    candidates.grouped(6).foldRight(Seq[(String, Double)]()){ (smallGroupOfCandidates, combinedScoreMap) =>
      assert(smallGroupOfCandidates.size <= 6)
      val allOptions = smallGroupOfCandidates.zipWithIndex.map{ case (opt, idx) => s" (${(idx + 'A').toChar}) $opt " }.mkString
      val smallQuestion = onlyQuestion + allOptions
      combinedScoreMap ++ evaluateASingleQuestion(smallQuestion, solver)
    }
  }

  def evaluateDataSetWithRemoteSolver(reader: SQuADReader, solver: String): Unit = {
    reader.instances.slice(0, 3).zipWithIndex.foreach{ case (ins, idx) =>
      println("Idx: " + idx + " / ratio: " + idx * 1.0 / reader.instances.size)
      ins.paragraphs.slice(0, 3).foreach{ p =>
        p.contextTAOpt match {
          case None => throw new Exception("The instance does not contain annotation . . . ")
          case Some(annotation) =>
            val candidateAnswers = getCandidateAnswer(annotation)
            p.questions.foreach{ q =>
              val goldAnswers = q.answers.map(_.answerText)
              val perOptionScores = handleQuestionWithManyCandidates(q.questionText, candidateAnswers, solver)
              println("q.questionText = " + q.questionText)
              println("gold = " + goldAnswers)
              println("predicted = " + perOptionScores.sortBy(-_._2))
              println("---------")
            }
        }
      }
    }
  }

  def testRemoteSolverWithSampleQuestion() = {
    evaluateASingleQuestion("Which two observations are both used to describe weather? (A) like (B) the difference (C) events (D) temperature and sky condition", "tableilp")
  }

  def main(args: Array[String]): Unit = {
    lazy val trainReader = new SQuADReader(Constants.squadTrainingDataFile, Some(AnnotationUtils.pipelineService))
    lazy val devReader = new SQuADReader(Constants.squadDevDataFile, Some(AnnotationUtils.pipelineService))
    val parser = new ArgumentParser(args)
    parser.experimentType() match {
      case 1 =>
        generateCandiateAnswers(devReader)
      case 2 => testQuantifier()
      case 3 => testPipelineAnnotation()
      case 4 => testRemoteSolverWithSampleQuestion()
      case 5 => evaluateDataSetWithRemoteSolver(devReader, "salience")
    }
  }
}
