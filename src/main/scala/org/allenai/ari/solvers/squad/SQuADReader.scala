package org.allenai.ari.solvers.squad

import java.io.File

import edu.illinois.cs.cogcomp.annotation.AnnotatorService
import edu.illinois.cs.cogcomp.core.datastructures.ViewNames
import org.allenai.ari.solvers.textilp._
import org.allenai.ari.solvers.textilp.utils.AnnotationUtils
import play.api.libs.json._

import scala.io.Source

/** Reads the SQuAD data, given the location to the json
  * file containing the annotated data. More details here:
  * https://rajpurkar.github.io/SQuAD-explorer/
  */
class SQuADReader(file: File, annotationServiceOpt: Option[AnnotatorService] = None, annotationUtils: AnnotationUtils) {
  private val jsonString = Source.fromFile(file).getLines().mkString
  private val jsonObject = Json.parse(jsonString)
  private val extraTerms = Seq("[citation needed]", "[b]", "[a]", "[info]", "[update]", "[f]", "[page needed]")
  private val topicVals = (jsonObject \\ "data").head.as[JsArray].value
  val instances = topicVals.zipWithIndex.map { case (value, topicIdx) =>
    println(" >>>> processing topic " + topicIdx + " out of " + topicVals.size)
    val title = (value \ "title").as[String]
    val paragraphValues = (value \ "paragraphs").as[JsArray].value
    val paragraphs = paragraphValues.zipWithIndex.map { case (paragraphValue, idx) =>
      println("Idx " + idx)
      val context = (paragraphValue \ "context").as[String]
      val contextCleaned = extraTerms.foldRight(context){ case (toDrop, newContext) => newContext.replace(toDrop, "") }
      val questions = (paragraphValue \ "qas").as[JsArray].value.map { qValue =>
        val question = (qValue \ "question").as[String]
        val id = (qValue \ "id").as[String]
        val answers = (qValue \ "answers").as[JsArray].value.map { aValue =>
          val start = (aValue \ "answer_start").as[Int]
          val text = (aValue \ "text").as[String]
          Answer(text, start)
        }
        val questionAnnotation = annotationServiceOpt match {
          case None => None
          case Some(service) =>
            val ta = annotationUtils.annotate(question)
            CandidateGeneration.questionTypeClassification.addView(ta)
            //assert(ta.getAvailableViews.contains(ViewNames.QUANTITIES))
            assert(ta.getAvailableViews.contains(ViewNames.LEMMA))
            assert(ta.getAvailableViews.contains(CandidateGeneration.questionTypeClassification.finalViewName))
            Some(ta)
        }
        Question(question, id, answers, questionAnnotation)
      }
      val contextAnnotation = annotationServiceOpt match {
        case None => None
        case Some(service) =>
          val ta = annotationUtils.annotate(contextCleaned)
          //assert(ta.getAvailableViews.contains(ViewNames.QUANTITIES))
          Some(ta)
      }
      Paragraph(contextCleaned, questions, contextAnnotation)
    }
    TopicGroup(title, paragraphs)
  }
}