package org.allenai.ari.solvers.textilp.utils

import edu.illinois.cs.cogcomp.annotation.AnnotatorService
import org.allenai.ari.solvers.textilp._
import play.api.libs.json._

import scala.io.Source
import java.io.File

import edu.illinois.cs.cogcomp.core.datastructures.ViewNames

/** Reads the SQuAD data, given the location to the json
  * file containing the annotated data. More details here:
  * https://rajpurkar.github.io/SQuAD-explorer/
  */
class SQuADReader(file: File, annotationServiceOpt: Option[AnnotatorService] = None) {
  private val jsonString = Source.fromFile(file).getLines().mkString
  private val jsonObject = Json.parse(jsonString)
  val instances = (jsonObject \\ "data" ).head.as[JsArray].value.slice(0, 30).map{ value =>
    val title = (value \ "title").as[String]
    val paragraphValues = (value \ "paragraphs").as[JsArray].value
    val paragraphs = paragraphValues.map{ paragraphValue =>
      val context = (paragraphValue \ "context").as[String]
      val questions = (paragraphValue \ "qas").as[JsArray].value.map{ qValue =>
        val question = (qValue \ "question").as[String]
        val id = (qValue \ "id").as[String]
        val answers = (qValue \ "answers").as[JsArray].value.map{ aValue =>
          val start = (aValue \ "answer_start").as[Int]
          val text = (aValue \ "text").as[String]
          Answer(text, start)
        }
        val questionAnnotation = annotationServiceOpt match {
          case None => None
          case Some(service) =>
            val ta = AnnotationUtils.annotate(question)
            assert(ta.getAvailableViews.contains(ViewNames.QUANTITIES))
            Some(ta)
        }
        Question(question, id, answers, questionAnnotation)
      }
      val contextAnnotation = annotationServiceOpt match {
        case None => None
        case Some(service) =>
          val ta = AnnotationUtils.annotate(context)
          assert(ta.getAvailableViews.contains(ViewNames.QUANTITIES))
          Some(ta)
      }
      Paragraph(context, questions, contextAnnotation)
    }
    TopicGroup(title, paragraphs)
  }
}