package org.allenai.ari.solvers.textilp.utils

import org.allenai.ari.solvers.textilp._
import play.api.libs.json._
import scala.io.Source
import java.io.File

/** Reads the SQuAD data, given the location to the json
  * file containing the annotated data. More details here:
  * https://rajpurkar.github.io/SQuAD-explorer/
  */
class SQuADReader(path: String) {
  private val jsonString = Source.fromFile(new File(path)).getLines().mkString
  private val jsonObject = Json.parse(jsonString)
  val instances = (jsonObject \\ "data" ).head.as[JsArray].value.map{ value =>
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
        Question(question, id, answers, None)
      }
      Paragraph(context, questions, None)
    }
    TopicGroup(title, paragraphs)
  }
}