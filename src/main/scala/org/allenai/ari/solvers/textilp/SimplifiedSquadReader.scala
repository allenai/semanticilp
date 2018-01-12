package org.allenai.ari.solvers.textilp

import java.io.File

import org.allenai.ari.solvers.textilp.utils.AnnotationUtils
import play.api.libs.json._

import scala.io.Source

/** Reads the SQuAD data, given the location to the json
  * file containing the annotated data. More details here:
  * https://rajpurkar.github.io/SQuAD-explorer/
  */
class SimplifiedSquadReader(anno: AnnotationUtils, file: File, maxTopics: Int = -1, maxParagraphs: Int = -1) {
  private val jsonString = Source.fromFile(file).getLines().mkString
  private val jsonObject = Json.parse(jsonString)
  private val topicVals = if (maxTopics > 0) {
    (jsonObject \\ "data").head.as[JsArray].value.slice(0, maxTopics)
  } else {
    (jsonObject \\ "data").head.as[JsArray].value
  }
  val instances = topicVals.zipWithIndex.map {
    case (value, topicIdx) =>
      println(" >>>> processing topic " + topicIdx + " out of " + topicVals.size)
      //println("value: " + value)
      val title = (value \ "title").as[String]
      //val paragraphValues = if (maxParagraphs > 0) {
      //  (value \ "paragraphs").as[JsArray].value.slice(0, maxParagraphs)
      //} else {
      //  (value \ "paragraphs").as[JsArray].value
      //}
      val paragraphs = {
        //case (paragraphValue, idx) =>
        val paragraphValue = value
        val context = (paragraphValue \ "context").as[String]
        println("context: " + context)
        //val contextCleaned = extraTerms.foldRight(context) { case (toDrop, newContext) => newContext.replace(toDrop, "") }.
        //  replaceAll("\\[.{0,20}\\]", ""). // dropping anything in the form of [ ... ]
        //  replaceAll("\\([S|s]ee.{0,40}\\)", ""). // dropping anything in the form of (See ...)
        //  replace(" ;", ";").replaceAll("; and|; the", ". ") // replace ";"s with "."s so that we don't miss splitting them as sentences.
        // creating this only to extract token boundaries from the raw (dirty) paragraph
        val contextAnnotation = anno.pipelineServerClientWithBasicViews.annotate(context)
        // AnnotationUtils.annotateWithBasicViews(context)
        // println("==> text: " + contextCleaned)
        val paragraphQuestionsJson = (paragraphValue \ "qas").as[JsArray]
        //println(" --> number of questions: " + paragraphQuestionsJson.value.size)
        val questions = paragraphQuestionsJson.value.map { qValue =>
          val question = (qValue \ "question").as[String]
          println("question: " + question)
          val id = (qValue \ "id").as[String]
          val answers = (qValue \ "candidates").as[JsArray].value.map { aValue =>
            // println("----")
            //val start = (aValue \ "answer_start").as[Int]
            val text = (aValue \ "candidateText").as[String]
            //text //-> start
            Answer(text, 0, None)
          }
          println("answers: " + answers.mkString("\n"))
          val questionAnnotation = anno.pipelineServerClientWithBasicViews.annotate(question)
          Question(question, id, answers, Some(questionAnnotation))
        } // drop anything that does not have answers
        val p = Paragraph(context, questions, Some(contextAnnotation))
        println(p)
        println("-----------")
        p
      }
      TopicGroup(title, Seq(paragraphs))
  }
}