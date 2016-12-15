package org.allenai.ari.solvers.textilp.utils

import java.net.URLEncoder

import org.allenai.ari.solvers.textilp.{AlignmentResults, Entity, EntityRelationResult, TermAlignment}
import play.api.libs.json.{JsArray, JsNumber, Json}

import scala.io.Source

object SolverUtils {
  def handleQuestionWithManyCandidates(onlyQuestion: String, candidates: Set[String], solver: String): Seq[(String, Double)] = {
    val optionScores = candidates.grouped(6).foldRight(Seq[(String, Double)]()) { (smallGroupOfCandidates, combinedScoreMap) =>
      assert(smallGroupOfCandidates.size <= 6)
      val allOptions = smallGroupOfCandidates.zipWithIndex.map { case (opt, idx) => s" (${(idx + 'A').toChar}) $opt " }.mkString
      val smallQuestion = onlyQuestion + allOptions
      combinedScoreMap ++ evaluateASingleQuestion(smallQuestion, solver)
    }

//    val maxIndex = optionScores.zipWithIndex.maxBy(_._1._2)._2
//    val options = optionScores.map{case (str, score) => TermAlignment(str)}.toList
//    options(maxIndex).alignmentIds += 0
//
//    AlignmentResults(List(TermAlignment(onlyQuestion, ArrayBuffer(0))),
//      options, List(TermAlignment("")))
    optionScores
  }

  /** query question against existing remote solvers
    * The question can have at most 6 options, A to F: "question text (A) option1 (B) option2 .... "
    */
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

  def sortedAnswerToSolverResponse(question: String, options: Set[String], snippet: String, sortedCanndidates: Seq[(String, Double)]): (AlignmentResults, EntityRelationResult) = {
    val maxScore = sortedCanndidates.head._2
    val selectedAnswers = sortedCanndidates.filter(_._2 == maxScore)

    val questionString = "Question: " + question
    val choiceString = "|Options: " + options.zipWithIndex.map { case (ans, key) => s" (${key + 1}) " + ans }.mkString(" ")
    val paragraphString = "|Paragraph: " + snippet
    val fullText = questionString + paragraphString + choiceString
    val entities = selectedAnswers.map { case (str, _) =>
      val begin = choiceString.indexOf(str) + paragraphString.length + questionString.length
      val end = begin + str.length
      Entity("  ", str, Seq((begin, end)))
    }
    AlignmentResults() -> EntityRelationResult(fullText, entities, Seq.empty, sortedCanndidates.toString)
  }
}
