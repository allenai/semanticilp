package org.allenai.ari.solvers.textilp

import java.io.{ File, FileWriter, PrintWriter }

import edu.illinois.cs.cogcomp.core.datastructures.ViewNames
import play.api.libs.json.{ JsObject, Json }

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

import scala.collection.mutable.{ Map => MutableMap }

import scala.collection.JavaConverters._

case class Paragraph2(text: String, questions: Seq[SQuestion], id: String = "")

case class SQuestion(text: String, answers: Seq[Answer2], questionId: String = "", sentences: Set[Int] = Set.empty)

case class Answer2(text: String, answerStart: Int = 0, isAns: Boolean = false, scores: MutableMap[String, Double] = MutableMap.empty)

/** Writing the results in the format that squad (hence BiDaF expects this format) */
object Paragraph2 {

  import play.api.libs.json._

  implicit val listOfparagraphWrite = new Writes[Seq[Paragraph2]] {
    def writes(p: Seq[Paragraph2]) = {
      Json.obj(
        "data" ->
          p.zipWithIndex.map {
            case (pp, pIdx) =>
              Json.obj(
                "paragraph" ->
                  Json.obj(
                    "text" -> pp.text,
                    "questions" -> pp.questions.zipWithIndex.map {
                      case (q, qIdx) =>
                        Json.obj(
                          "question" -> q.text,
                          "sentences_used" -> q.sentences,
                          "answers" ->
                            q.answers.map { a =>
                              Json.obj("text" -> a.text, "isAnswer" -> a.isAns)
                            }
                        )
                    }
                  ),
                "id" -> pp.id
              )
          }
      )
    }
  }

  def readJson3(file: String): Seq[Paragraph2] = {
    val content = Source.fromFile(file).getLines().toList.mkString
    val json = Json.parse(content)
    (json \ "data").as[JsArray].value.map { v =>
      val pId = (v \ "id").as[String]
      println(v)
      val p = v \ "paragraph"
      val text = (p \ "text").as[String]
      val questions = (p \ "questions").as[JsArray].value.map { q =>
        val id = (q \ "id").as[String]
        val text = (q \ "question").as[String]
        val sentences_used = (q \ "sentences_used").as[JsArray].value.map {
          _.as[Int]
        }.toSet
        val answers = (q \ "answers").as[JsArray].value.map { a =>
          val text = (a \ "text").as[String]
          val isAnswer = (a \ "isAnswer").as[Boolean]
          Answer2(text, 0, isAnswer)
        }
        SQuestion(text, answers, id, sentences_used)
      }
      Paragraph2(text, questions, pId)
    }
  }

  def writeJson3(file: String, p: Seq[Paragraph2]) = {
    val json = Json.obj(
      "data" ->
        p.zipWithIndex.map {
          case (pp, pIdx) =>
            Json.obj(
              "paragraph" ->
                Json.obj(
                  "text" -> pp.text,
                  "questions" -> pp.questions.zipWithIndex.map {
                    case (q, qIdx) =>
                      Json.obj(
                        "question" -> q.text,
                        "sentences_used" -> q.sentences,
                        "answers" ->
                          q.answers.map { a =>
                            Json.obj("text" -> a.text, "isAnswer" -> a.isAns)
                          },
                        "id" -> q.questionId
                      )
                  }
                ),
              "id" -> pp.id
            )
        }
    ).toString

    val pw = new PrintWriter(new File(file))
    pw.write(json)
    pw.close()
  }

  def writeJson4(file: String, p: Seq[Paragraph2]) = {
    val json = Json.obj(
      "data" ->
        p.zipWithIndex.map {
          case (pp, pIdx) =>
            Json.obj(
              "paragraph" ->
                Json.obj(
                  "text" -> pp.text,
                  "questions" -> pp.questions.zipWithIndex.map {
                    case (q, qIdx) =>
                      Json.obj(
                        "question" -> q.text,
                        "sentences_used" -> q.sentences,
                        "answers" ->
                          q.answers.map { a =>
                            Json.obj(
                              "text" -> a.text,
                              "isAnswer" -> a.isAns,
                              "scores" -> a.scores
                            )
                          },
                        "id" -> q.questionId
                      )
                  }
                ),
              "id" -> pp.id
            )
        }
    ).toString

    val pw = new PrintWriter(new File(file))
    pw.write(json)
    pw.close()
  }

  def readJson4(file: String): Seq[Paragraph2] = {
    val content = Source.fromFile(file).getLines().toList.mkString
    val json = Json.parse(content)
    (json \ "data").as[JsArray].value.map { v =>
      val pId = (v \ "id").as[String]
      println(v)
      val p = v \ "paragraph"
      val text = (p \ "text").as[String]
      val questions = (p \ "questions").as[JsArray].value.map { q =>
        val id = (q \ "id").as[String]
        val text = (q \ "question").as[String]
        val sentences_used = (q \ "sentences_used").as[JsArray].value.map {
          _.as[Int]
        }.toSet
        val answers = (q \ "answers").as[JsArray].value.map { a =>
          val text = (a \ "text").as[String]
          val isAnswer = (a \ "isAnswer").as[Boolean]
          val scores = (a \ "scores").as[Map[String, Double]]
          Answer2(text, 0, isAnswer, scala.collection.mutable.Map(scores.toList: _*))
        }
        SQuestion(text, answers, id, sentences_used)
      }
      Paragraph2(text, questions, pId)
    }
  }
  
  
  
  def readJson5(file: String): Seq[Paragraph2] = {
    val content = Source.fromFile(file).getLines().toList.mkString
    val json = Json.parse(content)
    (json \ "data").as[JsArray].value.map { v =>
      val pId = (v \ "id").as[String]
      //      if(pId.contains("used-oanc-output-Algarve-Intro-1.txt")) {
      //        println(" ===> " + pId)
      //      }
      val p = v \ "paragraph"
      val text = (p \ "text").as[String]
      val questions = (p \ "questions").as[JsArray].value.map { q =>
        val idx = (q \ "idx").as[String]
        val qType = (q \ "multisent").as[Boolean]
        val text = (q \ "question").as[String]
        val sentences_used = (q \ "sentences_used").as[JsArray].value.map {
          _.as[Int]
        }.toSet
        val answers = (q \ "answers").as[JsArray].value.map { a =>
          val text = (a \ "text").as[String]
          val isAnswer = (a \ "isAnswer").as[Boolean]
          val scores = (a \ "scores").as[Map[String, Double]]
          Answer2(text, 0, isAnswer, scala.collection.mutable.Map(scores.toList: _*))
        }
        SQuestion(text, answers, idx, sentences_used, qType)
      }
      Paragraph2(text, questions, pId)
    }
  }

  def writeJson5(file: String, p: Seq[Paragraph2]) = {
    val json = Json.obj(
      "data" ->
        p.zipWithIndex.map {
          case (pp, pIdx) =>
            Json.obj(
              "paragraph" ->
                Json.obj(
                  "text" -> pp.text,
                  "questions" -> pp.questions.zipWithIndex.map {
                    case (q, qIdx) =>
                      Json.obj(
                        "question" -> q.text,
                        "sentences_used" -> q.sentences,
                        "answers" ->
                          q.answers.map { a =>
                            Json.obj(
                              "text" -> a.text,
                              "isAnswer" -> a.isAns,
                              "scores" -> a.scores
                            )
                          },
                        "idx" -> q.questionId,
                        "multisent" -> q.mutliSent
                      )
                  }
                ),
              "id" -> pp.id
            )
        }
    ).toString

    val pw = new PrintWriter(new File(file))
    pw.write(json)
    pw.close()
  }

}
