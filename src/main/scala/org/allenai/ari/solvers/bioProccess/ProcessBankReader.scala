package org.allenai.ari.solvers.bioProccess

import java.io.File

import edu.illinois.cs.cogcomp.annotation.AnnotatorService
import edu.illinois.cs.cogcomp.core.datastructures.ViewNames
import org.allenai.ari.solvers.squad.CandidateGeneration
import org.allenai.ari.solvers.textilp.utils.AnnotationUtils
import org.allenai.ari.solvers.textilp.{Answer, Paragraph, Question}

import scala.xml.XML

class ProcessBankFileReader(file: File, annotationServiceOpt: Option[AnnotatorService] = None, annotationUtils: AnnotationUtils) {
  val instances = {
    val xml = XML.loadFile(file)
    println(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
    val text = (xml \\ "text").head.text.trim
    println("text: " + text)
    val questions = (xml \\ "question").map { q =>
      println("----------")
      val qid = (q \ "qid").text.trim
      val question = (q \ "q").text.replace("\n", "").trim
      val a0 = (q \ "a0").text.replace("\n", "").trim
      val a1 = (q \ "a1").text.replace("\n", "").trim
      val correct = (q \ "correct").text.toInt
      val answers = Seq(Answer(a0, -1), Answer(a1, -1))
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
      println("question: " + question)
      println("answers: " + answers)
      Question(question, qid, answers, questionAnnotation, Some(correct))
    }
    val contextAnnotation = annotationServiceOpt match {
      case None => None
      case Some(service) =>
        val ta = annotationUtils.annotate(text)
        //assert(ta.getAvailableViews.contains(ViewNames.QUANTITIES))
        Some(ta)
    }
    Paragraph(text, questions, contextAnnotation)
  }
}

class ProcessBankReader(annotationServiceOpt: Option[AnnotatorService] = None, annotationUtils: AnnotationUtils) {
  private val d = new File("other/questionSets/biologicalProcesses/qa/")
  private val files = if (d.exists && d.isDirectory) {
    d.listFiles.filter(_.isFile).toList
  } else {
    List[File]()
  }
  private val paragraphs = files.filter(_.getName != ".DS_Store").map{ f =>
    // .DS_Store
    println(f)
    val processQuestions = new ProcessBankFileReader(f, Some(annotationUtils.pipelineService), annotationUtils)
    processQuestions.instances
  }
  val trainingInstances = paragraphs.take(150)
  val testInstances = paragraphs.slice(150, 200)
}

object ProcessBankReader{
  val temporalKeywords = Set(" order", " first", " last", " ordering", " time")
  val trueAns = Set("true", "True", "Trure")
  val falseAns = Set("false", "False")
  val trueOrFalse = trueAns ++ falseAns

  implicit class ImplicitConversionsFromParagraphs(paragraphList: List[Paragraph]) {

    // keeps only true-false questions
    def filterTrueFalse: List[Paragraph] = {
      paragraphList.map{ p =>
        val filteredQuestions = p.questions.filter(q => q.answers.map(a => a.answerText).toSet.intersect(trueOrFalse).nonEmpty)
        Paragraph(p.context, filteredQuestions, p.contextTAOpt)
      }
    }

    def filterNotTrueFalse: List[Paragraph] = {
      paragraphList.map{ p =>
        val filteredQuestions = p.questions.filter(q => q.answers.map(a => a.answerText).toSet.intersect(trueOrFalse).isEmpty)
        Paragraph(p.context, filteredQuestions, p.contextTAOpt)
      }
    }

    // keeps only temporal questions
    def filterTemporals: List[Paragraph] = {
      paragraphList.map { p =>
        val filteredQuestions = p.questions.filter(q => temporalKeywords.exists(q.questionText.contains))
        Paragraph(p.context, filteredQuestions, p.contextTAOpt)
      }
    }

    def filterNotTemporals: List[Paragraph] = {
      paragraphList.map { p =>
        val filteredQuestions = p.questions.filterNot(q => temporalKeywords.exists(q.questionText.contains))
        Paragraph(p.context, filteredQuestions, p.contextTAOpt)
      }
    }
  }

  implicit class ImplicitConversionsFromQuestion(question: Question) {
    def isTrueFalse: Boolean = question.answers.map(a => a.answerText.trim).toSet.intersect(trueOrFalse).nonEmpty
    def isTemporal: Boolean = temporalKeywords.exists(question.questionText.contains)
    def trueIndex: Int = question.answers.zipWithIndex.collectFirst{ case (a, i) if trueAns.contains(a.answerText.trim) => i }.getOrElse(-1)
    def falseIndex: Int = question.answers.zipWithIndex.collectFirst{ case (a, i) if falseAns.contains(a.answerText.trim) => i }.getOrElse(-1)
  }

}