package org.allenai.ari.solvers.bioProccess

import java.io.File

import edu.illinois.cs.cogcomp.annotation.AnnotatorService
import edu.illinois.cs.cogcomp.core.datastructures.ViewNames
import org.allenai.ari.solvers.squad.CandidateGeneration
import org.allenai.ari.solvers.textilp.utils.AnnotationUtils
import org.allenai.ari.solvers.textilp.{Answer, Paragraph, Question}

import scala.xml.XML

class ProcessBankFileReader(file: File, annotationServiceOpt: Option[AnnotatorService] = None, annotationUtils: AnnotationUtils) {
  import ProcessBankReader._
  val instances = {
    val xml = XML.loadFile(file)
    println(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
    val text = normalizeText((xml \\ "text").head.text)
    println("text: " + text)
    val questions = (xml \\ "question").map { q =>
      println("----------")
      val qid = (q \ "qid").text.trim
      val question = (q \ "q").text.replace("\n", "").trim
      val a0 = (q \ "a0").text.replace("\n", "").trim
      val a1 = (q \ "a1").text.replace("\n", "").trim
      val correct = (q \ "correct").text.toInt
//      val a0TA = Some(annotationUtils.annotate(a0)) //if(question.isTemporal) Some(annotationUtils.annotate(a0)) else None
//      val a1TA = Some(annotationUtils.annotate(a1)) //if(question.isTemporal) Some(annotationUtils.annotate(a1)) else None
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
  val temporalKeywords = Set(" order", " first", " last", " ordering", " time", " final")
  val trueAns = Set("true", "True", "Trure")
  val falseAns = Set("false", "False")
  val trueOrFalse = trueAns ++ falseAns
  val causeTriggers = Set(
    "what initiates",
    "what allows ", // example: What allows microtubules to continue to overlap even though they are pushed apart?
    "What directly ", // example: What directly pushes spindle poles apart?
    "directly causes the ", // example:  What directly causes the final cellular response?
    "what caused ", // example: What caused adaptations?
    "which of the following is caused by", // example: Which of the following is caused by the increased frequency of individuals with favorable adaptations?
    "what is required for" // example: What is required for sequencing?
  )

  val resultTriggers = Set(
    "would happen without",
    " is the result of",
    "produces what",
    "what would happen if",
    "what happens when ",
    "what can happen after",
    "what is the immediate effect of",
    "what would happen without",
    "what is caused by ",
    "what would happen without ", // example: What would happen without NADPH?
    "what would happen if ",
    "what would happen without ",
    "what is the result of ",
    "what is caused by ",
    "what causes ",  // What causes one or more extra sets of chromosomes?
    "what is created by ",
    " lead to?", // example: What does descent with modification eventually lead to?
    " cause?", // example: What does the unequal ability of individuals to survive and reproduce cause?
    "what has caused ", // example: What has caused massive increases in speed and decreases in the cost of sequencing?
    "what is the result of ", // example: What is the result of the founder effect?
    " causes what?" // example: Gene flow causes what?)
  )

  def normalizeText(str: String): String = str.trim.replaceAll("\\(Figure .*\\)", "").replaceAll("\\(see Figure .*\\)", "")

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

    def filterCauseQuestions: List[Paragraph] = {
      paragraphList.map { p =>
        val filteredQuestions = p.questions.filter(q => q.isCauseQuestion)
        Paragraph(p.context, filteredQuestions, p.contextTAOpt)
      }
    }

    def filterCResultQuestions: List[Paragraph] = {
      paragraphList.map { p =>
        val filteredQuestions = p.questions.filter(q => q.isForCResultQuestion)
        Paragraph(p.context, filteredQuestions, p.contextTAOpt)
      }
    }
  }

  implicit class ImplicitConversionsFromQuestion(question: Question) {
    def isTrueFalse: Boolean = question.answers.map(a => a.answerText.trim).toSet.intersect(trueOrFalse).nonEmpty
    def isTemporal: Boolean = temporalKeywords.exists(question.questionText.contains)
    def trueIndex: Int = question.answers.zipWithIndex.collectFirst{ case (a, i) if trueAns.contains(a.answerText.trim) => i }.getOrElse(-1)
    def falseIndex: Int = question.answers.zipWithIndex.collectFirst{ case (a, i) if falseAns.contains(a.answerText.trim) => i }.getOrElse(-1)


    def isCauseQuestion: Boolean = causeTriggers.exists(question.questionText.toLowerCase.contains)

    def isForCResultQuestion: Boolean = resultTriggers.exists(question.questionText.toLowerCase.contains)

    // commented out to make it less confusing
    //def causalQuestion: Boolean = lookingForCauseQuestion || lookingForCResultQuestion
  }

  implicit class ImplicitConversionsFromAnswerString(str: String) {
    def isTrueFalse: Boolean = trueOrFalse.contains(str)
    def isTemporal: Boolean = temporalKeywords.exists(str.contains)
  }
}