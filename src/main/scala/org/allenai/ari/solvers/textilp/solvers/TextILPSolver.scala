package org.allenai.ari.solvers.textilp.solvers

import edu.illinois.cs.cogcomp.core.datastructures.ViewNames
import edu.illinois.cs.cogcomp.infer.ilp.OJalgoHook
import org.allenai.ari.solvers.textilp.alignment.{AlignmentFunction, KeywordTokenizer}
import org.allenai.ari.solvers.textilp._
import org.allenai.ari.solvers.textilp.ilpsolver._
import org.allenai.ari.solvers.textilp.utils.AnnotationUtils

import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer

class TextILPSolver extends TextSolver {
  val epsilon = 0.001

  lazy val keywordTokenizer = KeywordTokenizer.Default
  lazy val aligner = new AlignmentFunction("Entailment", 0.1, keywordTokenizer,
    useRedisCache = true, useContextInRedisCache = false)

  def solve(question: String, options: Set[String], snippet: String): (AlignmentResults, EntityRelationResult) = {
    val ilpSolver = new ScipSolver("textILP", ScipParams.Default)
//    val ilpSolver = new IllinoisInference(new OJalgoHook)
    val answers = options.map(o => Answer(o, -1))
    val qTA = AnnotationUtils.pipelineService.createBasicTextAnnotation("", "", question)
    val q = Question(question, "", answers, Some(qTA))
    val pTA = AnnotationUtils.pipelineService.createBasicTextAnnotation("", "", snippet) // AnnotationUtils.annotate(snippet, withQuantifier = false)
    val p = Paragraph(snippet, Seq(q), Some(pTA))
    createILPModel(q, p, ilpSolver, aligner)
  }

  def createILPModel[V <: IlpVar](
    q: Question,
    p: Paragraph,
    ilpSolver: IlpSolver[V, _],
    alignmentFunction: AlignmentFunction
  ): (AlignmentResults, EntityRelationResult) = {

    println("starting to create the model  . . . ")

    require(q.qTAOpt.isDefined, "the annotatins for the question is not defined")
    require(p.contextTAOpt.isDefined, "the annotatins for the paragraph is not defined")
    val qTA = q.qTAOpt.get
    val pTA = p.contextTAOpt.get
    val qTokens = qTA.getView(ViewNames.TOKENS).getConstituents.asScala.toSeq
    val pTokens = pTA.getView(ViewNames.TOKENS).getConstituents.asScala.toSeq

    ilpSolver.setAsMaximization()

    // create questionToken-paragraphToken alignment edges
    val questionParagraphAlignments = for {
      qCons <- qTokens
      pCons <- pTokens
      score = alignmentFunction.scoreCellCell(qCons.getSurfaceForm, pCons.getSurfaceForm)
      x = ilpSolver.createBinaryVar("", score)
      tmp = ilpSolver.addVar(x)
    } yield (qCons, pCons, x)

    // create paragraphToken-answerOption alignment edges
    val paragraphAnswerAlignments = for {
      pCons <- pTokens
      ans <- q.answers
      score = alignmentFunction.scoreCellCell(pCons.getSurfaceForm, ans.answerText)
      x = ilpSolver.createBinaryVar("", score)
      tmp = ilpSolver.addVar(x)
    } yield (pCons, ans, x)

    // high-level variables
    // active answer options
    val activeAnswerOptions = for {
      ans <- q.answers
      x = ilpSolver.createBinaryVar("", 0.0)
      tmp = ilpSolver.addVar(x)
    } yield (ans, x)

    def getVariablesConnectedToOption(ans: Answer): Seq[V] = {
      paragraphAnswerAlignments.filter { case (_, ansTmp, _) => ansTmp == ans }.map(_._3)
    }

    // variable must be active if anything connected to it is active
    activeAnswerOptions.foreach {
      case (ans, x) =>
        val connectedVariables = getVariablesConnectedToOption(ans)
        val allVars = connectedVariables :+ x
        val coeffs = Seq.fill(connectedVariables.length)(-1.0) :+ 1.0
        ilpSolver.addConsBasicLinear("activeOptionVar", allVars, coeffs, None, Some(0.0))
        connectedVariables.foreach { connectedVar =>
          val vars = Seq(connectedVar, x)
          val coeffs = Seq(1.0, -1.0)
          ilpSolver.addConsBasicLinear("activeOptionVar", vars, coeffs, None, Some(0.0))
        }
    }

    // constraints
    // alignment to only one option, i.e. there must be only one single active option
    val activeAnsVars = activeAnswerOptions.map { case (ans, x) => x }.toSeq
    val coeffs = Seq.fill(activeAnsVars.length)(1.0)
    ilpSolver.addConsBasicLinear("onlyOneActiveOption", activeAnsVars, coeffs, Some(1.0), Some(1.0))

    // sparsity parameters
    // alignment is preferred for lesser sentences

    println("created the ilp model. Now solving it  . . . ")

    println("Number of binary variables: " + ilpSolver.getNBinVars)
    println("Number of continuous variables: " + ilpSolver.getNContVars)
    println("Number of integer variables: " + ilpSolver.getNIntVars)
    println("Number of constraints: " + ilpSolver.getNConss)

    // solving and extracting the answer
    ilpSolver.solve()

    println("Done solving the model  . . . ")

    // extracting the solution
    val questionAlignments = qTokens.map { c => c -> TermAlignment(c.getSurfaceForm) }.toMap
    val choiceAlignments = q.answers.map { c => c -> TermAlignment(c.answerText) }.toMap
    val paragraphAlignments = pTokens.map { c => c -> TermAlignment(c.getSurfaceForm) }.toMap

    var iter = 0
    questionParagraphAlignments.foreach {
      case (c1, c2, x) =>
        if (ilpSolver.getSolVal(x) > 1.0 - epsilon) {
          questionAlignments(c1).alignmentIds.+=(iter)
          paragraphAlignments(c2).alignmentIds.+=(iter)
          iter = iter + 1
        }
    }


    val questionString = "Question: " + qTokens.map(_.getSurfaceForm).mkString(" ")
    val choiceString = "|Options: " + q.answers.zipWithIndex.map{case (ans, key) => s" (${key+1}) " + ans.answerText}.mkString(" ")
    val paragraphString = "|Paragraph: " + pTokens.map(_.getSurfaceForm).mkString(" ")

    val entities = ArrayBuffer[Entity]()
    val relations = ArrayBuffer[Relation]()
    var eIter = 0
    var rIter = 0

    questionParagraphAlignments.foreach {
      case (c1, c2, x) =>
        if (ilpSolver.getSolVal(x) > 1.0 - epsilon) {
          val t1 = "T" + eIter
          val t2 = "T" + (eIter + 1)
          val qBeginIndex = questionString.indexOf(c1.getSurfaceForm)
          val qEndIndex = qBeginIndex + c1.getSurfaceForm.length
          val pBeginIndex = paragraphString.indexOf(c2.getSurfaceForm) + questionString.length + choiceString.length
          val pEndIndex = pBeginIndex + c2.getSurfaceForm.length
          entities += Entity(t1, c1.getSurfaceForm, Seq((qBeginIndex, qEndIndex)))
          entities += Entity(t2, c2.getSurfaceForm, Seq((pBeginIndex, pEndIndex)))
          relations += Relation("R" + rIter, t1, t2)
          eIter = eIter + 2
          rIter = rIter + 1
        }
    }

    paragraphAnswerAlignments.foreach {
      case (c1, c2, x) =>
        if (ilpSolver.getSolVal(x) > 1.0 - epsilon) {
          paragraphAlignments(c1).alignmentIds.+=(iter)
          choiceAlignments(c2).alignmentIds.+=(iter)
          iter = iter + 1
        }
    }

    paragraphAnswerAlignments.foreach {
      case (c1, c2, x) =>
        if (ilpSolver.getSolVal(x) > 1.0 - epsilon) {
          val t1 = "T" + eIter
          val t2 = "T" + (eIter + 1)
          val pBeginIndex = paragraphString.indexOf(c1.getSurfaceForm) + questionString.length + choiceString.length
          val pEndIndex = pBeginIndex + c1.getSurfaceForm.length
          val oBeginIndex = choiceString.indexOf(c2.answerText) + questionString.length
          val oEndIndex = oBeginIndex + c2.answerText.length
          entities += Entity(t1, c1.getSurfaceForm, Seq((pBeginIndex, pEndIndex)))
          entities += Entity(t2, c2.answerText, Seq((oBeginIndex, oEndIndex)))
          relations += Relation("R" + rIter, t1, t2)
          eIter = eIter + 2
          rIter = rIter + 1
        }
    }

    println("returning the answer  . . . ")

    val alignmentResult = AlignmentResults(
      questionAlignments.values.toList,
      choiceAlignments.values.toList,
      paragraphAlignments.values.toList
    )

    val erView = EntityRelationResult(questionString + choiceString + paragraphString, entities, relations)

    alignmentResult -> erView
  }
}
