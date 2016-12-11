package org.allenai.ari.solvers.textilp.solvers

import edu.illinois.cs.cogcomp.core.datastructures.ViewNames
import org.allenai.ari.solvers.textilp.alignment.AlignmentFunction
import org.allenai.ari.solvers.textilp.{Answer, Paragraph, Question}
import org.allenai.ari.solvers.textilp.ilpsolver.{IlpSolver, IlpVar}

import scala.collection.JavaConverters._

class TextILPSolver {
  def solver(question: String, options: Set[String], snippet: String): Seq[(String, Double)] = {
    //val ilpModel = new ScipSolver("textILP", ScipParams.Default)

    Seq[(String, Double)]()
  }

  def createILPModel[V <: IlpVar](q: Question, p: Paragraph, ilpSolver: IlpSolver[V, _], alignmentFunction: AlignmentFunction): IlpSolver = {
    require(q.qTAOpt.isDefined, "the annotatins for the question is not defined")
    require(p.contextTAOpt.isDefined, "the annotatins for the paragraph is not defined")
    val qTA = q.qTAOpt.get
    val pTA = p.contextTAOpt.get
    val qTokens = qTA.getView(ViewNames.TOKENS).getConstituents.asScala.toSeq
    val pTokens = pTA.getView(ViewNames.TOKENS).getConstituents.asScala.toSeq

    // create questionToken-paragraphToken alignment edges
    val questionParagraphAlignments = for{
      qCons <- qTokens
      pCons <- pTokens
      score = alignmentFunction.scoreCellCell(qCons.getSurfaceForm, pCons.getSurfaceForm)
      x = ilpSolver.createBinaryVar("", score)
      ilpSolver.addVar(x)
    }
      yield (qCons, pCons, x)

    // create paragraphToken-answerOption alignment edges
    val paragraphAnswerAlignments = for{
      pCons <- pTokens
      ans <- q.answers
      score = alignmentFunction.scoreCellCell(pCons.getSurfaceForm, ans.answerText)
      x = ilpSolver.createBinaryVar("", score)
      ilpSolver.addVar(x)
    }
      yield (pCons, ans, x)


    // high-level variables
    // active answer options
    val activeAnswerOptions = for{
      ans <- q.answers
      x = ilpSolver.createBinaryVar("", 0.0)
      ilpSolver.addVar(x)
    }
      yield (ans, x)

    // variable must be active if anything connected to it is active
    activeAnswerOptions.foreach { case (ans, x) =>
      val connectedVariables = getVariablesConnectedToOption(ans)
      val allVars = connectedVariables :+ x
      val coeffs = Seq.fill(connectedVariables.length)(-1.0) :+ 1.0
      ilpSolver.addConsBasicLinear("activeOptionVar", allVars, coeffs, None, Some(0.0))
      connectedVariables.foreach{ connectedVar =>
        val vars = Seq(connectedVar, x)
        val coeffs = Seq(1.0, -1.0)
        ilpSolver.addConsBasicLinear("activeOptionVar", vars, coeffs, None, Some(0.0))
      }
    }

    // constraints
    // alignment to only one option, i.e. there must be only one single active option
    val activeAnsVars = activeAnswerOptions.map { case (ans, x) => x }
    val coeffs = Seq.fill(activeAnsVars.length)(1.0)
    ilpSolver.addConsBasicLinear("onlyOneActiveOption", activeAnsVars, coeffs, Some(1.0), Some(1.0))

    // sparsity parameters
    // alignment is preferred for lesser sentences

    def getVariablesConnectedToOption(ans: Answer): Seq[V] = {
      paragraphAnswerAlignments.filter{case (_, ansTmp, _) => ansTmp == ans}.map(_._3)
    }
  }
}
