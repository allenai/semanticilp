package org.allenai.ari.solvers.textilp.solvers

import org.allenai.ari.solvers.textilp.EntityRelationResult
import org.allenai.ari.solvers.textilp.utils.SolverUtils

class SalienceSolver extends TextSolver {

  /** this solver ignores the input snippet */
  def solve(question: String, options: Seq[String], snippet: String): (Seq[Int], EntityRelationResult) = {
    val sortedCanndidates = SolverUtils.handleQuestionWithManyCandidates(question, options, "salience").sortBy(-_._2)
    SolverUtils.sortedAnswerToSolverResponse(question, options, snippet, sortedCanndidates)
  }
}
