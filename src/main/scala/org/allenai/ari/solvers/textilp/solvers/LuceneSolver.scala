package org.allenai.ari.solvers.textilp.solvers

import org.allenai.ari.solvers.textilp.utils.SolverUtils
import org.allenai.ari.solvers.textilp.EntityRelationResult

class LuceneSolver extends TextSolver {

  /** this solver ignores the input snippet */
  def solve(question: String, options: Seq[String], snippet: String): (Seq[Int], EntityRelationResult) = {
    val sortedCanndidates = SolverUtils.handleQuestionWithManyCandidates(question, options, "lucene").sortBy(-_._2)
    //    println("sortedCanndidates" + sortedCanndidates)
    SolverUtils.sortedAnswerToSolverResponse(question, options, snippet, sortedCanndidates)
  }
}
