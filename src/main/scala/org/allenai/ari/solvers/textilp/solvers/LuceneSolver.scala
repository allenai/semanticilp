package org.allenai.ari.solvers.textilp.solvers

import org.allenai.ari.solvers.textilp.utils.SolverUtils
import org.allenai.ari.solvers.textilp.{AlignmentResults, EntityRelationResult}

class LuceneSolver extends TextSolver {

  /** this solver ignores the input snippet */
  def solve(question: String, options: Set[String], snippet: String): (AlignmentResults, EntityRelationResult) = {
    val sortedCanndidates = SolverUtils.handleQuestionWithManyCandidates(question, options, "lucene").sortBy(-_._2)
    SolverUtils.sortedAnswerToSolverResponse(question, options, snippet, sortedCanndidates)
  }
}
