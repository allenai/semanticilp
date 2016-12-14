package org.allenai.ari.solvers.textilp.solvers

import org.allenai.ari.solvers.textilp.{AlignmentResults, EntityRelationResult, ResultJson}
import org.allenai.ari.solvers.textilp.utils.SolverUtils

class SalienceSolver extends TextSolver {

  /** this solver ignores the input snippet */
  def solve(question: String, options: Set[String], snippet: String): (AlignmentResults, EntityRelationResult) = {
    SolverUtils.handleQuestionWithManyCandidates(question, options, "salience") -> ResultJson.emptyEntityRelation
  }
}
