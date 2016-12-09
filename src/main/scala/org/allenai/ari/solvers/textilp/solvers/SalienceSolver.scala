package org.allenai.ari.solvers.textilp.solvers

import org.allenai.ari.solvers.textilp.utils.SolverUtils

class SalienceSolver {
  def solver(question: String, options: Set[String], snippet: String): Seq[(String, Double)] = {
    SolverUtils.handleQuestionWithManyCandidates(question, options, snippet)
  }
}
