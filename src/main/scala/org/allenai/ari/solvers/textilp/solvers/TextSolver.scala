package org.allenai.ari.solvers.textilp.solvers

import org.allenai.ari.solvers.textilp.{AlignmentResults, EntityRelationResult}

trait TextSolver {
  def solve(question: String, options: Set[String], snippet: String): (AlignmentResults, EntityRelationResult)
}
