package org.allenai.ari.solvers.textilp.solvers

import org.allenai.ari.solvers.textilp.EntityRelationResult

trait TextSolver {
  def solve(question: String, options: Seq[String], snippet: String): (Seq[Int], EntityRelationResult)
}
