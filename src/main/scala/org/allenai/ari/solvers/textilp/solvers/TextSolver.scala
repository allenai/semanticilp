package org.allenai.ari.solvers.textilp.solvers

trait TextSolver {
  def solver(question: String, options: Set[String], snippet: String): Seq[(String, Double)]
}
