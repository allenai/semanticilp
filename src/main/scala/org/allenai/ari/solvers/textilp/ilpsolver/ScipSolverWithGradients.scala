package org.allenai.ari.solvers.textilp.ilpsolver

/** An extension of ScipSolver that keeps track of gradients of the objective function w.r.t.
  * a set of parameters.
  */
class ScipSolverWithGradients(
    probName: String,
    scipParams: ScipParams
) extends ScipSolver(probName, scipParams) with IlpSolverWithGradients[ScipVar, ScipCons] {
  // Nothing to implement; this class uses implementations in IlpSolverWithGradients when
  // available, such as for createBinaryVariableWithGradients()
}
