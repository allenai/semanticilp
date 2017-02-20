package org.allenai.ari.solvers.textilp.ilpsolver

import org.scalatest.{ FlatSpec, Matchers }

class ScipSolverSpec extends FlatSpec with Matchers {

  /** Build a simple ILP model: x0 + 2*x1 <= 2, objective function: - x0 - x1.
    *
    * @param scipSolver a ScipInterface object
    * @return a seq of (a subset of) variables of interest whose values may be queried later
    */
  private def buildSimpleModel(scipSolver: ScipSolver): Seq[ScipVar] = {
    // create binary variables
    val nvars = 2
    val vars = for {
      i <- 0 until nvars
      name = s"x$i"
      objCoeff = -1d
      x = scipSolver.createBinaryVar(name, objCoeff)
    } yield {
      scipSolver.addVar(x)
      x
    }
    val varsOfInterest = new Array[ScipVar](1)
    varsOfInterest(0) = vars(1) // add x1 to the list of variables of interest to return

    // create coefficients for the constraint
    val coeffs = (0 until nvars).map(_ + 1d)

    // add a linear constraint
    scipSolver.addConsBasicLinear("test", vars, coeffs, Some(0d), Some(2d))

    // return vars of interest
    varsOfInterest
  }

  "scipSolver" should "solve a simple ILP program correctly" in {
    val scipParams = new ScipParams(10d, 1, "scip.log", messagehdlrQuiet = true, 0)
    val scipSolver = new ScipSolver("example", scipParams)
    val varsOfInterest = buildSimpleModel(scipSolver)
    scipSolver.solve()

    // retrieve solution
    val primalBound = scipSolver.getPrimalbound
    val dualBound = scipSolver.getDualbound
    val solutionVals = scipSolver.getSolVals(varsOfInterest)

    // check solution
    primalBound should equal(dualBound)
    assert(primalBound === -1d +- 1e-4)
    assert(solutionVals === Seq(0d))
  }

}