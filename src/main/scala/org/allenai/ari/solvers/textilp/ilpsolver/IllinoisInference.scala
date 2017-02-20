package org.allenai.ari.solvers.textilp.ilpsolver

import edu.illinois.cs.cogcomp.infer.ilp.ILPSolver

/** Class to capture SCIP's implementation of variable type */
case class IllVar(ptr: Int) extends IlpVar

/** Class to capture SCIP's implementation of constraint type */
case class IllCons(ptr: Int) extends IlpCons

class IllinoisInference(ilpSolver: ILPSolver) extends IlpSolver[IllVar, IllCons] {

  /** set objective function as minimization */
  override def setAsMinimization(): Unit = ilpSolver.setMaximize(false)

  /** set objective function as maximization */
  override def setAsMaximization(): Unit = ilpSolver.setMaximize(true)

  /** create a binary variable */
  override def createBinaryVar(name: String, obj: Double): IllVar = IllVar(ilpSolver.addBooleanVariable(obj))

  /** create a relaxed binary variable, that is, a continuous various with domain [0,1] */
  override def createRelaxedBinaryVar(name: String, obj: Double): IllVar = ???

  /** create an integer variable */
  override def createIntegerVar(name: String, lb: Double, ub: Double, objCoeff: Double): IllVar =
    IllVar(ilpSolver.addIntegerVariable(objCoeff))

  /** create a continuous variable */
  override def createContinuousVar(name: String, lb: Double, ub: Double, objCoeff: Double): IllVar = ???

  /** add variable to the environment */
  override def addVar(x: IllVar): Unit = ???

  /** add constraint to the environment */
  override def addCons(c: IllCons): Unit = ???

  /** get the name of a variable */
  override def varGetName(l: IllVar): String = ???

  /** gets all the variables */
  override def getAllVars: Array[IllVar] = ???

  /** gets all the variables that are active in the final solution */
  override def getAllActiveVars: Array[IllVar] = ???

  /** get number of variables in the original ILP */
  override def getNOrigVars: Int = ???

  /** get number of binary variables in the original ILP */
  override def getNOrigBinVars: Int = ???

  /** get number of integer variables in the original ILP */
  override def getNOrigIntVars: Int = ???

  /** get number of continuous variables in the original ILP */
  override def getNOrigContVars: Int = ???

  /** get number of constraints in the original ILP */
  override def getNOrigConss: Int = ???

  /** get number of currently active variables when this method is called */
  override def getNVars: Int = ???

  /** get number of currently active binary variables when this method is called */
  override def getNBinVars: Int = ???

  /** get number of currently active integer variables when this method is called */
  override def getNIntVars: Int = ???

  /** get number of currently active continuous variables when this method is called */
  override def getNContVars: Int = ???

  /** get number of currently active constraints when this method is called */
  override def getNConss: Int = ???

  /** get number of variables in the ILP after presolve; defaults to -1 */
  override def getNPresolvedVars: Int = ???

  /** get number of binary variables in the ILP after presolve; defaults to -1 */
  override def getNPresolvedBinVars: Int = ???

  /** get number of integer variables in the ILP after presolve; defaults to -1 */
  override def getNPresolvedIntVars: Int = ???

  /** get number of continuous variables in the ILP after presolve; defaults to -1 */
  override def getNPresolvedContVars: Int = ???

  /** get number of constraints in the ILP after presolve; defaults to -1 */
  override def getNPresolvedConss: Int = ???

  /** get solution status */
  override def getStatus: IlpStatus = ???

  /** check whether a solution has been found */
  override def hasSolution: Boolean = ???

  /** check whether an optimal solution has been found */
  override def hasOptimalSolution: Boolean = ???

  /** get objective value (primal bound) */
  override def getPrimalbound: Double = ???

  /** get objective value (dual bound) */
  override def getDualbound: Double = ???

  /** get optimality gap */
  override def getGap: Double = ???

  /** get time spent in presolve routine */
  override def getPresolvingTime: Double = ???

  /** get time spent in main solve routine */
  override def getSolvingTime: Double = ???

  /** get total time spent by the ILP solver */
  override def getTotalTime: Double = ???

  /** Adds a basic linear constraints with an optional LHS and an optional RHS */
  override def addConsBasicLinear(name: String, vars: Seq[IllVar], coeffs: Seq[Double], lhsOpt: Option[Double], rhsOpt: Option[Double]): Unit = {
    if (lhsOpt.isDefined) {
      ilpSolver.addGreaterThanConstraint(vars.map(_.ptr).toArray, coeffs.toArray, lhsOpt.get)
    }
    if (rhsOpt.isDefined) {
      ilpSolver.addLessThanConstraint(vars.map(_.ptr).toArray, coeffs.toArray, rhsOpt.get)
    }
  }

  /** Export the generated ILP model to a file, either original or reduced/transformed */
  override def exportModel(ilpFile: String, useOriginal: Boolean): Unit = ???

  /** Solve the ILP model and report the result */
  override def solve(): Unit = ilpSolver.solve()

  /** Reset after calling solve() so that more constraint may be added */
  override def resetSolve(): Unit = ???

  /** Free up ILP solver's data structures */
  override def free(): Unit = ???

  /** Gets the coefficient for the variable in the objective function */
  override def getVarObjCoeff(l: IllVar): Double = ???

  /** get solution values */
  override def getSolVals(vars: Iterable[IllVar]): Iterable[Double] = ???

  /** get one solution value */
  override def getSolVal(variable: IllVar): Double = ilpSolver.getRealValue(variable.ptr)

  /** Sets the lower bound for a variable */
  override def chgVarLb(x: IllVar, bound: Double): Unit = ???

  /** If triggered, imposes a lower bound for a variable; trigger is binary variable */
  override def chgVarLb(x: IllVar, bound: Double, trigger: IllVar): Unit = ???

  /** Sets the upper bound for a variable */
  override def chgVarUb(x: IllVar, bound: Double): Unit = ???

  /** If triggered, imposes a upper bound for a variable; trigger is binary variable */
  override def chgVarUb(x: IllVar, bound: Double, trigger: IllVar): Unit = ???

  /** If triggered, imposes a basic linear constraint on the solver; trigger is binary variable */
  override def addConsBasicLinear(name: String, vars: Seq[IllVar], coeffs: Seq[Double], lhsOpt: Option[Double], rhsOpt: Option[Double], trigger: IllVar): Unit = ???

  /** Adds coefficient to a linear constraint (if it is not zero) */
  override def addCoefLinear(cons: IllCons, x: IllVar, coeff: Double): Unit = ???

  /** Gets the seq of coefficient values in the linear constraint */
  override def getValsLinear(cons: IllCons): Seq[Double] = ???

  /** Adds the constraint sum_i x_i = 1 */
  override def addConsExactlyOne(name: String, X: Seq[IllVar]): Unit = ???

  /** If triggered, imposes a set partitioning constraint, sum_i x_i = 1; trigger is binary var */
  override def addConsExactlyOne(name: String, vars: Seq[IllVar], trigger: IllVar): Unit = ???

  /** Adds the constraint sum_i x_i <= 1 */
  override def addConsAtMostOne(name: String, X: Seq[IllVar]): Unit = ???

  /** If triggered, imposes a set packing constraint, sum_i x_i <= 1; trigger is binary variable */
  override def addConsAtMostOne(name: String, vars: Seq[IllVar], trigger: IllVar): Unit = ???

  /** Adds the constraint sum_i x_i >= 1 */
  override def addConsAtLeastOne(name: String, X: Seq[IllVar]): Unit = ???

  /** If triggered, imposes a set covering constraint, sum_i x_i >= 1; trigger is binary variable */
  override def addConsAtLeastOne(name: String, vars: Seq[IllVar], trigger: IllVar): Unit = ???

  /** Adds coefficient in set partitioning / packing / covering constraint */
  override def addCoefSetppc(cons: IllCons, x: IllVar): Unit = ???

  /** Adds the constraint x <= y + c */
  override def addConsXLeqYPlusC(name: String, x: IllVar, y: IllVar, c: Double): Unit = ???

  /** If triggered, imposes the constraint x <= y + c; trigger is binary variable */
  override def addConsXLeqYPlusC(name: String, x: IllVar, y: IllVar, c: Double, trigger: IllVar): Unit = ???

  /** Adds the constraint x <= y */
  override def addConsXLeqY(name: String, x: IllVar, y: IllVar): Unit = ???

  /** If triggered, imposes the constraint x <= y; trigger is binary variable */
  override def addConsXLeqY(name: String, x: IllVar, y: IllVar, trigger: IllVar): Unit = ???

  /** Adds the constraint x = y + c */
  override def addConsXEqYPlusC(name: String, x: IllVar, y: IllVar, c: Double): Unit = ???

  /** If triggered, imposes the constraint x = y + c; trigger is binary variable */
  override def addConsXEqYPlusC(name: String, x: IllVar, y: IllVar, c: Double, trigger: IllVar): Unit = ???

  /** Adds the constraint x = y */
  override def addConsXEqY(name: String, x: IllVar, y: IllVar): Unit = ???

  /** If triggered, imposes the constraint x = y; trigger is binary variable */
  override def addConsXEqY(name: String, x: IllVar, y: IllVar, trigger: IllVar): Unit = ???

  /** Adds the constraint sum(X) >= k */
  override def addConsAtLeastK(name: String, X: Seq[IllVar], k: Double): Unit = ???

  /** If triggered, imposes the constraint sum(X) >= k; trigger is binary variable */
  override def addConsAtLeastK(name: String, X: Seq[IllVar], k: Double, trigger: IllVar): Unit = ???

  /** Adds the constraint sum(X) <= k */
  override def addConsAtMostK(name: String, X: Seq[IllVar], k: Double): Unit = ???

  /** If triggered, imposes the constraint sum(X) <= k; trigger is binary variable */
  override def addConsAtMostK(name: String, X: Seq[IllVar], k: Double, trigger: IllVar): Unit = ???

  /** Adds the Horn constraint x1 AND x2 AND ... AND xk => y;
    * modeled as: sum(X) - y <= |X| - 1
    */
  override def addConsHorn(name: String, body: Seq[IllVar], head: IllVar): Unit = ???

  /** If triggered, adds the Horn constraint x1 AND x2 AND ... AND xk => y;
    * modeled as: sum(X) - y + trigger <= |X|
    */
  override def addConsHorn(name: String, body: Seq[IllVar], head: IllVar, trigger: IllVar): Unit = ???

  /** Adds the constraint sum(X) >= k * y */
  override def addConsYImpliesAtLeastK(name: String, y: IllVar, X: Seq[IllVar], k: Double): Unit = ???

  /** If triggered, imposes the constraint sum(X) >= k * y; trigger is binary variable */
  override def addConsYImpliesAtLeastK(name: String, y: IllVar, X: Seq[IllVar], k: Double, trigger: IllVar): Unit = ???

  /** Adds the constraint sum(X) >= y */
  override def addConsYImpliesAtLeastOne(name: String, y: IllVar, X: Seq[IllVar]): Unit = ???

  /** If triggered, imposes the constraint sum(X) >= y; trigger is binary variable */
  override def addConsYImpliesAtLeastOne(name: String, y: IllVar, X: Seq[IllVar], trigger: IllVar): Unit = ???

  /** Adds the constraint sum(X) <= k * y */
  override def addConsYImpliesAtMostK(name: String, y: IllVar, X: Seq[IllVar], k: Double): Unit = ???

  /** If triggered, imposes the constraint sum(X) <= k * y; trigger is binary variable */
  override def addConsYImpliesAtMostK(name: String, y: IllVar, X: Seq[IllVar], k: Double, trigger: IllVar): Unit = ???

  /** Adds the constraint sum(X) <= y */
  override def addConsYImpliesAtMostOne(name: String, y: IllVar, X: Seq[IllVar]): Unit = ???

  /** If triggered, imposes the constraint sum(X) <= y; trigger is binary variable */
  override def addConsYImpliesAtMostOne(name: String, y: IllVar, X: Seq[IllVar], trigger: IllVar): Unit = ???

  /** Print result of the call to solve(), along with solution values of vars */
  override def printResult(vars: Seq[IllVar]): Unit = ???

  /** get the number of search nodes explored during branch and bound */
  override def getNNodes: Long = ???

  /** get the number of simplex iterations used when solving LP relaxations */
  override def getNLPIterations: Long = ???

  /** get the maximal depth of nodes explored during branch and bound */
  override def getMaxDepth: Int = ???
}
