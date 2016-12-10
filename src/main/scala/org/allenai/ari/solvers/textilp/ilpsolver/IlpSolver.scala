package org.allenai.ari.solvers.textilp.ilpsolver

import org.allenai.common.{ Enum, EnumCompanion }

/** Various relevant status values after an ILP is solved */
sealed abstract class IlpStatus(override val id: String) extends Enum[IlpStatus]
object IlpStatus extends EnumCompanion[IlpStatus] {
  case object IlpStatusUnknown extends IlpStatus("Unknown")
  case object IlpStatusOptimal extends IlpStatus("Optimal")
  case object IlpStatusFeasible extends IlpStatus("Feasible")
  case object IlpStatusInfeasible extends IlpStatus("Infeasible")
  register(IlpStatusUnknown, IlpStatusOptimal, IlpStatusFeasible, IlpStatusInfeasible)
}

/** A trait for ILP variable type. */
trait IlpVar {}

/** A trait for ILP constraint type. */
trait IlpCons {}

/** A trait for an ILP solver.
  * @tparam V abstract type for ILP variables
  * @tparam C abstract type for ILP constrants
  */
trait IlpSolver[V <: IlpVar, C <: IlpCons] {
  /** set objective function as minimization */
  def setAsMinimization(): Unit

  /** set objective function as maximization */
  def setAsMaximization(): Unit

  /** create a binary variable */
  def createBinaryVar(name: String, obj: Double): V

  /** create a relaxed binary variable, that is, a continuous various with domain [0,1] */
  def createRelaxedBinaryVar(name: String, obj: Double): V

  /** create an integer variable */
  def createIntegerVar(name: String, lb: Double, ub: Double, objCoeff: Double): V

  /** create a continuous variable */
  def createContinuousVar(name: String, lb: Double, ub: Double, objCoeff: Double): V

  /** add variable to the environment */
  def addVar(x: V): Unit

  /** add constraint to the environment */
  def addCons(c: C): Unit

  /** get the name of a variable */
  def varGetName(l: V): String

  /** gets all the variables */
  def getAllVars: Array[V]

  /** gets all the variables that are active in the final solution */
  def getAllActiveVars: Array[V]

  /** get number of variables in the original ILP */
  def getNOrigVars: Int

  /** get number of binary variables in the original ILP */
  def getNOrigBinVars: Int

  /** get number of integer variables in the original ILP */
  def getNOrigIntVars: Int

  /** get number of continuous variables in the original ILP */
  def getNOrigContVars: Int

  /** get number of constraints in the original ILP */
  def getNOrigConss: Int

  /** get number of currently active variables when this method is called */
  def getNVars: Int

  /** get number of currently active binary variables when this method is called */
  def getNBinVars: Int

  /** get number of currently active integer variables when this method is called */
  def getNIntVars: Int

  /** get number of currently active continuous variables when this method is called */
  def getNContVars: Int

  /** get number of currently active constraints when this method is called */
  def getNConss: Int

  /** get number of variables in the ILP after presolve; defaults to -1 */
  def getNPresolvedVars: Int

  /** get number of binary variables in the ILP after presolve; defaults to -1 */
  def getNPresolvedBinVars: Int

  /** get number of integer variables in the ILP after presolve; defaults to -1 */
  def getNPresolvedIntVars: Int

  /** get number of continuous variables in the ILP after presolve; defaults to -1 */
  def getNPresolvedContVars: Int

  /** get number of constraints in the ILP after presolve; defaults to -1 */
  def getNPresolvedConss: Int

  /** get solution status */
  def getStatus: IlpStatus

  /** Gets the coefficient for the variable in the objective function */
  def getVarObjCoeff(l: V): Double

  /** check whether a solution has been found */
  def hasSolution: Boolean

  /** check whether an optimal solution has been found */
  def hasOptimalSolution: Boolean

  /** get objective value (primal bound) */
  def getPrimalbound: Double

  /** get objective value (dual bound) */
  def getDualbound: Double

  /** get optimality gap */
  def getGap: Double

  /** get solution values */
  def getSolVals(vars: Iterable[V]): Iterable[Double]

  /** get one solution value */
  def getSolVal(variable: V): Double

  /** get time spent in presolve routine */
  def getPresolvingTime: Double

  /** get time spent in main solve routine */
  def getSolvingTime: Double

  /** get total time spent by the ILP solver */
  def getTotalTime: Double

  /** get the number of search nodes explored during branch and bound */
  def getNNodes: Long

  /** get the number of simplex iterations used when solving LP relaxations */
  def getNLPIterations: Long

  /** get the maximal depth of nodes explored during branch and bound */
  def getMaxDepth: Int

  /** Sets the lower bound for a variable */
  def chgVarLb(x: V, bound: Double): Unit

  /** If triggered, imposes a lower bound for a variable; trigger is binary variable */
  def chgVarLb(x: V, bound: Double, trigger: V): Unit

  /** Sets the upper bound for a variable */
  def chgVarUb(x: V, bound: Double): Unit

  /** If triggered, imposes a upper bound for a variable; trigger is binary variable */
  def chgVarUb(x: V, bound: Double, trigger: V): Unit

  /** Adds a basic linear constraints with an optional LHS and an optional RHS */
  def addConsBasicLinear(name: String, vars: Seq[V], coeffs: Seq[Double], lhsOpt: Option[Double],
    rhsOpt: Option[Double]): Unit

  /** If triggered, imposes a basic linear constraint on the solver; trigger is binary variable */
  def addConsBasicLinear(name: String, vars: Seq[V], coeffs: Seq[Double], lhsOpt: Option[Double],
    rhsOpt: Option[Double], trigger: V): Unit

  /** Adds coefficient to a linear constraint (if it is not zero) */
  def addCoefLinear(cons: C, x: V, coeff: Double): Unit

  /** Gets the seq of coefficient values in the linear constraint */
  def getValsLinear(cons: C): Seq[Double]

  /** Adds the constraint sum_i x_i = 1 */
  def addConsExactlyOne(name: String, X: Seq[V]): Unit

  /** If triggered, imposes a set partitioning constraint, sum_i x_i = 1; trigger is binary var */
  def addConsExactlyOne(name: String, vars: Seq[V], trigger: V): Unit

  /** Adds the constraint sum_i x_i <= 1 */
  def addConsAtMostOne(name: String, X: Seq[V]): Unit

  /** If triggered, imposes a set packing constraint, sum_i x_i <= 1; trigger is binary variable */
  def addConsAtMostOne(name: String, vars: Seq[V], trigger: V): Unit

  /** Adds the constraint sum_i x_i >= 1 */
  def addConsAtLeastOne(name: String, X: Seq[V]): Unit

  /** If triggered, imposes a set covering constraint, sum_i x_i >= 1; trigger is binary variable */
  def addConsAtLeastOne(name: String, vars: Seq[V], trigger: V): Unit

  /** Adds coefficient in set partitioning / packing / covering constraint */
  def addCoefSetppc(cons: C, x: V): Unit

  /** Adds the constraint x <= y + c */
  def addConsXLeqYPlusC(name: String, x: V, y: V, c: Double): Unit

  /** If triggered, imposes the constraint x <= y + c; trigger is binary variable */
  def addConsXLeqYPlusC(name: String, x: V, y: V, c: Double, trigger: V): Unit

  /** Adds the constraint x <= y */
  def addConsXLeqY(name: String, x: V, y: V): Unit

  /** If triggered, imposes the constraint x <= y; trigger is binary variable */
  def addConsXLeqY(name: String, x: V, y: V, trigger: V): Unit

  /** Adds the constraint x = y + c */
  def addConsXEqYPlusC(name: String, x: V, y: V, c: Double): Unit

  /** If triggered, imposes the constraint x = y + c; trigger is binary variable */
  def addConsXEqYPlusC(name: String, x: V, y: V, c: Double, trigger: V): Unit

  /** Adds the constraint x = y */
  def addConsXEqY(name: String, x: V, y: V): Unit

  /** If triggered, imposes the constraint x = y; trigger is binary variable */
  def addConsXEqY(name: String, x: V, y: V, trigger: V): Unit

  /** Adds the constraint sum(X) >= k */
  def addConsAtLeastK(name: String, X: Seq[V], k: Double): Unit

  /** If triggered, imposes the constraint sum(X) >= k; trigger is binary variable */
  def addConsAtLeastK(name: String, X: Seq[V], k: Double, trigger: V): Unit

  /** Adds the constraint sum(X) <= k */
  def addConsAtMostK(name: String, X: Seq[V], k: Double): Unit

  /** If triggered, imposes the constraint sum(X) <= k; trigger is binary variable */
  def addConsAtMostK(name: String, X: Seq[V], k: Double, trigger: V): Unit

  /** Adds the Horn constraint x1 AND x2 AND ... AND xk => y;
    * modeled as: sum(X) - y <= |X| - 1
    */
  def addConsHorn(name: String, body: Seq[V], head: V): Unit

  /** If triggered, adds the Horn constraint x1 AND x2 AND ... AND xk => y;
    * modeled as: sum(X) - y + trigger <= |X|
    */
  def addConsHorn(name: String, body: Seq[V], head: V, trigger: V): Unit

  /** Adds the constraint sum(X) >= k * y */
  def addConsYImpliesAtLeastK(name: String, y: V, X: Seq[V], k: Double): Unit

  /** If triggered, imposes the constraint sum(X) >= k * y; trigger is binary variable */
  def addConsYImpliesAtLeastK(name: String, y: V, X: Seq[V], k: Double, trigger: V): Unit

  /** Adds the constraint sum(X) >= y */
  def addConsYImpliesAtLeastOne(name: String, y: V, X: Seq[V]): Unit

  /** If triggered, imposes the constraint sum(X) >= y; trigger is binary variable */
  def addConsYImpliesAtLeastOne(name: String, y: V, X: Seq[V], trigger: V): Unit

  /** Adds the constraint sum(X) <= k * y */
  def addConsYImpliesAtMostK(name: String, y: V, X: Seq[V], k: Double): Unit

  /** If triggered, imposes the constraint sum(X) <= k * y; trigger is binary variable */
  def addConsYImpliesAtMostK(name: String, y: V, X: Seq[V], k: Double, trigger: V): Unit

  /** Adds the constraint sum(X) <= y */
  def addConsYImpliesAtMostOne(name: String, y: V, X: Seq[V]): Unit

  /** If triggered, imposes the constraint sum(X) <= y; trigger is binary variable */
  def addConsYImpliesAtMostOne(name: String, y: V, X: Seq[V], trigger: V): Unit

  /** Export the generated ILP model to a file, either original or reduced/transformed */
  def exportModel(ilpFile: String, useOriginal: Boolean): Unit

  /** Solve the ILP model and report the result */
  def solve(): Unit

  /** Reset after calling solve() so that more constraint may be added */
  def resetSolve(): Unit

  /** Print result of the call to solve(), along with solution values of vars */
  def printResult(vars: Seq[V]): Unit

  /** Free up ILP solver's data structures */
  def free(): Unit
}
