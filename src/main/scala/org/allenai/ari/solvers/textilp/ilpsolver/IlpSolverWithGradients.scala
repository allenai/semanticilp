package org.allenai.ari.solvers.textilp.ilpsolver

import scala.collection.mutable

/** A linear term, represented symbolically. E.g., 3 w1 + w2 is represented as
  * Seq((w1, 3), (w2, 1)).
  *
  * @param terms an ordered sequence of term-coefficient pairs
  * @tparam T type for a term, such as Parameter, IlpVar, String, etc.
  */
case class LinearTerm[T](terms: Seq[(T, Double)]) {
  /** A map version of terms, for quick access to individual coefficients. */
  val termMap = terms.toMap
}

/** The objective function coefficient of a variable in the ILP, represented in a symbolic form.
  * E.g., if the objective function is (3 w1 + w2) x + w1 y, then the coefficient of x is 3 w1 + w2,
  * represented in this case class as Seq((w1, 3), (w2, 1)). One caveat is that if the coefficient
  * is non-linear, e.g., (3 w1 w3 + w2), then it is represented using a linear approximation that
  * is locally (but not globally) accurate, namely, Seq((w1, 3 w3_curr), (w2, 1), (w3, 3 w1_curr))
  * where w1_curr and w3_curr denote the current real values of w1 and w3, resp. The local accuracy
  * is sufficient to, e.g., compute the exact gradient of each variable.
  *
  * @param terms an ordered sequence of parameter-coefficient pairs
  * @tparam P type for the parameter, e.g., Parameter or String (for parameter name)
  */
class SymbolicVarCoeff[P](terms: Seq[(P, Double)]) extends LinearTerm(terms)

/** The coefficient of a parameter represented in a symbolic form, when all terms involving that
  * parameter in the objective function of the ILP are aggregated together. E.g., if the objective
  * function is (3 w1 + w2) x + w1 y, then the coefficient of w1 is 3 x + y, represented in this
  * case class as Seq((x, 3), (y, 1)). One caveat is that if some variables coefficients are
  * non-linear, e.g., (3 w1 w3 + w2) x + w1 y, then the corresponding parameter coefficients are
  * represented using the same linear approximation as in VariableCoeff that is locally (but not
  * globally) accurate. The local accuracy is sufficient to, e.g., compute the exact gradient of
  * each parameter in an ILP solution.
  *
  * @param terms an ordered sequence of parameter-coefficient pairs
  * @tparam V type for the variable, e.g., IlpVar or String (for variable name)
  */
class SymbolicParamCoeff[V <: IlpVar](terms: Seq[(V, Double)]) extends LinearTerm(terms)

/** A trait for an ILP solver that implements methods related to computing gradients of the
  * objective function w.r.t. a set of parameters. The gradients show the direction of the biggest
  * changes in the objective function, which can be used for training a system using ILP as
  * inference engine.
  *
  * @tparam V abstract type for ILP variables
  * @tparam C abstract type for ILP constraints
  */
trait IlpSolverWithGradients[V <: IlpVar, C <: IlpCons] extends IlpSolver[V, C] {

  /** A map connecting parameters to their symbolic coefficient in the ILP objection function. */
  private val symbolicParamCoeffsMap = new mutable.HashMap[String, SymbolicParamCoeff[V]]

  /** A map of parameters to the value of their corresponding variables in the final solution. */
  def computeAllParamCoeffsInIlpSolution(): Map[String, Double] = {
    symbolicParamCoeffsMap.keys.map(key => key -> computeParamCoeffInIlpSolution(key)).toMap
  }

  /** The cumulative value of the variables related to a given parameter, in the current ILP
    * solution as captured by getSolVal().
    */
  private def computeParamCoeffInIlpSolution(parameterName: String): Double = {
    val symbolicParamCoeff = symbolicParamCoeffsMap.getOrElse(
      parameterName,
      throw new Exception(s"Parameter $parameterName not found in the objective! " +
        "This might be because you have not activated calculation of gradients in applications.conf")
    )
    // Note: the solver may have eliminated some variables, for which getSolVal() won't work
    // TODO: double-check that this filtering makes sense!
    symbolicParamCoeff.terms.filter(getAllVars.contains).map {
      case (v, coeff) => getSolVal(v) * coeff
    }.sum
  }

  /** Store the name and symbolic coefficient of a variable; return the variable back. */
  private def captureParamWeight(x: V, symbolicVarCoeff: SymbolicVarCoeff[String]): V = {
    symbolicVarCoeff.terms.foreach {
      case (paramName, coeff) =>
        val updatedTerms = symbolicParamCoeffsMap.get(paramName) match {
          case Some(symbolicParamCoeff) => symbolicParamCoeff.terms :+ ((x, coeff))
          case None => Seq((x, coeff))
        }
        symbolicParamCoeffsMap.update(paramName, new SymbolicParamCoeff(updatedTerms))
    }
    x // return the variable back
  }

  /** create a binary variable, and add relevant parameters */
  def createBinaryVarWithGradient(
    name: String,
    objCoeff: Double,
    symbolicObjCoeff: SymbolicVarCoeff[String]
  ): V = {
    captureParamWeight(createBinaryVar(name, objCoeff), symbolicObjCoeff)
  }

  /** create a relaxed binary variable, that is, a continuous various with domain [0,1], and add
    * relevant parameters
    */
  def createRelaxedBinaryVarWithGradient(
    name: String,
    objCoeff: Double,
    symbolicObjCoeff: SymbolicVarCoeff[String]
  ): V = {
    captureParamWeight(createRelaxedBinaryVar(name, objCoeff), symbolicObjCoeff)
  }

  /** create an integer variable, and add relevant parameter */
  def createIntegerVarWithGradient(
    name: String,
    lb: Double,
    ub: Double,
    objCoeff: Double,
    symbolicObjCoeff: SymbolicVarCoeff[String]
  ): V = {
    captureParamWeight(createIntegerVar(name, lb, ub, objCoeff), symbolicObjCoeff)
  }

  /** create a continuous variable and add relevant parameters */
  def createContinuousVarWithGradient(
    name: String,
    lb: Double,
    ub: Double,
    objCoeff: Double,
    symbolicObjCoeff: SymbolicVarCoeff[String]
  ): V = {
    captureParamWeight(createContinuousVar(name, lb, ub, objCoeff), symbolicObjCoeff)
  }

}
