
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @note    Fri Sep 15 14:34:10 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *------------------------------------------------------------------------------
 *  Data resulting from a line search step in the line search logic used by the
 *  native implementation of the Limited memory Broyden–Fletcher–Goldfarb–Shanno
 *  (BFGS) for unconstrained optimization (L-BFGS) algorithm.
 */

// Package definition.
package scalation
package optimization
package quasi_newton

// Project imports.
import scalation.mathstat.VectorD

// Case class.
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `LBFGSLineSearchStep` class stores the results of a single line search
 *  step performed by a line search algorithm in the native implementation of
 *  the L-BFGS algorithm. Every line search algorithm used by the native L-BFGS
 *  implementation should return an instance of this case class upon achieving a
 *  successful line search step.
 *
 *  @param x                    [[VectorD]] representing the values of the
 *                              variables obtained after performing the line
 *                              search step.
 *  @param g                    [[VectorD]] representing the gradient vector
 *                              obtained after performing the line search step.
 *  @param fx                   The objective function value obtained after
 *                              performing the line search step.
 *  @param step                 The step selected by the line search algorithm.
 *  @param numberOfIterations   The number of iterations needed to determine the
 *                              line search step performed.
 */
case class LBFGSLineSearchStep(
    x: VectorD,
    g: VectorD,
    fx: Double,
    step: Double,                        
    numberOfIterations: Int                            
)
