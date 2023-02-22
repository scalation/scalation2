
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sun May 26 15:07:54 EDT 2013
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Base Trait for Linear Optimizers
 */

package scalation
package optimization
package linear_opt

import scalation.mathstat._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MinimizerLP` trait sets the pattern for optimization algorithms for solving
 *  Linear Programming (LP) problems of the form:
 * 
 *  minimize    c x
 *  subject to  a x <= b, x >= 0
 *
 *  where a is the constraint matrix
 *        b is the limit/RHS vector
 *        c is the cost vector
 *
 *  Classes mixing in this trait must implement an objective function objF
 *  an iterative method (solve) that searches for improved solutions x-vectors
 *  with lower objective function values.
 */
trait MinimizerLP:

    private   val flaw    = flawf ("MinimizerLP")    // flaw function
    protected val EPSILON = 1E-9                     // number close to zero
    protected val checker: CheckLP                   // used to check LP solution

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The objective function, e.g., c x. 
     *  @param x  the coordinate values of the current point
     */
    def objF (x: VectorD): Double

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Run the simplex algorithm starting from an initial BFS and iteratively
     *  find a non-basic variable to replace a variable in the current basis
     *  so long as the objective function improves.  Return the optimal solution
     *  vector.
     */
    def solve (): VectorD

   //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether the current solution is correct.
     *  @param x  the primal solution vector x
     *  @param y  the dual solution vector y
     *  @param f  the minimum value of the objective function
     */
    def check (x: VectorD, y: VectorD, f: Double): Boolean =
        val correct = checker.isCorrect (x, y, f)
        if ! correct then flaw ("check", "the LP solution is NOT correct")
        correct
    end check

end MinimizerLP

