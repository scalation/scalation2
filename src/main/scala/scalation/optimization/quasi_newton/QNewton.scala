
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira, John Miller
 *  @version 2.0
 *  @note    Wed Feb 14 15:52:33 EST 2024
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Method for Quasi-Newton Optimizers to Compute the Sherman–Morrison Formula
 */

package scalation
package optimization
package quasi_newton

import scalation.mathstat.{MatrixD, VectorD}

import MatrixD.⊗                                  // outer product

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `QNewton` object provides methods useful for Quasi-Newton optimizers.
 */
object QNewton:

    private val EPS = Minimize.hp("eps").toDouble

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the change to the approximate Hessian inverse (aHi) matrix using
     *  the Sherman–Morrison formula.
     *  @see https://en.wikipedia.org/wiki/Broyden%E2%80%93Fletcher%E2%80%93Goldfarb%E2%80%93Shanno_algorithm
     *  @see https://mdav.ece.gatech.edu/ece-6270-spring2021/notes/09-bfgs.pdf
     *
     *  @param aHi  the current value of the approximate Hessian inverse (aHi)
     *  @param s    the step vector (next point - current point)
     *  @param y    the difference in the gradients (next - current)
     */
    def aHi_inc (aHi: MatrixD, s: VectorD, y: VectorD): MatrixD =
        // val sy = maxmag(s dot y, EPS)
        val sy = math.max (s dot y, EPS)
        val ay = aHi * y
        (⊗(s, s) * (sy + (y dot ay))) / sy~^2 - (⊗(ay, s) + ⊗(s, ay)) / sy
    end aHi_inc

end QNewton

