
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Wed Oct 24 16:25:29 EDT 2012
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Base Trait for Minimize
 */

package scalation
package optimization

import scalation.mathstat._

/** Type definition:  Tuple of the functional value f(x) and the point/vector x
 */
type FuncVec = (Double, VectorD)

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Return the better solution, the one with smaller functional value.
 *  @param cand  the candidate solution (functional value f and vector x)
 *  @param best  the best solution found so far
 */
inline def better (cand: FuncVec, best: FuncVec): FuncVec =
    if cand._1 < best._1 then cand else best
end better

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Check whether the candidate solution has blown up.
 *  @param cand  the candidate solution (functional value f and vector x)
 */
def blown (cand: FuncVec): Boolean =
    val (fx, x) = cand
    fx.isNaN || fx.isInfinite || x.exists ((z: Double) => z.isNaN || z.isInfinite)
end blown


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Minimize` object defines the hyper-parameters for extending optimizers.
 */
object Minimize:

    /** hyper-parameters for tuning the optimization algorithms - user tuning
     */
    val hp = new HyperParameter
    hp += ("eta", 0.5, 0.5)                                             // initial learning/convergence rate
//  hp += ("eta", 0.01, 0.01)                                           // initial learning/convergence rate
    hp += ("maxEpochs", 100, 100)                                       // maximum number of epochs/iterations
    hp += ("upLimit", 4, 4)                                             // up-limit hyper-parameter for stopping rule
    hp += ("eps", 1E-8, 1E-8)                                           // epilson, value close to zero
    hp += ("beta", 0.9, 0.9)                                            // momentum decay hyper-parameter
    hp += ("beta2", 0.999, 0.999)                                       // second momentum decay hyper-parameter
    hp += ("nu", 0.9, 0.9)                                              // 0 => SGD, 1 => (normalized) SHB

end Minimize


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Minimize` trait sets the pattern for optimization algorithms for solving
 *  Non-Linear Programming (NLP) problems of the form:
 * 
 *  minimize    f(x)
 *
 *  where f is the objective/loss function to be minimized
 */
trait Minimize:

    import Minimize.hp

    protected val eta    = hp("eta").toDouble                           // default initial step size
    protected val MAX_IT = hp("maxEpochs").toInt                        // maximum number of major steps/iterations 
    protected val EPS    = hp("eps").toDouble                           // epsilon, between machine epsilon and its square root
    protected val TOL    = 100.0 * EPS                                  // default tolerance level more relaxed

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve the Non-Linear Programming (NLP) problem by starting at x0 and
     *  iteratively moving down in the search space to a minimal point.
     *  Return the optimal point/vector x and its objective function value.
     *  @param x0  the starting point 
     *  @param α   the current learning rate
     */
    def solve (x0: VectorD, α: Double = eta): FuncVec

end Minimize

