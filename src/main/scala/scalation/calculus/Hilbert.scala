
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sat Oct  8 12:37:32 EDT 2016
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Operators for Functions in Hilbert Spaces   
 *
 *  @see www.jstatsoft.org/article/view/v051i04/v51i04.pdf
 */

// FIX - may rewrite using extension methods

package scalation
package calculus

import scala.math.sqrt

import Integral.∫

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Convert a function from `FunctionS2S` to `Hilbert`, which supports
 *  functional operators.
 *  @param f  the function to turn into a Hilbert function
 *
 */
def functionS2S2Hilbert (f: FunctionS2S) = new Hilbert (f)

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Hilbert` class provides operators to add, subtract, mutiply, divide and
 *  raise functions.  Given two functions, 'f' and 'g', a new function is created.
 *  It also provides methods for computing dot/inner products, norms and
 *  distances for functions defined in Hilbert Space.
 *  On interval [a, b]
 *      Lp-norm (f) = [ ∫f(t)^p dt ]^1/p
 *  @param f  the function to convert into a Hilbert function
 */
case class Hilbert (f: FunctionS2S):

    val f_ = this

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Negate the function 'f' (unary minus), returning a new function.
     */
    def unary_- = (x: Double) => -f(x)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add function 'f' and function 'g', returning a new function.
     *  @param g  the other function
     */
    def + (g: FunctionS2S) = (x: Double) => f(x) + g(x)         // function of x
    def + (g: Double)      = (x: Double) => f(x) + g            // constant function 

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From function 'f' subtract function 'g', returning a new function.
     *  @param g  the other function
     */
    def - (g: FunctionS2S) = (x: Double) => f(x) - g(x)
    def - (g: Double)      = (x: Double) => f(x) - g

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply function 'f' by function 'g', returning a new function.
     *  @param g  the other function
     */
    def * (g: FunctionS2S) = (x: Double) => f(x) * g(x)
    def * (g: Double)      = (x: Double) => f(x) * g

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide function 'f' by function 'g', returning a new function.
     *  @param g  the other function
     */
    def / (g: FunctionS2S) = (x: Double) => f(x) / g(x)
    def / (g: Double)      = (x: Double) => f(x) / g

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Raise function 'f' to the 'p'th power, returning a new function.
     *  @param p  the integer-valued power/exponent
     */
    def ~^ (p: Int) = (x: Double) => f(x) ~^ p

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Raise function 'f' to the 'p'th power, returning a new function.
     *  @param p  the power/exponent
     */
    def ~^ (p: Double) = (x: Double) => f(x) ~^ p

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the dot/inner product of functions 'f' and 'g'.
     *  @param g  the other function
     *  @param a  the start of the interval
     *  @param b  the end of the interval
     */
    infix def dot (g: FunctionS2S, a: Double = 0, b: Double = 1): Double = ∫ ((a, b), f_ * g)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the L2 norm squared of function 'f', returning a new function.
     *  @param a   the start of the interval
     *  @param b   the end of the interval
     */
    def normSq (a: Double = 0, b: Double = 1): Double = ∫ ((a, b), f_ ~^ 2)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the L2 norm of function 'f'.
     *  @param a  the start of the interval
     *  @param b  the end of the interval
     */
    def norm (a: Double = 0, b: Double = 1): Double = sqrt (normSq (a, b))

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Lp norm squared of function 'f'.
     *  @param p  the level, e.g., 1, 2, ...
     *  @param a  the start of the interval
     *  @param b  the end of the interval
     */
    def normSq_p (p: Int, a: Double = 0, b: Double = 1): Double = ∫ ((a, b), f_ ~^ p)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Lp norm of function 'f'.
     *  @param p  the level, e.g., 1, 2, ...
     *  @param a  the start of the interval
     *  @param b  the end of the interval
     */
    def norm_p (p: Int, a: Double = 0, b: Double = 1): Double = normSq_p (p, a, b) ~^ (1.0 / p.toDouble)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the distance in L2 space between function 'f' and function 'g'.
     *  @param g  the other function
     *  @param a  the start of the interval
     *  @param b  the end of the interval
     */
    def dist (g: FunctionS2S, a: Double = 0, b: Double = 1): Double = Hilbert (f_ - g).norm (a, b)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the distance in Lp space between function 'f' and function 'g'.
     *  @param g  the other function
     *  @param p  the level, e.g., 1, 2, ...
     *  @param a  the start of the interval
     *  @param b  the end of the interval
     */
    def dist_p (g: FunctionS2S, p: Int, a: Double = 0, b: Double = 1): Double = Hilbert (f_ - g).norm_p (p, a, b)

end Hilbert


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `hilbertTest` main function is used to test the `Hilbert` class.
 *  > runMain scalation.calculus.hilbertTest
 */
@main def hilbertTest (): Unit =

    banner ("functions: f(x) = 2t, g(x) = t^2")
    val f = Hilbert ((t: Double) => 2.0 * t)            // Hilbert function definition
    def g (t: Double)   = t * t                         // method defintion
                                              
    banner ("functional operators")
    val h = Hilbert (f * 2) + g                         // define a new function h
    println ("f.f(1)      = " + f.f(1))
    println ("g(1)        = " + g(1))
    println ("(-f)(1)     = " + (-f)(1))
    println ("(f + g)(1)  = " + (f + g)(1))
    println ("(f - g)(1)  = " + (f - g)(1))
    println ("(f * g)(1)  = " + (f * g)(1))
    println ("(f / g)(1)  = " + (f / g)(1))
    println ("(f ~^ 2)(1) = " + (f ~^ 2)(1))
    println ("h(1)        = " + h(1))

    banner ("dot product")
    println ("f dot f = " + (f dot f.f))
    println ("f dot g = " + (f dot g))

    banner ("norm (f)")
    println ("L2 norm = " + f.norm ())
    println ("L1 norm = " + f.norm_p (1))
    println ("L2 norm = " + f.norm_p (2))
    println ("L3 norm = " + f.norm_p (3))

    banner ("dist (f, g)")
    println ("L2 distance = " + f.dist (g))
    println ("L1 distance = " + f.dist_p (g, 1))
    println ("L2 distance = " + f.dist_p (g, 2))
    println ("L3 distance = " + f.dist_p (g, 3))

end hilbertTest

