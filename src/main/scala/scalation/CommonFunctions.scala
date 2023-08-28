
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sat Jan  1 13:54:44 EST 2022
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Function Type FunctionS2S and Common Functions of that Type
 *
 *  @see https://dotty.epfl.ch/api/scala/math.html
 *  @see https://en.wikipedia.org/wiki/Trigonometric_functions
 *
 *  Many common functions are supplied by the `scala.math` package
 */

package scalation

import scala.math.{cos, log, pow, sin, tan}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/*  Top-level definitions of common scalar functions.
 *  @see Util.scala for top-level definitions related to basic utilities.
 *  @see ValueType.scala for top-level definitions related to basic datatypes.
 */

/** Type definition for functions mapping scalar `Double` to scalar `Double`
 */
type FunctionS2S = Double => Double

/** The value of log 2
 */
val log2 = log (2.0)

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The identity function of type `FunctionS2S`.
 *  Its inverse function is itself.
 *  @param x  the value to return
 */
def id (x: Double): Double = x

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The reciprocal function of type `FunctionS2S`.
 *  Its inverse function is itself.
 *  @param x  the value to take the reciprocal of
 */
def recip (x: Double): Double = 1.0 / x

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The square function of type `FunctionS2S`.
 *  Its inverse function is sqrt.
 *  @param x  the value to square
 */
def sq (x: Double): Double = x * x

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The cube function of type `FunctionS2S`.
 *  Its inverse function is cbrt.
 *  @param x  the value to cube
 */
def cb (x: Double): Double = x * x * x

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The secant function of type `FunctionS2S` (function missing from `scala.math`).
 *  Its inverse function is asec.
 *  @param x  the angle in radians
 */
def sec (x: Double): Double = 1.0 / cos (x)

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The cosecant function of type `FunctionS2S` (function missing from `scala.math`).
 *  Its inverse function is acsc.
 *  @param x  the angle in radians
 */
def csc (x: Double): Double = 1.0 / sin (x)

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The cotangent function of type `FunctionS2S` (function missing from `scala.math`).
 *  Its inverse function is atan.
 *  @param x  the angle in radians
 */
def cot (x: Double): Double = 1.0 / tan (x)

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The log base 2 function of type `FunctionS2S`.
 *  Its inverse function is pow2.
 *  @param x  the value whose log is sought
 */
def log2 (x: Double): Double = log (x) / log2

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The power base 2 function of type `FunctionS2S`.
 *  Its inverse function is log2.
 *  @param x  the value of the exponent 2^x
 */
def pow2 (x: Double): Double = pow (2.0, x)

// Related Functions:

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The log base b function (note log is the natural log).
 *  Its inverse function is pow.
 *  @param b  the base of the logarithm
 *  @param x  the value whose log is sought
 */
def logb (b: Double, x: Double): Double = log (x) / log (b)

