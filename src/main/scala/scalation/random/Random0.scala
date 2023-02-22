
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Fri Aug  6 12:35:22 EDT 2021
 *  @see     LICENSE (MIT style license file).
 *  
 *  @title   First Attempt at a Random Number Generator
 */

package scalation
package random

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Random0` class generates random real numbers in the range (0, 1).
 *  It implements, using 32-bit integers (Int's).
 *      x_i = (x_i-1 + 1) % m
 *  @see `Random` for a better random number generator
 *  @param stream  the random number stream index
 */
case class Random0 (stream: Int = 0)
     extends RNG (stream):

    private val M     = 2147483647             // modulus for a popular 32-bit generator (2^31 - 1)
    private val NORM  = 1.0 / M.toDouble       // normalization to (0, 1)

    private var x     = stream                 // set the stream value to its seed stream index here

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the modulus used by this random number generator.
     */
    def getM: Double = M.toDouble

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the next random number as a `Double` in the interval (0, 1).
     *  Compute x_i = (x_i-1 + 1) % m using x = (x + 1) % m
     */
    inline def gen: Double = { x = (x + 1) % M; x * NORM }

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the next stream value as a `Int` in the set {1, 2, ... , m-1}.
     *  Compute x_i = (x_i-1 + 1) % m using x = (x + 1) % m
     */
    inline def igen: Int = {x = (x + 1) % M; x }

end Random0

