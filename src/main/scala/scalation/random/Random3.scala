
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Wed Sep 30 18:41:26 EDT 2009
 *  @see     LICENSE (MIT style license file).
 *  
 *  @note    LCG (Linear Congruential Generator) using 64-bit Long's
 */

package scalation
package random

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Random3` class generates random real numbers in the range (0, 1).
 *  It implements, using 64-bit integers (Int's), the 'MINSTD' generator, which
 *  is a multiplicative Linear Congruential Generator (LCG).
 *  These generators were commonly used in the last century.
 *
 *      x_i = a x_i-1 % m
 * 
 *  @see http://random.mat.sbg.ac.at/results/karl/server/node4.html#SECTION00042000000000000000
 *  In case a better generator is needed, a Multiple Recursive Generator (MRG)
 *  or Composite Multiple Recursive Generator (CMRG) should be used.
 *  @see `Random`
 *  @see http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.34.1024
 *  @param stream  the random number stream index
 */
case class Random3 (stream: Int = 0)
     extends RNG (stream):

//  private val A     = 48271L                           // alternative multiplier for a popular 32-bit generator
    private val A     = 16807L                           // multiplier for a popular 32-bit generator (7^5)
    private val M     = 2147483647L                      // modulus for a popular 32-bit generator (2^31 - 1)
    private val NORM  = 1.0 / M.toDouble                 // normalization to (0, 1)
    private val strm  = stream % RandomSeeds3.N_STREAMS  // can't go beyond stream limit
    private var x     = RandomSeeds3.seeds(strm).toLong  // set the stream value to its seed

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the modulus used by this random number generator.
     */
    def getM: Double = M.toDouble

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the next random number as a `Double` in the interval (0, 1).
     *  Compute x_i = a x_i-1 % m using x = a * x % m
     */
    inline def gen: Double = { x = A * x % M; x * NORM }

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the next stream value as a `Int` in the set {1, 2, ... , m-1}.
     *  Compute x_i = a x_i-1 % m using x = a * x % m
     */
    inline def igen: Int =
        x = A * x % M
        x.toInt
    end igen

end Random3

