
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sat Jan  1 13:54:44 EST 2022
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    A Simple Random Variate Generator that Uses `java.util.Random`
 *
 *  @see     `scalation.random` for better random number and variate generators
 */

package scalation

import java.util.Random

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SimpleUniform` case class for generating random numbers in the interval [a, b].
 *  @see `scalation.random.Uniform`
 *  @parma a  the lower bound
 *  @parma b  the uppper bound
 */
case class SimpleUniform (a: Double, b: Double):

    private val rng = new Random ()

    def gen: Double = a + (b - a) * rng.nextDouble

end SimpleUniform

