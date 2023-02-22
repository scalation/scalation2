
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Mon Feb 11 13:27:08 EST 2013
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Base Methods for Householder Orthogonalization
 */

package scalation
package mathstat

import scala.math.{signum, sqrt}

import MatrixD.{eye, outer}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Householder` object provides methods to compute Householder vectors and
 *  reflector matrices.
 */
object Householder:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute a Householder vector v and its corresponding scalar b,  where
     *  P = I - b * v * v.t is an orthogonal matrix and Px = ||x|| * e_1.
     *  @see Algorithm 5.1.1 in Matrix Computations.
     *  @param x  the vector to create the Householder vector from
     */
    def house (x: VectorD): (VectorD, Double) =
        var b = 0.0                                   // Householder scalar
        val v = x.copy                                // Householder vector (start by copying)
        v(0)  = 1.0                                   // reset first element to 1
        val s = (v dot v) - 1.0                       // sigma
        if ! (s =~ 0.0) then
            val y = x(0)
            val m = sqrt (y*y + s)                    // mu
            val z = if y <= 0 then y - m else -s / (y + m)
            v(0) = z
            b = 2.0*z*z / (s + z*z)
            v /= z
        end if
        (v, b)                                        // return Householder vector & scalar
    end house

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Householder unit vector u, where P = I - b * u * u.t is an
     *  orthogonal matrix.
     *  @see www.math.siu.edu/matlab/tutorial4.pdf
     *  @param x  the vector to create the unit Householder vector from
     */
    def houseV (x: VectorD): VectorD =
        val u = x / x.mag                             // divide x by its magnitude
        u(0) += signum (u(0)) * u.norm
        u /= u.norm                                   // normalize to a unit vector
    end houseV

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Householder reflector matrix h = I - 2*u*u.t.
     *  @see www.math.siu.edu/matlab/tutorial4.pdf
     *  @param x  the vector to create the Householder reflector from
     */
    def houseR (x: VectorD): MatrixD =
        val u = houseV (x)                            // unit Householder vector
        eye (x.dim, x.dim) - outer (u, u*2.0)         // I - 2 * outer product
    end houseR

end Householder


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `HouseholderTest` main function is used to test `Householder` object.
 *  @see www.math.siu.edu/matlab/tutorial4.pdf
 *  > runMain scalation.mathstat.householderTest
 */
@main def householderTest (): Unit =

    import Householder.{house, houseR, houseV}

    val x = VectorD (2.0, 4.0, 6.0)
    val y = VectorD (1.0, 2.0, 3.0, 4.0)
    
    println (s"x          = $x")
    println (s"house (x)  = ${house (x)}")
    println (s"houseV (x) = ${houseV (x)}")

    println (s"y          = $y")
    println (s"house (y)  = ${house (y)}")
    println (s"houseV (y) = ${houseV (y)}")     // answer: (.7690, .2374, .3561, .4749)
    println (s"houseR (y) = ${houseR (y)}")

end householderTest

