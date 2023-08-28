
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sat Mar 27 14:40:57 EDT 2021
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Model Part: Convolutional Filter (one dimensional)
 *
 *  @see leonardoaraujosantos.gitbook.io/artificial-inteligence/machine_learning/deep_learning/convolution
 *  @see e2eml.school/convolution_one_d.html
 */

package scalation
package modeling
package neuralnet

import scala.math.min

import scalation.mathstat._
import scalation.random.RandomVecD

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `CoFilter_1D` class provides a convolution filter (cofilter) for
 *  taking a weighted average over a window of an input vector.
 *  @param width  the width of the cofilter
 */
class CoFilter_1D (width: Int = 5):

    private val rvg = RandomVecD (width, 2.0)                                 // random vector genertor
    private var vec = rvg.gen                                                 // the filter's vector

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Update the parameters, i.e., the filter's vector.
     *  @param vec  the new vector parameters
     */
    def update (vec2: VectorD): Unit = vec = vec2

end CoFilter_1D


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `CoFilter_1D` object provides the convolution and pooling operators.
 */
object CoFilter_1D:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the 'full' convolution of cofilter c and input vector x.
     *  @param c  the cofilter vector of coefficient
     *  @param x  the input/data vector
     */
    def convf (c: VectorD, x: VectorD): VectorD =
        val y = new VectorD (c.dim + x.dim - 1)
        for k <- y.indices do
            var sum = 0.0
            for j <- 0 until min (k+1, c.dim) do
                if k - j < x.dim then sum += c(j) * x(k - j)
            end for
            y(k) = sum
        end for
        y
    end convf

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the 'same' convolution of cofilter c and input vector x.
     *  Same means that the size of the result is the same as the input.
     *  @param c  the cofilter vector of coefficient
     *  @param x  the input/data vector
     */
    def convs (c: VectorD, x: VectorD): VectorD =
        val y   = new VectorD (x.dim)
        val off = c.dim / 2
        for k <- off until y.dim + off do
            var sum = 0.0
            for j <- 0 until min (k+1, c.dim) do
                if k - j < x.dim then sum += c(j) * x(k - j)
            end for
            y(k-off) = sum
        end for
        y
    end convs

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the 'valid' (no padding) convolution of cofilter c and input vector x.
     *  Caveat: does not include reversal.
     *  @param c  the cofilter vector of coefficient
     *  @param x  the input/data vector
     */
    def conv (c: VectorD, x: VectorD): VectorD =
        val y = new VectorD (x.dim - c.dim + 1)
        for k <- y.indices do y(k) = x(k until k + c.dim) dot c
        y
    end conv

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the convolution over all data instances.
     *  @param c  the cofilter vector of coefficient
     *  @param x  the input/data matrix
     */
    def conv (c: VectorD, x: MatrixD): MatrixD =
        MatrixD (for i <- x.indices yield conv (c, x(i)))
    end conv

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the max-pooling results over all pooling windows.
     *  @param x  the input/data vector
     *  @param s  the the size of the pooling window
     */
    def pool (x: VectorD, s: Int = 2): VectorD =
        val p = new VectorD (x.dim / s)
        for j <- p.indices do
            val jj = s * j
            p(j)   = x(jj until jj+s).max
        end for
        p
    end pool

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the pooling results over all data instances.
     *  @param x  the input/data matrix
     *  @param s  the the size of the pooling window
     */
    def pool (x: MatrixD, s: Int): MatrixD =
        MatrixD (for i <- x.indices yield pool (x(i), s))
    end pool

end CoFilter_1D

import CoFilter_1D._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `coFilter_1DTest` main function is used to test the `CoFilter_1D` class.
 *  Test using the simple example from CNN_1D section of the ScalaTion textbook.
 *  > runMain scalation.modeling.neuralnet.coFilter_1DTest
 */
@main def coFilter_1DTest (): Unit =

    val x = MatrixD ((2, 5), 1, 2, 3, 4, 5,
                             6, 7, 8, 9, 10)
    val c = VectorD (0.5, 1, 0.5)
    val z = conv (c, x)
    val p = pool (z, 3)

    println (s"input       x = $x")
    println (s"filter      c = $c")
    println (s"feature map z = $z")
    println (s"pooled      p = $p")

end coFilter_1DTest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `coFilter_1DTest2` main function is used to test the `CoFilter_1D` class's
 *  convolutional operator.
 *  > runMain scalation.modeling.neuralnet.coFilter_1DTest2
 */
@main def coFilter_1DTest2 (): Unit =

    val c = VectorD (1, 2, 3, 4, 5)
    val x = VectorD (1, 2, 3, 4, 5, 6, 7)

    banner (s"c convolution x")
    println (s"c = $c")
    println (s"x = $x")

    banner ("Full Convolution: convf (c, x)")
    println (s"y = ${convf (c, x)}")
    banner ("Same Convolution: convs (c, x)")
    println (s"y = ${convs (c, x)}")
    banner ("Valid Convolution: conv (c.reverse, x)")
    println (s"y = ${conv (c.reverse, x)}")

    banner ("Valid Convolution: conv (c, x)")   // without expected reversal
    println (s"y = ${conv (c, x)}")

end coFilter_1DTest2

