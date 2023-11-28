
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Peng Hao, John Miller
 *  @version 2.0
 *  @date    Fri May 26 14:32:21 EDT 2017
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Matrix Inversion
 */

package scalation
package mathstat

import scala.math.abs

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Fac_Inverse` class provides methods to factor an n-by-n identity matrix I
 *  into the product of two matrices a and a^-1
 *      a * a^-1 = I
 *  where a is the given matrix and a^-1 is its inverse.
 *  @param a  the given n-by-n square matrix
 */
class Fac_Inverse (a: MatrixD)
      extends Factorization:

    private val flaw = flawf ("Fac_Inverse")             // flaw function
    private var ai: MatrixD = null                       // the inverse of a (a^-1)

    if a.dim != a.dim2 then flaw ("init", "matrix a must be square")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Factor matrix I into the product of a and a^-1 by computing the
     *  inverse of a.
     */
    def factor (): Fac_Inverse =
        ai = Fac_Inverse.inverse (a)
        factored = true
        this
    end factor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return both the matrices a and its inverse a^-1.
     */
    def factors: (MatrixD, MatrixD) = (a, ai)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Use the inverse matrix 'ai' to solve a system of equations a * x = b.
     *  Return the solution vector x.
     *  @param b  the constant vector
     */
    def solve (b: VectorD): VectorD = ai * b

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Efficient calculation of inverse matrix a^-1 from existing factorization.
     *      a * a^-1 = I
     */
    def inverse: MatrixD =
        if ! factored then factor ()
        ai
    end inverse

end Fac_Inverse


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Fac_Inverse` companion object  provides methods for matrix imversion.
 */
object Fac_Inverse:

    private val flaw  = flawf ("Fac_Inverse")            // flaw function
    private val debug = debugf("Fac_Inverse", true)      // flaw function

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Invert matrix a (requires a square matrix) and use partial pivoting.
     *  @param  the matrix whose inverse is sought
     *
    def inverse (a: MatrixD): MatrixD =
        val n = a.dim
        if a.dim2 != n then flaw ("inverse", "matrix a must be square")

        val b = a.copy                                   // deep copy this matrix into b
        val c = MatrixD.eye (n, n)                       // let c represent the augmentation I

        for i <- b.indices do
            var pivot = b.v(i)(i)
            if pivot =~ 0.0 then
                val k = partialPivoting (b, i)           // find the maximum element below pivot
                b.swap (i, k, i)                         // in b, swap rows i and k from column i
                c.swap (i, k)                            // in c, swap rows i and k from column 0
                pivot = b.v(i)(i)                        // reset the pivot
            end if
            for j <- b.indices do
                b.v(i)(j) /= pivot
                c.v(i)(j) /= pivot
            end for
            for k <- b.indices if k != i do
                val mul = b.v(k)(i)
                for j <- b.indices do
                    b.v(k)(j) -= mul * b.v(i)(j)
                    c.v(k)(j) -= mul * c.v(i)(j)
                end for
            end for
        end for
        c
    end inverse
     */

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Invert matrix a (requires a square matrix) and use partial pivoting.
     *  Augmented matrix [ b | c ] = [ a | I ] -> [ I | a^-1 ]
     *  @param  the matrix whose inverse is sought
     */
    def inverse (a: MatrixD): MatrixD =
        val n = a.dim
        if a.dim2 != n then flaw ("inverse", "matrix a must be square")

        val b = a.copy                                   // deep copy this matrix into b (protect a)
        val c = MatrixD.eye (n, n)                       // let c represent the augmentation I

        for i <- b.indices do
            var pivot = b.v(i)(i)
            if pivot =~ 0.0 then
                val k = partialPivoting (b, i)           // find the maximum element below pivot
                b.swap (i, k, i)                         // in b, swap rows i and k from column i
                c.swap (i, k)                            // in c, swap rows i and k from column 0
                pivot = b.v(i)(i)                        // reset the pivot
            end if
            b(i) /= pivot                                // divide b part of row i by pivot in [ b | c]
            c(i) /= pivot                                // divide c part of row i by pivot in [ b | c]
            for k <- b.indices if k != i do
                val mul = b.v(k)(i)
                b(k) -= b(i) * mul                       // clear row k, except pivot in b-part
                c(k) -= c(i) * mul                       // same multiplier applied to row k in c-part
            end for
        end for
        c
    end inverse

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Invert a lower triangular matrix a (requires a square matrix).
     *  @see www.cplusplus.com/forum/beginner/245146/
     *  @param a  the lower triangular (lt) matrix whose inverse is sought
     */
    def inverse_lt (a: MatrixD): MatrixD =
        val n = a.dim
        if a.dim2 != n then flaw ("inverse", "matrix a must be square")
        val c = new MatrixD (n, n)                       // let c represent the emerging inverse

        for i <- a.indices do
            if a(i, i) == 0.0 then flaw ("inverse_lt", "matrix a is singular")

            c(i, i) = 1.0 / a(i, i)
            for j <- 0 until i do
                val c_i = c(i); val a_i = a(i)
                for k <- j until i do c_i(j) += a_i(k) * c(k, j)
                c(i, j) = -c(i, j) / a(i, i)
            end for 
        end for
        c
    end inverse_lt

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Invert an upper triangular matrix a (requires a square matrix).
     *  @see www.researchgate.net/figure/Matrix-inversion-of-upper-triangular-matrices_fig1_220094411
     *  @param a  the upper triangular (ut) matrix whose inverse is sought
     */
    def inverse_ut (a: MatrixD): MatrixD =
        val n = a.dim
        if a.dim2 != n then flaw ("inverse_ut", "matrix a must be square")
        val c = new MatrixD (n, n)                       // let c represent the emerging inverse

        for j <- a.indices2 do
            if a(j, j) == 0.0 then flaw ("inverse_ut", "matrix a is singular")

            for i <- 0 until j do
                val c_i = c(i) 
                for k <- 0 until j do c_i(j) += c_i(k) * a(k, j)
            end for
            for k <- 0 until j do c(k, j) /= -a(j, j)
            c(j, j) = 1.0 / a(j, j)
        end for
        c
    end inverse_ut

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Use partial pivoting to find a maximal non-zero pivot and return its row
     *  index, i.e., find the maximum element '(k, i)' below the pivot '(i, i)'.
     *  @param a  the matrix to perform partial pivoting on
     *  @param i  the row and column index for the current pivot
     */
    private def partialPivoting (a: MatrixD, i: Int): Int =
        var max  = a.v(i)(i)                             // initially set to the pivot
        var kMax = i                                     // initially the pivot row

        for k <- i + 1 until a.dim if abs (a.v(k)(i)) > max do
            max  = abs (a.v(k)(i))
            kMax = k
        end for

        if kMax == i then flaw ("partialPivoting", s"unable to find a non-zero pivot for row $i")
        debug ("partialPivoting", s"replace pivot ($i, $i) with pivot ($kMax, $i)")
        kMax
    end partialPivoting

end Fac_Inverse


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `fac_InverseTest` main function is used to test the `Fac_Inverse` class.
 *  > runMain scalation.mathstat.fac_InverseTest
 */
@main def fac_InverseTest (): Unit =

    val a = MatrixD ((4, 4), 4.0,  0.4,   0.8, -0.2,
                             0.4,  1.04, -0.12, 0.28,
                             0.8, -0.12,  9.2,  1.4,
                            -0.2,  0.28,  1.4,  4.35)

    val b = VectorD (-0.2, -0.32, 13.52, 14.17)

    val l = MatrixD ((3, 3), 2, 0, 0,
                             3, 4, 0,
                             5, 6, 7)

    println (s"a = $a")
    println (s"b = $b")

    banner ("Matrix Inversion")
    val inv = new Fac_Inverse (a)
    inv.factor ()
    println (s"factors = ${inv.factors}")
    println (s"solve   = ${inv.solve (b)}")

    banner ("LU Factorization")
    val lu = new Fac_LU (a)
    lu.factor ()
    println (s"factors = ${lu.factors}")
    println (s"solve   = ${lu.solve (b)}")

    banner ("Test Inverse")
    val a_inv = Fac_Inverse.inverse (a)
    println (s"Fac_Inverse.inverse (a) = $a_inv")
    println (s"a * a_inv = ${a * a_inv}")
    
    banner ("Test Inverse for Lower Triangular")
    val l_inv = Fac_Inverse.inverse_lt (l)
    println (s"Fac_Inverse.inverse_lt (a) = $l_inv")
    println (s"l * l_inv = ${l * l_inv}")

    banner ("Test Inverse for Upper Triangular")
    val u     = l.transpose
    val u_inv = Fac_Inverse.inverse_ut (u)
    println (s"Fac_Inverse.inverse_ut (a) = $u_inv")
    println (s"u * u_inv = ${u * u_inv}")

end fac_InverseTest

