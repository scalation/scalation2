
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sat Jul 30 22:53:47 EDT 2016
 *  @see     LICENSE (MIT style license file).
 *
 *  @note:   LQ Matrix Factorization (case rows m < columns n)
 */

package scalation
package mathstat

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Fac_LQ` class provides methods to factor an 'm-by-n' matrix 'aa' into the
 *  product of two matrices, when m < n.
 *      'l' - an 'm-by-m' left lower triangular matrix
 *      'q' - an 'm-by-n' orthogonal matrix and
 *  such that 'aa = l * q'.
 *  Note, orthogonal means that 'q.t * q = I'.
 *  @see `Fac-QR`
 *  @param aa  the matrix to be factored into l and q
 */
class Fac_LQ (aa: MatrixD)
      extends Factorization:

    private val art = aa.transpose                           // transpose of aa
    private var l: MatrixD = null                            // the left lower triangular l matrix
    private var q: MatrixD = null                            // the orthogonal q matrix

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Factor matrix 'art' into the product of two matrices, 'art = qt * rt'.
     *  Then compute 'r' and 'q'.
     *  @see http://math.stackexchange.com/questions/1640695/rq-decomposition
     */
    def factor (): Fac_LQ =
        val (qt, lt) = new Fac_QR (art, true).factor12 ()
        l = lt.transpose
        q = qt.transpose
        factored = true 
        this
    end factor

    def factors: (MatrixD, MatrixD) = (l, q)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve for 'x' in 'aa*x = b' using the 'LQ' Factorization 'aa = l*q' via
     *  'x = q.t * l.inv * b'.
     *  @param  b the constant vector
     */
    def solve (b: VectorD): VectorD = q.transpose * Fac_Inverse.inverse_lt (l) * b

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Efficient calculation of inverse matrix a^-1 from existing factorization.
     *      a * a^-1 = I
     */
    def inverse: MatrixD =
        factor ()
        val l_inv = Fac_Inverse.inverse_lt (l)                 // lower triangular matrix inverse
        q.transpose * l_inv
    end inverse

end Fac_LQ


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `fac_LQTest` main function is used to test the `Fac_LQ` class.
 *  > runMain scalation.mathstat.fac_LQTest
 */
@main def fac_LQTest (): Unit =

    def test (a: MatrixD, b: VectorD): Unit =
        val lq = new Fac_LQ (a)                              // for factoring a into q * r
        val (l, q) = lq.factor12 ()                          // (l left lower triangular, q orthogonal)
   
        println ("--------------------------------------------------------")
        println ("a   = " + a)                               // original matrix
        println ("l   = " + l)                               // left matrix
        println ("q   = " + q)                               // orthogonal matrix
        println ("l*q = " + l*q)                             // check that l*r = a
        val x = lq.solve (b)
        println ("x   = " + x)                               // solve for x in a*x = b
        println ("b   = " + b)                               // rhs vector
        println ("a*x = " + a*x)                             // check that a*x = b
    end test

    val a1 = MatrixD ((2, 4), 1.0, 2.0, 3.0, 4.0,
                              5.0, 6.0, 7.0, 8.0)

    val b1 = VectorD (10, 12)
    test (a1, b1)

end fac_LQTest

