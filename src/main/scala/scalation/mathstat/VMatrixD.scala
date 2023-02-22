
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sat Sep 17 17:37:56 EDT 2022
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Virtual Matrix Data Structure of Doubles
 */

package scalation
package mathstat

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `VMatrixD` class maintains virtual-matrices where the first dim2 columns
 *  are stored, while the last dim2v are virtual, i.e., calculated as needed.
 *  Virtual-matrices offer memory reduction for Quadratic, Cubic, and Symbolic
 *  Regression, where augmented columns can be calculated rather than stored.
 *  @see scalation.modeling.SymbolicRegression
 *  @param dim_      the first (row) dimension of the matrix
 *  @param dim2_     the stored second (column) dimension of the matrix
 *  @param dim2v     the virtual second (column) dimension of the matrix
 *  @param formulas  the formulas used to calculate the last dim2v columns
 *                       Array (1, 1) => x1 * x1; Array (1, 2) => x1 * x2
 *  @param v_        the 2D array used to store matrix the first dim2_ columns
 */
class VMatrixD (dim_ :     Int,
                dim2_ :    Int,
                val dim2v: Int,
                formulas:  Array [Array [Int]],
                private [mathstat] v_ : Array [Array [Double]] = null)
      extends MatrixD (dim_, dim2_, v_):

    private val debug = debugf ("VMatrixD", true)               // partial invocation of debug function
    private val flaw  = flawf ("VMatrixD")                      // partial invocation of flaw function

    if formulas.size != dim2v then flaw ("init", "# formulas != # virtual columns")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the ELEMENT in row i, column j of this virtual-matrix.
     *  usage: x(3, 2)
     *  @param i  the row index
     *  @param j  the column index
     */
    override def apply (i: Int, j: Int): Double =
        if j < dim2 then v(i)(j)                                // stored value
        else product (i, formulas (j-dim2))                     // virtual/calculated value
    end apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Calculate the product of the given columns for row i.
     *  Example:  Array (0, 1) means calculate the following product v(i)(0) * v(i)(1)
     *  @param i     the current/working row
     *  @param cols  the columns to take the product of
     */
    private def product (i: Int, cols: Array [Int]): Double =
        var prod = 1.0
        for k <- cols do prod *= v(i)(k)
        prod
    end product

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply this virtual-matrix by matrix y (requires y to have at least the
     *  dimensions of this).
     *  FIX - to be optimized, needs to handle case of y being a virtual matrix
     *  @param y  the other matrix
     */
    override def * (y: MatrixD): MatrixD =
        val (m, n) = (dim, dim2 + dim2v)
        val x = new MatrixD (m, y.dim2)
        for i <- x.indices; j <- x.indices2 do
            var s = 0.0
            for k <- 0 until n do s += apply(i, k) * y(k, j)
            x(i, j) = s
        end for
        x
    end *

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply this virtual-matrix by vector y (requires y to have at least the
     *  dimensions of this).
     *  @param y  the vector to multiple by
     */
    override def * (y: VectorD): VectorD =
        val (m, n) = (dim, dim2 + dim2v)
        val x = new VectorD (m)
        for i <- x.indices do
            var s = 0.0
            for k <- 0 until n do s += apply(i, k) * y(k)
            x(i) = s
        end for
        x
    end *

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply the transpose this virtual-matrix by itself, e.g., X^T X.
     */
    def tTimes: MatrixD =
        val (m, n) = (dim, dim2 + dim2v)
        val x = new MatrixD (n, n)
        for i <- x.indices; j <- x.indices2 do
            var s = 0.0
            for k <- 0 until m do s += apply(k, i) * apply(k, j)
            x(i, j) = s
        end for
        x
    end tTimes

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply the transpose this virtual-matrix by vector y, e.g., X^T y.
     *  @param y  the vector to multiply by
     */
    def tTimes (y: VectorD): VectorD =
        val (m, n) = (dim, dim2 + dim2v)
        val x = new VectorD (n)
        for i <- x.indices do
            var s = 0.0
            for k <- 0 until m do s += apply(k, i) * y(k)
            x(i) = s
        end for
        x
    end tTimes

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert this virtual-matrix to a regular (fully stored) matrix.
     */
    override def toMatrixD: MatrixD =
        val (m, n) = (dim, dim2 + dim2v)
        val x = new MatrixD (m, n)
        for i <- x.indices; j <- x.indices2 do x(i, j) = apply(i, j)
        x
    end toMatrixD

end VMatrixD


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `VMatrixD companion object provides factory methods.
 */
object VMatrixD:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a virtual-matrix from a regular (fully stored) matrix and an array
     *  formulas for calculating the additional virtual columns.
     *  @param x         the source (fully stored) matrix
     *  @param formulas  the formulas used to calculate the additional virtual columns
     */
    def apply (x: MatrixD, formulas: Array [Array [Int]]): VMatrixD =
        new VMatrixD (x.dim, x.dim2, formulas.size, formulas, x.v) 
    end apply

end VMatrixD


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `vMatrixDTest` main function is used to test the `VMatrixD` class.
 *  It tests the calculation of X^T X, X^T y, and Cholesky Factorization for
 *  Regression and Quadratic (no-cross) Regression for the Texas Temperatures dataset.
 *  @see scalation.modeling.regressionTest7
 *  > runMain scalation.mathstat.vMatrixDTest
 */
@main def vMatrixDTest (): Unit =

    // 16 data points:         one      x1      x2       x3     y
    //                                 Lat    Elev     Long  Temp        County
    val xy = MatrixD ((16, 5), 1.0, 29.767,   41.0,  95.367, 56.0,    // Harris
                               1.0, 32.850,  440.0,  96.850, 48.0,    // Dallas
                               1.0, 26.933,   25.0,  97.800, 60.0,    // Kennedy
                               1.0, 31.950, 2851.0, 102.183, 46.0,    // Midland
                               1.0, 34.800, 3840.0, 102.467, 38.0,    // Deaf Smith
                               1.0, 33.450, 1461.0,  99.633, 46.0,    // Knox
                               1.0, 28.700,  815.0, 100.483, 53.0,    // Maverick
                               1.0, 32.450, 2380.0, 100.533, 46.0,    // Nolan
                               1.0, 31.800, 3918.0, 106.400, 44.0,    // El Paso
                               1.0, 34.850, 2040.0, 100.217, 41.0,    // Collington
                               1.0, 30.867, 3000.0, 102.900, 47.0,    // Pecos
                               1.0, 36.350, 3693.0, 102.083, 36.0,    // Sherman
                               1.0, 30.300,  597.0,  97.700, 52.0,    // Travis
                               1.0, 26.900,  315.0,  99.283, 60.0,    // Zapata
                               1.0, 28.450,  459.0,  99.217, 56.0,    // Lasalle
                               1.0, 25.900,   19.0,  97.433, 62.0)    // Cameron

    val y    = xy(?, 4)                               // column 4
    val mu_y = y.mean
    val sst  = (y - mu_y).normSq                      // sum of squares total

    //--------------------------------------------------------------------------
    banner ("Regression y = Xb via Cholesky Factorization")

    val x = xy(?, 0 until 4)                          // columns 0-3

    val xt    = x.transpose
    val x_t_x = xt * x                                // X^T X
    val x_t_y = xt * y                                // X^T y

    var fac = new Fac_Cholesky (x_t_x).factor ()
    var b   = fac.solve (x_t_y)                       // least-squares estimates
    var yp  = x * b                                   // predicted response
    var sse = (y - yp).normSq                         // sum of squared errors

    println (s"design matrix   x = $x")
    println (s"response vector y = $y")
    println (s"parameters      b = $b")
    println (s"predicted      yp = $yp")
    println (s"sse           sse = $sse")
    println (s"R^2           R^2 = ${1 - sse/sst}")

    //--------------------------------------------------------------------------
    banner ("Quadratic Regression y = Qb via Cholesky Factorization")

    val z = VMatrixD (x, Array (Array (1, 1),         // add x1 * x1 column to virtual-matrix
                                Array (2, 2),         // add x2 * x2 column
                                Array (3, 3)))        // add x3 * x3 column
    val q = z.toMatrixD                               // convert to fully stored

    val qt    = q.transpose
    val q_t_q = qt * q                                // Q^T Q
    val q_t_y = qt * y                                // Q^T y

    println (s"q_t_q = $q_t_q")
    println (s"q_t_y = $q_t_y")

    fac = new Fac_Cholesky (q_t_q).factor ()
    b   = fac.solve (q_t_y)                           // least-squares estimates
    yp  = q * b                                       // predicted response
    sse = (y - yp).normSq                             // sum of squared errors

    println (s"design matrix   q = $q")
    println (s"response vector y = $y")
    println (s"parameters      b = $b")
    println (s"predicted      yp = $yp")
    println (s"sse           sse = $sse")
    println (s"R^2           R^2 = ${1 - sse/sst}")

    //--------------------------------------------------------------------------
    banner ("Quadratic Regression y = Zb via Cholesky Factorization using Virtual Matrices")

    val z_t_z = z.tTimes                               // Z^T Z
    val z_t_y = z.tTimes (y)                           // Z^T y

    println (s"z_t_z = $z_t_z")
    println (s"z_t_y = $z_t_y")

    fac = new Fac_Cholesky (z_t_z).factor ()
    b   = fac.solve (z_t_y)                            // least-squares estimates
    yp  = z * b                                        // predicted response
    sse = (y - yp).normSq                              // sum of squared errors

    println (s"design matrix   z = $q")
    println (s"response vector y = $y")
    println (s"parameters      b = $b")
    println (s"predicted      yp = $yp")
    println (s"sse           sse = $sse")
    println (s"R^2           R^2 = ${1 - sse/sst}")

end vMatrixDTest

