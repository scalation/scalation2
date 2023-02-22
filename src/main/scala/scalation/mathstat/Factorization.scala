
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Thu Jun 17 19:29:23 EDT 2021
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Trait for Matrix Factorization Algorithms
 */

package scalation
package mathstat

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Factorization` trait is the template for classes implementing various forms
 *  of matrix factorization.
 */
trait Factorization:

    /** Flag indicating whether the matrix has been factored
     */
    protected var factored = false

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset by setting factored to false.
     */
    def reset (): Unit = factored = false

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return whether the matrix has been factored has aleady been factored.
     */
    inline def isFactored: Boolean = factored

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Factor a matrix into the product of two matrices without returning the
     *  two factored matrices.  Allows for example skipping the computation of the
     *  Q matrix in QR factorization when it is not needed, e.g., for regression.
     *  Class implementing the 'factor' method should set 'factored = true'.
     */
    def factor (): Factorization

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the two factored matrices.
     */
    def factors: (MatrixD, MatrixD)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Factor a matrix into the product of two matrices, e.g., 'a = l * l.t' or
     *  a = q * r, returning both the first and second matrices.
     */
    def factor12 (): (MatrixD, MatrixD) = { if ! factored then factor (); factors }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Factor a matrix into the product of two matrices, e.g., 'a = l * l.t',
     *  returning only the first matrix.
     */
    def factor1 (): MatrixD = { if ! factored then factor (); factors._1 }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Factor a matrix into the product of two matrices, e.g., 'a = l * l.t',
     *  returning only the second matrix.
     */
    def factor2 (): MatrixD = { if ! factored then factor (); factors._2 }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve a system of equations, e,g., 'a * x = b' for 'x', using the factored
     *  matrices, by first performing forward substitution and then backward substitution.
     *  Must factor before calling 'solve'.
     *  @param b  the constant vector
     */
    def solve (b: VectorD): VectorD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Efficient calculation of inverse matrix a^-1 from existing factorization.
     *      a * a^-1 = I
     */
    def inverse: MatrixD

end Factorization

