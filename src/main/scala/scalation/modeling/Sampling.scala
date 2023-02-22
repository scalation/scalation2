
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Wed Nov  7 17:08:17 EST 2018
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Model: Sampling
 */

package scalation
package modeling

import scalation.mathstat._
import scalation.random.RandomVecSample

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Create a random sub-sample of rows from matrix x, returning the sub-sample
 *  matrix and the indices selected.  Must change the stream parameter to get
 *  a different subsample.
 *  @param x       the data original matrix
 *  @param nSamp   the desired sample size (number of rows in matrix)
 *  @param stream  the random number stream to use
 */
def subSample (x: MatrixD, nSamp: Int, stream: Int): (MatrixD, VectorI) =
    if nSamp >= x.dim then
        (x, null)
    else
        val rsg   = RandomVecSample (x.dim, nSamp, stream)         // random sample generator
        val irows = rsg.igen                                       // select rows, e.g., 5, 3, 7
        (x(irows), irows)
    end if
end subSample

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Create a random sub-sample of rows from matrix x and vector y,
 *  returning the sub-sample matrix and vector and the indices selected.
 *  @param x       the original input/data matrix
 *  @param y       the original output/response vector
 *  @param nSamp   the desired sample size (number of rows in matrix)
 *  @param stream  the random number stream to use
 */
def subSample (x: MatrixD, y: VectorD, nSamp: Int, stream: Int): (MatrixD, VectorD, VectorI) =
    if nSamp >= x.dim then
        (x, y, null)
    else
        val rsg   = RandomVecSample (x.dim, nSamp, stream)         // random sample generator
        val irows = rsg.igen                                       // select rows, e.g., 5, 3, 7
        (x(irows), y(irows), irows)
    end if
end subSample

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Create a random sub-sample of rows from matrix x and integer-valued  vector y,
 *  returning the sub-sample matrix and vector and the indices selected.
 *  @param x       the original input/data matrix
 *  @param y       the original integer-valued output/response vector
 *  @param nSamp   the desired sample size (number of rows in matrix)
 *  @param stream  the random number stream to use
 */
def subSample (x: MatrixD, y: VectorI, nSamp: Int, stream: Int): (MatrixD, VectorI, VectorI) =
    if nSamp >= x.dim then
        (x, y, null)
    else
        val rsg   = RandomVecSample (x.dim, nSamp, stream)         // random sample generator
        val irows = rsg.igen                                       // select rows, e.g., 5, 3, 7
        (x(irows), y(irows), irows)
    end if
end subSample

