
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Santosh Uttam Bobade, Vinay Kumar Bingi, Mohammad Toutiaee, John Miller
 *  @version 2.0
 *  @date    Sat Jun 9 14:09:25 EDT 2018
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Collection of Simple Imputation Techniques for Missing Values or Outliers
 */

package scalation
package modeling

import scalation.mathstat._
import scalation.random.Normal

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Imputation` trait specifies an imputation operation called impute to be defined
 *  by the objects implementing it, i.e.,
 *      `ImputeRegression`  - impute missing values using `SimpleRegression`
 *      `ImputeForward`     - impute missing values using previous values and slopes
 *      `ImputeBackward`    - impute missing values using subsequent values and slopes
 *      `ImputeMean`        - impute missing values usind the filtered mean
 *      `ImputeNormal`      - impute missing values using the median of Normal random variates
 *      `ImputeMovingAvg`   - impute missing values using the moving average
 *      `ImputeNormalWin`   - impute missing values using the median of Normal random variates for a window
 */
trait Imputation:

    protected val debug   = debugf ("Imputation", false)               // debug function
    protected val DAMPEN  = 0.8                                        // dampening factor for slope
    protected val q       = 5                                          // number of elements in moving average

    protected var missVal = NO_DOUBLE                                  // default for missing value indicator
    protected var dist    = 3                                          // distance before and after point

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the missing value missVal to the new missing value indicator missVal_.
     *  @param missVal_  the new missing value indicator
     */
    def setMissVal (missVal_ : Double): Unit = missVal = missVal_

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the distance dist to the new value dist_.
     *  @param dist_  the new value for the distance
     */
    def setDist (dist_ : Int): Unit = dist = dist_

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Impute a value for vector x at index i.
     *  Does not modify the vector.
     *  @param x  the vector with missing values
     *  @param i  the index position for which to impute a value
     */
    def imputeAt (x: VectorD, i: Int): Double
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Impute a value for the first missing value in vector x from index i.
     *  The type (Int, Double) returns (vector index for imputation, imputed value).
     *  Does not modify the vector.
     *  @param x  the vector with missing values
     *  @param i  the starting index to look for missing values
     */
    def impute (x: VectorD, i: Int = 0): (Int, Double) = findMissing (x, i)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Replace all missing values in vector x with imputed values.  Will change
     *  the values in vector x.  Make a copy to preserve values x.copy.
     *  @param x  the vector with missing values
     */
    def imputeAll (x: VectorD): VectorD =
        var i = 0                                                      // starting index
        while true do
            val (im, z) = impute (x, i)                                // get index of missing and imputed value
            if im != -1 then x(im) = z else return x                   // update the vector or return
            i = im + 1                                                 // set index to missing position + 1
        end while
        x                                                              // return updated vector
    end imputeAll

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Replace all missing values in matrix x with imputed values.  Will change
     *  the values in matrix x.  Make a copy to preserve values x.copy.
     *  @param x  the matrix with missing values
     */
    def impute (x: MatrixD): MatrixD =
        for j <- x.indices2 do imputeAll (x(?, j))
        x
    end impute

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Impute a value for the first missing value in column c from index i.
     *  The type (Int, Double) returns (vector index for imputation, imputed value).
     *  Does not modify the column.
     *  @param c  the column with missing values
     *  @param i  the starting index to look for missing values
     *
    def imputeCol (c: VectorS, i: Int = 0): (Int, VectorS) =
        val x = c.toDouble                                             // convert to a double vector
        val (im, z) = impute (x, i)                                    // get index of missing and imputed value
        (im, VectorS.fromDouble (c, z))                                // convert back to column type
    end imputeCol
     */

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the median of three normally distributed random numbers.
     *  @param mu    the mean
     *  @param sig2  the variance
     */
    protected def normalMedian (mu: Double, sig2: Double): Double =
        val rn = Normal (mu, sig2)                                     // Normal random variate generator
        median3 (rn.gen, rn.gen, rn.gen)
    end normalMedian 

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the next non-missing value in vector x from index i.  If none, return missVal.
     *  @param x  the vector to be searched for a non-missing value
     *  @param i  the starting index to look for non-missing value
     */
    protected def nextVal (x: VectorD, i: Int): Double =
        val j = x.indexWhere (_ != missVal, i)                         // find first non-missing from i
        if j >= 0 then x(j) else missVal
    end nextVal

/*
        for j <- i until x.dim if x(j) != missVal do return x(j)       // find first non-missing from i
        println (s"nextVal: unable to find any non-missing values from $i to ${x.dim -1}")
        missVal
*/

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the previous non-missing value in vector x from index i.  If none, return missVal.
     *  @param x  the vector to be searched (backwards) for a non-missing value
     *  @param i  the starting index to look for non-missing value
     */
    protected def prevVal (x: VectorD, i: Int): Double =
        val j = x.lastIndexWhere (_ != missVal, i)                      // find first non-missing from i backward
        if j >= 0 then x(j) else missVal
    end prevVal

/*
        for j <- i to 0 by -1 if x(j) != missVal do return x(j)        // find first non-missing from i
        println (s"prevVal: unable to find any non-missing values from $i downto 0")
        missVal
*/

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the index of first missing value in vector x from index i and the
     *  new imputed value.
     *  @param x  the vector with missing values
     *  @param i  the starting index to look for missing value
     */
    def findMissing (x: VectorD, i: Int = 0): (Int, Double) =
        val j = x.indexOf (missVal, i)                                // find first missing from i
        if j >= 0 then (j,  imputeAt (x, j))                          // return (index, imputed value)
        else           (-1, nextVal (x, i))                           // return (not found, first value)
    end findMissing

/*
        var value = nextVal (x, i)
        for j <- i until x.dim if x(j) == missVal do                  // find first missing from i
            value = imputeAt (x, j)
            return (j, value)                                         // return (index, imputed value)
        end for
*/

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the index of last missing value in vector x from index i and the
     *  new imputed value.
     *  @param x   the vector with missing values
     *  @param i_  the starting index to look for missing value
     */
    def findLastMissing (x: VectorD, i_ : Int = -1): (Int, Double) =
        val i = if i_ < 0 then x.dim-1 else i_
        val j = x.lastIndexOf (missVal, i)                            // find last missing from i
        if j >= 0 then (j,  imputeAt (x, j))                          // return (index, imputed value)
        else           (-1, prevVal (x, i))                           // return (not found, last value)
    end findLastMissing

/*
        var value = prevVal (x, i)
        for j <- i to 0 by -1 if x(j) == missVal do                   // find last missing from ii
            val value = imputeAt (x, j)
            return (j, value)                                         // return (index, imputed value)
        end for
        (-1, value)                                                   // return (not found, last value)
*/

end Imputation


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ImputeRegression` object imputes missing values using `SimpleRegression`.
 */
object ImputeRegression extends Imputation:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Impute a value for the first missing value in vector x from index i
     *  using `SimpleRegression`.
     *  @param x  the vector with missing values
     *  @param i  the starting index to look for missing values
     */
    def imputeAt (x: VectorD, i: Int): Double =
        val xf = x.filter (_ != missVal)
        val t  = VectorD.range (0, xf.dim)
        val rg = SimpleRegression (t, xf, null)
        rg.train (rg.getX, xf)
        rg.predict (VectorD (1, t(i)))
    end imputeAt

end ImputeRegression


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ImputeForward` object imputes missing values using the previous value and slope.
 */
object ImputeForward extends Imputation:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Impute a value for the first missing value in vector x from index i
     *  using the previous value and slope.
     *  @param x  the vector with missing values
     *  @param i  the starting index to look for missing values
     */
    def imputeAt (x: VectorD, i: Int): Double =
        if i == 0 then      nextVal (x, 1)                             // next non-missing value
        else if i == 1 then x(0)                                       // first value
        else x(i-1) + DAMPEN * (x(i-1) - x(i-2))                       // slope adjusted previous value
    end imputeAt

end ImputeForward


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ImputeBackward` object imputes missing values using the subsequent value and slope.
 */
object ImputeBackward extends Imputation:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Impute a value for the first missing value in vector x from index i
     *  using the next value and slope.
     *  @param x   the vector with missing values
     *  @param i_  the starting index to look for missing values
     */
    def imputeAt (x: VectorD, i_ : Int): Double =
        val l = x.dim - 1                                              // last index position
        val i = if i_ < 0 then l else i_                               // -1 => last position

        if i == l then        prevVal (x, l-1)                         // previous non-missing value
        else if i == l-1 then x(l)                                     // last value 
        else x(i+1) - DAMPEN * (x(i+2) - x(i+1))                       // slope adjusted next value
    end imputeAt

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Impute a value for the first missing value in vector x from index i.
     *  The type (Int, Double) returns (vector index for imputation, imputed value).
     *  Does not modify the vector.
     *  @param x  the vector with missing values
     *  @param i  the starting index to look for missing values
     */
    override def impute (x: VectorD, i: Int = -1): (Int, Double) = findLastMissing (x, i)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Replace all missing values (in reverse) in vector x with imputed values.
     *  @param x  the vector with missing values
     */
    override def imputeAll (x: VectorD): VectorD =
        var i = x.dim - 1                                              // starting index
        while true do
            val (im, z) = impute (x, i)                                // get index of missing and imputed value
            if im != -1 then x(im) = z else return x                   // update the vector or return
            i = im - 1                                                 // set index to missing position + 1
        end while
        x                                                              // return updated vector
    end imputeAll

end ImputeBackward


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ImputeMean` object imputes missing values using the filtered mean.
 */
object ImputeMean extends Imputation:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Impute a value for the first missing value in vector x from index i
     *  using the filtered mean.
     *  @param x  the vector with missing values
     *  @param i  the starting index to look for missing values (ignored)
     */
    def imputeAt (x: VectorD, i: Int):  Double = 
        val xf = x.filter (_ != missVal)
        xf.mean
    end imputeAt

end ImputeMean


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ImputeNormal` object imputes missing values using the median Normal variates.
 */
object ImputeNormal extends Imputation:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Impute a value for the first missing value in vector x from index i
     *  using the median of three Normally distributed random values.
     *  @param x  the vector with missing values
     *  @param i  the starting index to look for missing values (ignored)
     */
    def imputeAt (x: VectorD, i: Int): Double =
        val xf = x.filter (_ != missVal)
        normalMedian (xf.mean, xf.variance)
    end imputeAt

end ImputeNormal


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ImputeMovingAvg` object imputes missing values using the moving average.
 */
object ImputeMovingAvg extends Imputation:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Impute a value for the first missing value in vector x from index i
     *  using the moving average of the last dist values.
     *  @param x  the vector with missing values
     *  @param i  the starting index to look for missing values
     */
    def imputeAt (x: VectorD, i: Int): Double =
        var (sum, cnt) = (0.0, 0)
        for k <- i - dist to i + dist do
             if k >= 0 && k < x.dim && k != i && x(k) != missVal then { sum += x(k); cnt += 1 }
        end for
        sum / cnt
    end imputeAt

end ImputeMovingAvg


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ImputeNormalWin` object imputes the missing values in the vector using
 *  Normal Distribution for a sliding window.
 */
object ImputeNormalWin extends Imputation:

    def imputeAt (x: VectorD, i: Int): Double =
         throw new UnsupportedOperationException ("ImputeNormalWin: 'impute' not supported, use 'imputeAll'")
    end imputeAt

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Impute all the missing values in vector x using Normal Distribution for
     *  a sliding window.
     *  @param q  size of the sliding window
     */
    override def imputeAll (x: VectorD): VectorD =
        val z     = new VectorD (x.dim)
        val sumq  = new SumSqQueue (q)
        sumq     += nextVal (x, 0)                                     // prime with first non-missing value

        for i <- x.indices do
            debug ("imputeAll", s"mu = ${sumq.mean}, sig2 = ${sumq.variance}")
            z(i) = if x(i) == missVal then normalMedian (sumq.mean, sumq.variance)
                   else x(i)
            sumq += z(i)
        end for

        z
    end imputeAll

end ImputeNormalWin


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `imputationTest` main function is used to test the objects extending the
 *  `Imputation` trait.
 *  > runMain scalation.modeling.imputationTest
 */
@main def imputationTest (): Unit =

     val x = VectorD (1, 2, 3, 4, NO_DOUBLE, 6, 7, 8, 9)
     val x2 = x.copy
     val x3 = x.copy
     var iv = (-1, NO_DOUBLE)

     banner("ImputeRegression.impute")
     iv = ImputeRegression.impute (x)
     x2(iv._1) = iv._2
     println (s"x  = $x")
     println (s"x2 = $x2")

     banner("ImputeForward.impute")
     iv = ImputeForward.impute (x)
     x2(iv._1) = iv._2
     println (s"x  = $x")
     println (s"x2 = $x2")

     banner("ImputeBackward.impute")
     iv = ImputeBackward.impute (x)
     x2(iv._1) = iv._2
     println (s"x  = $x")
     println (s"x2 = $x2")

     banner("ImputeMean.impute")
     iv = ImputeMean.impute (x)
     x2(iv._1) = iv._2
     println (s"x  = $x")
     println (s"x2 = $x2")

     banner("ImputeNormal.impute")
     iv = ImputeNormal.impute (x)
     x2(iv._1) = iv._2
     println (s"x  = $x")
     println (s"x2 = $x2")

     banner("ImputeMovingAvg.impute")
     iv = ImputeMovingAvg.impute (x)
     x2(iv._1) = iv._2
     println (s"x  = $x")
     println (s"x2 = $x2")

     banner("ImputeRegression.imputeAll")
     println ("x3 = " + ImputeRegression.imputeAll (x3.copy))

     banner("ImputeForward.imputeAll")
     println ("x3 = " + ImputeForward.imputeAll (x3.copy))

     banner("ImputeBackward.imputeAll")
     println ("x3 = " + ImputeBackward.imputeAll (x3.copy))

     banner("ImputeMean.imputeAll")
     println ("x3 = " + ImputeMean.imputeAll (x3.copy))

     banner("ImputeNormal.imputeAll")
     println ("x3 = " + ImputeNormal.imputeAll (x3.copy))

     banner("ImputeMovingAvg.imputeAll")
     println ("x3 = " + ImputeMovingAvg.imputeAll (x3.copy))

     banner("ImputeNormalWin.imputeAll")
     println ("x3 = " + ImputeNormalWin.imputeAll (x3.copy))

end imputationTest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `imputationTest2` main function is used to test the objects extending the
 *  `Imputation` trait.
 *  > runMain scalation.modeling.imputationTest2
 */
@main def imputationTest2 (): Unit =

     val x  = VectorD (NO_DOUBLE, NO_DOUBLE, 1, 2, 3, 4, 5, 6, 7, 8, 9)
     var x2 = null.asInstanceOf [VectorD]
     val x3 = x.copy
     var iv = (-1, NO_DOUBLE)

     banner("ImputeRegression.impute")
     iv = ImputeRegression.impute (x)
     x2 = x.copy
     x2(iv._1) = iv._2
     println (s"x  = $x")
     println (s"x2 = $x2")

     banner("ImputeForward.impute")
     iv = ImputeForward.impute (x)
     x2 = x.copy
     x2(iv._1) = iv._2
     println (s"x  = $x")
     println (s"x2 = $x2")

     banner("ImputeBackward.impute")
     iv = ImputeBackward.impute (x)
     x2 = x.copy
     x2(iv._1) = iv._2
     println (s"x  = $x")
     println (s"x2 = $x2")

     banner("ImputeMean.impute")
     iv = ImputeMean.impute (x)
     x2 = x.copy
     x2(iv._1) = iv._2
     println (s"x  = $x")
     println (s"x2 = $x2")

     banner("ImputeNormal.impute")
     iv = ImputeNormal.impute (x)
     x2 = x.copy
     x2(iv._1) = iv._2
     println (s"x  = $x")
     println (s"x2 = $x2")

     banner("ImputeMovingAvg.impute")
     iv = ImputeMovingAvg.impute (x)
     x2 = x.copy
     x2(iv._1) = iv._2
     println (s"x  = $x")
     println (s"x2 = $x2")

     banner("ImputeRegression.imputeAll")
     println ("x3 = " + ImputeRegression.imputeAll (x3.copy))

     banner("ImputeForward.imputeAll")
     println ("x3 = " + ImputeForward.imputeAll (x3.copy))

     banner("ImputeBackward.imputeAll")
     println ("x3 = " + ImputeBackward.imputeAll (x3.copy))

     banner("ImputeMean.imputeAll")
     println ("x3 = " + ImputeMean.imputeAll (x3.copy))

     banner("ImputeNormal.imputeAll")
     println ("x3 = " + ImputeNormal.imputeAll (x3.copy))

     banner("ImputeMovingAvg.imputeAll")
     println ("x3 = " + ImputeMovingAvg.imputeAll (x3.copy))

     banner("ImputeNormalWin.imputeAll")
     println ("x3 = " + ImputeNormalWin.imputeAll (x3.copy))

end imputationTest2

