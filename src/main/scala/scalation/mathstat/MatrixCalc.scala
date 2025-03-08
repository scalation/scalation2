
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Thu Jun 17 19:29:23 EDT 2021
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Matrix Calculations
 */

package scalation
package mathstat

import scala.math.round

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MattixCalc` class supports simple simple calculations on elements in a matrix.
 *  @param x       the matrix of data
 *  @param header  the column names
 */
class MatrixCalc (x: MatrixD, header: VectorS):

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform calculations for each row using the given formula.
     *  Usage:  val q = new MatrixCalc (x, hd)
     *  Usage:  q.fill (3, 2, 1, (zt: Double) => r * zt)
     *  @param c1       the column to be assigned
     *  @param c2       the column supplying the data
     *  @param offset   the row index offset applied to c2 (e.g., get past data)
     *  @param formula  the c1 = formula (c2) appropriately offset
     */
    def fill (c1: Int, c2: Int, offset: Int, formula: FunctionS2S): Unit =
        for i <- offset until x.dim do
            x(i, c1) = formula (x(i - offset, c2))
    end fill

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Show header and the matrix.
     */
    def show (): Unit =
        println (header)
        println (x)
    end show

end MatrixCalc


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `matrixCalc0` main function is used to test the `MatrixCalc` class.
 *  It performs the RW and AR(1) time series model calculations.
 *  > runMain scalation.mathstat.matrixCalc0
 */
@main def matrixCalc0 (): Unit =

//  val model = "NULL"
    val model = "RW"
//  val model = "AR"

    val y   = VectorD (1, 3, 4, 2, 5, 7, 9, 8, 6, 3)
    val m   = y.dim
    val t   = VectorD.range (0, m) 
    val hdr = VectorS ("t", "yt", "zt", "zˆt", "yˆt", "ε", "ε2")
    val x   = new MatrixD (m, hdr.dim) 

    val ybar = y.mean
    val r    = y(0 until m-1) corr y(1 until m)           // HW - why are r, r_ different
    val r_   = y.acorr (1)                                // try using r_
    println (s"ybar = $ybar, rho_1: r = $r, r_ = $r_")

    x(?, 0) = t                                           // time
    x(?, 1) = y                                           // time series y_t

    model match
    case "NULL" =>
        for i <- 1 until m do x(i, 4) = ybar              // y_t-hat
    case "RW" =>
        for i <- 1 until m do x(i, 4) = x(i-1, 1)         // y_t-hat
    case _ => 
        x(?, 2) = y - ybar                                // centered z_t
        for i <- 1 until m do x(i, 3) = x(i-1, 2) * r_    // z_t-hat, try both r, r_
        x(?, 4) = x(?, 3) + ybar                          // uncentered y_t-hat

    x(?, 5) = y - x(?, 4)                                 // error ε
    x(?, 6) = x(?, 5) ~^ 2                                // squared error
    println (hdr)
    println (x)

    val y_   = y(1 until m)
    val csse = x(1 until m, 6).sum
    val sst  = (y_ - ybar).normSq
    val rSq  = 1 - csse / sst
    println (s"csse = $csse, sst = $sst, rSq = $rSq")

end matrixCalc0


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `matrixCalc2` main function read a matrix from a CSV file and perform calculations,
 *  e.g. grade calculations.
 *  > runMain scalation.mathstat.matrixCalc2
 */
@main def matrixCalc2 (): Unit =

    val csvFile = "scores.csv"

    val xx = MatrixD.load (csvFile, 1, 3, fullPath = true)                        // skip 1 row and 3 columns
    val n = xx.dim2 - 3                                                           // last column should be empty
    val x = xx(?, 0 until n+1)

    println (s"x = $x")

    val w = x(0)(0 until n)                                                       // weights for grades
    println (s"total weight = ${w.sum}")                                          // total weight
    for i <- 1 until x.dim do x(i, n) = round (w dot x(i)(0 until n)).toDouble    // weighted total
    println (s"new x = $x")                                                       // updated matrix

end matrixCalc2


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `matrixCals3` main function allows for Custom calculations.
 *  > runMain scalation.mathstat.matrixCalc3
 */
@main def matrixCalc3 (): Unit =

   val x1 = VectorD (81,93,80,100,100,95,77,94)                                   // original   +1 on Exam I
   val x2 = VectorD (80,93,80,120,100,95,77,94)                                   // corrected +20 on Homework

   val w  = VectorD (2,2,2.5,0.25,0.25,0.75,0.75,1.5)

   println (s"w * x1 = ${w dot x1}")                                              // 868
   println (s"w * x2 = ${w dot x2}")                                              // 871

end matrixCalc3


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `matrixCals4` main function allows for Custom calculations.
 *  > runMain scalation.mathstat.matrixCalc4
 */
@main def matrixCalc4 (): Unit =

   val x = MatrixD ((7, 13),
       4.67647, 4.85294, 4.70588, 4.82353, 4.73529, 4.55882, 4.67647, 4.58824, 4.97059, 4.76471, 4.55882, 4.50000, 4.64706,  // 6370 sp24
       4.12500, 4.12500, 4.37500, 4.37500, 4.25000, 4.12500, 4.37500, 4.12500, 4.75000, 4.50000, 4.62500, 4.00000, 4.62500,  // 4370 sum24
       5.00000, 5.00000, 5.00000, 5.00000, 5.00000, 5.00000, 5.00000, 5.00000, 5.00000, 5.00000, 5.00000, 5.00000, 5.00000,  // 6370 sum24
       3.33333, 3.50000, 3.33333, 4.08333, 3.50000, 2.91667, 3.72727, 3.33333, 4.41667, 3.75000, 4.08333, 3.16667, 3.50000,  // 4360 fa24
       4.77778, 4.88889, 4.66667, 4.88889, 4.77778, 4.55556, 4.77778, 4.66667, 4.88889, 4.88889, 4.88889, 4.66667, 4.77778,  // 6360 fa24
       3.54545, 3.63636, 4.09091, 4.18182, 3.63636, 3.00000, 4.09091, 3.72727, 4.72727, 3.63636, 4.58333, 3.45455, 3.91667,  // 4370 fa24
       3.83333, 4.08333, 4.58333, 4.83333, 4.16667, 3.91667, 4.08333, 4.00000, 4.83333, 4.75000, 4.58333, 4.00000, 4.50000)  // 6370 fa24

   for i <- x.indices do println (x(i).mean)
   println (x.sum / (7 * 13))

end matrixCalc4

