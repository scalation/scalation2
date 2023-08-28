
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Thu Jun 17 19:29:23 EDT 2021
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Matrix Calculations
 */

package scalation
package mathstat

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Read a matrix from a CSV file and perform calculations, e.g. grade calculations.
 *  > runMain scalation.mathstat.matrixCalc
 */
@main def matrixCalc (): Unit =

//  val csvFile = "/Users/nancymiller/jam/courses/sp23_4360/scores_4360_2023.csv"
    val csvFile = "/Users/nancymiller/jam/courses/su23_4370/ex/su23/scores_su23.csv"

    val x = MatrixD.load (csvFile, 1, 2, fullPath = true)            // skip 1 row and 2 columns

    println (s"x = $x")

    val n = x.dim2 - 3                                               // last three colums should be empty
    val w = x(0)(0 until n)                                          // weights for grades
    println (s"total weight = ${w.sum}")                             // total weight
    for i <- 1 until x.dim do x(i, n) = w dot x(i)(0 until n)        // weighted total
    println (s"new x = $x")                                          // upadted matrix

end matrixCalc


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Custom calculation
 *  > runMain scalation.mathstat.matrixCalc2
 */
@main def matrixCalc2 (): Unit =

   val x1 = VectorD (81,93,80,100,100,95,77,94)           // original   +1 on Exam I
   val x2 = VectorD (80,93,80,120,100,95,77,94)           // corrected +20 on Homework

   val w  = VectorD (2,2,2.5,0.25,0.25,0.75,0.75,1.5)

   println (s"w * x1 = ${w dot x1}")                      // 868
   println (s"w * x2 = ${w dot x2}")                      // 871

end matrixCalc2

