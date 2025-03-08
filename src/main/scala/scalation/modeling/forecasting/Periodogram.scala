
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Mon Aug 26 13:42:17 EDT 2024
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Model Tool: Dynamics Time Warping (Periodogram)
 */

package scalation
package modeling
package forecasting

import scalation.mathstat._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Periodogram` class is used to analyze the frequency specturm of a time serioes.
 *  @param y   the first time series vector
 */
class Periodogram (y: VectorD):
 
    private val x = VectorC.fromDoubles (y)
    calculus.FFT.fft (x)
    new Plot (null, x.toDouble, null, "frequency domain signal", lines = true)

end Periodogram

import Example_Covid.loadData_yy

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `periodogramTest` main function tests the `Periodogram` class on real data.
 *  > runMain scalation.modeling.forecasting.periodogramTest
 */
@main def periodogramTest (): Unit =

    val vars = Array ("new_deaths", "icu_patients")
    val yy = loadData_yy (vars)
//  val y  = yy                                                           // full
    val y  = yy(0 until 116)                                              // clip the flat end
    val y0 = y(?, 0)                                                      // column 0
//  val y1 = y(?, 1)                                                      // column 1

    new Periodogram (y0)                                                  // apply Periodogram to columns 0

end periodogramTest

