
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sun Feb 27 19:46:59 EST 2022
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   
 */

package scalation
package modeling

import scala.collection.mutable.ArrayBuffer

import scalation.mathstat._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MonitorLoss` trait provides methods to track the converegence of the
 *  of optimization algorithms based on the value of the loss function.
 */
trait MonitorLoss:

    private val losses = ArrayBuffer [Double] ()                       // hold values for loss function

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Collect the next value for the loss function.
     *  @param loss  the value of the loss function
     */
    def collectLoss (loss: Double): Unit = losses += loss

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Plot the loss function versus the epoch.
     *  @param optName  the name of optimization algorithm (alt. name of network)
     */
    def plotLoss (optName: String): Unit =
        val loss  = VectorD (losses)
        println (s"loss = $loss")
        val epoch = VectorD.range (1, loss.dim+1)
        new Plot (epoch, loss, null, "loss vs epoch $optName")
    end plotLoss

end MonitorLoss

