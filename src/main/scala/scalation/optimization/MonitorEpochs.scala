
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sun Jun 25 16:30:31 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Keep Track of the Loss Function for each Epoch
 *
 *  @see     http://papers.ssrn.com/sol3/papers.cfm?abstract_id=2097904
 */

package scalation
package optimization

import scala.collection.mutable.ArrayBuffer

import scalation.mathstat._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MonitorEpochs` trait is used to monitor the loss function over the epochs.
 */
trait MonitorEpochs:

    protected val epochLoss = new ArrayBuffer [Double] () // record each functional value for each epoch

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the loss function for each epoch.
     */
    def lossPerEpoch (): ArrayBuffer [Double] = epochLoss

    def plotLoss (): Unit =
        println ("plotLoss")
        val el = new VectorD (epochLoss.size, epochLoss.toArray)
        new Plot (null, el, null, "loss function vs. epoch")

end MonitorEpochs

