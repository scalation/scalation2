
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Mon Sep 27 15:03:10 EDT 2021
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Temporal Objects are Time-Based
 */

package scalation
package database

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Temporal` trait provides time coordinates that are temporally ordered.
 *  @see `scalation.TimeNum`
 *  @param time  the time coordinate for the temporal object
 */
trait Temporal (var time: Double)
      extends Ordered [Temporal]:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compare two temporal objects based on their time coordinate.
     *  @param other  the other item to compare with this item
     */
//  def compare (other: Temporal): Int = time compare other.time
    def compare (other: Temporal): Int = other.time compare time     // for HPF - add wrapper class

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Rescale from actual-time (e.g., minutes) to animation-time (milliseconds).
     *  @param factor  the time rescaling factor
     */
    def r_rescale (factor: Double): Double = time * factor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert the temporal object to a string.
     */
    override def toString: String = s"Temporal ($time)"

end Temporal

