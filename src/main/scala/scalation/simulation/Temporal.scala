
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Mon Sep  7 15:05:06 EDT 2009
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Temporal Object are Identifiable and are Time-Based
 */

package scalation
package simulation

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Temporal` trait adds time (actTime) and temporal ordering to `Identifiable`.
 */
trait Temporal
      extends Identifiable:

    /** The activation time for the temporal object
     */
    var actTime: Double = 0.0

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compare two temporal objects based on their actTime.
     *  @param other  the other item to compare with this item
     */
    def compare (other: Temporal): Int = { actTime compare other.actTime }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert the temporal object to a string.
     */
    override def toString: String = s"Temporal ($me, $actTime)"

end Temporal

