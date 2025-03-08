
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Wed Feb  5 17:41:18 EST 2014
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Locatable Objects Have a Location
 */

package scalation
package simulation

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Locatable` trait provides location information/coordinates for objects
 *  in simulation models (e.g., `SimActor`s).
 */
trait Locatable2:

    /** Where this object is at (its location)
     */
    private var _at: Array [Double] = null

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the current location where this object is currently at.
     */
    final def at: Array [Double] = _at

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the current location of this object.
     *  @param at  the location of this object
     */
    final def at_= (at: Array [Double]): Unit = _at = at

end Locatable2

