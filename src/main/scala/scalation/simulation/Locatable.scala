
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
 *  in simulation models (e.g., `Component`s).
 */
trait Locatable:

    /** The flaw function
     */
    private val flaw = flawf ("Locatable")

    /** Where this object is at (its location)
     */
    private var _at: Array [Double] = null

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the location where this object is currently at.
     */
    def at: Array [Double] = _at

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the location of this object.
     *  @param at  the location of this object
     */
    def at_= (at: Array [Double]): Unit =
        if _at == null && at != null then _at = at
        else flaw ("at_=", "location may only be set once")
    end at_=

end Locatable

