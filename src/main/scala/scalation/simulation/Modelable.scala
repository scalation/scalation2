
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Mon Oct 10 11:42:43 EDT 2011
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Common Capabilities for Simulation Models
 */

package scalation
package simulation

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Modelable` trait defines what is common to simulation models supported
 *  in ScalaTion.
 */
trait Modelable:

    /** The clock that keep track of the current simulation time
     */
    protected var _clock = 0.0

    /** Simulation execution/termination flag
     */
    protected var simulating = false

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the current value of the director's clock.
     */
    def clock = _clock

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Run the simulation beginning with 'startTime' and continuing until a
     *  stopping rule evaluates to true.
     *  @param startTime  the start time of the simulation
     */
    def simulate (startTime: Double): Unit

end Modelable

