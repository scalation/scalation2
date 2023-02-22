
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Casey Bowman
 *  @version 2.0
 *  @date    Tue Feb  4 14:56:34 EST 2020
 *  @see     LICENSE (MIT style license file).
 */

package scalation
package simulation
package process
package traffic

val vehicleProps = Map ("τ" -> 1.0,                                 // reaction time
                        "amax" -> 2.0,                              // max acceleration
                        "bmax" -> -1.5,                             // max deceleration
                        "v0"   -> 0.0,                              // starting velocity
                        "vmax" -> 33.528,                           // max velocity
                        "T"    -> 3.0,                              // min time headway
                        "s"    -> 5.0,                              // min distance headway
                        "len"  -> 4.0)                              // length of the vehicle

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The Vehicle class extends the `SimActor` and represents a vehicle on a road.
 *  @param label     the label/name of the vehicle
 *  @param director  the model to which this vehicle belongs
 *  @param prop      the property map governing the motion of the vehicle
 */
abstract class Vehicle (label: String, director: Model,
                        prop: Map [String, Double] = vehicleProps)
         extends SimActor (label, director):

    var t_disp   = 0.0                                              // set initial total displacememt to 0
    var velocity = prop("v0")                                       // set initial velocity to v0

    def τ: Double    = prop("τ")                                    // reaction time
    def amax: Double = prop("amax")                                 // max acceleration
    def bmax: Double = prop("bmax")                                 // max deceleration
    def v0: Double   = prop("v0")                                   // starting velocity
    def vmax: Double = prop("vmax")                                 // max velocity
    def T: Double    = prop("T")                                    // min time headway
    def s: Double    = prop("s")                                    // min distance headway
    def len: Double  = prop("len")                                  // length of the vehicle

end Vehicle

