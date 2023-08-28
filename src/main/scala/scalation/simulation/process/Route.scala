
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sat Jan 25 19:44:16 EST 2014
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Route for Modeling Multi-Lane Pathway
 */

package scalation
package simulation
package process

import scala.math.hypot

import scalation.animation.CommandType._
import scalation.mathstat.VectorD
import scalation.random.Variate
//import scalation.scala2d.QCurve
import scalation.scala2d.Colors._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Route` class provides a multi-lane pathway between two other components.
 *  The components in a `Model` conceptually form a graph in which the edges
 *  are `Transport`s and the nodes are other `Component`s.
 *  A `Route` is a composite component that bundles several `Transport`s.
 *  @param name      the name of the route
 *  @param k         the number of lanes/transports in the route
 *  @param from      the starting component
 *  @param to        the ending component
 *  @param motion    the speed/trip-time to move down the transports in the route
 *  @param isSpeed   whether speed or trip-time is used for motion
 *  @param angle     angle in radians of direction (0 => east, Pi/2 => north, Pi => west, 3Pi/2 => south)
 *  @param bend      the bend or curvature of the route (0 => line)
 */
class Route (name: String, k: Int, val from: Component, val to: Component,
             motion: Variate, isSpeed: Boolean = false, angle: Double = 0.0, bend: Double = 0.0)
      extends Component:

    initComponent (name, Array ())

    private val MID   = k / 2                       // middle lane                
    private val GAP   = 10.0                        // gap between lanes 
    private val delta = calcShift                   // amount of shift in x and y directions

    val lane = Array.ofDim [Transport] (k)
    for i <- lane.indices do
        val radius = (i - (k - 1) / 2.0) * GAP
        val shift = VectorD ((i - (k - 1) / 2.0) * delta(0), (i - (k - 1) / 2.0) * delta(1))
        lane(i) = new Transport (s"${name}_$i", from, to, motion, isSpeed, bend, shift, shift)
        subpart += lane(i)
    end for

    private var _selector: Variate = _               // random variate for determining next direction

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the direction/turn random variate to determine next the direction.
     *  This allows an application model to select the next component.
     *  The decision is delegated to this route's lane(0) transport.
     */
    def selector: Variate = lane(0).selector

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the direction/turn random variate in this route's lane(0) transport.
     *  @param selectorRV  the random variate used to select the direction
     */
    def selector_= (selector: Variate): Unit = lane(0).selector = selector

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Calculate the amount of shift in the x and y directions.
     */
    private def calcShift: VectorD =
        val xdist = from.at(0) - to.at(0)
        val ydist = from.at(1) - to.at(1)
        val hyp   = hypot (xdist, ydist)
        VectorD ((ydist / hyp) * GAP, -(xdist / hyp) * GAP)
    end calcShift

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Give the location of the curve to be its starting point.
     */
    override def at: Array [Double] = lane(0).at
        
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Tell the animation engine to display this `Transport`.
     */
    def display (): Unit =
//      director.animate (this, CreateEdge, blue, QCurve (), from, to, Array (bend))
        for l <- lane do
            director.animate (l, CreateEdge, blue, l.curve, l.from, l.to,
                              Array (l.p1(0), l.p1(1), l.pc(0), l.pc(1), l.p2(0), l.p2(1)))
        end for
    end display

end Route


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `routeTest` main function is used to test the `Route` class, which is a composite
 *  class.  It simulates a two-lane road in one direction.
 *  > runMain scalation.simulation.process.routeTest
 */
@main def routeTest (): Unit =

    import scalation.random.{Bernoulli, Uniform}

    class RouteModel (name: String, nArrivals: Int, iArrivalRV: Variate, moveRV: Variate)
          extends Model (name):

        val rng     = Bernoulli ()
        val onRamp  = new Source ("onRamp", this, () => Car (), 0, nArrivals, iArrivalRV, (100.0, 200.0))
        val offRamp = new Sink ("offRamp", (400.0, 200.0))
        val road    = new Route ("lane", 2, onRamp, offRamp, moveRV, false, 0.25)

        addComponent (onRamp, offRamp, road)

        case class Car () extends SimActor ("c", this):

            def act (): Unit =
                val i = rng.igen             // choose a lane
                road.lane(i).move ()         // move along the lane
                offRamp.leave ()             // exit
            end act

        end Car

    end RouteModel

    val rm = new RouteModel ("road", 10, Uniform (4000, 6000), Uniform (2900, 3100))
    rm.simulate ()
    Model.shutdown ()

end routeTest

