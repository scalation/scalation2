
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Mon Sep  7 15:05:06 EDT 2009
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Source Creates Enitties/SimActors
 */

package scalation
package simulation
package process

import scala.collection.mutable.ListBuffer
import scala.runtime.ScalaRunTime.stringOf
import scala.util.control.Breaks.{break, breakable}

import scalation.animation.CommandType._
import scalation.random.Variate
import scalation.scala2d.Ellipse
import scalation.scala2d.Colors._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Source` class is used to periodically inject entities (`SimActors`) into a
 *  running simulation model.  May act as an arrival generator.  Source is both
 *  a simulation `Component` and special `SimActor` and therefore runs in own thread.
 *  @param name          the name of the source
 *  @param director      the director controlling the model
 *  @param makeEntity    the function to make entities of a specified type
 *  @param esubtype      indicator of the subtype of the entities to be made
 *  @param units         the number of entities to make
 *  @param iArrivalTime  the inter-arrival time distribution
 *  @param loc           the location of the source (x, y, w, h)
 */
class Source (name: String, director: Model, makeEntity: () => SimActor,
              esubtype: Int, units: Int,
              iArrivalTime: Variate, loc: Array [Double])
      extends SimActor (name, director) with Component:

    initStats (name)
    at = loc

    private val debug = debugf ("Source", true)                        // debug function
    
    debug ("Init", s"located at ${stringOf (at)}")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Auxiliary constructor that uses defaults for width 'w' and height 'h'.
     *  @param name          the name of the source
     *  @param director      the director controlling the model
     *  @param makeEntity    the function to make entities of a specified type
     *  @param esubtype      indicator of the subtype of the entities to me made
     *  @param units         the number of entities to make
     *  @param iArrivalTime  the inter-arrival time distribution
     *  @param xy            the (x, y) coordinates for the top-left corner of the source.
     */
    def this (name: String, director: Model, makeEntity: () => SimActor, esubtype: Int,
              units: Int, iArrivalTime: Variate, xy: (Double, Double)) =
        this (name, director, makeEntity, esubtype, units, iArrivalTime,
              Array (xy._1, xy._2, 20.0, 20.0))
    end this

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Display this source as a node on the animation canvas.
     */
    def display (): Unit =
        director.animate (this, CreateNode, limegreen, Ellipse (), at)
    end display

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The `Source`s as special `SimActor` will act over time to make entities
     *  (other `SimActor`s).
     */
    def act (): Unit =
        for rep <- 1 to director.reps do                                     // major loop - replications
            actTime = director.clock                                         // set to model start time

            breakable {
                debug ("act", s"start making $units SimActors")
                for i <- 1 to units do                                       // minor loop - make actors
                    if director.stopped then
                        println (s"Source.act: simulation unexpectedly ended at ${director.clock}")
                        break ()                        // terminate source, simulation ended
                    end if
                    val actor = makeEntity ()                                // make new actor
                    actor.mySource = this                                    // actor's source
                    actor.subtype  = esubtype                                // set the entity subtype 
                    director.numActors += 1                                  // number of actors created by all sources, so far
                    director.log.trace (this, "generates", actor, director.clock)
                    director.animate (actor, CreateToken, randomColor (actor.id), Ellipse (),
                             Array (at(0) + at(2) + RAD / 2.0, at(1) + at(3) / 2.0 - RAD))
                    actor.schedule (0.0)

                    if i < units then
                        val duration = iArrivalTime.gen
                        tally (duration)
                        schedule (duration)
                        yieldToDirector ()                                   // yield and wait duration time units
                    end if
                end for
            } // breakable

            if rep < director.reps then
                director.log.trace (this, "wait for next rep", director, director.clock)
                yieldToDirector ()                                // yield and wait for next replication
            end if
        end for

        director.log.trace (this, "terminates", null, director.clock)
        yieldToDirector (true)                                    // yield and terminate
    end act

end Source


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Source` companion object provides a builder method for sources.
 */
object Source:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a source using defaults for width 'w' and height 'h'.
     *  @param name          the name of the source
     *  @param director      the director controlling the model
     *  @param makeEntity    the function to make entities of a specified type
     *  @param esubtype      indicator of the subtype of the entities to me made
     *  @param units         the number of entities to make
     *  @param iArrivalTime  the inter-arrival time distribution
     *  @param xy            the (x, y) coordinates for the top-left corner of the source.
     */
    def apply (name: String, director: Model, makeEntity: () => SimActor, esubtype: Int, units: Int,
              iArrivalTime: Variate, xy: (Int, Int)): Source =
        new Source (name, director, makeEntity, esubtype, units, iArrivalTime,
                    Array (xy._1.toDouble, xy._2.toDouble, 20.0, 20.0))
    end apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a group of related sources using defaults for width 'w' and height 'h'.
     *  @param director      the director controlling the model
     *  @param makeEntity    the function to make entities of a specified type
     *  @param units         the number of entities to make
     *  @param xy            the (x, y) coordinates for the top-left corner of the reference source.
     *  @param src           repeated source specific info: name, subtype, distribution, offset
     */
    def group (director: Model, makeEntity: () => SimActor, units: Int, xy: (Int, Int),
               src: (String, Int, Variate, (Int, Int))*): List [Source] =
        val sourceGroup = new ListBuffer [Source] ()
        for s <- src do sourceGroup += Source (s._1, director, makeEntity, s._2, units, s._3,
                                              (xy._1 + s._4._1, xy._2 + s._4._2))
        sourceGroup.toList
    end group

end Source


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `sourceTest` main function tests the `Source` class by generating several Car objects.
 *  > runMain scalation.simulation.process.sourceTest
 */
@main def sourceTest (): Unit =

    import scalation.random.Uniform

    object CarModel extends Model ("CarModel"):

        val maker = Source ("maker", CarModel, () => Car (), 0, 5, Uniform (2000, 5000), (100, 200))

        addComponent (maker)

        case class Car () extends SimActor ("c", CarModel):
            def act (): Unit = println ("act")
        end Car

    end CarModel

    CarModel.simulate ()
    CarModel.waitFinished ()
    Model.shutdown ()

end sourceTest

