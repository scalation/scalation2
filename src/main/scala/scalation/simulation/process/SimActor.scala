
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Mon Sep  7 15:05:06 EDT 2009
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Active Entity/Simulation Actor
 */

package scalation
package simulation
package process

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SimActor` abstract class represents entities that are active in the model.
 *  The act abstract method, which specifies entity behavior, must be defined
 *  for each subclass.  Each `SimActor` extends ScalaTion's `Coroutine` class and
 *  may be  roughly thought of as running in its own thread.
 *  @param label     the label/name of the entity (`SimActor`)
 *  @param director  the director controlling the model
 *  @param prop      the properties (Map) for this actor, e.g., speed, color, weight
 */
abstract class SimActor (label: String, director: Model,
                         val prop: Map [String, Double] = null)
         extends Coroutine (label) with Temporal with Ordered [SimActor] with Locatable:

    name = label                // set the name for this entity (`SimActor`)

    /** The time at which this entity (`SimActor`) arrived
     */
    var arrivalT = director.clock

    /** The indicator of subtype of this entity (`SimActor`), e.g., for turning choices 
     */
    var subtype = 0

    /** The `Source` that created this entity (`SimActor`)
     */
    var mySource: Source = null 

    /** The next `Transport` to move along for this entity (`SimActor`)
     *  Must be specified, for example, before entering a bus
     */
    var nextTransport: Transport = null 

    /** The flaw function
     */
    private val flaw = flawf ("SimActor")

    /** Value of the trajectory along the `QCurve` for this entity (`SimActor`)
     */
    private var _trajectory = 0.0

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the current trajectory (along the `QCurve`) of this `SimActor`.
     */
    def trajectory: Double = _trajectory

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the value of the trajectory along the `QCurve` for this `SimActor`.
     *  @param t  the new trajectory for the `SimActor`
     */
    def trajectory_= (trajectory: Double): Unit =
        if trajectory >= 0.0 then _trajectory = trajectory
        else flaw ("trajectory_=", "the trajectory can not be negative")
    end trajectory_=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compare the activation times of the two actors, this and actor2.
     *  Their activation times are used to order them in the director's agenda
     *  (a time-based priority queue).
     *  @param actor2  the other actor to compare with this
     */
    def compare (actor2: SimActor): Int = actor2.actTime compare actTime

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The abstract method, 'act', is defined in each subclass to provide specific
     *  behavior.
     */
    def act (): Unit

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Schedule a reactivation of this `SimActor` delay time units in the future.
     *  @param delay  the time delay before reactivation
     */
    def schedule (delay: Double): Unit =
        actTime = director.clock + delay
        director.reschedule (this)
    end schedule

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Yield control to the director so the director can take the next action.
     *  @param quit  the flag indicating whether this actor is done
     */
    def yieldToDirector (quit: Boolean = false): Unit =
        director.log.trace (this, "resumes", director, director.clock)
        yyield (director, quit)
    end yieldToDirector

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Show the `SimActor`s full name and activation time.
     */
    override def toString: String = s"SimActor ($me at $actTime)"

end SimActor

