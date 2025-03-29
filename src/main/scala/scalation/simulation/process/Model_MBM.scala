
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Fri Sep 17 15:22:44 EDT 2021
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Base Model Class for Process Simulation - Method of Batch Means
 */

package scalation
package simulation
package process

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Model_MBM` class maintains a list of components making up the model and
 *  controls the flow of entities (`SimActor`s) through the model, following the
 *  process-interaction world-view.  It maintains a time-ordered priority queue
 *  to activate/re-activate each of the entities.  Each entity (`SimActor`) is
 *  implemented as a Scala `Actor` and may be thought of as running in its own thread.
 *  This derived class replaces the default Method of Independent Replivations with
 *  the Method of Batch Means.
 *  @param name       the name of the simulation model
 *  @param nBatch     the number of batches to run
 *  @param sizeB      the size of each batch
 *  @param animating  whether to animate the model
 *  @param aniRatio   the ratio of simulation speed vs. animation speed
 *  @param full       generate a full report with both sample and time-persistent statistics
 */
class Model_MBM (name: String, val nBatch: Int = 10, sizeB: Int = 100,
                 animating: Boolean = false, aniRatio: Double = 1.0, full: Boolean = true)
      extends Model (name, 1, animating, aniRatio, full):

    private val debug = debugf ("Model_MBM", true)               // debug function

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The model itself is an Actor (not an ordinary `SimActor`) and may be
     *  thought of as the director.  The director iteratively manages the clock
     *  and the agenda of actors until the simulation flag becomes false
     *  or the agenda (priority queue) becomes empty.
     *  Replaces replications with batches.
     */
    override def act (): Unit =
        debug ("act", s"starts MBM model for $nBatch batches at $_clock")
        log.trace (this, s"starts model for $nBatch batches", null, _clock)
        val maxAct  = nBatch * sizeB
        var bat     = 0
        var flipped = false

        _clock = startTime
        simulating = true
        if animating then display ()                             // turn animation on (true) off (false)

        debug ("act", s"before while nBatch = $nBatch, sizeB = $sizeB, maxAct = $maxAct")

        while numActors <= maxAct && ! agenda.isEmpty do         // LOOP THROUGH BATCHES

//          debug ("act", s"start while with numActors = $numActors")
            log.trace (this, s"starts batch $bat", null, _clock)

            if (numActors + 1) % sizeB == 0 then flipped = true
            if flipped && numActors % sizeB == 0 then
                flipped = false
                debug ("act", s"ends batch $bat $_clock")
                log.trace (this, s"ends batch $bat", null, _clock)
                bat += 1
                println (s"before resetStats bat = $bat")
                resetStats (bat, nBatch)                         // reset and aggregate statistics
                println ("after resetStats")
            end if
            _theActor = agenda.dequeue ()                        // next from priority queue
            _clock    = _theActor.actTime                        // advance the time
//          debug ("act", s"resumes $_theActor $_clock")
            log.trace (this, "resumes", _theActor, _clock)
            yyield (_theActor)                                   // director yields to actor

        end while

        debug ("act", s"ends last batch $bat at $_clock")
        log.trace (this, s"ends last batch $bat", null, _clock)
        simulating = false
        fini (bat)                                               // post-run results
        cleanup ()
        reportV (false)                                          // set to true to see all batch means
        println (s"coroutine counts = $counts")
        log.trace (this, "terminates model", null, _clock)
        hasFinished ()                                           // signal via semaphore that simulation is finished
        yyield (null, true)                                      // yield and terminate the director
    end act

end Model_MBM

