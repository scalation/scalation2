
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Michael Cotterell
 *  @version 2.0
 *  @date    Sat Mar 21 20:34:23 EDT 2015
 *  @see     LICENSE (MIT style license file).
 *
 * Coroutine implementation options: (1) Java Threads,
 * Prototypes for (2) Scala Actors, (3) Akka Actors, (4) Scala Continuations
 * This one uses Java Threads and a Cached Thread Pool
 */

package scalation
package simulation

import java.util.concurrent.{Executors, ExecutorService, Future, Semaphore, ThreadPoolExecutor}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Coroutine` class supports (one-at-a-time) quasi-concurrent programming.
 *  A coroutine runs/acts until it yields control from 'this' to 'that' coroutine.
 *  When resumed, a coroutines continues its execution where it left off.
 *  @param label  the label for the class of coroutines to be created.
 */
abstract class Coroutine (label: String = "cor")
         extends Runnable:

    import Coroutine._

    private val debug   = debugf ("Coroutine", false)      // debug function
    private val _sema   = new Semaphore (0)                // waiting semaphore
    private var started = false                            // whether this coroutine has started

    nCreated += 1
    private val id = label + "." + nCreated
    debug ("init", s"$id waits to be STARTed")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the Coroutine counts.
     */
    def counts: (Int, Int, Int) = (nCreated, nStarted, nTerminated)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Thread's 'run' method delegates to the 'act' method.  Upon interruption
     *  the 'act' method is run again from the beginning.
     */
    def run (): Unit =
        nStarted += 1
        try
            act ()
        catch case ex: InterruptedException => 
            debug ("run", s"INTERRUPTED coroutine $id")
        end try
        nTerminated +=1
        debug ("run", s"TERMINATE coroutine $id")
    end run

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Actor model features the 'act' method, even though threads are used.
     *  This abstract method must be implemented in application models.
     */
    def act (): Unit

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Yield control from 'this' to 'that' coroutine.
     *  @param that  the other coroutine to yield control to
     *  @param quit  whether 'this' coroutine is to terminate (true)
     *                                  or wait to be resumed (false)
     */
    def yyield (that: Coroutine, quit: Boolean = false): Unit =
        if that != null then
            if that.started then
                debug ("yyield", s"$id RESUMEs that coroutine ${that.id}")
                that.resume ()
            else
                debug ("yyield", s"$id STARTs that new coroutine ${that.id}")
                that.start ()
            end if
        end if

        if quit then
            debug ("yyield", s"$id TERMINATEs")
            return
        else
            _sema.acquire ()                // wait until resumed
        end if
    end yyield

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Start this coroutine, i.e., invoke its 'run' -> 'act' method. This
     *  function returns a future.
     */
    def start (): Future [_] =
        started = true
        if pool == null then
            flaw ("start", "the coroutine system must be started using Coroutine.startup; expect undefined behavior.")
        end if
        pool.submit (this)
    end start

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Interrupt this waiting coroutine.
     */
    def interrupt (): Unit =
        Thread.currentThread ().interrupt ()
    end interrupt

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Resume this coroutine.
     */
    private def resume (): Unit = _sema.release ()

end Coroutine


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Coroutine` companion object provides functions to start-up and shutdown
 *  the coroutine system as well as counters for the `Coroutine` class.
 */
object Coroutine:

    private val debug = debugf ("Coroutine", false)        // debug function
    private val flaw  = flawf ("Coroutine")                // flaw function

    private val CORE_THREADS          = 0                  // number of core threads
    private val SHUTDOWN_TIMEOUT      = 60                 // shutdown timeout, in seconds
    private var pool: ExecutorService = null               // thread pool

    private var nCreated = 0                               // number of Coroutines created
    private var nStarted = 0                               // number of Coroutines started
    private var nTerminated = 0                            // number of Coroutines terminated

    startup ()                                             // automatic startup at program start

//  sys.addShutdownHook ({                                 // automatic shutdown at program end
//      pool.shutdown ()
//      pool.shutdownNow ()
//  })

    private def threadPoolExecutor = pool.asInstanceOf [ThreadPoolExecutor]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Start-up the coroutine system.  This function can also set the core 
     *  number of threads for the internal cached thread pool.
     *  @param nCoreThreads  the new core size 
     */
    private def startup (nCoreThreads: Int = CORE_THREADS): Unit =
        if pool == null then
            pool = Executors.newCachedThreadPool ()
            if nCoreThreads != CORE_THREADS then threadPoolExecutor.setCorePoolSize (nCoreThreads)
        else
            flaw ("startup", "coroutine system is already started")
        end if
    end startup

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Shutdown the coroutine thread pool and return the largest number of threads
     *  that have ever simultaneously been in the pool.  Must be called at program
     *  end or application will hang.
     */
    def shutdown (): Int =
        var lps = 0                                     // largest pool size
        debug ("Coroutine", "shutdown")
        if pool != null then
            pool.shutdown ()                            // prevent new submissions to pool
            pool.shutdownNow ()                         // interrupt all threads remaining in pool
            lps  = threadPoolExecutor.getLargestPoolSize
            pool = null
        else flaw ("shutdown", "coroutine system is already shutdown")
        lps
    end shutdown

end Coroutine


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `CoroutineTest` object is used to test the `Coroutine` class.
 *  Should print:
 *    `Cor1`: phase 1
 *    `Cor2`: phase 1
 *    `Cor1`: phase 2
 *    `Cor2`: phase 2
 *  > runMain scalation.simulation.CoroutineTest
 */
object CoroutineTest extends App:     // object since it needs forward reference

    class Cor1 extends Coroutine:

        override def act (): Unit =
            println ("Cor1: phase 1")
            yyield (cor2)
            println ("Cor1: phase 2")
            yyield (cor2, true)
        end act

    end Cor1

    class Cor2 extends Coroutine:

        override def act (): Unit =
            println ("Cor2: phase 1")
            yyield (cor1)
            println ("Cor2: phase 2")
            yyield (null, true)
        end act

    end Cor2

// Coroutine.startup ()

    val cor1 = new Cor1 ()
    val cor2 = new Cor2 ()

    println ("start coroutines")
    cor1.start ()

end CoroutineTest

