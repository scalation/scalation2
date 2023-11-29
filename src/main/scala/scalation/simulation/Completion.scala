
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Fri Sep 17 23:52:26 EDT 2021
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Signalling between Threads/Coroutines 
 */

package scalation
package simulation

import java.util.concurrent.Semaphore

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Completion` trait is used for signalling between threads that the simulation
 *  has finished.  A `Semaphore` from the Java library is used to cause a thread/coroutine
 *  to wait when calling 'acquire' until another thread/coroutine calls 'release'.
 */
trait Completion:

    /** Application models extending the Model class may use this semaphore to wait for results
     */
    private val finished = new Semaphore (0)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Wait for the main simulation Thread/Coroutine to complete.  This can used
     *  used to time execution of models.
     *  @see apps.process.Bank2
     */
    def waitFinished (): Unit = finished.acquire                // wait on the semaphore

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Signal via the semaphore that the simulation has finished, thereby ending
     *  the wait due to calling the waitFinished method.
     */
    def hasFinished (): Unit = finished.release ()              // release the semaphore

end Completion

