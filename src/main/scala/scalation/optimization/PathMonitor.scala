
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @note    Wed Nov 08 13:52:02 EST 2023
 *  @see     LICENSE (MIT style license file).
 *------------------------------------------------------------------------------
 *  Trait to specify the logic needed by classes that monitor a path taken in a
 *  multidimensional space.
 */

// Package definition.
package scalation.optimization

// General imports.
import scala.collection.mutable.ArrayBuffer

// Project imports.
import scalation.mathstat.VectorD

// Trait.
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PathMonitor` trait specifies the logic needed to monitor a single path
 *  taken in a multidimensional graph.
 *
 *  Classes mixing in this trait should call the `clearPath` method before
 *  beginning to monitor a path and then should call the `add2Path` method
 *  whenever a new data point is produced in the path being monitored. After
 *  that, a call to the `getPath` method will return a deep copy of the path
 *  that was monitored throughout the calculations.
 */
trait PathMonitor:
    private val path = ArrayBuffer[VectorD]()       // Path being monitored.

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Adds a new multidimensional point to the path being monitored.
     *
     *  @param x    The data point to be added to the path being monitored.
     */
    def add2Path(x: VectorD): Unit =
        path += x

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Clears the current path being monitored.
     */
    def clearPath(): Unit =
        path.clear()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Returns a deep copy of the data path being monitored.
     *
     *  @return ArrayBuffer[VectorD]    A deep copy of the data path being
     *                                  monitored.
     */
    def getPath: ArrayBuffer[VectorD] =
        path.clone()
end PathMonitor
