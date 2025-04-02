
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @note    Wed Nov 08 13:52:02 EST 2023
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Logic Needed by Classes that Monitor a Path Multidimensional Space
 */

package scalation
package optimization

import scala.collection.mutable.ArrayBuffer

import scalation.mathstat.VectorD

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

    private val path = ArrayBuffer [VectorD] ()              // path being monitored

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Adds a new multidimensional point to the path being monitored.
     *
     *  @param x  the data point to be added to the path being monitored.
     */
    def add2Path (x: VectorD): Unit = path += x

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Clears the current path being monitored.
     */
    def clearPath (): Unit = path.clear ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Returns a deep copy of the data path being monitored.
     *
     *  @return  ArrayBuffern [VectorD], a deep copy of the data path being monitored.
     */
    def getPath: ArrayBuffer [VectorD] = path.clone ()

end PathMonitor

