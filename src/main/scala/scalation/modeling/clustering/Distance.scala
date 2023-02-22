
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Hao Peng, John Miler
 *  @version 2.0
 *  @date    Wed Nov 4 12:27:00 EDT 2017
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Top-level Function to Compute Distance between Points
 */

package scalation
package modeling
package clustering

import scalation.mathstat.VectorD

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Compute a distance metric (e.g., distance squared) between vectors/points
 *  x and z.  Override this methods to use a different metric, e.g.,
 *     norm   - the Euclidean distance, 2-norm
 *     norm1  - the Manhattan distance, 1-norm
 *  Currently uses squared Euclidean norm used for efficiency, may use other norms.
 *  @param x  the first vector/point
 *  @param z  the second vector/point
 */
inline def dist (x: VectorD, z: VectorD): Double = (x - z).normSq

