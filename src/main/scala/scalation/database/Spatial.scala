
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Mon Sep 27 15:03:10 EDT 2021
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Spatial Objects Positioned in Space
 */

package scalation
package database

import scalation.mathstat.VectorD

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Spatial` trait provides spatial coordinates that are spatially partially ordered.
 *  @param pos  the spatial Euclidean coordinate vector for the spatial object
 */
trait Spatial (val pos: VectorD)
      extends PartiallyOrdered [Spatial]:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compare two spatial objects based on their space coordinates.
     *  @param other  the other item to compare with this item
     */
    def tryCompareTo [B >: Spatial: AsPartiallyOrdered] (other: B): Option [Int] =
        pos tryCompareTo other.asInstanceOf [Spatial].pos
    end tryCompareTo

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Rescale from actual-position (e.g., world-coordinates) to animation-position
     *  (screen-coordinates).
     *  @param factor  the space rescaling factor
     */
    def s_rescale (factor: Double): VectorD = pos * factor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert the spatial object to a string.
     */
    override def toString: String = s"Spatial ($pos)"

end Spatial

