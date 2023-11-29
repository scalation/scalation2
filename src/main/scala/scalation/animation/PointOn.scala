
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Yulong Wang
 *  @version 2.0
 *  @date    Sun May 28 19:05:16 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Find Points where Lines Intersect Shapes
 */

package scalation
package animation

import scalation.mathstat.VectorD

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/*  The `pointOnRect` top level method finds where a line intersects a rectangle.
 *  Translated from
 *
 *  @see https://stackoverflow.com/questions/1585525/how-to-find-the-intersection-point-between-a-line-and-a-rectangle
 *
 *  "Finds the intersection point between the rectangle with parallel sides to the x and y axes
 *   the half-line pointing towards (x,y) originating from the middle of the rectangle
 *
 *   Note: the function works given min[XY] <= max[XY], even though minY may not be the "top" of the rectangle
 *   because the coordinate system is flipped.  Note: if the input is inside the rectangle,
 *   the line segment wouldn't have an intersection with the rectangle, but the projected half-line does.
 *   Warning: passing in the middle of the rectangle will return the midpoint itself there are infinitely
 *   many half-lines projected in all directions, so let's just shortcut to midpoint (GIGO)."
 *
 *  @author TWiStErRob
 *  @licence Dual CC0/WTFPL/Unlicence, whatever floats your boat
 *  @see <a href="http://stackoverflow.com/a/31254199/253468">source</a>
 *  @see <a href="http://stackoverflow.com/a/18292964/253468">based on</a>
 *
 *  @param x     the x coordinate of point to build the half-line from
 *  @param y     the y coordinate of point to build the half-line from
 *  @param minX  the the "left" side of the rectangle
 *  @param minY  the the "top" side of the rectangle
 *  @param maxX  the the "right" side of the rectangle
 *  @param maxY  the the "bottom" side of the rectangle
 *  @return  an object with x and y members for the intersection
 *  @throws  if validate == true and (x,y) is inside the rectangle
 */
def pointOnRect (x: Double, y: Double, minX: Double, minY: Double, maxX: Double, maxY: Double): VectorD =
    // assert minX <= maxX
    // assert minY <= maxY

    if (minX < x && x < maxX) && (minY < y && y < maxY) then
        println (s"Point ($x, $y) cannot be inside rectangle: ($minX, $minY) - ($maxX, $maxY)")
    val midX = (minX + maxX) / 2
    val midY = (minY + maxY) / 2
    // if (midX - x == 0) -> m == ±Inf -> minYx/maxYx == x (because value / ±Inf = ±0)
    val m = (midY - y) / (midX - x)

    if x <= midX then                                                     // check "left" side
        val minXy = m * (minX - x) + y
        if minY <= minXy && minXy <= maxY then return VectorD (minX, minXy)

    if x >= midX then                                                     // check "right" side
        val maxXy = m * (maxX - x) + y
        if minY <= maxXy && maxXy <= maxY then return VectorD (maxX, maxXy)

    if y <= midY then                                                     // check "top" side
        val minYx = (minY - y) / m + x
        if minX <= minYx && minYx <= maxX then return VectorD (minYx, minY)

    if y >= midY then                                                     // check "bottom" side
        val maxYx = (maxY - y) / m + x
        if minX <= maxYx && maxYx <= maxX then return VectorD (maxYx, maxY)

    // edge case when finding midpoint intersection: m = 0/0 = NaN
    if x == midX && y == midY then return VectorD (x, y)

    // should never happen
    println (s"Cannot find intersection for ($x, $y) cannot be inside rectangle: ($minX, $minY) - ($maxX, $maxY)")
    VectorD (0, 0)

end pointOnRect

