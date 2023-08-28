
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sun Jan 10 17:08:32 EST 2010
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Common Tranforms for Rectangular Shapes
 */

package scalation
package scala2d

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Transform` trait provides a simple technique for transforming
 *  (translation, scaling and rotation) rectangular shapes.
 */
trait Transform:

    private val flaw = flawf ("Transform")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Move/translate the shape to location (x, y).
     *  @param shape  the shape/object to move
     *  @param x      the x-coordinate
     *  @param y      the y-coordinate
     */
//    def move (shape: RectangularShape, x: Double, y: Double): Unit =
    def move (shape: RectPolyShape, x: Double, y: Double): Unit =
        shape.setFrame (x, y, shape.getWidth (), shape.getHeight ())
    end move

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Move/translate the shape to location (x, y).
     *  @param shape  the shape/object to move
     *  @param p      the point (x, y)-coordinates
     */
//    def move (shape: RectangularShape, p: Array [Double]): Unit =
    def move (shape: RectPolyShape, p: Array [Double]): Unit =
        if p.length != 2 then flaw ("move", "p array must be of size 2")
        else shape.setFrame (p(0), p(1), shape.getWidth (), shape.getHeight ())
    end move

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Scale/resize the shape to the new width and height parameters.
     *  @param shape  the shape/object to scale (change size)
     *  @param w      the width
     *  @param h      the height
     */
//    def scale (shape: RectangularShape, w: Double, h: Double): Unit =
    def scale (shape: RectPolyShape, w: Double, h: Double): Unit =
        shape.setFrame (shape.getBounds2D.getX, shape.getBounds2D.getY, w, h)
    end scale

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Scale/resize the shape to the new width and height parameters.
     *  @param shape  the shape/object to scale (change size)
     *  @param p      the point (w, h) parameters
     */
//    def scale (shape: RectangularShape, p: Array [Double]): Unit =
    def scale (shape: RectPolyShape, p: Array [Double]): Unit =
        if p.length != 2 then flaw ("scale", "p array must be of size 2")
        else shape.setFrame (shape.getBounds2D.getX, shape.getBounds2D.getY, p(0), p(1))
    end scale

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Rotate the shape by theta radians.
     *  @param shape  the shape/object to rotate
     *  @param theta   the rotation angle in radians
     */
    def rotate (shape: RectangularShape, theta: Double): Unit = ???      // FIX

end Transform

