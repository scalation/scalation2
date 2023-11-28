
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Tue May 13 16:18:42 EDT 2014
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Type Aliases for Basic Java awt Types
 *
 *  -----------------------------------------------------------------------------
 *  The `scala2d` package defines/redefines Java2D shapes (see `java.awt` and
 *  `java.awt.geom`).  The shapes are divided into five groups based on their
 *  dimensionality as well as their base type (class/trait):
 *  Basic:
 *      `BasicStroke`
 *      `Dimension`
 *      `Shape`
 *      `Graphics`
 *      `Graphics2D`
 *      `RectangularShape`
 *  0D:
 *      `R2 redefines `java.awt.geom.Point2D.Double`
 *  1D:
 *      //`CurvilinearShape` is introduced and the following subtypes are defined
 *      `Line`    extends `Line2D`
 *      `QCurve`  extends `QuadCurve`
 *      `Arrow`   `Line` with an arrowhead
 *      `QArrow`  `QuadCurve` with an arrowhead
 *  1-2D:
 *      `Path redefines `java.awt.geom.Path2D.Double` with subtypes
 *      `Polygon`
 *      `Triangle`
 *      `Quad`
 *      `Pentagon`
 *      `Hexagon`
 *      `Octagon`
 *  2D:
 *      `RectangularShape redefines `java.awt.geom.RectangularShape` with subtypes
 *      `Arc`
 *      `Ellipse`
 *      `Rectangle`
 *      `RoundRectangle`
 *  Note:
 *      `CubicCurve` is currently not supported
 */

package scalation
package scala2d

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Shapes` file provides type aliases for basic Java awt types.
 */

type BasicStroke      = java.awt.BasicStroke
type Graphics         = java.awt.Graphics
type Graphics2D       = java.awt.Graphics2D
type Rectangle2D      = java.awt.geom.Rectangle2D           // FIX - kept due to problem in mathstat.Plot
type RectangularShape = java.awt.geom.RectangularShape
type Shape            = java.awt.Shape

private [scala2d] type Dimension = java.awt.Dimension
private [scala2d] type R2        = java.awt.geom.Point2D.Double

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** `Arc` is a convenience case class for `Arc2D` (a subclass of `RectangularShape`).
 */
case class Arc () extends java.awt.geom.Arc2D.Double

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** `Ellipse` is a convenience case class for Ellipse2D (a subclass of `RectangularShape`).
 */
case class Ellipse () extends java.awt.geom.Ellipse2D.Double

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** `Line` is a convenience case class for `Line2D` (a subclass of `RectangularShape`).
 */
case class Line (x_1: Double, y_1: Double, x_2: Double, y_2: Double)
     extends java.awt.geom.Line2D.Double (x_1, y_1, x_2, y_2)

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** `Path` is a convenience case class for `Path2D.`  Its subtypes (case class `Polygon`,
    etc.) are defined in other files in the `scala2d` package.
 */
case class Path () extends java.awt.geom.Path2D.Double

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** `Rectangle` is a convenience case class for `Rectangle2D `(a subclass of
 *  `RectangularShape`).
 */
case class Rectangle () extends java.awt.geom.Rectangle2D.Double

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** `RoundRectangle` is a convenience case class for `RoundRectangle2D` (a subclass of
 *  `RectangularShape`).
 */
case class RoundRectangle () extends java.awt.geom.RoundRectangle2D.Double


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `lineTest` main function tests drawing a line.
 *  > runMain scalation.scala2d.lineTest
 */
@main def lineTest (): Unit =

    import Colors._

    banner ("Running LineTest")

    val p1 = new R2 (200, 200)
    val p2 = new R2 (400, 100)

    val line1 = Line (100, 100, 300, 300)
    val line2 = Line (p1.x, p1.y, p2.x, p2.y)

    class Canvas extends Panel:

        setBackground (white)

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /** Paint the components into the canvas (drawing panel).
         *  @param gr  low-resolution graphics environment
         */
        override def paintComponent (gr: Graphics): Unit =
            super.paintComponent (gr)
            val g2d = gr.asInstanceOf [Graphics2D]            // use hi-resolution
            g2d.setPaint (red);  g2d.draw (line1)
            g2d.setPaint (blue); g2d.draw (line2)
        end paintComponent

    end Canvas

    // Put the drawing canvas in the visualization frame

    new VizFrame ("LineTest", new Canvas (), 500, 500)

end lineTest

