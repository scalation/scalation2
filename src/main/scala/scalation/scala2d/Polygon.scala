
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sun Nov 28 01:47:06 EST 2021
 *  @see     LICENSE (MIT style license file). 
 *
 *  @title   Defines Several Common Types of Polygons
 */

package scalation
package scala2d

import scala.math.{cos, Pi, sin}

import scalation.mathstat.VectorD

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Useful trig constants.
 */
object TrigConstants:

    val cs45 = cos (Pi / 4)
    val sn45 = cs45
    val cs54 = cos (3 * Pi / 10)
    val sn54 = sin (3 * Pi / 10)
    val cs60 = cos (Pi / 3)
    val sn60 = sin (Pi / 3)
    val cs72 = cos (2 * Pi / 5)
    val sn72 = sin (2 * Pi / 5)

end TrigConstants

import Colors._
import TrigConstants._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Return the top-left coordinates (x, y) of the polygon's bounding box.
 *  @param bb  the polygon's bounding box
 */
def topLeft (bb: Rectangle2D): (Double, Double) = (bb.getMinX, bb.getMinY)


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Polygon` class adds an abstract setFrame method to `java.awt.Polygon` for compatibility
 *  with `RectangularShape`.
 */
abstract class Polygon () extends java.awt.Polygon ():

    def setFrame (x: Double, y: Double, w: Double, h: Double): Unit

end Polygon


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RectPolyShape` type is a union type for rectangular shapes and polygons.
 */
type RectPolyShape = RectangularShape | Polygon

extension (s: RectPolyShape)
    def getCenterX (): Double =
        s match
        case _: RectangularShape => s.asInstanceOf [RectangularShape].getCenterX ()
        case _: Polygon => s.asInstanceOf [Polygon].getBounds2D ().getCenterX ()
    end getCenterX
    def getCenterY (): Double =
        s match
        case _: RectangularShape => s.asInstanceOf [RectangularShape].getCenterY ()
        case _: Polygon => s.asInstanceOf [Polygon].getBounds2D ().getCenterY ()
    end getCenterY
    def getWidth (): Double =
        s match
        case _: RectangularShape => s.asInstanceOf [RectangularShape].getWidth ()
        case _: Polygon => s.asInstanceOf [Polygon].getBounds2D ().getWidth ()
    end getWidth
    def getHeight (): Double =
        s match
        case _: RectangularShape => s.asInstanceOf [RectangularShape].getHeight ()
        case _: Polygon => s.asInstanceOf [Polygon].getBounds2D ().getHeight ()
    end getHeight
    def setFrame (x: Double, y: Double, w: Double, h: Double): Unit =
        s match
        case _: RectangularShape => s.asInstanceOf [RectangularShape].setFrame (x, y, w, h)
        case _: Polygon => s.asInstanceOf [Polygon].setFrame (x, y, w, h)
    end setFrame


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Triangle` class provides 3-sided polygons.
 */
case class Triangle () extends Polygon ():

    private val flaw = flawf ("Triangle")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the points/vertices for the triangle. 
     *  @param x  the x-coordinates
     *  @param y  the y-coordinates
     */
    def addPoints (x: VectorD, y: VectorD): Unit =
        if x.dim != 3 || y.dim != 3 then flaw ("addPoints", "need exactly 3 vertices to make a triangle")
        for i <- x.indices do addPoint (x(i).toInt, y(i).toInt)
    end addPoints

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the frame for the right triangle.
     *  @param tx  the top-left x-coordinate
     *  @param ty  the top-left y-coordinate
     *  @param w   the width of the triangle
     *  @param h   the height of the triangle
     */
    def setFrame (tx: Double, ty: Double, w: Double, h: Double): Unit =
        val x = VectorD (0, 0, w) + tx
        val y = VectorD (0, h, h) + ty
        addPoints (x, y)
    end setFrame

end Triangle


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Quad` class provides 4-sided polygons.
 */
case class Quad () extends Polygon ():

    private val flaw = flawf ("Quad")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the points/vertices for the quad. 
     *  @param x  the x-coordinates
     *  @param y  the y-coordinates
     */
    def addPoints (x: VectorD, y: VectorD): Unit =
        if x.dim != 4 || y.dim != 4 then flaw ("addPoints", "need exactly 4 vertices to make a quad")
        for i <- x.indices do addPoint (x(i).toInt, y(i).toInt)
    end addPoints

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the frame for the square quad.
     *  @param tx  the top-left x-coordinate
     *  @param ty  the top-left y-coordinate
     *  @param w   the width of the quad
     *  @param h   the height of the quad
     */
    def setFrame (tx: Double, ty: Double, w: Double, h: Double): Unit =
        val x = VectorD (0, 0, w, w) + tx
        val y = VectorD (0, h, h, 0) + ty
        addPoints (x, y)
    end setFrame

end Quad


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Pentagon` class provides 5-sided polygons.
 */
case class Pentagon () extends Polygon ():

    private val flaw = flawf ("Pentagon")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the points/vertices for the pentagon. 
     *  @param x  the x-coordinates
     *  @param y  the y-coordinates
     */
    def addPoints (x: VectorD, y: VectorD): Unit =
        if x.dim != 5 || y.dim != 5 then flaw ("addPoints", "need exactly 5 vertices to make a pentagon")
        for i <- x.indices do addPoint (x(i).toInt, y(i).toInt)
    end addPoints

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the frame for the pentagon.
     *  @param tx  the top-left x-coordinate
     *  @param ty  the top-left y-coordinate
     *  @param ww  the width of the pentagon (1+2cos72)w
     *  @param hh  the height of the pentagon (1+cos54)h
     */
    def setFrame (tx: Double, ty: Double, ww: Double, hh: Double): Unit =
        val w = ww / (1+2*cs72)                                                  // horizontal side
        val h = hh / (1+cs54)                                                    // vertical side
        val x = VectorD (cs72*w, (1+cs72)*w, ww, (.5+cs72)*w, 0) + tx
        val y = VectorD (0, 0, sn72*h, hh, sn72 *h)              + ty
        addPoints (x, y)
    end setFrame

end Pentagon


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Hexagon` class provides 6-sided polygons.
 */
case class Hexagon () extends Polygon ():

    private val flaw = flawf ("Hexagon")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the points/vertices for the quad.
     *  @param x  the x-coordinates
     *  @param y  the y-coordinates
     */
    def addPoints (x: VectorD, y: VectorD): Unit =
        if x.dim != 6 || y.dim != 6 then flaw ("addPoints", "need exactly 6 vertices to make a hexagon")
        for i <- x.indices do addPoint (x(i).toInt, y(i).toInt)
    end addPoints

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the frame for the hexagon.
     *  @param tx  the top-left x-coordinate
     *  @param ty  the top-left y-coordinate
     *  @param ww  the width of the hexagon (1+2cos60)
     *  @param hh  the height of the hexagon (2sin60)
     */
    def setFrame (tx: Double, ty: Double, ww: Double, hh: Double): Unit =
        val w = ww / (1+2*cs60)                                                  // horizontal side
        val h = hh / (2*sn60)                                                    // vertical side
        val x = VectorD (cs60*w, (1+cs60)*w, ww, (1+cs60)*w, cs60*w, 0) + tx
        val y = VectorD (0, 0, sn60*h, hh, hh, sn60*h)                  + ty
        addPoints (x, y)
    end setFrame

end Hexagon


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Octagon` class provides 8-sided polygons.
 */
case class Octagon () extends Polygon ():

    private val flaw = flawf ("Octagon")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the points/vertices for the quad.
     *  @param x  the x-coordinates
     *  @param y  the y-coordinates
     */
    def addPoints (x: VectorD, y: VectorD): Unit =
        if x.dim != 8 || y.dim != 8 then flaw ("addPoints", "need exactly 8 vertices to make a hexagon")
        for i <- x.indices do addPoint (x(i).toInt, y(i).toInt)
    end addPoints

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the frame for the octagon.
     *  @param tx  the top-left x-coordinate
     *  @param ty  the top-left y-coordinate
     *  @param ww  the width of the octagon (1+2cos45)w
     *  @param hh  the height of the octagon (1+2sin45)h
     */
    def setFrame (tx: Double, ty: Double, ww: Double, hh: Double): Unit =
        val w = ww / (1+2*cs45)                                                  // horizontal side
        val h = hh / (1+2*sn45)                                                  // vertical side
        val x = VectorD (cs45*w, (1+cs45)*w, ww, ww, (1+cs45)*w, cs45*w, 0, 0) + tx
        val y = VectorD (0, 0, sn45*h, (1+sn45)*h, hh, hh, (1+sn45)*h, sn45*h) + ty
        addPoints (x, y)
    end setFrame

end Octagon


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `polygonTest` main function tests the `Triangle`, `Quad`, `Pentagon`, `Hexagon`
 *  and `Octagon` classes, testing the addPoints method.
 *  > runMain scalation.scala2d.polygonTest
 */
@main def polygonTest (): Unit =

    banner ("Running polygonTest")

    val len = 80
    val dot = Ellipse ()

    val triangle = Triangle ()
    triangle.addPoints (VectorD (100, 100, 100+len),
                        VectorD (100, 100+len, 100+len))
    val triangleXY = topLeft (triangle.getBounds2D)

    val square = Quad ()
    square.addPoints (VectorD (400, 400+len, 400+len, 400),
                      VectorD (100, 100, 100+len, 100+len))
    val squareXY = topLeft (square.getBounds2D)

    val parogram = Quad ()
    parogram.addPoints (VectorD (100, 100+len, 100+2*len, 100+len),
                        VectorD (350, 350, 350+len, 350+len)) 
    val parogramXY = topLeft (parogram.getBounds2D)

    val pentagon = Pentagon ()
    pentagon.addPoints (VectorD (400+cs72*len, 400+(1+cs72)*len, 400+(1+2*cs72)*len, 400+(.5+cs72)*len,  400),
                        VectorD (350, 350, 350+sn72*len, 350+(1+2*cs72)*len, 350+sn72*len))
    val pentagonXY = topLeft (pentagon.getBounds2D)

    val hexagon = Hexagon ()
    hexagon.addPoints (VectorD (100+cs60*len, 100+(1+cs60)*len, 100+(1+2*cs60)*len, 100+(1+cs60)*len, 100+cs60*len, 100),
                       VectorD (600, 600, 600+sn60*len, 600+2*sn60*len, 600+2*sn60*len, 600+sn60*len))
    val hexagonXY = topLeft (hexagon.getBounds2D)

    val octagon = Octagon  ()
    octagon.addPoints (VectorD (400+cs45*len, 400+(1+cs45)*len, 400+(1+2*cs45)*len, 400+(1+2*cs45)*len,
                                400+(1+cs45)*len, 400+cs45*len, 400, 400),
                       VectorD (600, 600, 600+sn45*len, 600+(1+sn45)*len,
                                600+(1+2*sn45)*len, 600+(1+2*sn45)*len, 600+(1+sn45)*len, 600+sn45*len))
    val octagonXY  = topLeft (octagon.getBounds2D)

    class Canvas extends Panel:

        setBackground (white)

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /** Paint the components into the canvas (drawing panel).
         *  @param gr  low-resolution graphics environment
         */
        override def paintComponent (gr: Graphics): Unit =
            super.paintComponent (gr)
            val g2d = gr.asInstanceOf [Graphics2D]            // use hi-resolution
            g2d.setPaint (orange);  g2d.fill (triangle)
            g2d.setPaint (black);   dot.setFrame (triangleXY._1, triangleXY._2, 5, 5); g2d.fill (dot)
            g2d.setPaint (green);   g2d.fill (square)
            g2d.setPaint (black);   dot.setFrame (squareXY._1, squareXY._2, 5, 5); g2d.fill (dot)
            g2d.setPaint (blue);    g2d.fill (parogram)
            g2d.setPaint (black);   dot.setFrame (parogramXY._1, parogramXY._2, 5, 5); g2d.fill (dot)
            g2d.setPaint (yellow);  g2d.fill (pentagon)
            g2d.setPaint (black);   dot.setFrame (pentagonXY._1, pentagonXY._2, 5, 5); g2d.fill (dot)
            g2d.setPaint (cyan);    g2d.fill (hexagon)
            g2d.setPaint (black);   dot.setFrame (hexagonXY._1, hexagonXY._2, 5, 5); g2d.fill (dot)
            g2d.setPaint (magenta); g2d.fill (octagon)
            g2d.setPaint (black);   dot.setFrame (octagonXY._1, octagonXY._2, 5, 5); g2d.fill (dot)
        end paintComponent

    end Canvas

    // Put the drawing canvas in the visualization frame

    new VizFrame ("polygonTest", new Canvas (), 700, 1100)

end polygonTest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `polygonTest2` main function tests the `Triangle`, `Quad`, `Pentagon`, `Hexagon` 
 *  and `Octagon` classes, testing the setFrame method.
 *  > runMain scalation.scala2d.polygonTest2
 */
@main def polygonTest2 (): Unit =

    banner ("Running polygonTest2")

    val len = 80
    val dot = Ellipse ()

    val triangle = Triangle ()
    triangle.setFrame (100, 100, len, len)
    val triangleXY = topLeft (triangle.getBounds2D)

    val square = Quad ()
    square.setFrame (400, 100, len, len)
    val squareXY = topLeft (square.getBounds2D)

    val pentagon = Pentagon ()
    pentagon.setFrame (100, 350, len, len)
    val pentagonXY = topLeft (pentagon.getBounds2D)

    val hexagon = Hexagon ()
    hexagon.setFrame (400, 350, len, len)
    val hexagonXY = topLeft (hexagon.getBounds2D)

    val octagon = Octagon ()
    octagon.setFrame (100, 600, len, len)
    val octagonXY = topLeft (octagon.getBounds2D)

    val stopSignNS = Octagon ()                      // stop sign for North-South traffic
    stopSignNS.setFrame (400, 600, len, .7*len)
    val stopSignNSXY = topLeft (stopSignNS.getBounds2D)

    val stopSignEW = Octagon ()                      // stop sign for East-West traffic
    stopSignEW.setFrame (400, 850, .7*len, len)
    val stopSignEWXY = topLeft (stopSignEW.getBounds2D)

    class Canvas extends Panel:

        setBackground (white)

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /** Paint the components into the canvas (drawing panel).
         *  @param gr  low-resolution graphics environment
         */
        override def paintComponent (gr: Graphics): Unit =
            super.paintComponent (gr)
            val g2d = gr.asInstanceOf [Graphics2D]            // use hi-resolution
            g2d.setPaint (orange);  g2d.fill (triangle)
            g2d.setPaint (black);   dot.setFrame (triangleXY._1, triangleXY._2, 5, 5); g2d.fill (dot)
            g2d.setPaint (purple);  g2d.fill (square)
            g2d.setPaint (black);   dot.setFrame (squareXY._1, squareXY._2, 5, 5); g2d.fill (dot)
            g2d.setPaint (yellow);  g2d.fill (pentagon)
            g2d.setPaint (black);   dot.setFrame (pentagonXY._1, pentagonXY._2, 5, 5); g2d.fill (dot)
            g2d.setPaint (cyan);    g2d.fill (hexagon)
            g2d.setPaint (black);   dot.setFrame (hexagonXY._1, hexagonXY._2, 5, 5); g2d.fill (dot)
            g2d.setPaint (magenta); g2d.fill (octagon)
            g2d.setPaint (black);   dot.setFrame (octagonXY._1, octagonXY._2, 5, 5); g2d.fill (dot)
            g2d.setPaint (red);     g2d.fill (stopSignNS)
            g2d.setPaint (black);   dot.setFrame (stopSignNSXY._1, stopSignNSXY._2, 5, 5); g2d.fill (dot)
            g2d.setPaint (green);   g2d.fill (stopSignEW)
            g2d.setPaint (black);   dot.setFrame (stopSignEWXY._1, stopSignEWXY._2, 5, 5); g2d.fill (dot)
        end paintComponent

    end Canvas

    // Put the drawing canvas in the visualization frame

    new VizFrame ("polygonTest2", new Canvas (), 700, 1100)

end polygonTest2

