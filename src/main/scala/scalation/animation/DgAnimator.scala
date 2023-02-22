
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Mon Sep 14 14:15:51 EDT 2009
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Animation Engine for Animating Graphs
 */

package scalation
package animation

import java.awt.Font
import java.util.concurrent.ConcurrentLinkedQueue 

import scala.math.round
import scala.util.control.Breaks.{breakable, break}

import scalation.scala2d._
import scalation.scala2d.Colors._

import CommandType._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DgAnimator` class is an animation engine for animating graphs.
 *  For example, it can animate bipartite graphs to animate Petri Nets.
 *  @param _title    the title for the display frame
 *  @param fgColor   the foreground color
 *  @param bgColor   the background color
 *  @param aniRatio  the ratio of simulation speed vs. animation speed
 */
class DgAnimator (_title: String, fgColor: Color = black, bgColor: Color = white,
                  aniRatio: Double = 1.0)
      extends VizFrame (_title, null, 1200, 1000) with Runnable:

    /** The debug function
     */
    private val debug = debugf ("DgAnimator", true)

    /** The flaw function
     */
    private val flaw = flawf ("DgAnimator")

    /** Clock for animation engine
     */
    private var clock = 0.0

    /** Stop time for animation engine
     */
    private var stopTime = 0.0

    /** Graph to animate
     */
    private val graph = new Dgraph ("Animated_Graph")

    /** Animation command processor
     */
    private val ani = new Animator (graph)

    /** Shared queue holding animation commands
     */    
    private val cmdQ = new ConcurrentLinkedQueue [AnimateCommand] ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Save the graphics into an image file.
     *  @param fname  the file name
     */
    def saveImage (fname: String): Unit = writeImage (fname, this)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The canvas Panel is used to place shapes in the drawing region.
     */
    class Canvas extends Panel:
    
        setBackground (bgColor)
        val f = new Font ("Serif", Font.BOLD, 12)

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /** Paint the display panel component.
         *  @param gr  low-resolution graphics environment
         */
        override def paintComponent (gr: Graphics): Unit =
            super.paintComponent (gr)
            val g2d = gr.asInstanceOf [Graphics2D]            // use hi-reso;ution

            //:: Display the animation clock

            g2d.setFont (f)
            g2d.setPaint (fgColor)
            g2d.drawString ("CLOCK = " + "%10.3f".format(clock), 20, getH - 30)

            //:: Display all nodes in graph and tokens bound to these nodes.

            // println ("paintComponent: paint " + graph.nodes.length + " nodes")
            for node <- graph.nodes do
                g2d.setPaint (node.color)
                g2d.fill (node.shape)
                g2d.setPaint (black)
                g2d.draw (node.shape)
                val x = node.shape.getCenterX ().asInstanceOf [Float] - 20.0f
                val y = node.shape.getBounds2D.getMaxY ().asInstanceOf [Float] + 12.0f
                g2d.drawString (node.label, x, y)
                val node_tokens = node.tokens.toList             // copy to avoid ConcurrentModificationException
                for token <- node_tokens do
                    g2d.setPaint (token.color)
                    g2d.fill (token.shape)
                end for
            end for

            //:: Display all edges in graph and tokens bound to these edges.

            // println ("paintComponent: paint " + graph.edges.length + " edges")
            for edge <- graph.edges do
                g2d.setPaint (edge.color)
                g2d.draw (edge.shape)
                val x = edge.shape.getCenterX.asInstanceOf [Float] - 30.0f
                val y = edge.shape.getCenterY.asInstanceOf [Float]
                g2d.drawString (edge.label, x, y)
                val edge_tokens = edge.tokens.toList             // copy to avoid Exception
                for token <- edge_tokens if token.shape.getWidth () > 0.0 do
                    g2d.setPaint (token.color)
                    g2d.fill (token.shape)
                end for
            end for

            //:: Display all free tokens in the graph.

            // println ("paintComponent: paint " + graph.freeTokens.length + " free tokens")
            val free_tokens = graph.freeTokens.toList            // copy to avoid Exception
            for token <- free_tokens if token.shape.getWidth () > 0.0 do
                g2d.setPaint (token.color)
                g2d.fill (token.shape)
            end for
        end paintComponent

    end Canvas

    {
        getContentPane ().add (new Canvas ())
        setVisible (true)
    } // primary constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Invoke the animation command.
     *  @param c  the animation command to invoke
     */
    private def invokeCommand (c: AnimateCommand): Unit =
        if c.action != MoveToken then                                          // remove if to see all move steps
            println (s"DgAnimator.invokeCommand: $c")
        end if

        c.action match
        case CreateNode =>
            ani.createNode (c.eid, c.shape.asInstanceOf [RectPolyShape], c.label, c.primary, c.color, c.pts)
//            ani.createNode (c.eid, c.shape.asInstanceOf [RectangularShape], c.label, c.primary, c.color, c.pts)
        case CreateEdge =>
            ani.createEdge (c.eid, c.shape.asInstanceOf [CurvilinearShape], c.label, c.primary, c.color, c.from_eid, c.to_eid, c.pts)
//            ani.createEdge (c.eid, c.shape.asInstanceOf [QCurve], c.label, c.primary, c.color, c.from_eid, c.to_eid, c.pts)
        case CreateToken =>
            ani.createToken (c.eid, c.shape.asInstanceOf [RectangularShape], c.label, c.primary, c.color, c.from_eid, c.pts)
        case DestroyNode =>
            ani.destroyNode (c.eid)
        case DestroyEdge =>
            ani.destroyEdge (c.eid)
        case DestroyToken =>
            ani.destroyToken (c.eid)
        case MoveNode =>
            ani.moveNode (c.eid, c.pts)
        case MoveToken =>
            ani.moveToken (c.eid, c.pts)
        case MoveToken2Node =>
            ani.moveToken2Node (c.eid, c.from_eid)
        case MoveTokens2Node =>
            ani.moveTokens2Node (c.color, c.from_eid, c.to_eid, c.pts)
        case MoveToken2Edge =>
            ani.moveToken2Edge (c.eid, c.from_eid, 10.0)   // FIX: 10.0?
        case ScaleNode =>
            ani.scaleNode (c.eid, c.pts)
        case ScaleToken =>
            ani.scaleToken (c.eid, c.pts)
        case ScaleTokensAt =>
            ani.scaleTokensAt (c.color, c.from_eid, c.to_eid, c.pts)
        case SetPaintNode =>
            ani.setPaintNode (c.eid, c.color)
        case SetPaintEdge =>
            ani.setPaintEdge (c.eid, c.color)
        case SetPaintToken =>
            ani.setPaintToken (c.eid, c.color)
        case TimeDilation =>
            ani.timeDilation (c.pts)
        end match
    end invokeCommand

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Repeatedly execute animation commands, sleep and repaint.
     */
    def run (): Unit =
        var cmd: AnimateCommand = null
        var when  = 0.0
        var delay = 0L
        var nCmds = 0

        println (s"DgAnimator.run: start animation at time $clock")
        printCommandQueue (clock)

        breakable {
            while clock < stopTime do

                //:: Get the next animation command from the shared queue.

                if cmdQ.isEmpty then
                    println ("DgAnimator.run: command queue is empty")
                    break ()
                end if

                cmd   = cmdQ.poll ()
                when  = cmd.time
                delay = round ((when - clock) * aniRatio * ani.timeDilationFactor)

                //:: Sleep for the given number (delay) of milliseconds.

                Thread.sleep (delay)

                //:: set the animation clock and invoke the animation command

                clock = when
                nCmds += 1
                invokeCommand (cmd)

                //:: Repaint the canvas.

                repaint ()
            end while
        } // breakable

        println (s"DgAnimator.run: end animation at time $clock with $nCmds commands invoked")
    end run

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Start the animation by staring the animation thread.
     *  @param tStart  the animation start time
     *  @param tStop   the animation stop time
     */
    def animate (tStart: Double, tStop: Double): Unit =
        clock    = tStart
        stopTime = tStop
        new Thread (this).start ()
    end animate

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Invoke animation command cmd immediately (useful for testing).
     *  @param cmd  the animation command to invoke
     */
    def invokeNow (cmd: AnimateCommand): Unit =
        invokeCommand (cmd)
        repaint ()
    end invokeNow

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the animation command queue.
     */
    def getCommandQueue: ConcurrentLinkedQueue [AnimateCommand] = cmdQ

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Print the contents of the command queue one animation command per line.
     */
    def printCommandQueue (t: Double): Unit =
        println (s"At time t = $t: command queue = ")
        println (cmdQ.toString.replace ("), A", ")\nA"))
        println ("-" * 80)
    end printCommandQueue

end DgAnimator


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `dgAnimatorTest` main function is used to test the `DgAnimator` class.
 *  > runMain scalation.animation.dgAnimatorTest
 */
@main def dgAnimatorTest (): Unit =

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sample method for loading the shared command queue.
     *  Ordinarily these commands would come from some simulation engine.
     *  @param cq  the animation command queue
     */
    def loadCommandQueue (cq: ConcurrentLinkedQueue [AnimateCommand]): Unit =
       //:: Place the nodes into graph.

       cq.add (AnimateCommand (CreateNode, 1, Ellipse (),   "node1", false, yellow, Array (100.0, 110.0, 30.0, 30.0), 0))
       cq.add (AnimateCommand (CreateNode, 2, Ellipse (),   "node2", false, yellow, Array (100.0, 290.0, 30.0, 30.0), 0))
       cq.add (AnimateCommand (CreateNode, 3, Rectangle (), "node3", true,  gold,   Array (300.0, 185.0, 30.0, 60.0), 1000))
       cq.add (AnimateCommand (CreateNode, 4, Ellipse (),   "node4", false, silver, Array (500.0, 110.0, 30.0, 30.0), 2000))
       cq.add (AnimateCommand (CreateNode, 5, Ellipse (),   "node5", false, silver, Array (500.0, 290.0, 30.0, 30.0), 2000))
       cq.add (AnimateCommand (CreateNode, 6, Rectangle (), "node6", true,  gold,   Array (300.0,  35.0, 30.0, 60.0), 3000))
       cq.add (AnimateCommand (CreateNode, 7, Rectangle (), "node7", true,  gold,   Array (300.0, 335.0, 30.0, 60.0), 3000))

       //:: Place the edges into graph.

       cq.add (AnimateCommand (CreateEdge, 8,  QCurve (), "edge1", true, lightyellow, null, 4000, 1, 3))
       cq.add (AnimateCommand (CreateEdge, 9,  QCurve (), "edge2", true, lightyellow, null, 4000, 2, 3))
       cq.add (AnimateCommand (CreateEdge, 10, QCurve (), "edge3", true, lightyellow, null, 5000, 3, 4))
       cq.add (AnimateCommand (CreateEdge, 11, QCurve (), "edge4", true, lightyellow, null, 5000, 3, 5))
       cq.add (AnimateCommand (CreateEdge, 12, QCurve (), "edge5", true, lightyellow, null, 6000, 4, 6))
       cq.add (AnimateCommand (CreateEdge, 13, QCurve (), "edge6", true, lightyellow, null, 6000, 5, 7))
       cq.add (AnimateCommand (CreateEdge, 14, QCurve (), "edge7", true, lightyellow, null, 7000, 6, 1))
       cq.add (AnimateCommand (CreateEdge, 15, QCurve (), "edge8", true, lightyellow, null, 7000, 7, 2))

       //:: Place the tokens into graph.

       cq.add (AnimateCommand (CreateToken, 16, Ellipse (), "token1", false, blue, null, 8000, 1))
       cq.add (AnimateCommand (CreateToken, 17, Ellipse (), "token2", false, cyan, null, 8000, 2))

       //:: Move the tokens around graph.

       for i <- 0 to 10 do
           cq.add (AnimateCommand (MoveToken2Node, 16, null, null, false, null, null, 12000 + 10000 * i, 3))
           cq.add (AnimateCommand (MoveToken2Node, 17, null, null, false, null, null, 12000 + 10000 * i, 3))
           cq.add (AnimateCommand (MoveToken2Node, 16, null, null, false, null, null, 13000 + 10000 * i, 4))
           cq.add (AnimateCommand (MoveToken2Node, 17, null, null, false, null, null, 13000 + 10000 * i, 5))
           cq.add (AnimateCommand (MoveToken2Node, 16, null, null, false, null, null, 17000 + 10000 * i, 6))
           cq.add (AnimateCommand (MoveToken2Node, 17, null, null, false, null, null, 17000 + 10000 * i, 7))
           cq.add (AnimateCommand (MoveToken2Node, 16, null, null, false, null, null, 18000 + 10000 * i, 1))
           cq.add (AnimateCommand (MoveToken2Node, 17, null, null, false, null, null, 18000 + 10000 * i, 2))
       end for
    end loadCommandQueue

    println ("Run DgAnimatorTest")
    val dga = new DgAnimator ("DgAnimator")
    loadCommandQueue (dga.getCommandQueue)
    dga.animate (0, 100000)

end dgAnimatorTest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `dgAnimatorTest2` function is used to test the `DgAnimator` class.
 *  > runMain scalation.animation.dgAnimatorTest2
 */
@main def dgAnimatorTest2 (): Unit =

    println ("Run dgAnimatorTest2")
    val dga  = new DgAnimator ("DgAnimator")
    val aniQ = dga.getCommandQueue

    //:: Place the nodes into graph.

    aniQ.add (AnimateCommand (CreateNode, 1, Ellipse (),   "node1", false, yellow, Array (100.0, 110.0, 30.0, 30.0), 0))
    aniQ.add (AnimateCommand (CreateNode, 2, Ellipse (),   "node2", false, yellow, Array (100.0, 290.0, 30.0, 30.0), 0))
    aniQ.add (AnimateCommand (CreateNode, 3, Rectangle (), "node3", true,  gold,   Array (300.0, 185.0, 30.0, 60.0), 1000))
    aniQ.add (AnimateCommand (CreateNode, 4, Ellipse (),   "node4", false, silver, Array (500.0, 110.0, 30.0, 30.0), 2000))
    aniQ.add (AnimateCommand (CreateNode, 5, Ellipse (),   "node5", false, silver, Array (500.0, 290.0, 30.0, 30.0), 2000))
    aniQ.add (AnimateCommand (CreateNode, 6, Rectangle (), "node6", true,  gold,   Array (300.0,  35.0, 30.0, 60.0), 3000))
    aniQ.add (AnimateCommand (CreateNode, 7, Rectangle (), "node7", true,  gold,   Array (300.0, 335.0, 30.0, 60.0), 3000))

    dga.animate (0, 100000)

end dgAnimatorTest2

