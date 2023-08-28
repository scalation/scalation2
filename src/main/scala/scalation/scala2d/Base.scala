
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Tue May 13 16:18:42 EDT 2014
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Classes/types and objects intended to Make Switching GUI's Easier
 */

package scalation
package scala2d

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `scala2d` package contains classes, traits and objects for
 *  simple 2D graphics in Scala, based upon `java.swing`, `java.awt` and
 *  It makes `java.awt` and `javax.swing` GUI classes available
 *  and insulates the rest of ScalaTion from changes to GUI libraries.
 *  Only `scalation.scala2d` should import from java.awt` or `javax.swing`.
 *  @see Shapes.scala for providing classes in `java.awt.geom`.
 */

// type definition for awt and awt.event classes

type Font          = java.awt.Font
val  Font_BOLD     = java.awt.Font.BOLD
val  Font_ITALIC   = java.awt.Font.ITALIC
type MouseEvent    = java.awt.event.MouseEvent
type MouseListener = java.awt.event.MouseListener
type MouseAdapter  = java.awt.event.MouseAdapter

// type definition for swing classes

type Label         = javax.swing.JLabel
type Panel         = javax.swing.JPanel
type ScrollPane    = javax.swing.JScrollPane
type Table         = javax.swing.JTable

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BorderLayout` object is a convenience object for `java.awt.BorderLayout`.
 */
object BorderLayout extends java.awt.BorderLayout:

    val AFTER_LAST_LINE = java.awt.BorderLayout.AFTER_LAST_LINE
    val CENTER          = java.awt.BorderLayout.CENTER
    val NORTH           = java.awt.BorderLayout.NORTH

end BorderLayout

import javax.swing.WindowConstants.EXIT_ON_CLOSE

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Frame` class is a convenience class for `JFrame`.
 */
class Frame (title: String) extends javax.swing.JFrame (title):

    setDefaultCloseOperation (EXIT_ON_CLOSE)

end Frame

