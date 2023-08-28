
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Fri Mar 16 15:13:38 EDT 2018
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Model Support: Neural Network Parameters (weights and biases)
 */

package scalation
package modeling
package neuralnet

import scalation.mathstat._

type NetParams = Array [NetParam]

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Extension method that allows dot products and multipilication to be handled
 *  for either case.
 *  @param b  the parameters (weights and biases)
 */
extension (b: MatrixD | NetParam)
    def dot (x: VectorD): VectorD =
        b match 
        case b: MatrixD  => b.asInstanceOf [MatrixD] dot x
        case b: NetParam => b.asInstanceOf [NetParam] dot x
    end dot
    def * (x: MatrixD): MatrixD =
        b match 
        case b: MatrixD  => b.asInstanceOf [MatrixD] * x
        case b: NetParam => b.asInstanceOf [NetParam] * x
    end *


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NetParam` class bundles parameter weights and biases together.
 *  @param w  the weight matrix
 *  @param b  the bias/intercept vector
 */
case class NetParam (var w: MatrixD, var b: VectorD = null):

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Make a copy of the network parameters.
     */
    def copy: NetParam = NetParam (w.copy, if b != null then b.copy else null)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Make a trimmed copy of the network parameters.
     *  @param dim   the first dimension of weight matrix
     *  @param dim2  the second dimension of weight matrix and dimension of bias vector
     */
    def trim (dim: Int, dim2: Int): NetParam =
        NetParam (w(0 until dim, 0 until dim2), if b != null then b(0 until dim2) else null)
    end trim

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Update/assign `NetParam` c to this.
     */
    def update (c: NetParam): Unit = { w = c.w; b = c.b }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set/assign `NetParam` c to this (needed for val cases).
     */
    def set (c: NetParam): Unit = { w = c.w; b = c.b }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Update/assign (cw, cb) to this.
     */
    def update (cw: MatrixD, cb: VectorD = null): Unit = { w = cw; b = cb }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set/assign (cw, cb) to this (needed for val cases).
     */
    def set (cw: MatrixD, cb: VectorD = null): Unit = { w = cw; b = cb }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add and assign the changes to the parameters (weights and biases).
     *  @param c  the change to the parameters
     */
    def += (c: NetParam): Unit = 
        w += c.w
        if c.b != null then b += c.b
    end +=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add and assign the changes to the parameters (weights and biases).
     *  @param cw  the change to the weights
     *  @param cb  the change to the baises
     */
    def += (cw: MatrixD, cb: VectorD): Unit = 
        w += cw
        if cb != null then b += cb
    end +=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Subtract and assign the changes to the parameters (weights and biases).
     *  @param c  the change to the parameters
     */
    def -= (c: NetParam): Unit = 
        w -= c.w
        if c.b != null then b -= c.b
    end -=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Subtract and assign the changes to the parameters (weights and biases).
     *  @param cw  the change to the weights
     *  @param cb  the change to the baises
     */
    def -= (cw: MatrixD, cb: VectorD = null): Unit =
        w -= cw
        if cb != null then b -= cb
    end -=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply matrix x by the weight matrix w and add the bias vector b,
     *  unless the bias is null.
     *  @param x  the matrix to multiply by
     */
    def * (x: MatrixD): MatrixD =
        if b != null then x * w + b
        else x * w
    end *

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply matrix x by the weight matrix w and add the bias vector b.
     *  This is the right associative version (`NetParam` on the left).
     *  @param x  the matrix to multiply by
     */
    def *: (x: MatrixD): MatrixD = x * w + b

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply vector x by the weight matrix w and add the bias vector b.
     *  @param x  the vector to multiply by
     */
    def dot (x: VectorD): VectorD = (w dot x) + b

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return whether this and `NetParam` c are approximately equal.
     *  @param c  the other network parameters
     */
    def =~ (c: NetParam): Boolean = 
        if (w - c.w).normFSq > 1E-3 then { println (s"w = $w =~ c.w = ${c.w} is false"); return false }
        if (b - c.b).normSq  > 1E-3 then { println (s"b = $b =~ c.b = #{c.b} is false"); return false }
        true
    end =~

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert this `NetParam` object to a matrix where the first row is the 
     *  bias vector b and the rest of the rows contain the weight matrix w.
     */
    def toMatrixD: MatrixD = if b == null then w else b +: w 

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert this `NetParam` object to a string showing this (b) parameter's
     *  weight matrix b.w and bias vector b.b.
     */
    override def toString: String = s"b.w = $w \n b.b = $b"

end NetParam

