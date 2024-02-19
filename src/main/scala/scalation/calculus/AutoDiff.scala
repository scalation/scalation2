
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sun May 21 01:50:40 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Reverse Mode Automatic Differentiation
 *
 *  @see https://www.jmlr.org/papers/volume18/17-468/17-468.pdf
 *       https://alexander-schiendorfer.github.io/2020/02/16/automatic-differentiation.html
 *       https://www.cs.toronto.edu/~rgrosse/courses/csc321_2018/slides/lec10.pdf
 */

//  U N D E R   D E V E L O P M E N T

package scalation
package calculus

import scala.collection.mutable.{ArrayBuffer => VEC}

import scalation.mathstat.MatrixD
import scalation.modeling.AFF
import scalation.modeling.ActivationFun._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Node` case class maintains information about a node in a Computation Graph.
 *  @param v_dim2  the output second dimension
 *  @param b       the parameter matrix (weight => multiply, bias => add)
 *  @param u       the input vector
 *  @param f       the activation function family or null for none
 */
case class Node (v_dim2: Int, b: MatrixD, u: MatrixD, f: AFF):

    val v = new MatrixD (u.dim, v_dim2)                           // output value computed in forward pass
    var vb: MatrixD = null                                        // adjoint value computed in backward pass

    def copy (z: MatrixD): Unit =                                 // FIX - fails if dimensions are not right
        for i <- v.indices; j <- v.indices2 do v(i, j) = z(i, j)
    end copy

    override def toString: String =
        val nm = if f == null then "null" else f.name
        s"Node ($nm <- f $b <- b $u <- u  $v <- v)\n"
    end toString

end Node


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `AutoDiff` class supports Automatic Differentiation.
 */
class AutoDiff (y: MatrixD):

    val pipe = VEC [Node] ()                                      // computation graph as a pipeline

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add node to the computation graph.
     *  @param node  the node to add to the pipeline
     */
    def add (node: Node): Node =
        pipe += node
        node
    end add

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Make a forward pass through the computation graph, computing output values
     *  based on the calculation v = f(u + b(0)) where u is the weighted input,
     *  b(0) is the bias vector and f is the activation function.
     */
    def forward (): Unit =
        for i <- pipe.indices do
            val (b, u, f) = (pipe(i).b, pipe(i).u,  pipe(i).f)
            println (s"FORWARD (i = $i): input u = $u")
            val vv: MatrixD =                                     // compute node i's output value
            if b == null && f == null then                        // no parameters and no activation function
                println (s"forward (i = $i): output y = $y")
                MatrixD ((1, 1), 0.5 * (u - y).normFSq)           // apply norm on difference
            else if f == null then                                // no activation function
                println (s"forward (i = $i): weight b = $b")
                u * b                                             // multiply by weight matrix
            else
                println (s"forward (i = $i): activation f = ${f.name}")
                f.fM (u)                                          // apply activation function
//              f.fM (u + b(0))                                   // add bias vector and apply activation function
            println (s"forward: assign output for node ($i) = $vv")
            pipe(i).copy (vv)                                     // assign computed value to node i's output
        end for
    end forward

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Make a backward pass through the computation graph, computing partial
     *  derivatives and adjoints using reverse mode automatic differentiation.
     *  Compute adjoint: v-bar-sub-i = vb_i = vb_i+1 * d_v_i+1 / d_v_i
     *  @see https://www.jmlr.org/papers/volume18/17-468/17-468.pdf, section 3.2 Reverse Mode
     *  @param e  the negative error matrix
     */
    def backward (e: MatrixD): Unit =                             // FIX -- does not work correctly, problem with delta0
        val sz = pipe.size
//      pipe(sz-1).vb = MatrixD.fill (e.dim, e.dim2, 1.0)
        pipe(sz-1).vb = e
        var dv2: MatrixD = null
        for i <- sz-2 to 1 by -1 do
            val (v, f, b) = (pipe(i).v, pipe(i).f, pipe(i).b)
            banner (s"backward: step $i")
            val dv: MatrixD =                                     // compute node i's partial derivative
            if f == null then                                     // no activation function
                println (s"backward: no activation function")
                println (s"+++ backward: dv2 = $dv2")
                println (s"+++ backward: b = $b")
                dv2 * b.transpose
            else
                println (s"backward: activation f = ${f.name}")
                f.dM (v)
            println (s"backward: partial derivative for node ($i) = $dv")
            val v_bar = pipe(i+1).vb *~ dv                        // compute node i's adjoint value
            pipe(i).vb = v_bar
            println (s"backward: adjoint value for     node ($i) = $v_bar")
            dv2 = dv                                              // save partial derivative
        end for
    end backward

end AutoDiff


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `autoDiffTest` main function tests the `AutoDiff` class.
 *  Compare to the step-by-step results for `Perceptron`.
 *  @see `scalation.modeling.neuralnet.neuralNet_3LTest`
 *  > runMain scalation.calculus.autoDiffTest
 */
@main def autoDiffTest (): Unit =

    import scalation.mathstat.VectorD

    // 9 data points:    Constant    x1    x2     y
    val xy = MatrixD ((9, 4), 1.0,  0.0,  0.0,  0.5,
                              1.0,  0.0,  0.5,  0.3,
                              1.0,  0.0,  1.0,  0.2,

                              1.0,  0.5,  0.0,  0.8,
                              1.0,  0.5,  0.5,  0.5,
                              1.0,  0.5,  1.0,  0.3,

                              1.0,  1.0,  0.0,  1.0,
                              1.0,  1.0,  0.5,  0.8,
                              1.0,  1.0,  1.0,  0.5)

    val b  = VectorD (0.1, 0.2, 0.1)                                // initial weights/parameters
    val _1 = VectorD.one (xy.dim)                                   // vector of all ones

    println (s"xy = $xy")
    val (x, y) = (xy.not(?, 3), xy(?, 3))                           // input matrix, output/response vector
    println (s"x = $x")
    println (s"y = $y")
    val sst = (y - y.mean).normSq                                   // sum of squares total
    println (s"sst = $sst")

    val eta = 1.0                                                   // learning rate

    for epoch <- 1 to 2 do
        banner (s"improvement step $epoch")
        val u   = x * b                                             // pre-activation value
        val yp  = sigmoid_ (u)                                      // predicted response from calculation for sigmoid
        val e   = yp - y                                            // negative error
        val sse = e.normSq                                          // sum of squared errors
        val ls  = 0.5 * sse                                         // loss function
        val fp  = yp * (_1 - yp)                                    // derivative (f') for sigmoid
        val d   = e * fp                                            // delta
        val g   = x.transpose * d                                   // gradient
        val bup = g * eta                                           // parameter update
        b      -= bup                                               // new parameter vector

        println (s"epoch = $epoch, loss = $ls, sse = $sse, rSq = ${1 - sse/sst}")
        println (s"forward:  $u <- u \n $yp <- yp \n $e <- e \n $ls <- ls")
        println (s"backward: $fp <- fp \n $d <- d \n $g <- g")
    end for

    val ad = new AutoDiff (MatrixD.fromVector (y))                  // Automatic Differentiation

    // Form Computation Graph for 3-by-1 perceptron

    val n0 = ad.add (Node (1, MatrixD.fromVector (b), x, null))     // step 0: u  = Xb
    val n1 = ad.add (Node (1, null, n0.v, f_sigmoid))               // step 1: yp = f(u)
    val n2 = ad.add (Node (1, null, n1.v, null))                    // step 2: L  = .5 || yp - y ||^2

    banner ("AD Forward Pass")
    ad.forward ()                                                   // forward pass to compute intermediate and output values
    println (s"predicted output yp = n1.v = ${n1.v}")
    println (s"loss             ls = n2.v = ${n2.v}")

    banner ("Show Computation Graph")
    println (s"pipe = ${ad.pipe}")

    banner ("AD Backward Pass")
    // currently passing in negative error e, should it be loss?
    ad.backward (n2.v)         

end autoDiffTest


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `autoDiffTest2` main function tests the `AutoDiff` class.
 *  Compare to the step-by-step results for `NeuralNet_3L`.
 *  @see `scalation.modeling.neuralnet.neuralNet_3LTest11`
 *  FIX: values up to delta0 are correct, delta0 has one column in AutoDiff, but 2 in NeuralNet_3L
 *  > runMain scalation.calculus.autoDiffTest2
 */
@main def autoDiffTest2 (): Unit =

    import scalation.mathstat.VectorD

    val x1 = VectorD (1, 2, 3, 4, 5, 6, 7, 8, 9)
    val x2 = VectorD (8, 7, 6, 5, 5, 4, 4, 3, 2)
    val yv = VectorD (1, 2, 4, 7, 9, 8, 6, 5, 3)
    val x  = MatrixD (x1, x2).transpose
    val y  = MatrixD.fromVector (yv)

    println (s"x = $x")                                             // 9x2 input matrix
    println (s"y = $y")                                             // 9x1 output matrix
    val sst = (y - y.mean).normFSq                                  // sum of squares total (sst)
    println (s"sst = $sst")

    banner ("initialize")
    val ad = new AutoDiff (y)                                       // Automatic Differentiation
    val aa = MatrixD.fill (2, 2, 0.1)                               // input to hidden layer 2x2 weight matrix
    val a_ = MatrixD.fill (1, 2, 0.1)                               // hidden layer 1x2 bias matrix
    val bb = MatrixD.fill (2, 1, 0.1)                               // hidden to output layer weight matrix
    val b_ = MatrixD.fill (1, 1, 0.1)                               // output layer 1x1 bias matrix

    println (s"hidden layer aa = $aa, a_ = $a_")                    // weight and bias matrices
    println (s"output layer bb = $bb, b_ = $b_")                    // weight and bias matrices

    // Form Computation Graph for 2-by-2-by-1 neural network

    val n0 = ad.add (Node (2, aa, x, null))                         // step 0: U  = XA
    val n1 = ad.add (Node (2, a_, n0.v, f_sigmoid))                 // step 1: Z  = f0(U + a_)
    val n2 = ad.add (Node (1, bb, n1.v, null))                      // step 2: V  = ZB
    val n3 = ad.add (Node (1, b_, n2.v, f_id))                      // step 3: Yp = f1(V + b_)
    val n4 = ad.add (Node (1, null, n3.v, f_id))                    // step 4: E  = Yp - Y

    banner ("AD Forward Pass")
    ad.forward ()                                                   // forward pass to compute intermediate and output values
    val loss = n4.v.normFSq                                         // sum of squared errors (sse)
    println (s"predicted output yp = n3.v = ${n3.v}")
    println (s"negative error   e  = n4.v = ${n4.v}")
    println (s"loss = $loss")

    banner ("Show Computation Graph")
    println (s"pipe = ${ad.pipe}")

    banner ("AD Backward Pass")
    // currently passing in negative error e, should it be loss?
    ad.backward (n4.v)                                              // backward pass to compute partial derivatives and adjoints

end autoDiffTest2

