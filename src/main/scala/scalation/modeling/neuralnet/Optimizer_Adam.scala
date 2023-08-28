
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Hao Peng
 *  @version 2.0
 *  @date    Sat Mar  5 22:38:03 EST 2022
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Optimization: ADAptive Moment estimation (Adam) Optimizer
 */

// U N D E R   D E V E L O P M E N T

package scalation
package modeling
package neuralnet

import scala.math.min

import scalation.mathstat._

import Optimizer._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Optimizer_Adam` class provides functions to optimize the parameters (weights
 *  and biases) of Neural Networks with various numbers of layers.
 *  This optimizer implements a
 *  @see https://arxiv.org/pdf/1412.6980.pdf
 */
class Optimizer_Adam extends Optimizer:

    private val debug = debugf ("Optimizer_Adam", true)                   // debug function
    private val flaw  = flawf ("Optimizer_Adam")                          // flaw function

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given training data x and y for a 2-layer, multi-output Neural Network, fit
     *  the parameter/weight matrix b.  Iterate over several epochs, where each epoch
     *  divides the training set into nB batches.  Each batch is used to update the
     *  the parameter's weights.
     *  @param x     the m-by-n input matrix (training data consisting of m input vectors)
     *  @param y     the m-by-ny output matrix (training data consisting of m output vectors)
     *  @param bb    the array of parameters (weights & biases) between every two adjacent layers
     *  @param eta_  the initial learning/convergence rate
     *  @param ff    the array of activation function family for every two adjacent layers
     */
    def optimize2 (x: MatrixD, y: MatrixD,
                   bb: NetParams, eta_ : Double, ff: Array [AFF]): (Double, Int) =
        val permGen   = permGenerator (x.dim)                             // permutation vector generator
        val b         = bb(0)                                             // net-parameters: weight matrix and bias vector
        val f         = ff(0)                                             // activation function
        val bSize     = min (hp("bSize").toInt, x.dim)                    // batch size
        val maxEpochs = hp("maxEpochs").toInt                             // maximum number of epochs
        val upLimit   = hp("upLimit").toInt                               // limit on increasing lose
        val β1        = hp("beta").toDouble                               // momentum hyper-parameter
        val β2        = hp("beta2").toDouble                              // second momentum hyper-parameter
        val nB        = x.dim / bSize                                     // the number of batches
        var eta       = eta_                                              // set initial learning rate

        var mt        = new MatrixD (b.w.dim, b.w.dim2)                   // first moment estimate
        var vt        = new MatrixD (b.w.dim, b.w.dim2)                   // second raw moment estimate

        println (s"optimize2: bSize = $bSize, nB = $nB")

        var sse_best_   = -0.0
        var (go, epoch) = (true, 1)
        cfor (go && epoch <= maxEpochs, epoch += 1) {                     // iterate over each epoch
            val batches = permGen.igen.chop (nB)                          // permute indices & split into nB batches

            for ib <- batches do b -= updateWeight (x(ib), y(ib), epoch)     // iteratively update weight matrix b

            val sse = (y - f.fM (b * x)).normFSq                          // recompute sum of squared errors
            collectLoss (sse)                                             // collect loss per epoch
            debug ("optimize2", s"parameters for $epoch th epoch: sse = $sse")
            val (b_best, sse_best) = stopWhen (Array (b), sse)
            if b_best != null then
                b.set (b_best (0))
                sse_best_ = sse_best                                      // save best in sse_best_
                go = false
            else
                if epoch % ADJUST_PERIOD == 0 then eta *= ADJUST_FACTOR   // adjust the learning rate
            end if
        } // cfor

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /*  Update the parameter/weight matrix b based on the current batch.
         *  Take a step in the direction opposite to the gradient.
         *  @see https://arxiv.org/pdf/1412.6980.pdf
         *  @param x  the input matrix for the current batch
         *  @param y  the output matrix for the current batch
         */
        inline def updateWeight (x: MatrixD, y: MatrixD, t: Int): MatrixD =
            val yp  = f.fM (b * x)                                        // Yp = f(XB)
            val ee  = yp - y                                              // negative of the error matrix
            val gt  = f.dM (yp) *~ ee                                     // delta matrix for y

            mt      = mt * β1 + gt * (1 - β1)                             // update biased first moment estimate)
            vt      = vt * β2 + gt~^2 * (1 - β2)                          // update biased second raw moment estimate)
            val mht = mt / (1 - β1~^t)                                    // compute bias-corrected first moment estimate)
            val vht = vt / (1 - β2~^t)                                    // compute bias-corrected second raw moment estimate)
            val d   = mht / (vht~^0.5 + EPSILON)                          // parameter update correction matrix (corrected delta)

            val eta_o_sz = eta / x.dim                                    // eta over the current batch size
            x.transpose * d * eta_o_sz                                    // gradient-based change in input-output weights (bup)
        end updateWeight

        debug ("optimize2", s"parameters b = $b")
        if go then ((y - f.fM (b * x)).normFSq, maxEpochs)                // return best and number of epochs
        else       (sse_best_, epoch - upLimit)
    end optimize2

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given training data x and y for a 3-layer Neural Network, fit the parameters
     *  (weights and biases) a & b.  Iterate over several epochs, where each epoch divides
     *  the training set into nB batches.  Each batch is used to update the weights. 
     *  @param x     the m-by-n input matrix (training data consisting of m input vectors)
     *  @param y     the m-by-ny output matrix (training data consisting of m output vectors)
     *  @param bb    the array of parameters (weights & biases) between every two adjacent layers
     *  @param eta_  the initial learning/convergence rate
     *  @param ff    the array of activation function family for every two adjacent layers
     */
    def optimize3 (x: MatrixD, y: MatrixD,
                   bb: NetParams, eta_ : Double, ff: Array [AFF]): (Double, Int) =
        val idx       = VectorI.range (0, x.dim)                          // instance index range
        val permGen   = permGenerator (x.dim)                             // permutation vector generator
        val (a, b)    = (bb(0), bb(1))                                    // two sets of net-parameters
        val (f, f1)   = (ff(0), ff(1))                                    // two activation functions
        val bSize     = min (hp("bSize").toInt, x.dim)                    // batch size
        val maxEpochs = hp("maxEpochs").toInt                             // maximum number of epochs
        val upLimit   = hp("upLimit").toInt                               // limit on increasing lose
        val beta      = hp("beta").toDouble                               // momentum hyper-parameter
        val nB        = x.dim / bSize                                     // the number of batches
        var eta       = eta_                                              // counter for number of times moving up
        var moa       = new MatrixD (a.w.dim, a.w.dim2)                   // momentum matrix a
        var mob       = new MatrixD (b.w.dim, b.w.dim2)                   // momentum matrix b

        println (s"optimize3: bSize = $bSize, nB = $nB")

        var sse_best_   = -0.0
        var (go, epoch) = (true, 1)
        cfor (go && epoch <= maxEpochs, epoch += 1) {                     // iterate over each epoch
            val batches = permGen.igen.chop (nB)                          // permute indices & split into nB batches

            for ib <- batches do
                val ab = updateWeight (x(ib), y(ib))                      // iteratively update weight matrices a & b
                a -= ab._1; b -= ab._2
            end for

            val sse = (y - b * f1.fM (f.fM (a * x))).normFSq
            collectLoss (sse)                                             // collect the loss per epoch
//          debug ("optimize3", s"parameters for $epoch th epoch: sse = $sse")
            val (b_best, sse_best) = stopWhen (Array (a, b), sse)
            if b_best != null then
                a.set (b_best(0))
                b.set (b_best(1))
                sse_best_ = sse_best                                      // save best in sse_best_
                go = false
            else
                if epoch % ADJUST_PERIOD == 0 then eta *= ADJUST_FACTOR   // adjust the learning rate
            end if
        } // cfor

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /*  Compute the parameter/weight matrices a and b updates based on the current batch.
         *  A step in the direction opposite to the gradient.
         *  @param x  the input matrix for the current batch
         *  @param y  the output matrix for the current batch
         */
        inline def updateWeight (x: MatrixD, y: MatrixD): (NetParam, NetParam) =
            var z  = f.fM (a * x)                                         // Z  = f(XA)
            var yp = f1.fM (b * z)                                        // Yp = f(ZB)
            var ee = yp - y                                               // negative of the error matrix
            val d1 = f1.dM (yp) *~ ee                                     // delta matrix for y
            val d0 = f.dM (z) *~ (d1 * b.w.transpose)                     // delta matrix for z
    
            val eta_o_sz = eta / x.dim                                    // eta over current batch size
            moa = moa * beta + x.transpose * d0 * eta_o_sz                // update momentum a
            mob = mob * beta + z.transpose * d1 * eta_o_sz                // update momentum b
            (NetParam (moa, d0.mean * eta),                               // change to a parameters (weights and biases)
             NetParam (mob, d1.mean * eta))                               // change to b parameters (weights and biases)
        end updateWeight

        debug ("optimize3", s"parameters a = $a \n b = $b")
        if go then ((y - b * f1.fM (f.fM (a * x))).normFSq, maxEpochs)                // return best and number of epochs
        else       (sse_best_, epoch - upLimit)
    end optimize3

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given training data x and y, fit the parameter/weight matrices bw and
     *  bias vectors bi. Iterate over several epochs, where each epoch divides the
     *  training set into nB batches. Each batch is used to update the weights.
     *  @param x     the m-by-n input matrix (training data consisting of m input vectors)
     *  @param y     the m-by-ny output matrix (training data consisting of m output vectors)
     *  @param b     the array of parameters (weights & biases) between every two adjacent layers
     *  @param eta_  the initial learning/convergence rate
     *  @param f     the array of activation function family for every two adjacent layers
     */
    def optimize (x: MatrixD, y: MatrixD,
                  b: NetParams, eta_ : Double, f: Array [AFF]): (Double, Int) =
        val permGen   = permGenerator (x.dim)                             // permutation vector generator
        val bSize     = min (hp("bSize").toInt, x.dim)                    // batch size
        val maxEpochs = hp("maxEpochs").toInt                             // maximum number of epochs
        val upLimit   = hp("upLimit").toInt                               // limit on increasing lose
        val beta      = hp("beta").toDouble                               // momentum hyper-parameter
        val nB        = x.dim / bSize                                     // the number of batches
        var eta       = eta_                                              // counter for number of times moving up
        var sse       = 0.0                                               // stores accumulated sse over batches for epoch
        println (s"optimize: bSize = $bSize, nB = $nB")

        val nl     = f.size                                               // number of layers
        val layers = 0 until nl                                           // range for layers
        val z      = Array.ofDim [MatrixD] (nl+1)                         // array to store activations, layer by layer
        val d      = Array.ofDim [MatrixD] (nl)                           // array to store all deltas
        var mo     = Array.ofDim [MatrixD] (nl)                           // momentum array
        for l <- layers do mo(l) = new MatrixD (b(l).w.dim, b(l).w.dim2)

        var sse_best_   = -0.0
        var (go, epoch) = (true, 1)
        cfor (go && epoch <= maxEpochs, epoch += 1) {                     // iterate over each epoch
            sse         = 0.0
            val batches = permGen.igen.chop (nB)                          // permute indices &split into nB batches

            for ib <- batches do sse += updateWeight (x(ib), y(ib))       // update parameter array b

            collectLoss (sse)                                             // collect the loss per epoch
//          debug ("optimize", s" parameters for $epoch th epoch: b = $b, sse = $sse")
            val (b_best, sse_best) = stopWhen (b, sse)
            if b_best != null then
                for l <- b.indices do b(l).set (b_best(l))
                sse_best_ = sse_best                                      // save best in sse_best_
                go = false
            else
                if epoch % ADJUST_PERIOD == 0 then eta *= ADJUST_FACTOR   // adjust the learning rate
            end if
        } // cfor

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /*  Compute the parameter array b updates based on the current batch.
         *  A step in the direction opposite to the gradient.
         *  @param x  the input matrix for the current batch
         *  @param y  the output matrix for the current batch
         */
        inline def updateWeight (x: MatrixD, y: MatrixD): Double =
            z(0) = x                                                      // initial activation, which is the input matrix
            for l <- layers do z(l+1) = f(l).fM (b(l) * z(l))             // feedforward and store all activations

            val yp  = z.last                                              // predicted value of y
            val ee  = yp - y                                              // negative of the error matrix
            d(nl-1) = f.last.dM (yp) *~ ee                                // delta for the last layer before output
            for l <- nl-2 to 0 by -1 do
                d(l) = f(l).dM (z(l+1)) *~ (d(l+1) * b(l+1).w.transpose)  // deltas for all previous hidden layers

            val eta_o_sz = eta / x.dim                                    // learning rate divided by size of mini-batch
            for l <- layers do
//              b(l).w *= 1.0 - eta * (lambda / x.dim)                    // regularization factor, weight decay
                mo(l) = mo(l) * beta + z(l).transpose * d(l) * eta_o_sz   // update l-th momentum
                b(l) -= (mo(l), d(l).mean * eta)                          // update l-th parameter (weights and biases)
            end for

            ee.normFSq                                                    // return the sse of this batch
        end updateWeight

        debug ("optimize", s"parameters b = $b")
        if go then (sse, maxEpochs)                                       // return best and number of epochs
        else       (sse_best_, epoch - upLimit)
    end optimize
end Optimizer_Adam

