
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/*
 *      Limited memory BFGS (L-BFGS).
 *
 * Copyright (c) 1990, Jorge Nocedal
 * Copyright (c) 2007-2010 Naoaki Okazaki
 * All rights reserved.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

/* $Id$ */

/*
This library is a C port of the FORTRAN implementation of Limited-memory
Broyden-Fletcher-Goldfarb-Shanno (L-BFGS) method written by Jorge Nocedal.
The original FORTRAN source code is available at:
http://www.ece.northwestern.edu/~nocedal/lbfgs.html
The L-BFGS algorithm is described in:
    - Jorge Nocedal.
      Updating Quasi-Newton Matrices with Limited Storage.
      <i>Mathematics of Computation</i>, Vol. 35, No. 151, pp. 773--782, 1980.
    - Dong C. Liu and Jorge Nocedal.
      On the limited memory BFGS method for large scale optimization.
      <i>Mathematical Programming</i> B, Vol. 45, No. 3, pp. 503-528, 1989.
The line search algorithms used in this implementation are described in:
    - John E. Dennis and Robert B. Schnabel.
      <i>Numerical Methods for Unconstrained Optimization and Nonlinear
      Equations</i>, Englewood Cliffs, 1983.
    - Jorge J. More and David J. Thuente.
      Line search algorithm with guaranteed sufficient decrease.
      <i>ACM Transactions on Mathematical Software (TOMS)</i>, Vol. 20, No. 3,
      pp. 286-307, 1994.
This library also implements Orthant-Wise Limited-memory Quasi-Newton (OWL-QN)
method presented in:
    - Galen Andrew and Jianfeng Gao.
      Scalable training of L1-regularized log-linear models.
      In <i>Proceedings of the 24th International Conference on Machine
      Learning (ICML 2007)</i>, pp. 33-40, 2007.
I would like to thank the original author, Jorge Nocedal, who has been
distributing the effieicnt and explanatory implementation in an open source
licence.
*/

package scalation
package optimization

import scala.math.abs

import mathstat.{FunctionV2S, VectorD}

import BFGS_code._

class L_BFGS (ff: FunctionV2S, gf: VectorD => VectorD)
      extends Minimizer:

    private var param: BFGS_parameter = null

    def lineSearch (x: VectorD, dir: VectorD, step: Double): Double = ???

    def setPram (param_ : BFGS_parameter): Unit = { param = param_ }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve the Non-Linear Programming (NLP) problem by starting at x0 and
     *  iteratively moving down in the search space to a minimal point.
     *  Return the optimal point/vector x and its objective function value.
     *  @param x0     the starting point
     *  @param step   the initial step size (ignored)
     *  @param toler  the tolerance (ignored)
     */
    def solve (x0: VectorD, step: Double = -0.0, toler: Double = -0.0): FuncVec =
        val res = L_BFGS.lbfgs (x0.dim, x0, ff, gf, param)
        (res._2, res._1)
    end solve

end L_BFGS


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `L_BFGS` object contains a lbfgs function for nonlinear optimization.
 */
object L_BFGS:

    private val debug = debugf ("L_BFGS", true)                  // debug function

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Limited-memory Broyden-Fletcher-Goldfarb-Shanno (L-BFGS) Optimization Algorithm.
     *------------------------------------------------------------------------------
     *  @param n  The number of variables.
     *------------------------------------------------------------------------------
     *  @param x  The array of variables.  A client program can set default values for
     *         the optimization and receive the optimization result through this array.
     *------------------------------------------------------------------------------
     *  @param ptr_fx  The pointer to the variable that receives the final value of the
     *         objective function for the variables.  This argument can be set to \c null
     *         if the final value of the objective function is unnecessary.
     *------------------------------------------------------------------------------
     *  @param proc_evaluate  The callback function to provide function and gradient
     *         evaluations given a current values of variables.  A client program must
     *         implement a callback function compatible with \ref lbfgs_evaluate_t and
     *         pass the pointer to the callback function.
     *------------------------------------------------------------------------------
     *  @param proc_progress  The callback function to receive the progress (the number
     *         of iterations, the current value of the objective function) of the
     *         minimization process. This argument can be set to \c null if a progress
     *         report is unnecessary.
     *------------------------------------------------------------------------------
     *  @param instance  A user data for the client program.  The callback functions
     *         will receive the value of this argument.
     *------------------------------------------------------------------------------
     *  @param param  The pointer to a structure representing parameters for L-BFGS
     *         optimization.  A client program can set this parameter to \c null to
     *         use the default parameters.  Call lbfgs_parameter_init() function to
     *         fill a structure with the default values.
     *------------------------------------------------------------------------------
     *  @return (VectorD, Double, Int)  The optimal location,  value of the objective
     *          function and the status code.  This function returns zero if the minimization
     *          process terminates without an error.  A non-zero value indicates an error.
     */
    def lbfgs (n:       Int,
               x_ :     VectorD,
               ff:      VectorD => Double,
               gf:      VectorD => VectorD,
               param_ : BFGS_parameter = null): (VectorD, Double, Int) =

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        case class Iteration_data (var alpha: Double  = 0.0,
                                   var s:     VectorD = new VectorD (n),     // [n]
                                   var y:     VectorD = new VectorD (n),     // [n]
                                   var ys:    Double  = 0.0)                 // vecdot(y, s)
        end Iteration_data

        // Evaluate the function value and its gradient.
        var x  = x_
        var fx = ff(x)                                           // value of objective function at x
        var gx = gf(x)                                           // vector value of gradient function at x

        debug ("lbfgs", s"start optimization from x = $x, fx = $fx, gx = $gx")

        BFGS_LS.set_ff (ff)
        BFGS_LS.set_gf (gf)
    
        // Set the (hyper) parameters to the new spedification or use their default values.
        val param = if param_ != null then { BFGS_LS.set_param (param_); param_ }
                    else BFGS_parameter ()
    
        // Determine which line search algorithm to use
        debug ("lbfgs", s"select linesearch: ${param.linesearch}")
        val linesearch = BFGS_LS.select_linesearch
    
        // Allocate an array for storing previous values of the objective function.
        val pf = if param.past > 0 then Array.ofDim [Double] (param.past) else null
    
        // Allocate limited memory storage and initialize the limited memory.
        val m  = param.m                                         // the size of the limited memory (lm)
        val lm = Array.fill [Iteration_data] (m)(Iteration_data ())
    
        var xnorm = 0.0                                          // norn of position x
        var gnorm = 0.0                                          // norm of gradient gx

        // Store the initial value of the objective function.
        if pf != null then pf(0) = fx
    
        // Compute the direction, assuming the initial hessian matrix H_0 as the identity matrix.
        var d = -gx
        debug ("lbfgs", s"direction d = $d, d.norm = ${d.norm}")
    
        // Make sure that the initial variables are not a minimizer.
        xnorm = x.norm
        gnorm = gx.norm
        if xnorm < 1.0 then xnorm = 1.0
        if gnorm / xnorm <= param.epsilon then
           debug ("lbfgs", "return already minimized")
           return (x, fx, LBFGS_ALREADY_MINIMIZED.code)
        end if
    
        // Compute the initial step: step = 1.0 / sqrt(vecdot(d, d, n))
        var step  = 1.0 / d.norm                                 // step size
        var w     = new VectorD (n)                              // orthant updated by linesearch
        var ls    = 0                                            // number of steps taken by linesearch
        var bound = 0                                            // limited memory bound
        var end_  = 0                                            // current end in limited memory
        var k     = 1                                            // number of iterations (in while loop)

        while true do
            banner (s"lbfgs iteration $k: step = $step")
            val xp = x.copy                                      // save the current position vector
            val gp = gx.copy                                     // save the current gradient vector
            var d  = -gx      //.copy                                     // best direction vector, modified later

/*
    val n    = 2                           // the dimension of the search space
    val x    = VectorD (0, 0)              // the current location/point vector
    val f    = 26.0                        // the objective function value f(x)
    val g    = VectorD (-6, -8)            // the gradient vector at x
    val s    = VectorD (6, 8)              // the search direction (e.g., opposite g)
    val step = 0.2                         // the initial step size
    val xp   = VectorD (0, 0)              // the previous location/point vector
    val gp   = VectorD (0, 0)              // the previous gradient vector
*/


            // Perform linesearch in direction d
            debug ("lbfgs", s"before linesearch: d = $d, x = $x")
//          line_search_backtracking (n, x, f, g, s, step, xp, gp, wa)
//          val ret = linesearch (n, x, fx, gx, d, step, xp, gp, w)
            val ret = linesearch (x, fx, gx, d, step)

            fx = ret._1; step = ret._2
            debug ("lbfgs", s"after linesearch: ls = $ls, x = $x")

            if ls < 0 then                                       // linesearch ls < 0 => revert to the previous point
                x  = xp.copy
                gx = gp.copy
                debug ("lbfgs", "return due to linesearch ls < 0")
                return (x, fx, ls) 
            end if
    
            // Compute x and g norms.
            xnorm = x.norm
            gnorm = gx.norm
    
            // TEST for Convergence.  The criterion is given by the following formula:
            //      |g(x)| / \max(1, |x|) < \epsilon
            if xnorm < 1.0 then xnorm = 1.0
            if gnorm / xnorm <= param.epsilon then
                debug ("lbfgs", "return due to convergence")
                return (x, fx, LBFGS_SUCCESS.code)              // convergence
            end if
    
            // TEST for Stopping Criterion.  The criterion is given by the following formula:
            //      |(f(past_x) - f(x))| / f(x) < \delta
            if pf != null then
                if param.past <= k then                                          // does not test the stopping criterion while k < past
                    val rate = (pf(k % param.past) - fx) / fx                    // compute the relative improvement from the past
                    if abs (rate) < param.delta then return (x, fx, LBFGS_STOP.code)  // the stopping criterion
                end if 
                pf(k % param.past) = fx                                          // store the current value of the objective function
            end if
    
            // TEST for Maximum Number of Iterations.
            if param.max_iterations != 0 && param.max_iterations < k+1 then
                return (x, fx, LBFGSERR_MAXIMUMITERATION.code)                   
            end if
    
            // Update vectors s and y:
            //      s_{k+1} = x_{k+1} - x_{k} = \step * d_{k}.
            //      y_{k+1} = g_{k+1} - g_{k}.
            var it = lm(end_)
            it.s = x - xp
            it.y = gx - gp
    
            // Compute scalars ys and yy:
            //      ys = y^t \cdot s = 1 / \rho.
            //      yy = y^t \cdot y.
            // Notice that yy is used for scaling the hessian matrix H_0 (Cholesky factor).
            val ys = it.y dot it.s
            val yy = it.y dot it.y
            it.ys = ys
    
            // Recursive formula to compute dir = -(H \cdot g).
            //      This is described in page 779 of: Jorge Nocedal.
            //      Updating Quasi-Newton Matrices with Limited Storage.
            //      Mathematics of Computation, Vol. 35, No. 151, pp. 773--782, 1980.
            bound = if m <= k then m else k
            k    += 1
            end_  = (end_ + 1) % m
    
            var j = end_
            for i <- 0 until bound do
                j = (j + m - 1) % m                       // if (--j == -1) j = m-1
                it = lm(j)
                it.alpha  = it.s dot d                    // \alpha_{j} = \rho_{j} s^{t}_{j} \cdot q_{k+1}.
                it.alpha /= it.ys
                d = it.y - it.alpha                       // q_{i} = q_{i+1} - \alpha_{i} y_{i}.
            end for
    
            d *= ys / yy
    
            for i <- 0 until bound do
                it = lm(j)
                val beta = (it.y dot d) / it.ys           // \beta_{j} = \rho_{j} y^t_{j} \cdot \gamma_{i}.
                                                          // \gamma_{i+1} = \gamma_{i} + (\alpha_{j} - \beta_{j}) s_{j}.
                d = it.s + (it.alpha - beta)
                j = (j + 1) % m                           // if (++j == m) j = 0
            end for
    
            // Now the search direction d is ready, try step = 1 first.
//            step *= 0.99
//            step = 1.0
        end while
        (x, fx, 0)
    end lbfgs
    
end L_BFGS


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `l_BFGSTest` main function tests the `L_BFGS` object.
 *  > runMain scalation.optimization.l_BFGSTest
 */
@main def l_BFGSTest (): Unit =

    val n  = 2                                             // dimension of the search space
    val x0 = new VectorD (n)                               // starting location

    banner ("\nMinimize: (x_0 - 3)^2 + (x_1 - 4)^2 + 1")

    def ff (x: VectorD): Double  = (x(0) - 3)~^2 + (x(1) - 4)~^2 + 1.0
    def gf (x: VectorD): VectorD = VectorD (2*x(0) - 6, 2*x(1) - 8)

    val (x, fx, code) = L_BFGS.lbfgs (n, x0, ff, gf)
    println (s"optimal solution x = $x with an objective value f(x) = $fx, with status code $code")

end l_BFGSTest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `l_BFGSTest2` main function is used to test the `L_BFGS` class.
 *  > runMain scalation.optimization.l_BFGSTest2
 */
@main def l_BFGSTest2 (): Unit =

    val n  = 2                                             // dimension of the search space
    val x0 = new VectorD (n)                               // starting location

    banner ("\nMinimize: x_0^4 + (x_0 - 3)^2 + (x_1 - 4)^2 + 1")

    def ff (x: VectorD): Double = x(0)~^4 + (x(0) - 3.0)~^2 + (x(1) - 4.0)~^2 + 1.0
    def gf (x: VectorD): VectorD = VectorD (4*x(0)~^3 + 2*x(0) - 6, 2*x(1) - 8)

    val (x, fx, code) = L_BFGS.lbfgs (n, x0, ff, gf)
    println (s"optimal solution x = $x with an objective value f(x) = $fx, with status code $code")

end l_BFGSTest2


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `l_BFGSTest3` main function is used to test the `L_BFGS` class.
 *  > runMain scalation.optimization.l_BFGSTest3
 */
@main def l_BFGSTest3 (): Unit =

    val n  = 2                                             // dimension of the search space
    val x0 = VectorD (0.1, 0.0)                            // starting location

    banner ("\nMinimize: 1/x(0) + x_0^4 + (x_0 - 3)^2 + (x_1 - 4)^2 + 1")

    def ff (x: VectorD): Double = 1/x(0) +  x(0)~^4 + (x(0) - 3.0)~^2 + (x(1) - 4.0)~^2 + 1.0
    def gf (x: VectorD): VectorD = VectorD (-x(0)~^(-2) + 4*x(0)~^3 + 2*x(0) - 6, 2*x(1) - 8)

    val (x, fx, code) = L_BFGS.lbfgs (n, x0, ff, gf)
    println (s"optimal solution x = $x with an objective value f(x) = $fx, with status code $code")

end l_BFGSTest3

