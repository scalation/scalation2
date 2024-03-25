
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Hao Peng, André Laranjeira
 *  @version 2.0
 *  @note    Fri Sep 30 13:37:32 EDT 2011
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Broyden–Fletcher–Goldfarb–Shanno (BFGS) Quasi-Newton Optimizer
 *
 *  @see The Superlinear Convergence of a Modified BFGS-Type Method for Unconstrained Optimization
 *  @see On the Robustness of Conjugate-Gradient Methods and Quasi-Newton Methods
 *  @see Limited Memory BFGS for Nonsmooth Optimization
 *  @see http://en.wikipedia.org/wiki/BFGS_method
 *  @see http://www.personal.psu.edu/cxg286/Math555.pdf
 *  @see http://people.orie.cornell.edu/aslewis/publications/bfgs_inexactLS.pdf
 *  @see http://people.orie.cornell.edu/aslewis/publications/bfgs_exactLS.pdf
 */

// Package definition.
package scalation
package optimization
package quasi_newton

// General imports.
import scala.math.{abs, max}
import scala.util.control.Breaks.{break, breakable}

// Project imports.
import scalation.calculus.Differential.∇
import scalation.mathstat.*
import scalation.optimization.functions.*

// Simplifying imports.
import MatrixD.eye
import QNewton.aHi_inc

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BFGS` the class implements the Broyden–Fletcher–Goldfarb–Shanno (BFGS)
 *  Quasi-Newton Algorithm for solving Non-Linear Programming (NLP) problems.
 *  BFGS determines a search direction by deflecting the steepest descent direction
 *  vector (opposite the gradient) by  multiplying it by a matrix that approximates
 *  the inverse Hessian.  Note, this  implementation may be set up to work with the
 *  matrix b (approximate Hessian) or directly with the aHi matrix (the inverse of b).
 *
 *  minimize    f(x)
 *  subject to  g(x) <= 0   [ optionally g(x) == 0 ]
 *
 *  @param f        the objective function to be minimized
 *  @param g        the constraint function to be satisfied, if any
 *  @param ineq     whether the constraint is treated as inequality (default) or equality
 *  @param exactLS  whether to use exact (e.g., `GoldenLS`)
 *                            or inexact (e.g., `WolfeLS`) Line Search
 */
class BFGS (f: FunctionV2S, g: FunctionV2S = null,
            ineq: Boolean = true, exactLS: Boolean = false)
      extends Minimizer, PathMonitor:

    private val debug  = debugf ("BFGS", true)              // debug function
    private val flaw   = flawf ("BFGS")                     // flaw function
    private val WEIGHT = 1000.0                             // weight on penalty for constraint violation
    private var bfgs   = true                               // use BFGS (true) or Gradient Descent (false)

    private var df: Array [FunctionV2S] = null              // gradient as explicit functions for partials

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Use the Gradient Descent algorithm rather than the default BFGS algorithm.
     */
    def setSteepest (): Unit = bfgs = false

//  private var b: MatrixD    = null                        // approx. Hessian matrix (use b or aHi)
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /*  Update the b matrix, whose inverse is used to deflect -gradient to a
     *  better direction than steepest descent (-gradient).
     *  @param s  the step vector (next point - current point)
     *  @param y  the difference in the gradients (next - current)
     */
//  def updateB (s: VectorD, y: VectorD): Unit =
//  {
//      var sy = s dot y                                    // dot product of s and y
//      if abs (sy) < TOL then sy = TOL
//      val sb = s * b
//      b += MatrixD.outer (y, y) / sy - MatrixD.outer (sb, sb) / (sb dot s)
//  } // updateB

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the partial derivative functions.  If these functions are available,
     *  they are more efficient and more accurate than estimating the values
     *  using difference quotients (the default approach).
     *  @param grad  the gradient as explicit functions for partials
     */
    def setDerivatives (grad: Array [FunctionV2S]): Unit =
        if g != null then flaw ("setDerivatives", "only works for unconstrained problems")
        df = grad                               // use given functions for partial derivatives
    end setDerivatives

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The objective function f plus a weighted penalty based on the constraint
     *  function g.
     *  @param x  the coordinate values of the current point
     */
    override def fg (x: VectorD): Double =
        val f_x = f(x)
        if g == null then                                  // unconstrained
            f_x
        else                                               // constrained, g(x) <= 0
            val penalty = if ineq then max (g(x), 0.0) else abs (g(x))
            f_x + abs (f_x) * WEIGHT * penalty * penalty
        end if
    end fg

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform an exact GoldenSectionLS or inexact WolfeLS Line Search.
     *  Search in direction dir, returning the distance z to move in that direction.
     *  Default to 
     *  @param x     the current point
     *  @param dir   the direction to move in
     *  @param step  the initial step size
     */
    def lineSearch (x: VectorD, dir: VectorD, step: Double = STEP): Double =
        def f_1D (z: Double): Double = fg(x + dir * z)         // create a 1D function
        val ls = if exactLS then new GoldenSectionLS (f_1D)    // Golden Section Line Search
                 else new WolfeLS (f_1D)                       // Wolfe line search ((c1 = .0001, c2 = .9)
        ls.search (step)                                       // perform a Line Search
    end lineSearch

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve the following Non-Linear Programming (NLP) problem using BFGS:
     *  min { f(x) | g(x) <= 0 }.  To use explicit functions for gradient,
     *  replace "∇ (fg)(xn)" with "gradientD (df, xn)"
     *  @param x0     the starting point 
     *  @param step_  the initial step size
     *  @param toler  the tolerance
     */
    def solve (x0: VectorD, step_ : Double = STEP, toler: Double = TOL): FuncVec =
        debug ("solve", s"x0 = $x0, step_ = $step_, toler = $toler")

        var step = step_                                          // set the current step size
        var x    = (x0, ∇ (fg)(x0))                               // current (point, gradient)
        var xx:  (VectorD, VectorD) = (null, null)                // next (point, gradient)
        var dir: VectorD = null                                   // initial direction is -gradient
        var s:   VectorD = null                                   // step vector

        var aHi = eye (x0.dim, x0.dim)                            // approximate Hessian inverse (aHi) matrix
                                                                  // start with identity matrix

        debug ("solve", s"||gradient||^2 = ${x._2.normSq}")

        var mgn         = 0.0                                     // mean gradient normSq
        var diff        = 0.0                                     // diff between current and next point
        val diffTol     = toler * toler                           // tolerance for changes in diff
        var count       = 0                                       // number of times mgn stayed roughly same (< diffTol)
        val maxCount    = 10                                      // max number of times mgn stayed roughly same => terminate
        val n           = x0.dim                                  // size of the parameter vector
        var goodGrad    = true                                    // good gradient value flag (not NaN nor infinity)
        var xn: VectorD = null                                    // next value for x (point)

        breakable {
            for it <- 1 to MAX_IT do
                debug ("solve", s"start of iteration $it: step = $step, f(x) = ${fg(x._1)}")
                if goodGrad then
                    dir = if bfgs then -(aHi * x._2) else -x._2
                end if
                s  = dir * lineSearch (x._1, dir, step)           // update step vector
                xn = x._1 + s                                     // next x point
                if goodGrad then
                    for xx_i <- xn if xx_i.isNaN || xx_i.isInfinite do break ()
                    diff = (xn - x._1).normSq / n                 // measure of distance moved
                end if
                xx = (xn, ∇ (fg)(xn))                             // compute the next point
                mgn = xx._2.normSq / n                            // compute mean gradient normSq
                debug ("solve", s"current mean gradient normSq = $mgn")

                if mgn.isNaN || mgn.isInfinite then
                    goodGrad = false                              // gradient blew up
                    step /= 2.0                                   // halve the step size 
                else if mgn < toler || count > maxCount then { x = xx; break () }  // return when vanished gradient or haven't moved
                else if goodGrad then
                    if diff < diffTol then count += 1             // increment no movement counter
                    if step < step_   then step  *= 1.5           // increase step size by 50%
                else
                    goodGrad = true                               // gradient is currently fine
                end if

                if goodGrad then
                    if bfgs then aHi += aHi_inc (aHi, s, xx._2 - x._2)     // update the deflection matrix aHi
                    debug ("solve", s"(it = $it) move from ${x._1} to ${xx._1} where fg(xx._1) = ${fg(xx._1)}")
                    x = xx                                        // make the next point the current point
                end if
            end for
        } // breakable
        (fg(x._1), x._1)                                          // return functional value and current point
    end solve

   //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve for an optima by finding a local optima close to the starting point/guess 'x0'.
     *  This version uses explicit functions for the gradient (partials derivatives).
     *  @param x0     the starting point/guess
     *  @param grad   the gradient as explicit functions for partials
     *  @param step_  the initial step size
     *  @param toler  the tolerance
     */
    def solve2 (x0: VectorD, grad: FunctionV2V, step_ : Double = STEP, toler: Double = TOL): FuncVec =
        debug ("solve2", s"x0 = $x0, step_ = $step_, toler = $toler")
        clearPath()

        var step = step_                                          // set the current step size
        var x    = (x0, grad (x0))                                // current (point, gradient)
        var xx:  (VectorD, VectorD) = (null, null)                // next (point, gradient)
        var dir: VectorD = null                                   // initial direction is -gradient
        var s:   VectorD = null                                   // step vector

        var aHi = eye (x0.dim, x0.dim)                            // approximate Hessian inverse (aHi) matrix
                                                                  // start with identity matrix
        add2Path(x._1)

        debug ("solve2", s"||gradient||^2 = ${x._2.normSq}")

        var mgn         = 0.0                                     // mean gradient normSq
        var diff        = 0.0                                     // diff between current and next point
        val diffTol     = toler * toler                           // tolerance for changes in diff
        var count       = 0                                       // number of times mgn stayed roughly same (< diffTol)
        val maxCount    = 10                                      // max number of times mgn stayed roughly same => terminate
        val n           = x0.dim                                  // size of the parameter vector
        var goodGrad    = true                                    // good gradient value flag (not NaN nor infinity)
        var xn: VectorD = null                                    // next value for x (point)

        breakable {
            for it <- 1 to MAX_IT do
                debug ("solve2", s"start of iteration $it: step = $step, f(x) = ${fg(x._1)}")
                if goodGrad then
                    dir = if bfgs then -(aHi * x._2) else -x._2
                end if
                s  = dir * lineSearch (x._1, dir, step)           // update step vector
                xn = x._1 + s                                     // next x point
                if goodGrad then
                    for xx_i <- xn if xx_i.isNaN || xx_i.isInfinite do break ()
                    diff = (xn - x._1).normSq / n                 // measure of distance moved
                end if
                xx = (xn, grad (xn))                              // compute the next point
                mgn = xx._2.normSq / n                            // compute mean gradient normSq
                debug ("solve2", s"current mean gradient normSq = $mgn")

                if mgn.isNaN || mgn.isInfinite then
                    goodGrad = false                              // gradient blew up
                    step /= 2.0                                   // halve the step size
                else if mgn < toler || count > maxCount then { x = xx; break () }  // return when vanished gradient or haven't moved
                else if goodGrad then
                    if diff < diffTol then count += 1             // increment no movement counter
                    if step < step_   then step  *= 1.5           // increase step size by 50%
                else
                    goodGrad = true                               // gradient is currently fine
                end if

                if goodGrad then
                    if bfgs then aHi += aHi_inc (aHi, s, xx._2 - x._2)     // update the deflection matrix aHi
                    debug ("solve2", s"(it = $it) move from ${x._1} to ${xx._1} where fg(xx._1) = ${fg(xx._1)}")
                    x = xx                                        // make the next point the current point
                end if

                add2Path(x._1)
            end for
        } // breakable
        (fg(x._1), x._1)                                          // return functional value and current point
    end solve2

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve for an optima by finding a local optima close to the starting
     *  point/guess 'x0'.
     *  This version uses explicit functions for the gradient (partials
     *  derivatives) and the line search algorithm implementations developed for
     *  L-BFGS (except for `BacktrackingOrthantWise`, which is currently NOT
     *  supported). More details can be found in [[LBFGSLineSearchAlgorithm]].
     *
     *  @param x0                   The starting point/guess.
     *  @param grad                 The gradient as explicit functions for
     *                              partials.
     *  @param toler                The tolerance.
     *  @param lineSearchAlgorithm  [[LBFGSLineSearchAlgorithm]] representing
     *                              the line search algorithm chosen for the
     *                              optimization. Cannot be
     *                              `BacktrackingOrthantWise`, as it is not
     *                              currently supported.
     *  @param lineSearchParams     [[LBFGSLineSearchParameters]] representing
     *                              the parameters that control the line search
     *                              execution.
     */
    def solve3 (
        x0: VectorD,
        grad: FunctionV2V,
        toler: Double = TOL,
        lineSearchAlgorithm: LBFGSLineSearchAlgorithm = LBFGSLineSearchAlgorithm.Default,
        lineSearchParams: LBFGSLineSearchParameters = LBFGSLineSearchParameters()
    ): FuncVec =
        debug ("solve3", s"x0 = $x0, toler = $toler")
        clearPath()

        var x    = (x0, grad (x0))                                // current (point, gradient)
        var xx:  (VectorD, VectorD) = (null, null)                // next (point, gradient)
        var dir: VectorD = null                                   // initial direction is -gradient
        var s:   VectorD = null                                   // step vector
        add2Path(x._1)

        if lineSearchAlgorithm == LBFGSLineSearchAlgorithm.BacktrackingOrthantWise then
            debug("solve3", "orthantwise is not currently supported by BFGS")
            return (fg(x._1), x._1)
        end if

        val lineSearchImplementation = LBFGSLineSearch.getImplementation(lineSearchAlgorithm)
        var aHi = eye (x0.dim, x0.dim)                            // approximate Hessian inverse (aHi) matrix
        // start with identity matrix

        debug ("solve3", s"||gradient||^2 = ${x._2.normSq}")

        var mgn         = 0.0                                     // mean gradient normSq
        var diff        = 0.0                                     // diff between current and next point
        val diffTol     = toler * toler                           // tolerance for changes in diff
        var count       = 0                                       // number of times mgn stayed roughly same (< diffTol)
        val maxCount    = 10                                      // max number of times mgn stayed roughly same => terminate
        val n           = x0.dim                                  // size of the parameter vector
        var xn: VectorD = null                                    // next value for x (point)
        val dampeningCoefficient = 0.99                           // how much the increase to aHi should be dampened

        dir = if bfgs then -(aHi * x._2) else -x._2
        var step = 1 / dir.norm                                   // set the current step size

        breakable {
            for it <- 1 to MAX_IT do
                debug ("solve3", s"start of iteration $it: step = $step, f(x) = ${fg(x._1)}")
                dir = if bfgs then -(aHi * x._2) else -x._2

                lineSearchImplementation.lineSearch(
                    n,
                    x._1,
                    f(x._1),
                    grad(x._1),
                    dir,
                    step,
                    LBFGSCallbackData(n, None, FunctionEvaluation(f, grad)),
                    lineSearchParams
                ) match
                    case lineSearchStep: LBFGSLineSearchStep =>
                        s = dir * lineSearchStep.step
                    case lineSearchFailure: LBFGSLineSearchFailure =>
                        debug("solve3", s"line search failed: $lineSearchFailure")
                        break()

                xn = x._1 + s                                     // next x point

                for xx_i <- xn if xx_i.isNaN || xx_i.isInfinite do break ()
                diff = s.normSq / n                 // measure of distance moved

                xx = (xn, grad (xn))                              // compute the next point
                mgn = xx._2.normSq / n                            // compute mean gradient normSq
                debug ("solve3", s"current mean gradient normSq = $mgn")

                if mgn < toler || count > maxCount then
                    x = xx
                    break ()    // return when vanished gradient or haven't moved
                else if diff < diffTol then
                    count += 1             // increment no movement counter
                end if

                if bfgs then aHi += aHi_inc (aHi, s, xx._2 - x._2) * dampeningCoefficient    // update the deflection matrix aHi
                debug ("solve3", s"(it = $it) move from ${x._1} to ${xx._1} where fg(xx._1) = ${fg(xx._1)}")
                x = xx                                        // make the next point the current point

                step = lineSearchParams.defaultStep
                add2Path(x._1)
            end for
        } // breakable

        (fg(x._1), x._1)                                          // return functional value and current point
    end solve3

end BFGS


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BFGS` companion object provides factory methods.
 */
object BFGS:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a Steepest Descent (default) or BFGS optimizer.
     *  @param f         the objective function to be minimized
     *  @param g         the constraint function to be satisfied, if any
     *  @param ineq      whether the constraint is treated as inequality (default) or equality
     *  @param exactLS   whether to use exact (e.g., `GoldenLS`)
     *                             or inexact (e.g., `WolfeLS`) Line Search
     *  @param steepest  whether to use Steepest Descent rather than BFGS
     */
    def apply (f: FunctionV2S, g: FunctionV2S = null,
               ineq: Boolean = true, exactLS: Boolean = false,
               steepest: Boolean = true): BFGS =
        if steepest then
           val steep = new BFGS (f, f, ineq, exactLS)
           steep.setSteepest ()
           steep
        else
           new BFGS (f, f, ineq, exactLS)
        end if
    end apply

end BFGS


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `bFGSTest` main function is used to test the `BFGS` class on f(x):
 *      f(x) = (x_0 - 3)^2^ + (x_1 - 4)^2^ + 1
 *  {{{
 *  > runMain scalation.optimization.bFGSTest
 *  }}}
 */
@main def bFGSTest (): Unit =

    val step = 1.0                                          // step size (may need adjustment)
    val n    = 2                                            // dimension of the search space
    val x0   = new VectorD (n)                              // starting point

    banner ("Minimize: (x_0 - 3)^2 + (x_1 - 4)^2 + 1")
    def f = ParaboloidExampleFunction.objectiveFunction
    def grad = ParaboloidExampleFunction.gradientFunction

    val optimizer = new BFGS (f)
//  val opt = optimizer.solve (x0, step)                    // use numerical partials
    val opt = optimizer.solve2 (x0, grad, step)             // use functions for partials
    println (s"][ optimal solution (f(x), x) = $opt")

end bFGSTest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `bFGSTest2` main function is used to test the `BFGS` class on f(x):
 *      f(x) = x_0^4^ + (x_0 - 3)^2^ + (x_1 - 4)^2^ + 1
 *  {{{
 *  > runMain scalation.optimization.bFGSTest2
 *  }}}
 */
@main def bFGSTest2 (): Unit =

    val step = 1.0                                          // step size (may need adjustment)
    val n  = 2                                              // dimension of the search space
    val x0 = new VectorD (n)                                // starting point

    banner ("Minimize: x_0^4 + (x_0 - 3)^2 + (x_1 - 4)^2 + 1")
    def f = QuarticExampleFunction.objectiveFunction
    def grad = QuarticExampleFunction.gradientFunction

    val optimizer = new BFGS (f)
//  val opt = optimizer.solve (x0, step)                    // use numerical partials
    val opt = optimizer.solve2 (x0, grad, step)             // use functions for partials
    println (s"][ optimal solution (f(x), x) = $opt")

end bFGSTest2


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `bFGSTest3` main function is used to test the `BFGS_NoLS` class.
 *  This test uses the Rosenbrock function.
        f(x) = (1 - x_0)^2^ + 100 (x_1 - x_0^2^)^2^")
 *  {{{
 *  > runMain scalation.optimization.bFGSTest3
 *  }}}
 */
@main def bFGSTest3 (): Unit =

    val step = 1.0                                          // step size (may need adjustment)
    val n    = 2                                            // dimension of the search space
    val x0   = new VectorD (n)                              // starting point

    banner ("Minimize: (1 - x_0)^2 + 100 (x_1 - x_0^2)^2")
    def f = RosenbrockFunction.objectiveFunction
    def grad = RosenbrockFunction.gradientFunction

    val optimizer = new BFGS (f)
//  val opt = optimizer.solve (x0, step)                    // use numerical partials
    val opt = optimizer.solve2 (x0, grad, step)             // use functions for partials
    println (s"][ optimal solution (f(x), x) = $opt")

end bFGSTest3


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `bFGSTest4` main function is used to test the `BFGS` class on f(x):
 *      f(x) = 1/x(0) + x_0^4^ + (x_0 - 3)^2^ + (x_1 - 4)^2^ + 1
 *  {{{
 *  > runMain scalation.optimization.bFGSTest4
 *  }}}
 */
@main def bFGSTest4 (): Unit =  

    val step = 1.0                                          // step size (may need adjustment)
    val x0   = VectorD (0.1, 0.0)                           // starting location

    banner ("Minimize: 1/x(0) + x_0^4 + (x_0 - 3)^2 + (x_1 - 4)^2 + 1")
    def f = ReciprocalExampleFunction.objectiveFunction
    def grad = ReciprocalExampleFunction.gradientFunction

    val optimizer = new BFGS (f)
//  val opt = optimizer.solve (x0, step)                    // use numerical partials
    val opt = optimizer.solve2 (x0, grad, step)             // use functions for partials
    println (s"][ optimal solution (f(x), x) = $opt")

//  opt = optimizer.resolve (n)                             // try multiple starting points
//  println (s"][ optimal solution (f(x), x) = $opt")

end bFGSTest4

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `bFGSBealeFunction` main function is used to test the `BFGS` class on f(x):
 *      f(x) = (1.5 - x(0) + x(0)*x(1))^2^ + (2.25 - x(0) + x(0)*(x(1)^2^))^2^ + (2.625 - x(0) + x(0)*(x(1)^3^))^2^
 *  {{{
 *  > runMain scalation.optimization.bFGSBealeFunction
 *  }}}
 */
@main def bFGSBealeFunction (): Unit =
//    val functionDomainLowerBound = VectorD(-10, -10)
//    val functionDomainUpperBound = VectorD(10, 10)

    def f = BealeFunction.objectiveFunction
    def grad = BealeFunction.gradientFunction
    val optimizer = new BFGS (f)

    banner("-----------------Test 1--------------------------------------")
    val opt = optimizer.solve3 (VectorD(3, 0), grad, lineSearchParams = LBFGSLineSearchParameters(defaultStep=4))
    println (s"][ optimal solution (f(x), x) = $opt")
    banner("-----------------Test 2--------------------------------------")
    val opt2 = optimizer.solve3 (VectorD(2, -1), grad, lineSearchParams = LBFGSLineSearchParameters(defaultStep=4))
    println (s"][ optimal solution (f(x), x) = $opt2")
    banner("-----------------Test 3--------------------------------------")
    val opt3 = optimizer.solve3 (VectorD(0, 1), grad, lineSearchParams = LBFGSLineSearchParameters(defaultStep=4))
    println (s"][ optimal solution (f(x), x) = $opt3")
    banner("-----------------Test 4--------------------------------------")
    val opt4 = optimizer.solve3 (VectorD(-2, -1), grad, lineSearchParams = LBFGSLineSearchParameters(defaultStep=4))
    println (s"][ optimal solution (f(x), x) = $opt4")

//    val plot = new PlotC(BealeFunction.objectiveFunction, functionDomainLowerBound, functionDomainUpperBound, optimizer.getPath, BealeFunction.functionMinimum)
//    writeImage("./plots/BFGS/BFGS_bealeFunction_plot.png", plot)

//  opt = optimizer.resolve (n)                             // try multiple starting points
//  println (s"][ optimal solution (f(x), x) = $opt")

end bFGSBealeFunction

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `bFGSBoothFunction` main function is used to test the `BFGS` class on f(x):
 *      f(x) = (x(0) + 2 * x(1) - 7)^2^ + (2 * x(0) + x(1) - 5)^2^
 *  {{{
 *  > runMain scalation.optimization.bFGSBoothFunction
 *  }}}
 */
@main def bFGSBoothFunction (): Unit =

    def f = BoothFunction.objectiveFunction
    def grad = BoothFunction.gradientFunction
    val optimizer = new BFGS (f)

    banner("-----------------Test 1--------------------------------------")
    val opt = optimizer.solve3 (VectorD(2, 1), grad, lineSearchParams = LBFGSLineSearchParameters(defaultStep=4))
    println (s"][ optimal solution (f(x), x) = $opt")
    banner("-----------------Test 2--------------------------------------")
    val opt2 = optimizer.solve3 (VectorD(-1, 5), grad, lineSearchParams = LBFGSLineSearchParameters(defaultStep=4))
    println (s"][ optimal solution (f(x), x) = $opt2")
    banner("-----------------Test 3--------------------------------------")
    val opt3 = optimizer.solve3 (VectorD(-5, 5), grad, lineSearchParams = LBFGSLineSearchParameters(defaultStep=4))
    println (s"][ optimal solution (f(x), x) = $opt3")
    banner("-----------------Test 4--------------------------------------")
    val opt4 = optimizer.solve3 (VectorD(-5, -2), grad, lineSearchParams = LBFGSLineSearchParameters(defaultStep=4))
    println (s"][ optimal solution (f(x), x) = $opt4")

//  opt = optimizer.resolve (n)                             // try multiple starting points
//  println (s"][ optimal solution (f(x), x) = $opt")

end bFGSBoothFunction

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `bFGSCamel3Function` main function is used to test the `BFGS` class on f(x):
 *      f(x) = 2*x(0)^2^ - 1.05*x(0)^4^ + (1/6.0)*x(0)^6^ + x(0)*x(1) + x(1)^2^
 *  {{{
 *  > runMain scalation.optimization.bFGSCamel3Function
 *  }}}
 */
@main def bFGSCamel3Function (): Unit =

    def f = Camel3Function.objectiveFunction
    def grad = Camel3Function.gradientFunction
    val optimizer = new BFGS (f)

    banner("-----------------Test 1--------------------------------------")
    val opt = optimizer.solve3 (VectorD(2, 2), grad, lineSearchParams = LBFGSLineSearchParameters(defaultStep=4))
    println (s"][ optimal solution (f(x), x) = $opt")
    banner("-----------------Test 2--------------------------------------")
    val opt2 = optimizer.solve3 (VectorD(-3, -3), grad, lineSearchParams = LBFGSLineSearchParameters(defaultStep=4))
    println (s"][ optimal solution (f(x), x) = $opt2")
    banner("-----------------Test 3--------------------------------------")
    val opt3 = optimizer.solve3 (VectorD(4, -4), grad, lineSearchParams = LBFGSLineSearchParameters(defaultStep=4))
    println (s"][ optimal solution (f(x), x) = $opt3")
    banner("-----------------Test 4--------------------------------------")
    val opt4 = optimizer.solve3 (VectorD(-5, 5), grad, lineSearchParams = LBFGSLineSearchParameters(defaultStep=4))
    println (s"][ optimal solution (f(x), x) = $opt4")

//  opt = optimizer.resolve (n)                             // try multiple starting points
//  println (s"][ optimal solution (f(x), x) = $opt")

end bFGSCamel3Function

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `bFGSCubeFunction` main function is used to test the `BFGS` class on f(x):
 *      f(x) = 100*(x(1) - x(0)^3^)^2^ + (1-x(0))^2^
 *  {{{
 *  > runMain scalation.optimization.bFGSCubeFunction
 *  }}}
 */
@main def bFGSCubeFunction (): Unit =

    def f = CubeFunction.objectiveFunction
    def grad = CubeFunction.gradientFunction
    val optimizer = new BFGS (f)

    banner("-----------------Test 1--------------------------------------")
    val opt = optimizer.solve3 (VectorD(0, 0), grad, lineSearchParams = LBFGSLineSearchParameters(defaultStep=4))
    println (s"][ optimal solution (f(x), x) = $opt")
    banner("-----------------Test 2--------------------------------------")
    val opt2 = optimizer.solve3 (VectorD(3, 3), grad, lineSearchParams = LBFGSLineSearchParameters(defaultStep=4))
    println (s"][ optimal solution (f(x), x) = $opt2")
    banner("-----------------Test 3--------------------------------------")
    val opt3 = optimizer.solve3 (VectorD(4, 5), grad, lineSearchParams = LBFGSLineSearchParameters(defaultStep=4))
    println (s"][ optimal solution (f(x), x) = $opt3")
    banner("-----------------Test 4--------------------------------------")
    val opt4 = optimizer.solve3 (VectorD(5, -5), grad, lineSearchParams = LBFGSLineSearchParameters(defaultStep=4))
    println (s"][ optimal solution (f(x), x) = $opt4")

//  opt = optimizer.resolve (n)                             // try multiple starting points
//  println (s"][ optimal solution (f(x), x) = $opt")

end bFGSCubeFunction


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `bFGSFreudensteinRothFunction` main function is used to test the `BFGS` class on f(x):
 *      f(x) = (x(0) - 13 + x(1)*((5-x(1))*x(1) -2))^2^ + (x(0) -29 + x(1)*((x(1) + 1)*x(1) -14))^2^
 *  {{{
 *  > runMain scalation.optimization.bFGSFreudensteinRothFunction
 *  }}}
 */
@main def bFGSFreudensteinRothFunction (): Unit =
    def f =  FreudensteinRothFunction.objectiveFunction
    def grad = FreudensteinRothFunction.gradientFunction
    val optimizer = new BFGS (f)

    banner("-----------------Test 1--------------------------------------")
    val opt = optimizer.solve3 (VectorD(5, 7), grad, lineSearchParams = LBFGSLineSearchParameters(defaultStep=4))
    println (s"][ optimal solution (f(x), x) = $opt")
    banner("-----------------Test 2--------------------------------------")
    val opt2 = optimizer.solve3 (VectorD(3, 2), grad, lineSearchParams = LBFGSLineSearchParameters(defaultStep=4))
    println (s"][ optimal solution (f(x), x) = $opt2")
    banner("-----------------Test 3--------------------------------------")
    val opt3 = optimizer.solve3 (VectorD(1, 0), grad, lineSearchParams = LBFGSLineSearchParameters(defaultStep=4))
    println (s"][ optimal solution (f(x), x) = $opt3")
    banner("-----------------Test 4--------------------------------------")
    val opt4 = optimizer.solve3 (VectorD(-1, -2), grad, lineSearchParams = LBFGSLineSearchParameters(defaultStep=4))
    println (s"][ optimal solution (f(x), x) = $opt4")

//  opt = optimizer.resolve (n)                             // try multiple starting points
//  println (s"][ optimal solution (f(x), x) = $opt")

end bFGSFreudensteinRothFunction

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `bFGSFreudensteinRothFunction` main function is used to test the `BFGS` class on f(x):
 *      f(x) = (x(0) - 13 + x(1)*((5-x(1))*x(1) -2))^2^ + (x(0) -29 + x(1)*((x(1) + 1)*x(1) -14))^2^
 *  {{{
 *  > runMain scalation.optimization.bFGSFreudensteinRothFunction
 *  }}}
 */
@main def bFGSMcCormickFunction (): Unit =

    def f = McCormickFunction.objectiveFunction
    def grad = McCormickFunction.gradientFunction
    val optimizer = new BFGS (f)

    banner("-----------------Test 1--------------------------------------")
    val opt = optimizer.solve3 (VectorD(-1, -1), grad, lineSearchParams = LBFGSLineSearchParameters(defaultStep=4))
    println (s"][ optimal solution (f(x), x) = $opt")
    banner("-----------------Test 2--------------------------------------")
    val opt2 = optimizer.solve3 (VectorD(0, 0), grad, lineSearchParams = LBFGSLineSearchParameters(defaultStep=4))
    println (s"][ optimal solution (f(x), x) = $opt2")
    banner("-----------------Test 3--------------------------------------")
    val opt3 = optimizer.solve3 (VectorD(0, 2), grad, lineSearchParams = LBFGSLineSearchParameters(defaultStep=4))
    println (s"][ optimal solution (f(x), x) = $opt3")
    banner("-----------------Test 4--------------------------------------")
    val opt4 = optimizer.solve3 (VectorD(3, 3), grad, lineSearchParams = LBFGSLineSearchParameters(defaultStep=4))
    println (s"][ optimal solution (f(x), x) = $opt4")

//  opt = optimizer.resolve (n)                             // try multiple starting points
//  println (s"][ optimal solution (f(x), x) = $opt")

end bFGSMcCormickFunction
