
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Hao Peng
 *  @version 2.0
 *  @date    Fri Oct 7 12:27:00 EDT 2017
 *  @see     LICENSE (MIT style license file).
 *------------------------------------------------------------------------------
 *  Limited memory Broyden–Fletcher–Goldfarb–Shanno (BFGS) for Bound constrained
 *  optimization (L-BFGS-B) algorithm. Originally proposed by Byrd et. al in 1995.
 *  See the first two links for the original paper and authors' software (written
 *  in Fortran) distribution site, respectively. This implementation is translated
 *  from a C++ implementation found in the last link.
 *  @see www.ece.northwestern.edu/~nocedal/PSfiles/limited.ps.gz
 *  @see users.iems.northwestern.edu/~nocedal/lbfgsb.html
 *  @see github.com/PatWie/CppNumericalSolvers/blob/master/include/cppoptlib/solver/lbfgsbsolver.h
 */

package scalation
package optimization

import scala.collection.mutable.ArrayBuffer
import scala.math.{abs, max, min}
import scala.util.control.Breaks.{break, breakable}

import scalation.calculus.Differential.∇
import scalation.mathstat._

import MatrixD.eye

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `L_BFGS_B` the class implements the Limited memory Broyden–Fletcher–
 *  Goldfarb–Shanno for Bound constrained optimization (L-BFGS-B)
 *  Quasi-Newton Algorithm for solving Non-Linear Programming (NLP) problems.
 *  L-BFGS-B determines a search direction by  deflecting the steepest descent direction
 *  vector (opposite the gradient) by *  multiplying it by a matrix that approximates
 *  the inverse Hessian. Furthermore, only a few vectors represent the approximation
 *  of the Hessian Matrix (limited memory). The parameters estimated are also bounded
 *  within user specified lower and upper bounds.
 *
 *  minimize    f(x)
 *  subject to  g(x) <= 0   [ optionally g(x) == 0 ]
 *
 *  @param f        the objective function to be minimized
 *  @param g        the constraint function to be satisfied, if any
 *  @param ineq     whether the constraint is treated as inequality (default) or equality
 *  @param exactLS  whether to use exact (e.g., `GoldenLS`)
 *                            or inexact (e.g., `WolfeLS`) Line Search
 *  @param l        vector of lower bounds for all input parameters
 *  @param u        vector of upper bounds for all input parameters
 */
class L_BFGS_B (f: FunctionV2S, g: FunctionV2S = null, ineq: Boolean = true, exactLS: Boolean = false,
                private var l: VectorD = null, private var u: VectorD = null)
      extends Minimizer:

    private val debug           = debugf ("L-BFGS", true)     // the debug flag
    private val WEIGHT          = 1000.0                      // weight on penalty for constraint violation
    private var ww, mm: MatrixD = null                        // workspace matrices
    private var theta           = 0.0                         // a scaling parameter
    private var dim             = 0                           // dimension of the input vector
    private var hs              = 5                           // history size, number of historical vectors to store

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sort pairs (k, v) according to v into ascending order.
     *  @param v  ArrayBuffer of Tuple2 to be sorted by the 2nd element
     */
    private def sortIndices (v: ArrayBuffer [(Int, Double)]): VectorI =
        val sv  = v.sortBy (_._2)
        val idx = new VectorI (sv.length)
        for i <- idx.indices do idx(i) = sv(i)._1
        idx
    end sortIndices

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Algorithm CP: Computation of the Generalized Cauchy Point. See page 8 of
     *  @see www.ece.northwestern.edu/~nocedal/PSfiles/limited.ps.gvz
     *  @param x   the parameter vector
     *  @param gr  the gradient vector
     */
    private def getGCP (x: VectorD, gr: VectorD): (VectorD, VectorD) =
        debug ("getGCP", s"x = $x, gr = $gr")

        val setOfT = new ArrayBuffer [(Int, Double)] ()
        val d      = -gr
        for j <- 0 until dim do
            if gr(j) == 0 then setOfT.append ((j, MAX_VALUE))
            else
                val tmp = if gr(j) < 0 then (x(j) - u(j)) / gr(j)
                          else (x(j) - l(j)) / gr(j)
                setOfT.append ((j, tmp))
                if tmp == 0 then d(j) = 0
            end if
        end for
        val sortedIndices = sortIndices (setOfT)
        val xCauchy       = x.copy

        val p             = ww.transpose * d
        val c             = new VectorD (ww.dim2)
        var fPrime        = -d dot d
        var fDoublePrime  = max (-theta * fPrime - (p dot mm * p), EPSILON)
        val f_dp_orig     = fDoublePrime
        var dt_min        = -fPrime / fDoublePrime
        var t_old         = 0.0

        var i = 0
        breakable {
            for j <- 0 until dim do
                i = j
                if setOfT (sortedIndices(j))._2 > 0 then break ()
            end for
        } // breakable
        var b  = sortedIndices (i)
        var t  = setOfT (b)._2
        var dt = t

        while dt_min >= dt && i < dim do
            xCauchy(b)   =  if d(b) > 0      then u(b)
                            else if d(b) < 0 then l(b)
                            else xCauchy(b)
            val zb        = xCauchy(b) - x(b)
            c            += p * dt
            val wbt       = ww(b)
            fPrime       += dt * fDoublePrime + gr(b) * gr(b) + theta * gr(b) * zb - gr(b) * (wbt dot mm * c)
            fDoublePrime += -theta  *  gr(b) *  gr(b) - 2.0   * (gr(b) * (wbt dot mm * p))
                            - gr(b) *  gr(b) * (wbt dot mm * wbt)
            fDoublePrime  = max (EPSILON * f_dp_orig, fDoublePrime)
            p            += wbt * gr(b)
            d(b)          = 0
            dt_min        = -fPrime / fDoublePrime
            t_old         = t
            i            += 1
            if i < dim then
                b  = sortedIndices (i)
                t  = setOfT (b)._2
                dt = t - t_old
            end if
        end while

        dt_min = max (dt_min, 0.0)
        t_old += dt_min

        for ii <- i until xCauchy.dim do
            val si      = sortedIndices (ii)
            xCauchy(si) = x(si) + t_old * d(si)
        end for
        c += p * dt_min
        (xCauchy, c)
    end getGCP

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the alpha* parameter, a positive scalar.  See Equation 5.8 on page 11 of
     *  @see www.ece.northwestern.edu/~nocedal/PSfiles/limited.ps.gvz
     *  @param x_cp     vector of cauchy point
     *  @param du       vector containing intermediate results used to find alpha*
     *  @param freeVar  an ArrayBuffer storing the indices of free variable
     */
    private def findAlpha (x_cp: VectorD, du: VectorD, freeVar: ArrayBuffer [Int]): Double =
        debug ("findAlpha", s"x_cp = $x_cp, du = $du, freeVar = $freeVar")

        var alphastar = 1.0
        val n         = freeVar.size
        assert (du.dim == n)
        for i <- 0 until n do
            val fi = freeVar(i)
            alphastar = if du(i) > 0 then min (alphastar, (u(fi) - x_cp(fi)) / du(i))
                        else min (alphastar, (l(fi) - x_cp(fi)) / du(i))
        end for
        alphastar
    end findAlpha

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Minimization of the subspace of free variables.  See Section 5 on page 9 of
     *  @see www.ece.northwestern.edu/~nocedal/PSfiles/limited.ps.gvz
     *  @param x        the parameter vector
     *  @param gr       the gradient vector
     *  @param xCauchy  the vector of cauchy points
     *  @param c        vector obtained from getGCP used to initialize the subspace
     *                  minimization process
     */
    private def subspaceMinimize (x: VectorD, gr: VectorD, xCauchy: VectorD, c: VectorD): VectorD =
        debug ("subspaceMinimize", s"x = $x, gr = $gr, xCauchy = $xCauchy, c = $c")

        val thetaInverse = 1.0 / theta
        val freeVarIdx   = new ArrayBuffer [Int] ()
        for i <- xCauchy.indices if xCauchy(i) != u(i) && xCauchy(i) != l(i) do freeVarIdx.append (i)
        val freeVarCount = freeVarIdx.size
        val wwzz         = new MatrixD (ww.dim2, freeVarCount)
        for i <- 0 until freeVarCount do wwzz(?, i) = ww(freeVarIdx(i))
        val rr           = (gr + (xCauchy - x) * theta - ww * (mm * c))
        val r            = new VectorD (freeVarCount)
        for i <- 0 until freeVarCount do r(i) = rr(freeVarIdx(i))

        var v  = mm * (wwzz * r)
        var nn = wwzz * wwzz.transpose * thetaInverse
        nn     = eye (nn.dim, nn.dim) - mm * nn

        val lu = new Fac_LU (nn)
        lu.factor ()
        v = lu.solve (v)

        val du          = r * -thetaInverse - wwzz.transpose * v * thetaInverse * thetaInverse
        val alpha_star  = findAlpha (xCauchy, du, freeVarIdx)
        val dStar       = du * alpha_star
        val subspaceMin = xCauchy.copy
        for i <- 0 until freeVarCount do subspaceMin (freeVarIdx(i)) += dStar(i)
        subspaceMin
    end subspaceMinimize

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Obtain the mean gradient norm squared
     *  @param x   the parameter vector
     *  @param gr  the gradient vector
     */
    private def getMgn (x: VectorD, gr: VectorD): Double =
        val x_gr       = x - gr
        val checkLower = VectorD (for i <- l.indices yield max (x_gr(i)      , l(i)))
        val checkUpper = VectorD (for i <- u.indices yield min (checkLower(i), u(i)))
        val mgn        = (checkUpper - x).normSq / dim
        mgn
    end getMgn

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Force the values within 'v' to stay within the pre-defined bounds.
     *  @param v  the Vector containing values to be adjusted
     */
    private def forceBounds (v: VectorD): Unit =
        for i <- v.indices do
            if v(i) > u(i)      then v(i) = u(i)
            else if v(i) < l(i) then v(i) = l(i)
        end for
    end forceBounds

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Modify the number of historical vectors to store.
     *  @param hs_  the new history size
     */
    def setHistorySize (hs_ : Int): Unit = { hs = hs_ }

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The objective function f plus a weighted penalty based on the constraint
     *  function g.
     *  @param x  the coordinate values of the current point
     */
    override def fg (x: VectorD): Double =
        val f_x = f(x)
        if g == null then                             // unconstrained
            f_x
        else                                          // constrained, g(x) <= 0
            val penalty = if ineq then max (g(x), 0.0) else abs (g(x))
            f_x + abs (f_x) * WEIGHT * penalty * penalty
        end if
    end fg

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform an exact `GoldenSectionLS` or inexact `WolfeLS` Line Search.
     *  Search in direction dir, returning the distance z to move in that direction.
     *  @param x     the current point
     *  @param dir   the direction to move in
     *  @param step  the initial step size
     */
    def lineSearch (x: VectorD, dir: VectorD, step: Double = STEP): Double =
        debug ("linesearch", s"x = $x, dir = $dir, step = $step")

        def f_1D (z: Double): Double = fg(x + dir * z)          // create a 1D function
        val ls = if exactLS then new GoldenSectionLS (f_1D )    // Golden Section Line Search
                 else new WolfeLS (f_1D)                        // Wolfe line search ((c1 = .0001, c2 = .9)
        ls.search (step)                                        // perform a Line Search
    end lineSearch

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve the following Non-Linear Programming (NLP) problem using L-BFGS_B:
     *  min { f(x) | g(x) <= 0 }.
     *  @param x0           the starting point
     *  @param alphaInit    the initial step size
     *  @param toler        the tolerance
     */
    def solve (x0: VectorD, alphaInit : Double = STEP, toler: Double = TOL): FuncVec =
        debug ("solve", s"x0 = $x0, alphaInit = $alphaInit, toler = $toler")

        var best = (MAX_VALUE, VectorD.nullv)

        dim = x0.size
        theta = 1.0

        if l == null then l = VectorD.fill (dim)(NEGATIVE_INFINITY)
        if u == null then u = VectorD.fill (dim)(POSITIVE_INFINITY)

        ww = new MatrixD (dim, 0)
        mm = new MatrixD (0, 0)

        val yHistory = new ArrayBuffer [VectorD] ()
        val sHistory = new ArrayBuffer [VectorD] ()
        var yHistoryMx: MatrixD = null
        var sHistoryMx: MatrixD = null

        var (x, gr)  = (x0, ∇ (fg)(x0))
        var fv       = fg(x)
        var mgn      = 0.0
        var count    = 0
        val countMax = 10

        breakable {
            for k <- 1 to MAX_IT do
                banner (s"solve: iteration $k: f(x) = $fv, x = $x")
                val f_old   = fv
                val x_old   = x
                val g_old   = gr
                val mgn_old = mgn
    
                // STEP 2: compute the cauchy point
                val (xCauchy, c) = getGCP (x, gr)
                forceBounds (xCauchy)

                // STEP 3: compute a search direction d_k by the primal method for the sub-problem
                val subspaceMin = subspaceMinimize (x, gr, xCauchy, c)
                forceBounds (subspaceMin)

                // STEP 4: perform linesearch
                val rate = lineSearch (x, subspaceMin-x, alphaInit)

                // STEP 5: compute gradient
                x = x - (x - subspaceMin) * rate                            // update current guess and function information
                forceBounds (x)

                fv = fg(x)
                if blown ((fv, x)) then { best = better ((f_old, x_old), best); break () }

                gr  = ∇ (fg)(x)
                mgn = getMgn (x, gr)
                if mgn < toler || count > countMax then { best = better ((fv, x), best); break () }
                if abs (mgn - mgn_old) < toler then count += 1

                val newY = gr - g_old                                       // prepare for next iteration
                val newS = x - x_old

                // STEP 6
                val test = abs (newS dot newY)
                if test > EPSILON * newY.normSq then
                    if yHistory.size >= hs then { yHistory.remove (0); sHistory.remove (0) }
                    yHistory append newY
                    sHistory append newS
                    theta = (newY dot newY) / (newY dot newS)

                    // STEP 7
                    yHistoryMx = MatrixD (yHistory).transpose
                    sHistoryMx = MatrixD (sHistory).transpose
                    ww         = yHistoryMx ++^ (sHistoryMx * theta)
                    val aa     = sHistoryMx.transpose * yHistoryMx
                    val ll     = aa.lower
                    ll(?, ?)   = 0.0                                       // set ll's diagonal to 0
                    val dd     = new MatrixD (aa.dim, aa.dim2)
                    dd.setDiag (-aa(?))                                    // set dd diangonal to aa's
                    val mm2    = (dd ++^ ll.transpose) ++ (ll ++^ (sHistoryMx.transpose * sHistoryMx * theta))
                    mm         = Fac_LU.inverse (mm2)()
                end if

                debug ("solve", s"(k = $k) move from $x_old to $x where fg(x) = $fv")

                best = better ((fv, x), best)
                if abs (f_old - fv) < toler then break ()                  // successive function values too similar
            end for
        } // breakable
        banner (s"solve: optimal solution = $best")
        best
    end solve

end L_BFGS_B


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `l_BFGS_BTest` main function is used to test the `L_BFGS_B` class.
 *      f(x) = (x_0 - 3)^2 + (x_1 - 4)^2 + 1
 *  > runMain scalation.optimization.l_BFGS_BTest
 */
@main def l_BFGS_BTest (): Unit =

    val n  = 2
    val x0 = new VectorD (n)
    def f (x: VectorD): Double = (x(0) - 3)~^2 + (x(1) - 4)~^2 + 1

    banner ("Minimize (no bounds): (x_0 - 3)^2 + (x_1 - 4)^2 + 1")
    var optimizer = new L_BFGS_B (f)
    var opt = optimizer.solve (x0)
    println (s"o][ ptimal solution (x, f(x)) = $opt")

    banner ("Minimize (bounds): (x_0 - 3)^2 + (x_1 - 4)^2 + 1")
    val l  = VectorD.fill (x0.dim)(3.5)
    val u  = VectorD.fill (x0.dim)(5.0)
    optimizer = new L_BFGS_B (f, l = l, u = u)
    opt = optimizer.solve (x0)
    println (s"][ optimal solution (x, f(x)) = $opt")

end l_BFGS_BTest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `l_BFGS_BTest2` main function is used to test the `L_BFGS_B` class.
 *      f(x) = x_0^4 + (x_0 - 3)^2 + (x_1 - 4)^2 + 1
 *  > runMain scalation.optimization.l_BFGS_BTest2
 */
@main def l_BFGS_BTest2 (): Unit =

    val n  = 2
    val x0 = new VectorD (n)
    def f (x: VectorD): Double = x(0)~^4 + (x(0) - 3)~^2 + (x(1) - 4)~^2 + 1

    banner ("Minimize (no bounds): x_0^4 + (x_0 - 3)^2 + (x_1 - 4)^2 + 1")
    var optimizer = new L_BFGS_B (f)
    var opt = optimizer.solve (x0)
    println (s"][ optimal solution (x, f(x)) = $opt")

    banner ("Minimize (bounds): x_0^4 + (x_0 - 3)^2 + (x_1 - 4)^2 + 1")
    val l  = VectorD.fill (x0.dim)(3.5)
    val u  = VectorD.fill (x0.dim)(5.0)
    optimizer = new L_BFGS_B (f, l = l, u = u)
    opt = optimizer.solve (x0)
    println (s"][ optimal solution (x, f(x)) = $opt")

end l_BFGS_BTest2


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `l_BFGS_BTest3` main function is used to test the `L_BFGS_B` class.
 *      f(x) = 1/x_0 + x_0^4 + (x_0 - 3)^2 + (x_1 - 4)^2 + 1
 *  > runMain scalation.optimization.l_BFGS_BTest3
 */
@main def l_BFGS_BTest3 (): Unit =

    val n  = 2
    val x0 = VectorD (0.1, 0.0)
    def f (x: VectorD): Double = 1/x(0) + x(0)~^4 + (x(0) - 3)~^2 + (x(1) - 4)~^2 + 1

    banner ("Minimize (no bounds): 1/x_0 + x_0^4 + (x_0 - 3)^2 + (x_1 - 4)^2 + 1")
    var optimizer = new L_BFGS_B (f)
    var opt = optimizer.solve (x0)
    println (s"][ optimal solution (x, f(x)) = $opt")

    opt = optimizer.resolve (n)
    println (s"][ optimal solution (x, f(x)) = $opt")

    banner ("Minimize (bounds): 1/x_0 + x_0^4 + (x_0 - 3)^2 + (x_1 - 4)^2 + 1")
    val l  = VectorD.fill (x0.dim)(3.5)
    val u  = VectorD.fill (x0.dim)(5.0)
    optimizer = new L_BFGS_B (f, l = l, u = u)
    opt = optimizer.solve (x0)
    println (s"][ optimal solution (x, f(x)) = $opt")

end l_BFGS_BTest3

