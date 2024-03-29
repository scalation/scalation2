
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sun Sep  4 21:57:30 EDT 2011
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Two-Phase Simplex Algorithm
 *
 *  @see Linear Programming and Network Flows, Bazaraa and Jarvis
 *  @see www.wiley.com/WileyCDA/WileyTitle/productCd-0470462728,subjectCd-BA04.html
 *  @see Algorithms, 4th Edition, Robert Sedgewick and Kevin Wayne
 *  @see www.cs.princeton.edu/algs4/63or/Simplex.java.html
 *  @see en.wikibooks.org/wiki/Operations_Research/The_Simplex_Method
 */

package scalation
package optimization
package linear_opt

import scala.runtime.ScalaRunTime.stringOf
import scala.util.control.Breaks.{breakable, break}

import scalation.mathstat._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Simplex2P` class solves Linear Programming (LP) problems using a tableau based
 *  Simplex Algorithm.  Given a constraint matrix a, limit/RHS vector b and
 *  cost vector c, find values for the solution/decision vector x that minimize
 *  the objective function f(x), while satisfying all of the constraints, i.e.,
 *
 *  minimize    f(x) = c x
 *  subject to  a x <= b, x >= 0
 *
 *  In case of a_i x >= b_i, use -b_i as an indicator of a ">=" constraint.
 *  The program will flip such negative b_i back to positive as well as use
 *  a surplus and artificial variable instead of the usual slack variable, i.e.,
 *  a_i x <= b_i  =>  a_i x + s_i = b_i    // use slack variable s_i with coefficient 1
 *  a_i x >= b_i  =>  a_i x + s_i = b_i    // use surplus variable s_i with coefficient -1
 *  For each '>=' constraint, an artificial variable is introduced and put into
 *  the initial basis.  These artificial variables must be removed from the basis
 *  during Phase I of the Two-Phase Simplex Algorithm.  After this, or if there
 *  are no artificial variables, Phase II is used to find an optimal value for x
 *  and the optimum value for f. 
 *
 *  Creates an MM-by-nn simplex tableau with
 *  -- [0..M-1, 0..N-1]    = a (constraint matrix)
 *  -- [0..M-1, N..M+N-1]  = s (slack/surplus variable matrix)
 *  -- [0..M-1, M+N..nn-2] = r (artificial variable matrix)
 *  -- [0..M-1, nn-1]      = b (limit/RHS vector)
 *  -- [M, 0..nn-2]        = c (cost vector)
 *
 *  @param a  the M-by-N constraint matrix
 *  @param b  the M-length limit/RHS vector (input b_i negative for surplus)
 *  @param c  the N-length cost vector
 */
class Simplex2P (a: MatrixD, b: VectorD, c: VectorD)
      extends MinimizerLP:

    private val debug    = debugf ("Simplex2P", true)        // debug function
    private val flaw     = flawf ("Simplex2P")               // flaw function
    private val DANTIZ   = true                              // use Dantiz's pivot rule, else Bland's
    private val CHECK    = true                              // CHECK mode => check feasibility for each pivot
    private val _0       = 0.0                               // zero, for Floating Point Error (FPE) try setting to EPSILON

    private val M        = a.dim                             // the number of constraints (row)
    private val N        = a.dim2                            // the number of decision variables
    private val R        = b.count (_ < 0.0)                 // the number of artificial variables (countNeg)
    private val MpN      = M + N                             // the number of non-artificial variables
    private val MM       = M + 1                             // # row in tableau

    private var nn       = MpN + R + 1                       // # columns in tableau
    private var jj       = nn - 1                            // the last column (b)
    private val MAX_IT   = 200 * N                           // maximum number of iterations
    private var flip     = 1.0                               // 1(slack) or -1(surplus) depending on b_i

    if b.dim != M then flaw ("init", s"b.dim = ${b.dim} != + $M")
    if c.dim != N then flaw ("init", s"c.dim = ${c.dim} != + $N")

    private val t  = new MatrixD (MM, nn)                    // the MM-by-nn simplex tableau
    private var jr = -1                                      // index counter for artificial variables
    for i <- 0 until M do
         flip = if b(i) < _0 then -1.0 else 1.0
         t.set (i, a(i))                                     // col x: constraint matrix a
         t(i, N + i) = flip                                  // col y: slack/surplus variable matrix s
         if flip < 0 then { jr += 1; t(i, MpN + jr) = 1.0 }  // col r: artificial variable matrix r
         t(i, jj) = b(i) * flip                              // col b: limit/RHS vector b
    end for

    private val x_B = Array.ofDim [Int] (M)                  // the indices of the basis

    val checker = new CheckLP (a, b, c)                      // checker determines if LP solution is correct

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Initialize the basis to the slack and artificial variables.  Perform
     *  row operations to make cost row (t(M)) zero in artificial var columns. 
     *  If b(i) is negative have a surplus and artificial variable, otherwise,
     *  just a slack variable.
     */
    def initBasis (): Unit =
        jr = -1
        for i <- 0 until M do
            if b(i) >= _0 then
                x_B(i) = N + i                               // put slack variable in basis
            else
                jr    += 1
                x_B(i) = MpN + jr                            // put artificial variable in basis
                t(M)  += t(i)                                // row op to make t(M, MpN + j) zero
            end if
        end for
    end initBasis

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the index of the first positive element in vector v (-1 otherwise).
     *  @param v  the vector to examine
     *  @param e  the ending index (exclusive) for the search
     */
    private def firstPos (v: VectorD, e: Int): Int =
        var j = -1
        var (go, i) = (true, 0)
        cfor (go && i < e, i += 1) {  
            if v(i) > 0.0 then { j = i; go = false }
        } // cfor
        j
    end firstPos

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the argument maximum of vector v or -1 if its value is not positive.
     *  @param v  the vector to examine
     *  @param e  the ending index (exclusive) for the search
     */
    private def argmaxPos (v: VectorD, e: Int): Int =
        val j = v.argmax (e); if v(j) > 0.0 then j else -1
    end argmaxPos

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the best variable x_l to enter the basis.  Determine the index of
     *  entering variable corresponding to column l (e.g., using Dantiz's Rule
     *  or Bland's Rule).  Return -1 to indicate no such column.
     *  t(M).argmaxPos (jj)       // use Dantiz's rule (index of max positive, cycling possible)
     *  t(M).firstPos (jj)        // use Bland's rule  (index of first positive, FPE possible)
     */
    def entering (): Int = if DANTIZ then argmaxPos (t(M), jj) else firstPos (t(M), jj)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the best variable x_k to leave the basis given that x_l is entering.
     *  Determine the index of the leaving variable corresponding to row k using
     *  the Min-Ratio Rule.  Return -1 to indicate no such row.
     *  @param l  the entering variable (column)
     */
    def leaving (l: Int): Int =
        val b_ = t(?, jj)                                    // updated b column (RHS)
        var k  = -1
        for i <- 0 until M if t(i, l) > _0 do                // find the pivot row
            if k == -1 then k = i
            else if b_(i) / t(i, l) <= b_(k) / t(k, l) then k = i   // lower ratio => reset k
        end for
        if k == -1 then flaw ("leaving", "the solution is UNBOUNDED")
        debug ("leaving", s"pivot = ($k, $l)") 
        k
    end leaving

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Pivot on entry (k, l) using Gauss-Jordan elimination to replace variable
     *  x_k with x_l in the basis.
     *  @param k  the leaving variable (row)
     *  @param l  the entering variable (column)
     */
    def pivot (k: Int, l: Int): Unit =
        println ("pivot: entering = " + l + " leaving = " + k)
        t(k) /= t(k, l)                                      // make pivot 1
        for i <- 0 to M if i != k do t(i) -= t(k) * t(i, l)  // zero rest of column l
        x_B(k) = l                                           // update basis (l replaces k)
    end pivot

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Run the simplex algorithm starting from an initial BFS and iteratively
     *  find a non-basic variable to replace a variable in the current basis
     *  so long as the objective function improves.  Runs a single phase and
     *  return the optimal vector x.
     */
    def solve_1 (): VectorD =
        showTableau (0)                                      // for iter = 0
        var k    = -1                                        // the leaving variable (row)
        var l    = -1                                        // the entering variable (column)

        breakable {
            for it <- 1 to MAX_IT do
                l = entering (); if l == -1 then break ()    // -1 => optimal solution found
                k = leaving (l); if k == -1 then break ()    // -1 => solution is unbounded
                pivot (k, l)                                 // pivot: k leaves and l enters
                if CHECK && infeasible then break ()         // quit if infeasible
                showTableau (it)
            end for
        } // breakable

        primal                                               // return the optimal vector x
    end solve_1

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether the current solution (x = primal) is still primal feasible.
     */
    def infeasible: Boolean =
        if ! checker.isPrimalFeasible (primal) then
            flaw ("infeasible", "solution x is no longer PRIMAL FEASIBLE")
            true
        else
            false
        end if
    end infeasible

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Remove the artificial variables and reset the cost row (M) in the tableau.
     */
    def removeArtificials (): Unit =
        nn -= R                                              // reduce the effective width of the tableau
        jj -= R                                              // reset the index of the last column (jj)
        t(?, jj) = t(?, jj + R)                              // move the b vector to the new jj column
        t(M)(0 until N) = -c                                 // set cost row (M) in the tableau to given cost
        showTableau (-1)
        for j <- 0 until N if x_B contains j do
            val pivotRow = t(?, j).argmax (M)                // find the pivot row where element = 1
            t(M) -= t(pivotRow) * t(M, j)                    // make cost row 0 in pivot column (j)
        end for
    end removeArtificials

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve the LP minimization problem using two phases if necessary.  Note:
     *  phase I is always minimization.  Two phases are necessary if the number
     *  of artificial variables R > 0.
     */
    def solve (): VectorD =
        var x: VectorD = null                                // the decision variables
//      var y: VectorD = null                                // the dual variables
        var f = Double.PositiveInfinity                      // worst possible value for minimization

        if R > 0 then
            t(M)(MpN until jj) = -1.0                        // set cost row (M) in the tableau to remove artificials
        else
            t(M)(0 until N) = -c                             // set cost row (M) in the tableau to given cost vector
        end if
        initBasis ()                                         // initialize the basis to the slack and artificial vars

        if R > 0 then                                        // there are artificial variables => phase I required
            println ("solve:  Phase I ---------------------------------------------")
            println (s"decision = $N, slack = ${M-R}, surplus = $R, artificial = $R")
            x = solve_1 ()                                   // solve the Phase I problem: optimal f = 0
            f = objF (x)
            println (s"solve:  Phase I solution x = $x, f = $f")
            removeArtificials ()                             // remove the artificial variables and reset cost row
        end if

        println ("solve:  Phase II --------------------------------------------")
        x = solve_1 ()                                       // solve the Phase II problem for final solution
        f = objF (x)
        println (s"solve:  Phase II solution x = $x, f = $f")
        x
    end solve

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the primal solution vector x (only the basic variables are non-zero).
     */
    def primal: VectorD =
        val x = new VectorD (N)
        for i <- 0 until M if x_B(i) < N do x(x_B(i)) = t(i, jj)   // RHS value
        x
    end primal

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the dual solution vector y (cost row (M) under the slack columns).
     */
    def dual: VectorD = t(M)(N until MpN)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the value of the objective function f(x) = c x.
     */
    def objF (x: VectorD): Double = t(M, jj)                 // bottom, right cell in tableau

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Show the current tableau.
     *  @param iter  the number of iterations do far
     */
    def showTableau (iter: Int): Unit =
        println ("showTableau: --------------------------------------------------------")
        println (this)
        println (s"showTableau: after $iter iterations, with limit of $MAX_IT")
    end showTableau

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert the current tableau and basis to a string suitable for display.
     */
    override def toString: String =
        val s = new StringBuilder ()
        for i <- 0 to M do
            s ++= (if i == 0 then "tableau = | " else        "          | ")
            for j <- 0 until jj-1 do s++= "%8.3f, ".format (t(i, j))
            s ++= "%8.3f | %8.3f |\n".format (t(i, jj-1), t(i, jj))
        end for
        s ++= "basis = " + stringOf (x_B)
        s.toString
    end toString

end Simplex2P


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `simplex2PTest` main function is used to test the `Simplex2P` class.
 *  > runMain scalation.optimization.linear_opt.simplex2PTest
 */
@main def simplex2PTest (): Unit =

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test the Simplex2P Algorithm for solving Linear Programming problems.
     *  @param a the constraint matrix
     *  @param b the limit/RHS vector
     *  @param c the cost vector
     */
    def test (a: MatrixD, b: VectorD, c: VectorD): Unit =
        val lp = new Simplex2P (a, b, c)   // test the Two-Phase Simplex Algorithm
        val x  = lp.solve ()               // the primal solution vector x
        val y  = lp.dual                   // the dual solution vector y
        val f  = lp.objF (x)               // the minimum value of the objective function

        println ("primal x = " + x)
        println ("dual   y = " + y)
        println ("objF   f = " + f)
        println ("optimal? = " + lp.check (x, y, f))
    end test

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test case 1:
     *  Phase I  solution - not needed
     *  Phase II solution x = (1/3, 0, 13/3), x_B = (0, 2, 4), f = -17
     *  @see Linear Programming and Network Flows, Example 3.9
     */
    def test1 (): Unit =
        val a = MatrixD ((3, 3), 1.0, 1.0,  2.0,
                                 1.0, 1.0, -1.0,
                                -1.0, 1.0,  1.0)
        val c = VectorD         (1.0, 1.0, -4.0)
        val b = VectorD (9.0, 2.0, 4.0)
        test (a, b, c)
    end test1

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test case 2:
     *  Phase I  solution x = (.5, 1.5), x_B = (0, 1, 4), f =  0
     *  Phase II solution x = (0, 3),    x_B = (1, 2, 3), f = -6
     *  @see Linear Programming and Network Flows, Example 4.3
     *  Negative values for b indicate a '>=' constraint.
     */
    def test2 (): Unit =
        val a = MatrixD ((3, 2), 1.0,  1.0,
                                -1.0,  1.0,
                                 0.0,  1.0)
        val c = VectorD         (1.0, -2.0)
        val b = VectorD (-2.0, -1.0, 3.0)
        test (a, b, c)
    end test2

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test case 3:
     *  Phase I  solution x = (2, 0, 4), x_B = (0, 2, 5), f = 0
     *  Phase II solution x = (2, 0, 4), x_B = (0, 2, 5), f = 36
     *  @see college.cengage.com/mathematics/larson/elementary_linear/4e/shared/downloads/c09s4.pdf
     *  Negative values for b indicate a '>=' constraint.
     */
    def test3 (): Unit =
        val a = MatrixD ((3, 3), 1.0,  1.0, 1.0,
                                 0.0,  1.0, 2.0,
                                -1.0,  2.0, 2.0)
        val c = VectorD         (2.0, 10.0, 8.0)
        val b = VectorD (-6.0, -8.0, -4.0)
        test (a, b, c)
    end test3

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test case 4:
     *  Phase I  solution - not needed
     *  Phase II solution x = (2/3, 10/3, 0), x_B = (0, 1, 5), f = -22/3
     *  @see Linear Programming and Network Flows, Example 5.2
     */
    def test4 (): Unit =
        val a = MatrixD ((3, 3), 1.0,  1.0,  1.0,
                                -1.0,  2.0, -2.0,
                                 2.0,  1.0,  0.0)
        val c = VectorD        (-1.0, -2.0,  1.0)
        val b = VectorD (4.0, 6.0, 5.0)
        test (a, b, c)
    end test4

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test case 5:
     *  Phase I  solution x = (4/5, 8/5), x_B = (0, 1), f = 0
     *  Phase II solution x = (4/5, 8/5), x_B = (0, 1), f = 8.8
     *  @see Linear Programming and Network Flows, Example 6.14
     */
    def test5 (): Unit =
        val a = MatrixD ((2, 2), 3.0, 1.0,
                                 1.0, 2.0)
        val c = VectorD         (3.0, 4.0)
        val b = VectorD (-4.0, -4.0)
        test (a, b, c)
    end test5

    banner ("LP test1"); test1 ()
    banner ("LP test2"); test2 ()
    banner ("LP test3"); test3 ()
    banner ("LP test4"); test4 ()
    banner ("LP test5"); test5 ()

end simplex2PTest

