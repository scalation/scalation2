
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sun Oct  2 22:43:44 EDT 2011
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Tests Several Non-Linear Optimization Algorithms
 *
 *  @see     http://www.ai7.uni-bayreuth.de/test_problem_coll.pdf
 */

package scalation
package optimization

import scala.collection.mutable.LinkedHashMap

import scalation.mathstat._
import scalation.optimization.quasi_newton.{BFGS, LBFGS_B}

import Minimizer._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `nLPTest` main function used to test several Non-Linear Programming (NLP)
 *  algorithms on unconstrained problems.
 *  Algorithms:
 *      - Gradient Descent with Golden Section Line Search
 *      - Polak-Ribiere Conjugate Gradient with Golden Section Line Search
 *      - Gradient Descent with Wolfe Line Search (option ib BFGS)
 *      - Broyden–Fletcher–Goldfarb–Shanno (BFGS) with Wolfe Line Search
 *      - Limited Memory Broyden–Fletcher–Goldfarb–Shanno (LBFGS) with Wolfe Line Search
 *      - Limited Memory Broyden–Fletcher–Goldfarb–Shanno Bounded (LBFGS_B) with Wolfe Line Search
 *      - Nelder-Mead Simplex
 *      - Coordinate Descent
 *      - Grid Search
 *  > runMain scalation.optimization.nLPTest
 */
@main def nLPTest (): Unit =

    println ("nLPTest: unconstrained")

    val res = LinkedHashMap [String, FuncVec] ()

    def test1 (): Unit =
        banner ("Minimize f(x)  = (x_0 - 3)^2 + (x_1 - 4)^2 + 1")
        def f(x: VectorD): Double = (x(0) - 3)~^2 + (x(1) - 4)~^2 + 1.0
//      def gr (x: VectorD): VectorD = VectorD (2*x(0) - 6, 2*x(1) - 8)

        res += "GradientDescent"   -> test (new GradientDescent (f), new VectorD (2))
        res += "SteepestDescent"   -> test (BFGS (f), new VectorD (2))
        res += "ConjugateGradient" -> test (new ConjugateGradient (f), new VectorD (2))
        res += "BFGS"              -> test (new BFGS (f), new VectorD (2))
//      res += "LBFGS"             -> test (new LBFGS (f), new VectorD (2))
        res += "LBFGS_B"           -> test (new LBFGS_B (f), new VectorD (2))
//      res += "NelderMeadSimplex" -> test (new NelderMeadSimplex (f, 2), new VectorD (2))
        res += "CoordinateDescent" -> test (new CoordinateDescent (f), new VectorD (2))
        for ((n, o) <- res) println (s"$n\t$o")
    end test1

    def test2 (): Unit =
        banner ("Minimize f(x)  = (x_0 - 30)^2 + (x_1 - 40)^2 + 1")
        def f(x: VectorD): Double = (x(0) - 30)~^2 + (x(1) - 40)~^2 + 1.0
//      def gr (x: VectorD): VectorD = VectorD (2*x(0) - 60, 2*x(1) - 80)

        res += "GradientDescent"   -> test (new GradientDescent (f), new VectorD (2))
        res += "SteepestDescent"   -> test (BFGS (f), new VectorD (2))
        res += "ConjugateGradient" -> test (new ConjugateGradient (f), new VectorD (2))
        res += "BFGS"              -> test (new BFGS (f), new VectorD (2))
//      res += "LBFGS"             -> test (new LBFGS (f), new VectorD (2))
        res += "LBFGS_B"           -> test (new LBFGS_B (f), new VectorD (2))
//      res += "NelderMeadSimplex" -> test (new NelderMeadSimplex (f, 2), new VectorD (2))
        res += "CoordinateDescent" -> test (new CoordinateDescent (f), new VectorD (2))
        for ((n, o) <- res) println (s"$n\t$o")
    end test2

    def test3 (): Unit =
        banner ("Minimize f(x)  = x_0^4 + (x_0 - 3)^2 + (x_1 - 4)^2 + 1")
        def f(x: VectorD): Double = x(0)~^4 + (x(0) - 3)~^2 + (x(1) - 4)~^2 + 1.0
//      def gr (x: VectorD): VectorD = VectorD (4*x(0)~^3 + 2*x(0) - 6, 2*x(1) - 8)

        res += "GradientDescent"   -> test (new GradientDescent (f), new VectorD (2))
        res += "SteepestDescent"   -> test (BFGS (f), new VectorD (2))
        res += "ConjugateGradient" -> test (new ConjugateGradient (f), new VectorD (2))
        res += "BFGS"              -> test (new BFGS (f), new VectorD (2))
//      res += "LBFGS"             -> test (new LBFGS (f), new VectorD (2))
        res += "LBFGS_B"           -> test (new LBFGS_B (f), new VectorD (2))
//      res += "NelderMeadSimplex" -> test (new NelderMeadSimplex (f, 2), new VectorD (2))
        res += "CoordinateDescent" -> test (new CoordinateDescent (f), new VectorD (2))
        for ((n, o) <- res) println (s"$n\t$o")
    end test3

    // @see http://math.fullerton.edu/mathews/n2003/gradientsearch/GradientSearchMod/Links/GradientSearchMod_lnk_5.html
/*
    def test4 (): Unit =
        banner ("Minimize f(x)  = x_0/4 + 5x_0^2 + x_0^4 - 9x_0^2 x_1 + 3x_1^2 + 2x_1^4")
        def f(x: VectorD): Double = x(0)/4 + 5*x(0)~^2 + x(0)~^4 - 9*x(0)~^2*x(1) + 3*x(1)~^2 + 2*x(1)~^4
        def gr (x: VectorD): VectorD = VectorD (0.25 + 10*x(0) + 4*x(0)~^3 - 9*x(1), -9*x(0) + 6*x(1) + 8*x(1)~^3)

        res += "GradientDescent"   -> test (new GradientDescent (f), new VectorD (2))
        res += "SteepestDescent"   -> test (BFGS (f), new VectorD (2))
        res += "ConjugateGradient" -> test (new ConjugateGradient (f), new VectorD (2))
        res += "BFGS"              -> test (new BFGS (f), new VectorD (2))
//      res += "LBFGS"             -> test (new LBFGS (f), new VectorD (2))
        res += "LBFGS_B"           -> test (new LBFGS_B (f), new VectorD (2))
//      res += "NelderMeadSimplex" -> test (new NelderMeadSimplex (f, 2), new VectorD (2))
        res += "CoordinateDescent" -> test (new CoordinateDescent (f), new VectorD (2))
        for ((n, o) <- res) println (s"$n\t$o")
    end test4
*/

    test1 ()
    test2 ()
    test3 ()
//  test4 ()

end nLPTest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `nLPTest2` main function used to test several Non-Linear Programming (NLP)
 *  algorithms on constrained problems.  FIX
 *  > runMain scalation.optimization.nLPTest2
 */
@main def nLPTest2 (): Unit =

    println ("FIX")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test the NLP algorithms on objective function 'f' with constraint function 'g'.
     *  @param f      the objective function to minimize 
     *  @param g      the constraint function to be satisfied
     *  @param n      the dimensionality of the problem
     *  @param f_str  the f function as a string
     *  @param g_str  the g function as a string
     *
    def test (f: FunctionV2S, g: FunctionV2S, n: Int, f_str: String, g_str: String): Unit =
        println ("\nMinimize   f(x) = " + f_str +
                 "\nSubject to g(x) = " + g_str)

        val x0   = new VectorD (n)          // zero vector

        val sdcs = new GradientDescent (f)
        var opt = sdcs.solve (x0)
        println (s"][ sdgs: optimal solution (f(x), x) = $opt")

        val prcg = new ConjugateGradient (f, g)
        opt = prcg.solve (x0)
        println (s"][ prcg: optimal solution (f(x), x) = $opt")

        val sdws = new BFGS (f, g); sdws.setSteepest ()
        opt = sdws.solve (x0)
        println (s"][ sdws: optimal solution (f(x), x) = $opt")

        val bfgs = new BFGS (f, g)
        opt = bfgs.solve (x0)
        println (s"][ bfgs: optimal solution (f(x), x) = $opt")

//      val l_bfgs_b = new LBFGS_G (f, g)
//      opt = l_bfgs_b.solve (x0)
//      println (s"][ bfgs: optimal solution (f(x), x) = $opt")
    end test

    def test0 (): Unit =    // x* = (3, 4), f* = 1
        val f_str =  "f(x) = (x_0 - 3)^2 + (x_1 - 4)^2 + 1"
        val g_str =  "g(x) = x_0 <= 1"
        def f (x: VectorD) = (x(0) - 3.0)~^2 + (x(1) - 40.0)~^2 + 1.0
        def g (x: VectorD) = x(0) - 1.0
        test (f, g, 2, f_str, g_str)
    end test0

    def test1 (): Unit =    // x* = (
        val f_str = "f(x)  = 100(x_1 - x_0^2)^2 + (1 - x_0)^2"
        val g_str = "g(x)  = x_1 >= -1.5"
        def f (x: VectorD) = 100.0 * (x(1) - x(0)~^2)~^2 + (1.0 - x(0))~^2
        def g (x: VectorD) = -x(1) - 1.5
        test (f, g, 2, f_str, g_str)
    end test1

    test0 ()
     */

end nLPTest2

