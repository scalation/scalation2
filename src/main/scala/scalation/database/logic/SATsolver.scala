
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Tue Oct  6 16:15:23 EDT 2020
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    SATsolver - Simple (Inefficient) Satisfiablity Solver
 *
 *  @see Satisfiability Solvers
 *  https://www.cs.cornell.edu/gomes/pdf/2008_gomes_knowledge_satisfiability.pdf
 */

package scalation
package database
package logic

type BoolVec = IndexedSeq [Boolean]

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Convert the integer k into a Boolean sequence of size n.
 *  @param k  the integer to convert to a Boolean sequence
 *  @param n  the number of variables
 */
def toBoolVec (k: Int, n: Int): BoolVec =
    for i <- 0 until n yield (k & (1 << i)) != 0
end toBoolVec

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Return the power set for an sequence of n Boolean variables.
 *  @param n  the number of variables
 */
def powerset (n: Int): IndexedSeq [BoolVec] =
    for i <- 0 until 2~^n yield toBoolVec (i, n)
end powerset

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Determine whether the propositional logic formula f of n Boolean variables is satisfiable.
 *  Satisfiability:  exists x such that f(x) is true.
 *  @param f  the formula
 *  @param n  the number of variables
 */
def satSolver (f: BoolVec => Boolean, n: Int): Boolean = powerset (n).exists (f(_))
/*
    import scala.util.control.Breaks.{break, breakable}
    var satisfy = false
    breakable {
        for x <- powerset (n) do if f(x) then { satisfy = true; break () }
    } // breakable
    satisfy
end satSolver
*/


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `sATsolverTest` main function is used to test the powerSet and satSolver functions.
 *  > runMain scalation.database.logic.sATsolverTest
 */
@main def sATsolverTest (): Unit =

    def f (x: BoolVec): Boolean = x(0) && x(1)
    def g (x: BoolVec): Boolean = x(0) && x(1) && !x(0)
    
    val x = IndexedSeq (true, true)
    println (s"f($x})           = ${f(x)}")
    println (s"powerset(2)      = ${powerset(2)}")
    println (s"satSolver (f, 2) = ${satSolver (f, 2)}")
    println (s"satSolver (g, 2) = ${satSolver (g, 2)}")

end sATsolverTest

