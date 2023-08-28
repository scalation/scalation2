
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sat Jan  1 13:54:44 EST 2022
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   A Simple Function Calculator (Double - double precision floating point numbers)
 */

package scalation

import scala.math._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Calc` object provides a template for evaluating/calculating scalar functions.
 *  Replace the part after the equal sign with your formula.
 */
object Calc:

    def f(x: Double): Double = 6.67408E-11 * 5.97219E24 * x / 6.371E6~^2 + log (1.0)

end Calc


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `runCalc` main function evaluates the function f on the given input.
 *  @param x  the command-line argument passed to function f
 *  > runMain scalation.runCalc <value-for-x>
 */
@main def runCalc (x: Double): Unit =

    import Calc.f

    println (s"f($x) = ${f(x)}")

end runCalc


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `runCalcHelp` main function list the operators used in floating point calculations.
 *  > runMain scalation.runCalcHelp
 */
@main def runCalcHelp (): Unit =

    val help = """
        Operators and Functions:
        +	add
        -       subtract
        *       multiply
        /       divide
        %       mod
        ~^      exponentiate
        log     logarithm
        see     https://scala-lang.org/api/3.x/scala/math.html
        see     https://cobweb.cs.uga.edu/~jam/scalation_2.0/target/scala-3.1.1-RC2/api/scalation.html
    """

    println (help)

end runCalcHelp

