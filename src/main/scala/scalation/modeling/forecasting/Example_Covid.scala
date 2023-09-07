
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sat Jul 29 11:30:42 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Example Time Series Data: Covid-19 Weekly Data
 */

package scalation
package modeling
package forecasting

import scalation.mathstat._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Example_Covid` object provides a convenient way to load Covid-19 weekly data.
 */
object Example_Covid:

    import scala.collection.mutable.HashMap

    val header = Array ("new_cases",
                        "new_deaths",
                        "reproduction_rate",
                        "icu_patients",
                        "hosp_patients",
                        "new_tests",
                        "positive_rate",
                        "tests_per_case",
                        "people_vaccinated",
                        "people_fully_vaccinated",
                        "total_boosters",
                        "new_vaccinations",
                        "excess_mortality_cumulative_absolute",
                        "excess_mortality_cumulative",
                        "excess_mortality",
                        "excess_mortality_cumulative_per_million")

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Load the Covid-19 weekly data into a matrix for the exogenous variables x
     *  and a vector for the response/endogenous variable y.
     *  @param x_strs  the column names for the exogenous variables x
     *  @param y_str   the column name for the endogenous variable y
     *  @param trim    the number of initial rows to trim away (e.g., they are all 0)
     */
    def loadData (x_strs: Array [String], y_str: String, trim: Int = 0): (MatrixD, VectorD) =
        val col = HashMap [String, Int] ()
        for i <- header.indices do col += header(i) -> i

        val data = MatrixD.load ("covid_19_weekly.csv", 1+trim, 1)      // skip first row (header) + trim and first column
        val x_cols = for s <- x_strs yield col(s)
        (data(?, x_cols), data(?, col(y_str)))
    end loadData

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Load the Covid-19 weekly data into a vector for the response/endogenous variable y.
     *  @param y_str  the column name for the endogenous variable y
     *  @param trim   the number of initial rows to trim away (e.g., they are all 0)
     */
    def loadData_y (y_str: String, trim: Int = 0): VectorD =
        val col = HashMap [String, Int] ()
        for i <- header.indices do col += header(i) -> i

        val data = MatrixD.load ("covid_19_weekly.csv", 1+trim, 1)      // skip first row (header) + trim and first column
        data(?, col(y_str))
    end loadData_y

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Load the Covid-19 weekly data into a matrix for the variables y.
     *  @param y_str  the column names for the variables y (e.g., used in a VAR model)
     *  @param trim   the number of initial rows to trim away (e.g., they are all 0)
     */
    def loadData_yy (y_strs: Array [String], trim: Int = 0): MatrixD =
        val col = HashMap [String, Int] ()
        for i <- header.indices do col += header(i) -> i

        val data = MatrixD.load ("covid_19_weekly.csv", 1+trim, 1)      // skip first row (header) + trim and first column
        val y_cols = for s <- y_strs yield col(s)
        data(?, y_cols)
    end loadData_yy

end Example_Covid


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `example_CovidTest` main function test the `Example_Covid` object.
 *  > runMain scalation.modeling.forecasting.example_CovidTest
 */
@main def example_CovidTest (): Unit =

    val (x, y) = Example_Covid.loadData (Array ("new_cases", "new_tests", "positive_rate"), "new_deaths")

    new Plot (null, y, null, "new_deaths", lines = true)

end example_CovidTest


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `example_CovidTest2` main function test the `Example_Covid` object.
 *  This performs Exploratory Data Analysis (EDA) to find relationships
 *  between contemporaneous variables.
 *  > runMain scalation.modeling.forecasting.example_CovidTest2
 */
@main def example_CovidTest2 (): Unit =

    import Example_Covid._
    import scala.collection.mutable.Set

    val (x, y) = loadData (header, "new_deaths")

    new Plot (null, y, null, "new_deaths", lines = true)

    for j <- x.indices2 do
        banner (s"EDA for new_deaths vs. ${header(j)}")
        var xj  = x(?, j)                                               // get column j
        xj = scaleV (extreme (xj), (0.0, 2.0))(xj)                      // rescale vector xj to [0, 2]
        val xxj = MatrixD.fromVector (xj)
//      val mod = SymbolicRegression.quadratic (xxj, y)
//      val mod = SymbolicRegression.rescale (xxj, y, null, Set (1.0, 2.0, 3.0), cross = false)
        val mod = SymbolicRegression (xxj, y, null, Set (0.5, 1.0, 2.0, 3.0), cross = false)
        mod.trainNtest ()()
        val yp = mod.predict (mod.getX)
        println (mod.summary ())
        new Plot (xj, y, yp, s"y, yp (new_deaths) vs. x_$j (${header(j)})")
    end for

end example_CovidTest2

