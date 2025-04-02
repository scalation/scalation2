
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sat Jul 29 11:30:42 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Example Time Series Data: Covid-19 Weekly Data
 */

package scalation
package modeling
package forecasting

import scala.math.min
import scala.runtime.ScalaRunTime.stringOf

import scalation.mathstat._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Example_Covid` object provides a convenient way to load Covid-19 weekly data.
 *  See test cases (odd In-ST, even TnT Split) below for
 *                                         Loss/Equations   Optimizer
 *  (a:  1,  2) Plot and EDA               -                -
 *  Univariate:
 *  (b:  3,  4) Baseline Models            none or CSSE     none or various
 *  (c:  5,  6) AR(p) Models               Yule-Walker      Durbin-Levinson
 *  (d:  7,  8) ARMA(p, q=0) Models        CSSE             BFGS
 *  (e:  9, 10) ARY(p) Models              CSSE             QR Factorization
 *  (f: 11, 12) ARY_D(p) Models            CSSE + Direct    QR Factorization
 *  (g: 13, 14) ARMA(p, q=1) Models        CSSE             BFGS
 *  Multivariate:
 *  (h: 15, 16) ARX(p, 2, 2) Models        CSSE             QR Factorization
 *  (i: 17, 18) ARX_D Models               CSSE + Direct    QR Factorization
 *  (j: 19, 20) ARX_Quad_D Models          CSSE             QR Factorization
 *
 *  Known Bugs: 13, 14
 */
object Example_Covid:

    import scala.collection.mutable.HashMap

    val fileName = "covid_19_weekly.csv"

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

    val response = "new_deaths"                                   // main response/output variable
    val NO_EXO   = Array.ofDim [String] (0)                       // empty array => no exogenous variables

    val yy = Example_Covid.loadData_y ()
//  val y  = yy                                                   // full
    val y  = yy(0 until 116)                                      // clip the flat end

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Load the Covid-19 weekly data into a matrix for the exogenous variables x
     *  and a vector for the response/endogenous variable y.
     *  @param x_strs  the column names for the exogenous variables x
     *  @param y_str   the column name for the endogenous variable y
     *  @param trim    the number of initial rows to trim away (e.g., they are all 0)
     */
    def loadData (x_strs: Array [String], y_str: String = response, trim: Int = 0): (MatrixD, VectorD) =
        val col = HashMap [String, Int] ()
        for i <- header.indices do col += header(i) -> i

        val data = MatrixD.load (fileName, 1+trim, 1)             // skip first row (header) + trim first column
        val x_cols = for s <- x_strs yield col(s)
        (data(?, x_cols), data(?, col(y_str)))
    end loadData

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Load the Covid-19 weekly data into a vector for the response/endogenous variable y.
     *  @param y_str  the column name for the endogenous variable y
     *  @param trim   the number of initial rows to trim away (e.g., they are all 0)
     */
    def loadData_y (y_str: String = response, trim: Int = 0): VectorD =
        val col = HashMap [String, Int] ()
        for i <- header.indices do col += header(i) -> i

        val data = MatrixD.load (fileName, 1+trim, 1)             // skip first row (header) + trim first column
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

        val data = MatrixD.load (fileName, 1+trim, 1)             // skip first row (header) + trim first column
        val y_cols = for s <- y_strs yield col(s)
        data(?, y_cols)
    end loadData_yy

end Example_Covid

import Example_Covid._
import MakeMatrix4TS.hp

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `example_CovidTest` main function tests the `Example_Covid` object.
 *  Prints and plots the response column ("new_deaths").
 *  > runMain scalation.modeling.forecasting.example_CovidTest
 */
@main def example_CovidTest (): Unit =

    banner (s"Print the response = $response column for the Covid-19 dataset (${y.dim} points")
    for i <- y.indices do println (s"$i \t ${y(i)}")

    banner (s"Plot the response = $response column for the Covid-19 dataset (${y.dim} points")
    new Plot (null, y, null, s"y ($response)", lines = true)

end example_CovidTest


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `example_CovidTest2` main function tests the `Example_Covid` object.
 *  Performs Exploratory Data Analysis (EDA) to find relationships between
 *  contemporaneous variables.
 *  > runMain scalation.modeling.forecasting.example_CovidTest2
 */
@main def example_CovidTest2 (): Unit =

    import scala.collection.mutable.Set

    val (xx, yy) = loadData (header, response)
//  val (x, y)   = (xx, yy)                                             // full
    val (x, y)   = (xx(0 until 116), yy(0 until 116))                   // clip the flat end

    new Plot (null, y, null, s"y ($response)", lines = true)

    for j <- x.indices2 do
        banner (s"EDA for response = $response vs. ${header(j)}")
        var xj  = x(?, j)                                               // get column j
        xj = scaleV (extreme (xj), (0.0, 2.0))(xj)                      // rescale vector xj to [0, 2]
        val xxj = MatrixD.fromVector (xj)
//      val mod = SymbolicRegression.quadratic (xxj, y)
//      val mod = SymbolicRegression.rescale (xxj, y, null, Set (1.0, 2.0, 3.0), cross = false)
        val mod = SymbolicRegression (xxj, y, null, Set (0.5, 1.0, 2.0, 3.0), cross = false)
        mod.trainNtest ()()
        val yp = mod.predict (mod.getX)
        println (mod.summary ())
        new Plot (xj, y, yp, s"y, yp ($response) vs. x_$j (${header(j)})")
    end for

end example_CovidTest2


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `example_CovidTest3` main function tests the `Example_Covid` object.
 *  Uses In-Sample Testing (In-ST), i.e., train and test on the same data.
 *  Runs several baseline models for horizons 1 to 6, see sMAPE metrics below:
 *
 *  55.1927,    53.9282,    52.7133,    51.8648,    51.9621,	52.0771  Null
 *  54.6045,    53.3254,    52.1120,    51.2903,    51.4475,    51.4937  Trend
 *  24.2641,    31.8588,    42.4430,    50.1029,    57.4933,    63.5406  SMA
 *  26.4055,    31.5936,    43.7356,    50.1744,    58.3506,    63.7234  WMA
 *  18.6934,    29.1811,    38.6542,    47.1281,    54.8713,    61.9944  SES
 *  19.0371,    29.5797,    39.0740,    47.4638,    55.1785,    62.1818  RW
 *  18.3265,    28.7734,    38.2039,    46.7814,    54.5563,    61.7930  RWS
 *  18.7298,    28.4908,    37.0997,    45.6487,    51.7248,    56.3708  AR(1)
 *
 *  > runMain scalation.modeling.forecasting.example_CovidTest3
 */
@main def example_CovidTest3 (): Unit =

    val hh = 6                                                            // max forecasting horizon

    new Plot (null, y, null, s"y ($response)", lines = true)

    new NullModel (y, hh).inSampleTest ()
    new TrendModel (y, hh).inSampleTest ()
    new SimpleMovingAverage (y, hh).inSampleTest ()
    new WeightedMovingAverage (y, hh).inSampleTest ()
    new SimpleExpSmoothing (y, hh).inSampleTest ()
    new RandomWalk (y, hh).inSampleTest ()
    new RandomWalkS (y, hh).inSampleTest ()
    new AR (y, hh).inSampleTest ()

end example_CovidTest3


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `example_CovidTest4` main function tests the `Example_Covid` object.
 *  Uses Train-n-Test Split (TnT) with Rolling Validation.
 *  Runs several baseline models for horizons 1 to 6, see sMAPE metrics below:
 *
 *  57.1057,    60.0825,    62.9136,    64.7453,    67.9247,    70.6674  Null
 *  61.9077,    65.1881,    68.7187,    71.4655,    73.9327,    75.9584  Trend
 *  22.3044,    30.4325,    45.3661,    55.7217,    67.6973,    77.4038  SMA
 *  23.8526,    30.0945,    46.9748,    55.8104,    68.7352,    77.7010  WMA
 *  18.3769,    27.1712,    40.3425,    51.8124,    63.7356,    75.0046  SES
 *  18.6713,    27.5720,    40.9387,    52.3496,    64.2481,    75.3015  RW
 *  18.0855,    26.7084,    39.6941,    51.2218,    63.1873,    74.6834  RWS
 *  19.1590,    31.1975,    44.4850,    55.3120,    65.5536,    74.4969  AR(1)
 *
 *  > runMain scalation.modeling.forecasting.example_CovidTest4
 */
@main def example_CovidTest4 (): Unit =

    val hh = 6                                                          // max forecasting horizon

    new Plot (null, y, null, s"y ($response)", lines = true)

    var mod: Forecaster = null

    banner ("TnT Test: Null Model")
    mod = new NullModel (y, hh)
    mod.trainNtest ()()
    mod.setSkip (0)                                                     // start at beginning of test-set
    mod.rollValidate ()                                                 // TnT with Rolling Validation
    mod.diagnoseAll (y, mod.getYf, Forecaster.teRng (y.dim))            // only diagnose on the testing set

    banner ("TnT Test: Trend Model")
    mod = new TrendModel (y, hh)
    mod.trainNtest ()()
    mod.setSkip (0)
    mod.rollValidate ()                                                 // TnT with Rolling Validation
    mod.diagnoseAll (y, mod.getYf, Forecaster.teRng (y.dim))            // only diagnose on the testing set

    banner ("TnT Test: Simple Moving Average Model")
    mod = new SimpleMovingAverage (y, hh)
    mod.trainNtest ()()
    mod.setSkip (0)
    mod.rollValidate ()                                                 // TnT with Rolling Validation
    mod.diagnoseAll (y, mod.getYf, Forecaster.teRng (y.dim))            // only diagnose on the testing set

    banner ("TnT Test: Weighted Moving Average Model")
    mod = new WeightedMovingAverage (y, hh)
    mod.trainNtest ()()
    mod.setSkip (0)
    mod.rollValidate ()                                                 // TnT with Rolling Validation
    mod.diagnoseAll (y, mod.getYf, Forecaster.teRng (y.dim))            // only diagnose on the testing set

    banner ("TnT Test: Simple Exponential Smoothing Model")
    mod = new SimpleExpSmoothing (y, hh)
    mod.trainNtest ()()
    mod.setSkip (0)
    mod.rollValidate ()                                                 // TnT with Rolling Validation
    mod.diagnoseAll (y, mod.getYf, Forecaster.teRng (y.dim))            // only diagnose on the testing set

    banner ("TnT Test: Random Walk Model")
    mod = new RandomWalk (y, hh)
    mod.trainNtest ()()
    mod.setSkip (0)
    mod.rollValidate ()                                                 // TnT with Rolling Validation
    mod.diagnoseAll (y, mod.getYf, Forecaster.teRng (y.dim))            // only diagnose on the testing set

    banner ("TnT Test: Random Walk Slope Adjusted Model")
    mod = new RandomWalkS (y, hh)
    mod.trainNtest ()()
    mod.setSkip (0)
    mod.rollValidate ()                                                 // TnT with Rolling Validation
    mod.diagnoseAll (y, mod.getYf, Forecaster.teRng (y.dim))            // only diagnose on the testing set

    banner ("TnT Test: Auto-Regressive AR(1) Model")
    mod = new AR (y, hh)
    mod.trainNtest ()()
    mod.setSkip (0)
    mod.rollValidate ()                                                 // TnT with Rolling Validation
    mod.diagnoseAll (y, mod.getYf, Forecaster.teRng (y.dim))            // only diagnose on the testing set

end example_CovidTest4


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `example_CovidTest5` main function tests the `Example_Covid` object.
 *  Uses In-Sample Testing (In-ST), i.e., train and test on the same data.
 *  Runs Auto-Regressive AR(p) models for several p values and horizons 1 to 6,
 *  see sMAPE metrics below:
 *
 *  18.7298,    28.4908,    37.0997,    45.6487,    51.7248,    56.3708  AR(1)
 *  16.3579,    24.7155,    33.0480,    40.0707,    46.0049,    50.8265  AR(2)
 *  16.0114,    22.7408,    29.5631,    35.2773,    40.9870,    45.8408  AR(3)
 *  15.8988,    22.5738,    28.5298,    33.3360,    39.1586,    43.1606  AR(4)
 *  15.9279,    22.5769,    28.5035,    33.3019,    39.1381,    43.0520  AR(5)
 *  15.9647,    22.6143,    28.5229,    33.3735,    39.1651,    42.9640  AR(6)
 *  16.0207,    23.2172,    29.4751,    35.2827,    41.0976,    46.1932  AR(7)
 *  16.0501,    22.7281,    28.6740,    34.1866,    39.5963,    44.9223  AR(8)
 *  16.0196,    22.5269,    28.4223,    34.1619,    39.7297,    44.4649  AR(9)
 *  16.1069,    22.6213,    28.6435,    34.2722,    39.9638,    44.8023  AR(10)
 *
 *  > runMain scalation.modeling.forecasting.example_CovidTest5
 */
@main def example_CovidTest5 (): Unit =

    val hh = 6                                                          // max forecasting horizon
    val hp = AR.hp                                                      // hyper-parameters for AR family of models

    new Plot (null, y, null, s"y ($response)", lines = true)

    for p <- 1 to 10 do                                                 // AR hyper-parameter settings
        hp("p") = p
        new AR (y, hh).inSampleTest ()                                  // create and test an AR model
    end for

end example_CovidTest5
 

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `example_CovidTest6` main function tests the `Example_Covid` object.
 *  Uses Train-n-Test Split (TnT) with Rolling Validation.
 *  Runs Auto-Regressive AR(p) models for several p values and horizons 1 to 6,
 *  see sMAPE metrics below:
 *
 *  19.1590,    31.1975,    44.4850,    55.3120,    65.5536,    74.4969  AR(1)
 *  17.1764,    27.8131,    41.0173,    52.3883,    62.4018,    71.3206  AR(2)
 *  16.1569,    24.1092,    35.0634,    45.3502,    56.0450,    65.4998  AR(3)
 *  15.2413,    23.2293,    30.1320,    40.3648,    48.8558,    57.8766  AR(4)
 *  15.4399,    23.3058,    30.4161,    40.4655,    49.3913,    58.6573  AR(5)
 *  15.7443,    22.8374,    29.7678,    38.5566,    45.5084,    50.8096  AR(6)
 *  15.8906,    24.2516,    31.1198,    40.2877,    47.4982,    56.6783  AR(7)
 *  15.8394,    24.8442,    31.2414,    40.4416,    47.5974,    56.3880  AR(8)
 *  15.2112,    23.6265,    30.7560,    40.1489,    49.4426,    58.3781  AR(9)
 *  15.7954,    23.7332,    32.8467,    42.5300,    52.3179,    60.5518  AR(10)
 *
 *  > runMain scalation.modeling.forecasting.example_CovidTest6
 */
@main def example_CovidTest6 (): Unit =

    val hh = 6                                                          // max forecasting horizon
    val hp = AR.hp                                                      // hyper-parameters for AR family of models

    new Plot (null, y, null, s"y ($response)", lines = true)

    for p <- 1 to 10 do                                                 // AR hyper-parameter settings
        hp("p") = p
        val mod = new AR (y, hh)                                        // create an AR model
        banner (s"TnT Test: ${mod.modelName} Model")
        mod.trainNtest ()()

        mod.setSkip (0)
        mod.rollValidate ()                                             // TnT with Rolling Validation
        mod.diagnoseAll (y, mod.getYf, Forecaster.teRng (y.dim))        // only diagnose on the testing set
    end for

end example_CovidTest6


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `example_CovidTest7` main function tests the `Example_Covid` object.
 *  Uses In-Sample Testing (In-ST), i.e., train and test on the same data.
 *  Runs Auto-Regressive, Moving Average ARMA(p, 0) models for several p and
 *  horizons 1 to 6, see sMAPE metrics below:
 *
 *  20.2191,    29.9108,    38.1525,    45.5858,    52.2918,    57.3670  ARMA(1, 0)
 *  17.7900,    25.3293,    33.3283,    39.5055,    44.9095,    50.6043  ARMA(2, 0)
 *  17.4057,    23.9135,    30.5357,    35.5950,    40.6434,    46.4122  ARMA(3, 0)
 *  17.2928,    23.6678,    29.5574,    34.0383,    38.9062,    44.1568  ARMA(4, 0)
 *  17.2850,    23.6708,    29.5699,    34.0520,    38.9330,    44.2125  ARMA(5, 0)
 *  17.3271,    23.9829,    29.9874,    34.6032,    39.0682,    43.6979  ARMA(6, 0)
 *  17.2335,    24.0097,    29.9465,    34.3426,    38.9182,    44.4357  ARMA(7, 0)
 *  17.2811,    23.7288,    29.5992,    34.0946,    38.6983,    44.1365  ARMA(8, 0)
 *  17.2044,    23.6396,    29.5609,    34.2834,    38.9406,    44.1984  ARMA(9, 0)
 *  17.2588,    23.6012,    29.4737,    34.3447,    39.0981,    44.1297  ARMA(10, 0)
 *
 *  > runMain scalation.modeling.forecasting.example_CovidTest7
 */
@main def example_CovidTest7 (): Unit =

    val hh = 6                                                          // max forecasting horizon
    val hp = AR.hp                                                      // hyper-parameters for AR family of models
    hp("q") = 0                                                         // no MA terms => AR with different optimizer

    new Plot (null, y, null, s"y ($response)", lines = true)

    for p <- 1 to 10 do                                                 // ARMA hyper-parameter settings
        hp("p") = p
        val mod = new ARMA (y, hh)                                      // create an ARMA model
        banner (s"In-ST Test: ${mod.modelName} Model")
        mod.trainNtest ()()

        mod.forecastAll ()
        mod.diagnoseAll (mod.getY, mod.getYf)
    end for

end example_CovidTest7


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `example_CovidTest8` main function tests the `Example_Covid` object.
 *  Uses Train-n-Test Split (TnT) with Rolling Validation.
 *  Runs Auto-Regressive, Moving Average ARMA(p, 0) models for several p values
 *  and horizons 1 to 6, see sMAPE metrics below:
 *
 *  19.0003,    30.3936,    43.8008,    54.8254,    65.3736,    74.5465  ARMA(1, 0)
 *  17.0385,    26.7633,    39.4985,    51.0132,    61.2488,    70.4454  ARMA(2, 0)
 *  16.0454,    22.1844,    31.7033,    41.1297,    51.6017,    61.3707  ARMA(3, 0)
 *  15.2966,    20.7829,    27.7076,    36.3322,    41.5452,    49.0153  ARMA(4, 0)
 *  15.6244,    20.6003,    29.0435,    36.8354,    43.1722,    48.1613  ARMA(5, 0)
 *  15.6619,    23.1335,    32.0946,    41.3166,    50.0557,    60.0608  ARMA(6, 0)
 *  16.0957,    22.2142,    32.4196,    39.8389,    47.6075,    51.5675  ARMA(7, 0)
 *  15.8659,    25.6319,    36.0707,    45.6189,    54.9417,    58.8670  ARMA(8, 0)
 *  15.5716,    24.2525,    34.1386,    44.2350,    55.1113,    60.8057  ARMA(9, 0)
 *  14.9008,    22.6571,    30.4335,    41.6601,    50.1669,    61.2246  ARMA(10, 0)
 *
 *  > runMain scalation.modeling.forecasting.example_CovidTest8
 */
@main def example_CovidTest8 (): Unit =

    val hh = 6                                                          // max forecasting horizon
    val hp = AR.hp                                                      // hyper-parameters for AR family of models
    hp("q") = 0                                                         // no MA terms => AR with different optimizer

    new Plot (null, y, null, s"y ($response)", lines = true)

    for p <- 1 to 10 do                                                 // ARMA hyper-parameter settings
        hp("p") = p
        val mod = new ARMA (y, hh)                                      // create an ARMA model
        banner (s"TnT Test: ${mod.modelName} Model")
        mod.trainNtest ()()

        mod.setSkip (0)
        mod.rollValidate ()                                             // TnT with Rolling Validation
        mod.diagnoseAll (y, mod.getYf, Forecaster.teRng (y.dim))        // only diagnose on the testing set
    end for

end example_CovidTest8


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `example_CovidTest9` main function tests the `Example_Covid` object.
 *  Uses In-Sample Testing (In-ST), i.e., train and test on the same data.
 *  Runs Auto-Regressive, Lagged Regression ARY(p) models for several p values and
 *  horizons 1 to 6, see sMAPE metrics below:
 *
 *  20.1794,    29.8589,    38.1450,    45.5634,    52.3478,    57.4474  ARY(1)
 *  17.7728,    25.1705,    33.1900,    39.4218,    44.8621,    50.5991  ARY(2)
 *  17.3594,    23.7550,    30.3838,    35.4514,    40.5868,    46.4292  ARY(3)
 *  17.2457,    23.5122,    29.4110,    33.9350,    38.8422,    44.2303  ARY(4)
 *  17.2314,    23.5178,    29.4345,    33.9602,    38.9022,    44.3249  ARY(5)
 *  17.2503,    23.8232,    29.8341,    34.4885,    39.0138,    43.8011  ARY(6)
 *  17.1625,    23.8385,    29.8227,    34.2751,    38.9853,    44.6092  ARY(7)
 *  17.2067,    23.5579,    29.4741,    34.0077,    38.6431,    44.3218  ARY(8)
 *  17.1326,    23.4530,    29.4149,    34.1103,    38.8254,    44.3564  ARY(9)
 *  17.1791,    23.4175,    29.3213,    34.1509,    38.8917,    44.2659  ARY(10)
 *
 *  > runMain scalation.modeling.forecasting.example_CovidTest9
 */
@main def example_CovidTest9 (): Unit =

    val hh = 6                                                          // max forecasting horizon
    hp("lambda") = 1.0                                                  // regularization parameter

    new Plot (null, y, null, s"y ($response)", lines = true)

    for p <- 1 to 10 do                                                 // ARY hyper-parameter settings
        hp("p") = p
        val mod = ARY (y, hh)                                           // create an ARY model
        banner (s"In-ST Test: ${mod.modelName} Model")
        mod.trainNtest_x ()()                                           // needs x matrix => use _x version

        mod.forecastAll ()
        mod.diagnoseAll (mod.getY, mod.getYf)
    end for

end example_CovidTest9


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `example_CovidTest10` main function tests the `Example_Covid` object.
 *  Uses Train-n-Test Split (TnT) with Rolling Validation.
 *  Runs Auto-Regressive, Lagged Regression ARY(p) models for several p values,
 *  and horizons 1 to 6, see sMAPE metrics below:
 *
 *  19.0003,    30.3936,    43.8008,    54.8254,    65.3736,    74.5465  ARY(1)
 *  16.8486,    26.3959,    39.1085,    50.6966,    61.0053,    70.3446  ARY(2)
 *  15.7448,    21.8608,    31.3677,    40.9140,    51.5319,    61.5140  ARY(3)
 *  14.7953,    20.1791,    26.5422,    35.2717,    40.7200,    48.6407  ARY(4)
 *  14.9856,    19.5241,    27.1485,    35.1070,    40.1716,    47.1898  ARY(5)
 *  15.0238,    21.1032,    28.4153,    36.6326,    42.5539,    49.8734  ARY(6)
 *  15.5620,    20.7860,    29.8501,    37.1646,    43.7716,    48.4778  ARY(7)
 *  15.1719,    23.2761,    32.2952,    40.3584,    46.1975,    51.3488  ARY(8)
 *  14.9497,    22.5065,    31.3207,    39.5034,    45.5495,    51.4103  ARY(9)
 *  14.4824,    21.5906,    29.9550,    37.9214,    43.3013,    52.2868  ARY(10)
 *
 *  FIX - discrepancy between rollValidate and diagnoseAll handled by sft parameter - why needed?
 *
 *  > runMain scalation.modeling.forecasting.example_CovidTest10
 */
@main def example_CovidTest10 (): Unit =

    val hh = 6                                                          // max forecasting horizon
    hp("lambda") = 1.0                                                  // regularization parameter

    new Plot (null, y, null, s"y ($response)", lines = true)

    for p <- 1 to 10 do                                                 // ARY hyper-parameter settings
        hp("p") = p
        val mod = ARY (y, hh)                                           // create an ARY model
        banner (s"TnT Test: ${mod.modelName} Model")
        mod.trainNtest_x ()()                                           // needs x matrix => use _x version

        mod.setSkip (0)
        mod.rollValidate ()                                             // TnT with Rolling Validation
        mod.diagnoseAll (y, mod.getYf, Forecaster.teRng (y.dim), 0)     // only diagnose on the testing set
    end for

end example_CovidTest10


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `example_CovidTest11` main function tests the `Example_Covid` object.
 *  Uses In-Sample Testing (In-ST), i.e., train and test on the same data.
 *  Runs Auto-Regressive, Lagged Regression, Direct ARY_D(p) models for several p values,
 *  and horizons 1 to 6, see sMAPE metrics below:
 *
 *  19.9912,    30.1349,    38.7483,    45.1096,    49.5424,    52.5320  ARY_D(1)
 *  17.7245,    24.2871,    31.1716,    35.9357,    40.5132,    46.4806  ARY_D(2)
 *  17.2367,    23.2007,    29.4120,    33.5757,    38.8647,    44.1707  ARY_D(3)
 *  17.1336,    23.1984,    29.1758,    33.5773,    38.6493,    43.8045  ARY_D(4)
 *  17.1196,    23.1224,    29.1769,    33.6120,    38.7839,    43.9346  ARY_D(5)
 *  17.1324,    23.1273,    29.2292,    33.8956,    39.1209,    44.0869  ARY_D(6)
 *  16.9815,    23.2879,    29.2536,    33.9433,    39.1474,    44.2361  ARY_D(7)
 *  17.0492,    23.1888,    29.2826,    34.0878,    39.2379,    44.7474  ARY_D(8)
 *  16.9841,    23.1090,    29.2154,    34.1249,    39.2711,    44.7709  ARY_D(9)
 *  17.0676,    23.1089,    28.9425,    33.9046,    38.9082,    44.0469  ARY_D(10)
 *
 *  > runMain scalation.modeling.forecasting.example_CovidTest11
 */
@main def example_CovidTest11 (): Unit =

    val hh = 6                                                          // max forecasting horizon
//  hp("lambda") = 1.0                                                  // regularization parameter

    new Plot (null, y, null, s"y ($response)", lines = true)

    for p <- 1 to 10 do                                                 // ARY hyper-parameter settings
        hp("p") = p
        val mod = ARY_D (y, hh)                                         // create an ARY_D model
        banner (s"In-ST Test: ${mod.modelName} Model")
        mod.trainNtest_x ()()                                           // note: suffix "_x" currently required

        mod.forecastAll (mod.getYy)                                     // forecast h-steps ahead (h = 1 to hh) for all y
        mod.diagnoseAll (mod.getY, mod.getYf)
    end for

end example_CovidTest11


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `example_CovidTest12` main function tests the `Example_Covid` object.
 *  Uses Train-n-Test Split (TnT) with Rolling Validation.
 *  Runs Auto-Regressive, Lagged Regression, Direct ARY_D(p) models for several p values,
 *  and horizons 1 to 6, see sMAPE metrics below:
 *
 *  18.9312,    31.2905,    45.7578,    57.0037,    65.9690,    72.4626  ARY_D(1)
 *  16.8059,    23.1653,    31.9736,    40.6603,    46.6809,    57.1835  ARY_D(2)
 *  15.9031,    20.7335,    27.3975,    35.5557,    39.3269,    51.2769  ARY_D(3)
 *  15.0132,    20.2209,    27.5774,    35.4134,    39.7899,    48.6745  ARY_D(4)
 *  15.2338,    19.4826,    27.6054,    35.6699,    39.8746,    48.4355  ARY_D(5)
 *  15.1603,    19.7425,    27.7367,    35.7799,    40.1055,    49.1122  ARY_D(6)
 *  15.5484,    22.7247,    31.0076,    38.5501,    44.5176,    50.8537  ARY_D(7)
 *  15.3248,    23.2628,    30.6794,    39.0621,    44.5661,    52.6579  ARY_D(8)
 *  15.0875,    21.7912,    30.2152,    37.4165,    42.6637,    52.9831  ARY_D(9)
 *  14.7569,    22.2172,    30.9435,    40.5641,    46.2016,    57.6445  ARY_D(10)
 *
 *  > runMain scalation.modeling.forecasting.example_CovidTest12
 */
@main def example_CovidTest12 (): Unit =

    val hh = 6                                                          // max forecasting horizon
//  hp("lambda") = 1.0                                                  // regularization parameter

    new Plot (null, y, null, s"y ($response)", lines = true)

    for p <- 1 to 10 do                                                 // ARX hyper-parameter settings
        hp("p") = p
        val mod = ARY_D (y, hh)                                         // create model ARY_D for time series data
        banner (s"TnT Test: ${mod.modelName} Model")
        mod.trainNtest_x ()()                                           // note: suffix "_x" currently required

        mod.setSkip (0)
        mod.rollValidate ()                                             // TnT with Rolling Validation
        mod.diagnoseAll (y, mod.getYf, Forecaster.teRng (y.dim), 0)     // only diagnose on the testing set
    end for

end example_CovidTest12


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `example_CovidTest13` main function tests the `Example_Covid` object.
 *  Uses In-Sample Testing (In-ST), i.e., train and test on the same data.
 *  Runs Auto-Regressive, Moving Average ARMA(p, q) models for several p values,
 *  and horizons 1 to 6, see sMAPE metrics below:
 *
 *  FIX - good for h = 1, but then sMAPE scores explode
 *
 *  > runMain scalation.modeling.forecasting.example_CovidTest13
 */
@main def example_CovidTest13 (): Unit =

    val hh = 6                                                          // max forecasting horizon
    val hp = AR.hp                                                      // hyper-parameters for AR family of models
    hp("q") = 1                                                         // one MA term

    new Plot (null, y, null, s"y ($response)", lines = true)

    for p <- 2 to 2 do                                                  // ARMA hyper-parameter settings
        hp("p") = p
        val mod = new ARMA (y, hh)                                      // create an ARMA model
        banner (s"In-ST Test: ${mod.modelName} Model")
        mod.trainNtest ()()

        mod.forecastAll ()
        mod.diagnoseAll (mod.getY, mod.getYf)
    end for

end example_CovidTest13


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `example_CovidTest14` main function tests the `Example_Covid` object.
 *  Uses Train-n-Test Split (TnT) with Rolling Validation.
 *  Runs Auto-Regressive, Moving Average ARMA(p, q) models for several p values.
 *  and horizons 1 to 6, see sMAPE metrics below:
 * 
 *  FIX - for all h sMAPE scores have exploded
 *
 *  > runMain scalation.modeling.forecasting.example_CovidTest14
 */
@main def example_CovidTest14 (): Unit =

    val hh = 6                                                          // max forecasting horizon
    val hp = AR.hp                                                      // hyper-parameters for AR family of models
    hp("q") = 1                                                         // one MA term

    new Plot (null, y, null, s"y ($response)", lines = true)

    for p <- 2 to 2 do                                                  // ARMA hyper-parameter settings
        hp("p") = p
        val mod = new ARMA (y, hh)                                      // create an ARMA model
        banner (s"TnT Test: ${mod.modelName} Model")
        mod.trainNtest ()()

        mod.setSkip (0)
        mod.rollValidate ()                                             // TnT with Rolling Validation
        mod.diagnoseAll (y, mod.getYf, Forecaster.teRng (y.dim))        // only diagnose on the testing set
    end for

end example_CovidTest14


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `example_CovidTest15` main function tests the `Example_Covid` object.
 *  Uses In-Sample Testing (In-ST), i.e., train and test on the same data.
 *  Runs Auto-Regressive, Exogenous ARX(p, q, n) models for several p values,
 *  and horizons 1 to 6, see sMAPE metrics below:
 *
 *  18.3346,    26.5990,    35.8624,    44.8289,    53.7512,    60.5086  ARX(1, 1, 2)
 *  15.5184,    20.9192,    27.8176,    35.3589,    43.9210,    50.5047  ARX(2, 2, 2)
 *  15.3592,    20.1736,    25.4967,    32.6258,    40.4916,    47.2481  ARX(3, 2, 2)
 *  15.3224,    19.8423,    25.0511,    31.9170,    38.9812,    45.6829  ARX(4, 2, 2)
 *  15.3200,    19.8433,    25.0510,    31.9146,    38.9858,    45.6849  ARX(5, 2, 2)
 *  15.4286,    19.9065,    25.7220,    32.6493,    39.6406,    46.0115  ARX(6, 2, 2)
 *  15.3576,    19.9718,    25.4068,    32.3474,    39.0521,    45.5616  ARX(7, 2, 2)
 *  15.4913,    19.5610,    25.4153,    32.2240,    39.3885,    45.8530  ARX(8, 2, 2)
 *  15.3410,    19.6328,    25.6180,    32.6323,    39.8298,    46.6052  ARX(9, 2, 2)
 *  15.4446,    19.6831,    25.6035,    32.8968,    40.6220,    47.7878  ARX(10, 2, 2)
 *
 *  > runMain scalation.modeling.forecasting.example_CovidTest15
 */
@main def example_CovidTest15 (): Unit =

//  val exo_vars  = Array ("icu_patients", "hosp_patients", "new_tests", "people_vaccinated")
    val exo_vars  = Array ("icu_patients", "hosp_patients")
    val (xxe, yy) = loadData (exo_vars, response)
    println (s"xxe.dims = ${xxe.dims}, yy.dim = ${yy.dim}")

//  val xe = xxe                                                        // full
    val xe = xxe(0 until 116)                                           // clip the flat end
//  val y  = yy                                                         // full
    val y  = yy(0 until 116)                                            // clip the flat end
    val hh = 6                                                          // maximum forecasting horizon

    banner (s"exo_vars = ${stringOf (exo_vars)}, endo_var = $response")
    println (s"xe.dims = ${xe.dims}, y.dim = ${y.dim}")
    new Plot (null, y, null, s"y ($response)", lines = true)

    for p <- 1 to 10 do                                                 // ARX hyper-parameter settings
        hp("p") = p
        hp("q") = min (2, p)
        val mod = ARX (xe, y, hh)                                       // create model for time series data
        banner (s"In-ST Test: ${mod.modelName} Model")
        mod.trainNtest_x ()()                                           // train and test on full dataset
 
        mod.forecastAll ()
        mod.diagnoseAll (mod.getY, mod.getYf)
    end for

end example_CovidTest15


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `example_CovidTest16` main function tests the `Example_Covid` object.
 *  Uses Train-n-Test Split (TnT) with Rolling Validation.
 *  Runs Auto-Regressive, Exogenous ARX(p, q, n) models for several p values,
 *  and horizons 1 to 6, see sMAPE metrics below:
 *
 *  12.2356,    20.6830,    35.2603,    43.9974,    51.5944,    52.0301  ARX(1, 1, 2)
 *  9.72391,    20.6254,    25.4950,    34.2458,    44.5078,    49.9804  ARX(2, 2, 2)
 *  10.0738,    21.4470,    26.2178,    34.2212,    44.0982,    49.4524  ARX(3, 2, 2)
 *  9.29391,    19.6487,    22.8980,    31.6528,    41.6049,    46.9430  ARX(4, 2, 2)
 *  10.2806,    19.2649,    23.1211,    32.1942,    41.9189,    47.2119  ARX(5, 2, 2)
 *  11.4258,    19.7370,    24.5103,    34.4673,    44.9873,    49.7458  ARX(6, 2, 2)
 *  11.2501,    19.0128,    22.3547,    31.9938,    42.1729,    47.1063  ARX(7, 2, 2)
 *  10.9763,    18.8067,    22.5181,    32.0960,    41.8394,    47.1825  ARX(8, 2, 2)
 *  11.1796,    19.3087,    23.7479,    33.1067,    42.8283,    47.6904  ARX(9, 2, 2)
 *  10.9499,    20.6255,    25.8116,    35.5139,    45.2163,    50.0280  ARX(10, 2, 2)
 *
 *  > runMain scalation.modeling.forecasting.example_CovidTest16
 */
@main def example_CovidTest16 (): Unit =

//  val exo_vars  = Array ("icu_patients", "hosp_patients", "new_tests", "people_vaccinated")
    val exo_vars  = Array ("icu_patients", "hosp_patients")
    val (xxe, yy) = loadData (exo_vars, response)
    println (s"xxe.dims = ${xxe.dims}, yy.dim = ${yy.dim}")

//  val xe = xxe                                                        // full
    val xe = xxe(0 until 116)                                           // clip the flat end
//  val y  = yy                                                         // full
    val y  = yy(0 until 116)                                            // clip the flat end
    val hh = 6                                                          // maximum forecasting horizon

    banner (s"exo_vars = ${stringOf (exo_vars)}, endo_var = $response")
    println (s"xe.dims = ${xe.dims}, y.dim = ${y.dim}")
    new Plot (null, y, null, s"y ($response)", lines = true)

    for p <- 1 to 10 do                                                 // ARX hyper-parameter settings
        hp("p") = p
        hp("q") = min (2, p)
        val mod = ARX (xe, y, hh)                                       // create an ARX model
        banner (s"TnT Test: ${mod.modelName} Model")
        mod.trainNtest_x ()()                                           // use customized trainNtest_x

        mod.setSkip (0)
        mod.rollValidate ()                                             // TnT with Rolling Validation
        println (s"After Roll TnT Forecast Matrix yf = ${mod.getYf}")
        mod.diagnoseAll (y, mod.getYf, Forecaster.teRng (y.dim), 0)     // only diagnose on the testing set
    end for

end example_CovidTest16


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `example_CovidTest17` main function tests the `Example_Covid` object.
 *  Uses In-Sample Testing (In-ST), i.e., train and test on the same data.
 *  Runs Auto-Regressive, Exogenous ARX_D(p, q, n) models for several p values.
 *  > runMain scalation.modeling.forecasting.example_CovidTest17
 *
@main def example_CovidTest17 (): Unit =

    val exo_vars = Array ("icu_patients", "hosp_patients", "new_tests", "people_vaccinated")
    val (xx, yy) = Example_Covid.loadData (exo_vars, response)
//  val (x, y)   = (xx, yy)                                             // full
    val (x, y)   = (xx(0 until 116), yy(0 until 116))                   // clip the flat end
    val hh = 6                                                          // max forecasting horizon

    banner (s"exo_vars = ${stringOf (exo_vars)}, endo_var = $response")
    println (s"x.dims = ${x.dims}, y.dim = ${y.dim}")
    new Plot (null, y, null, s"y ($response)", lines = true)

    for p <- 1 to 10 do                                                 // ARX hyper-parameter settings
        val mod = ARX_MV.exo (y, p, x, hh)(1, p+1)                      // create an ARX_MV model
        banner (s"In-ST Test: ${mod.modelName} Model")
        mod.trainNtest ()()
    end for

end example_CovidTest17
 */


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `example_CovidTest18` main function tests the `Example_Covid` object.
 *  Uses Train-n-Test Split (TnT) with Rolling Validation.
 *  Runs Auto-Regressive, Exogenous ARX_D(p, q, n) models for several p values.
 *  > runMain scalation.modeling.forecasting.example_CovidTest18
 *
@main def example_CovidTest18 (): Unit =

    val exo_vars = Array ("icu_patients", "hosp_patients", "new_tests", "people_vaccinated")
    val (xx, yy) = Example_Covid.loadData (exo_vars, response)
//  val (x, y)   = (xx, yy)                                             // full
    val (x, y)   = (xx(0 until 116), yy(0 until 116))                   // clip the flat end
    val hh = 6                                                          // max forecasting horizon

    banner (s"exo_vars = ${stringOf (exo_vars)}, endo_var = $response")
    println (s"x.dims = ${x.dims}, y.dim = ${y.dim}")
    new Plot (null, y, null, s"y ($response)", lines = true)

    for p <- 1 to 10 do                                                 // ARX hyper-parameter settings
        val mod = ARX_MV.exo (y, p, x, hh)(1, p+1)                      // create an ARX_MV model
        banner (s"TnT Test: ${mod.modelName} Model")
        mod.trainNtest ()()
        ARX_MV.rollValidate (mod)                                       // direct does all horizon at once
    end for

end example_CovidTest18
 */


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `example_CovidTest19` main function tests the `Example_Covid` object.
 *  Uses In-Sample Testing (In-ST), i.e., train and test on the same data.
 *  Runs Auto-Regressive, Exogenous ARX_Quad_D(p, q, n) models for several p values.
 *  > runMain scalation.modeling.forecasting.example_CovidTest19
 *
@main def example_CovidTest19 (): Unit =

    val exo_vars = Array ("icu_patients", "hosp_patients", "new_tests", "new_cases", "people_vaccinated", "people_fully_vaccinated")
//  val exo_vars = Array ("icu_patients", "hosp_patients", "new_tests", "people_vaccinated")
    val (xx, yy) = Example_Covid.loadData (exo_vars, response)
//  val (x, y)   = (xx, yy)                                             // full
    val (x, y)   = (xx(0 until 116), yy(0 until 116))                   // clip the flat end
    val hh = 6                                                          // max forecasting horizon

    banner (s"exo_vars = ${stringOf (exo_vars)}, endo_var = $response")
    println (s"x.dims = ${x.dims}, y.dim = ${y.dim}")
    new Plot (null, y, null, s"y ($response)", lines = true)

    for p <- 1 to 10 do                                                 // ARX hyper-parameter settings
        val mod = ARX_Quad_MV.exo (y, p, x, hh)(1, p+1)                 // create an ARX_Quad_MV model
        banner (s"In-ST Test: ${mod.modelName} Model")
        mod.trainNtest ()()
    end for

end example_CovidTest19
 */


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `example_CovidTest20` main function tests the `Example_Covid` object.
 *  Uses Train-n-Test Split (TnT) with Rolling Validation.
 *  Runs Auto-Regressive, Exogenous ARX_Quad_D(p, q, n) models for several p values.
 *  > runMain scalation.modeling.forecasting.example_CovidTest20
 *
@main def example_CovidTest20 (): Unit =

    val exo_vars = Array ("icu_patients", "hosp_patients", "new_tests", "people_vaccinated")
//  val exo_vars = Array ("icu_patients", "hosp_patients", "new_tests", "new_cases", "people_vaccinated", "people_fully_vaccinated")
    val (xx, yy) = Example_Covid.loadData (exo_vars, response)
//  val (x, y)   = (xx, yy)                                             // full
    val (x, y)   = (xx(0 until 116), yy(0 until 116))                   // clip the flat end
    val hh = 6                                                          // max forecasting horizon
    val pw = 1.5                                                        // power pw: tune with values around 2.0

    banner (s"exo_vars = ${stringOf (exo_vars)}, endo_var = $response")
    println (s"x.dims = ${x.dims}, y.dim = ${y.dim}")
    new Plot (null, y, null, s"y ($response)", lines = true)

    for p <- 1 to 10 do                                                 // ARX hyper-parameter settings
        val mod = ARX_Quad_MV.exo (y, p, x, hh, pw)(1, p+1)             // create an ARX_Quad_MV model
        banner (s"TnT Test: ${mod.modelName} Model")
        mod.trainNtest ()()
        ARX_MV.rollValidate (mod)                                       // direct does all horizon at once
    end for

end example_CovidTest20
 */

