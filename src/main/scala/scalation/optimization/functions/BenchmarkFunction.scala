
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @note    Mon Jan 22 15:14:41 EST 2024
 *  @see     LICENSE (MIT style license file).
 *------------------------------------------------------------------------------
 *  Trait to specify the logic needed by an object that represents a benchmark
 *  function.
 */

// Package definition.
package scalation
package optimization
package functions

// Project imports.
import scalation.calculus.Differential
import scalation.mathstat.VectorD
import scalation.optimization.quasi_newton.FunctionOptimization
import scalation.optimization.quasi_newton.lbfgs_ffm.FunctionOptimizationFFM

// Trait.
/** The `BenchmarkFunction` trait specifies the requirements for the logic
 *  of an object representing a benchmark function. The methods provided in this
 *  trait are used for tests and benchmarks performed on function optimization
 *  and gradient descent classes.
 *
 *  Classes mixing in this trait must declare the `functionMinimum` field and
 *  implement the `objectiveFunction` method. Additionally, overriding the
 *  default implementation of the `gradientFunction` method is highly
 *  recommended. The `objectiveFunction` method represents the mathematical
 *  function the object will model. The `functionMinimum` field represents the
 *  variable values that minimize the output of the `objectiveFunction` method.
 *  Finally, the `gradientFunction` method represents the gradient function for
 *  the `objectiveFunction` function. An approximation for this method is
 *  automatically provided by making use of the [[Differential]] class, but
 *  overriding it with a hard-coded definition of the gradient function is
 *  highly recommended as it will greatly improve the accuracy of the results.
 */
trait BenchmarkFunction:
    // Public fields.
    val functionMinimum: VectorD

    // Public methods.
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The objective function used for benchmarking or testing purposes. Can be
     *  any mathematical function that receives an arbitrary number of real
     *  numbers as input and produces a single real number as an output.
     *
     *  @param x        [[VectorD]] with the values of the variables to be used
     *                  as input for the objective function.
     *  @return Double  The output of the objective function given by using the
     *                  values in `x` as input.
     *
     */
    def objectiveFunction(x: VectorD): Double

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The mathematical function that represents the gradient of the objective
     *  function represented by `objectiveFunction`. An approximation using the
     *  [[Differential]] class is provided as the default implementation of this
     *  method, but it is highly encouraged to override this method with a
     *  hard-coded implementation of the correct gradient function in order to
     *  greatly improve the accuracy of the results.
     *
     *  The function described in this method implementation should correspond
     *  to the gradient of the function described in `objectiveFunction` or else
     *  the results obtained in any tests or benchmarks will be void of meaning.
     *
     *  @param x        [[VectorD]] with the values of the variables to be used
     *                  as input for the gradient function.
     *  @return VectorD The gradient of the objective function in the position
     *                  given by the values in `x`.
     *
     */
    def gradientFunction(x: VectorD): VectorD = Differential.grad(objectiveFunction, x)
    
    // Final public methods.
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Creates a new [[FunctionOptimization]] case class with the objective
     *  function information represented by `objectiveFunction` and
     *  `gradientFunction`. Useful for running tests and benchmarks on the
     *  [[LBFGS]] object.
     *
     *  @return FunctionOptimization    [[FunctionOptimization]] case class
     *                                  created with the objective function
     *                                  information contained in
     *                                  `objectiveFunction` and
     *                                  `gradientFunction`.
     *
     */
    final def toFunctionOptimization: FunctionOptimization =
        FunctionOptimization(this.objectiveFunction, this.gradientFunction)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Creates a new [[FunctionOptimizationFFM]] case class with the objective
     *  function information represented by `objectiveFunction` and
     *  `gradientFunction`. Useful for running tests and benchmarks on the
     *  `LBFGS_FFM` object.
     *
     *  @return FunctionOptimizationFFM [[FunctionOptimizationFFM]] case class
     *                                  created with the objective function
     *                                  information contained in
     *                                  `objectiveFunction` and
     *                                  `gradientFunction`.
     *
     */
    final def toFunctionOptimizationFFM: FunctionOptimizationFFM =
        FunctionOptimizationFFM(this.objectiveFunction, this.gradientFunction)

end BenchmarkFunction
