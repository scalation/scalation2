
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Tue Jul  6 11:33:53 EDT 2021
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Test: Inverse via Matrix Factorization
 */

package scalation
package mathstat

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `inverseTest` main function checks the correctness of inverse matrices produced
 *  by several matrix factorization algorithms.
 *  > runMain scalation.mathstat.inverseTest
 */
@main def inverseTest (): Unit =

    def test (a: MatrixD, fac: Factorization): Unit =
        val a_inv = fac.inverse
        println (s"a_inv = $a_inv")
        println (s"a * a_inv = ${a * a_inv}")
    end test

    val a = MatrixD ((3, 3), 1, 2, 3,
                             2, 5, 6,
                             9, 3, 7)
    println (s"a = $a")

    banner ("Fac_Inverse: Inverse Test a")
    test (a, new Fac_Inverse (a))

    banner ("Fac_LU: Inverse Test a")
    test (a, new Fac_LU (a))

    banner ("Fac_Cholesky: Inverse Test a")
    test (a, new Fac_Cholesky (a))

    banner ("Fac_QR: Inverse Test a")
    test (a, new Fac_QR (a, true))

    banner ("Fac_SVD: Inverse Test a")
    test (a, new Fac_SVD (a))

/*
    val b = MatrixD ((3, 3), 1, 2, 3,
                             2, 5, 6,
                             3, 6, 7)
*/
    val b = a.transpose * a                       // symmetric and positive definite
    println (s"b = $b")

    banner ("Fac_Inverse: Inverse Test b")
    test (b, new Fac_Inverse (b))

    banner ("Fac_LU: Inverse Test b")
    test (b, new Fac_LU (b))

    banner ("Fac_Cholesky: Inverse Test b")
    test (b, new Fac_Cholesky (b))

    banner ("Fac_QR: Inverse Test b")
    test (b, new Fac_QR (b, true))

    banner ("Fac_SVD: Inverse Test b")
    test (b, new Fac_SVD (b))

end inverseTest

