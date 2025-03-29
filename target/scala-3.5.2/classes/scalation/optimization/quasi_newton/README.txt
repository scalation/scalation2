
ScalaTion Quasi-Newton Optimizers:
---------------------------------

Regular:
    BFGS.scala    -- Broyden–Fletcher–Goldfarb–Shanno
    LBFGS.scala   -- Limited-Memory Broyden–Fletcher–Goldfarb–Shanno

With Bounds:
    BFGS_B.scala
    LBFGS_B.scala

With No Line Search:
    BFGS_NoLS.scala
    LBFGS_NoLS.scala

With Momentum:
    DM_BFGS.scala
    DM_LBFGS.scala

Stochastic Versions:
    S_BFGS.scala
    S_LBFGS.scala

Solve Methods:
-------------

solve  -- numerical gradient computation
solve2 -- explicit gradient vector function given

BFGS has solve3 (numerical graient), solve4 that use LBFGS line search algorithms

Hyper-parameters:
----------------

Step Size
Momentum
Line Search

Test Cases:
----------

ARMA.scala -- compare with SARIMAX from statsmodels
    Lake Levels
    COVID-19
    Influenze-Like Illness (ILI)

