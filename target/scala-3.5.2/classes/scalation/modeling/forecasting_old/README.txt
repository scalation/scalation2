
This package consists of older and experimental code.
End users should use the `forecasting` package

Simple Univariate Time Series Models
------------------------------------

Utilities:

Forecaster.scala          -- Base Trait for Forecasters with Vector Input
RollingValidation.scala   -- Rolling Validation for Forecasters
Stationarity.scala        -- Unit Root Tests for Time Series Stationarity
Stationarity_KPSS.scala   -- Kwiatkowski–Phillips–Schmidt–Shin (KPSS) Test for Stationarity

Baseline:

NullModel.scala           -- Null/Mean Model:
                             y_t = mean
RandomWalk.scala          -- Random Walk/Last-Value Model:
                             y_t = y_t-1
MovingAverage.scala       -- Simple Moving Average Model:
                             y_t = mean of last q values
TrendModel.scala          -- Linear Trend Model:
                             y_t = a + b t

Simple Models:

SimpleExpSmoothing.scala  -- Simple Exponential Smoothing (SES) Model:
                             s_t = α y_t-1 + (1-α)s_t-1; y_t = s_t 
QuadSpline.scala          -- Quadratic Spline Model:
                             y_t = y_t = a + b t + c t^2
AR.scala                  -- Auto-Regressive (AR) Model:                
                             y_t = δ + φ_1 y_t-1 + ... + φ_p y_t-p
ARMA.scala                -- Auto-Regressive, Moving-Average (ARMA) Model:
                             y_t = δ + φ_1 y_t-1 + ... + φ_p y_t-p
                                     + θ_1 e_t-1 + ... + θ_q e_t-q

