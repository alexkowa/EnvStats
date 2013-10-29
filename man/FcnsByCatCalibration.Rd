\name{FcnsByCatCalibration}
\alias{FcnsByCatCalibration}
\alias{Calibration}
\concept{Inverse Prediction}
\title{
  EnvStats Functions for Calibration
}
\description{
  The \pkg{EnvStats} functions listed below are useful for performing calibration and 
  inverse prediction to determine the concentration of a chemical based on a machine signal.
}
\details{
  \tabular{ll}{
    \emph{Function Name}                  \tab \emph{Description} \cr
    \code{\link{anovaPE}}                 \tab Compute lack-of-fit and pure error ANOVA table for a \cr
                                          \tab linear model. \cr
    \code{\link{calibrate}}               \tab Fit a calibration line or curve. \cr
    \code{\link{detectionLimitCalibrate}} \tab Determine detection limit based on using a calibration \cr
                                          \tab line (or curve) and inverse regression. \cr
    \code{\link{inversePredictCalibrate}} \tab Predict concentration using a calibration line (or curve) \cr
                                          \tab and inverse regression. \cr
    \code{\link{pointwise}}               \tab Pointwise confidence limits for predictions. \cr
    \code{\link{predict.lm}}              \tab Predict method for linear model fits. \cr
  }
}
\keyword{ package }


