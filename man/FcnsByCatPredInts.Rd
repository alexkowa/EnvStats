\name{FcnsByCatPredInts}
\alias{FcnsByCatPredInts}
\alias{Prediction Intervals}
\title{
  EnvStats Functions for Prediction Intervals
}
\description{
  The \pkg{EnvStats} functions listed below are useful for computing prediction intervals and 
  simultaneous prediction intervals.  See \link[=FcnsByCatPower]{Power and Sample Size} for a 
  list of functions useful for computing power and sample size for a design based on a 
  prediction interval width, or a design based on a hypothesis test for future observations 
  falling outside of a prediciton interval.
}
\details{
  \tabular{ll}{
  \emph{Function Name}                    \tab \emph{Description} \cr
  \code{\link{predIntGamma}},             \tab Prediction interval for the next \eqn{k} \cr
  \code{\link{predIntGammaAlt}}           \tab observations or next set of \eqn{k} means for a \cr
                                          \tab Gamma distribution. \cr
  \code{\link{predIntGammaSimultaneous}}, \tab Construct a simultaneous prediction interval for the \cr
  \code{\link{predIntGammaAltSimultaneous}} \tab next \eqn{r} sampling occasions based on a \cr
                                          \tab Gamma distribution. \cr
                                          \tab \cr
  \code{\link{predIntLnorm}},             \tab Prediction interval for the next \eqn{k} \cr
  \code{\link{predIntLnormAlt}}           \tab observations or geometric means from a \cr
                                          \tab Lognormal distribution. \cr
  \code{\link{predIntLnormSimultaneous}}, \tab Construct a simultaneous prediction interval for the \cr
  \code{\link{predIntLnormAltSimultaneous}} \tab next \eqn{r} sampling occasions based on a \cr
                                          \tab Lognormal distribution. \cr
                                          \tab \cr
  \code{\link{predIntNorm}}               \tab Prediction interval for the next \eqn{k} observations \cr
                                          \tab or means from a Normal (Gaussian) distribution. \cr
  \code{\link{predIntNormK}}              \tab Compute the value of \eqn{K} for a prediction interval \cr
                                          \tab for a Normal distribution. \cr
  \code{\link{predIntNormSimultaneous}}   \tab Construct a simultaneous prediction interval for the \cr
                                          \tab next \eqn{r} sampling occasions based on a \cr
                                          \tab Normal distribution. \cr
  \code{\link{predIntNormSimultaneousK}}  \tab Compute the value of \eqn{K} for a simultaneous \cr
                                          \tab prediction interval for the next \eqn{r} sampling \cr
                                          \tab occasions based on a Normal distribution. \cr
                                          \tab \cr
  \code{\link{predIntNpar}}               \tab Nonparametric prediction interval for the next \eqn{k} \cr
                                          \tab of \eqn{K} observations. \cr
  \code{\link{predIntNparSimultaneous}}   \tab Construct a nonparametric simultaneous prediction \cr
                                          \tab interval for the next \eqn{r} sampling occasions. \cr
                                          \tab \cr  
  \code{\link{predIntPois}}               \tab Prediction interval for the next \eqn{k} observations \cr
                                          \tab or sums from a Poisson distribution. \cr
  } 
}
\keyword{ package }
