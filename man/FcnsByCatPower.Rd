\name{FcnsByCatPower}
\alias{FcnsByCatPower}
\alias{Power and Sample Size Calculations}
\alias{Power and Sample Size}
\title{
  EnvStats Functions for Power and Sample Size Calculations
}
\description{
  The \pkg{EnvStats} functions listed below are useful for power and sample size calculations.
}
\details{
  \bold{Confidence Intervals}

  \tabular{ll}{
  \emph{Function Name}                    \tab \emph{Description} \cr
  \code{\link{ciTableProp}}               \tab Confidence intervals for binomial proportion, or \cr
                                          \tab difference between two proportions, following Bacchetti (2010) \cr
  \code{\link{ciBinomHalfWidth}}          \tab Compute the half-width of a confidence interval for a \cr
                                          \tab Binomial proportion or the difference between two proportions. \cr
  \code{\link{ciBinomN}}                  \tab Compute the sample size necessary to achieve a specified \cr
                                          \tab half-width of a confidence interval for a Binomial proportion or \cr
                                          \tab the difference between two proportions. \cr
  \code{\link{plotCiBinomDesign}}         \tab Create plots for a sampling design based on a confidence interval \cr
                                          \tab for a Binomial proportion or the difference between two proportions. \cr
  \code{\link{ciTableMean}}               \tab Confidence intervals for mean of normal distribution, or \cr
                                          \tab difference between two means, following Bacchetti (2010) \cr
  \code{\link{ciNormHalfWidth}}           \tab Compute the half-width of a confidence interval for the mean of a \cr
                                          \tab Normal distribution or the difference between two means. \cr
  \code{\link{ciNormN}}                   \tab Compute the sample size necessary to achieve a specified half-width \cr
                                          \tab of a confidence interval for the mean of a Normal distribution or \cr
                                          \tab the difference between two means. \cr
  \code{\link{plotCiNormDesign}}          \tab Create plots for a sampling design based on a confidence interval \cr
                                          \tab for the mean of a Normal distribution or the difference between \cr
                                          \tab two means. \cr
  \code{\link{ciNparConfLevel}}           \tab Compute the confidence level associated with a nonparametric \cr
                                          \tab confidence interval for a percentile.                        \cr
  \code{\link{ciNparN}}                   \tab Compute the sample size necessary to achieve a specified    \cr
                                          \tab confidence level for a nonparametric confidence interval for \cr
                                          \tab a percentile.                                               \cr
  \code{\link{plotCiNparDesign}}          \tab Create plots for a sampling design based on a nonparametric \cr
                                          \tab confidence interval for a percentile.
  } 


  \bold{Hypothesis Tests}

  \tabular{ll}{
  \emph{Function Name}                    \tab \emph{Description} \cr
  \code{\link{aovN}}                      \tab Compute the sample sizes necessary to achieve a \cr
                                          \tab specified power for a one-way fixed-effects analysis \cr
                                          \tab of variance test. \cr
  \code{\link{aovPower}}                  \tab Compute the power of a one-way fixed-effects analysis of \cr
                                          \tab variance test. \cr
  \code{\link{plotAovDesign}}             \tab Create plots for a sampling design based on a one-way \cr
                                          \tab analysis of variance. \cr
  \code{\link{propTestN}}                 \tab Compute the sample size necessary to achieve a specified \cr
                                          \tab power for a one- or two-sample proportion test. \cr
  \code{\link{propTestPower}}             \tab Compute the power of a one- or two-sample proportion test. \cr
  \code{\link{propTestMdd}}               \tab Compute the minimal detectable difference associated with \cr
                                          \tab a one- or two-sample proportion test. \cr
  \code{\link{plotPropTestDesign}}        \tab Create plots involving sample size, power, difference, and \cr
                                          \tab significance level for a one- or two-sample proportion test. \cr
  \code{\link{tTestAlpha}}                \tab Compute the Type I Error associated with specified values for \cr
                                          \tab for power, sample size(s), and scaled MDD for a one- or \cr
                                          \tab two-sample t-test. \cr
  \code{\link{tTestN}}                    \tab Compute the sample size necessary to achieve a specified \cr
                                          \tab power for a one- or two-sample t-test. \cr
  \code{\link{tTestPower}}                \tab Compute the power of a one- or two-sample t-test. \cr
  \code{\link{tTestScaledMdd}}            \tab Compute the scaled minimal detectable difference \cr
                                          \tab associated with a one- or two-sample t-test. \cr
  \code{\link{plotTTestDesign}}           \tab Create plots for a sampling design based on a one- or \cr
                                          \tab two-sample t-test. \cr
  \code{\link{tTestLnormAltN}}            \tab Compute the sample size necessary to achieve a specified \cr
                                          \tab power for a one- or two-sample t-test, assuming lognormal \cr
                                          \tab data. \cr
  \code{\link{tTestLnormAltPower}}        \tab Compute the power of a one- or two-sample t-test, assuming \cr
                                          \tab lognormal data. \cr
  \code{\link{tTestLnormAltRatioOfMeans}} \tab Compute the minimal or maximal detectable ratio of means \cr 
                                          \tab associated with a one- or two-sample t-test, assuming \cr
                                          \tab lognormal data. \cr
  \code{\link{plotTTestLnormAltDesign}}   \tab Create plots for a sampling design based on a one- or \cr
                                          \tab two-sample t-test, assuming lognormal data. \cr
  \code{\link{linearTrendTestN}}          \tab Compute the sample size necessary to achieve a specified \cr
                                          \tab power for a t-test for linear trend. \cr
  \code{\link{linearTrendTestPower}}      \tab Compute the power of a t-test for linear trend. \cr
  \code{\link{linearTrendTestScaledMds}}  \tab Compute the scaled minimal detectable slope for a t-test \cr
                                          \tab for linear trend. \cr
  \code{\link{plotLinearTrendTestDesign}} \tab Create plots for a sampling design based on a t-test for \cr
                                          \tab linear trend. \cr
  }


  \bold{Prediction Intervals}

  \emph{Normal Distribution Prediction Intervals}

  \tabular{ll}{
  \emph{Function Name}                    \tab \emph{Description} \cr
  \code{\link{predIntNormHalfWidth}}      \tab Compute the half-width of a prediction \cr
                                          \tab interval for a normal distribution. \cr
  \code{\link{predIntNormK}}              \tab Compute the required value of \eqn{K} for \cr
                                          \tab a prediction interval for a Normal \cr
                                          \tab distribution. \cr                        
  \code{\link{predIntNormN}}              \tab Compute the sample size necessary to \cr
                                          \tab achieve a specified half-width for a \cr
                                          \tab prediction interval for a Normal \cr
                                          \tab distribution. \cr
  \code{\link{plotPredIntNormDesign}}     \tab Create plots for a sampling design \cr
                                          \tab based on the width of a prediction \cr
                                          \tab interval for a Normal distribution. \cr
  \code{\link{predIntNormTestPower}}      \tab Compute the probability that at least \cr
                                          \tab one future observation (or mean) \cr 
                                          \tab falls outside a prediction interval \cr
                                          \tab for a Normal distribution. \cr
  \code{\link{plotPredIntNormTestPowerCurve}} \tab Create plots for a sampling \cr
                                          \tab design based on a prediction interval \cr
                                          \tab for a Normal distribution. \cr
  \code{\link{predIntNormSimultaneousTestPower}} 
                                          \tab Compute the probability that at \cr
                                          \tab least one set of future observations \cr
                                          \tab (or means) violates the given rule \cr
                                          \tab based on a simultaneous prediction \cr
                                          \tab interval for a Normal distribution. \cr
  \code{\link{plotPredIntNormSimultaneousTestPowerCurve}} \tab Create plots for a sampling design \cr
                                          \tab based on a simultaneous prediction \cr
                                          \tab interval for a Normal distribution. \cr
  }

  \emph{Lognormal Distribution Prediction Intervals}

  \tabular{ll}{
  \emph{Function Name}                    \tab \emph{Description} \cr
  \code{\link{predIntLnormAltTestPower}}  \tab Compute the probability that at least \cr
                                          \tab one future observation (or geometric \cr
                                          \tab mean) falls outside a prediction \cr
                                          \tab interval for a lognormal distribution. \cr
  \code{\link{plotPredIntLnormAltTestPowerCurve}} \tab Create plots for a sampling design \cr
                                          \tab based on a prediction interval for a \cr
                                          \tab lognormal distribution. \cr
  \code{\link{predIntLnormAltSimultaneousTestPower}} \tab Compute the probability that at least \cr
                                          \tab one set of future observations (or \cr
                                          \tab geometric means) violates the given \cr
                                          \tab rule based on a simultaneous \cr
                                          \tab prediction interval for a lognormal \cr
                                          \tab distribution. \cr
  \code{\link{plotPredIntLnormAltSimultaneousTestPowerCurve}} \tab Create plots for a sampling design \cr
                                          \tab based on a simultaneous prediction \cr
                                          \tab interval for a lognormal distribution. \cr
  }

  \emph{Nonparametric Prediction Intervals}

  \tabular{ll}{
  \emph{Function Name}                    \tab \emph{Description} \cr
                                          \tab \cr
  \code{\link{predIntNparConfLevel}}      \tab Compute the confidence level associated with \cr
                                          \tab a nonparametric prediction interval. \cr
  \code{\link{predIntNparN}}              \tab Compute the required sample size to achieve \cr
                                          \tab a specified confidence level for a \cr
                                          \tab nonparametric prediction interval. \cr  
  \code{\link{plotPredIntNparDesign}}     \tab Create plots for a sampling design based on \cr
                                          \tab the confidence level and sample size of a \cr
                                          \tab nonparametric prediction interval. \cr                      
  \code{\link{predIntNparSimultaneousConfLevel}} \tab Compute the confidence level associated with \cr
                                          \tab a simultaneous nonparametric prediction \cr
                                          \tab interval. \cr
  \code{\link{predIntNparSimultaneousN}}  \tab Compute the required sample size for a \cr
                                          \tab simultaneous nonparametric prediction \cr
                                          \tab interval. \cr
  \code{\link{plotPredIntNparSimultaneousDesign}} \tab Create plots for a sampling design based on \cr
                                          \tab a simultaneous nonparametric prediction \cr
                                          \tab interval. \cr
  \code{\link{predIntNparSimultaneousTestPower}} \tab Compute the probability that at least one \cr
                                          \tab set of future observations violates the \cr
                                          \tab given rule based on a nonparametric \cr
                                          \tab simultaneous prediction interval. \cr
  \code{\link{plotPredIntNparSimultaneousTestPowerCurve}} \tab Create plots for a sampling design based on \cr
                                          \tab a simultaneous nonparametric prediction \cr
                                          \tab interval. \cr
  }


  \bold{Tolerance Intervals}

  \tabular{ll}{
  \emph{Function Name}                    \tab \emph{Description} \cr
  \code{\link{tolIntNormHalfWidth}}       \tab Compute the half-width of a tolerance \cr
                                          \tab interval for a normal distribution. \cr
  \code{\link{tolIntNormK}}               \tab Compute the required value of \eqn{K} for a \cr
                                          \tab tolerance interval for a Normal distribution. \cr                        
  \code{\link{tolIntNormN}}               \tab Compute the sample size necessary to achieve a \cr
                                          \tab specified half-width for a tolerance interval \cr
                                          \tab for a Normal distribution. \cr
  \code{\link{plotTolIntNormDesign}}      \tab Create plots for a sampling design based on a \cr
                                          \tab tolerance interval for a Normal distribution. \cr
                                          \tab \cr
  \code{\link{tolIntNparConfLevel}}       \tab Compute the confidence level associated with a \cr
                                          \tab nonparametric tolerance interval for a specified \cr 
                                          \tab sample size and coverage. \cr
  \code{\link{tolIntNparCoverage}}        \tab Compute the coverage associated with a \cr
                                          \tab nonparametric tolerance interval for a specified \cr 
                                          \tab sample size and confidence level. \cr
  \code{\link{tolIntNparN}}               \tab Compute the sample size required for a nonparametric \cr
                                          \tab tolerance interval with a specified coverage and \cr
                                          \tab confidence level. \cr
  \code{\link{plotTolIntNparDesign}}      \tab Create plots for a sampling design based on a \cr
                                          \tab nonparametric tolerance interval. \cr

  }
}
\keyword{ package }
