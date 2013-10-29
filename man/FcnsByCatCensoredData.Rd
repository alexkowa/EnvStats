\name{FcnsByCatCensoredData}
\alias{FcnsByCatCensoredData}
\alias{Censored Data}
\alias{EnvStats Functions for Censored Data}
\alias{Functions for Censored Data}
\concept{Less Than Detection Limit}
\title{
  EnvStats Functions for Censored Data
}
\description{
  The \pkg{EnvStats} functions listed below are useful for dealing with Type I censored data.
}
\details{

  \bold{Data Transformations}

  \tabular{ll}{
    \emph{Function Name}                \tab \emph{Description} \cr
    \code{\link{boxcoxCensored}}        \tab Compute values of an objective for Box-Cox Power \cr
                                        \tab transformations, or compute optimal transformation, \cr
                                        \tab for Type I censored data. \cr
                                        \tab \cr
    \code{\link{print.boxcoxCensored}}  \tab Print an object of class \code{\link[=boxcoxCensored.object]{"boxcoxCensored"}}. \cr
                                        \tab \cr
    \code{\link{plot.boxcoxCensored}}   \tab Plot an object of class \code{\link[=boxcoxCensored.object]{"boxcoxCensored"}}. \cr
  }


  \bold{Estimating Distribution Parameters}

  \tabular{ll}{
    \emph{Function Name}                  \tab \emph{Description} \cr
    \code{\link{egammaCensored}}          \tab Estimate shape and scale parameters for a gamma distribution \cr 
                                          \tab based on Type I censored data. \cr
                                          \tab \cr
    \code{\link{egammaAltCensored}}       \tab Estimate mean and CV for a gamma distribution \cr
                                          \tab based on Type I censored data. \cr
                                          \tab \cr
    \code{\link{elnormCensored}}          \tab Estimate parameters for a lognormal distribution (log-scale) \cr 
                                          \tab based on Type I censored data. \cr
                                          \tab \cr
    \code{\link{elnormAltCensored}}       \tab Estimate parameters for a lognormal distribution (original scale) \cr
                                          \tab based on Type I censored data. \cr
                                          \tab \cr
    \code{\link{enormCensored}}           \tab Estimate parameters for a Normal distribution based on Type I \cr
                                          \tab censored data. \cr
                                          \tab \cr
    \code{\link{epoisCensored}}           \tab Estimate parameter for a Poisson distribution based on Type I \cr
                                          \tab censored data. \cr
                                          \tab \cr
    \code{\link{enparCensored}}           \tab Estimate the mean and standard deviation nonparametrically. \cr
                                          \tab \cr
    \code{\link{gpqCiNormSinglyCensored}} \tab Generate the generalized pivotal quantity used to construct a \cr
                                          \tab confidence interval for the mean of a Normal distribution based \cr
                                          \tab on Type I singly censored data. \cr
                                          \tab \cr
    \code{\link{gpqCiNormMultiplyCensored}} \tab Generate the generalized pivotal quantity used to construct a \cr
                                          \tab confidence interval for the mean of a Normal distribution based \cr
                                          \tab on Type I multiply censored data. \cr
                                          \tab \cr
    \code{\link{print.estimateCensored}}  \tab Print an object of class \code{\link[=estimateCensored.object]{"estimateCensored"}}. \cr
  }


  \bold{Estimating Distribution Quantiles}

  \tabular{ll}{
    \emph{Function Name}                  \tab \emph{Description} \cr
    \code{\link{eqlnormCensored}}         \tab Estimate quantiles of a \link{Lognormal} distribution (log-scale) \cr 
                                          \tab based on Type I censored data, and optionally construct \cr
                                          \tab a confidence interval for a quantile. \cr
                                          \tab \cr
    \code{\link{eqnormCensored}}          \tab Estimate quantiles of a \link{Normal} distribution \cr
                                          \tab based on Type I censored data, and optionally construct \cr
                                          \tab a confidence interval for a quantile. \cr
                                          \tab \cr
  }

  All of the functions for computing quantiles (and associated confidence intervals) for complete (uncensored) 
  data are listed in the help file \link{Estimating Distribution Quantiles}.  All of these functions, with 
  the exception of \code{\link{eqnpar}}, will accept an object of class 
  \code{\link[=estimateCensored.object]{"estimateCensored"}}.  Thus, you may estimate 
  quantiles (and construct \emph{approximate} confidence intervals) for any distribution for which:

  \enumerate{
    \item There exists a function to estimate distribution parameters using censored data 
    (see the section \bold{Estimating Distribution Parameters} above).
    \item There exists a function to estimate quantiles for that distribution based on complete data 
    (see the help file \link{Estimating Distribution Quantiles}).
  }

  Nonparametric estimates of quantiles (and associated confidence intervals) can be constructed from censored 
  data as long as the order statistics used in the results are above all left-censored observations or below 
  all right-censored observations.  See the help file for \code{\link{eqnpar}} for more information and 
  examples.  \cr


  \bold{Goodness-of-Fit Tests} 

  \tabular{ll}{
    \emph{Function Name}                  \tab \emph{Description} \cr
    \code{\link{gofTestCensored}}         \tab Perform a goodness-of-fit test based on Type I left- or \cr
                                          \tab right-censored data. \cr
                                          \tab \cr
    \code{\link{print.gofCensored}}       \tab Print an object of class \code{\link[=gofCensored.object]{"gofCensored"}}. \cr
                                          \tab \cr
    \code{\link{plot.gofCensored}}        \tab Plot an object of class \code{\link[=gofCensored.object]{"gofCensored"}}. \cr
  }


  \bold{Hypothesis Tests}

  \tabular{ll}{
    \emph{Function Name}                  \tab \emph{Description} \cr
    \code{\link{twoSampleLinearRankTestCensored}} \tab Perform two-sample linear rank tests based on \cr
                                          \tab censored data. \cr
                                          \tab \cr
    \code{\link{print.htestCensored}}     \tab Printing method for object of class \cr
                                          \tab \code{\link[=htestCensored.object]{"htestCensored"}}. \cr
  }


  \bold{Plotting Probability Distributions} 

  \tabular{ll}{
    \emph{Function Name}                  \tab \emph{Description} \cr
    \code{\link{cdfCompareCensored}}      \tab Plot two cumulative distribution functions based on Type I \cr
                                          \tab censored data. \cr
                                          \tab \cr
    \code{\link{ecdfPlotCensored}}        \tab Plot an empirical cumulative distribution function based on \cr
                                          \tab Type I censored data. \cr
                                          \tab \cr
    \code{\link{ppointsCensored}}         \tab Compute plotting positions for Type I censored data. \cr
                                          \tab \cr
    \code{\link{qqPlotCensored}}          \tab Produce quantile-quantile (Q-Q) plots, also called probability \cr
                                          \tab plots, based on Type I censored data. \cr
  }


  \bold{Prediction and Tolerance Intervals} 

  \tabular{ll}{
    \emph{Function Name}                  \tab \emph{Description} \cr
    \code{\link{gpqTolIntNormSinglyCensored}}  \tab Generate the generalized pivotal quantity used to construct a \cr
                                          \tab tolerance interval for a Normal distribution based \cr
                                          \tab on Type I singly censored data. \cr
                                          \tab \cr
    \code{\link{gpqTolIntNormMultiplyCensored}} \tab Generate the generalized pivotal quantity used to construct a \cr
                                          \tab tolerance interval for a Normal distribution based \cr
                                          \tab on Type I multiply censored data. \cr
                                          \tab \cr
    \code{\link{tolIntLnormCensored}}     \tab Tolerance interval for a lognormal distribution (log-scale) \cr 
                                          \tab based on Type I censored data. \cr
                                          \tab \cr
    \code{\link{tolIntNormCensored}}      \tab Tolerance interval for a Normal distribution based on Type I \cr
                                          \tab censored data. \cr
                                          \tab \cr
  }

  All of the functions for computing prediction and tolerance intervals for complete (uncensored) 
  data are listed in the help files \link{Prediction Intervals} and \link{Tolerance Intervals}.  
  All of these functions, with the exceptions of \code{\link{predIntNpar}} and \code{\link{tolIntNpar}}, 
  will accept an object of class \code{\link[=estimateCensored.object]{"estimateCensored"}}.  Thus, you 
  may construct \emph{approximate} prediction or tolerance intervals for any distribution for which:

  \enumerate{
    \item There exists a function to estimate distribution parameters using censored data 
    (see the section \bold{Estimating Distribution Parameters} above).
    \item There exists a function to create a prediction or tolerance interval for that 
    distribution based on complete data (see the help files \link{Prediction Intervals} and 
    \link{Tolerance Intervals}).
  }

  Nonparametric prediction and tolerance intervals can be constructed from censored 
  data as long as the order statistics used in the results are above all left-censored observations or below 
  all right-censored observations.  See the help files for \code{\link{predIntNpar}}, 
  \code{\link{predIntNparSimultaneous}}, and \code{\link{tolIntNpar}} for more information and examples.

}

\keyword{ package }
