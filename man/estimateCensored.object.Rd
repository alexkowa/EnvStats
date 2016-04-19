\name{estimateCensored.object}
\alias{estimateCensored.object}
\alias{estimateCensored}
\docType{class}
\title{
  S3 Class "estimateCensored"
}
\description{
  Objects of S3 class \code{"estimateCensored"} are returned by any of the 
  \pkg{EnvStats} functions that estimate the parameters or quantiles of a 
  probability distribution and optionally construct confidence, 
  prediction, or tolerance intervals based on a sample of \bold{\emph{censored}} 
  data assumed to come from that distribution.
}
\details{
  Objects of S3 class \code{"estimateCensored"} are lists that contain 
  information about the estimated distribution parameters, 
  quantiles, and (if present) intervals, as well as the censoring side, 
  censoring levels and percentage of censored observations.  
  The names of the \pkg{EnvStats} 
  functions that produce objects of class \code{"estimateCensored"} 
  have the following forms:

  \tabular{ll}{
  \bold{Form of Function Name} \tab \bold{Result} \cr
  \code{e}\emph{abb}\code{Censored} \tab Parameter Estimation \cr
  \code{eq}\emph{abb}\code{Censored} \tab Quantile Estimation \cr
  \code{predInt}\emph{Abb}\code{Censored} \tab Prediction Interval \cr
  \code{tolInt}\emph{Abb}\code{Censored} \tab Tolerance Interval
  }

  where \emph{abb} denotes the abbreviation of the name of a 
  probability distribution (see the help file for 
  \code{\link{Distribution.df}} for a list of available probability 
  distributions and their abbreviations), and \emph{Abb} denotes the 
  same thing as \emph{abb} except the first letter of the abbreviation 
  for the probability distribution is capitalized.  

  See the sections \bold{Estimating Distribution Parameters},  
  \bold{Estimating Distribution Quantiles}, and 
  \bold{Prediction and Tolerance Intervals} in the help file 
  \link[=FcnsByCatCensoredData]{EnvStats Functions for Censored Data} 
  for a list of functions that estimate distribution parameters, estimate  
  distribution quantiles, create prediction intervals, or create tolerance intervals 
  using censored data.  

  For example: 
  \itemize{
  \item The function \code{\link{enormCensored}} returns an object of class 
    \code{"estimateCensored"} (a list) with information about the estimated 
    mean and standard deviation of the assumed normal (Gaussian) 
    distribution, information about the amount and side of censoring, and also an 
    optional confidence interval for the mean.
  \item The function \code{\link{eqnormCensored}} returns a list of class 
    \code{"estimateCensored"} with information about the estimated mean and 
    standard deviation of the assumed normal distribution, information about the 
    amount and side of censoring, the 
    estimated user-specified quantile(s), and an optional confidence 
    interval for a single quantile.
  \item The function \code{\link{tolIntNormCensored}} returns a list of class 
    \code{"estimateCensored"} with information about the estimated mean and 
    standard deviation of the assumed normal distribution, information about the amount 
    and side of censoring, and the computed tolerance interval.
  }
}
\section{Methods}{
  Generic functions that have methods for objects of class 
  \code{"estimateCensored"} include: \cr
  \code{\link{print}}.
}
\value{
  \strong{Required Components} \cr
  The following components must be included in a legitimate list of 
  class \code{"estimateCensored"}.

  \item{distribution}{character string indicating the name of the 
    assumed distribution (this equals \code{"Nonparametric"}) for 
    nonparametric procedures).}
  \item{sample.size}{numeric scalar indicating the sample size used 
      to estimate the parameters or quantiles.}
  \item{censoring.side}{character string indicating whether the data are 
    left- or right-censored.}
  \item{censoring.levels}{numeric scalar or vector indicating the censoring level(s).}
  \item{percent.censored}{numeric scalar indicating the percent of non-missing 
    observations that are censored.}
  \item{data.name}{character string indicating the name of the data 
    object used to compute the estimateCensored parameters or quantiles.}
  \item{censoring.name}{character string indicating the name of the data object 
    used to identify which values are censored.}
  \item{bad.obs}{numeric scalar indicating the number of missing (\code{NA}), 
    undefined (\code{NaN}) and/or infinite (\code{Inf}, \code{-Inf}) 
    values that were removed from the data object prior to performing 
    the estimation.} \cr

  \strong{Optional Components} \cr
  The following components may optionally be included in a legitimate 
  list of class \code{"estimateCensored"}.

  \item{parameters}{(parametric estimation only) a numeric vector 
    with a names attribute containing the names and values of the 
    estimateCensored distribution parameters.}
  \item{n.param.est}{(parametric estimation only) a scalar indicating 
    the number of distribution parameters estimateCensored.}
  \item{method}{(parametric estimation only) a character string 
    indicating the method used to compute the estimateCensored parameters.}
  \item{quantiles}{a numeric vector of estimateCensored quantiles.}
  \item{quantile.method}{a character string indicating the method of 
    quantile estimation.}
  \item{interval}{a list of class \code{"intervalEstimate"} containing 
    information on a confidence, tolerance, or prediction interval.} \cr
 
  All lists of class \code{"intervalEstimateCensored"} contain the following 
  component:

  \item{name}{a character string inidicating the kind of interval.  
    Possible values are: \cr
    \code{"Confidence"}, \code{"Tolerance"}, or \code{"Prediction"}.} \cr
 
  The number and names of the other components in a list of class 
  \code{"intervalEstimate"} depends on the kind of interval it is.  
  These components may include:

  \item{parameter}{a character string indicating the parameter for 
    which the interval is constructed (e.g., \code{"mean"}, 
    \code{"95'th \%ile"}, etc.).}
  \item{limits}{a numeric vector containing the lower and upper 
    bounds of the interval.}
  \item{type}{the type of interval (i.e., \code{"two-sided"}, 
    \code{"lower"}, or \code{"upper"}).}
  \item{method}{the method used to construct the interval 
    (e.g., \code{"normal.approx"}).}
  \item{conf.level}{the confidence level associated with the interval.}
  \item{sample.size}{the sample size associated with the interval.}
  \item{dof}{(parametric intervals only) the degrees of freedom 
    associated with the interval.}
  \item{limit.ranks}{(nonparametric intervals only) the rank(s) of 
    the order statistic(s) used to construct the interval.}
  \item{m}{(prediction intervals only) the total number of future 
    observations (\code{n.mean=1}, \code{n.median=1}, or 
    \code{n.sum=1}) or averages (\code{n.mean>1}), medians \cr
    (\code{n.median>1}), or sums (\code{n.sum>1}).}
  \item{k}{(prediction intervals only) the minimum number of future 
    observations \cr
    (\code{n.mean=1}, \code{n.median=1}, or \code{n.sum=1}),  
    or averages (\code{n.mean>1}), medians \cr
    (\code{n.median>1}) or sums (\code{n.sum>1}) out of the total \code{m} 
    that the interval should contain.}
  \item{n.mean}{(prediction intervals only) the sample size associated 
    with the future averages that should be contained in the interval.}
  \item{n.median}{(prediction intervals only) the sample size associated 
    with the future medians that should be contained in the interval.}
  \item{n.sum}{(Poisson prediction intervals only) the sample size 
    associated with the future sums that should be contained in the 
    interval.}
  \item{rule}{(simultaneous prediction intervals only) the rule used to 
    construct the simultaneous prediction interval.}
  \item{delta.over.sigma}{(simultaneous prediction intervals only) numeric 
    scalar indicating the ratio \eqn{\Delta / \sigma}. The quantity 
    \eqn{\Delta} (delta) denotes the difference between the mean of 
    the population that was sampled to construct the prediction interval, 
    and the mean of the population that will be sampled to produce the 
    future observations.  The quantity \eqn{\sigma} (sigma) denotes the 
    population standard deviation for both populations.}
}
\note{
  Since objects of class \code{"estimateCensored"} are lists, you may extract 
  their components with the \code{$} and \code{[[} operators.
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\seealso{
  \link[=FcnsByCatCensoredData]{EnvStats Functions for Censored Data}, 
  \code{\link{Distribution.df}}, \code{\link{estimate.object}}.
}
\examples{
  # Create an object of class "estimateCensored", then print it out. 
  # (Note: the call to set.seed simply allows you to reproduce 
  # this example.)

  set.seed(250) 

  dat <- rnorm(20, mean = 100, sd = 20)
  censored <- dat < 90
  dat[censored] <- 90

  estimateCensored.obj <- enormCensored(dat, censored, ci = TRUE) 

  mode(estimateCensored.obj) 
  #[1] "list" 

  class(estimateCensored.obj) 
  #[1] "estimateCensored" 

  names(estimateCensored.obj) 
  # [1] "distribution"     "sample.size"      "censoring.side"   "censoring.levels"
  # [5] "percent.censored" "parameters"       "n.param.est"      "method"          
  # [9] "data.name"        "censoring.name"   "bad.obs"          "interval"        
  #[13] "var.cov.params" 

  names(estimateCensored.obj$interval) 
  #[1] "name"       "parameter"  "limits"     "type"       "method"     "conf.level"
 

  estimateCensored.obj 
  
  #Results of Distribution Parameter Estimation
  #Based on Type I Censored Data
  #--------------------------------------------
  #
  #Assumed Distribution:            Normal
  #
  #Censoring Side:                  left
  #
  #Censoring Level(s):              90 
  #
  #Estimated Parameter(s):          mean = 96.52796
  #                                 sd   = 14.62275
  #
  #Estimation Method:               MLE
  #
  #Data:                            dat
  #
  #Censoring Variable:              censored
  #
  #Sample Size:                     20
  #
  #Percent Censored:                25%
  #
  #Confidence Interval for:         mean
  #
  #Confidence Interval Method:      Profile Likelihood
  #
  #Confidence Interval Type:        two-sided
  #
  #Confidence Level:                95%
  #
  #Confidence Interval:             LCL =  88.82415
  #                                 UCL = 103.27604


  #----------

  # Extract the confidence limits for the mean

  estimateCensored.obj$interval$limits
  #     LCL      UCL 
  # 91.7801 103.7839 

  #----------

  # Clean up

  rm(dat, censored, estimateCensored.obj)
}
\keyword{classes}

