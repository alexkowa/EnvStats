\name{estimate.object}
\alias{estimate.object}
\alias{estimate}
\docType{class}
\title{
  S3 Class "estimate"
}
\description{
  Objects of S3 class \code{"estimate"} are returned by any of the 
  \pkg{EnvStats} functions that estimate the parameters or quantiles of a 
  probability distribution and optionally construct confidence, 
  prediction, or tolerance intervals based on a sample of data 
  assumed to come from that distribution.
}
\details{
  Objects of S3 class \code{"estimate"} are lists that contain 
  information about the estimated distribution parameters, 
  quantiles, and intervals.  The names of the \pkg{EnvStats} 
  functions that produce objects of class \code{"estimate"} 
  have the following forms:

  \tabular{ll}{
  \bold{Form of Function Name} \tab \bold{Result} \cr
  \code{e}\emph{abb} \tab Parameter Estimation \cr
  \code{eq}\emph{abb} \tab Quantile Estimation \cr
  \code{predInt}\emph{Abb} \tab Prediction Interval \cr
  \code{tolInt}\emph{Abb} \tab Tolerance Interval \cr
  }

  where \emph{abb} denotes the abbreviation of the name of a 
  probability distribution (see the help file for 
  \code{\link{Distribution.df}} for a list of available probability 
  distributions and their abbreviations), and \emph{Abb} denotes the 
  same thing as \emph{abb} except the first letter of the abbreviation 
  for the probability distribution is capitalized.  

  See the help files 
  \link{Estimating Distribution Parameters} and 
  \link{Estimating Distribution Quantiles} 
  for lists of functions that estimate distribution parameters 
  and quantiles.  See the help files \link{Prediction Intervals} 
  and \link{Tolerance Intervals} for lists of functions 
  that create prediction and tolerance intervals.

  For example: 
  \itemize{
  \item The function \code{\link{enorm}} returns an object of class 
    \code{"estimate"} (a list) with information about the estimated 
    mean and standard deviation of the assumed normal (Gaussian) 
    distribution, as well as an optional confidence interval for 
    the mean.
  \item The function \code{\link{eqnorm}} returns a list of class 
    \code{"estimate"} with information about the estimated mean and 
    standard deviation of the assumed normal distribution, the 
    estimated user-specified quantile(s), and an optional confidence 
    interval for a single quantile.
  \item The function \code{\link{predIntNorm}} returns a list of class 
    \code{"estimate"} with information about the estimated mean and 
    standard deviation of the assumed normal distribution, along with a 
    prediction interval for a user-specified number of future 
    observations (or means, medians, or sums).
  \item The function \code{\link{tolIntNorm}} returns a list of class 
    \code{"estimate"} with information about the estimated mean and 
    standard deviation of the assumed normal distribution, along with a 
    tolerance interval.
  }
}
\section{Methods}{
  Generic functions that have methods for objects of class 
  \code{"estimate"} include: \cr
  \code{\link{print}}.
}
\value{
  \strong{Required Components} \cr
  The following components must be included in a legitimate list of 
  class \code{"estimate"}.

  \item{distribution}{character string indicating the name of the 
    assumed distribution (this equals \code{"Nonparametric"}) for 
    nonparametric procedures).}
  \item{sample.size}{numeric scalar indicating the sample size used 
    to estimate the parameters or quantiles.}
  \item{data.name}{character string indicating the name of the data 
    object used to compute the estimated parameters or quantiles.}
  \item{bad.obs}{numeric scalar indicating the number of missing (\code{NA}), 
    undefined (\code{NaN}) and/or infinite (\code{Inf}, \code{-Inf}) 
    values that were removed from the data object prior to performing 
    the estimation.} \cr

  \strong{Optional Components} \cr
  The following components may optionally be included in a legitimate 
  list of class \code{"estimate"}.

  \item{parameters}{(parametric estimation only) a numeric vector 
    with a names attribute containing the names and values of the 
    estimated distribution parameters.}
  \item{n.param.est}{(parametric estimation only) a scalar indicating 
    the number of distribution parameters estimated.}
  \item{method}{(parametric estimation only) a character string 
    indicating the method used to compute the estimated parameters.}
  \item{quantiles}{a numeric vector of estimated quantiles.}
  \item{quantile.method}{a character string indicating the method of 
    quantile estimation.}
  \item{interval}{a list of class \code{"intervalEstimate"} containing 
    information on a confidence, tolerance, or prediction interval.} \cr
 
  All lists of class \code{"intervalEstimate"} contain the following 
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
  Since objects of class \code{"estimate"} are lists, you may extract 
  their components with the \code{$} and \code{[[} operators.
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\seealso{
  \link{Estimating Distribution Parameters}, \link{Estimating Distribution Quantiles}, 
  \code{\link{Distribution.df}}, \link{Prediction Intervals}, 
  \link{Tolerance Intervals}, \code{\link{estimateCensored.object}}.
}
\examples{
  # Create an object of class "estimate", then print it out. 
  # (Note: the call to set.seed simply allows you to reproduce 
  # this example.)

  set.seed(250) 

  dat <- rnorm(20, mean = 3, sd = 2) 

  estimate.obj <- enorm(dat, ci = TRUE) 

  mode(estimate.obj) 
  #[1] "list" 

  class(estimate.obj) 
  #[1] "estimate" 

  names(estimate.obj) 
  #[1] "distribution" "sample.size"  "parameters" 
  #[4] "n.param.est"  "method"       "data.name" 
  #[7] "bad.obs"      "interval" 

  names(estimate.obj$interval) 
  #[1] "name"        "parameter"   "limits" 
  #[4] "type"        "method"      "conf.level" 
  #[7] "sample.size" "dof" 

  estimate.obj 
  
  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            Normal
  #
  #Estimated Parameter(s):          mean = 2.861160
  #                                 sd   = 1.180226
  #
  #Estimation Method:               mvue
  #
  #Data:                            dat
  #
  #Sample Size:                     20
  #
  #Confidence Interval for:         mean
  #
  #Confidence Interval Method:      Exact
  #
  #Confidence Interval Type:        two-sided
  #
  #Confidence Level:                95%
  #
  #Confidence Interval:             LCL = 2.308798
  #                                 UCL = 3.413523

  #----------

  # Extract the confidence limits for the mean

  estimate.obj$interval$limits
  #     LCL      UCL 
  #2.308798 3.413523 

  #----------

  # Clean up

  rm(dat, estimate.obj)
}
\keyword{classes}

