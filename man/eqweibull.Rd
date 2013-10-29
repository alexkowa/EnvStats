\name{eqweibull}
\alias{eqweibull}
\title{
  Estimate Quantiles of a Weibull Distribution
}
\description{
  Estimate quantiles of a \link[stats:Weibull]{Weibull distribution}.
}
\usage{
  eqweibull(x, p = 0.5, method = "mle", digits = 0)
}
\arguments{
  \item{x}{
  a numeric vector of observations, or an object resulting from a call to an 
  estimating function that assumes a Weibull distribution 
  (e.g., \code{\link{eweibull}}).  If \code{x} is a numeric vector, 
  missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) 
  values are allowed but will be removed.
}
  \item{p}{
  numeric vector of probabilities for which quantiles will be estimated.  
  All values of \code{p} must be between 0 and 1.  The default value is \code{p=0.5}.
}
  \item{method}{
  character string specifying the method of estimating the distribution parameters.  
  Possible values are 
  \code{"mle"} (maximum likelihood; the default), \code{"mme"} (methods of moments), 
  and \code{"mmue"} (method of moments based on the unbiased estimator of variance).  
  See the DETAILS section of the help file for \code{\link{eweibull}} for more 
  information.
}
  \item{digits}{
  an integer indicating the number of decimal places to round to when printing out 
  the value of \code{100*p}. The default value is \code{digits=0}.
}
}
\details{
  The function \code{eqweibull} returns estimated quantiles as well as 
  estimates of the shape and scale parameters.  

  Quantiles are estimated by 1) estimating the shape and scale parameters by 
  calling \code{\link{eweibull}}, and then 2) calling the function 
  \code{\link[stats:Weibull]{qweibull}} and using the estimated values for 
  shape and scale.
}
\value{
  If \code{x} is a numeric vector, \code{eqweibull} returns a 
  list of class \code{"estimate"} containing the estimated quantile(s) and other 
  information. See \code{\link{estimate.object}} for details.

  If \code{x} is the result of calling an estimation function, \code{eqweibull} 
  returns a list whose class is the same as \code{x}.  The list 
  contains the same components as \code{x}, as well as components called 
  \code{quantiles} and \code{quantile.method}.
}
\references{
  Forbes, C., M. Evans, N. Hastings, and B. Peacock. (2011).  Statistical Distributions. 
  Fourth Edition. John Wiley and Sons, Hoboken, NJ.

  Johnson, N. L., S. Kotz, and N. Balakrishnan. (1994). 
  \emph{Continuous Univariate Distributions, Volume 1}. 
  Second Edition. John Wiley and Sons, New York.
}
\author{
    Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  The \link[stats:Weibull]{Weibull distribution} is named after the Swedish physicist 
  Waloddi Weibull, who used this distribution to model breaking strengths of 
  materials.  The Weibull distribution has been extensively applied in the fields 
  of reliability and quality control.

  The \link[stats:Exponential]{exponential distribution} is a special case of the 
  Weibull distribution: a Weibull random variable with parameters \code{shape=}\eqn{1} 
  and \code{scale=}\eqn{\beta} is equivalent to an exponential random variable with 
  parameter \code{rate=}\eqn{1/\beta}.

  The Weibull distribution is related to the 
  \link[=EVD]{Type I extreme value (Gumbel) distribution} as follows: 
  if \eqn{X} is a random variable from a Weibull distribution with parameters 
  \code{shape=}\eqn{\alpha} and \code{scale=}\eqn{\beta}, then 
  \deqn{Y = -log(X) \;\;\;\; (10)} 
  is a random variable from an extreme value distribution with parameters 
  \code{location=}\eqn{-log(\beta)} and \code{scale=}\eqn{1/\alpha}.
}
\seealso{
  \code{\link{eweibull}}, \link[stats]{Weibull}, \link[stats]{Exponential}, 
  \link{EVD}, \code{\link{estimate.object}}.
}
\examples{
  # Generate 20 observations from a Weibull distribution with parameters 
  # shape=2 and scale=3, then estimate the parameters via maximum likelihood,
  # and estimate the 90'th percentile. 
  # (Note: the call to set.seed simply allows you to reproduce this example.)

  set.seed(250) 
  dat <- rweibull(20, shape = 2, scale = 3) 
  eqweibull(dat, p = 0.9) 

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            Weibull
  #
  #Estimated Parameter(s):          shape = 2.673098
  #                                 scale = 3.047762
  #
  #Estimation Method:               mle
  #
  #Estimated Quantile(s):           90'th %ile = 4.163755
  #
  #Quantile Estimation Method:      Quantile(s) Based on
  #                                 mle Estimators
  #
  #Data:                            dat
  #
  #Sample Size:                     20

  #----------

  # Clean up
  #---------
  rm(dat)
}
\keyword{ distribution }
\keyword{ htest }
