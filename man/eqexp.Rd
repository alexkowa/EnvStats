\name{eqexp}
\alias{eqexp}
\title{
  Estimate Quantiles of an Exponential Distribution
}
\description{
  Estimate quantiles of an \link[stats:Exponential]{exponential distribution}.
}
\usage{
  eqexp(x, p = 0.5, method = "mle/mme", digits = 0)
}
\arguments{
  \item{x}{
  a numeric vector of observations, or an object resulting from a call to an 
  estimating function that assumes an exponential distribution 
  (e.g., \code{\link{eexp}}).  If \code{x} is a numeric vector, 
  missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) 
  values are allowed but will be removed.
}
  \item{p}{
  numeric vector of probabilities for which quantiles will be estimated.  
  All values of \code{p} must be between 0 and 1.  The default value is \code{p=0.5}.
}
  \item{method}{
  character string specifying the method to use to estimate the rate parameter.  
  Currently the only possible value is \code{"mle/mme"} 
  (maximum likelihood/method of moments; the default).  See the DETAILS section of 
  the help file for \code{\link{eexp}} for more information. 
}
  \item{digits}{
  an integer indicating the number of decimal places to round to when printing out 
  the value of \code{100*p}. The default value is \code{digits=0}.
}
}
\details{
  The function \code{eqexp} returns estimated quantiles as well as 
  the estimate of the rate parameter.  

  Quantiles are estimated by 1) estimating the rate parameter by 
  calling \code{\link{eexp}}, and then 2) calling the function 
  \code{\link[stats:Exponential]{qexp}} and using the estimated value for 
  rate.
}
\value{
  If \code{x} is a numeric vector, \code{eqexp} returns a 
  list of class \code{"estimate"} containing the estimated quantile(s) and other 
  information. See \code{\link{estimate.object}} for details.

  If \code{x} is the result of calling an estimation function, \code{eqexp} 
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
  The \link[stats:Exponential]{exponential distribution} is a special case of the 
  \link[stats:GammaDist]{gamma distribution}, and 
  takes on positive real values.  A major use of the exponential distribution is 
  in life testing where it is used to model the lifetime of a product, part, 
  person, etc.

  The exponential distribution is the only continuous distribution with a 
  \dQuote{lack of memory} property.  That is, if the lifetime of a part follows 
  the exponential distribution, then the distribution of the time until failure 
  is the same as the distribution of the time until failure given that the part 
  has survived to time \eqn{t}.

  The exponential distribution is related to the double exponential (also called 
  Laplace) distribution, and to the \link[=EVD]{extreme value distribution}.
}
\seealso{
  \code{\link{eexp}}, \link[stats:Exponential]{Exponential}, 
  \code{\link{estimate.object}}.
}
\examples{
  # Generate 20 observations from an exponential distribution with parameter 
  # rate=2, then estimate the parameter and estimate the 90th percentile. 
  # (Note: the call to set.seed simply allows you to reproduce this example.)

  set.seed(250) 
  dat <- rexp(20, rate = 2) 
  eqexp(dat, p = 0.9) 

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            Exponential
  #
  #Estimated Parameter(s):          rate = 2.260587
  #
  #Estimation Method:               mle/mme
  #
  #Estimated Quantile(s):           90'th %ile = 1.018578
  #
  #Quantile Estimation Method:      Quantile(s) Based on
  #                                 mle/mme Estimators
  #
  #Data:                            dat
  #
  #Sample Size:                     20
  #

  #----------

  # Clean up
  #---------
  rm(dat)
}
\keyword{ distribution }
\keyword{ htest }
