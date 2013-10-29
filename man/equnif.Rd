\name{equnif}
\alias{equnif}
\title{
  Estimate Quantiles of a Uniform Distribution
}
\description{
  Estimate quantiles of a \link[stats:Uniform]{uniform distribution}.
}
\usage{
  equnif(x, p = 0.5, method = "mle", digits = 0)
}
\arguments{
  \item{x}{
  a numeric vector of observations, or an object resulting from a call to an 
  estimating function that assumes a uniform distribution 
  (e.g., \code{\link{eunif}}). If \code{x} is a numeric vector, 
  missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) 
  values are allowed but will be removed.
}
  \item{p}{
  numeric vector of probabilities for which quantiles will be estimated.  
  All values of \code{p} must be between 0 and 1.  The default value is \code{p=0.5}.
}
  \item{method}{
  character string specifying the method of estimating the distribution parameters.  
  The possible values are 
  \code{"mle"} (maximum likelihood; the default), \code{"mme"} (method of moments), 
  and \code{"mmue"} (method of moments based on the unbiased estimator of variance).  
  See the DETAILS section of the help file for \code{\link{eunif}} for more 
  information on these estimation methods.
}
  \item{digits}{
  an integer indicating the number of decimal places to round to when printing out 
  the value of \code{100*p}. The default value is \code{digits=0}.
}
}
\details{
  The function \code{equnif} returns estimated quantiles as well as 
  estimates of the location and scale parameters.  

  Quantiles are estimated by 1) estimating the location and scale parameters by 
  calling \code{\link{eunif}}, and then 2) calling the function 
  \code{\link[stats:Uniform]{qunif}} and using the estimated values for 
  location and scale.
}
\value{
  If \code{x} is a numeric vector, \code{equnif} returns a 
  list of class \code{"estimate"} containing the estimated quantile(s) and other 
  information. See \code{\link{estimate.object}} for details.

  If \code{x} is the result of calling an estimation function, \code{equnif} 
  returns a list whose class is the same as \code{x}.  The list 
  contains the same components as \code{x}, as well as components called 
  \code{quantiles} and \code{quantile.method}.
}
\references{
  Forbes, C., M. Evans, N. Hastings, and B. Peacock. (2011).  Statistical Distributions. 
  Fourth Edition. John Wiley and Sons, Hoboken, NJ.

  Johnson, N. L., S. Kotz, and N. Balakrishnan. (1995). 
  \emph{Continuous Univariate Distributions, Volume 2}. 
  Second Edition. John Wiley and Sons, New York.
}
\author{
    Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  The \link[stats:Uniform]{uniform distribution} (also called the rectangular 
  distribution) with parameters \code{min} and \code{max} takes on values on the 
  real line between \code{min} and \code{max} with equal probability.  It has been 
  used to represent the distribution of round-off errors in tabulated values.  Another 
  important application is that the distribution of the cumulative distribution 
  function (cdf) of any kind of continuous random variable follows a uniform 
  distribution with parameters \code{min=0} and \code{max=1}.
}
\seealso{
  \code{\link{eunif}}, \link[stats]{Uniform}, \code{\link{estimate.object}}.
}
\examples{
  # Generate 20 observations from a uniform distribution with parameters 
  # min=-2 and max=3, then estimate the parameters via maximum likelihood
  # and estimate the 90th percentile. 
  # (Note: the call to set.seed simply allows you to reproduce this example.)

  set.seed(250) 
  dat <- runif(20, min = -2, max = 3) 
  equnif(dat, p = 0.9) 

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            Uniform
  #
  #Estimated Parameter(s):          min = -1.574529
  #                                 max =  2.837006
  #
  #Estimation Method:               mle
  #
  #Estimated Quantile(s):           90'th %ile = 2.395852
  #
  #Quantile Estimation Method:      Quantile(s) Based on
  #                                 mle Estimators
  #
  #Data:                            dat
  #
  #Sample Size:                     20

  #----------
  # Clean up

  rm(dat)
}
\keyword{ distribution }
\keyword{ htest }
