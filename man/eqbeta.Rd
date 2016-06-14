\name{eqbeta}
\alias{eqbeta}
\title{
  Estimate Quantiles of a Beta Distribution
}
\description{
  Estimate quantiles of a \link[stats:Beta]{beta distribution}.
}
\usage{
  eqbeta(x, p = 0.5, method = "mle", digits = 0)
}
\arguments{
  \item{x}{
  a numeric vector of observations, or an object resulting from a call to an 
  estimating function that assumes a beta distribution 
  (e.g., \code{\link{ebeta}}). If \code{x} is a numeric vector, 
  missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) 
  values are allowed but will be removed.
}
  \item{p}{
  numeric vector of probabilities for which quantiles will be estimated.  
  All values of \code{p} must be between 0 and 1.  The default value is \code{p=0.5}.
}
  \item{method}{
  character string specifying the method to use to estimate the shape and scale 
  parameters of the distribution.  The possible values are 
  \code{"mle"} (maximum likelihood; the default),  
  \code{"mme"} (method of moments), and 
  \code{"mmue"} (method of moments based on the unbiased estimator of variance). 
  See the DETAILS section of the help file for \code{\link{ebeta}} for more information.
}
  \item{digits}{
  an integer indicating the number of decimal places to round to when printing out 
  the value of \code{100*p}. The default value is \code{digits=0}.
}
}
\details{
  The function \code{eqbeta} returns estimated quantiles as well as 
  estimates of the shape1 and shape2 parameters.  

  Quantiles are estimated by 1) estimating the shape1 and shape2 parameters by 
  calling \code{\link{ebeta}}, and then 2) calling the function 
  \code{\link[stats:Beta]{qbeta}} and using the estimated values for 
  shape1 and shape2.
}
\value{
  If \code{x} is a numeric vector, \code{eqbeta} returns a 
  list of class \code{"estimate"} containing the estimated quantile(s) and other 
  information. See \code{\link{estimate.object}} for details.

  If \code{x} is the result of calling an estimation function, \code{eqbeta} 
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
  The beta distribution takes real values between 0 and 1.  Special cases of the 
  beta are the \link[stats:Uniform]{Uniform}[0,1] when \code{shape1=1} and 
  \code{shape2=1}, and the arcsin distribution when \code{shape1=0.5} and \cr
  \code{shape2=0.5}.  The arcsin distribution appears in the theory of random walks.  
  The beta distribution is used in Bayesian analyses as a conjugate to the binomial 
  distribution.
}
\seealso{
  \code{\link{ebeta}}, \code{\link[stats:Beta]{Beta}}, \code{\link{estimate.object}}.
}
\examples{
  # Generate 20 observations from a beta distribution with parameters 
  # shape1=2 and shape2=4, then estimate the parameters via 
  # maximum likelihood and estimate the 90'th percentile.
  # (Note: the call to set.seed simply allows you to reproduce this example.)

  set.seed(250) 
  dat <- rbeta(20, shape1 = 2, shape2 = 4) 
  eqbeta(dat, p = 0.9)

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            Beta
  #
  #Estimated Parameter(s):          shape1 =  5.392221
  #                                 shape2 = 11.823233
  #
  #Estimation Method:               mle
  #
  #Estimated Quantile(s):           90'th %ile = 0.4592796
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
