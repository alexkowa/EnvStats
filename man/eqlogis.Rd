\name{eqlogis}
\alias{eqlogis}
\title{
  Estimate Quantiles of a Logistic Distribution
}
\description{
  Estimate quantiles of a \link[stats:Logistic]{logistic distribution}.
}
\usage{
  eqlogis(x, p = 0.5, method = "mle", digits = 0)
}
\arguments{
  \item{x}{
  a numeric vector of observations, or an object resulting from a call to an 
  estimating function that assumes a logistic distribution 
  (e.g., \code{\link{elogis}}). If \code{x} is a numeric vector, 
  missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) 
  values are allowed but will be removed.
}
  \item{p}{
  numeric vector of probabilities for which quantiles will be estimated.  
  All values of \code{p} must be between 0 and 1.  The default value is \code{p=0.5}.
}
  \item{method}{
  character string specifying the method to use to estimate the distribution parameters.  
  Possible values are 
  \code{"mle"} (maximum likelihood; the default), \code{"mme"} (methods of moments), 
  and \code{"mmue"} (method of moments based on the unbiased estimator of variance).  
  See the DETAILS section of the help file for \code{\link{elogis}} for more 
  information. 
}
  \item{digits}{
  an integer indicating the number of decimal places to round to when printing out 
  the value of \code{100*p}. The default value is \code{digits=0}.
}
}
\details{
  The function \code{eqlogis} returns estimated quantiles as well as 
  estimates of the location and scale parameters.  

  Quantiles are estimated by 1) estimating the location and scale parameters by 
  calling \code{\link{elogis}}, and then 2) calling the function 
  \code{\link[stats:Logistic]{qlogis}} and using the estimated values for 
  location and scale.
}
\value{
  If \code{x} is a numeric vector, \code{eqlogis} returns a 
  list of class \code{"estimate"} containing the estimated quantile(s) and other 
  information. See \code{\link{estimate.object}} for details.

  If \code{x} is the result of calling an estimation function, \code{eqlogis} 
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
  The \link[stats:Logistic]{logistic distribution} is defined on the real line 
  and is unimodal and symmetric about its location parameter (the mean).  It has 
  longer tails than a normal (Gaussian) distribution.  It is used to model growth 
  curves and bioassay data.
}
\seealso{
  \code{\link{elogis}}, \link[stats]{Logistic}, \code{\link{estimate.object}}.
}
\examples{
  # Generate 20 observations from a logistic distribution with 
  # parameters location=0 and scale=1, then estimate the parameters 
  # and estimate the 90th percentile. 
  # (Note: the call to set.seed simply allows you to reproduce this example.)

  set.seed(250) 
  dat <- rlogis(20) 
  eqlogis(dat, p = 0.9) 

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            Logistic
  #
  #Estimated Parameter(s):          location = -0.2181845
  #                                 scale    =  0.8152793
  #
  #Estimation Method:               mle
  #
  #Estimated Quantile(s):           90'th %ile = 1.573167
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
