\name{eqgeom}
\alias{eqgeom}
\title{
  Estimate Quantiles of a Geometric Distribution
}
\description{
  Estimate quantiles of a \link[stats:Geometric]{geometric distribution}.
}
\usage{
  eqgeom(x, p = 0.5, method = "mle/mme", digits = 0)
}
\arguments{
  \item{x}{
  a numeric vector of observations, or an object resulting from a call to an 
  estimating function that assumes a geometric distribution 
  (e.g., \code{\link{egeom}}). If \code{x} is a numeric vector, 
  missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) 
  values are allowed but will be removed.
}
  \item{p}{
  numeric vector of probabilities for which quantiles will be estimated.  
  All values of \code{p} must be between 0 and 1.  The default value is \code{p=0.5}.
}
  \item{method}{
  character string specifying the method to use to estimate the probability parameter.  
  Possible values are \code{"mle/mme"} (maximum likelihood and method of moments; 
  the default) and \code{"mvue"} (minimum variance unbiased).  You cannot use 
  \code{method="mvue"} if \code{length(x)=1}.  See the DETAILS section of the help file 
  for \code{\link{egeom}} for more information on these estimation methods.
}
  \item{digits}{
  an integer indicating the number of decimal places to round to when printing out 
  the value of \code{100*p}. The default value is \code{digits=0}.
}
}
\details{
  The function \code{eqgeom} returns estimated quantiles as well as 
  the estimate of the rate parameter.  

  Quantiles are estimated by 1) estimating the probability parameter by 
  calling \code{\link{egeom}}, and then 2) calling the function 
  \code{\link[stats:Geometric]{qgeom}} and using the estimated value for 
  the probability parameter.
}
\value{
  If \code{x} is a numeric vector, \code{eqgeom} returns a 
  list of class \code{"estimate"} containing the estimated quantile(s) and other 
  information. See \code{\link{estimate.object}} for details.

  If \code{x} is the result of calling an estimation function, \code{eqgeom} 
  returns a list whose class is the same as \code{x}.  The list 
  contains the same components as \code{x}, as well as components called 
  \code{quantiles} and \code{quantile.method}.
}
\references{
  Forbes, C., M. Evans, N. Hastings, and B. Peacock. (2011).  Statistical Distributions. 
  Fourth Edition. John Wiley and Sons, Hoboken, NJ.

  Johnson, N. L., S. Kotz, and A. Kemp. (1992).  
  \emph{Univariate Discrete Distributions}.  Second Edition. John Wiley and Sons, 
  New York, Chapter 5.
}
\author{
    Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  The \link[stats:Geometric]{geometric distribution} with parameter 
  \code{prob=}\eqn{p} is a special case of the 
  \link[stats:NegBinomial]{negative binomial distribution} with parameters 
  \code{size=1} and \code{prob=p}.

  The negative binomial distribution has its roots in a gambling game where 
  participants would bet on the number of tosses of a coin necessary to achieve 
  a fixed number of heads.  The negative binomial distribution has been applied 
  in a wide variety of fields, including accident statistics, birth-and-death 
  processes, and modeling spatial distributions of biological organisms.
}
\seealso{
  \code{\link{egeom}}, \link[stats]{Geometric}, \code{\link{enbinom}}, 
  \link[stats]{NegBinomial}, \code{\link{estimate.object}}.
}
\examples{
  # Generate an observation from a geometric distribution with parameter 
  # prob=0.2, then estimate the parameter prob and the 90'th percentile. 
  # (Note: the call to set.seed simply allows you to reproduce this example.)

  set.seed(250) 
  dat <- rgeom(1, prob = 0.2) 
  dat 
  #[1] 4 

  eqgeom(dat, p = 0.9)

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            Geometric
  #
  #Estimated Parameter(s):          prob = 0.2
  #
  #Estimation Method:               mle/mme
  #
  #Estimated Quantile(s):           90'th %ile = 10
  #
  #Quantile Estimation Method:      Quantile(s) Based on
  #                                 mle/mme Estimators
  #
  #Data:                            dat
  #
  #Sample Size:                     1

  #----------

  # Clean up
  #---------
  rm(dat)
}
\keyword{ distribution }
\keyword{ htest }
