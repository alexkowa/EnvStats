\name{eqnbinom}
\alias{eqnbinom}
\title{
  Estimate Quantiles of a Negative Binomial Distribution
}
\description{
  Estimate quantiles of a \link[stats:NegBinomial]{negative binomial distribution}.
}
\usage{
  eqnbinom(x, size = NULL, p = 0.5, method = "mle/mme", digits = 0)
}
\arguments{
  \item{x}{
  vector of non-negative integers indicating the number of trials that took place 
  \emph{before} \code{size} \dQuote{successes} occurred (the total number of 
  trials that took place is \code{x+1}), or an object resulting 
  from a call to an estimating function that assumes a negative binomial distribution 
  (e.g., \code{\link{enbinom}}).  If \code{x} is a vector of non-negative integers, then 
  missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) 
  values are allowed but will be removed.  If \code{length(x)=n} and \code{n} is 
  greater than 1, it is assumed that \code{x} represents observations from \code{n} 
  separate negative binomial experiments that all had the same probability of 
  success (\code{prob}), but possibly different values of \code{size}.
}
  \item{size}{
  vector of positive integers indicating the number of \dQuote{successes} that 
  must be observed before the trials are stopped.  Missing (\code{NA}), 
  undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) values are allowed 
  but will be removed.  The length of \code{size} must be 1 or else the same 
  length as \code{x}.
}
  \item{p}{
  numeric vector of probabilities for which quantiles will be estimated.  
  All values of \code{p} must be between 0 and 1.  The default value is \code{p=0.5}.
}
  \item{method}{
  character string specifying the method of estimating the probability parameter.  
  Possible values are 
  \code{"mle/mme"} (maximum likelihood and method of moments; the default) and 
  \code{"mvue"} (minimum variance unbiased).  You cannot use \code{method="mvue"} if 
  the sum of the elements in \code{size} is 1.  See the DETAILS section of the help file 
  for \code{\link{enbinom}} for more information on these estimation methods.
}
  \item{digits}{
  an integer indicating the number of decimal places to round to when printing out 
  the value of \code{100*p}. The default value is \code{digits=0}.
}
}
\details{
  The function \code{eqnbinom} returns estimated quantiles as well as 
  estimates of the \code{prob} parameter.  

  Quantiles are estimated by 1) estimating the prob parameter by 
  calling \code{\link{enbinom}}, and then 2) calling the function 
  \code{\link[stats:NegBinomial]{qnbinom}} and using the estimated value for 
  \code{prob}.
}
\value{
  If \code{x} is a numeric vector, \code{eqnbinom} returns a 
  list of class \code{"estimate"} containing the estimated quantile(s) and other 
  information. See \code{\link{estimate.object}} for details.

  If \code{x} is the result of calling an estimation function, \code{eqnbinom} 
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
  The \link[stats:NegBinomial]{negative binomial distribution} has its roots in 
  a gambling game where participants would bet on the number of tosses of a 
  coin necessary to achieve a fixed number of heads.  The negative binomial 
  distribution has been applied in a wide variety of fields, including accident 
  statistics, birth-and-death processes, and modeling spatial distributions of 
  biological organisms.

  The \link[stats:Geometric]{geometric distribution} with parameter \code{prob=}\eqn{p} 
  is a special case of the negative binomial distribution with parameters 
  \code{size=1} and \code{prob=}\eqn{p}.
}
\seealso{
  \code{\link{enbinom}}, \link[stats]{NegBinomial}, \code{\link{egeom}}, 
  \link[stats]{Geometric}, \code{\link{estimate.object}}.
}
\examples{
  # Generate an observation from a negative binomial distribution with 
  # parameters size=2 and prob=0.2, then estimate the parameter prob 
  # and the 90th percentile. 
  # Note: the call to set.seed simply allows you to reproduce this example. 
  # Also, the only parameter that is estimated is prob; the parameter 
  # size is supplied in the call to enbinom.  The parameter size is printed in 
  # order to show all of the parameters associated with the distribution.

  set.seed(250) 
  dat <- rnbinom(1, size = 2, prob = 0.2) 
  dat
  #[1] 5

  eqnbinom(dat, size = 2, p = 0.9)

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            Negative Binomial
  #
  #Estimated Parameter(s):          size = 2.0000000
  #                                 prob = 0.2857143
  #
  #Estimation Method:               mle/mme for 'prob'
  #
  #Estimated Quantile(s):           90'th %ile = 11
  #
  #Quantile Estimation Method:      Quantile(s) Based on
  #                                 mle/mme for 'prob' Estimators
  #
  #Data:                            dat, 2
  #
  #Sample Size:                     1


  #----------
  # Clean up

  rm(dat)
}
\keyword{ distribution }
\keyword{ htest }
