\name{ebeta}
\alias{ebeta}
\title{
   Estimate Parameters of a Beta Distribution
}
\description{
  Estimate the shape parameters of a \link[stats:Beta]{beta distribution}.
}
\usage{
  ebeta(x, method = "mle")
}
\arguments{
  \item{x}{
  numeric vector of observations.  All observations must be between greater than 
  0 and less than 1. 
}
  \item{method}{
  character string specifying the method of estimation.  The possible values are 
  \code{"mle"} (maximum likelihood; the default), \code{"mme"} (method of moments), 
  and \code{"mmue"} (method of moments based on the unbiased estimator of variance).  
  See the DETAILS section for more information on these estimation methods. 
}
}
\details{
  If \code{x} contains any missing (\code{NA}), undefined (\code{NaN}) or 
  infinite (\code{Inf}, \code{-Inf}) values, they will be removed prior to 
  performing the estimation.

  Let \eqn{\underline{x} = (x_1, x_2, \ldots, x_n)} be a vector of \eqn{n} observations 
  from a \link[stats:Beta]{beta distribution} with parameters 
  \code{shape1=}\eqn{\nu} and \code{shape2=}\eqn{\omega}.

  \emph{Maximum Likelihood Estimation} (\code{method="mle"}) \cr
  The maximum likelihood estimators (mle's) of the shape parameters \eqn{\nu} and 
  \eqn{\omega} are the solutions of the simultaneous equations:
  \deqn{\Psi(\hat{\nu}) - \Psi(\hat{\nu} + \hat{\omega}) = (1/n) \sum_{i=1}^{n} log(x_i)}
  \deqn{\Psi(\hat{\nu}) - \Psi(\hat{\nu} + \hat{\omega}) = (1/n) \sum_{i=1}^{n} log(1 - x_i)}
  where \eqn{\Psi()} is the \link[base:Special]{digamma function} (Forbes et al., 2011).  

  \emph{Method of Moments Estimators} (\code{method="mme"})\cr
  The method of moments estimators (mme's) of the shape parameters \eqn{\nu} and 
  \eqn{\omega} are given by (Forbes et al., 2011):
  \deqn{\hat{\nu} = \bar{x} \{ [ \bar{x}(1 - \bar{x}) / s_{m}^2] - 1 \}}
  \deqn{\hat{\omega} = (1 - \bar{x}) \{ [ \bar{x}(1 - \bar{x}) / s_{m}^2] - 1 \}}
  where
  \deqn{\bar{x} = \frac{1}{n} \sum_{i=1}^n x_i; \, s_{m}^2 = \frac{1}{n} \sum_{i=1}^n (x_i - \bar{x})^2}

  \emph{Method of Moments Estimators Based on the Unbiased Estimator of Variance} (\code{method="mmue"}) \cr
  These estimators are the same as the method of moments estimators except that 
  the method of moments estimator of variance is replaced with the unbiased estimator 
  of variance:
  \deqn{s^2 = \frac{1}{n-1} \sum_{i=1}^n (x_i - \bar{x})^2}
}
\value{
  a list of class \code{"estimate"} containing the estimated parameters and other information.  
  See \cr
  \code{\link{estimate.object}} for details.
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
  \link[stats:Beta]{Beta}.
}
\examples{
  # Generate 20 observations from a beta distribution with parameters 
  # shape1=2 and shape2=4, then estimate the parameters via 
  # maximum likelihood. 
  # (Note: the call to set.seed simply allows you to reproduce this example.)

  set.seed(250) 
  dat <- rbeta(20, shape1 = 2, shape2 = 4) 
  ebeta(dat) 

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
  #Data:                            dat
  #
  #Sample Size:                     20

  #==========

  # Repeat the above, but use the method of moments estimators:

  ebeta(dat, method = "mme")

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            Beta
  #
  #Estimated Parameter(s):          shape1 =  5.216311
  #                                 shape2 = 11.461341
  #
  #Estimation Method:               mme
  #
  #Data:                            dat
  #
  #Sample Size:                     20

  #==========

  # Clean up
  #---------
  rm(dat)
}
\keyword{ distribution }
\keyword{ htest }
