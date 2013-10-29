\name{eqbinom}
\alias{eqbinom}
\title{
  Estimate Quantiles of a Binomial Distribution
}
\description{
  Estimate quantiles of a \link[stats:Binomial]{binomial distribution}.
}
\usage{
 eqbinom(x, size = NULL, p = 0.5, method = "mle/mme/mvue", digits = 0)
}
\arguments{
  \item{x}{
  numeric or logical vector of observations, or an object resulting from a call to an 
  estimating function that assumes a binomial distribution 
  (e.g., \code{\link{ebinom}}).  If \code{x} is a vector of observations, then when 
  \code{size} is not supplied, \code{x} must be 
  a numeric vector of 0s (\dQuote{failures}) and 1s (\dQuote{successes}), or else a logical vector 
  of \code{FALSE} values (\dQuote{failures}) and \code{TRUE} values (\dQuote{successes}).  When 
  \code{size} is supplied, \code{x} must be a non-negative integer containing the number of 
  \dQuote{successes} out of the number of trials indicated by \code{size}.  
  Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) 
  values are allowed but will be removed.
}
  \item{size}{
  positive integer indicating the of number of trials; \code{size} must be at least as 
  large as the value of \code{x}.
}
  \item{p}{
  numeric vector of probabilities for which quantiles will be estimated.  
  All values of \code{p} must be between 0 and 1.  The default value is \code{p=0.5}.
}
  \item{method}{
  character string specifying the method of estimation.  The only possible value is 
  \code{"mle/mme/mvue"} (maximum likelihood, method of moments, and minimum variance unbiased).  
  See the DETAILS section of the help file for \code{\link{ebinom}} for more information.
}
  \item{digits}{
  an integer indicating the number of decimal places to round to when printing out 
  the value of \code{100*p}. The default value is \code{digits=0}.
}
}
\details{
  The function \code{eqbinom} returns estimated quantiles as well as 
  estimates of the \code{prob} parameter.  

  Quantiles are estimated by 1) estimating the prob parameter by 
  calling \code{\link{ebinom}}, and then 2) calling the function 
  \code{\link[stats:Binomial]{qbinom}} and using the estimated value for 
  \code{prob}.
}
\value{
  If \code{x} is a numeric vector, \code{eqbinom} returns a 
  list of class \code{"estimate"} containing the estimated quantile(s) and other 
  information. See \code{\link{estimate.object}} for details.

  If \code{x} is the result of calling an estimation function, \code{eqbinom} 
  returns a list whose class is the same as \code{x}.  The list 
  contains the same components as \code{x}, as well as components called 
  \code{quantiles} and \code{quantile.method}.
}
\references{
  Agresti, A., and B.A. Coull. (1998). Approximate is Better than "Exact" for Interval Estimation 
  of Binomial Proportions. \emph{The American Statistician}, \bold{52}(2), 119--126.

  Agresti, A., and B. Caffo. (2000). Simple and Effective Confidence Intervals for Proportions 
  and Differences of Proportions Result from Adding Two Successes and Two Failures. \emph{The 
  American Statistician}, \bold{54}(4), 280--288.

  Berthouex, P.M., and L.C. Brown. (1994). \emph{Statistics for Environmental Engineers}. 
  Lewis Publishers, Boca Raton, FL, Chapters 2 and 15.

  Cochran, W.G. (1977). \emph{Sampling Techniques}. John Wiley and Sons, New York, Chapter 3.

  Fisher, R.A., and F. Yates. (1963). 
  \emph{Statistical Tables for Biological, Agricultural, and Medical Research}. 6th edition. 
  Hafner, New York, 146pp.

  Fleiss, J. L. (1981). \emph{Statistical Methods for Rates and Proportions}. Second Edition. 
  John Wiley and Sons, New York, Chapters 1-2.

  Forbes, C., M. Evans, N. Hastings, and B. Peacock. (2011).  Statistical Distributions. 
  Fourth Edition. John Wiley and Sons, Hoboken, NJ.

  Gilbert, R.O. (1987). \emph{Statistical Methods for Environmental Pollution Monitoring}. 
  Van Nostrand Reinhold, New York, NY, Chapter 11.

  Johnson, N. L., S. Kotz, and A.W. Kemp. (1992).  \emph{Univariate 
  Discrete Distributions}.  Second Edition.  John Wiley and Sons, New York, 
  Chapter 3.

  Millard, S.P., and Neerchal, N.K. (2001). \emph{Environmental Statistics with S-PLUS}. 
  CRC Press, Boca Raton, Florida.

  Newcombe, R.G. (1998a). Two-Sided Confidence Intervals for the Single Proportion:  Comparison of 
  Seven Methods. \emph{Statistics in Medicine}, \bold{17}, 857--872.

  Ott, W.R. (1995). \emph{Environmental Statistics and Data Analysis}. 
  Lewis Publishers, Boca Raton, FL, Chapter 4.

  USEPA. (1989b). \emph{Statistical Analysis of Ground-Water Monitoring Data at RCRA Facilities, Interim Final Guidance}. 
  EPA/530-SW-89-026. Office of Solid Waste, U.S. Environmental Protection Agency, Washington, D.C.

  USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
  EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.  
  U.S. Environmental Protection Agency, Washington, D.C. p.6-38.

  Zar, J.H. (2010). \emph{Biostatistical Analysis}. Fifth Edition. 
  Prentice-Hall, Upper Saddle River, NJ, Chapter 24. 
}
\author{
    Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  The binomial distribution is used to model processes with binary (Yes-No, Success-Failure, 
  Heads-Tails, etc.) outcomes.  It is assumed that the outcome of any one trial is independent 
  of any other trial, and that the probability of \dQuote{success}, \eqn{p}, is the same on 
  each trial.  A binomial discrete random variable \eqn{X} is the number of \dQuote{successes} in 
  \eqn{n} independent trials.  A special case of the binomial distribution occurs when \eqn{n=1}, 
  in which case \eqn{X} is also called a Bernoulli random variable.

  In the context of environmental statistics, the binomial distribution is sometimes used to model 
  the proportion of times a chemical concentration exceeds a set standard in a given period of 
  time (e.g., Gilbert, 1987, p.143).  The binomial distribution is also used to compute an upper 
  bound on the overall Type I error rate for deciding whether a facility or location is in 
  compliance with some set standard.  Assume the null hypothesis is that the facility is in compliance.  
  If a test of hypothesis is conducted periodically over time to test compliance and/or several tests 
  are performed during each time period, and the facility or location is always in compliance, and 
  each single test has a Type I error rate of \eqn{\alpha}, and the result of each test is 
  independent of the result of any other test (usually not a reasonable assumption), then the number 
  of times the facility is declared out of compliance when in fact it is in compliance is a 
  binomial random variable with probability of \dQuote{success} \eqn{p=\alpha} being the 
  probability of being declared out of compliance (see USEPA, 2009).
}
\seealso{
  \code{\link{ebinom}}, \code{\link[stats:Binomial]{Binomial}}, 
  \code{\link{estimate.object}}.
}
\examples{
  # Generate 20 observations from a binomial distribution with 
  # parameters size=1 and prob=0.2, then estimate the 'prob' 
  # parameter and the 90'th percentile. 
  # (Note: the call to set.seed simply allows you to reproduce this example. 

  set.seed(251) 
  dat <- rbinom(20, size = 1, prob = 0.2) 
  eqbinom(dat, p = 0.9) 

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            Binomial
  #
  #Estimated Parameter(s):          size = 20.0
  #                                 prob =  0.1
  #
  #Estimation Method:               mle/mme/mvue for 'prob'
  #
  #Estimated Quantile(s):           90'th %ile = 4
  #
  #Quantile Estimation Method:      Quantile(s) Based on
  #                                 mle/mme/mvue for 'prob' Estimators
  #
  #Data:                            dat
  #
  #Sample Size:                     20
  #
  #
  #

  #----------
  # Clean up

  rm(dat)
}
\keyword{ distribution }
\keyword{ htest }
