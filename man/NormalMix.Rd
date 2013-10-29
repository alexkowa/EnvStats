\name{NormalMix}
\alias{NormalMix}
\alias{dnormMix}
\alias{pnormMix}
\alias{qnormMix}
\alias{rnormMix}
\title{
  Mixture of Two Normal Distributions
}
\description{
  Density, distribution function, quantile function, and random generation 
  for a mixture of two normal distribution with parameters 
  \code{mean1}, \code{sd1}, \code{mean2}, \code{sd2}, and \code{p.mix}.
}
\usage{
  dnormMix(x, mean1 = 0, sd1 = 1, mean2 = 0, sd2 = 1, p.mix = 0.5)
  pnormMix(q, mean1 = 0, sd1 = 1, mean2 = 0, sd2 = 1, p.mix = 0.5)
  qnormMix(p, mean1 = 0, sd1 = 1, mean2 = 0, sd2 = 1, p.mix = 0.5)
  rnormMix(n, mean1 = 0, sd1 = 1, mean2 = 0, sd2 = 1, p.mix = 0.5)
}
\arguments{
  \item{x}{
  vector of quantiles.
}
  \item{q}{
  vector of quantiles.
}
  \item{p}{
  vector of probabilities between 0 and 1.
}
  \item{n}{
  sample size.  If \code{length(n)} is larger than 1, then \code{length(n)} 
  random values are returned.
}
  \item{mean1}{
  vector of means of the first normal random variable.  
  The default is \code{mean1=0}.  
}
  \item{sd1}{
  vector of standard deviations of the first normal random variable.  
  The default is \code{sd1=1}.  
}
  \item{mean2}{
  vector of means of the second normal random variable.  
  The default is \code{mean2=0}.
}
  \item{sd2}{
  vector of standard deviations of the second normal random variable.  
  The default is \code{sd2=1}.
}
  \item{p.mix}{
  vector of probabilities between 0 and 1 indicating the mixing proportion.  
  For \code{rnormMix} this must be a single, non-missing number.
}
}
\details{
  Let \eqn{f(x; \mu, \sigma)} denote the density of a 
  \link[stats:Normal]{normal random variable} with parameters 
  \code{mean=}\eqn{\mu} and \code{sd=}\eqn{\sigma}.  The density, \eqn{g}, of a 
  normal mixture random variable with parameters \code{mean1=}\eqn{\mu_1}, 
  \code{sd1=}\eqn{\sigma_1}, \code{mean2=}\eqn{\mu_2}, 
  \code{sd2=}\eqn{\sigma_2}, and \code{p.mix=}\eqn{p} is given by:
  \deqn{g(x; \mu_1, \sigma_1, \mu_2, \sigma_2, p) = 
    (1 - p) f(x; \mu_1, \sigma_1) + p f(x; \mu_2, \sigma_2)}
}
\value{
  \code{dnormMix} gives the density, \code{pnormMix} gives the distribution function, 
  \code{qnormMix} gives the quantile function, and \code{rnormMix} generates random 
  deviates. 
}
\references{
  Johnson, N. L., S. Kotz, and A.W. Kemp. (1992).  \emph{Univariate Discrete 
  Distributions}. Second Edition. John Wiley and Sons, New York, pp.53-54, and 
  Chapter 8.

  Johnson, N. L., S. Kotz, and N. Balakrishnan. (1994). 
  \emph{Continuous Univariate Distributions, Volume 1}. 
  Second Edition. John Wiley and Sons, New York.
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  A normal mixture distribution is sometimes used to model data 
  that appear to be \dQuote{contaminated}; that is, most of the values appear to 
  come from a single normal distribution, but a few \dQuote{outliers} are 
  apparent.  In this case, the value of \code{mean2} would be larger than the 
  value of \code{mean1}, and the mixing proportion \code{p.mix} would be fairly 
  close to 0 (e.g., \code{p.mix=0.1}).  The value of the second standard deviation 
  (\code{sd2}) may or may not be the same as the value for the first 
  (\code{sd1}).

  Another application of the normal mixture distribution is to bi-modal data; 
  that is, data exhibiting two modes.
}
\seealso{
  \link[stats:Normal]{Normal},  \link{LognormalMix},  
  \link{Probability Distributions and Random Numbers}.
}
\examples{
  # Density of a normal mixture with parameters mean1=0, sd1=1, 
  #  mean2=4, sd2=2, p.mix=0.5, evaluated at 1.5: 

  dnormMix(1.5, mean2=4, sd2=2) 
  #[1] 0.1104211

  #----------

  # The cdf of a normal mixture with parameters mean1=10, sd1=2, 
  # mean2=20, sd2=2, p.mix=0.1, evaluated at 15: 

  pnormMix(15, 10, 2, 20, 2, 0.1) 
  #[1] 0.8950323

  #----------

  # The median of a normal mixture with parameters mean1=10, sd1=2, 
  # mean2=20, sd2=2, p.mix=0.1: 

  qnormMix(0.5, 10, 2, 20, 2, 0.1) 
  #[1] 10.27942

  #----------

  # Random sample of 3 observations from a normal mixture with 
  # parameters mean1=0, sd1=1, mean2=4, sd2=2, p.mix=0.5. 
  # (Note: the call to set.seed simply allows you to reproduce this example.)

  set.seed(20) 
  rnormMix(3, mean2=4, sd2=2)
  #[1] 0.07316778 2.06112801 1.05953620
}
\keyword{ distribution }
\keyword{ datagen }
