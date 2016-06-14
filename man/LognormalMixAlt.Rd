\name{LognormalMixAlt}
\alias{LognormalMixAlt}
\alias{dlnormMixAlt}
\alias{plnormMixAlt}
\alias{qlnormMixAlt}
\alias{rlnormMixAlt}
\title{
  Mixture of Two Lognormal Distributions (Alternative Parameterization)
}
\description{
  Density, distribution function, quantile function, and random generation 
  for a mixture of two lognormal distribution with parameters 
  \code{mean1}, \code{cv1}, \code{mean2}, \code{cv2}, and \code{p.mix}.
}
\usage{
  dlnormMixAlt(x, mean1 = exp(1/2), cv1 = sqrt(exp(1) - 1), 
      mean2 = exp(1/2), cv2 = sqrt(exp(1) - 1), p.mix = 0.5)
  plnormMixAlt(q, mean1 = exp(1/2), cv1 = sqrt(exp(1) - 1), 
      mean2 = exp(1/2), cv2 = sqrt(exp(1) - 1), p.mix = 0.5) 
  qlnormMixAlt(p, mean1 = exp(1/2), cv1 = sqrt(exp(1) - 1), 
      mean2 = exp(1/2), cv2 = sqrt(exp(1) - 1), p.mix = 0.5) 
  rlnormMixAlt(n, mean1 = exp(1/2), cv1 = sqrt(exp(1) - 1), 
      mean2 = exp(1/2), cv2 = sqrt(exp(1) - 1), p.mix = 0.5)
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
  vector of means of the first lognormal random variable.  The default is \cr
  \code{meanlog1=sqrt(exp(1) - 1)}.  
}
  \item{cv1}{
  vector of coefficient of variations of the first lognormal random variable.  
  The default is \code{sdlog1=sqrt(exp(1) - 1)}.  
}
  \item{mean2}{
  vector of means of the second lognormal random variable.  The default is \cr
  \code{mean2=sqrt(exp(1) - 1)}.
}
  \item{cv2}{
  vector of coefficient of variations of the second lognormal random variable.  
  The default is \code{sdlog2=sqrt(exp(1) - 1)}.
}
  \item{p.mix}{
  vector of probabilities between 0 and 1 indicating the mixing proportion.  
  For \code{rlnormMixAlt} this must be a single, non-missing number.
}
}
\details{
  Let \eqn{f(x; \eta, \theta)} denote the density of a 
  \link[=LognormalAlt]{lognormal random variable} with parameters 
  \code{mean=}\eqn{\eta} and \code{cv=}\eqn{\theta}.  The density, \eqn{g}, of a 
  lognormal mixture random variable with parameters \code{mean1=}\eqn{\eta_1}, 
  \code{cv1=}\eqn{\theta_1}, \code{mean2=}\eqn{\eta_2}, 
  \code{cv2=}\eqn{\theta_2}, and \code{p.mix=}\eqn{p} is given by:
  \deqn{g(x; \eta_1, \theta_1, \eta_2, \theta_2, p) = 
    (1 - p) f(x; \eta_1, \theta_1) + p f(x; \eta_2, \theta_2)}

  The default values for \code{mean1} and \code{cv1} correspond to a 
  \link[stats:Lognormal]{lognormal distribution} with parameters 
  \code{meanlog=0} and \code{sdlog=1}.  Similarly for the default values 
  of \code{mean2} and \code{cv2}.
}
\value{
  \code{dlnormMixAlt} gives the density, \code{plnormMixAlt} gives the distribution 
  function, \code{qlnormMixAlt} gives the quantile function, and 
  \code{rlnormMixAlt} generates random deviates. 
}
\references{
  Gilliom, R.J., and D.R. Helsel. (1986).  Estimation of Distributional Parameters 
  for Censored Trace Level Water Quality Data: 1. Estimation Techniques.  
  \emph{Water Resources Research} \bold{22}, 135-146.

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
  A lognormal mixture distribution is often used to model positive-valued data 
  that appear to be \dQuote{contaminated}; that is, most of the values appear to 
  come from a single lognormal distribution, but a few \dQuote{outliers} are 
  apparent.  In this case, the value of \code{mean2} would be larger than the 
  value of \code{mean1}, and the mixing proportion \code{p.mix} would be fairly 
  close to 0 (e.g., \code{p.mix=0.1}).
}
\seealso{
  \link{LognormalAlt}, \link{LognormalMix}, \link[stats:Lognormal]{Lognormal},  
  \link{NormalMix},  \link{Probability Distributions and Random Numbers}.
}
\examples{
  # Density of a lognormal mixture with parameters mean=2, cv1=3, 
  # mean2=4, cv2=5, p.mix=0.5, evaluated at 1.5: 

  dlnormMixAlt(1.5, mean1 = 2, cv1 = 3, mean2 = 4, cv2 = 5, p.mix = 0.5) 
  #[1] 0.1436045

  #----------

  # The cdf of a lognormal mixture with parameters mean=2, cv1=3, 
  # mean2=4, cv2=5, p.mix=0.5, evaluated at 1.5: 

  plnormMixAlt(1.5, mean1 = 2, cv1 = 3, mean2 = 4, cv2 = 5, p.mix = 0.5) 
  #[1] 0.6778064

  #----------

  # The median of a lognormal mixture with parameters mean=2, cv1=3, 
  # mean2=4, cv2=5, p.mix=0.5: 

  qlnormMixAlt(0.5, 2, 3, 4, 5, 0.5) 
  #[1] 0.6978355

  #----------

  # Random sample of 3 observations from a lognormal mixture with 
  # parameters mean1=2, cv1=3, mean2=4, cv2=5, p.mix=0.5. 
  # (Note: the call to set.seed simply allows you to reproduce this example.)

  set.seed(20) 
  rlnormMixAlt(3, 2, 3, 4, 5, 0.5) 
  #[1]  0.70672151 14.43226313  0.05521329
}
\keyword{ distribution }
\keyword{ datagen }
