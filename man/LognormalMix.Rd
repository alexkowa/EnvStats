\name{LognormalMix}
\alias{LognormalMix}
\alias{dlnormMix}
\alias{plnormMix}
\alias{qlnormMix}
\alias{rlnormMix}
\title{
  Mixture of Two Lognormal Distributions
}
\description{
  Density, distribution function, quantile function, and random generation 
  for a mixture of two lognormal distribution with parameters 
  \code{meanlog1}, \code{sdlog1}, \code{meanlog2}, \code{sdlog2}, and \code{p.mix}.
}
\usage{
  dlnormMix(x, meanlog1 = 0, sdlog1 = 1, meanlog2 = 0, sdlog2 = 1, p.mix = 0.5)
  plnormMix(q, meanlog1 = 0, sdlog1 = 1, meanlog2 = 0, sdlog2 = 1, p.mix = 0.5) 
  qlnormMix(p, meanlog1 = 0, sdlog1 = 1, meanlog2 = 0, sdlog2 = 1, p.mix = 0.5) 
  rlnormMix(n, meanlog1 = 0, sdlog1 = 1, meanlog2 = 0, sdlog2 = 1, p.mix = 0.5)
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
  \item{meanlog1}{
  vector of means of the first lognormal random variable on the log scale.  
  The default is \code{meanlog1=0}.  
}
  \item{sdlog1}{
  vector of standard deviations of the first lognormal random variable on 
  the log scale.  The default is \code{sdlog1=1}.  
}
  \item{meanlog2}{
  vector of means of the second lognormal random variable on the log scale.  
  The default is \code{meanlog2=0}.
}
  \item{sdlog2}{
  vector of standard deviations of the second lognormal random variable on 
  the log scale.  The default is \code{sdlog2=1}.
}
  \item{p.mix}{
  vector of probabilities between 0 and 1 indicating the mixing proportion.  
  For \code{rlnormMix} this must be a single, non-missing number.
}
}
\details{
  Let \eqn{f(x; \mu, \sigma)} denote the density of a 
  \link[stats:Lognormal]{lognormal random variable} with parameters 
  \code{meanlog=}\eqn{\mu} and \code{sdlog=}\eqn{\sigma}.  The density, \eqn{g}, of a 
  lognormal mixture random variable with parameters \code{meanlog1=}\eqn{\mu_1}, 
  \code{sdlog1=}\eqn{\sigma_1}, \code{meanlog2=}\eqn{\mu_2}, 
  \code{sdlog2=}\eqn{\sigma_2}, and \code{p.mix=}\eqn{p} is given by:
  \deqn{g(x; \mu_1, \sigma_1, \mu_2, \sigma_2, p) = 
    (1 - p) f(x; \mu_1, \sigma_1) + p f(x; \mu_2, \sigma_2)}
}
\value{
  \code{dlnormMix} gives the density, \code{plnormMix} gives the distribution function, 
  \code{qlnormMix} gives the quantile function, and \code{rlnormMix} generates random 
  deviates. 
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
  apparent.  In this case, the value of \code{meanlog2} would be larger than the 
  value of \code{meanlog1}, and the mixing proportion \code{p.mix} would be fairly 
  close to 0 (e.g., \code{p.mix=0.1}).  The value of the second standard deviation 
  (\code{sdlog2}) may or may not be the same as the value for the first 
  (\code{sdlog1}).
}
\seealso{
  \link[stats:Lognormal]{Lognormal},  \link{NormalMix},  
  \link{Probability Distributions and Random Numbers}.
}
\examples{
  # Density of a lognormal mixture with parameters meanlog1=0, sdlog1=1, 
  # meanlog2=2, sdlog2=3, p.mix=0.5, evaluated at 1.5: 

  dlnormMix(1.5, meanlog1 = 0, sdlog1 = 1, meanlog2 = 2, sdlog2 = 3, p.mix = 0.5) 
  #[1] 0.1609746

  #----------

  # The cdf of a lognormal mixture with parameters meanlog1=0, sdlog1=1, 
  # meanlog2=2, sdlog2=3, p.mix=0.2, evaluated at 4: 

  plnormMix(4, 0, 1, 2, 3, 0.2) 
  #[1] 0.8175281

  #----------

  # The median of a lognormal mixture with parameters meanlog1=0, sdlog1=1, 
  # meanlog2=2, sdlog2=3, p.mix=0.2: 

  qlnormMix(0.5, 0, 1, 2, 3, 0.2) 
  #[1] 1.156891

  #----------

  # Random sample of 3 observations from a lognormal mixture with 
  # parameters meanlog1=0, sdlog1=1, meanlog2=3, sdlog2=4, p.mix=0.2. 
  # (Note: the call to set.seed simply allows you to reproduce this example.)

  set.seed(20) 
  rlnormMix(3, 0, 1, 2, 3, 0.2) 
  #[1] 0.08975283 1.07591103 7.85482514
}
\keyword{ distribution }
\keyword{ datagen }
