\name{GEVD}
\alias{GEVD}
\alias{Generalized Extreme Value Distribution}
\alias{dgevd}
\alias{pgevd}
\alias{qgevd}
\alias{rgevd}
\title{
  The Generalized Extreme Value Distribution
}
\description{
  Density, distribution function, quantile function, and random generation 
  for the generalized extreme value distribution.
}
\usage{
  dgevd(x, location = 0, scale = 1, shape = 0)
  pgevd(q, location = 0, scale = 1, shape = 0)
  qgevd(p, location = 0, scale = 1, shape = 0)
  rgevd(n, location = 0, scale = 1, shape = 0)
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
  \item{location}{
  vector of location parameters.
}
  \item{scale}{
  vector of positive scale parameters.
}
  \item{shape}{
  vector of shape parameters.
}
}
\details{
  Let \eqn{X} be a generalized extreme value random variable with parameters 
  \code{location=}\eqn{\eta}, \code{scale=}\eqn{\theta}, and \code{shape=}\eqn{\kappa}.  
  When the shape parameter \eqn{\kappa = 0}, the generalized extreme value distribution 
  reduces to the \link[=EVD]{extreme value distribution}.  When the shape parameter 
  \eqn{\kappa \ne 0}, the cumulative distribution function of \eqn{X} is given by:
  \deqn{F(x; \eta, \theta, \kappa) = exp\{-[1 - \kappa(x-\eta)/\theta]^{1/\kappa}\}}
  where \eqn{-\infty < \eta, \kappa < \infty} and \eqn{\theta > 0}.  
  When \eqn{\kappa > 0}, the range of \eqn{x} is:
  \deqn{-\infty < x \le \eta + \theta/\kappa}
  and when \eqn{\kappa < 0} the range of \eqn{x} is:
  \deqn{\eta + \theta/\kappa \le x < \infty}

  The \eqn{p^th} quantile of \eqn{X} is given by:
  \deqn{x_{p} = \eta + \frac{\theta \{1 - [-log(p)]^{\kappa}\}}{\kappa}}
}
\value{
  density (\code{devd}), probability (\code{pevd}), quantile (\code{qevd}), or 
  random sample (\code{revd}) for the generalized extreme value distribution with 
  location parameter(s) determined by \code{location}, scale parameter(s) 
  determined by \code{scale}, and shape parameter(s) determined by \code{shape}.
}
\references{
  Forbes, C., M. Evans, N. Hastings, and B. Peacock. (2011).  Statistical Distributions. 
  Fourth Edition. John Wiley and Sons, Hoboken, NJ.

  Jenkinson, A.F. (1955).  The Frequency Distribution of the Annual Maximum 
  (or Minimum) of Meteorological Events.  \emph{Quarterly Journal of the Royal 
  Meteorological Society}, \bold{81}, 158--171.

  Johnson, N. L., S. Kotz, and N. Balakrishnan. (1995). 
  \emph{Continuous Univariate Distributions, Volume 2}. 
  Second Edition. John Wiley and Sons, New York.
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  Two-parameter \link[=EVD]{extreme value distributions (EVD)} have been applied 
  extensively since the 1930's to several fields of study, including the distributions 
  of hydrological and meteorological variables, human lifetimes, and strength of 
  materials.  The three-parameter generalized extreme value distribution (GEVD) was 
  introduced by Jenkinson (1955) to model annual maximum and minimum values of 
  meteorological events.  Since then, it has been used extensively in the hydological 
  and meteorological fields.

  The three families of EVDs are all special kinds of GEVDs.  When the shape 
  parameter \eqn{\kappa = 0}, the GEVD reduces to the 
  \link[=EVD]{Type I extreme value (Gumbel) distribution}.  (The function 
  \code{\link{zTestGevdShape}} allows you to test the null hypothesis that the shape 
  parameter is equal to 0.)  When \eqn{\kappa > 0}, the GEVD is the same as the Type II 
  extreme value distribution, and when \eqn{\kappa < 0} it is the same as the 
  Type III extreme value distribution.
}
\seealso{
  \code{\link{egevd}}, \code{\link{zTestGevdShape}}, \code{\link{EVD}}, 
  \link{Probability Distributions and Random Numbers}.
}
\examples{
  # Density of a generalized extreme value distribution with 
  # location=0, scale=1, and shape=0, evaluated at 0.5: 

  dgevd(.5) 
  #[1] 0.3307043

  #----------

  # The cdf of a generalized extreme value distribution with 
  # location=1, scale=2, and shape=0.25, evaluated at 0.5: 

  pgevd(.5, 1, 2, 0.25) 
  #[1] 0.2795905

  #----------

  # The 90'th percentile of a generalized extreme value distribution with 
  # location=-2, scale=0.5, and shape=-0.25: 

  qgevd(.9, -2, 0.5, -0.25) 
  #[1] -0.4895683

  #----------

  # Random sample of 4 observations from a generalized extreme value 
  # distribution with location=5, scale=2, and shape=1. 
  # (Note: the call to set.seed simply allows you to reproduce this example.)

  set.seed(20) 
  rgevd(4, 5, 2, 1) 
  #[1] 6.738692 6.473457 4.446649 5.727085
}
\keyword{ distribution }
\keyword{ datagen }
