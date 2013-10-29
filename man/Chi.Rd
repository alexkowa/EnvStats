\name{Chi}
\alias{Chi}
\alias{dchi}
\alias{pchi}
\alias{qchi}
\alias{rchi}
\title{
  The Chi Distribution
}
\description{
  Density, distribution function, quantile function, and random generation for the 
  chi distribution.
}
\usage{
  dchi(x, df)
  pchi(q, df)
  qchi(p, df)
  rchi(n, df)
}
\arguments{
  \item{x}{
  vector of (positive) quantiles.
}
  \item{q}{
  vector of (positive) quantiles.
}
  \item{p}{
  vector of probabilities between 0 and 1.
}
  \item{n}{
  sample size.  If \code{length(n)} is larger than 1, then \code{length(n)} 
  random values are returned.
}
  \item{df}{
  vector of (positive) degrees of freedom (> 0).  Non-integer values are allowed.
}
}
\details{
  Elements of \code{x}, \code{q}, \code{p}, or \code{df} that are missing will 
  cause the corresponding elements of the result to be missing.

  The chi distribution with \eqn{n} degrees of freedom is the distribution of the 
  positive square root of a random variable having a 
  \link[stats:Chisquare]{chi-squared} distribution with \eqn{n} degrees of freedom.

  The chi density function is given by:
  \deqn{f(x, \nu) = g(x^2, \nu) 2x,    x > 0}
  where \eqn{g(x,\nu)} denotes the density function of a chi-square random variable 
  with \eqn{n} degrees of freedom.
}
\value{
  density (\code{dchi}), probability (\code{pchi}), quantile (\code{qchi}), or 
  random sample (\code{rchi}) for the chi distribution with \code{df} degrees of freedom.
}
\references{
  Forbes, C., M. Evans, N. Hastings, and B. Peacock. (2011).  Statistical Distributions. 
  Fourth Edition. John Wiley and Sons, Hoboken, NJ.

  Johnson, N. L., S. Kotz, and N. Balakrishnan. (1995). 
  \emph{Continuous Univariate Distributions, Volume 1}. 
  Second Edition. John Wiley and Sons, New York.
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  The chi distribution takes on positive real values.  It is important because 
  for a sample of \eqn{n} observations from a \link[stats:Normal]{normal} distribution, 
  the sample standard deviation multiplied by the square root of the degrees of 
  freedom \eqn{\nu} and divided by the true standard deviation follows a chi 
  distribution with \eqn{\nu} degrees of freedom.  The chi distribution is also 
  used in computing exact prediction intervals for the next \eqn{k} observations 
  from a normal distribution (see \code{\link{predIntNorm}}).
}
\seealso{
  \link{Chisquare}, \link{Normal}, \code{\link{predIntNorm}}, 
  \link{Probability Distributions and Random Numbers}.
}
\examples{
  # Density of a chi distribution with 4 degrees of freedom, evaluated at 3:

  dchi(3, 4) 
  #[1] 0.1499715

  #----------

  # The 95'th percentile of a chi distribution with 10 degrees of freedom:

  qchi(.95, 10) 
  #[1] 4.278672

  #----------

  # The cumulative distribution function of a chi distribution with 
  # 5 degrees of freedom evaluated at 3:

  pchi(3, 5) 
  #[1] 0.8909358

  #----------

  # A random sample of 2 numbers from a chi distribution with 7 degrees of freedom. 
  # (Note: the call to set.seed simply allows you to reproduce this example.)

  set.seed(20) 
  rchi(2, 7) 
  #[1] 3.271632 2.035179
}
\keyword{ distribution }
\keyword{ datagen }
