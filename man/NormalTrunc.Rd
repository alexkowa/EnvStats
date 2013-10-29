\name{NormalTrunc}
\alias{NormalTrunc}
\alias{dnormTrunc}
\alias{pnormTrunc}
\alias{qnormTrunc}
\alias{rnormTrunc}
\title{
  The Truncated Normal Distribution
}
\description{
  Density, distribution function, quantile function, and random generation 
  for the truncated normal distribution with parameters \code{mean}, 
  \code{sd}, \code{min}, and \code{max}.
}
\usage{
  dnormTrunc(x, mean = 0, sd = 1, min = -Inf, max = Inf)
  pnormTrunc(q, mean = 0, sd = 1, min = -Inf, max = Inf)
  qnormTrunc(p, mean = 0, sd = 1, min = -Inf, max = Inf)
  rnormTrunc(n, mean = 0, sd = 1, min = -Inf, max = Inf)
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
  \item{mean}{
  vector of means of the distribution of the non-truncated random variable.  
  The default is \code{mean=0}.
}
  \item{sd}{
  vector of (positive) standard deviations of the non-truncated random variable.  
  The default is \code{sd=1}.
}
  \item{min}{
  vector of minimum values for truncation on the left.  The default value is 
  \code{min=-Inf}.
}
  \item{max}{
  vector of maximum values for truncation on the right.  The default value is 
  \code{max=Inf}.
}
}
\details{
  See the help file for \link[stats:Normal]{the normal distribution} 
  for information about the density and cdf of a normal distribution.

  \emph{Probability Density and Cumulative Distribution Function} \cr
  Let \eqn{X} denote a random variable with density function \eqn{f(x)} and 
  cumulative distribution function \eqn{F(x)}, and let 
  \eqn{Y} denote the truncated version of \eqn{X} where \eqn{Y} is truncated 
  below at \code{min=}\eqn{A} and above at\code{max=}\eqn{B}.  Then the density 
  function of \eqn{Y}, denoted \eqn{g(y)}, is given by:
  \deqn{g(y) = frac{f(y)}{F(B) - F(A)}, A \le y \le B}
  and the cdf of Y, denoted \eqn{G(y)}, is given by:
  \tabular{lll}{
    \eqn{G(y) =}  \tab  0                                    \tab for \eqn{y < A} \cr
                  \tab \eqn{\frac{F(y) - F(A)}{F(B) - F(A)}} \tab for \eqn{A \le y \le B} \cr
                  \tab 1                                     \tab for \eqn{y > B} \cr
  }

  \emph{Quantiles} \cr
  The \eqn{p^{th}} quantile \eqn{y_p} of \eqn{Y} is given by:
  \tabular{lll}{
    \eqn{y_p =}  \tab \eqn{A}                                \tab for \eqn{p = 0} \cr
                 \tab \eqn{F^{-1}\{p[F(B) - F(A)] + F(A)\} } \tab for \eqn{0 < p < 1} \cr
                 \tab \eqn{B}                                \tab for \eqn{p = 1} \cr
  } 

  \emph{Random Numbers} \cr
  Random numbers are generated using the inverse transformation method:
  \deqn{y = G^{-1}(u)}
  where \eqn{u} is a random deviate from a uniform \eqn{[0, 1]} distribution. 

  \emph{Mean and Variance} \cr
  The expected value of a truncated normal random variable with parameters 
  \code{mean=}\eqn{\mu}, \code{sd=}\eqn{\sigma}, \code{min=}\eqn{A}, and 
  \code{max=}\eqn{B} is given by:
  \deqn{E(Y) = \mu + \sigma^2 \frac{f(A) - f(B)}{F(B) - F(A)}}
  (Johnson et al., 1994, p.156; Schneider, 1986, p.17).

  The variance of this random variable is given by:
  \deqn{\sigma^2 + \sigma^3 \{z_A f(A) - z_B f(B) - \sigma[f(A) - f(B)]^2 \}}
  where
  \deqn{z_A = \frac{A - \mu}{\sigma}; \, z_B = \frac{B - \mu}{\sigma}}
  (Johnson et al., 1994, p.158; Schneider, 1986, p.17).
}
\value{
  \code{dnormTrunc} gives the density, \code{pnormTrunc} gives the distribution function, 
  \code{qnormTrunc} gives the quantile function, and \code{rnormTrunc} generates random 
  deviates. 
}
\references{
  Forbes, C., M. Evans, N. Hastings, and B. Peacock. (2011).  Statistical Distributions. 
  Fourth Edition. John Wiley and Sons, Hoboken, NJ.

  Johnson, N. L., S. Kotz, and N. Balakrishnan. (1994). 
  \emph{Continuous Univariate Distributions, Volume 1}. 
  Second Edition. John Wiley and Sons, New York.

  Schneider, H. (1986).  \emph{Truncated and Censored Samples from Normal Populations}.  
  Marcel Dekker, New York, Chapter 2.
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  A truncated normal distribution is sometimes used as an input distribution 
  for probabilistic risk assessment.
}
\seealso{
  \link[stats:Normal]{Normal},   
  \link{Probability Distributions and Random Numbers}.
}
\examples{
  # Density of a truncated normal distribution with parameters 
  # mean=10, sd=2, min=8, max=13, evaluated at 10 and 11.5:

  dnormTrunc(c(10, 11.5), 10, 2, 8, 13) 
  #[1] 0.2575358 0.1943982

  #----------

  # The cdf of a truncated normal distribution with parameters 
  # mean=10, sd=2, min=8, max=13, evaluated at 10 and 11.5:

  pnormTrunc(c(10, 11.5), 10, 2, 8, 13) 
  #[1] 0.4407078 0.7936573

  #----------

  # The median of a truncated normal distribution with parameters 
  # mean=10, sd=2, min=8, max=13:

  qnormTrunc(.5, 10, 2, 8, 13) 
  #[1] 10.23074

  #----------

  # A random sample of 3 observations from a truncated normal distribution 
  # with parameters mean=10, sd=2, min=8, max=13. 
  # (Note: the call to set.seed simply allows you to reproduce this example.)

  set.seed(20) 
  rnormTrunc(3, 10, 2, 8, 13) 
  #[1] 11.975223 11.373711  9.361258
}
\keyword{ distribution }
\keyword{ datagen }
