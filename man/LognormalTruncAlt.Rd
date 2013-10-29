\name{LognormalTruncAlt}
\alias{LognormalTruncAlt}
\alias{dlnormTruncAlt}
\alias{plnormTruncAlt}
\alias{qlnormTruncAlt}
\alias{rlnormTruncAlt}
\title{
  The Truncated Lognormal Distribution (Alternative Parameterization)
}
\description{
  Density, distribution function, quantile function, and random generation 
  for the truncated lognormal distribution with parameters \code{mean}, 
  \code{cv}, \code{min}, and \code{max}.
}
\usage{
  dlnormTruncAlt(x, mean = exp(1/2), cv = sqrt(exp(1) - 1), min = 0, max = Inf)
  plnormTruncAlt(q, mean = exp(1/2), cv = sqrt(exp(1) - 1), min = 0, max = Inf)
  qlnormTruncAlt(p, mean = exp(1/2), cv = sqrt(exp(1) - 1), min = 0, max = Inf)
  rlnormTruncAlt(n, mean = exp(1/2), cv = sqrt(exp(1) - 1), min = 0, max = Inf)
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
  The default is \code{mean=exp(1/2)}.
}
  \item{cv}{
  vector of (positive) coefficient of variations of the non-truncated random variable. 
  The default is \code{cv=sqrt(exp(1)-1)}.
}
  \item{min}{
  vector of minimum values for truncation on the left.  The default value is 
  \code{min=0}.
}
  \item{max}{
  vector of maximum values for truncation on the right.  The default value is 
  \code{max=Inf}.
}
}
\details{
  See the help file for \link{LognormalAlt} for information about the 
  density and cdf of a lognormal distribution with this alternative 
  parameterization.

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

  The \eqn{p^{th}} quantile \eqn{y_p} of \eqn{Y} is given by:
  \tabular{lll}{
    \eqn{y_p =}  \tab \eqn{A}                                \tab for \eqn{p = 0} \cr
                 \tab \eqn{F^{-1}\{p[F(B) - F(A)] + F(A)\} } \tab for \eqn{0 < p < 1} \cr
                 \tab \eqn{B}                                \tab for \eqn{p = 1} \cr
  } 

  Random numbers are generated using the inverse transformation method:
  \deqn{y = G^{-1}(u)}
  where \eqn{u} is a random deviate from a uniform \eqn{[0, 1]} distribution.
}
\value{
  \code{dlnormTruncAlt} gives the density, \code{plnormTruncAlt} gives the distribution function, 
  \code{qlnormTruncAlt} gives the quantile function, and \code{rlnormTruncAlt} generates random 
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
  A truncated lognormal distribution is sometimes used as an input distribution 
  for probabilistic risk assessment.
}
\seealso{
  \link{LognormalAlt},   
  \link{Probability Distributions and Random Numbers}.
}
\examples{
  # Density of a truncated lognormal distribution with parameters 
  # mean=10, cv=1, min=0, max=20, evaluated at 2 and 12:

  dlnormTruncAlt(c(2, 12), 10, 1, 0, 20) 
  #[1] 0.08480874 0.03649884

  #----------

  # The cdf of a truncated lognormal distribution with parameters 
  # mean=10, cv=1, min=0, max=20, evaluated at 2 and 12:

  plnormTruncAlt(c(2, 4), 10, 1, 0, 20) 
  #[1] 0.07230627 0.82467603

  #----------

  # The median of a truncated lognormal distribution with parameters 
  # mean=10, cv=1, min=0, max=20:

  qlnormTruncAlt(.5, 10, 1, 0, 20) 
  #[1] 6.329505

  #----------

  # A random sample of 3 observations from a truncated lognormal distribution 
  # with parameters mean=10, cv=1, min=0, max=20. 
  # (Note: the call to set.seed simply allows you to reproduce this example.)

  set.seed(20) 
  rlnormTruncAlt(3, 10, 1, 0, 20) 
  #[1]  6.685391 17.445387 18.543553
}
\keyword{ distribution }
\keyword{ datagen }
