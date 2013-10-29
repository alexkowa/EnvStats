\name{EulersConstant}
\alias{EulersConstant}
\alias{Eulers Constant}
\concept{Euler's Constant}
\title{
  Euler's Constant
}
\description{
  Explanation of Euler's Constant.
}
\details{
  Euler's Constant, here denoted \eqn{\epsilon}, is a real-valued number that can 
  be defined in several ways.  Johnson et al. (1992, p. 5) use the definition: 
  \deqn{\epsilon = \lim_{n \to \infty}[1 + \frac{1}{2} + \frac{1}{3} + \ldots + \frac{1}{n} - log(n)]}
  and note that it can also be expressed as 
  \deqn{\epsilon = -\Psi(1)} 
  where \eqn{\Psi()} is the \link[base:Special]{digamma function} 
  (Johnson et al., 1992, p.8).

  The value of Euler's Constant, to 10 decimal places, is 0.5772156649.

  The expression for the mean of a 
  \link[=EVD]{Type I extreme value (Gumbel) distribution} involves Euler's 
  constant; hence Euler's constant is used to compute the method of moments 
  estimators for this distribution (see \code{\link{eevd}}).
}
\references{
  Johnson, N. L., S. Kotz, and A.W. Kemp. (1992).  
  \emph{Univariate Discrete Distributions}.  Second Edition.  
  John Wiley and Sons, New York, pp.4-8.

  Johnson, N. L., S. Kotz, and N. Balakrishnan. (1995). 
  \emph{Continuous Univariate Distributions, Volume 2}. 
  Second Edition. John Wiley and Sons, New York.
}
\author{
    Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\seealso{
  \link[=EVD]{Extreme Value Distribution}, \code{\link{eevd}}.
}
\keyword{ distribution }


