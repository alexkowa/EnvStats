\name{evNormOrdStats}
\alias{evNormOrdStats}
\alias{evNormOrdStatsScalar}
\title{
  Expected Value of Order Statistics from Random Sample from Standard Normal Distribution
}
\description{
  Compute the expected value of order statistics from a random sample from 
  a standard \link[stats:Normal]{normal distribution}.
}
\usage{
  evNormOrdStats(n = 1, approximate = FALSE)

  evNormOrdStatsScalar(r = 1, n = 1, approximate = FALSE)
}
\arguments{
  \item{n}{
  positive integer indicating the sample size.
}
  \item{r}{
  positive integer between \code{1} and \code{n} specifying the order statistic 
  for which to compute the expected value.
}
  \item{approximate}{
  logical scalar indicating whether to use the Blom score approximation (Blom, 1958).  
  The default value is \code{FALSE}.
}
}
\details{
  Let \eqn{\underline{z} = z_1, z_2, \ldots, z_n} denote a vector of \eqn{n} 
  observations from a \link[stats:Normal]{normal distribution} with parameters 
  \code{mean=0} and \code{sd=1}.  That is, \eqn{\underline{z}} denotes a vector of 
  \eqn{n} observations from a \emph{standard} normal distribution.  Let 
  \eqn{z_{(r)}} denote the \eqn{r}'th order statistic of \eqn{\underline{z}}, 
  for \eqn{r = 1, 2, \ldots, n}.  The probability density function of 
  \eqn{z_{(r)}} is given by:
  \deqn{f_{r,n}(t) = \frac{n!}{(r-1)!(n-r)!} [\Phi(t)]^{r-1} [1 - \Phi(t)]^{n-r} \phi(t) \;\;\;\;\;\; (1)}
  where \eqn{\Phi} and \eqn{\phi} denote the cumulative distribution function and 
  probability density function of the standard normal distribution, respectively 
  (Johnson et al., 1994, p.93). Thus, the expected value of \eqn{z_{(r)}} is given by:
  \deqn{E(r, n) = E[z_{(r)}] = \int_{-\infty}^{\infty} t f_{r,n}(t) dt \;\;\;\;\;\; (2)}
  It can be shown that if \eqn{n} is odd, then
  \deqn{E[(n+1)/2, n] = 0 \;\;\;\;\;\; (3)}
  Also, for all values of \eqn{n},
  \deqn{E(r, n) = -E(n-r, n) \;\;\;\;\;\; (4)}

  The function \code{evNormOrdStatsScalar} computes the value of \eqn{E(r,n)} for 
  user-specified values of \eqn{r} and \eqn{n}.

  The function \code{evNormOrdStats} computes the values of \eqn{E(r,n)} for all 
  values of \eqn{r} for a user-specified value of \eqn{n}.

  For large values of \eqn{n}, the function \code{evNormOrdStats} with 
  \code{approximate=FALSE} may take a long time to execute.  When 
  \code{approximate=TRUE}, \code{evNormOrdStats} and \code{evNormOrdStatsScalar} 
  use the following approximation to \eqn{E(r,n)}, which was proposed by 
  Blom (1958, pp. 68-75):
  \deqn{E(r, n) \approx \Phi^{-1}(\frac{r - 3/8}{n + 1/4}) \;\;\;\;\;\; (5)}
  This approximation is quite accurate.  For example, for \eqn{n \ge 2}, the 
  approximation is accurate to the first decimal place, and for \eqn{n \ge 9} it 
  is accurate to the second decimal place.
}
\value{
  For \code{evNormOrdStats}: a numeric vector of length \code{n} containing the 
  expected values of all the order statistics for a random sample of \code{n} 
  standard normal deviates.

  For \code{evNormOrdStatsScalar}: a numeric scalar containing the expected value 
  of the \code{r}'th order statistic from a random sample of \code{n} standard 
  normal deviates.
}
\references{
  Johnson, N. L., S. Kotz, and N. Balakrishnan. (1994). 
  \emph{Continuous Univariate Distributions, Volume 1}. 
  Second Edition. John Wiley and Sons, New York, pp. 93--99.

  Royston, J.P. (1982).  Algorithm AS 177.  Expected Normal Order Statistics 
  (Exact and Approximate).  \emph{Applied Statistics} \bold{31}, 161--165.
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  The expected values of normal order statistics are used to construct normal 
  quantile-quantile (Q-Q) plots (see \code{\link{qqPlot}}) and to compute 
  goodness-of-fit statistics (see \code{\link{gofTest}}).  Usually, however, 
  approximations are used instead of exact values.  The functions 
  \code{evNormOrdStats} and \cr 
  \code{evNormOrdStatsScalar} have been included mainly 
  because \code{evNormOrdStatsScalar} is called by \code{\link{elnorm3}} and \cr 
  \code{\link{predIntNparSimultaneousTestPower}}.
}
\seealso{
  \link{Normal}, \code{\link{elnorm3}}, 
  \code{\link{predIntNparSimultaneousTestPower}}, \code{\link{gofTest}}, 
  \code{\link{qqPlot}}.
}
\examples{
  # Compute the expected value of the minimum for a random sample of size 10 
  # from a standard normal distribution:

  evNormOrdStatsScalar(r = 1, n = 10) 
  #[1] -1.538753

  #----------

  # Compute the expected values of all of the order statistics for a random sample 
  # of size 10 from a standard normal distribution:

  evNormOrdStats(10) 
  #[1] -1.5387527 -1.0013570 -0.6560591 -0.3757647 -0.1226888
  #[6]  0.1226888  0.3757647  0.6560591  1.0013570  1.5387527

  # Compare the above with Blom (1958) scores:

  evNormOrdStats(10, approx = TRUE) 
  #[1] -1.5466353 -1.0004905 -0.6554235 -0.3754618 -0.1225808
  #[6]  0.1225808  0.3754618  0.6554235  1.0004905  1.5466353
}
\keyword{ distribution }
