\name{evNormOrdStats}
\alias{evNormOrdStats}
\alias{evNormOrdStatsScalar}
\title{
  Expected Value of Order Statistics for Random Sample from Standard Normal Distribution
}
\description{
  Compute the expected values of order statistics for a random sample from 
  a standard \link[stats:Normal]{normal distribution}.
}
\usage{
  evNormOrdStats(n = 1, 
    method = "royston", lower = -9, inc = 0.025, warn = TRUE, 
    alpha = 3/8, nmc = 2000, seed = 47, approximate = NULL)

  evNormOrdStatsScalar(r = 1, n = 1, 
    method = "royston", lower = -9, inc = 0.025, warn = TRUE, 
    alpha = 3/8, nmc = 2000, conf.level = 0.95, seed = 47, approximate = NULL) 
}
\arguments{
  \item{n}{
  positive integer indicating the sample size.
}
  \item{r}{
  positive integer between \code{1} and \code{n} specifying the order statistic 
  for which to compute the expected value.
}
  \item{method}{
  character string indicating what method to use.  The possible values are:
  \itemize{
    \item \code{"royston"}.  Method based on approximating the exact integral as 
          given in Royston (1982).
    \item \code{"blom"}.  Method based on the approximation formula proposed by 
          Blom (1958).
    \item \code{"mc"}.  Method based on Monte Carlo simulation.
  }

  See the DETAILS section below.
}
  \item{lower}{
  numeric scalar \eqn{\le -9} defining the lower bound used for approximating the 
  integral when \code{method="royston"}.  The upper bound is automatically set 
  to \code{-lower}.  The default value is \code{lower=-9}.
}
  \item{inc}{
  numeric scalar between \code{.Machine$double.eps} and \code{0.025} that determines 
  the width of each subdivision used to approximate the integral when \cr
  \code{method="royston"}.  The default value is \code{inc=0.025}.
}
  \item{warn}{
  logical scalar indicating whether to issue a warning when 
  \code{method="royston"} and the sample size is greater than 2000.  
  The default value is \code{warn=TRUE}.
}
  \item{alpha}{
  numeric scalar between 0 and 0.5 that determines the constant used when \cr 
  \code{method="blom"}.  The default value is \code{alpha=3/8}.
}
  \item{nmc}{
  integer \eqn{\ge 100} denoting the number of Monte Carlo simulations to use 
  when \code{method="mc"}.  The default value is \code{nmc=2000}.
}
  \item{conf.level}{
  numeric scalar between 0 and 1 denoting the confidence level of 
  the confidence interval for the expected value of the normal 
  order statistic when \code{method="mc"}.  
  The default value is \code{conf.level=0.95}.
}
  \item{seed}{
  integer between \eqn{-(2^31 - 1)} and \eqn{2^31 - 1} specifying 
  the argument to \code{\link{set.seed}} (the random number seed) 
  when \code{method="mc"}.  The default value is \code{seed=47}.
}
  \item{approximate}{
  logical scalar included for backwards compatibility with versions of 
  \pkg{EnvStats} prior to version 2.3.0.  
  When \code{method} is not supplied and \cr
  \code{approximate=FALSE}, \code{method} is set to \code{method="royston"}.  
  When \code{method} is not supplied and \code{approximate=TRUE}, 
  \code{method} is set to \code{method="blom"}.  
  This argument is ignored if \code{method} is supplied and/or 
  \code{approxmiate=NULL} (the default).
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
  \deqn{E(r, n) = -E(n-r+1, n) \;\;\;\;\;\; (4)}

  The function \code{evNormOrdStatsScalar} computes the value of \eqn{E(r,n)} for 
  user-specified values of \eqn{r} and \eqn{n}.

  The function \code{evNormOrdStats} computes the values of \eqn{E(r,n)} for all 
  values of \eqn{r} (i.e., for \eqn{r = 1, 2, \ldots, n}) 
  for a user-specified value of \eqn{n}.

  \bold{Exact Method Based on Royston's Approximation to the Integral} (\code{method="royston"}) 

  When \code{method="royston"}, the integral in Equation (2) above is approximated by 
  computing the value of the integrand between the values of \code{lower} and 
  \code{-lower} using increments of \code{inc}, then summing these values and 
  multiplying by \code{inc}.  In particular, the integrand is restructured as:
  \deqn{t \; f_{r,n}(t) = t \; exp\{log(n!) - log[(r-1)!] - log[(n-r)!] +  (r-1)log[\Phi(t)] + (n-r)log[1 - \Phi(t)] + log[\phi(t)]\} \;\;\; (5)}
  By default, as per Royston (1982), the integrand is evaluated between -9 and 9 in 
  increments of 0.025.  The approximation is computed this way for values of 
  \eqn{r} between \eqn{1} and \eqn{[n/2]}, where \eqn{[x]} denotes the floor of \eqn{x}.  
  If \eqn{r > [n/2]}, then the approximation is computed for \eqn{E(n-r+1, n)} and 
  Equation (4) is used. 

  Note that Equation (1) in Royston (1982) differs from Equations (1) and (2) above 
  because Royston's paper is based on the \eqn{r^{th}} \emph{largest} value, 
  not the \eqn{r^{th}} order statistic. 

  Royston (1982) states that this algorithm \dQuote{is accurate to at least seven decimal 
  places on a 36-bit machine,} that it has been validated up to a sample size 
  of \eqn{n=2000}, and that the accuracy for \eqn{n > 2000} may be improved by 
  reducing the value of the argument \code{inc}.  Note that making 
  \code{inc} smaller will increase the computation time.  \cr

  \bold{Approxmation Based on Blom's Method} (\code{method="blom"}) 

  When \code{method="blom"}, the following approximation to \eqn{E(r,n)}, 
  proposed by Blom (1958, pp. 68-75), is used:
  \deqn{E(r, n) \approx \Phi^{-1}(\frac{r - \alpha}{n - 2\alpha + 1}) \;\;\;\;\;\; (5)}
  By default, \eqn{\alpha = 3/8 = 0.375}.  This approximation is quite accurate.  
  For example, for \eqn{n \ge 2}, the approximation is accurate to the first decimal place, 
  and for \eqn{n \ge 9} it is accurate to the second decimal place.

  Harter (1961) discusses appropriate values of \eqn{\alpha} for various sample sizes 
  \eqn{n} and values of \eqn{r}. \cr

  \bold{Approximation Based on Monte Carlo Simulation} (\code{method="mc"}) 

  When \code{method="mc"}, Monte Carlo simulation is used to estmate the expected value 
  of the \eqn{r^{th}} order statistic.  That is, \eqn{N =} \code{nmc} trials are run in which, 
  for each trial, a random sample of \eqn{n} standard normal observations is 
  generated and the \eqn{r^{th}} order statistic is computed.  Then, the average value 
  of this order statistic over all \eqn{N} trials is computed, along with a  
  confidence interval for the expected value, assuming an approximately 
  normal distribution for the mean of the order statistic (the confidence interval 
  is computed by supplying the simulated values of the \eqn{r^{th}} order statistic 
  to the function \code{\link{enorm}}).

  \bold{NOTE:} This method has not been optimized for large sample sizes \eqn{n} 
  (i.e., large values of the argument \code{n}) and/or a large number of 
  Monte Carlo trials \eqn{N} (i.e., large values of the argument \code{nmc}) and 
  may take a long time to execute in these cases.
}
\value{
  For \code{evNormOrdStats}: a numeric vector of length \code{n} containing the 
  expected values of all the order statistics for a random sample of \code{n} 
  standard normal deviates.

  For \code{evNormOrdStatsScalar}: a numeric scalar containing the expected value 
  of the \code{r}'th order statistic from a random sample of \code{n} standard 
  normal deviates.  When \code{method="mc"}, the returned object also has a 
  \code{cont.int} attribute that contains the 95% confidence interval for the expected value, 
  and a \code{nmc} attribute indicating the number of Monte Carlo trials run.  
}
\references{
  Blom, G. (1958). \emph{Statistical Estimates and Transformed Beta Variables}. 
  John Wiley and Sons, New York.

  Harter, H. L. (1961). \emph{Expected Values of Normal Order Statistics} 
  \bold{48}, 151--165.

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
  because \code{evNormOrdStatsScalar} is called by \code{\link{elnorm3}} and 
  \code{\link{predIntNparSimultaneousTestPower}}.
}
\seealso{
  \link{Normal}, \code{\link{ppoints}}, \code{\link{elnorm3}}, 
  \code{\link{predIntNparSimultaneousTestPower}}, \code{\link{gofTest}}, 
  \code{\link{qqPlot}}.
}
\examples{
  # Compute the expected value of the minimum for a random sample of size 10 
  # from a standard normal distribution:

  # Based on method="royston"
  #--------------------------
  evNormOrdStatsScalar(r = 1, n = 10) 
  #[1] -1.538753


  # Based on method="blom"
  #-----------------------
  evNormOrdStatsScalar(r = 1, n = 10, method = "blom") 
  #[1] -1.546635


  # Based on method="mc" with 10,000 Monte Carlo trials
  #----------------------------------------------------
  evNormOrdStatsScalar(r = 1, n = 10, method = "mc", nmc = 10000) 
  #[1] -1.544318
  #attr(,"confint")
  #   95%LCL    95%UCL 
  #-1.555838 -1.532797 
  #attr(,"nmc")
  #[1] 10000

  #====================

  # Compute the expected values of all of the order statistics 
  # for a random sample of size 10 from a standard normal distribution
  # based on Royston's (1982) method:
  #--------------------------------------------------------------------

  evNormOrdStats(10) 
  #[1] -1.5387527 -1.0013570 -0.6560591 -0.3757647 -0.1226678
  #[6]  0.1226678  0.3757647  0.6560591  1.0013570  1.5387527


  # Compare the above with Blom (1958) scores:
  #-------------------------------------------

  evNormOrdStats(10, method = "blom") 
  #[1] -1.5466353 -1.0004905 -0.6554235 -0.3754618 -0.1225808
  #[6]  0.1225808  0.3754618  0.6554235  1.0004905  1.5466353
}
\keyword{ distribution }
