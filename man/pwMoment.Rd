\name{pwMoment}
\alias{pwMoment}
\title{
    Estimate Probability-Weighted Moments
}
\description{
  Estimate the \eqn{1jk}'th probability-weighted moment from a random sample, 
  where either \eqn{j = 0}, \eqn{k = 0}, or both.
}
\usage{
  pwMoment(x, j = 0, k = 0, method = "unbiased", 
    plot.pos.cons = c(a = 0.35, b = 0), na.rm = FALSE)
}
\arguments{
  \item{x}{
  numeric vector of observations. 
}
  \item{j, k}{
  non-negative integers specifying the order of the moment.
}
  \item{method}{
  character string specifying what method to use to compute the 
  probability-weighted moment.  The possible values are \code{"unbiased"} 
  (method based on the U-statistic; the default), or \code{"plotting.position"} 
  (method based on the plotting position formula).  See the DETAILS section for 
  more information.
}
  \item{plot.pos.cons}{
  numeric vector of length 2 specifying the constants used in the formula for the 
  plotting positions when \code{method="plotting.position"}.  The default value is 
  \code{plot.pos.cons=c(a=0.35, b=0)}.  If this vector has a names attribute with 
  the value \code{c("a","b")} or \code{c("b","a")}, then the elements will be 
  matched by name in the formula for computing the plotting positions.  Otherwise, 
  the first element is mapped to the name \code{"a"} and the second element to the 
  name \code{"b"}.  See the DETAILS section for more information.  This argument is 
  ignored if \code{method="ubiased"}.
}
  \item{na.rm}{
  logical scalar indicating whether to remove missing values from \code{x}.  
  If \code{na.rm=FALSE} (the default) and \code{x} contains missing values, 
  then a missing value (\code{NA}) is returned.  If \code{na.rm=TRUE}, missing 
  values are removed from \code{x} prior to computing the probability-weighted 
  moment.
}
}
\details{
  The definition of a probability-weighted moment, introduced by 
  Greenwood et al. (1979), is as follows.  Let \eqn{X} denote a random variable 
  with cdf \eqn{F}, and let \eqn{x(p)} denote the \eqn{p}'th quantile of the 
  distribution.  Then the \eqn{ijk}'th probability-weighted moment is given by:
  \deqn{M(i, j, k) = E[X^i F^j (1 - F)^k] = \int^1_0 [x(F)]^i F^j (1 - F)^k \, dF}
  where \eqn{i}, \eqn{j}, and \eqn{k} are real numbers.  Note that if \eqn{i} is a 
  nonnegative integer, then \eqn{M(i, 0, 0)} is the conventional \eqn{i}'th moment 
  about the origin.

  Greenwood et al. (1979) state that in the special case where \eqn{i}, \eqn{j}, and 
  \eqn{k} are nonnegative integers:
  \deqn{M(i, j, k) = B(j + 1, k + 1) E[X^i_{j+1, j+k+1}]}
  where \eqn{B(a, b)} denotes the \link[stats:Beta]{beta function} evaluated at 
  \eqn{a} and \eqn{b}, and 
  \deqn{E[X^i_{j+1, j+k+1}]}     
  denotes the \eqn{i}'th moment about the origin of the \eqn{(j + 1)}'th order 
  statistic for a sample of size \eqn{(j + k + 1)}. In particular, 
  \deqn{M(1, 0, k) = \frac{1}{k+1} E[X_{1, k+1}]}
  \deqn{M(1, j, 0) = \frac{1}{j+1} E[X_{j+1, j+1}]}
  where 
  \deqn{E[X_{1, k+1}]}
  denotes the expected value of the first order statistic (i.e., the minimum) in a 
  sample of size \eqn{(k + 1)}, and 
  \deqn{E[X_{j+1, j+1}]} 
  denotes the expected value of the \eqn{(j+1)}'th order statistic (i.e., the maximum) 
  in a sample of size \eqn{(j+1)}.

  \emph{Unbiased Estimators} (\code{method="unbiased"}) \cr
  Landwehr et al. (1979) show that, given a random sample of \eqn{n} values from 
  some arbitrary distribution, an unbiased, distribution-free, and parameter-free 
  estimator of \eqn{M(1, 0, k)} is given by:
  \deqn{\hat{M}(1, 0, k) = \frac{1}{n} \sum^{n-k}_{i=1} x_{i,n} \frac{{n-i \choose k}}{{n-1 \choose k}}}
  where the quantity \eqn{x_{i,n}} denotes the \eqn{i}'th order statistic in the 
  random sample of size \eqn{n}.  Hosking et al. (1985) note that this estimator is 
  closely related to U-statistics (Hoeffding, 1948; Lehmann, 1975, pp. 362-371).  
  Hosking et al. (1985) note that an unbiased, distribution-free, and parameter-free 
  estimator of \eqn{M(1, j, 0)} is given by:
  \deqn{\hat{M}(1, j, 0) = \frac{1}{n} \sum^n_{i=j+1} x_{i,n} \frac{{i-1 \choose j}}{{n-1 \choose j}}}
  \cr
 
  \emph{Plotting-Position Estimators} (\code{method="plotting.position"}) \cr
  Hosking et al. (1985) propose alternative estimators of \eqn{M(1, 0, k)} and 
  \eqn{M(1, j, 0)} based on plotting positions:
  \deqn{\hat{M}(1, 0, k) = \frac{1}{n} \sum^n_{i=1} (1 - p_{i,n})^k x_{i,n}}
  \deqn{\hat{M}(1, j, 0) = \frac{1}{n} \sum^n_{i=1} p_{i,n}^j x_{i,n}}
  where 
  \deqn{p_{i,n} = \hat{F}(x_{i,n})}
  denotes the plotting position of the \eqn{i}'th order statistic in the random 
  sample of size \eqn{n}, that is, a distribution-free estimate of the cdf of 
  \eqn{X} evaluated at the \eqn{i}'th order statistic.  Typically, plotting 
  positions have the form:
  \deqn{p_{i,n} = \frac{i-a}{n+b}}
  where \eqn{b > -a > -1}.  For this form of plotting position, the 
  plotting-position estimators are asymptotically equivalent to the U-statistic 
  estimators.
}
\value{
  A numeric scalar--the value of the \eqn{1jk}'th probability-weighted moment 
  as defined by Greenwood et al. (1979).
}
\references{
  Greenwood, J.A., J.M. Landwehr, N.C. Matalas, and J.R. Wallis. (1979).  
  Probability Weighted Moments: Definition and Relation to Parameters of Several 
  Distributions Expressible in Inverse Form.  \emph{Water Resources Research} 
  \bold{15}(5), 1049--1054. 

  Hoeffding, W. (1948).  A Class of Statistics with Asymptotically Normal 
  Distribution.  \emph{Annals of Mathematical Statistics} \bold{19}, 293--325.

  Hosking, J.R.M. (1990).  L-Moments: Analysis and Estimation of Distributions 
  Using Linear Combinations of Order Statistics.  \emph{Journal of the Royal 
  Statistical Society, Series B} \bold{52}(1), 105--124.
 
  Hosking, J.R.M., and J.R. Wallis (1995).  A Comparison of Unbiased and 
  Plotting-Position Estimators of L Moments.  \emph{Water Resources Research} 
  \bold{31}(8), 2019--2025.

  Hosking, J.R.M., J.R. Wallis, and E.F. Wood. (1985).  Estimation of the 
  Generalized Extreme-Value Distribution by the Method of 
  Probability-Weighted Moments.  \emph{Technometrics} \bold{27}(3), 251--261.

  Landwehr, J.M., N.C. Matalas, and J.R. Wallis. (1979).  Probability Weighted 
  Moments Compared With Some Traditional Techniques in Estimating Gumbel 
  Parameters and Quantiles.  \emph{Water Resources Research} \bold{15}(5), 
  1055--1064.

  Lehmann, E.L. (1975).  \emph{Nonparametrics: Statistical Methods Based on Ranks}.  
  Holden-Day, Oakland, CA, pp.362-371.
}
\author{
    Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  Greenwood et al. (1979) introduced the concept of probability-weighted moments 
  as a tool to derive estimates of distribution parameters for distributions that 
  can be (perhaps only be) expressed in inverse form.  The term \dQuote{inverse form} 
  simply means that instead of characterizing the distribution by the formula for 
  its cumulative distribution function (cdf), the distribution is characterized by 
  the formula for the \eqn{p}'th quantile (\eqn{0 \le p \le 1}).

  For distributions that can only be expressed in inverse form, moment estimates of 
  their parameters are not available, and maximum likelihood estimates are not easy 
  to compute.  Greenwood et al. (1979) show that in these cases, it is often possible 
  to derive expressions for the distribution parameters in terms of 
  probability-weighted moments.  Thus, for these cases the distribution parameters 
  can be estimated based on the sample probability-weighted moments, which are fairly 
  easy to compute.  Furthermore, for distributions whose parameters can be expressed 
  as functions of conventional moments, the method of probability-weighted moments 
  provides an alternative to method of moments and maximum likelihood estimators.

  Landwehr et al. (1979) use the method of probability-weighted moments to estimate 
  the parameters of the \link[=EVD]{Type I Extreme Value (Gumbel) distribution}.

  Hosking et al. (1985) use the method of probability-weighted moments to estimate 
  the parameters of the \link[=GEVD]{generalized extreme value distribution}.

  Hosking (1990) and Hosking and Wallis (1995) show the relationship between 
  probabiity-weighted moments and \link[=lMoment]{L-moments}.

  Hosking and Wallis (1995) recommend using the unbiased estimators of 
  probability-weighted moments for almost all applications.
}
\seealso{
  \code{\link{eevd}}, \code{\link{egevd}}, \code{\link{lMoment}}.
}
\examples{
  # Generate 20 observations from a generalized extreme value distribution 
  # with parameters location=10, scale=2, and shape=.25, then compute the 
  # 0'th, 1'st and 2'nd probability-weighted moments. 
  # (Note: the call to set.seed simply allows you to reproduce this example.)

  set.seed(250) 
  dat <- rgevd(20, location = 10, scale = 2, shape = 0.25) 

  pwMoment(dat) 
  #[1] 10.59556
 
  pwMoment(dat, 1) 
  #[1] 5.798481
  
  pwMoment(dat, 2) 
  #[1] 4.060574
  
  pwMoment(dat, k = 1) 
  #[1] 4.797081
 
  pwMoment(dat, k = 2) 
  #[1] 3.059173
 
  pwMoment(dat, 1, method = "plotting.position") 
  # [1] 5.852913
 
  pwMoment(dat, 1, method = "plotting.position", 
    plot.pos = c(.325, 1)) 
  #[1] 5.586817 

  #----------

  # Clean Up
  #---------
  rm(dat)
}
\keyword{ distribution }
\keyword{ htest }
