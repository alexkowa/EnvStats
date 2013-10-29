\name{tolIntPois}
\alias{tolIntPois}
\title{
  Tolerance Interval for a Poisson Distribution
}
\description{
  Construct a \eqn{\beta}-content or \eqn{\beta}-expectation tolerance 
  interval for a \link[stats:Poisson]{Poisson distribution}.
}
\usage{
  tolIntPois(x, coverage = 0.95, cov.type = "content", ti.type = "two-sided", 
    conf.level = 0.95)
}
\arguments{
  \item{x}{
  numeric vector of observations, or an object resulting from a call to an 
  estimating function that assumes a Poisson distribution 
  (i.e., \code{\link{epois}} or \code{\link{epoisCensored}}).  
  If \code{cov.type="content"} then \code{x} must be a numeric vector.  
  If \code{x} is a numeric vector, 
  missing (\code{NA}), undefined (\code{NaN}), and 
  infinite (\code{Inf}, \code{-Inf}) values are allowed but will be removed.
}
  \item{coverage}{
  a scalar between 0 and 1 indicating the desired coverage of the tolerance interval.  
  The default value is \code{coverage=0.95}.  If \code{cov.type="expectation"}, 
  this argument is ignored.
}
  \item{cov.type}{
  character string specifying the coverage type for the tolerance interval.  
  The possible values are \code{"content"} (\eqn{\beta}-content; the default), and 
  \code{"expectation"} (\eqn{\beta}-expectation).  See the DETAILS section for more 
  information.
}
  \item{ti.type}{
  character string indicating what kind of tolerance interval to compute.  
  The possible values are \code{"two-sided"} (the default), \code{"lower"}, and 
  \code{"upper"}.
}
  \item{conf.level}{
  a scalar between 0 and 1 indicating the confidence level associated with the tolerance 
  interval.  The default value is \code{conf.level=0.95}.
}
}
\details{
  If \code{x} contains any missing (\code{NA}), undefined (\code{NaN}) or 
  infinite (\code{Inf}, \code{-Inf}) values, they will be removed prior to 
  performing the estimation.

  A tolerance interval for some population is an interval on the real line constructed so as to 
  contain \eqn{100 \beta \%} of the population (i.e., \eqn{100 \beta \%} of all 
  future observations), where \eqn{0 < \beta < 1}.  The quantity \eqn{100 \beta \%} is called 
  the \emph{coverage}.

  There are two kinds of tolerance intervals (Guttman, 1970):
  \itemize{ 
    \item A \eqn{\beta}-content tolerance interval with confidence level \eqn{100(1-\alpha)\%} is 
    constructed so that it contains at least \eqn{100 \beta \%} of the population (i.e., the 
    coverage is at least \eqn{100 \beta \%}) with probability \eqn{100(1-\alpha)\%}, where 
    \eqn{0 < \alpha < 1}. The quantity \eqn{100(1-\alpha)\%} is called the confidence level or 
    confidence coefficient associated with the tolerance interval.

    \item A \eqn{\beta}-expectation tolerance interval is constructed so that the \emph{average} coverage of 
    the interval is \eqn{100 \beta \%}.
  } 

  \bold{Note:} A \eqn{\beta}-expectation tolerance interval with coverage \eqn{100 \beta \%} is 
  equivalent to a prediction interval for one future observation with associated confidence level 
  \eqn{100 \beta \%}.  Note that there is no explicit confidence level associated with a 
  \eqn{\beta}-expectation tolerance interval.  If a \eqn{\beta}-expectation tolerance interval is 
  treated as a \eqn{\beta}-content tolerance interval, the confidence level associated with this 
  tolerance interval is usually around 50\% (e.g., Guttman, 1970, Table 4.2, p.76).  

  Because of the discrete nature of the \link[stats:Poisson]{Poisson distribution}, 
  even true tolerance intervals (tolerance intervals based on the true value of 
  \eqn{\lambda}) will usually not contain exactly \eqn{\beta\%} of the population.  
  For example, for the Poisson distribution with parameter \code{lambda=2}, the 
  interval [0, 4] contains 94.7\% of this distribution and the interval [0, 5] 
  contains 98.3\% of this distribution.  Thus, no interval can contain exactly 95\% 
  of this distribution.

  \emph{\eqn{\beta}-Content Tolerance Intervals for a Poisson Distribution} \cr
  Zacks (1970) showed that for monotone likelihood ratio (MLR) families of discrete 
  distributions, a uniformly most accurate upper \eqn{\beta100\%} \eqn{\beta}-content 
  tolerance interval with associated confidence level \eqn{(1-\alpha)100\%} is 
  constructed by finding the upper \eqn{(1-\alpha)100\%} confidence limit for the 
  parameter associated with the distribution, and then computing the \eqn{\beta}'th 
  quantile of the distribution assuming the true value of the parameter is equal to 
  the upper confidence limit.  This idea can be extended to one-sided lower and 
  two-sided tolerance limits.

  It can be shown that all distributions that are one parameter exponential families 
  have the MLR property, and the Poisson distribution is a one-parameter exponential 
  family, so the method of Zacks (1970) can be applied to a Poisson distribution.

  Let \eqn{X} denote a \link[stats:Poisson]{Poisson random variable} with parameter 
  \code{lambda=}\eqn{\lambda}.  Let \eqn{x_{p|\lambda}} denote the \eqn{p}'th quantile 
  of this distribution. That is,
  \deqn{Pr(X < x_{p|\lambda}) \le p \le Pr(X \le x_{p|\lambda}) \;\;\;\;\;\; (1)}
  Note that due to the discrete nature of the Poisson distribution, there will be 
  several values of \eqn{p} associated with one value of \eqn{X}.  For example, for 
  \eqn{\lambda=2}, the value 1 is the \eqn{p}'th quantile for any value of \eqn{p} 
  between 0.140 and 0.406.

  Let \eqn{\underline{x}} denote a vector of \eqn{n} observations from a 
  \link[stats:Poisson]{Poisson distribution} with parameter \code{lambda=}\eqn{\lambda}.  
  When \code{ti.type="upper"}, the first step is to compute the one-sided upper 
  \eqn{(1-\alpha)100\%} confidence limit for \eqn{\lambda} based on the observations 
  \eqn{\underline{x}} (see the help file for \code{\link{epois}}).  Denote this upper 
  confidence limit by \eqn{UCL}.  The one-sided upper \eqn{\beta100\%} tolerance limit 
  is then given by:
  \deqn{[0, x_{\beta | \lambda = UCL}] \;\;\;\;\;\; (2)}
  Similarly, when \code{ti.type="lower"}, the first step is to compute the one-sided 
  lower \eqn{(1-\alpha)100\%} confidence limit for \eqn{\lambda} based on the 
  observations \eqn{\underline{x}}.  Denote this lower confidence limit by \eqn{LCL}.  
  The one-sided lower \eqn{\beta100\%} tolerance limit is then given by:
  \deqn{[x_{1-\beta | \lambda = LCL}, \infty] \;\;\;\;\;\; (3)}
  Finally, when \code{ti.type="two-sided"}, the first step is to compute the two-sided 
  \eqn{(1-\alpha)100\%} confidence limits for \eqn{\lambda} based on the 
  observations \eqn{\underline{x}}.  Denote these confidence limits by \eqn{LCL} and 
  \eqn{UCL}. The two-sided \eqn{\beta100\%} tolerance limit is then given by:
  \deqn{[x_{\frac{1-\beta}{2} | \lambda = LCL}, x_{\frac{1+\beta}{2} | \lambda = UCL}] \;\;\;\;\;\; (4)}
  Note that the function \code{tolIntPois} uses the exact confidence limits for 
  \eqn{\lambda} when computing \eqn{\beta}-content tolerance limits (see 
  \code{\link{epois}}).
  \cr

  \emph{\eqn{\beta}-Expectation Tolerance Intervals for a Poisson Distribution} \cr
  As stated above, a \eqn{\beta}-expectation tolerance interval with coverage 
  \eqn{\beta100\%} is equivalent to a prediction interval for one future observation 
  with associated confidence level \eqn{\beta100\%}.  This is because the probability 
  that any single future observation will fall into this interval is \eqn{\beta100\%}, 
  so the distribution of the number of \eqn{N} future observations that will fall into 
  this interval is \link[stats:Binomial]{binomial} with parameters 
  \code{size=}\eqn{N} and \code{prob=}\eqn{\beta}.  Hence the expected proportion of 
  future observations that fall into this interval is \eqn{\beta100\%} and is 
  independent of the value of \eqn{N}.  See the help file for \code{\link{predIntPois}} 
  for information on how these intervals are constructed.
}
\value{
  If \code{x} is a numeric vector, \code{tolIntPois} returns a list of class 
  \code{"estimate"} containing the estimated parameters, a component called 
  \code{interval} containing the tolerance interval information, and other 
  information.  See \code{\link{estimate.object}} for details.

  If \code{x} is the result of calling an estimation function, \code{tolIntPois} 
  returns a list whose class is the same as \code{x}.  The list contains the same 
  components as \code{x}.  If \code{x} already has a component called 
  \code{interval}, this component is replaced with the tolerance interval 
  information.
}
\references{
  Gibbons, R.D. (1987b).  Statistical Models for the Analysis of Volatile Organic 
  Compounds in Waste Disposal Sites.  \emph{Ground Water} \bold{25}, 572--580.

  Gibbons, R.D., D.K. Bhaumik, and S. Aryal. (2009). 
  \emph{Statistical Methods for Groundwater Monitoring}, Second Edition.  
  John Wiley & Sons, Hoboken.

  Guttman, I. (1970). \emph{Statistical Tolerance Regions: Classical and Bayesian}. 
  Hafner Publishing Co., Darien, CT.

  Hahn, G.J., and W.Q. Meeker. (1991). \emph{Statistical Intervals: A Guide for Practitioners}. 
  John Wiley and Sons, New York.

  Johnson, N. L., S. Kotz, and A. Kemp. (1992).  \emph{Univariate Discrete 
  Distributions}.  Second Edition.  John Wiley and Sons, New York, Chapter 4.

  Krishnamoorthy K., and T. Mathew. (2009). 
  \emph{Statistical Tolerance Regions: Theory, Applications, and Computation}. 
  John Wiley and Sons, Hoboken.

  Millard, S.P., and N.K. Neerchal. (2001). \emph{Environmental Statistics with S-PLUS}. 
  CRC Press, Boca Raton.

  Zacks, S. (1970).  Uniformly Most Accurate Upper Tolerance Limits for Monotone 
  Likelihood Ratio Families of Discrete Distributions.  \emph{Journal of the 
  American Statistical Association} \bold{65}, 307--316.
}
\author{
    Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  Tolerance intervals have long been applied to quality control and 
  life testing problems (Hahn, 1970b,c; Hahn and Meeker, 1991; Krishnamoorthy and 
  Mathew, 2009).  References that discuss tolerance intervals in the context of 
  environmental monitoring include:  Berthouex and Brown (2002, Chapter 21), 
  Gibbons et al. (2009), Millard and Neerchal (2001, Chapter 6), Singh et al. (2010b), 
  and USEPA (2009).

  Gibbons (1987b) used the Poisson distribution to model the number of detected 
  compounds per scan of the 32 volatile organic priority pollutants (VOC), and 
  also to model the distribution of chemical concentration (in ppb).  He explained 
  the derivation of a one-sided upper \eqn{\beta}-content tolerance limit for a 
  Poisson distribution based on the work of Zacks (1970) using the Pearson-Hartley 
  approximation to the confidence limits for the mean parameter \eqn{\lambda} 
  (see the help file for \code{\link{epois}}).  Note that there are several 
  typographical errors in the derivation and examples on page 575 of Gibbons (1987b) 
  because there is confusion between where the value of \eqn{\beta} (the coverage) 
  should be and where the value of \eqn{1-\alpha} (the confidence level) should be.  
  Gibbons et al. (2009, pp.103-104) gives correct formulas.
}
\seealso{
  \code{\link{Poisson}}, \code{\link{epois}}, \code{\link{eqpois}}, 
  \code{\link{estimate.object}}, \link{Tolerance Intervals}, 
  \link{Estimating Distribution Parameters}, \link{Estimating Distribution Quantiles}.
}
\examples{
  # Generate 20 observations from a Poisson distribution with parameter 
  # lambda=2. The interval [0, 4] contains 94.7\% of this distribution and 
  # the interval [0,5] contains 98.3\% of this distribution.  Thus, because 
  # of the discrete nature of the Poisson distribution, no interval contains 
  # exactly 95\% of this distribution.  Use tolIntPois to estimate the mean 
  # parameter of the true distribution, and construct a one-sided upper 95\% 
  # beta-content tolerance interval with associated confidence level 90\%. 
  # (Note: the call to set.seed simply allows you to reproduce this example.)

  set.seed(250) 
  dat <- rpois(20, 2) 
  tolIntPois(dat, conf.level = 0.9)

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            Poisson
  #
  #Estimated Parameter(s):          lambda = 1.8
  #
  #Estimation Method:               mle/mme/mvue
  #
  #Data:                            dat
  #
  #Sample Size:                     20
  #
  #Tolerance Interval Coverage:     95%
  #
  #Coverage Type:                   content
  #
  #Tolerance Interval Method:       Zacks
  #
  #Tolerance Interval Type:         two-sided
  #
  #Confidence Level:                90%
  #
  #Tolerance Interval:              LTL = 0
  #                                 UTL = 6

  #------

  # Clean up
  rm(dat)
}
\keyword{ distribution }
\keyword{ htest }
