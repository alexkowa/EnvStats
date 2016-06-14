\name{predIntLnormAltSimultaneousTestPower}
\alias{predIntLnormAltSimultaneousTestPower}
\title{
  Probability That at Least One Set of Future Observations Violates the Given Rule Based on a Simultaneous Prediction Interval for a Lognormal Distribution
}
\description{
  Compute the probability that at least one set of future observations violates the 
  given rule based on a simultaneous prediction interval for the next \eqn{r} future 
  sampling occasions for a \link[=LognormalAlt]{lognormal distribution}.  The 
  three possible rules are: \eqn{k}-of-\eqn{m}, California, or Modified California.
}
\usage{
  predIntLnormAltSimultaneousTestPower(n, df = n - 1, n.geomean = 1, k = 1, 
    m = 2, r = 1, rule = "k.of.m", ratio.of.means = 1, cv = 1, pi.type = "upper", 
    conf.level = 0.95, r.shifted = r, K.tol = .Machine$double.eps^0.5, 
    integrate.args.list = NULL)
}
\arguments{
  \item{n}{
  vector of positive integers greater than 2 indicating the sample size upon which 
  the prediction interval is based.
}
  \item{df}{
  vector of positive integers indicating the degrees of freedom associated with 
  the sample size.  The default value is \code{df=n-1}.
}
  \item{n.geomean}{
  positive integer specifying the sample size associated with the future geometric 
  means.  
  The default value is \code{n.geomean=1} (i.e., individual observations).  Note 
  that all future geometric means must be based on the same sample size.
}
  \item{k}{
  for the \eqn{k}-of-\eqn{m} rule (\code{rule="k.of.m"}), vector of positive integers 
  specifying the minimum number of observations (or averages) out of \eqn{m} 
  observations (or averages) (all obtained on one future sampling \dQuote{occassion}) 
  the prediction interval should contain with confidence level \code{conf.level}.  
  The default value is \code{k=1}.  This argument is ignored when the argument 
  \code{rule} is not equal to \code{"k.of.m"}.
}
  \item{m}{
  vector of positive integers specifying the maximum number of future observations (or 
  averages) on one future sampling \dQuote{occasion}.  
  The default value is \code{m=2}, except when \code{rule="Modified.CA"}, in which 
  case this argument is ignored and \code{m} is automatically set equal to \code{4}.
}
  \item{r}{
  vector of positive integers specifying the number of future sampling \dQuote{occasions}.  
  The default value is \code{r=1}.
}
  \item{rule}{
  character string specifying which rule to use.  The possible values are 
  \code{"k.of.m"} (\eqn{k}-of-\eqn{m} rule; the default), \code{"CA"} (California rule), 
  and \code{"Modified.CA"} (modified California rule).  
  See the DETAILS section below for more information.
}
  \item{ratio.of.means}{
  numeric vector specifying the ratio of the mean of the population that will be 
  sampled to produce the future observations vs. the mean of the population that 
  was sampled to construct the prediction interval.  See the DETAILS section below 
  for more information.  The default value is \code{ratio.of.means=1}.
}
  \item{cv}{
  numeric vector of positive values specifying the coefficient of variation for 
  both the population that was sampled to construct the prediction interval \bold{and} 
  the population that will be sampled to produce the future observations.  The 
  default value is \code{cv=1}.
}
  \item{pi.type}{
  character string indicating what kind of prediction interval to compute.  
  The possible values are \code{pi.type="upper"} (the default), and 
  \code{pi.type="lower"}.
}
  \item{conf.level}{
  vector of values between 0 and 1 indicating the confidence level of the prediction interval.  
  The default value is \code{conf.level=0.95}.
}
  \item{r.shifted}{
  vector of positive integers specifying the number of future sampling occasions for 
  which the mean is shifted.  All values must be integeters 
  between \code{1} and the corresponding element of \code{r}.  The default value is 
  \code{r.shifted=r}.
}
  \item{K.tol}{
  numeric scalar indicating the tolerance to use in the nonlinear search algorithm to 
  compute \eqn{K}.  The default value is \code{K.tol=.Machine$double.eps^(1/2)}.  
  For many applications, the value of \eqn{K} needs to be known only to the second 
  decimal place, in which case setting \code{K.tol=1e-4} will speed up computation a 
  bit.
}
  \item{integrate.args.list}{
  a list of arguments to supply to the \code{\link{integrate}} function.  The 
  default value is \code{integrate.args.list=NULL} which means that the 
  default values of \code{\link{integrate}} are used.
}
}
\details{
  \emph{What is a Simultaneous Prediction Interval?} \cr
  A prediction interval for some population is an interval on the real line constructed 
  so that it will contain \eqn{k} future observations from that population 
  with some specified probability \eqn{(1-\alpha)100\%}, where 
  \eqn{0 < \alpha < 1} and \eqn{k} is some pre-specified positive integer.  
  The quantity \eqn{(1-\alpha)100\%} is called  
  the confidence coefficient or confidence level associated with the prediction 
  interval.  The function \code{\link{predIntNorm}} computes a standard prediction 
  interval based on a sample from a \link[stats:Normal]{normal distribution}.

  The function \code{\link{predIntLnormAltSimultaneous}} computes a simultaneous 
  prediction interval (assuming lognormal observations) that will contain a 
  certain number of future observations 
  with probability \eqn{(1-\alpha)100\%} for each of \eqn{r} future sampling 
  \dQuote{occasions}, where \eqn{r} is some pre-specified positive integer.  
  The quantity \eqn{r} may refer to \eqn{r} distinct future sampling occasions in 
  time, or it may for example refer to sampling at \eqn{r} distinct locations on 
  one future sampling occasion, 
  assuming that the population standard deviation is the same at all of the \eqn{r} 
  distinct locations.

  The function \code{\link{predIntLnormAltSimultaneous}} computes a simultaneous 
  prediction interval based on one of three possible rules:
  \itemize{
  \item For the \eqn{k}-of-\eqn{m} rule (\code{rule="k.of.m"}), at least \eqn{k} of 
    the next \eqn{m} future observations will fall in the prediction 
    interval with probability \eqn{(1-\alpha)100\%} on each of the \eqn{r} future 
    sampling occasions.  If obserations are being taken sequentially, for a particular 
    sampling occasion, up to \eqn{m} observations may be taken, but once 
    \eqn{k} of the observations fall within the prediction interval, sampling can stop.  
    Note:  When \eqn{k=m} and \eqn{r=1}, the results of \code{predIntNormSimultaneous} 
    are equivalent to the results of \code{\link{predIntNorm}}.

  \item For the California rule (\code{rule="CA"}), with probability 
    \eqn{(1-\alpha)100\%}, for each of the \eqn{r} future sampling occasions, either 
    the first observation will fall in the prediction interval, or else all of the next 
    \eqn{m-1} observations will fall in the prediction interval. That is, if the first 
    observation falls in the prediction interval then sampling can stop.  Otherwise, 
    \eqn{m-1} more observations must be taken.

  \item For the Modified California rule (\code{rule="Modified.CA"}), with probability 
    \eqn{(1-\alpha)100\%}, for each of the \eqn{r} future sampling occasions, either the 
    first observation will fall in the prediction interval, or else at least 2 out of 
    the next 3 observations will fall in the prediction interval.  That is, if the first 
    observation falls in the prediction interval then sampling can stop.  Otherwise, up 
    to 3 more observations must be taken.
  }

  \emph{Computing Power} \cr
  The function \code{\link{predIntNormSimultaneousTestPower}} computes the 
  probability that at least one set of future observations or averages will 
  violate the given rule based on a simultaneous prediction interval for the 
  next \eqn{r} future sampling occasions for a normal distribution, 
  based on the assumption of normally distributed observations, 
  where the population mean for the future observations is allowed to differ from 
  the population mean for the observations used to construct the prediction interval.

  The function \code{predIntLnormAltSimultaneousTestPower} assumes all observations are 
  from a \link[=LognormalAlt]{lognormal distribution}.  The observations used to 
  construct the prediction interval are assumed to come from a lognormal distribution 
  with mean \eqn{\theta_2} and coefficient of variation \eqn{\tau}.  The future 
  observations are assumed to come from a lognormal distribution with mean 
  \eqn{\theta_1} and coefficient of variation \eqn{\tau}; that is, the means are 
  allowed to differ between the two populations, but not the coefficient of variation.

  The function \code{predIntLnormAltSimultaneousTestPower} calls the function \cr
  \code{\link{predIntNormSimultaneousTestPower}}, with the argument 
  \code{delta.over.sigma} given by:
  \deqn{\frac{\delta}{\sigma} = \frac{log(R)}{\sqrt{log(\tau^2 + 1)}} \;\;\;\;\;\; (1)}
  where \eqn{R} is given by:
  \deqn{R = \frac{\theta_1}{\theta_2} \;\;\;\;\;\; (2)}
  and corresponds to the argument \code{ratio.of.means} for the function \cr
  \code{predIntLnormAltSimultaneousTestPower}, and \eqn{\tau} corresponds to the 
  argument \code{cv}.
}
\value{
  vector of values between 0 and 1 equal to the probability that 
  the rule will be violated.
}
\references{
  See the help file for \code{\link{predIntLnormAltSimultaneous}}.
}
\author{
    Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  See the help files for \code{\link{predIntLnormAltSimultaneous}} and 
  \code{\link{predIntNormSimultaneousTestPower}}.
}
\seealso{
  \code{\link{predIntLnormAltSimultaneous}}, \cr
  \code{\link{plotPredIntLnormAltSimultaneousTestPowerCurve}}, \cr
  \code{\link{predIntNormSimultaneous}}, \cr
  \code{\link{plotPredIntNormSimultaneousTestPowerCurve}}, \cr
  \link{Prediction Intervals}, \link{LognormalAlt}.
}
\examples{
  # For the k-of-m rule with n=4, k=1, m=3, and r=1, show how the power increases 
  # as ratio.of.means increases.  Assume a 95% upper prediction interval.

  predIntLnormAltSimultaneousTestPower(n = 4, m = 3, ratio.of.means = 1:3) 
  #[1] 0.0500000 0.2356914 0.4236723

  #----------

  # Look at how the power increases with sample size for an upper one-sided 
  # prediction interval using the k-of-m rule with k=1, m=3, r=20, 
  # ratio.of.means=4, and a confidence level of 95%.

  predIntLnormAltSimultaneousTestPower(n = c(4, 8), m = 3, r = 20, ratio.of.means = 4) 
  #[1] 0.4915743 0.8218175

  #----------

  # Compare the power for the 1-of-3 rule with the power for the California and 
  # Modified California rules, based on a 95% upper prediction interval and 
  # ratio.of.means=4.  Assume a sample size of n=8.  Note that in this case the 
  # power for the Modified California rule is greater than the power for the 
  # 1-of-3 rule and California rule.

  predIntLnormAltSimultaneousTestPower(n = 8, k = 1, m = 3, ratio.of.means = 4) 
  #[1] 0.6594845 

  predIntLnormAltSimultaneousTestPower(n = 8, m = 3, rule = "CA", ratio.of.means = 4) 
  #[1] 0.5864311 

  predIntLnormAltSimultaneousTestPower(n = 8, rule = "Modified.CA", ratio.of.means = 4) 
  #[1] 0.691135

  #----------

  # Show how the power for an upper 95% simultaneous prediction limit increases 
  # as the number of future sampling occasions r increases.  Here, we'll use the 
  # 1-of-3 rule with n=8 and ratio.of.means=4.

  predIntLnormAltSimultaneousTestPower(n = 8, k = 1, m = 3, r = c(1, 2, 5, 10), 
    ratio.of.means = 4) 
  #[1] 0.6594845 0.7529576 0.8180814 0.8302302
}
\keyword{ distribution }
\keyword{ htest }
