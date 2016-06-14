\name{predIntNormSimultaneousTestPower}
\alias{predIntNormSimultaneousTestPower}
\title{
  Probability That at Least One Set of Future Observations Violates the Given Rule Based on a Simultaneous Prediction Interval for a Normal Distribution
}
\description{
  Compute the probability that at least one set of future observations violates the 
  given rule based on a simultaneous prediction interval for the next \eqn{r} future 
  sampling occasions for a normal distribution.  The three possible rules are: 
  \eqn{k}-of-\eqn{m}, California, or Modified California.
}
\usage{
  predIntNormSimultaneousTestPower(n, df = n - 1, n.mean = 1, k = 1, m = 2, r = 1, 
    rule = "k.of.m", delta.over.sigma = 0, pi.type = "upper", conf.level = 0.95, 
    r.shifted = r, K.tol = .Machine$double.eps^0.5, integrate.args.list = NULL)
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
  \item{n.mean}{
  positive integer specifying the sample size associated with the future averages.  
  The default value is \code{n.mean=1} (i.e., individual observations).  Note that all 
  future averages must be based on the same sample size.
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
  \item{delta.over.sigma}{
  numeric vector indicating the ratio \eqn{\Delta/\sigma}.  The quantity 
  \eqn{\Delta} (delta) denotes the difference between the mean of the population 
  that was sampled to construct the prediction interval, and the mean of the 
  population that will be sampled to produce the future observations.  The quantity 
  \eqn{\sigma} (sigma) denotes the population standard deviation for both populations.  
  See the DETAILS section below for more information.  The default value is 
  \code{delta.over.sigma=0}.
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
  which the scaled mean is shifted by \eqn{\Delta/\sigma}.  All values must be 
  integeters between \code{1} and the corresponding element of \code{r}.  
  The default value is \code{r.shifted=r}.
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
  \bold{What is a Simultaneous Prediction Interval?} \cr
  A prediction interval for some population is an interval on the real line constructed 
  so that it will contain \eqn{k} future observations from that population 
  with some specified probability \eqn{(1-\alpha)100\%}, where 
  \eqn{0 < \alpha < 1} and \eqn{k} is some pre-specified positive integer.  
  The quantity \eqn{(1-\alpha)100\%} is called  
  the confidence coefficient or confidence level associated with the prediction 
  interval.  The function \code{\link{predIntNorm}} computes a standard prediction 
  interval based on a sample from a \link[stats:Normal]{normal distribution}.

  The function \code{\link{predIntNormSimultaneous}} computes a simultaneous prediction 
  interval that will contain a certain number of future observations with probability 
  \eqn{(1-\alpha)100\%} for each of \eqn{r} future sampling \dQuote{occasions}, 
  where \eqn{r} is some pre-specified positive integer.  The quantity \eqn{r} may 
  refer to \eqn{r} distinct future sampling occasions in time, or it may for example 
  refer to sampling at \eqn{r} distinct locations on one future sampling occasion, 
  assuming that the population standard deviation is the same at all of the \eqn{r} 
  distinct locations.

  The function \code{\link{predIntNormSimultaneous}} computes a simultaneous prediction 
  interval based on one of three possible rules:
  \itemize{
  \item For the \eqn{k}-of-\eqn{m} rule (\code{rule="k.of.m"}), at least \eqn{k} of 
    the next \eqn{m} future observations will fall in the prediction 
    interval with probability \eqn{(1-\alpha)100\%} on each of the \eqn{r} future 
    sampling occasions.  If obserations are being taken sequentially, for a particular 
    sampling occasion, up to \eqn{m} observations may be taken, but once 
    \eqn{k} of the observations fall within the prediction interval, sampling can stop.  
    Note:  When \eqn{k=m} and \eqn{r=1}, the results of 
    \code{\link{predIntNormSimultaneous}} are equivalent to the results of 
    \code{\link{predIntNorm}}.

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

  Simultaneous prediction intervals can be extended to using averages (means) in place 
  of single observations (USEPA, 2009, Chapter 19).  That is, you can create a 
  simultaneous prediction interval 
  that will contain a specified number of averages (based on which rule you choose) on 
  each of \eqn{r} future sampling occassions, where each each average is based on 
  \eqn{w} individual observations.  For the function \code{\link{predIntNormSimultaneous}}, 
  the argument \code{n.mean} corresponds to \eqn{w}.
  \cr
  
  \bold{The Form of a Prediction Interval} \cr
  Let \eqn{\underline{x} = x_1, x_2, \ldots, x_n} denote a vector of \eqn{n} 
  observations from a \link[stats:Normal]{normal distribution} with parameters 
  \code{mean=}\eqn{\mu} and \code{sd=}\eqn{\sigma}.  Also, let \eqn{w} denote the 
  sample size associated with the future averages (i.e., \code{n.mean=}\eqn{w}).  
  When \eqn{w=1}, each average is really just a single observation, so in the rest of 
  this help file the term \dQuote{averages} will replace the phrase 
  \dQuote{observations or averages}.

  For a normal distribution, the form of a two-sided \eqn{(1-\alpha)100\%} 
  prediction interval is: 
  \deqn{[\bar{x} - Ks, \bar{x} + Ks] \;\;\;\;\;\; (1)} 
  where \eqn{\bar{x}} denotes the sample mean:
  \deqn{\bar{x} = \frac{1}{n} \sum_{i=1}^n x_i \;\;\;\;\;\; (2)}
  \eqn{s} denotes the sample standard deviation:
  \deqn{s^2 = \frac{1}{n-1} \sum_{i=1}^n (x_i - \bar{x})^2 \;\;\;\;\;\; (3)}
  and \eqn{K} denotes a constant that depends on the sample size \eqn{n}, the 
  confidence level, the number of future sampling occassions \eqn{r}, and the 
  sample size associated with the future averages, \eqn{w}.  Do not confuse the 
  constant \eqn{K} (uppercase K) with the number of future averages \eqn{k} 
  (lowercase k) in the \eqn{k}-of-\eqn{m} rule.  The symbol \eqn{K} is used here 
  to be consistent with the notation used for tolerance intervals 
  (see \code{\link{tolIntNorm}}).  

  Similarly, the form of a one-sided lower prediction interval is: 
  \deqn{[\bar{x} - Ks, \infty] \;\;\;\;\;\; (4)} 
  and the form of a one-sided upper prediction interval is:
  \deqn{[-\infty, \bar{x} + Ks] \;\;\;\;\;\; (5)} 

  \bold{Note:}  For simultaneous prediction intervals, only lower 
  (\code{pi.type="lower"}) and upper \cr
  (\code{pi.type="upper"}) prediction 
  intervals are available.
  
  The derivation of the constant \eqn{K} is explained in the help file for 
  \code{\link{predIntNormSimultaneousK}}.
  \cr

  \bold{Computing Power} \cr
  The "power" of the prediction interval is defined as the probability that 
  at least one set of future observations violates the given rule based on a 
  simultaneous prediction interval for the next \eqn{r} future sampling occasions, 
  where the population mean for the future observations is allowed to differ from 
  the population mean for the observations used to construct the prediction interval.  

  The quantity \eqn{\Delta} (upper case delta) denotes the difference between the 
  mean of the population that was sampled to construct the prediction interval, and 
  the mean of the population that will be sampled to produce the future observations.  
  The quantity \eqn{\sigma} (sigma) denotes the population standard deviation of both 
  of these populations.  The argument \code{delta.over.sigma} corresponds to the 
  quantity \eqn{\Delta/\sigma}.
  \cr

  \emph{Power Based on the k-of-m Rule} (\code{rule="k.of.m"}) \cr
  For the \eqn{k}-of-\eqn{m} rule (\code{rule="k.of.m"}) with \eqn{w=1} 
  (i.e., \code{n.mean=1}), at least \eqn{k} of the next \eqn{m} future 
  observations will fall in the prediction interval 
  with probability \eqn{(1-\alpha)100\%} on each of the \eqn{r} future sampling 
  occasions.  If observations are being taken sequentially, for a particular 
  sampling occasion, up to \eqn{m} observations may be taken, but once \eqn{k} 
  of the observations fall within the prediction interval, sampling can stop.  
  Note: When \eqn{k=m} and \eqn{r=1}, this kind of simultaneous prediction 
  interval becomes the same as a standard prediction interval for the next 
  \eqn{k} observations (see \code{\link{predIntNorm}}).

  Davis and McNichols (1987) show that for a one-sided upper prediction interval 
  (\code{pi.type="upper"}), the probability \eqn{p} that at least \eqn{k} of the 
  next \eqn{m} future observations will be contained in the interval given in 
  Equation (5) above, for each of \eqn{r} 
  future sampling occasions, is given by:
  \deqn{p = \int_0^1 T(\sqrt{n}K; n-1, \sqrt{n}[\Phi^{-1}(v) + \frac{\Delta}{\sigma}]) r[I(v; k, m+1-k)]^{r-1} [\frac{v^{k-1}(1-v)^{m-k}}{B(k, m+1-k)}] dv \;\;\;\;\;\; (6)}
  where \eqn{T(x; \nu, \delta)} denotes the cdf of the 
  \link[stats:TDist]{non-central Student's t-distribution} with parameters 
  \code{df=}\eqn{\nu} and \code{ncp=}\eqn{\delta} evaluated at \eqn{x}; 
  \eqn{\Phi(x)} denotes the cdf of the standard \link[stats:Normal]{normal distribution} 
  evaluated at \eqn{x}; \eqn{I(x; \nu, \omega)} denotes the cdf of the 
  \link[stats:Beta]{beta distribution} with parameters \code{shape1=}\eqn{\nu} and 
  \code{shape2=}\eqn{\omega}; and \eqn{B(\nu, \omega)} denotes the value of the 
  \link[base:Special]{beta function} with parameters \code{a=}\eqn{\nu} and 
  \code{b=}\eqn{\omega}.

  The quantity \eqn{\Delta} (upper case delta) denotes the difference between the 
  mean of the population that was sampled to construct the prediction interval, and 
  the mean of the population that will be sampled to produce the future observations.  
  The quantity \eqn{\sigma} (sigma) denotes the population standard deviation of both 
  of these populations.  Usually you assume \eqn{\Delta=0} unless you are interested 
  in computing the power of the rule to detect a change in means between the 
  populations, as we are here.

  If we are interested in using averages instead of single observations, with 
  \eqn{w \ge 1} (i.e., \code{n.mean}\eqn{\ge 1}), the first 
  term in the integral in Equation (6) that involves the cdf of the 
  \link[stats:TDist]{non-central Student's t-distribution} becomes:
  \deqn{T(\sqrt{n}K; n-1, \frac{\sqrt{n}}{\sqrt{w}}[\Phi^{-1}(v) + \frac{\sqrt{w}\Delta}{\sigma}]) \;\;\;\;\;\; (7)}
  
  For a given confidence level \eqn{(1-\alpha)100\%}, the power of the rule to detect 
  a change in means is simply given by:
  \deqn{Power = 1 - p \;\;\;\;\;\; (8)}
  where \eqn{p} is defined in Equation (6) above using the value of \eqn{K} that 
  corresponds to \eqn{\Delta/\sigma = 0}.  Thus, when the argument 
  \code{delta.over.sigma=0}, the value of \eqn{p} is \eqn{1-\alpha} and the power is 
  simply \eqn{\alpha 100\%}.  As \code{delta.over.sigma} increases above 0, the power 
  increases.

  When \code{pi.type="lower"}, the same value of \code{K} is used as when 
  \code{pi.type="upper"}, but Equation (4) is used to construct the prediction 
  interval.  Thus, the power increases as \code{delta.over.sigma} decreases below 0.
  \cr

  \emph{Power Based on the California Rule} (\code{rule="CA"}) \cr  
  For the California rule (\code{rule="CA"}), with probability \eqn{(1-\alpha)100\%}, 
  for each of the \eqn{r} future sampling occasions, either the first observation will 
  fall in the prediction interval, or else all of the next \eqn{m-1} observations will 
  fall in the prediction interval.  That is, if the first observation falls in the 
  prediction interval then sampling can stop.  Otherwise, \eqn{m-1} more observations 
  must be taken.

  The derivation of the power is the same as for the \eqn{k}-of-\eqn{m} rule, except 
  that Equation (6) becomes the following (Davis, 1998b):
  \deqn{p = \int_0^1 T(\sqrt{n}K; n-1, \sqrt{n}[\Phi^{-1}(v) + \frac{\Delta}{\sigma}]) r\{v[1+v^{m-2}(1-v)]\}^{r-1} [1+v^{m-2}(m-1-mv)] dv \;\;\;\;\;\; (9)}
  \cr

  \emph{Power Based on the Modified California Rule} (\code{rule="Modified.CA"}) \cr 
  For the Modified California rule (\code{rule="Modified.CA"}), with probability 
  \eqn{(1-\alpha)100\%}, for each of the \eqn{r} future sampling occasions, either the 
  first observation will fall in the prediction interval, or else at least 2 out of 
  the next 3 observations will fall in the prediction interval.  That is, if the first 
  observation falls in the prediction interval then sampling can stop.  Otherwise, up 
  to 3 more observations must be taken.

  The derivation of the power is the same as for the \eqn{k}-of-\eqn{m} rule, except 
  that Equation (6) becomes the following (Davis, 1998b):
  \deqn{p = \int_0^1 T(\sqrt{n}K; n-1, \sqrt{n}[\Phi^{-1}(v) + \frac{\Delta}{\sigma}]) r\{v[1+v(3-v[5-2v])]\}^{r-1} \{1+v[6-v(15-8v)]\} dv \;\;\;\;\;\; (10)}
}
\value{
  vector of values between 0 and 1 equal to the probability that 
  the rule will be violated.
}
\references{
  See the help file for \code{\link{predIntNormSimultaneous}}.
}
\author{
    Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  See the help file for \code{\link{predIntNormSimultaneous}}.

  In the course of designing a sampling program, an environmental scientist may wish 
  to determine the relationship between sample size, significance level, power, and 
  scaled difference if one of the objectives of the sampling program is to determine 
  whether two distributions differ from each other.  The functions 
  \code{predIntNormSimultaneousTestPower} and \cr
  \code{\link{plotPredIntNormSimultaneousTestPowerCurve}} can be 
  used to investigate these relationships for the case of normally-distributed 
  observations. 
}
\seealso{
  \code{\link{predIntNormSimultaneous}}, \code{\link{predIntNormSimultaneousK}}, \cr
  \code{\link{plotPredIntNormSimultaneousTestPowerCurve}}, 
  \code{\link{predIntNorm}}, \code{\link{predIntNormK}}, \cr
  \code{\link{predIntNormTestPower}}, \link{Prediction Intervals}, 
  \link{Normal}.
}
\examples{
  # For the k-of-m rule with n=4, k=1, m=3, and r=1, show how the power increases 
  # as delta.over.sigma increases. Assume a 95% upper prediction interval.

  predIntNormSimultaneousTestPower(n = 4, m = 3, delta.over.sigma = 0:2) 
  #[1] 0.0500000 0.2954156 0.7008558

  #----------

  # Look at how the power increases with sample size for an upper one-sided 
  # prediction interval using the k-of-m rule with k=1, m=3, r=20, 
  # delta.over.sigma=2, and a confidence level of 95%.

  predIntNormSimultaneousTestPower(n = c(4, 8), m = 3, r = 20, delta.over.sigma = 2) 
  #[1] 0.6075972 0.9240924

  #----------

  # Compare the power for the 1-of-3 rule with the power for the California and 
  # Modified California rules, based on a 95% upper prediction interval and 
  # delta.over.sigma=2.  Assume a sample size of n=8.  Note that in this case the 
  # power for the Modified California rule is greater than the power for the 
  # 1-of-3 rule and California rule.

  predIntNormSimultaneousTestPower(n = 8, k = 1, m = 3, delta.over.sigma = 2) 
  #[1] 0.788171 

  predIntNormSimultaneousTestPower(n = 8, m = 3, rule = "CA", delta.over.sigma = 2) 
  #[1] 0.7160434 

  predIntNormSimultaneousTestPower(n = 8, rule = "Modified.CA", delta.over.sigma = 2) 
  #[1] 0.8143687

  #----------

  # Show how the power for an upper 95% simultaneous prediction limit increases 
  # as the number of future sampling occasions r increases.  Here, we'll use the 
  # 1-of-3 rule with n=8 and delta.over.sigma=1.

  predIntNormSimultaneousTestPower(n = 8, k = 1, m = 3, r=c(1, 2, 5, 10), 
    delta.over.sigma = 1) 
  #[1] 0.3492512 0.4032111 0.4503603 0.4633773

  #==========

  # USEPA (2009) contains an example on page 19-23 that involves monitoring 
  # nw=100 compliance wells at a large facility with minimal natural spatial 
  # variation every 6 months for nc=20 separate chemicals.  
  # There are n=25 background measurements for each chemical to use to create
  # simultaneous prediction intervals.  We would like to determine which kind of
  # resampling plan based on normal distribution simultaneous prediction intervals to
  # use (1-of-m, 1-of-m based on means, or Modified California) in order to have
  # adequate power of detecting an increase in chemical concentration at any of the
  # 100 wells while at the same time maintaining a site-wide false positive rate
  # (SWFPR) of 10% per year over all 4,000 comparisons 
  # (100 wells x 20 chemicals x semi-annual sampling).

  # The function predIntNormSimultaneousTestPower includes the argument "r" 
  # that is the number of future sampling occasions (r=2 in this case because 
  # we are performing semi-annual sampling), so to compute the individual test 
  # Type I error level alpha.test (and thus the individual test confidence level), 
  # we only need to worry about the number of wells (100) and the number of 
  # constituents (20): alpha.test = 1-(1-alpha)^(1/(nw x nc)).  The individual 
  # confidence level is simply 1-alpha.test.  Plugging in 0.1 for alpha, 
  # 100 for nw, and 20 for nc yields an individual test confidence level of 
  # 1-alpha.test = 0.9999473.

  nc <- 20
  nw <- 100
  conf.level <- (1 - 0.1)^(1 / (nc * nw))
  conf.level
  #[1] 0.9999473

  # Now we can compute the power of any particular sampling strategy using
  # predIntNormSimultaneousTestPower.  For example, here is the power of 
  # detecting an increase of three standard deviations in concentration using 
  # the prediction interval based on the "1-of-2" resampling rule:

  predIntNormSimultaneousTestPower(n = 25, k = 1, m = 2, r = 2, rule = "k.of.m", 
    delta.over.sigma = 3,  pi.type = "upper", conf.level = conf.level)
  #[1] 0.3900202

  # The following commands will reproduce the table shown in Step 2 on page
  # 19-23 of USEPA (2009).  Because these commands can take more than a few 
  # seconds to execute, we have commented them out here.  To run this example, 
  # just remove the pound signs (#) that are in front of R commands.

  #rule.vec <- c(rep("k.of.m", 3), "Modified.CA",  rep("k.of.m", 3))

  #m.vec <- c(2, 3, 4, 4, 1, 2, 1)

  #n.mean.vec <- c(rep(1, 4), 2, 2, 3)

  #n.scenarios <- length(rule.vec)

  #K.vec <- numeric(n.scenarios)

  #Power.vec <- numeric(n.scenarios)

  #K.vec <- predIntNormSimultaneousK(n = 25, k = 1, m = m.vec,  n.mean = n.mean.vec, 
  #  r = 2, rule = rule.vec,  pi.type = "upper", conf.level = conf.level)

  #Power.vec <- predIntNormSimultaneousTestPower(n = 25, k = 1, m = m.vec, 
  #  n.mean = n.mean.vec, r = 2, rule = rule.vec,  delta.over.sigma = 3, 
  #  pi.type = "upper",  conf.level = conf.level)

  #Power.df <- data.frame(Rule = rule.vec, k = rep(1, n.scenarios),  m = m.vec, 
  #  N.Mean = n.mean.vec, K = round(K.vec, 2),  Power = round(Power.vec, 2),
  #  Total.Samples = m.vec * n.mean.vec)

  #Power.df

  #         Rule k m N.Mean    K Power Total.Samples
  #1      k.of.m 1 2      1 3.16  0.39             2
  #2      k.of.m 1 3      1 2.33  0.65             3
  #3      k.of.m 1 4      1 1.83  0.81             4
  #4 Modified.CA 1 4      1 2.57  0.71             4
  #5      k.of.m 1 1      2 3.62  0.41             2
  #6      k.of.m 1 2      2 2.33  0.85             4
  #7      k.of.m 1 1      3 2.99  0.71             3

  # The above table shows the K-multipliers for each prediction interval, along with
  # the power of detecting a change in concentration of three standard deviations at
  # any of the 100 wells during the course of a year, for each of the sampling
  # strategies considered.  The last three rows of the table correspond to sampling
  # strategies that involve using the mean of two or three observations.

  #==========

  # Clean up
  #---------
  rm(nc, nw, conf.level, rule.vec, m.vec, n.mean.vec, n.scenarios, K.vec, 
    Power.vec, Power.df)
}
\keyword{ distribution }
\keyword{ design }
\keyword{ htest }
