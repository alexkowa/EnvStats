\name{predIntNormSimultaneousK}
\alias{predIntNormSimultaneousK}
\title{
  Compute the Value of \eqn{K} for a Simultaneous Prediction Interval for a Normal Distribution
}
\description{
  Compute the value of \eqn{K} (the multiplier of estimated standard deviation) used
  to construct a simultaneous prediction interval based on data from a
  \link[stats:Normal]{normal distribution}.
  The function \cr
  \code{predIntNormSimultaneousK} is called by \code{\link{predIntNormSimultaneous}}.
}
\usage{
  predIntNormSimultaneousK(n, df = n - 1, n.mean = 1, k = 1, m = 2, r = 1,
    rule = "k.of.m", delta.over.sigma = 0, pi.type = "upper", conf.level = 0.95,
    K.tol = .Machine$double.eps^0.5, integrate.args.list = NULL)
}
\arguments{
  \item{n}{
  a positive integer greater than 2 indicating the sample size upon which the
  prediction interval is based.
}
  \item{df}{
  the degrees of freedom associated with the prediction interval.  The default is
  \code{df=n-1}.
}
  \item{n.mean}{
  positive integer specifying the sample size associated with the future averages.
  The default value is \code{n.mean=1} (i.e., individual observations).  Note that all
  future averages must be based on the same sample size.
}
  \item{k}{
  for the \eqn{k}-of-\eqn{m} rule (\code{rule="k.of.m"}), a positive integer
  specifying the minimum number of observations (or averages) out of \eqn{m}
  observations (or averages) (all obtained on one future sampling \dQuote{occassion})
  the prediction interval should contain with confidence level \code{conf.level}.
  The default value is \code{k=1}.  This argument is ignored when the argument
  \code{rule} is not equal to \code{"k.of.m"}.
}
  \item{m}{
  positive integer specifying the maximum number of future observations (or
  averages) on one future sampling \dQuote{occasion}.
  The default value is \code{m=2}, except when \code{rule="Modified.CA"}, in which
  case this argument is ignored and \code{m} is automatically set equal to \code{4}.
}
  \item{r}{
  positive integer specifying the number of future sampling \dQuote{occasions}.
  The default value is \code{r=1}.
}
  \item{rule}{
  character string specifying which rule to use.  The possible values are
  \code{"k.of.m"} (\eqn{k}-of-\eqn{m} rule; the default), \code{"CA"} (California rule),
  and \code{"Modified.CA"} (modified California rule).
  See the DETAILS section below for more information.
}
  \item{delta.over.sigma}{
  numeric scalar indicating the ratio \eqn{\Delta/\sigma}.  The quantity
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
  \code{pi.type="lower"}.  \bold{NOTE:} In Versions 2.4.0 - 2.8.1 of \emph{EnvStats}, 
  the value \code{pi.type="two-sided"} was allowed, but these two-sided simultaneous 
  prediction intervals were based on faulty assumptions and were \bold{NOT} valid.

}
  \item{conf.level}{
  a scalar between 0 and 1 indicating the confidence level of the prediction interval.
  The default value is \code{conf.level=0.95}.
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

  The function \code{predIntNormSimultaneous} computes a simultaneous prediction
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

  Simultaneous prediction intervals can be extended to using averages (means) in place
  of single observations (USEPA, 2009, Chapter 19).  That is, you can create a
  simultaneous prediction interval
  that will contain a specified number of averages (based on which rule you choose) on
  each of \eqn{r} future sampling occassions, where each each average is based on
  \eqn{w} individual observations.  For the functions
  \code{\link{predIntNormSimultaneous}} and \code{predIntNormSimultaneousK},
  the argument \code{n.mean} corresponds to \eqn{w}.
  \cr

  \emph{The Form of a Prediction Interval for 1 Future Observation} \cr
  Let \eqn{\underline{x} = x_1, x_2, \ldots, x_n} denote a vector of \eqn{n}
  observations from a \link[stats:Normal]{normal distribution} with parameters
  \code{mean=}\eqn{\mu} and \code{sd=}\eqn{\sigma}.  Also, let \eqn{w} denote the
  sample size associated with the future averages (i.e., \code{n.mean=}\eqn{w}).
  When \eqn{w=1}, each average is really just a single observation, so in the rest of
  this help file the term \dQuote{averages} will sometimes replace the phrase
  \dQuote{observations or averages}.

  For a normal distribution, the form of a two-sided \eqn{(1-\alpha)100\%}
  simultaneous prediction interval is:
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

  The derivation of the constant \eqn{K} for 1 future observation is 
  explained in the help file for \code{\link{predIntNormK}}.
  \cr

  \emph{The Form of a Simultaneous Prediction Interval} \cr
  For \bold{simultaneous} prediction intervals, only lower 
  (\code{pi.type="lower"}) and upper (\code{pi.type="upper"}) prediction
  intervals are available.  Two-sided simultaneous prediction intervals were 
  available in Versions 2.4.0 - 2.8.1 of \emph{EnvStats}, but these prediction 
  intervals were based on an incorrect algorithm for \eqn{K}.

  Equations (4) and (5) above hold for simultaneous prediction intervals, but the 
  derivation of the constant \eqn{K} is more difficult, and is explained below.
  \cr

  \bold{The Derivation of K for Future Observations} \cr
  First we will show the derivation based on future observations (i.e.,
  \eqn{w=1}, \code{n.mean=1}), and then extend the formulas to future averages.

  \emph{The Derivation of K for the k-of-m Rule} (\code{rule="k.of.m"}) \cr
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

  For the case when \eqn{r=1} future sampling occasion, both Hall and Prairie (1973)
  and Fertig and Mann (1977) discuss the derivation of \eqn{K}.  Davis and McNichols
  (1987) extend the derivation to the case where \eqn{r} is a positive integer.  They
  show that for a one-sided upper prediction interval (\code{pi.type="upper"}), the
  probability \eqn{p} that at least \eqn{k} of the next \eqn{m} future observations
  will be contained in the interval given in Equation (5) above, for each of \eqn{r}
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
  populations (see \code{\link{predIntNormSimultaneousTestPower}}).

  For given values of the confidence level (\eqn{p}), sample size (\eqn{n}),
  minimum number of future observations to be contained in the interval per
  sampling occasion (\eqn{k}), number of future observations per sampling occasion
  (\eqn{m}), and number of future sampling occasions (\eqn{r}), Equation (6) can
  be solved for \eqn{K}.  The function \code{predIntNormSimultaneousK} uses the
  \R function \code{\link{nlminb}} to solve Equation (6) for \eqn{K}.

  When \code{pi.type="lower"}, the same value of \eqn{K} is used as when
  \code{pi.type="upper"}, but Equation (4) is used to construct the prediction
  interval.
  \cr

  \emph{The Derivation of K for the California Rule} (\code{rule="CA"}) \cr
  For the California rule (\code{rule="CA"}), with probability \eqn{(1-\alpha)100\%},
  for each of the \eqn{r} future sampling occasions, either the first observation will
  fall in the prediction interval, or else all of the next \eqn{m-1} observations will
  fall in the prediction interval.  That is, if the first observation falls in the
  prediction interval then sampling can stop.  Otherwise, \eqn{m-1} more observations
  must be taken.

  The formula for \eqn{K} is the same as for the \eqn{k}-of-\eqn{m} rule, except that
  Equation (6) becomes the following (Davis, 1998b):
  \deqn{p = \int_0^1 T(\sqrt{n}K; n-1, \sqrt{n}[\Phi^{-1}(v) + \frac{\Delta}{\sigma}]) r\{v[1+v^{m-2}(1-v)]\}^{r-1} [1+v^{m-2}(m-1-mv)] dv \;\;\;\;\;\; (7)}
  \cr

  \emph{The Derivation of K for the Modified California Rule} (\code{rule="Modified.CA"}) \cr
  For the Modified California rule (\code{rule="Modified.CA"}), with probability
  \eqn{(1-\alpha)100\%}, for each of the \eqn{r} future sampling occasions, either the
  first observation will fall in the prediction interval, or else at least 2 out of
  the next 3 observations will fall in the prediction interval.  That is, if the first
  observation falls in the prediction interval then sampling can stop.  Otherwise, up
  to 3 more observations must be taken.

  The formula for \eqn{K} is the same as for the \eqn{k}-of-\eqn{m} rule, except that
  Equation (6) becomes the following (Davis, 1998b):
  \deqn{p = \int_0^1 T(\sqrt{n}K; n-1, \sqrt{n}[\Phi^{-1}(v) + \frac{\Delta}{\sigma}]) r\{v[1+v(3-v[5-2v])]\}^{r-1} \{1+v[6-v(15-8v)]\} dv \;\;\;\;\;\; (8)}
  \cr

  \bold{The Derivation of K for Future Means} \cr
  For each of the above rules, if we are interested in using averages instead of
  single observations, with \eqn{w \ge 1} (i.e., \code{n.mean}\eqn{\ge 1}), the first
  term in the integral in Equations (6)-(8) that involves the cdf of the
  \link[stats:TDist]{non-central Student's t-distribution} becomes:
  \deqn{T(\sqrt{n}K; n-1, \frac{\sqrt{n}}{\sqrt{w}}[\Phi^{-1}(v) + \frac{\sqrt{w}\Delta}{\sigma}]) \;\;\;\;\;\; (9)}
}
\value{
  A numeric scalar equal to \eqn{K}, the multiplier of estimated standard
  deviation that is used to construct the simultaneous prediction interval.
}
\references{
  \bold{Barclay's California Code of Regulations}. (1991). Title 22,
  Section 66264.97 [concerning hazardous waste facilities] and Title 23,
  Section 2550.7(e)(8) [concerning solid waste facilities].
  Barclay's Law Publishers, San Francisco, CA.

  Davis, C.B. (1998a).  \emph{Ground-Water Statistics & Regulations:  Principles,
  Progress and Problems}.  Second Edition.  Environmetrics & Statistics Limited,
  Henderson, NV.

  Davis, C.B. (1998b). Personal Communication, September 3, 1998.

  Davis, C.B., and R.J. McNichols. (1987).  One-sided Intervals for at Least \eqn{p}
  of \eqn{m} Observations from a Normal Population on Each of \eqn{r} Future Occasions.
  \emph{Technometrics} \bold{29}, 359--370.

  Fertig, K.W., and N.R. Mann. (1977).  One-Sided Prediction Intervals for at Least
  \eqn{p} Out of \eqn{m} Future Observations From a Normal Population.
  \emph{Technometrics} \bold{19}, 167--177.

  Gibbons, R.D., D.K. Bhaumik, and S. Aryal. (2009).
  \emph{Statistical Methods for Groundwater Monitoring}, Second Edition.
  John Wiley & Sons, Hoboken.

  Hahn, G.J. (1969). Factors for Calculating Two-Sided Prediction Intervals for
  Samples from a Normal Distribution.
  \emph{Journal of the American Statistical Association} \bold{64}(327), 878-898.

  Hahn, G.J. (1970a). Additional Factors for Calculating Prediction Intervals for
  Samples from a Normal Distribution.
  \emph{Journal of the American Statistical Association} \bold{65}(332), 1668-1676.

  Hahn, G.J. (1970b). Statistical Intervals for a Normal Population, Part I: Tables,
  Examples and Applications. \emph{Journal of Quality Technology} \bold{2}(3), 115-125.

  Hahn, G.J. (1970c). Statistical Intervals for a Normal Population, Part II:
  Formulas, Assumptions, Some Derivations. \emph{Journal of Quality Technology}
  \bold{2}(4), 195-206.

  Hahn, G.J., and W.Q. Meeker. (1991). \emph{Statistical Intervals: A Guide for
  Practitioners}.  John Wiley and Sons, New York.

  Hahn, G., and W. Nelson. (1973). A Survey of Prediction Intervals and Their
  Applications.  \emph{Journal of Quality Technology} \bold{5}, 178-188.

  Hall, I.J., and R.R. Prairie. (1973).  One-Sided Prediction Intervals to Contain at
  Least \eqn{m} Out of \eqn{k} Future Observations.
  \emph{Technometrics} \bold{15}, 897--914.

  Millard, S.P. (1987).  Environmental Monitoring, Statistics, and the Law:  Room for
  Improvement (with Comment).  \emph{The American Statistician} \bold{41}(4), 249--259.

  Millard, S.P., and Neerchal, N.K. (2001). \emph{Environmental Statistics with S-PLUS}.
  CRC Press, Boca Raton, Florida.

  USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
  EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.
  U.S. Environmental Protection Agency, Washington, D.C.

  USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
  EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
  U.S. Environmental Protection Agency, Washington, D.C.
}
\author{
    Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  \emph{Motivation} \cr
  Prediction and tolerance intervals have long been applied to quality control and
  life testing problems (Hahn, 1970b,c; Hahn and Nelson, 1973).  In the context of
  environmental statistics, prediction intervals are useful for analyzing data from
  groundwater detection monitoring programs at hazardous and solid waste facilities.

  One of the main statistical problems that plague groundwater monitoring programs at
  hazardous and solid waste facilities is the requirement of testing several wells and
  several constituents at each well on each sampling occasion.  This is an obvious
  multiple comparisons problem, and the naive approach of using a standard t-test at
  a conventional \eqn{\alpha}-level (e.g., 0.05 or 0.01) for each test leads to a
  very high probability of at least one significant result on each sampling occasion,
  when in fact no contamination has occurred.  This problem was pointed out years ago
  by Millard (1987) and others.

  Davis and McNichols (1987) proposed simultaneous prediction intervals as a way of
  controlling the facility-wide false positive rate (FWFPR) while maintaining adequate
  power to detect contamination in the groundwater.  Because of the ubiquitous presence
  of spatial variability, it is usually best to use simultaneous prediction intervals
  at each well (Davis, 1998a).  That is, by constructing prediction intervals based on
  background (pre-landfill) data on each well, and comparing future observations at a
  well to the prediction interval for that particular well.  In each of these cases,
  the individual \eqn{\alpha}-level at each well is equal to the FWFRP divided by the
  product of the number of wells and constituents.

  Often, observations at downgradient wells are not available prior to the
  construction and operation of the landfill.  In this case, upgradient well data can
  be combined to create a background prediction interval, and observations at each
  downgradient well can be compared to this prediction interval.  If spatial
  variability is present and a major source of variation, however, this method is not
  really valid (Davis, 1994; Davis, 1998a).

  Chapter 19 of USEPA (2009) contains an extensive discussion of using the
  \eqn{1}-of-\eqn{m} rule and the Modified California rule.

  Chapters 1 and 3 of Gibbons et al. (2009) discuss simultaneous prediction intervals
  for the normal and lognormal distributions, respectively.
  \cr

  \emph{The k-of-m Rule} \cr
  For the \eqn{k}-of-\eqn{m} rule, Davis and McNichols (1987) give tables with
  \dQuote{optimal} choices of \eqn{k} (in terms of best power for a given overall
  confidence level) for selected values of \eqn{m}, \eqn{r}, and \eqn{n}.  They found
  that the optimal ratios of \eqn{k} to \eqn{m} (i.e., \eqn{k/m}) are generally small,
  in the range of 15-50\%.
  \cr

  \emph{The California Rule} \cr
  The California rule was mandated in that state for groundwater monitoring at waste
  disposal facilities when resampling verification is part of the statistical program
  (Barclay's Code of California Regulations, 1991).  The California code mandates a
  \dQuote{California} rule with \eqn{m \ge 3}.  The motivation for this rule may have
  been a desire to have a majority of the observations in bounds (Davis, 1998a).  For
  example, for a \eqn{k}-of-\eqn{m} rule with \eqn{k=1} and \eqn{m=3}, a monitoring
  location will pass if the first observation is out of bounds, the second resample
  is out of bounds, but the last resample is in bounds, so that 2 out of 3 observations
  are out of bounds.  For the California rule with \eqn{m=3}, either the first
  observation must be in bounds, or the next 2 observations must be in bounds in order
  for the monitoring location to pass.

  Davis (1998a) states that if the FWFPR is kept constant, then the California rule
  offers little increased power compared to the \eqn{k}-of-\eqn{m} rule, and can
  actually decrease the power of detecting contamination.
  \cr

  \emph{The Modified California Rule} \cr
  The Modified California Rule was proposed as a compromise between a 1-of-\eqn{m}
  rule and the California rule.  For a given FWFPR, the Modified California rule
  achieves better power than the California rule, and still requires at least as many
  observations in bounds as out of bounds, unlike a 1-of-\eqn{m} rule.
  \cr

  \emph{Different Notations Between Different References} \cr
  For the \eqn{k}-of-\eqn{m} rule described in this help file, both
  Davis and McNichols (1987) and USEPA (2009, Chapter 19) use the variable \eqn{p} instead of \eqn{k} to represent the minimum number
  of future observations the interval should contain on each of the \eqn{r} sampling
  occasions.

  Gibbons et al. (2009, Chapter 1) presents extensive lists of the value of
  \eqn{K} for both \eqn{k}-of-\eqn{m} rules and California rules.  Gibbons et al.'s
  notation reverses the meaning of \eqn{k} and \eqn{r} compared to the notation used
  in this help file.  That is, in Gibbons et al.'s notation, \eqn{k} represents the
  number of future sampling occasions or monitoring wells, and \eqn{r} represents the
  minimum number of observations the interval should contain on each sampling occasion.

  USEPA (2009, Chapter 19) uses \eqn{p} in place of \eqn{k}.
}
\seealso{
  \code{\link{predIntNormSimultaneous}},
  \code{\link{predIntNormSimultaneousTestPower}},
  \code{\link{predIntNorm}}, \code{\link{predIntNormK}},
  \code{\link{predIntLnormSimultaneous}}, \code{\link{tolIntNorm}},
  \link{Normal}, \code{\link{estimate.object}}, \code{\link{enorm}}
}
\examples{
  # Compute the value of K for an upper 95% simultaneous prediction 
  # interval to contain at least 1 out of the next 3 observations 
  # given a background sample size of n=8.

  predIntNormSimultaneousK(n = 8, k = 1, m = 3)
  #[1] 0.5123091

  #----------

  # Compare the value of K for a 95% 1-of-3 upper prediction interval to 
  # the value for the California and Modified California rules.  
  # Note that the value of K for the Modified California rule is between 
  # the value of K for the 1-of-3 rule and the California rule. 

  predIntNormSimultaneousK(n = 8, k = 1, m = 3) 
  #[1] 0.5123091 
 
  predIntNormSimultaneousK(n = 8, m = 3, rule = "CA")
  #[1] 1.252077

  predIntNormSimultaneousK(n = 8, rule = "Modified.CA")
  #[1] 0.8380233

  #----------

  # Show how the value of K for an upper 95% simultaneous prediction 
  # limit increases as the number of future sampling occasions r increases.  
  # Here, we'll use the 1-of-3 rule.

  predIntNormSimultaneousK(n = 8, k = 1, m = 3)
  #[1] 0.5123091 


  predIntNormSimultaneousK(n = 8, k = 1, m = 3, r = 10)
  #[1] 1.363002

  #==========

  # Example 19-1 of USEPA (2009, p. 19-17) shows how to compute an 
  # upper simultaneous prediction limit for the 1-of-3 rule for 
  # r = 2 future sampling occasions.  The data for this example are 
  # stored in EPA.09.Ex.19.1.sulfate.df.

  # We will pool data from 4 background wells that were sampled on 
  # a number of different occasions, giving us a sample size of 
  # n = 25 to use to construct the prediction limit.

  # There are 50 compliance wells and we will monitor 10 different 
  # constituents at each well at each of the r=2 future sampling 
  # occasions.  To determine the confidence level we require for 
  # the simultaneous prediction interval, USEPA (2009) recommends 
  # setting the individual Type I Error level at each well to 
 
  # 1 - (1 - SWFPR)^(1 / (Number of Constituents * Number of Wells))
  
  # which translates to setting the confidence limit to 

  # (1 - SWFPR)^(1 / (Number of Constituents * Number of Wells))

  # where SWFPR = site-wide false positive rate.  For this example, we 
  # will set SWFPR = 0.1.  Thus, the confidence level is given by:

  nc <- 10
  nw <- 50
  SWFPR <- 0.1
  conf.level <- (1 - SWFPR)^(1 / (nc * nw))

  conf.level
  #[1] 0.9997893

  #----------

  # Compute the value of K for the upper simultaneous prediction 
  # limit for the 1-of-3 plan.

  predIntNormSimultaneousK(n = 25, k = 1, m = 3, r = 2, 
    rule = "k.of.m", pi.type = "upper", conf.level = conf.level)
  #[1] 2.014365


 #==========

  \dontrun{
  # Try to compute K for a two-sided simultaneous prediction interval:

  predIntNormSimultaneousK(n = 25, k = 1, m = 3, r = 2,
   rule = "k.of.m", pi.type = "two-sided", conf.level = conf.level)
  #Error in predIntNormSimultaneousK(n = 25, k = 1, m = 3, r = 2, rule = "k.of.m",  : 
  #  Two-sided simultaneous prediction intervals are not currently available.
  # NOTE: Two-sided simultaneous prediction intervals computed using
  # Versions 2.4.0 - 2.8.1 of EnvStats are *NOT* valid.
  }

  #==========

  # Cleanup
  #--------

  rm(nc, nw, SWFPR, conf.level)
}
\keyword{ distribution }
\keyword{ htest }
