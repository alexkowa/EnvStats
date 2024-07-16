\name{predIntLnormSimultaneous}
\alias{predIntLnormSimultaneous}
\alias{predIntLnormAltSimultaneous}
\title{
  Simultaneous Prediction Interval for a Lognormal Distribution
}
\description{
  Estimate the mean and standard deviation on the log-scale for a
  \link[stats:Lognormal]{lognormal distribution}, or estimate the mean
  and coefficient of variation for a
  \link[=LognormalAlt]{lognormal distribution (alternative parameterization)},
  and construct a simultaneous prediction
  interval for the next \eqn{r} sampling occasions, based on one of three possible
  rules: k-of-m, California, or Modified California.
}
\usage{
  predIntLnormSimultaneous(x, n.geomean = 1, k = 1, m = 2, r = 1, rule = "k.of.m",
    delta.over.sigma = 0, pi.type = "upper", conf.level = 0.95,
    K.tol = .Machine$double.eps^0.5)

  predIntLnormAltSimultaneous(x, n.geomean = 1, k = 1, m = 2, r = 1, rule = "k.of.m",
    delta.over.sigma = 0, pi.type = "upper", conf.level = 0.95,
    K.tol = .Machine$double.eps^0.5, est.arg.list = NULL)
}
\arguments{
  \item{x}{
  For \code{predIntLnormSimultaneous}, \code{x} can be a numeric vector of positive
  observations,
  or an object resulting from a call to an estimating function that assumes a
  lognormal distribution (i.e., \code{\link{elnorm}} or \code{\link{elnormCensored}}).
  You \emph{cannot} supply objects resulting from a call to estimating functions that
  use the alternative parameterization such as \code{\link{elnormAlt}} or
  \code{\link{elnormAltCensored}}.

  For \code{predIntLnormAltSimultaneous}, a numeric vector of positive observations.

  If \code{x} is a numeric vector,
  missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf})
  values are allowed but will be removed.
}
  \item{n.geomean}{
  positive integer specifying the sample size associated with future
  geometric means.  The default value is \code{n.geomean=1} (i.e., individual
  observations).  Note that all future geometric means must be based on the same
  sample size.
}
  \item{k}{
  for the \eqn{k}-of-\eqn{m} rule (\code{rule="k.of.m"}), a positive integer
  specifying the minimum number of observations (or geometric means) out of \eqn{m}
  observations (or geometric means) (all obtained on one future sampling \dQuote{occassion})
  the prediction interval should contain with confidence level \code{conf.level}.
  The default value is \code{k=1}.  This argument is ignored when the argument
  \code{rule} is not equal to \code{"k.of.m"}.
}
  \item{m}{
  positive integer specifying the maximum number of future observations (or
  geometric means) on one future sampling \dQuote{occasion}.
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
  (on the log-scale) that was sampled to construct the prediction interval, and the
  mean of the population (on the log-scale) that will be sampled to produce the future
  observations.  The quantity \eqn{\sigma} (sigma) denotes the population standard
  deviation (on the log-scale) for both populations.
  See the DETAILS section below for more information.  The default value is
  \code{delta.over.sigma=0}.
}
  \item{pi.type}{
  character string indicating what kind of prediction interval to compute.
  The possible values are \code{pi.type="upper"} (the default),
  \code{pi.type="lower"} and \code{pi.type="two-sided"}.
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
  \item{est.arg.list}{
  a list containing arguments to pass to the function
  \code{\link{elnormAlt}} for estimating the mean and coefficient of variation.
  The default value is \code{est.arg.list=NULL}, which implies the default values
  will be used in the call to \code{\link{elnormAlt}}.
}
}
\details{
  The function \code{predIntLnormSimultaneous} returns a simultaneous prediction
  interval as well as estimates of the meanlog and sdlog parameters.
  The function \code{predIntLnormAltSimultaneous} returns a prediction interval as
  well as estimates of the mean and coefficient of variation.

  A simultaneous prediction interval for a lognormal distribution is constructed by
  taking the natural logarithm of the observations and constructing a prediction
  interval based on the normal (Gaussian) distribution by calling
  \code{\link{predIntNormSimultaneous}}.
  These prediction limits are then exponentiated to produce a prediction interval on
  the original scale of the data.
}
\value{
  If \code{x} is a numeric vector, \code{predIntLnormSimultaneous} returns a list of
  class \code{"estimate"} containing the estimated parameters, the prediction interval,
  and other information.  See the help file for \cr
  \code{\link{estimate.object}} for details.

  If \code{x} is the result of calling an estimation function,
  \code{predIntLnormSimultaneous} returns a list whose class is the same as \code{x}.
  The list contains the same components as \code{x}, as well as a component called
  \code{interval} containing the prediction interval information.
  If \code{x} already has a component called \code{interval}, this component is
  replaced with the prediction interval information.
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
  of \eqn{m} Observations from a Lognormal Population on Each of \eqn{r} Future Occasions.
  \emph{Technometrics} \bold{29}, 359--370.

  Fertig, K.W., and N.R. Mann. (1977).  One-Sided Prediction Intervals for at Least
  \eqn{p} Out of \eqn{m} Future Observations From a Lognormal Population.
  \emph{Technometrics} \bold{19}, 167--177.

  Gibbons, R.D., D.K. Bhaumik, and S. Aryal. (2009).
  \emph{Statistical Methods for Groundwater Monitoring}, Second Edition.
  John Wiley & Sons, Hoboken.

  Hahn, G.J. (1969). Factors for Calculating Two-Sided Prediction Intervals for
  Samples from a Lognormal Distribution.
  \emph{Journal of the American Statistical Association} \bold{64}(327), 878-898.

  Hahn, G.J. (1970a). Additional Factors for Calculating Prediction Intervals for
  Samples from a Lognormal Distribution.
  \emph{Journal of the American Statistical Association} \bold{65}(332), 1668-1676.

  Hahn, G.J. (1970b). Statistical Intervals for a Lognormal Population, Part I: Tables,
  Examples and Applications. \emph{Journal of Quality Technology} \bold{2}(3), 115-125.

  Hahn, G.J. (1970c). Statistical Intervals for a Lognormal Population, Part II:
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
  Davis and McNichols (1987) and USEPA (2009, Chapter 19) use the variable
  \eqn{p} instead of \eqn{k} to represent the minimum number
  of future observations the interval should contain on each of the \eqn{r} sampling
  occasions.

  Gibbons et al. (2009, Chapter 1) presents extensive lists of the value of
  \eqn{K} for both \eqn{k}-of-\eqn{m} rules and California rules.  Gibbons et al.'s
  notation reverses the meaning of \eqn{k} and \eqn{r} compared to the notation used
  in this help file.  That is, in Gibbons et al.'s notation, \eqn{k} represents the
  number of future sampling occasions or monitoring wells, and \eqn{r} represents the
  minimum number of observations the interval should contain on each sampling occasion.
}
\seealso{
  \code{\link{predIntLnormAltSimultaneousTestPower}},
  \code{\link{predIntNorm}}, \code{\link{predIntNormSimultaneous}}, \cr
  \code{\link{predIntNormSimultaneousTestPower}},
  \code{\link{tolIntLnorm}}, \link{Lognormal}, \link{LognormalAlt}, \cr
  \code{\link{estimate.object}}, \code{\link{elnorm}}, \code{\link{elnormAlt}}.
}
\examples{
  # Generate 8 observations from a lognormal distribution with parameters
  # mean=10 and cv=1, then use predIntLnormAltSimultaneous to estimate the
  # mean and coefficient of variation of the true distribution and construct an
  # upper 95% prediction interval to contain at least 1 out of the next
  # 3 observations.
  # (Note: the call to set.seed simply allows you to reproduce this example.)

  set.seed(479)
  dat <- rlnormAlt(8, mean = 10, cv = 1)

  predIntLnormAltSimultaneous(dat, k = 1, m = 3)

  # Compare the 95% 1-of-3 upper prediction limit to the California and
  # Modified California upper prediction limits.  Note that the upper
  # prediction limit for the Modified California rule is between the limit
  # for the 1-of-3 rule and the limit for the California rule.

  predIntLnormAltSimultaneous(dat, k = 1, m = 3)$interval$limits["UPL"]

  predIntLnormAltSimultaneous(dat, m = 3, rule = "CA")$interval$limits["UPL"]

  predIntLnormAltSimultaneous(dat, rule = "Modified.CA")$interval$limits["UPL"]

  # Show how the upper 95% simultaneous prediction limit increases
  # as the number of future sampling occasions r increases.
  # Here, we'll use the 1-of-3 rule.

  predIntLnormAltSimultaneous(dat, k = 1, m = 3)$interval$limits["UPL"]

  predIntLnormAltSimultaneous(dat, k = 1, m = 3, r = 10)$interval$limits["UPL"]

  # Compare the upper simultaneous prediction limit for the 1-of-3 rule
  # based on individual observations versus based on geometric means of
  # order 4.

  predIntLnormAltSimultaneous(dat, k = 1, m = 3)$interval$limits["UPL"]

  predIntLnormAltSimultaneous(dat, n.geomean = 4, k = 1,
    m = 3)$interval$limits["UPL"]

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

  #----------

  # Look at the data:

  names(EPA.09.Ex.19.1.sulfate.df)

  EPA.09.Ex.19.1.sulfate.df[,
    c("Well", "Date", "Sulfate.mg.per.l", "log.Sulfate.mg.per.l")]



  # Construct the upper simultaneous prediction limit for the
  # 1-of-3 plan assuming a lognormal distribution for the
  # sulfate data

  Sulfate <- EPA.09.Ex.19.1.sulfate.df$Sulfate.mg.per.l

  predIntLnormSimultaneous(x = Sulfate, k = 1, m = 3, r = 2,
    rule = "k.of.m", pi.type = "upper", conf.level = conf.level)

  #==========

  # NOTE: Two-sided simultaneous prediction intervals computed using
  # Versions 2.4.0 - 2.8.1 of EnvStats are *NOT* valid.
  \dontrun{
  predIntLnormSimultaneous(x = Sulfate, k = 1, m = 3, r = 2,
    rule = "k.of.m", pi.type = "two-sided", conf.level = conf.level)
  }

}
\keyword{ distribution }
\keyword{ htest }
