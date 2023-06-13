\name{predIntGammaSimultaneous}
\alias{predIntGammaSimultaneous}
\alias{predIntGammaAltSimultaneous}
\title{
  Simultaneous Prediction Interval for a Gamma Distribution
}
\description{
  Estimate the shape and scale parameters for a
  \link[stats:GammaDist]{gamma distribution},
  or estimate the mean and coefficient of variation for a
  \link[=GammaAlt]{gamma distribution (alternative parameterization)},
  and construct a simultaneous prediction interval for the next \eqn{r} sampling
  occasions, based on one of three possible rules: k-of-m, California, or
  Modified California.
}
\usage{
  predIntGammaSimultaneous(x, n.transmean = 1, k = 1, m = 2, r = 1,
    rule = "k.of.m", delta.over.sigma = 0, pi.type = "upper", conf.level = 0.95,
    K.tol = 1e-07, est.method = "mle", normal.approx.transform = "kulkarni.powar")

  predIntGammaAltSimultaneous(x, n.transmean = 1, k = 1, m = 2, r = 1,
    rule = "k.of.m", delta.over.sigma = 0, pi.type = "upper", conf.level = 0.95,
    K.tol = 1e-07, est.method = "mle", normal.approx.transform = "kulkarni.powar")
}
\arguments{
  \item{x}{
  numeric vector of non-negative observations. Missing (\code{NA}), undefined (\code{NaN}), and
  infinite (\code{Inf}, \code{-Inf}) values are allowed but will be removed.
}
  \item{n.transmean}{
  positive integer specifying the sample size associated with future transformed
  means (see the DETAILS section for an explanation of what the transformation is).
  The default value is \code{n.transmean=1} (i.e., individual observations).
  Note that all future transformed means must be based on the same sample size.
}
  \item{k}{
  for the \eqn{k}-of-\eqn{m} rule (\code{rule="k.of.m"}), a positive integer
  specifying the minimum number of observations (or transformed means) out of \eqn{m}
  observations (or transformed means) (all obtained on one future sampling \dQuote{occassion})
  the prediction interval should contain with confidence level \code{conf.level}.
  The default value is \code{k=1}.  This argument is ignored when the argument
  \code{rule} is not equal to \code{"k.of.m"}.
}
  \item{m}{
  positive integer specifying the maximum number of future observations (or
  transformed means) on one future sampling \dQuote{occasion}.
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
  (on the transformed scale) that was sampled to construct the prediction interval,
  and the mean of the population (on the transformed scale) that will be sampled to
  produce the future observations.  The quantity \eqn{\sigma} (sigma) denotes the
  population standard deviation (on the transformed scale) for both populations.
  See the DETAILS section below for more information.  The default value is
  \code{delta.over.sigma=0}.
}
  \item{pi.type}{
  character string indicating what kind of prediction interval to compute.
  The possible values are \code{pi.type="upper"} (the default), and
  \code{pi.type="lower"}.
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
  \item{est.method}{
  character string specifying the method of estimation for the shape and scale
  distribution parameters.  The possible values are
  \code{"mle"} (maximum likelihood; the default),
  \code{"bcmle"} (bias-corrected mle), \code{"mme"} (method of moments), and
  \code{"mmue"} (method of moments based on the unbiased estimator of variance).
  See the DETAILS section of the help file for \code{\link{egamma}} for more
  information.
}
  \item{normal.approx.transform}{
  character string indicating which power transformation to use.
  Possible values are \code{"kulkarni.powar"} (the default), \code{"cube.root"}, and \cr
  \code{"fourth.root"}.  See the DETAILS section for more informaiton.
}
}
\details{
  The function \code{predIntGammaSimultaneous} returns a simultaneous prediction
  interval as well as estimates of the shape and scale parameters.
  The function \code{predIntGammaAltSimultaneous} returns a simultaneous prediction
  interval as well as estimates of the mean and coefficient of variation.

  Following Krishnamoorthy et al. (2008), the simultaneous prediction interval is
  computed by:
  \enumerate{
  \item using a power transformation on the original data to induce approximate
    normality,
  \item calling \code{\link{predIntNormSimultaneous}} with the transformed data to
    compute the simultaneous prediction interval, and then
  \item back-transforming the interval to create a simultaneous prediction interval
    on the original scale.
  }
  The argument \code{normal.approx.transform} determines which transformation is used.
  The value \code{normal.approx.transform="cube.root"} uses
  the cube root transformation suggested by Wilson and Hilferty (1931) and used by
  Krishnamoorthy et al. (2008) and Singh et al. (2010b), and the value
  \code{normal.approx.transform="fourth.root"} uses the fourth root transformation
  suggested by Hawkins and Wixley (1986) and used by Singh et al. (2010b).
  The default value \code{normal.approx.transform="kulkarni.powar"}
  uses the "Optimum Power Normal Approximation Method" of Kulkarni and Powar (2010).
  The "optimum" power \eqn{p} is determined by:
  \tabular{ll}{
  \eqn{p = -0.0705 - 0.178 \, shape + 0.475 \, \sqrt{shape}} \tab if \eqn{shape \le 1.5} \cr
  \eqn{p = 0.246} \tab if \eqn{shape > 1.5} \cr
  }
  where \eqn{shape} denotes the estimate of the shape parameter.  Although
  Kulkarni and Powar (2010) use the maximum likelihood estimate of shape to
  determine the power \eqn{p}, for the functions \cr
  \code{predIntGammaSimultaneous} and \code{predIntGammaAltSimultaneous} the power
  \eqn{p} is based on whatever estimate of shape is used
  (e.g., \code{est.method="mle"}, \code{est.method="bcmle"}, etc.).

  When the argument \code{n.transmean} is larger than 1 (i.e., you are
  constructing a prediction interval for future means, not just single
  observations), in order to properly compare a future mean with the
  prediction limits, you must follow these steps:
  \enumerate{
    \item Take the observations that will be used to compute the mean and
          transform them by raising them to the power given by the value in the
          component \code{interval$normal.transform.power} (see the section VALUE below).
    \item Compute the mean of the transformed observations.
    \item Take the mean computed in step 2 above and raise it to the inverse of the power
          originally used to transform the observations.
  }
}
\value{
  A list of class \code{"estimate"} containing the estimated parameters,
  the simultaneous prediction interval, and other information.
  See \code{\link{estimate.object}} for details.

  In addition to the usual components contained in an object of class
  \code{"estimate"}, the returned value also includes two additional
  components within the \code{"interval"} component:

  \item{n.transmean}{the value of \code{n.transmean} supplied in the
    call to \code{predIntGammaSimultaneous} or \code{predIntGammaAltSimultaneous}.}
  \item{normal.transform.power}{the value of the power used to
    transform the original data to approximate normality.}
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

  Evans, M., N. Hastings, and B. Peacock. (1993). \emph{Statistical Distributions}.
  Second Edition. John Wiley and Sons, New York, Chapter 18.

  Gibbons, R.D., D.K. Bhaumik, and S. Aryal. (2009).
  \emph{Statistical Methods for Groundwater Monitoring}, Second Edition.
  John Wiley & Sons, Hoboken.

  Fertig, K.W., and N.R. Mann. (1977).  One-Sided Prediction Intervals for at Least
  \eqn{p} Out of \eqn{m} Future Observations From a Lognormal Population.
  \emph{Technometrics} \bold{19}, 167--177.

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

  Hawkins, D. M., and R.A.J. Wixley. (1986). A Note on the Transformation of
  Chi-Squared Variables to Normality. \emph{The American Statistician}, \bold{40},
  296--298.

  Johnson, N.L., S. Kotz, and N. Balakrishnan. (1994).
  \emph{Continuous Univariate Distributions, Volume 1}. Second Edition.
  John Wiley and Sons, New York, Chapter 17.

  Krishnamoorthy K., T. Mathew, and S. Mukherjee. (2008). Normal-Based Methods for a
  Gamma Distribution: Prediction and Tolerance Intervals and Stress-Strength Reliability.
  \emph{Technometrics}, \bold{50}(1), 69--78.

  Krishnamoorthy K., and T. Mathew. (2009).
  \emph{Statistical Tolerance Regions: Theory, Applications, and Computation}.
  John Wiley and Sons, Hoboken.

  Kulkarni, H.V., and S.K. Powar. (2010). A New Method for Interval Estimation of the Mean
  of the Gamma Distribution. \emph{Lifetime Data Analysis}, \bold{16}, 431--447.

  Millard, S.P. (1987).  Environmental Monitoring, Statistics, and the Law:  Room for
  Improvement (with Comment).  \emph{The American Statistician} \bold{41}(4), 249--259.

  Millard, S.P., and N.K. Neerchal. (2001). \emph{Environmental Statistics with S-PLUS}.
  CRC Press, Boca Raton.

  Singh, A., A.K. Singh, and R.J. Iaci. (2002).
  \emph{Estimation of the Exposure Point Concentration Term Using a Gamma Distribution}.
  EPA/600/R-02/084. October 2002. Technology Support Center for Monitoring and
  Site Characterization, Office of Research and Development, Office of Solid Waste and
  Emergency Response, U.S. Environmental Protection Agency, Washington, D.C.

  Singh, A., R. Maichle, and N. Armbya. (2010a).
  \emph{ProUCL Version 4.1.00 User Guide (Draft)}. EPA/600/R-07/041, May 2010.
  Office of Research and Development, U.S. Environmental Protection Agency, Washington, D.C.

  Singh, A., N. Armbya, and A. Singh. (2010b).
  \emph{ProUCL Version 4.1.00 Technical Guide (Draft)}. EPA/600/R-07/041, May 2010.
  Office of Research and Development, U.S. Environmental Protection Agency, Washington, D.C.

  Wilson, E.B., and M.M. Hilferty. (1931). The Distribution of Chi-Squares.
  \emph{Proceedings of the National Academy of Sciences}, \bold{17}, 684--688.

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
  \emph{The Gamma Distribution} \cr
  The \link[stats:GammaDist]{gamma distribution} takes values on the positive real line.
  Special cases of the gamma are the \link[=Exponential]{exponential} distribution and
  the \link[=Chisquare]{chi-square} distributions.  Applications of the gamma include
  life testing, statistical ecology, queuing theory, inventory control, and precipitation
  processes. A gamma distribution starts to resemble a normal distribution as the
  shape parameter a tends to infinity.

  Some EPA guidance documents (e.g., Singh et al., 2002; Singh et al., 2010a,b) strongly
  recommend against using a lognormal model for environmental data and recommend trying a
  gamma distribuiton instead.
  \cr

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
\section{Warning}{
  It is possible for the lower prediction limit based on the transformed data to be
  less than 0.  In this case, the lower prediction limit on the original scale is
  set to 0 and a warning is issued stating that the normal approximation is not
  accurate in this case.
}
\seealso{
  \code{\link{GammaDist}}, \code{\link{GammaAlt}},
  \code{\link{predIntNorm}}, \code{\link{predIntNormSimultaneous}}, \cr
  \code{\link{predIntNormSimultaneousTestPower}}, \code{\link{tolIntGamma}},
  \code{\link{egamma}}, \code{\link{egammaAlt}}, \code{\link{estimate.object}}.
}
\examples{
  # Generate 8 observations from a gamma distribution with parameters
  # mean=10 and cv=1, then use predIntGammaAltSimultaneous to estimate the
  # mean and coefficient of variation of the true distribution and construct an
  # upper 95% prediction interval to contain at least 1 out of the next
  # 3 observations.
  # (Note: the call to set.seed simply allows you to reproduce this example.)

  set.seed(479)
  dat <- rgammaAlt(8, mean = 10, cv = 1)

  predIntGammaAltSimultaneous(dat, k = 1, m = 3)

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            Gamma
  #
  #Estimated Parameter(s):          mean = 13.875825
  #                                 cv   =  1.049504
  #
  #Estimation Method:               MLE
  #
  #Data:                            dat
  #
  #Sample Size:                     8
  #
  #Prediction Interval Method:      exact using
  #                                 Kulkarni & Powar (2010)
  #                                 transformation to Normality
  #                                 based on MLE of 'shape'
  #
  #Normal Transform Power:          0.2204908
  #
  #Prediction Interval Type:        upper
  #
  #Confidence Level:                95%
  #
  #Minimum Number of
  #Future Observations
  #Interval Should Contain:         1
  #
  #Total Number of
  #Future Observations:             3
  #
  #Prediction Interval:             LPL =  0.00000
  #                                 UPL = 15.87101

  #----------

  # Compare the 95% 1-of-3 upper prediction limit to the California and
  # Modified California upper prediction limits.  Note that the upper
  # prediction limit for the Modified California rule is between the limit
  # for the 1-of-3 rule and the limit for the California rule.

  predIntGammaAltSimultaneous(dat, k = 1, m = 3)$interval$limits["UPL"]
  #     UPL
  #15.87101

  predIntGammaAltSimultaneous(dat, m = 3, rule = "CA")$interval$limits["UPL"]
  #     UPL
  #34.11499

  predIntGammaAltSimultaneous(dat, rule = "Modified.CA")$interval$limits["UPL"]
  #     UPL
  #22.58809

  #----------

  # Show how the upper 95% simultaneous prediction limit increases
  # as the number of future sampling occasions r increases.
  # Here, we'll use the 1-of-3 rule.

  predIntGammaAltSimultaneous(dat, k = 1, m = 3)$interval$limits["UPL"]
  #     UPL
  #15.87101

  predIntGammaAltSimultaneous(dat, k = 1, m = 3, r = 10)$interval$limits["UPL"]
  #     UPL
  #37.86825

  #----------

  # Compare the upper simultaneous prediction limit for the 1-of-3 rule
  # based on individual observations versus based on transformed means of
  # order 4.

  predIntGammaAltSimultaneous(dat, k = 1, m = 3)$interval$limits["UPL"]
  #     UPL
  #15.87101

  predIntGammaAltSimultaneous(dat, n.transmean = 4, k = 1,
    m = 3)$interval$limits["UPL"]
  #     UPL
  #14.76528

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

  # Look at the data:

  names(EPA.09.Ex.19.1.sulfate.df)
  #[1] "Well"                 "Month"                "Day"
  #[4] "Year"                 "Date"                 "Sulfate.mg.per.l"
  #[7] "log.Sulfate.mg.per.l"

  EPA.09.Ex.19.1.sulfate.df[,
    c("Well", "Date", "Sulfate.mg.per.l", "log.Sulfate.mg.per.l")]

  #    Well       Date Sulfate.mg.per.l log.Sulfate.mg.per.l
  #1  GW-01 1999-07-08             63.0             4.143135
  #2  GW-01 1999-09-12             51.0             3.931826
  #3  GW-01 1999-10-16             60.0             4.094345
  #4  GW-01 1999-11-02             86.0             4.454347
  #5  GW-04 1999-07-09            104.0             4.644391
  #6  GW-04 1999-09-14            102.0             4.624973
  #7  GW-04 1999-10-12             84.0             4.430817
  #8  GW-04 1999-11-15             72.0             4.276666
  #9  GW-08 1997-10-12             31.0             3.433987
  #10 GW-08 1997-11-16             84.0             4.430817
  #11 GW-08 1998-01-28             65.0             4.174387
  #12 GW-08 1999-04-20             41.0             3.713572
  #13 GW-08 2002-06-04             51.8             3.947390
  #14 GW-08 2002-09-16             57.5             4.051785
  #15 GW-08 2002-12-02             66.8             4.201703
  #16 GW-08 2003-03-24             87.1             4.467057
  #17 GW-09 1997-10-16             59.0             4.077537
  #18 GW-09 1998-01-28             85.0             4.442651
  #19 GW-09 1998-04-12             75.0             4.317488
  #20 GW-09 1998-07-12             99.0             4.595120
  #21 GW-09 2000-01-30             75.8             4.328098
  #22 GW-09 2000-04-24             82.5             4.412798
  #23 GW-09 2000-10-24             85.5             4.448516
  #24 GW-09 2002-12-01            188.0             5.236442
  #25 GW-09 2003-03-24            150.0             5.010635

  # The EPA guidance document constructs the upper simultaneous
  # prediction limit for the 1-of-3 plan assuming a lognormal
  # distribution for the sulfate data.  Here we will compare
  # the value of the limit based on assuming a lognormal distribution
  # versus assuming a gamma distribution.

  Sulfate <- EPA.09.Ex.19.1.sulfate.df$Sulfate.mg.per.l

  pred.int.list.lnorm <-
    predIntLnormSimultaneous(x = Sulfate, k = 1, m = 3, r = 2,
      rule = "k.of.m", pi.type = "upper", conf.level = conf.level)

  pred.int.list.gamma <-
    predIntGammaSimultaneous(x = Sulfate, k = 1, m = 3, r = 2,
      rule = "k.of.m", pi.type = "upper", conf.level = conf.level)


  pred.int.list.lnorm$interval$limits["UPL"]
  #     UPL
  #159.5497

  pred.int.list.gamma$interval$limits["UPL"]
  #     UPL
  #153.3232

  #==========

  # Cleanup
  #--------
  rm(dat, nc, nw, SWFPR, conf.level, Sulfate, pred.int.list.lnorm,
    pred.int.list.gamma)
}
\keyword{ distribution }
\keyword{ htest }
