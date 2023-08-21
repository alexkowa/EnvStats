\name{predIntLnorm}
\alias{predIntLnorm}
\alias{predIntLnormAlt}
\title{
  Prediction Interval for a Lognormal Distribution
}
\description{
  Estimate the mean and standard deviation on the log-scale for a
  \link[stats:Lognormal]{lognormal distribution}, or estimate the mean
  and coefficient of variation for a
  \link[=LognormalAlt]{lognormal distribution (alternative parameterization)},
  and construct a prediction interval for the next \eqn{k} observations or
  next set of \eqn{k} geometric means.
}
\usage{
  predIntLnorm(x, n.geomean = 1, k = 1, method = "Bonferroni",
    pi.type = "two-sided", conf.level = 0.95)

  predIntLnormAlt(x, n.geomean = 1, k = 1, method = "Bonferroni",
    pi.type = "two-sided", conf.level = 0.95, est.arg.list = NULL)
}
\arguments{
  \item{x}{
  For \code{predIntLnorm}, \code{x} can be a numeric vector of positive observations,
  or an object resulting from a call to an estimating function that assumes a
  lognormal distribution (i.e., \code{\link{elnorm}} or \code{\link{elnormCensored}}).
  You \emph{cannot} supply objects resulting from a call to estimating functions that
  use the alternative parameterization such as \code{\link{elnormAlt}} or
  \code{\link{elnormAltCensored}}.

  For \code{predIntLnormAlt}, a numeric vector of positive observations.

  If \code{x} is a numeric vector,
  missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf})
  values are allowed but will be removed.
}
  \item{n.geomean}{
  positive integer specifying the sample size associated with the \eqn{k} future
  geometric means.  The default value is \code{n.geomean=1} (i.e., individual
  observations).  Note that all future geometric means must be based on the same
  sample size.
}
  \item{k}{
  positive integer specifying the number of future observations or geometric means the
  prediction interval should contain with confidence level \code{conf.level}.
  The default value is \code{k=1}.
}
  \item{method}{
  character string specifying the method to use if the number of future observations
  (\code{k}) is greater than 1.  The possible values are \code{method="Bonferroni"}
  (approximate method based on Bonferonni inequality; the default), and \cr
  \code{method="exact"} (exact method due to Dunnett, 1955).  See the DETAILS section
  of \code{\link{predIntNormK}} for more information.
  This argument is ignored if \code{k=1}.
}
  \item{pi.type}{
  character string indicating what kind of prediction interval to compute.
  The possible values are \code{pi.type="two-sided"} (the default),
  \code{pi.type="lower"}, and \code{pi.type="upper"}.
}
  \item{conf.level}{
  a scalar between 0 and 1 indicating the confidence level of the prediction interval.
  The default value is \code{conf.level=0.95}.
}
  \item{est.arg.list}{
  for \code{predIntLnormAlt}, a list containing arguments to pass to the function \cr
  \code{\link{elnormAlt}} for estimating the mean and coefficient of variation.
  The default value is \code{est.arg.list=NULL}, which implies the default values
  will be used in the call to \code{\link{elnormAlt}}.
}
}
\details{
  The function \code{predIntLnorm} returns a prediction interval as well as
  estimates of the meanlog and sdlog parameters.
  The function \code{predIntLnormAlt} returns a prediction interval as well as
  estimates of the mean and coefficient of variation.

  A prediction interval for a lognormal distribution is constructed by taking the
  natural logarithm of the observations and constructing a prediction interval
  based on the normal (Gaussian) distribution by calling \code{\link{predIntNorm}}.
  These prediction limits are then exponentiated to produce a prediction interval on
  the original scale of the data.
}
\value{
  If \code{x} is a numeric vector, a list of class
  \code{"estimate"} containing the estimated parameters, the prediction interval,
  and other information.  See the help file for \cr
  \code{\link{estimate.object}} for details.

  If \code{x} is the result of calling an estimation function,
  \code{predIntLnorm} returns a list whose class is the same as \code{x}.
  The list contains the same components as \code{x}, as well as a component called
  \code{interval} containing the prediction interval information.
  If \code{x} already has a component called \code{interval}, this component is
  replaced with the prediction interval information.
}
\references{
  Berthouex, P.M., and L.C. Brown. (2002). \emph{Statistics for Environmental Engineers}.
  Lewis Publishers, Boca Raton.

  Dunnett, C.W. (1955). A Multiple Comparisons Procedure for Comparing Several Treatments
  with a Control. \emph{Journal of the American Statistical Association} \bold{50}, 1096-1121.

  Dunnett, C.W. (1964). New Tables for Multiple Comparisons with a Control.
  \emph{Biometrics} \bold{20}, 482-491.

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

  Hahn, G.J., and W.Q. Meeker. (1991). \emph{Statistical Intervals: A Guide for Practitioners}.
  John Wiley and Sons, New York.

  Hahn, G., and W. Nelson. (1973). A Survey of Prediction Intervals and Their Applications.
  \emph{Journal of Quality Technology} \bold{5}, 178-188.

  Helsel, D.R., and R.M. Hirsch. (1992). \emph{Statistical Methods in Water Resources Research}.
  Elsevier, New York.

  Helsel, D.R., and R.M. Hirsch. (2002). \emph{Statistical Methods in Water Resources}.
  Techniques of Water Resources Investigations, Book 4, chapter A3. U.S. Geological Survey.
  (available on-line at:  \url{https://pubs.usgs.gov/tm/04/a03/tm4a3.pdf}).

  Krishnamoorthy K., and T. Mathew. (2009).
  \emph{Statistical Tolerance Regions: Theory, Applications, and Computation}.
  John Wiley and Sons, Hoboken.

  Millard, S.P., and Neerchal, N.K. (2001). \emph{Environmental Statistics with S-PLUS}.
  CRC Press, Boca Raton, Florida.

  Miller, R.G. (1981a). \emph{Simultaneous Statistical Inference}. McGraw-Hill, New York.

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
  Prediction and tolerance intervals have long been applied to quality control and
  life testing problems (Hahn, 1970b,c; Hahn and Nelson, 1973; Krishnamoorthy and Mathew, 2009).
  In the context of environmental statistics, prediction intervals are useful for
  analyzing data from groundwater detection monitoring programs at hazardous and
  solid waste facilities (e.g., Gibbons et al., 2009; Millard and Neerchal, 2001;
  USEPA, 2009).
}
\seealso{
  \code{\link{elnorm}}, \code{\link{elnormAlt}},
  \code{\link{predIntNorm}}, \code{\link{predIntNormK}},
  \code{\link{predIntLnormSimultaneous}}, \cr
  \code{\link{predIntLnormAltSimultaneous}},
  \code{\link{tolIntLnorm}},  \code{\link{tolIntLnormAlt}},
  \link{Lognormal}, \code{\link{estimate.object}}.
}
\examples{
  # Generate 20 observations from a lognormal distribution with parameters
  # meanlog=0 and sdlog=1.  The exact two-sided 90% prediction interval for
  # k=1 future observation is given by: [exp(-1.645), exp(1.645)] = [0.1930, 5.181].
  # Use predIntLnorm to estimate the distribution parameters, and construct a
  # two-sided 90% prediction interval.
  # (Note: the call to set.seed simply allows you to reproduce this example.)

  set.seed(47)
  dat <- rlnorm(20, meanlog = 0, sdlog = 1)
  predIntLnorm(dat, conf = 0.9)

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            Lognormal
  #
  #Estimated Parameter(s):          meanlog = -0.1035722
  #                                 sdlog   =  0.9106429
  #
  #Estimation Method:               mvue
  #
  #Data:                            dat
  #
  #Sample Size:                     20
  #
  #Prediction Interval Method:      exact
  #
  #Prediction Interval Type:        two-sided
  #
  #Confidence Level:                90%
  #
  #Number of Future Observations:   1
  #
  #Prediction Interval:             LPL = 0.1795898
  #                                 UPL = 4.5264399

  #----------

  # Repeat the above example, but do it in two steps.
  # First create a list called est.list containing information about the
  # estimated parameters, then create the prediction interval.

  est.list <- elnorm(dat)

  est.list
  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            Lognormal
  #
  #Estimated Parameter(s):          meanlog = -0.1035722
  #                                 sdlog   =  0.9106429
  #
  #Estimation Method:               mvue
  #
  #Data:                            dat
  #
  #Sample Size:                     20

  predIntLnorm(est.list, conf = 0.9)
  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            Lognormal
  #
  #Estimated Parameter(s):          meanlog = -0.1035722
  #                                 sdlog   =  0.9106429
  #
  #Estimation Method:               mvue
  #
  #Data:                            dat
  #
  #Sample Size:                     20
  #
  #Prediction Interval Method:      exact
  #
  #Prediction Interval Type:        two-sided
  #
  #Confidence Level:                90%
  #
  #Number of Future Observations:   1
  #
  #Prediction Interval:             LPL = 0.1795898
  #                                 UPL = 4.5264399

  #----------

  # Using the same data from the first example, create a one-sided
  # upper 99% prediction limit for the next 3 geometric means of order 2
  # (i.e., each of the 3 future geometric means is based on a sample size
  # of 2 future observations).

  predIntLnorm(dat, n.geomean = 2, k = 3, conf.level = 0.99,
    pi.type = "upper")

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            Lognormal
  #
  #Estimated Parameter(s):          meanlog = -0.1035722
  #                                 sdlog   =  0.9106429
  #
  #Estimation Method:               mvue
  #
  #Data:                            dat
  #
  #Sample Size:                     20
  #
  #Prediction Interval Method:      Bonferroni
  #
  #Prediction Interval Type:        upper
  #
  #Confidence Level:                99%
  #
  #Number of Future
  #Geometric Means:                 3
  #
  #Sample Size for
  #Geometric Means:                 2
  #
  #Prediction Interval:             LPL = 0.000000
  #                                 UPL = 7.047571

  #----------

  # Compare the result above that is based on the Bonferroni method
  # with the exact method

  predIntLnorm(dat, n.geomean = 2, k = 3, conf.level = 0.99,
    pi.type = "upper", method = "exact")$interval$limits["UPL"]

  #    UPL
  #7.00316

  #----------

  # Clean up
  rm(dat, est.list)

  #--------------------------------------------------------------------

  # Example 18-2 of USEPA (2009, p.18-15) shows how to construct a 99%
  # upper prediction interval for the log-scale mean of 4 future observations
  # (future mean of order 4) assuming a lognormal distribution based on
  # chrysene concentrations (ppb) in groundwater at 2 background wells.
  # Data were collected once per month over 4 months at the 2 background
  # wells, and also at a compliance well.
  # The question to be answered is whether there is evidence of
  # contamination at the compliance well.

  # Here we will follow the example, but look at the geometric mean
  # instead of the log-scale mean.

  #----------

  # The data for this example are stored in EPA.09.Ex.18.2.chrysene.df.

  EPA.09.Ex.18.2.chrysene.df

  #   Month   Well  Well.type Chrysene.ppb
  #1      1 Well.1 Background          6.9
  #2      2 Well.1 Background         27.3
  #3      3 Well.1 Background         10.8
  #4      4 Well.1 Background          8.9
  #5      1 Well.2 Background         15.1
  #6      2 Well.2 Background          7.2
  #7      3 Well.2 Background         48.4
  #8      4 Well.2 Background          7.8
  #9      1 Well.3 Compliance         68.0
  #10     2 Well.3 Compliance         48.9
  #11     3 Well.3 Compliance         30.1
  #12     4 Well.3 Compliance         38.1

  Chrysene.bkgd <- with(EPA.09.Ex.18.2.chrysene.df,
    Chrysene.ppb[Well.type == "Background"])
  Chrysene.cmpl <- with(EPA.09.Ex.18.2.chrysene.df,
    Chrysene.ppb[Well.type == "Compliance"])


  #----------

  # A Shapiro-Wilks goodness-of-fit test for normality indicates
  # we should reject the assumption of normality and assume a
  # lognormal distribution for the background well data:

  gofTest(Chrysene.bkgd)

  #Results of Goodness-of-Fit Test
  #-------------------------------
  #
  #Test Method:                     Shapiro-Wilk GOF
  #
  #Hypothesized Distribution:       Normal
  #
  #Estimated Parameter(s):          mean = 16.55000
  #                                 sd   = 14.54441
  #
  #Estimation Method:               mvue
  #
  #Data:                            Chrysene.bkgd
  #
  #Sample Size:                     8
  #
  #Test Statistic:                  W = 0.7289006
  #
  #Test Statistic Parameter:        n = 8
  #
  #P-value:                         0.004759859
  #
  #Alternative Hypothesis:          True cdf does not equal the
  #                                 Normal Distribution.

  gofTest(Chrysene.bkgd, dist = "lnorm")

  #Results of Goodness-of-Fit Test
  #-------------------------------
  #
  #Test Method:                     Shapiro-Wilk GOF
  #
  #Hypothesized Distribution:       Lognormal
  #
  #Estimated Parameter(s):          meanlog = 2.5533006
  #                                 sdlog   = 0.7060038
  #
  #Estimation Method:               mvue
  #
  #Data:                            Chrysene.bkgd
  #
  #Sample Size:                     8
  #
  #Test Statistic:                  W = 0.8546352
  #
  #Test Statistic Parameter:        n = 8
  #
  #P-value:                         0.1061057
  #
  #Alternative Hypothesis:          True cdf does not equal the
  #                                 Lognormal Distribution.

  #----------

  # Here is the one-sided 99% upper prediction limit for
  # a geometric mean based on 4 future observations:

  predIntLnorm(Chrysene.bkgd, n.geomean = 4, k = 1,
    conf.level = 0.99, pi.type = "upper")

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            Lognormal
  #
  #Estimated Parameter(s):          meanlog = 2.5533006
  #                                 sdlog   = 0.7060038
  #
  #Estimation Method:               mvue
  #
  #Data:                            Chrysene.bkgd
  #
  #Sample Size:                     8
  #
  #Prediction Interval Method:      exact
  #
  #Prediction Interval Type:        upper
  #
  #Confidence Level:                99%
  #
  #Number of Future
  #Geometric Means:                 1
  #
  #Sample Size for
  #Geometric Means:                 4
  #
  #Prediction Interval:             LPL =  0.00000
  #                                 UPL = 46.96613

  UPL <- predIntLnorm(Chrysene.bkgd, n.geomean = 4, k = 1,
    conf.level = 0.99, pi.type = "upper")$interval$limits["UPL"]

  UPL
  #     UPL
  #46.96613

  # Is there evidence of contamination at the compliance well?

  geoMean(Chrysene.cmpl)
  #[1] 44.19034

  # Since the geometric mean at the compliance well is less than
  # the upper prediction limit, there is no evidence of contamination.

  #----------

  # Cleanup
  #--------

  rm(Chrysene.bkgd, Chrysene.cmpl, UPL)
}
\keyword{ distribution }
\keyword{ htest }
