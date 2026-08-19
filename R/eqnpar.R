#' Estimate Quantiles of a Distribution Nonparametrically
#' @description
#' Estimate quantiles of a distribution, and optionally create confidence intervals
#'   for them, without making any assumptions about the form of the distribution.
#' @usage
#' eqnpar(x, p = 0.5, type = 7, ci = FALSE, lcl.rank = NULL, ucl.rank = NULL,
#'     lb = -Inf, ub = Inf, ci.type = "two-sided",
#'     ci.method = "interpolate", digits = getOption("digits"),
#'     approx.conf.level = 0.95, min.coverage = TRUE, tol = 0)
#' @rawRd
#' \encoding{latin1}
#' @rawRd
#' \arguments{
#'   \item{x}{
#'   a numeric vector of observations.  Missing (\code{NA}), undefined (\code{NaN}), and
#'   infinite (\code{Inf}, \code{-Inf}) values are allowed but will be removed.
#' }
#'   \item{p}{
#'   numeric vector of probabilities for which quantiles will be estimated.
#'   All values of \code{p} must be between 0 and 1.  When \code{ci=TRUE}, \code{p}
#'   must be a scalar. The default value is \code{p=0.5}.
#' }
#'   \item{type}{
#'   an integer between 1 and 9 indicating which algorithm to use to estimate the
#'   quantile.  The default value is \code{type=7}.  See the help file for
#'   \code{\link[stats]{quantile}} for details.
#' }
#'   \item{ci}{
#'   logical scalar indicating whether to compute a confidence interval for the quantile.
#'   The default value is \code{ci=FALSE}.
#' }
#'   \item{lcl.rank, ucl.rank}{
#'   positive integers indicating the ranks of the order statistics that are used
#'   for the lower and upper bounds of the confidence interval for the specified
#'   quantile.  Both arguments must be integers between 1 and the number of non-missing
#'   values in \code{x}, and \code{lcl.rank} must be strictly less than \code{ucl.rank}.
#'   Setting values for \code{lcl.rank} and/or \code{ucl.rank} allows the user to
#'   bypass the automatic selection of order statistics.  By default the value of these
#'   arguments is \code{NULL}, in which case order statistics are chosen based on the
#'   value of \code{ci.type} and \code{ci.method}.  If only \code{lcl.rank} is supplied,
#'   a lower confidence interval is constructed.  If only \code{ucl.rank} is supplied,
#'   an upper confidence interval is constructed.  If both \code{lcl.rank} and
#'   \code{ucl.rank} are supplied, a two-sided confidence interval is constructed.
#'   These arguments are ignored if \code{ci=FALSE}.
#' }
#'   \item{lb, ub}{
#'   scalars indicating lower and upper bounds on the distribution.  By default, \cr
#'   \code{lb=-Inf} and \code{ub=Inf}.  If you are constructing a confidence interval
#'   for a quantile from a distribution that you know has a lower bound other than
#'   \code{-Inf} (e.g., \code{0}), set \code{lb} to this value.  Similarly, if you
#'   know the distribution has an upper bound other than \code{Inf}, set
#'   \code{ub} to this value.  These arguments are ignored if \code{ci=FALSE}.
#' }
#'   \item{ci.type}{
#'   character string indicating what kind of confidence interval to compute.
#'   The possible values are \code{"two-sided"} (the default), \code{"lower"}, and
#'   \code{"upper"}.  This argument is ignored if \code{ci=FALSE}, or
#'   \code{lcl.rank} and/or \code{ucl.rank} are supplied.
#' }
#'   \item{ci.method}{
#'   character string indicating the method to use to construct the confidence interval.
#'   The possible values are \code{"interpolate"} (the default), \code{"exact"}, and
#'   \code{"normal.approx"}.  See the DETAILS section for more information on these methods.
#'   This argument is ignored if \code{ci=FALSE}, or \code{lcl.rank} and/or
#'   \code{ucl.rank} are supplied.
#' }
#'   \item{digits}{
#'   an integer indicating the number of decimal places to round to when printing out
#'   the value of \code{100*p}. The default value is \code{digits=0}.
#' }
#'   \item{approx.conf.level}{
#'   a scalar between 0 and 1 indicating the desired confidence level of the confidence
#'   interval.  The default value is \code{0.95}.  The true confidence level usually
#'   will not be exactly equal to \code{approx.conf.level} (see DETAILS).  This argument
#'   is ignored if \code{ci=FALSE}, or \code{lcl.rank} and/or \code{ucl.rank} are supplied.
#' }
#'   \item{min.coverage}{
#'   for the case when \code{ci.method="exact"}, a logical scalar indicating whether the
#'   confidence interval should have a minimum coverage at least as great as the value
#'   of the argument \code{approx.conf.level}. The default value is \cr
#'   \code{min.coverage=TRUE}.  This argument is ignored if \code{ci=FALSE} or
#'   \code{ci.method} is not equal to \code{"exact"}.
#' }
#'   \item{tol}{
#'   for the case when \code{ci.method="exact"} and \code{min.coverage=FALSE},
#'   a scalar between 0 and 1 indicating the maximum amount of coverage greater than
#'   the value of \code{approx.conf.level} the user is willing to allow.
#'   The default value is \code{tol=0}.
#' }
#' }
#' @details
#' This help page has been shortened to keep function help focused on usage,
#' arguments, return values, and examples. Extended method details, formulas,
#' and background material are available in \code{vignette("extended-function-details", package = "EnvStats")}, section \code{eqnpar}.
#' @rawRd
#' \value{
#'   a list of class \code{"estimate"} containing the estimated quantile(s) and other
#'   information.  See \cr
#'   \code{\link{estimate.object}} for details.
#' }
#' @rawRd
#' \references{
#'   Berthouex, P.M., and L.C. Brown. (2002). \emph{Statistics for Environmental Engineers}.
#'   Lewis Publishers, Boca Raton.
#'
#'   Conover, W.J. (1980). \emph{Practical Nonparametric Statistics}. Second Edition.
#'   John Wiley and Sons, New York.
#'
#'   Gilbert, R.O. (1987). \emph{Statistical Methods for Environmental Pollution Monitoring}.
#'   Van Nostrand Reinhold, New York, NY, pp.132-136.
#'
#'   Helsel, D.R., and R.M. Hirsch. (1992). \emph{Statistical Methods in Water Resources Research}.
#'   Elsevier, New York, NY, pp.88-90.
#'
#'   Hettmansperger, T.P., and Sheather, S.J. (1986).  Confidence Intervals Based on
#'   Interpolated Order Statistics.  \emph{Statistics & Probability Letters}, \bold{4}, 75--79.
#'
#'   Nyblom, J. (1992).  Note on Interpolated Order Statistics.
#'   \emph{Statistics & Probability Letters}, \bold{14}, 129--131.
#'
#'   USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
#'   EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.
#'   U.S. Environmental Protection Agency, Washington, D.C.
#'
#'   Zar, J.H. (2010). \emph{Biostatistical Analysis}. Fifth Edition.
#'   Prentice-Hall, Upper Saddle River, NJ.
#' }
#' @rawRd
#' \author{
#'     Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#'
#'     The author is grateful to Michael \enc{H?hle}{Hoehle},
#'     Department of Mathematics, Stockholm University
#'     (\url{http://www2.math.su.se/~hoehle}) for making me aware of the work of Nyblom (1992),
#'     and for suggesting improvements to the algorithm that was used in \pkg{EnvStats}
#'     Version 2.1.1 to construct a confidence interval when \code{ci.method="exact"}.
#' }
#' @rawRd
#' \note{
#'   Percentiles are sometimes used in environmental standards and regulations.  For example,
#'   Berthouex and Brown (2002, p.71) note that England has water quality limits based on
#'   the 90th and 95th percentiles of monitoring data not exceeding specified levels.  They also
#'   note that the U.S. EPA has specifications for air quality monitoring, aquatic standards on
#'   toxic chemicals, and maximum daily limits for industrial effluents that are all based on
#'   percentiles.  Given the importance of these quantities, it is essential to characterize
#'   the amount of uncertainty associated with the estimates of these quantities.
#'   This is done with confidence intervals.
#'
#'   It can be shown (e.g., Conover, 1980, pp.119-121) that an upper confidence interval for the
#'   \eqn{p}'th quantile with confidence level \eqn{100(1-\alpha)\%} is equivalent to an
#'   upper \eqn{\beta}-content tolerance interval with coverage \eqn{100p\%} and confidence level
#'   \eqn{100(1-\alpha)\%}.  Also, a lower confidence interval for the \eqn{p}'th quantile with
#'   confidence level \eqn{100(1-\alpha)\%} is equivalent to a lower \eqn{\beta}-content tolerance
#'   interval with coverage \eqn{100(1-p)\%} and confidence level \eqn{100(1-\alpha)\%}.  See the
#'   help file for \code{\link{tolIntNpar}} for more information on nonparametric tolerance
#'   intervals.
#' }
#' @rawRd
#' \seealso{
#'   \code{\link[stats]{quantile}}, \code{\link{tolIntNpar}},
#'   \link[=FcnsByCatEstDistQuants]{Estimating Distribution Quantiles},
#'   \link[=FcnsByCatTolInts]{Tolerance Intervals}, \code{\link{estimate.object}}.
#' }
#' @rawRd
#' \examples{
#'
#'   # The data frame ACE.13.TCE.df contains observations on
#'   # Trichloroethylene (TCE) concentrations (mg/L) at
#'   # 10 groundwater monitoring wells before and after remediation.
#'   #
#'   # Compute the median concentration for each period along with
#'   # a 95\% confidence interval for the median.
#'   #
#'   # Before remediation:  20.3 [8.8, 35.9]
#'   # After  remediation:   2.5 [0.8,  5.9]
#'
#'   with(ACE.13.TCE.df,
#'     eqnpar(TCE.mg.per.L[Period=="Before"], ci = TRUE))
#'
#'   #Results of Distribution Parameter Estimation
#'   #--------------------------------------------
#'
#'   #Assumed Distribution:            None
#'
#'   #Estimated Quantile(s):           Median = 20.3
#'
#'   #Quantile Estimation Method:      Nonparametric
#'
#'   #Data:                            TCE.mg.per.L[Period == "Before"]
#'
#'   #Sample Size:                     10
#'
#'   #Confidence Interval for:         50'th %ile
#'
#'   #Confidence Interval Method:      interpolate (Nyblom, 1992)
#'
#'   #Confidence Interval Type:        two-sided
#'
#'   #Confidence Level:                95%
#'
#'   #Confidence Limit Rank(s):        2 9
#'   #                                 3 8
#'
#'   #Confidence Interval:             LCL =  8.804775
#'   #                                 UCL = 35.874775
#'
#'   #----------
#'
#'   with(ACE.13.TCE.df, eqnpar(TCE.mg.per.L[Period=="After"], ci = TRUE))
#'
#'   #Results of Distribution Parameter Estimation
#'   #--------------------------------------------
#'
#'   #Assumed Distribution:            None
#'
#'   #Estimated Quantile(s):           Median = 2.48
#'
#'   #Quantile Estimation Method:      Nonparametric
#'
#'   #Data:                            TCE.mg.per.L[Period == "After"]
#'
#'   #Sample Size:                     10
#'
#'   #Confidence Interval for:         50'th %ile
#'
#'   #Confidence Interval Method:      interpolate (Nyblom, 1992)
#'
#'   #Confidence Interval Type:        two-sided
#'
#'   #Confidence Level:                95%
#'
#'   #Confidence Limit Rank(s):        2 9
#'   #                                 3 8
#'
#'   #Confidence Interval:             LCL = 0.7810901
#'   #                                 UCL = 5.8763063
#'
#'   #==========
#'
#'   # Generate 20 observations from a cauchy distribution with parameters
#'   # location=0, scale=1.  The true 75th percentile of this distribution is 1.
#'   # Use eqnpar to estimate the 75th percentile and construct a 90% confidence interval.
#'   # (Note: the call to set.seed simply allows you to reproduce this example.)
#'
#'   set.seed(250)
#'   dat <- rcauchy(20, location = 0, scale = 1)
#'
#'   #-------------------------------------------------------
#'   # First, use the default method, ci.method="interpolate"
#'   #-------------------------------------------------------
#'
#'   eqnpar(dat, p = 0.75, ci = TRUE, approx.conf.level = 0.9)
#'
#'   #Results of Distribution Parameter Estimation
#'   #--------------------------------------------
#'   #
#'   #Assumed Distribution:            None
#'   #
#'   #Estimated Quantile(s):           75'th %ile = 1.524903
#'   #
#'   #Quantile Estimation Method:      Nonparametric
#'   #
#'   #Data:                            dat
#'   #
#'   #Sample Size:                     20
#'   #
#'   #Confidence Interval for:         75'th %ile
#'   #
#'   #Confidence Interval Method:      interpolate (Nyblom, 1992)
#'   #
#'   #Confidence Interval Type:        two-sided
#'   #
#'   #Confidence Level:                90%
#'   #
#'   #Confidence Limit Rank(s):        12 19
#'   #                                 13 18
#'   #
#'   #Confidence Interval:             LCL = 0.8191423
#'   #                                 UCL = 2.1215570
#'
#'   #----------
#'
#'   #-------------------------------------------------------------
#'   # Now use ci.method="exact".
#'   # Note that the returned confidence level is greater than 90%.
#'   #-------------------------------------------------------------
#'
#'   eqnpar(dat, p = 0.75, ci = TRUE, approx.conf.level = 0.9,
#'     ci.method = "exact")
#'
#'   #Results of Distribution Parameter Estimation
#'   #--------------------------------------------
#'   #
#'   #Assumed Distribution:            None
#'   #
#'   #Estimated Quantile(s):           75'th %ile = 1.524903
#'   #
#'   #Quantile Estimation Method:      Nonparametric
#'   #
#'   #Data:                            dat
#'   #
#'   #Sample Size:                     20
#'   #
#'   #Confidence Interval for:         75'th %ile
#'   #
#'   #Confidence Interval Method:      exact
#'   #
#'   #Confidence Interval Type:        two-sided
#'   #
#'   #Confidence Level:                93.47622%
#'   #
#'   #Confidence Limit Rank(s):        12 19
#'   #
#'   #Confidence Interval:             LCL = 0.7494692
#'   #                                 UCL = 2.2156601
#'
#'   #----------
#'
#'   #----------------------------------------------------------
#'   # Now use ci.method="exact" with min.coverage=FALSE.
#'   # Note that the returned confidence level is less than 90%.
#'   #----------------------------------------------------------
#'
#'   eqnpar(dat, p = 0.75, ci = TRUE, approx.conf.level = 0.9,
#'     ci.method = "exact", min.coverage = FALSE, )
#'
#'   #Results of Distribution Parameter Estimation
#'   #--------------------------------------------
#'   #
#'   #Assumed Distribution:            None
#'   #
#'   #Estimated Quantile(s):           75'th %ile = 1.524903
#'   #
#'   #Quantile Estimation Method:      Nonparametric
#'   #
#'   #Data:                            dat
#'   #
#'   #Sample Size:                     20
#'   #
#'   #Confidence Interval for:         75'th %ile
#'   #
#'   #Confidence Interval Method:      exact
#'   #
#'   #Confidence Interval Type:        two-sided
#'   #
#'   #Confidence Level:                89.50169%
#'   #
#'   #Confidence Limit Rank(s):        13 20
#'   #
#'   #Confidence Interval:             LCL = 1.018038
#'   #                                 UCL = 5.002399
#'
#'   #----------
#'
#'   #-----------------------------------------------------------
#'   # Now supply our own bounds for the confidence interval.
#'   # The first example above based on the Interpolate method
#'   # used lcl.rank=12, ucl.rank=19 and lcl.rank=13, ucl.rank=18
#'   # and interpolated between these two confidence intervals.
#'   # Here we will specify lcl.rank=13 and ucl.rank=18.  The
#'   # resulting confidence level is 81%.
#'   #-----------------------------------------------------------
#'
#'   eqnpar(dat, p = 0.75, ci = TRUE, lcl.rank = 13, ucl.rank = 18)
#'
#'   #Results of Distribution Parameter Estimation
#'   #--------------------------------------------
#'   #
#'   #Assumed Distribution:            None
#'   #
#'   #Estimated Quantile(s):           75'th %ile = 1.524903
#'   #
#'   #Quantile Estimation Method:      Nonparametric
#'   #
#'   #Data:                            dat
#'   #
#'   #Sample Size:                     20
#'   #
#'   #Confidence Interval for:         75'th %ile
#'   #
#'   #Confidence Interval Method:      exact
#'   #
#'   #Confidence Interval Type:        two-sided
#'   #
#'   #Confidence Level:                80.69277%
#'   #
#'   #Confidence Limit Rank(s):        13 18
#'   #
#'   #Confidence Interval:             LCL = 1.018038
#'   #                                 UCL = 2.071172
#'
#'   #----------
#'
#'   # Clean up
#'   rm(dat)
#'
#'   #==========
#'
#'   # Modify Example 17-4 on page 17-21 of USEPA (2009).  This example uses
#'   # copper concentrations (ppb) from 3 background wells to set an upper
#'   # limit for 2 compliance wells.  Here we will attempt to compute an upper
#'   # 95% confidence interval for the 95'th percentile of the distribution of
#'   # copper concentrations in the background wells.
#'   #
#'   # The data are stored in EPA.92c.copper2.df.
#'   #
#'   # Note that even though these data are Type I left singly censored,
#'   # it is still possible to compute an estimate of the 95'th percentile.
#'
#'   EPA.92c.copper2.df
#'   #   Copper.orig Copper Censored Month Well  Well.type
#'   #1           <5    5.0     TRUE     1    1 Background
#'   #2           <5    5.0     TRUE     2    1 Background
#'   #3          7.5    7.5    FALSE     3    1 Background
#'   #...
#'   #9          9.2    9.2    FALSE     1    2 Background
#'   #10          <5    5.0     TRUE     2    2 Background
#'   #11          <5    5.0     TRUE     3    2 Background
#'   #...
#'   #17          <5    5.0     TRUE     1    3 Background
#'   #18         5.4    5.4    FALSE     2    3 Background
#'   #19         6.7    6.7    FALSE     3    3 Background
#'   #...
#'   #29         6.2    6.2    FALSE     5    4 Compliance
#'   #30          <5    5.0     TRUE     6    4 Compliance
#'   #31         7.8    7.8    FALSE     7    4 Compliance
#'   #...
#'   #38          <5    5.0     TRUE     6    5 Compliance
#'   #39         5.6    5.6    FALSE     7    5 Compliance
#'   #40          <5    5.0     TRUE     8    5 Compliance
#'
#'   # Because of the small sample size of n=24 observations, it is not possible
#'   # to create a nonparametric confidence interval for the 95th percentile
#'   # that has an associated confidence level of 95%.  If we tried to do this,
#'   # we would get an error message:
#'
#'   # with(EPA.92c.copper2.df,
#'   #    eqnpar(Copper[Well.type=="Background"], p = 0.95, ci = TRUE, lb = 0,
#'   #      ci.type = "upper", approx.conf.level = 0.95))
#'   #
#'   #Error in ci.qnpar.interpolate(x = c(5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,  :
#'   #  Minimum coverage of 0.95 is not possible with the given sample size.
#'
#'   # So instead, we will use ci.method="exact" with min.coverage=FALSE
#'   # to construct the confidence interval.  Note that the associated
#'   # confidence level is only 71%.
#'
#'   with(EPA.92c.copper2.df,
#'      eqnpar(Copper[Well.type=="Background"], p = 0.95, ci = TRUE,
#'         ci.method = "exact", min.coverage = FALSE,
#'         ci.type = "upper", lb = 0,
#'         approx.conf.level = 0.95))
#'
#'   #Results of Distribution Parameter Estimation
#'   #--------------------------------------------
#'   #
#'   #Assumed Distribution:            None
#'   #
#'   #Estimated Quantile(s):           95'th %ile = 7.925
#'   #
#'   #Quantile Estimation Method:      Nonparametric
#'   #
#'   #Data:                            Copper[Well.type == "Background"]
#'   #
#'   #Sample Size:                     24
#'   #
#'   #Confidence Interval for:         95'th %ile
#'   #
#'   #Confidence Interval Method:      exact
#'   #
#'   #Confidence Interval Type:        upper
#'   #
#'   #Confidence Level:                70.8011%
#'   #
#'   #Confidence Limit Rank(s):        NA 24
#'   #
#'   #Confidence Interval:             LCL = 0.0
#'   #                                 UCL = 9.2
#'
#'   #----------
#'
#'   # For the above example, the true confidence level is 71\% instead of 95\%.
#'   # This is a function of the small sample size.  In fact, as Example 17-4 on
#'   # pages 17-21 of USEPA (2009) shows, the largest quantile for which you can
#'   # construct a nonparametric confidence interval that will have associated
#'   # confidence level of 95\% is the 88'th percentile:
#'
#'   with(EPA.92c.copper2.df,
#'     eqnpar(Copper[Well.type=="Background"], p = 0.88, ci = TRUE,
#'       ci.type = "upper", lb = 0, ucl.rank = 24,
#'       approx.conf.level = 0.95))
#'
#'   #Results of Distribution Parameter Estimation
#'   #--------------------------------------------
#'   #
#'   #Assumed Distribution:            None
#'   #
#'   #Estimated Quantile(s):           88'th %ile = 6.892
#'   #
#'   #Quantile Estimation Method:      Nonparametric
#'   #
#'   #Data:                            Copper[Well.type == "Background"]
#'   #
#'   #Sample Size:                     24
#'   #
#'   #Confidence Interval for:         88'th %ile
#'   #
#'   #Confidence Interval Method:      exact
#'   #
#'   #Confidence Interval Type:        upper
#'   #
#'   #Confidence Level:                95.3486%
#'   #
#'   #Confidence Limit Rank(s):        NA 24
#'   #
#'   #Confidence Interval:             LCL = 0.0
#'   #                                 UCL = 9.2
#'
#'   #==========
#'
#'   # Reproduce Example 21-6 on pages 21-21 to 21-22 of USEPA (2009).
#'   # Use 12 measurements of nitrate (mg/L) at a well used for drinking water
#'   # to determine with 95\% confidence whether or not the infant-based, acute
#'   # risk standard of 10 mg/L has been violated.  Assume that the risk
#'   # standard represents an upper 95'th percentile limit on nitrate
#'   # concentrations.  So what we need to do is construct a one-sided
#'   # lower nonparametric confidence interval for the 95'th percentile
#'   # that has associated confidence level of no more than 95%, and we will
#'   # compare the lower confidence limit with the MCL of 10 mg/L.
#'   #
#'   # The data for this example are stored in EPA.09.Ex.21.6.nitrate.df.
#'
#'   # Look at the data:
#'   #------------------
#'
#'   EPA.09.Ex.21.6.nitrate.df
#'   #   Sampling.Date       Date Nitrate.mg.per.l.orig Nitrate.mg.per.l Censored
#'   #1      7/28/1999 1999-07-28                  <5.0              5.0     TRUE
#'   #2       9/3/1999 1999-09-03                  12.3             12.3    FALSE
#'   #3     11/24/1999 1999-11-24                  <5.0              5.0     TRUE
#'   #4       5/3/2000 2000-05-03                  <5.0              5.0     TRUE
#'   #5      7/14/2000 2000-07-14                   8.1              8.1    FALSE
#'   #6     10/31/2000 2000-10-31                  <5.0              5.0     TRUE
#'   #7     12/14/2000 2000-12-14                    11             11.0    FALSE
#'   #8      3/27/2001 2001-03-27                  35.1             35.1    FALSE
#'   #9      6/13/2001 2001-06-13                  <5.0              5.0     TRUE
#'   #10     9/16/2001 2001-09-16                  <5.0              5.0     TRUE
#'   #11    11/26/2001 2001-11-26                   9.3              9.3    FALSE
#'   #12      3/2/2002 2002-03-02                  10.3             10.3    FALSE
#'
#'   # Determine what order statistic to use for the lower confidence limit
#'   # in order to achieve no more than 95% confidence.
#'   #---------------------------------------------------------------------
#'
#'   conf.levels <- ciNparConfLevel(n = 12, p = 0.95, lcl.rank = 1:12,
#'     ci.type = "lower")
#'   names(conf.levels) <- 1:12
#'
#'   round(conf.levels, 2)
#'   #   1    2    3    4    5    6    7    8    9   10   11   12
#'   #1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00 0.98 0.88 0.54
#'
#'   # Using the 11'th largest observation for the lower confidence limit
#'   # yields a confidence level of 88%.  Using the 10'th largest
#'   # observation yields a confidence level of 98%.  The example in
#'   # USEPA (2009) uses the 10'th largest observation.
#'   #
#'   # The 10'th largest observation is 11 mg/L which exceeds the
#'   # MCL of 10 mg/L, so there is evidence of contamination.
#'   #--------------------------------------------------------------------
#'
#'   with(EPA.09.Ex.21.6.nitrate.df,
#'     eqnpar(Nitrate.mg.per.l, p = 0.95, ci = TRUE,
#'       ci.type = "lower", lcl.rank = 10))
#'
#'   #Results of Distribution Parameter Estimation
#'   #--------------------------------------------
#'   #
#'   #Assumed Distribution:            None
#'   #
#'   #Estimated Quantile(s):           95'th %ile = 22.56
#'   #
#'   #Quantile Estimation Method:      Nonparametric
#'   #
#'   #Data:                            Nitrate.mg.per.l
#'   #
#'   #Sample Size:                     12
#'   #
#'   #Confidence Interval for:         95'th %ile
#'   #
#'   #Confidence Interval Method:      exact
#'   #
#'   #Confidence Interval Type:        lower
#'   #
#'   #Confidence Level:                98.04317%
#'   #
#'   #Confidence Limit Rank(s):        10 NA
#'   #
#'   #Confidence Interval:             LCL =  11
#'   #                                 UCL = Inf
#'
#'   #==========
#'
#'   # Clean up
#'   #---------
#'
#'   rm(conf.levels)
#' }
#' @rawRd
#' \keyword{ distribution }
#' @rawRd
#' \keyword{ htest }

eqnpar <-
function (x, p = 0.5, type = 7, ci = FALSE, lcl.rank = NULL, 
    ucl.rank = NULL, lb = -Inf, ub = Inf, ci.type = "two-sided", 
    ci.method = "interpolate", digits = getOption("digits"), 
    approx.conf.level = 0.95, min.coverage = TRUE, tol = 0) 
{
    if (!is.vector(x, mode = "numeric") & !is.factor(x)) 
        stop("'x' must be a numeric vector or a factor.")
    if (is.factor(x)) {
        x <- as.numeric(x)
        warning("x is a factor; numeric values of levels used.\n")
    }
    if (!is.vector(p, mode = "numeric")) 
        stop("'p' must be a numeric vector.")
    if (any(!is.finite(p))) 
        stop("NA/NaN/Inf values not allowed for 'p'")
    if (any(p < 0) || any(p > 1)) 
        stop("All values of 'p' must be between 0 and 1.")
    data.name <- deparse(substitute(x))
    if ((bad.obs <- sum(!(x.ok <- is.finite(x)))) > 0) {
        is.not.finite.warning(x)
        x <- x[x.ok]
        warning(paste(bad.obs, "observations with NA/NaN/Inf in 'x' removed."))
    }
    n <- length(x)
    if (n < 2 || length(unique(x)) < 2) 
        stop(paste("'x' must contain at least 2 non-missing distinct values. ", 
            "This is not true for 'x' =", data.name))
    q <- quantile(x, p, type = type)
    if (length(p) == 1 && p == 0.5) 
        names(q) <- "Median"
    else {
        pct <- round(100 * p, digits)
        names(q) <- paste(pct, number.suffix(pct), " %ile", sep = "")
    }
    ret.list <- list(distribution = "None", sample.size = n, 
        method = "Nonparametric", quantiles = q, quantile.method = "Nonparametric", 
        data.name = data.name, bad.obs = bad.obs)
    if (ci) {
        if (any(length.list(lb, ub) != 1) || lb >= ub) 
            stop(paste("'lb' and 'ub' must be scalars,", "and 'lb' must be strictly less than 'ub'"))
        if (length(p) > 1) 
            stop("When 'ci' = TRUE, 'p' must be a scalar")
        ci.type <- match.arg(ci.type, c("two-sided", "lower", 
            "upper"))
        ci.method <- match.arg(ci.method, c("interpolate", "exact", 
            "normal.approx"))
        if (!is.numeric(approx.conf.level) || length(approx.conf.level) != 
            1 || approx.conf.level <= 0 || approx.conf.level >= 
            1) 
            stop("'approx.conf.level' must be a scalar between 0 and 1")
        if (ci.type == "two-sided" && ci.method == "exact") {
            if (!is.numeric(tol) || length(tol) != 1 || tol < 
                0 || tol >= 1) 
                stop("'tol' must be a scalar between 0 and 1")
        }
        ci.obj <- ci.qnpar(x = x, p = p, lcl.rank = lcl.rank, 
            ucl.rank = ucl.rank, lb = lb, ub = ub, ci.type = ci.type, 
            ci.method = ci.method, digits = digits, approx.conf.level = approx.conf.level, 
            min.coverage = min.coverage, tol = tol)
        ret.list <- c(ret.list, list(interval = ci.obj))
    }
    oldClass(ret.list) <- "estimate"
    ret.list
}
