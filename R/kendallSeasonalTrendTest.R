#' Nonparametric Test for Monotonic Trend Within Each Season Based on Kendall's Tau Statistic
#' @aliases kendallSeasonalTrendTest.default kendallSeasonalTrendTest.formula
#' @aliases kendallSeasonalTrendTest.data.frame kendallSeasonalTrendTest.matrix
#' @description
#' Perform a nonparametric test for a monotonic trend within each season based on Kendall's tau
#'   statistic, and optionally compute a confidence interval for the slope across all seasons.
#' @usage
#' kendallSeasonalTrendTest(y, ...)
#'
#' \method{kendallSeasonalTrendTest}{formula}(y, data = NULL, subset,
#'   na.action = na.pass, ...)
#'
#' \method{kendallSeasonalTrendTest}{default}(y, season, year,
#'   alternative = "two.sided", correct = TRUE, ci.slope = TRUE, conf.level = 0.95,
#'   independent.obs = TRUE, data.name = NULL, season.name = NULL, year.name = NULL,
#'   parent.of.data = NULL, subset.expression = NULL, ...)
#'
#' \method{kendallSeasonalTrendTest}{data.frame}(y, ...)
#'
#' \method{kendallSeasonalTrendTest}{matrix}(y, ...)
#' @rawRd
#' \arguments{
#'   \item{y}{
#'   an object containing data for the trend test.  In the default method,
#'   the argument \code{y} must be numeric vector of observations.
#'   When \code{y} is a data frame, all columns must be numeric.
#'   When \code{y} is a matrix, it must be a numeric matrix.
#'   In the formula method, \code{y} must be a formula of the form
#'   \code{y ~ season + year}, where \code{y}, \code{season}, and \code{year}
#'   specify what variables to use for the these arguments in the call to
#'   \code{kendallSeasonalTrendTest.default}.  Missing (\code{NA}), undefined (\code{NaN}),
#'   and infinite (\code{Inf}, \code{-Inf}) values are allowed but will be
#'   removed.
#' }
#'   \item{data}{
#'   specifies an optional data frame, list or environment (or object coercible by
#'   \code{as.data.frame} to a data frame) containing the variables in the model.
#'   If not found in \code{data}, the variables are taken from \code{environment(formula)},
#'   typically the environment from which \code{kendallTrendTest} is called.
#' }
#'   \item{subset}{
#'   specifies an optional vector specifying a subset of observations to be used.
#' }
#'   \item{na.action}{
#'   specifies a function which indicates what should happen when the data contain \code{NA}s.
#'   The default is \code{\link{na.pass}}.
#' }
#'   \item{season}{
#'   numeric or character vector or a factor indicating the seasons in which the observations in
#'   \code{y} were taken.  The length of \code{season} must equal the length of \code{y}.
#' }
#'   \item{year}{
#'   numeric vector indicating the years in which the observations in \code{y} were taken.
#'   The length of \code{year} must equal the length of \code{y}.
#' }
#'   \item{alternative}{
#'   character string indicating the kind of alternative hypothesis.  The
#'   possible values are \code{"two.sided"} (tau not equal to 0; the default),
#'   \code{"less"} (tau less than 0), and \code{"greater"} (tau greater than 0).
#' }
#'   \item{correct}{
#'   logical scalar indicating whether to use the correction for continuity in
#'   computing the \eqn{z}-statistic that is based on the test statistic \eqn{S'}.
#'   The default value is \code{TRUE}.
#' }
#'   \item{ci.slope}{
#'   logical scalar indicating whether to compute a confidence interval for the
#'   slope.  The default value is \code{TRUE}.
#' }
#'   \item{conf.level}{
#'   numeric scalar between 0 and 1 indicating the confidence level associated
#'   with the confidence interval for the slope.  The default value is
#'   \code{0.95}.
#' }
#'   \item{independent.obs}{
#'   logical scalar indicating whether to assume the observations in \code{y} are seially independent.
#'   The default value is \code{TRUE}.
#' }
#'   \item{data.name}{
#'   character string indicating the name of the data used for the trend test.
#'   The default value is \code{deparse(substitute(y))}.
#' }
#'   \item{season.name}{
#'   character string indicating the name of the data used for the season.
#'   The default value is \code{deparse(substitute(season))}.
#' }
#'   \item{year.name}{
#'   character string indicating the name of the data used for the year.
#'   The default value is \code{deparse(substitute(year))}.
#' }
#'   \item{parent.of.data}{
#'   character string indicating the source of the data used for the trend test.
#' }
#'   \item{subset.expression}{
#'   character string indicating the expression used to subset the data.
#' }
#'   \item{\dots}{
#'   additional arguments affecting the test for trend.
#' }
#' }
#' @details
#' This help page has been shortened to keep function help focused on usage,
#' arguments, return values, and examples. Extended method details, formulas,
#' and background material are available in \code{vignette("extended-function-details", package = "EnvStats")}, section \code{kendallSeasonalTrendTest}.
#' @rawRd
#' \value{
#'   A list of class \code{"htestEnvStats"} containing the results of the hypothesis
#'   test.  See the help file for \code{\link{htestEnvStats.object}} for details.
#'   In addition, the following components are part of the list returned by \cr
#'   \code{kendallSeasonalTrendTest}:
#'
#'   \item{seasonal.S}{numeric vector.  The value of the Kendall S-statistic for each season.}
#'   \item{var.seasonal.S}{numeric vector.  The variance of the Kendall S-statistic for each season.
#'     This component only appears when \code{independent.obs=TRUE}.}
#'   \item{var.cov.seasonal.S}{numeric matrix.  The estimated variance-covariance matrix of the Kendall
#'     S-statistics for each season. This component only appears when \cr
#'     \code{independent.obs=FALSE}.}
#'   \item{seasonal.estimates}{numeric matrix.  The estimated Kendall's tau, slope, and intercept for
#'     each season.}
#' }
#' @rawRd
#' \references{
#'   Bradley, J.V. (1968). \emph{Distribution-Free Statistical Tests}.
#'   Prentice-Hall, Englewood Cliffs, NJ.
#'
#'   Conover, W.J. (1980). \emph{Practical Nonparametric Statistics}. Second Edition.
#'   John Wiley and Sons, New York, pp.256-272.
#'
#'   Gibbons, R.D., D.K. Bhaumik, and S. Aryal. (2009).
#'   \emph{Statistical Methods for Groundwater Monitoring}, Second Edition.
#'   John Wiley & Sons, Hoboken.
#'
#'   Gilbert, R.O. (1987). \emph{Statistical Methods for Environmental Pollution Monitoring}.
#'   Van Nostrand Reinhold, New York, NY, Chapter 16.
#'
#'   Helsel, D.R. and R.M. Hirsch. (1988). Discussion of Applicability of the t-test for Detecting Trends
#'   in Water Quality Variables. \emph{Water Resources Bulletin} \bold{24}(1), 201-204.
#'
#'   Helsel, D.R., and R.M. Hirsch. (1992). \emph{Statistical Methods in Water Resources Research}.
#'   Elsevier, NY.
#'
#'   Helsel, D.R., and R. M. Hirsch. (2002). \emph{Statistical Methods in Water Resources}.
#'   Techniques of Water Resources Investigations, Book 4, chapter A3. U.S. Geological Survey.
#'   Available on-line at \url{https://pubs.usgs.gov/tm/04/a03/tm4a3.pdf}.
#'
#'   Hirsch, R.M., J.R. Slack, and R.A. Smith. (1982). Techniques of Trend Analysis for Monthly Water Quality
#'   Data. \emph{Water Resources Research} \bold{18}(1), 107-121.
#'
#'   Hirsch, R.M. and J.R. Slack. (1984). A Nonparametric Trend Test for Seasonal Data with Serial Dependence.
#'   \emph{Water Resources Research} \bold{20}(6), 727-732.
#'
#'   Hirsch, R.M., R.B. Alexander, and R.A. Smith. (1991). Selection of Methods for the Detection and
#'   Estimation of Trends in Water Quality. \emph{Water Resources Research} \bold{27}(5), 803-813.
#'
#'   Hollander, M., and D.A. Wolfe. (1999). \emph{Nonparametric Statistical Methods, Second Edition}.
#'   John Wiley and Sons, New York.
#'
#'   Johnson, R.A., and D.W. Wichern. (2007).  \emph{Applied Multivariate Statistical
#'   Analysis}, Sixth Edition.  Pearson Prentice Hall, Upper Saddle River, NJ.
#'
#'   Kendall, M.G. (1938). A New Measure of Rank Correlation. \emph{Biometrika} \bold{30}, 81-93.
#'
#'   Kendall, M.G. (1975). \emph{Rank Correlation Methods}. Charles Griffin, London.
#'
#'   Mann, H.B. (1945). Nonparametric Tests Against Trend. \emph{Econometrica} \bold{13}, 245-259.
#'
#'   Millard, S.P., and Neerchal, N.K. (2001). \emph{Environmental Statistics with S-PLUS}.
#'   CRC Press, Boca Raton, Florida.
#'
#'   Sen, P.K. (1968). Estimates of the Regression Coefficient Based on Kendall's Tau.
#'   \emph{Journal of the American Statistical Association} \bold{63}, 1379-1389.
#'
#'   Theil, H. (1950). A Rank-Invariant Method of Linear and Polynomial Regression Analysis, I-III.
#'   \emph{Proc. Kon. Ned. Akad. v. Wetensch. A.} \bold{53}, 386-392, 521-525, 1397-1412.
#'
#'   USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
#'   EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.
#'   U.S. Environmental Protection Agency, Washington, D.C.
#'
#'   USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
#'   EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
#'   U.S. Environmental Protection Agency, Washington, D.C.
#'
#'   van Belle, G., and J.P. Hughes. (1984). Nonparametric Tests for Trend in Water Quality.
#'   \emph{Water Resources Research} \bold{20}(1), 127-136.
#' }
#' @rawRd
#' \author{
#'   Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \note{
#'   Kendall's test for independence or trend is a nonparametric test.  No assumptions are made about the
#'   distribution of the \eqn{X} and \eqn{Y} variables.  Hirsch et al. (1982) introduced the seasonal
#'   Kendall test to test for trend within each season.  They note that Kendall's test for trend is easy to
#'   compute, even in the presence of missing values, and can also be used with censored values.
#'
#'   van Belle and Hughes (1984) note that the seasonal Kendall test introduced by Hirsch et al. (1982) is
#'   similar to a multivariate extension of the sign test proposed by Jonckheere (1954).  Jonckheeere's test
#'   statistic is based on the unweighted sum of the seasonal tau statistics, while Hirsch et al.'s test is
#'   based on the weighted sum (weighted by number of observations within a season) of the seasonal tau
#'   statistics.
#'
#'   van Belle and Hughes (1984) also note that Kendall's test for trend is slightly less powerful than the test
#'   based on Spearman's rho, but it converges to normality faster.  Also, Bradley (1968, p.288) shows that for
#'   the case of a linear model with normal (Gaussian) errors, the asymptotic relative efficiency of
#'   Kendall's test for trend versus the parametric test for a zero slope is 0.98.
#'
#'   Based on the work of Dietz and Killeen (1981), Hirsch and Slack (1984) describe a modified version of the
#'   seasonal Kendall test that allows for serial dependence in the observations.  They performed a Monte Carlo
#'   study to determine the empirical significance level and power of this modified test vs. the test that
#'   assumes independent observations and found a trade-off between power and the correct significance level.
#'   For \eqn{p = 12} seasons, they found the modified test gave correct significance levels for \eqn{n \geq 10}
#'   as long as the lag-one autocorrelation was 0.6 or less, while the original test that assumes independent
#'   observations yielded highly inflated significance levels.  On the other hand, if in fact the observations
#'   are serially independent, the original test is more powerful than the modified test.
#'
#'   Hirsch and Slack (1984) also looked at the performance of the test for trend introduced by
#'   Dietz and Killeen (1981), which is a weighted sums of squares of the seasonal Kendall S-statistics,
#'   where the matrix of weights is the inverse of the covariance matrix.  The Dietz-Killeen test statistic,
#'   unlike the one proposed by Hirsh and Slack (1984), tests for trend in either direction in any season,
#'   and is asymptotically distributed as a chi-square random variable with \eqn{p} (number of seasons)
#'   degrees of freedom.  Hirsch and Slack (1984), however, found that the test based on this statistic is
#'   quite conservative (i.e., the significance level is much smaller than the assumed significance level)
#'   and has poor power even for moderate sample sizes.  The chi-square approximation becomes reasonably
#'   close only when \eqn{n > 40} if \eqn{p = 12}, \eqn{n > 30} if \eqn{p = 4}, and \eqn{n > 20} if
#'   \eqn{p = 2}.
#'
#'   Lettenmaier (1988) notes the poor power of the test proposed by Dietz and Killeen (1981) and states the
#'   poor power apparently results from an upward bias in the estimated variance of the statistic, which can
#'   be traced to the inversion of the estimated covariance matrix.  He suggests an alternative test statistic
#'   (to test trend in either direction in any season) that is the sum of the squares of the scaled seasonal
#'   Kendall S-statistics (scaled by their standard deviations).  Note that this test statistic ignores
#'   information about the covariance between the seasonal Kendall S-statistics, although its distribution
#'   depends on these covariances.  In the case of no serial dependence, Lettenmaier's test statistic is
#'   exactly the same as the Dietz-Killeen test statistic.  In the case of serial dependence,
#'   Lettenmaier (1988) notes his test statistic is a quadratic form of a multivariate normal random variable
#'   and therefore all the moments of this random variable are easily computed.  Lettenmaier (1988)
#'   approximates the distribution of his test statistic as a scaled non-central chi-square distribution
#'   (with fractional degrees of freedom).  Based on extensive Monte Carlo studies, Lettenmaier (1988) shows
#'   that for the case when the trend is the same in all seasons, the seasonal Kendall's test of
#'   Hirsch and Slack (1984) is superior to his test and far superior to the Dietz-Killeen test.
#'   The power of Lettenmaier's test approached that of the seasonal Kendall test for large trend magnitudes.
#' }
#' @rawRd
#' \seealso{
#'   \code{\link{kendallTrendTest}}, \code{\link{htestEnvStats.object}}, \code{\link{cor.test}}.
#' }
#' @rawRd
#' \examples{
#'   # Reproduce Example 14-10 on page 14-38 of USEPA (2009).  This example
#'   # tests for trend in analyte concentrations (ppm) collected monthly
#'   # between 1983 and 1985.
#'
#'   head(EPA.09.Ex.14.8.df)
#'   #     Month Year Unadj.Conc Adj.Conc
#'   #1  January 1983       1.99     2.11
#'   #2 February 1983       2.10     2.14
#'   #3    March 1983       2.12     2.10
#'   #4    April 1983       2.12     2.13
#'   #5      May 1983       2.11     2.12
#'   #6     June 1983       2.15     2.12
#'
#'   tail(EPA.09.Ex.14.8.df)
#'   #       Month Year Unadj.Conc Adj.Conc
#'   #31      July 1985       2.31     2.23
#'   #32    August 1985       2.32     2.24
#'   #33 September 1985       2.28     2.23
#'   #34   October 1985       2.22     2.24
#'   #35  November 1985       2.19     2.25
#'   #36  December 1985       2.22     2.23
#'
#'
#'   # Plot the data
#'   #--------------
#'   Unadj.Conc <- EPA.09.Ex.14.8.df$Unadj.Conc
#'   Adj.Conc   <- EPA.09.Ex.14.8.df$Adj.Conc
#'   Month      <- EPA.09.Ex.14.8.df$Month
#'   Year       <- EPA.09.Ex.14.8.df$Year
#'   Time       <- paste(substring(Month, 1, 3), Year - 1900, sep = "-")
#'   n          <- length(Unadj.Conc)
#'   Three.Yr.Mean <- mean(Unadj.Conc)
#'
#'   dev.new()
#'   par(mar = c(7, 4, 3, 1) + 0.1, cex.lab = 1.25)
#'   plot(1:n, Unadj.Conc, type = "n", xaxt = "n",
#' 	xlab = "Time (Month)",
#' 	ylab = "ANALYTE CONCENTRATION (mg/L)",
#' 	main = "Figure 14-15. Seasonal Time Series Over a Three Year Period",
#' 	cex.main = 1.1)
#'   axis(1, at = 1:n, labels = rep("", n))
#'   at <- rep(c(1, 5, 9), 3) + rep(c(0, 12, 24), each = 3)
#'   axis(1, at = at, labels = Time[at])
#'   points(1:n, Unadj.Conc, pch = 0, type = "o", lwd = 2)
#'   points(1:n, Adj.Conc, pch = 3, type = "o", col = 8, lwd = 2)
#'   abline(h = Three.Yr.Mean, lwd = 2)
#'   legend("topleft", c("Unadjusted", "Adjusted", "3-Year Mean"), bty = "n",
#'     pch = c(0, 3, -1), lty = c(1, 1, 1), lwd = 2, col = c(1, 8, 1),
#'     inset = c(0.05, 0.01))
#'
#'
#'   # Perform the seasonal Kendall trend test
#'   #----------------------------------------
#'
#'   kendallSeasonalTrendTest(Unadj.Conc ~ Month + Year,
#'     data = EPA.09.Ex.14.8.df)
#'
#'   #Results of Hypothesis Test
#'   #--------------------------
#'   #
#'   #Null Hypothesis:                 All 12 values of tau = 0
#'   #
#'   #Alternative Hypothesis:          The seasonal taus are not all equal
#'   #                                 (Chi-Square Heterogeneity Test)
#'   #                                 At least one seasonal tau != 0
#'   #                                 and all non-zero tau's have the
#'   #                                 same sign (z Trend Test)
#'   #
#'   #Test Name:                       Seasonal Kendall Test for Trend
#'   #                                 (with continuity correction)
#'   #
#'   #Estimated Parameter(s):          tau       =    0.9722222
#'   #                                 slope     =    0.0600000
#'   #                                 intercept = -131.7350000
#'   #
#'   #Estimation Method:               tau:        Weighted Average of
#'   #                                             Seasonal Estimates
#'   #                                 slope:      Hirsch et al.'s
#'   #                                             Modification of
#'   #                                             Thiel/Sen Estimator
#'   #                                 intercept:  Median of
#'   #                                             Seasonal Estimates
#'   #
#'   #Data:                            y      = Unadj.Conc
#'   #                                 season = Month
#'   #                                 year   = Year
#'   #
#'   #Data Source:                     EPA.09.Ex.14.8.df
#'   #
#'   #Sample Sizes:                    January   =  3
#'   #                                 February  =  3
#'   #                                 March     =  3
#'   #                                 April     =  3
#'   #                                 May       =  3
#'   #                                 June      =  3
#'   #                                 July      =  3
#'   #                                 August    =  3
#'   #                                 September =  3
#'   #                                 October   =  3
#'   #                                 November  =  3
#'   #                                 December  =  3
#'   #                                 Total     = 36
#'   #
#'   #Test Statistics:                 Chi-Square (Het) = 0.1071882
#'   #                                 z (Trend)        = 5.1849514
#'   #
#'   #Test Statistic Parameter:        df = 11
#'   #
#'   #P-values:                        Chi-Square (Het) = 1.000000e+00
#'   #                                 z (Trend)        = 2.160712e-07
#'   #
#'   #Confidence Interval for:         slope
#'   #
#'   #Confidence Interval Method:      Gilbert's Modification of
#'   #                                 Theil/Sen Method
#'   #
#'   #Confidence Interval Type:        two-sided
#'   #
#'   #Confidence Level:                95%
#'   #
#'   #Confidence Interval:             LCL = 0.05786914
#'   #                                 UCL = 0.07213086
#'
#'   #==========
#'
#'   # Clean up
#'   #---------
#'
#'   rm(Unadj.Conc, Adj.Conc, Month, Year, Time, n, Three.Yr.Mean, at)
#'   graphics.off()
#' }
#' @rawRd
#' \keyword{htestEnvStats}
#' @rawRd
#' \keyword{nonparametric}
#' @rawRd
#' \keyword{regression}

kendallSeasonalTrendTest <-
function (y, ...) 
UseMethod("kendallSeasonalTrendTest")
