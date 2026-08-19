#' Two-Sample Linear Rank Test to Detect a Difference Between Two Distributions Based on Censored Data
#' @description
#' Two-sample linear rank test to detect a difference (usually a shift) between two
#'   distributions based on censored data.
#' @usage
#' twoSampleLinearRankTestCensored(x, x.censored, y, y.censored,
#'     censoring.side = "left", location.shift.null = 0, scale.shift.null = 1,
#'     alternative = "two.sided", test = "logrank", variance = "hypergeometric",
#'     surv.est = "prentice", shift.type = "location")
#' @rawRd
#' \arguments{
#'   \item{x}{
#'   numeric vector of values for the first sample.
#'   Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf},
#'   \code{-Inf}) values are allowed but will be removed.
#' }
#'   \item{x.censored}{
#'   numeric or logical vector indicating which values of \code{x} are censored.
#'   This must be the same length as \code{x}.  If the mode of \code{x.censored} is
#'   \code{"logical"}, \code{TRUE} values correspond to elements of \code{x} that are
#'   censored, and \code{FALSE} values correspond to elements of \code{x} that are not
#'   censored.  If the mode of \code{x.censored} is \code{"numeric"}, it must contain only
#'   \code{1}'s and \code{0}'s; \code{1} corresponds to \code{TRUE} and \code{0}
#'   corresponds to \code{FALSE}.  Missing (\code{NA}) values are allowed but will be
#'   removed.
#' }
#'   \item{y}{
#'   numeric vector of values for the second sample.
#'   Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf},
#'   \code{-Inf}) values are allowed but will be removed.
#' }
#'   \item{y.censored}{
#'   numeric or logical vector indicating which values of \code{y} are censored.
#'   This must be the same length as \code{y}.  If the mode of \code{y.censored} is
#'   \code{"logical"}, \code{TRUE} values correspond to elements of \code{y} that are
#'   censored, and \code{FALSE} values correspond to elements of \code{y} that are not
#'   censored.  If the mode of \code{y.censored} is \code{"numeric"}, it must contain only
#'   \code{1}'s and \code{0}'s; \code{1} corresponds to \code{TRUE} and \code{0}
#'   corresponds to \code{FALSE}.  Missing (\code{NA}) values are allowed but will be
#'   removed.
#' }
#'   \item{censoring.side}{
#'   character string indicating on which side the censoring occurs for the data in
#'   \code{x} and \code{y}.  The possible values are \code{"left"} (the default) and
#'   \code{"right"}.
#' }
#'   \item{location.shift.null}{
#'   numeric scalar indicating the hypothesized value of \eqn{\Delta}, the location
#'   shift between the two distributions, under the null hypothesis.  The default value is
#'   \code{location.shift.null=0}.  This argument is ignored if \code{shift.type="scale"}.
#' }
#'   \item{scale.shift.null}{
#'   numeric scalar indicating the hypothesized value of \eqn{\tau}, the scale shift
#'   between the two distributions, under the null hypothesis.  The default value is \cr
#'   \code{scale.shift.null=1}.  This argument is ignored if \code{shift.type="location"}.
#' }
#'   \item{alternative}{
#'   character string indicating the kind of alternative hypothesis.  The possible values
#'   are \code{"two.sided"} (the default), \code{"less"}, and \code{"greater"}.  See the
#'   DETAILS section below for more information.
#' }
#'   \item{test}{
#'   character string indicating which linear rank test to use.  The possible values are:
#'   \code{"logrank"} (the default), \code{"tarone-ware"}, \code{"gehan"},
#'   \code{"peto-peto"}, \code{"normal.scores.1"}, \code{"normal.scores.2"},
#'   and \code{"generalized.sign"}.  See the DETAILS section below for more information.
#' }
#'   \item{variance}{
#'   character string indicating which kind of variance to compute for the test.  The
#'   possible values are: \code{"hypergeometric"} (the default), \code{"permutation"},
#'   and \code{"asymptotic"}.  See the DETAILS section below for more information.
#' }
#'   \item{surv.est}{
#'   character string indicating what method to use to estimate the survival function.
#'   The possible values are \code{"prentice"} (the default), \code{"kaplan-meier"}, \cr
#'   \code{"peto-peto"}, and \code{"altshuler"}.  When \code{test="logrank"} the
#'   argument \cr
#'   \code{surv.est} is automatically set to \code{"altshuler"} and cannot
#'   be changed by the user.
#'   See the DETAILS section below for more information.
#' }
#'   \item{shift.type}{
#'   character string indicating which kind of shift is being tested.  The possible values
#'   are \code{"location"} (the default) and \code{"scale"}.
#' }
#' }
#' @details
#' This help page has been shortened to keep function help focused on usage,
#' arguments, return values, and examples. Extended method details, formulas,
#' and background material are available in \code{vignette("extended-function-details", package = "EnvStats")}, section \code{twoSampleLinearRankTestCensored}.
#' @rawRd
#' \value{
#'   a list of class \code{"htestCensored"} containing the results of the hypothesis test.
#'   See the help file for \code{\link{htestCensored.object}} for details.
#' }
#' @rawRd
#' \references{
#'   Altshuler, B. (1970).  Theory for the Measurement of Competing Risks in Animal
#'   Experiments.  \emph{Mathematical Biosciences} \bold{6}, 1--11.
#'
#'   Breslow, N.E. (1970).  A Generalized Kruskal-Wallis Test for Comparing K Samples
#'   Subject to Unequal Patterns of Censorship.  \emph{Biometrika} \bold{57}, 579--594.
#'
#'   Conover, W.J. (1980).  \emph{Practical Nonparametric Statistics}.  Second Edition.
#'   John Wiley and Sons, New York, Chapter 4.
#'
#'   Cox, D.R. (1972).  Regression Models and Life Tables (with Discussion).
#'   \emph{Journal of the Royal Statistical Society of London, Series B} \bold{34},
#'   187--220.
#'
#'   Divine, G., H.J. Norton, R. Hunt, and J. Dinemann. (2013).  A Review of Analysis
#'   and Sample Size Calculation Considerations for Wilcoxon Tests.  \emph{Anesthesia
#'   & Analgesia} \bold{117}, 699--710.
#'
#'   Fleming, T.R., and D.P. Harrington. (1981).  A Class of Hypothesis Tests for One and
#'   Two Sample Censored Survival Data.
#'   \emph{Communications in Statistics -- Theory and Methods} \bold{A10}(8), 763--794.
#'
#'   Fleming, T.R., and D.P. Harrington. (1991).
#'   \emph{Counting Processes & Survival Analysis}.  John Wiley and Sons, New York,
#'   Chapter 7.
#'
#'   Gehan, E.A. (1965).  A Generalized Wilcoxon Test for Comparing Arbitrarily
#'   Singly-Censored Samples.  \emph{Biometrika} \bold{52}, 203--223.
#'
#'   Harrington, D.P., and T.R. Fleming. (1982).  A Class of Rank Test Procedures for
#'   Censored Survival Data.  \emph{Biometrika} \bold{69}(3), 553--566.
#'
#'   Heller, G., and E. S. Venkatraman. (1996).  Resampling Procedures to Compare Two
#'   Survival Distributions in the Presence of Right-Censored Data.
#'   \emph{Biometrics} \bold{52}, 1204--1213.
#'
#'   Hettmansperger, T.P. (1984).  \emph{Statistical Inference Based on Ranks}.
#'   John Wiley and Sons, New York, 323pp.
#'
#'   Hollander, M., and D.A. Wolfe. (1999). \emph{Nonparametric Statistical Methods,
#'   Second Edition}.  John Wiley and Sons, New York.
#'
#'   Kaplan, E.L., and P. Meier. (1958).  Nonparametric Estimation From Incomplete
#'   Observations.  \emph{Journal of the American Statistical Association} \bold{53},
#'   457--481.
#'
#'   Latta, R.B. (1981).  A Monte Carlo Study of Some Two-Sample Rank Tests with Censored
#'   Data.  \emph{Journal of the American Statistical Association} \bold{76}(375),
#'   713--719.
#'
#'   Mantel, N. (1966).  Evaluation of Survival Data and Two New Rank Order Statistics
#'   Arising in its Consideration.  \emph{Cancer Chemotherapy Reports} \bold{50}, 163-170.
#'
#'   Millard, S.P., and S.J. Deverel. (1988).  Nonparametric Statistical Methods for
#'   Comparing Two Sites Based on Data With Multiple Nondetect Limits.
#'   \emph{Water Resources Research}, \bold{24}(12), 2087--2098.
#'
#'   Millard, S.P., and N.K. Neerchal. (2001).  \emph{Environmental Statistics with
#'   S-PLUS}.  CRC Press, Boca Raton, FL, pp.432--435.
#'
#'   Peto, R., and J. Peto. (1972).  Asymptotically Efficient Rank Invariant Test
#'   Procedures (with Discussion).
#'   \emph{Journal of the Royal Statistical Society of London, Series A} \bold{135},
#'   185--206.
#'
#'   Prentice, R.L. (1978).  Linear Rank Tests with Right Censored Data.
#'   \emph{Biometrika} \bold{65}, 167--179.
#'
#'   Prentice, R.L. (1985).  Linear Rank Tests.  In Kotz, S., and N.L. Johnson, eds.
#'   \emph{Encyclopedia of Statistical Science}.  John Wiley and Sons, New York.
#'   Volume 5, pp.51--58.
#'
#'   Prentice, R.L., and P. Marek. (1979).  A Qualitative Discrepancy Between Censored
#'   Data Rank Tests.  \emph{Biometrics} \bold{35}, 861--867.
#'
#'   Tarone, R.E., and J. Ware. (1977).  On Distribution-Free Tests for Equality of
#'   Survival Distributions.  \emph{Biometrika} \bold{64}(1), 156--160.
#'
#'   USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
#'   EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.
#'   U.S. Environmental Protection Agency, Washington, D.C.
#'
#'   USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
#'   EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
#'   U.S. Environmental Protection Agency, Washington, D.C.
#' }
#' @rawRd
#' \author{
#'   Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \note{
#'   All of the tests computed by \code{twoSampleLinearRankTestCensored}
#'   (logrank, Tarone-Ware, Gehan, Peto-Peto, normal scores, and generalized sign)
#'   are based on a
#'   statistic that is essentially the sum over all uncensored time points of the
#'   weighted difference between the observed and expected number of observations at each
#'   time point (see Equation (15) above).  The tests differ in how they weight the
#'   differences between the observed and expected number of observations.
#'
#'   Prentice and Marek (1979) point out that the Gehan test uses weights that depend on
#'   the censoring rates within each group and can lead to non-significant outcomes in
#'   the case of heavy censoring when in fact a very large difference between the two
#'   groups exists.
#'
#'   Latta (1981) performed a Monte Carlo simulation to study the power of the Gehan,
#'   logrank, and Peto-Peto tests using all three different estimators of variance
#'   (permutation, hypergeometric, and asymptotic).  He used lognormal, Weibull, and
#'   exponential distributions to generate the observations, and studied two different
#'   cases of censoring: uniform censoring for both samples vs. no censoring in the first
#'   sample and uniform censoring in the second sample.  Latta (1981) used sample sizes
#'   of 10 and 50 (both the equal and unequal cases were studied).  Latta (1981) found
#'   that all three tests maintained the nominal Type I error level (\eqn{\alpha}-level)
#'   in the case of equal sample sizes and equal censoring.  Also, the Peto-Peto test
#'   based on the asymptotic variance appeared to maintain the nominal \eqn{\alpha}-level
#'   in all situations, but the other tests were slightly biased in the case of unequal
#'   sample sizes and/or unequal censoring.  In particular, tests based on the
#'   hypergeometric variance are slightly biased for unequal sample sizes.  Latta (1981)
#'   concludes that if there is no censoring or light censoring, any of the tests may be
#'   used (but the hypergeometric variance should not be used if the sample sizes are
#'   very different).  In the case of heavy censoring where sample sizes are far apart
#'   and/or the censoring is very different between samples, the Peto-Peto test based on
#'   the asymptotic variance should be used.
#'
#'   Millard and Deverel (1988) also performed a Monte Carlo simulation similar to
#'   Latta's (1981) study.  They only used the lognormal distribution to generate
#'   observations, but also looked at the normal scores test and two ad-hoc modifications
#'   of the MWW test.  They found the \dQuote{Normal Scores 2} test shown in Table 1
#'   above to be the best behaved test in terms of maintaining the nominal
#'   \eqn{\alpha}-level, but the other tests behaved almost as well.  As Latta (1981)
#'   found, when sample sizes and censoring are very different between the two groups,
#'   the nominal \eqn{\alpha}-level of most of the tests is slightly biased.  In the
#'   cases where the nominal \eqn{\alpha}-level was maintained, the Peto-Peto test based
#'   on the asymptotic variance appeared to be as powerful or more powerful than the
#'   normal scores tests.
#'
#'   Neither of the Monte Carlo studies performed by Latta (1981) and Millard and Deverel
#'   (1988) looked at the behavior of the two-sample linear rank tests in the presence of
#'   several tied uncensored observations (because both studies generated observations
#'   from continuous distributions).  Note that the results shown in Table 9 of
#'   Millard and Deverel (1988, p.2097) are not all correct because they did not
#'   allow for tied uncensored values.  The last example in the EXAMPLES section below
#'   shows the correct values that should appear in that table.
#'
#'   Heller and Venkatraman (1996) performed a Monte Carlo simulation study to compare
#'   the behaviors of the Peto-Peto test (using the Prentice, 1978, estimator of survival;
#'   they call this the Prentice-Wilcoxon test)
#'   and logrank test under varying censoring conditions with sample sizes of 20 and 50
#'   per group based on using the following methods to compute p-values:
#'   the asymptotic standard normal approximation,
#'   a permutation test approach (this is \bold{NOT} the same as the permutation variance),
#'   and a bootstrap approach.  Observed times were generated from Weibull and lognormal
#'   survival time distributions with independent uniform censoring.  They found that
#'   for the Peto-Peto test, "the asymptotic test procedure was the most accurate;
#'   resampling procedures did not improve upon its accuracy."  For the logrank test,
#'   with sample sizes of 20 per group, the usual test based on the asymptotic standard
#'   normal approximation tended to have a very slightly higher Type I error rate than
#'   assumed (however, for an assumed Type I error rate of 0.05, the largest Type I error
#'   rate observed was less than 0.065), whereas the permuation and bootstrap tests
#'   performed better; with sample sizes of 50 per group there was no difference in
#'   test performance.
#'
#'   Fleming and Harrington (1981) introduced a family of tests (sometimes called G-rho
#'   tests) that contain the logrank and Peto-Peto tests as special cases.  A single
#'   parameter \eqn{\rho} (rho) controls the weights given to the uncensored and
#'   censored observations.  Positive values of \eqn{\rho} produce tests more sensitive
#'   to early differences in the survival function, that is, differences in the cdf at
#'   small values.  Negative values of \eqn{\rho} produce tests more sensitive to late
#'   differences in the survival function, that is, differences in the cdf at large
#'   values.
#'
#'   The function \code{\link[survival:survdiff]{survdiff}} in the \R package
#'   \pkg{survival} implements the G-rho family of tests suggested by Flemming and
#'   Harrington (1981).  Calling \code{survdiff} with \code{rho=0} (the default) yields
#'   the logrank test.  Calling \code{survdiff} with \code{rho=1} yields the Peto-Peto
#'   test based on the Kaplan-Meier estimate of survival.  The function \code{survdiff}
#'   always uses the hypergeometric estimate of variance and the Kaplan-Meier estimate of
#'   survival, but it uses the \dQuote{left-continuous} version of the Kaplan-Meier
#'   estimate.  The left-continuous K-M estimate of survival is defined as
#'   follows:  at each death (unique uncensored observation), the estimated survival is
#'   equal to the estimated survival based on the ordinary K-M estimate at the prior
#'   death time (or 1 for the first death).
#' }
#' @rawRd
#' \seealso{
#'   \code{\link{twoSampleLinearRankTest}}, \code{\link[survival:survdiff]{survdiff}},
#'   \code{\link{wilcox.test}}, \code{\link{htestCensored.object}}.
#' }
#' @rawRd
#' \examples{
#'   # The last part of the EXAMPLES section in the help file for
#'   # cdfCompareCensored compares the empirical distribution of copper and zinc
#'   # between two sites:  Alluvial Fan and Basin-Trough (Millard and Deverel, 1988).
#'   # The data for this example are stored in Millard.Deverel.88.df.  Perform a
#'   # test to determine if there is a significant difference between these two
#'   # sites (perform a separate test for the copper and the zinc).
#'
#'   Millard.Deverel.88.df
#'   #    Cu.orig Cu Cu.censored Zn.orig  Zn Zn.censored         Zone Location
#'   #1       < 1  1        TRUE     <10  10        TRUE Alluvial.Fan        1
#'   #2       < 1  1        TRUE       9   9       FALSE Alluvial.Fan        2
#'   #3         3  3       FALSE      NA  NA       FALSE Alluvial.Fan        3
#'   #.
#'   #.
#'   #.
#'   #116       5  5       FALSE      50  50       FALSE Basin.Trough       48
#'   #117      14 14       FALSE      90  90       FALSE Basin.Trough       49
#'   #118       4  4       FALSE      20  20       FALSE Basin.Trough       50
#'
#'
#'   #------------------------------
#'   # First look at the copper data
#'   #------------------------------
#'
#'   Cu.AF <- with(Millard.Deverel.88.df,
#'     Cu[Zone == "Alluvial.Fan"])
#'
#'   Cu.AF.cen <- with(Millard.Deverel.88.df,
#'     Cu.censored[Zone == "Alluvial.Fan"])
#'
#'   Cu.BT <- with(Millard.Deverel.88.df,
#'     Cu[Zone == "Basin.Trough"])
#'
#'   Cu.BT.cen <- with(Millard.Deverel.88.df,
#'     Cu.censored[Zone == "Basin.Trough"])
#'
#'   # Note the large number of tied observations in the copper data
#'   #--------------------------------------------------------------
#'
#'   table(Cu.AF[!Cu.AF.cen])
#'   # 1  2  3  4  5  7  8  9 10 11 12 16 20
#'   # 5 21  6  3  3  3  1  1  1  1  1  1  1
#'
#'   table(Cu.BT[!Cu.BT.cen])
#'   # 1  2  3  4  5  6  8  9 12 14 15 17 23
#'   # 7  4  8  5  1  2  1  2  1  1  1  1  1
#'
#'
#'   # Logrank test with hypergeometric variance:
#'   #-------------------------------------------
#'   twoSampleLinearRankTestCensored(x = Cu.AF, x.censored = Cu.AF.cen,
#'     y = Cu.BT, y.censored = Cu.BT.cen)
#'
#'   #Results of Hypothesis Test
#'   #Based on Censored Data
#'   #--------------------------
#'   #
#'   #Null Hypothesis:                 Fy(t) = Fx(t)
#'   #
#'   #Alternative Hypothesis:          Fy(t) != Fx(t) for at least one t
#'   #
#'   #Test Name:                       Two-Sample Linear Rank Test:
#'   #                                 Logrank Test
#'   #                                 with Hypergeometric Variance
#'   #
#'   #Censoring Side:                  left
#'   #
#'   #Censoring Level(s):              x =  1  5 10 20
#'   #                                 y =  1  2  5 10 15
#'   #
#'   #Data:                            x = Cu.AF
#'   #                                 y = Cu.BT
#'   #
#'   #Censoring Variable:              x = Cu.AF.cen
#'   #                                 y = Cu.BT.cen
#'   #
#'   #Number NA/NaN/Inf's Removed:     x = 3
#'   #                                 y = 1
#'   #
#'   #Sample Sizes:                    nx = 65
#'   #                                 ny = 49
#'   #
#'   #Percent Censored:                x = 26.2%
#'   #                                 y = 28.6%
#'   #
#'   #Test Statistics:                 nu     = -1.8791355
#'   #                                 var.nu = 13.6533490
#'   #                                 z      = -0.5085557
#'   #
#'   #P-value:                         0.6110637
#'
#'
#'   # Compare the p-values produced by the Normal Scores 2 test
#'   # using the hypergeomtric vs. permutation variance estimates.
#'   # Note how much larger the estimated variance is based on
#'   # the permuation variance estimate:
#'   #-----------------------------------------------------------
#'
#'   twoSampleLinearRankTestCensored(x = Cu.AF, x.censored = Cu.AF.cen,
#'     y = Cu.BT, y.censored = Cu.BT.cen,
#'     test = "normal.scores.2")$p.value
#'   #[1] 0.2008913
#'
#'   twoSampleLinearRankTestCensored(x = Cu.AF, x.censored = Cu.AF.cen,
#'     y = Cu.BT, y.censored = Cu.BT.cen,
#'     test = "normal.scores.2", variance = "permutation")$p.value
#'   #[1] [1] 0.657001
#'
#'
#'   #--------------------------
#'   # Now look at the zinc data
#'   #--------------------------
#'
#'   Zn.AF <- with(Millard.Deverel.88.df,
#'     Zn[Zone == "Alluvial.Fan"])
#'
#'   Zn.AF.cen <- with(Millard.Deverel.88.df,
#'     Zn.censored[Zone == "Alluvial.Fan"])
#'
#'   Zn.BT <- with(Millard.Deverel.88.df,
#'     Zn[Zone == "Basin.Trough"])
#'
#'   Zn.BT.cen <- with(Millard.Deverel.88.df,
#'     Zn.censored[Zone == "Basin.Trough"])
#'
#'   # Note the moderate number of tied observations in the zinc data,
#'   # and the "outlier" of 620 in the Alluvial Fan data.
#'   #---------------------------------------------------------------
#'
#'   table(Zn.AF[!Zn.AF.cen])
#'   #  5   7   8   9  10  11  12  17  18  19  20  23  29  30  33  40  50 620
#'   #  1   1   1   1  20   2   1   1   1   1  14   1   1   1   1   1   1   1
#'
#'   table(Zn.BT[!Zn.BT.cen])
#'   # 3  4  5  6  8 10 11 12 13 14 15 17 20 25 30 40 50 60 70 90
#'   # 2  2  2  1  1  5  1  2  1  1  1  2 11  1  4  3  2  2  1  1
#'
#'
#'   # Logrank test with hypergeometric variance:
#'   #-------------------------------------------
#'   twoSampleLinearRankTestCensored(x = Zn.AF, x.censored = Zn.AF.cen,
#'     y = Zn.BT, y.censored = Zn.BT.cen)
#'
#'   #Results of Hypothesis Test
#'   #Based on Censored Data
#'   #--------------------------
#'   #
#'   #Null Hypothesis:                 Fy(t) = Fx(t)
#'   #
#'   #Alternative Hypothesis:          Fy(t) != Fx(t) for at least one t
#'   #
#'   #Test Name:                       Two-Sample Linear Rank Test:
#'   #                                 Logrank Test
#'   #                                 with Hypergeometric Variance
#'   #
#'   #Censoring Side:                  left
#'   #
#'   #Censoring Level(s):              x =  3 10
#'   #                                 y =  3 10
#'   #
#'   #Data:                            x = Zn.AF
#'   #                                 y = Zn.BT
#'   #
#'   #Censoring Variable:              x = Zn.AF.cen
#'   #                                 y = Zn.BT.cen
#'   #
#'   #Number NA/NaN/Inf's Removed:     x = 1
#'   #                                 y = 0
#'   #
#'   #Sample Sizes:                    nx = 67
#'   #                                 ny = 50
#'   #
#'   #Percent Censored:                x = 23.9%
#'   #                                 y =  8.0%
#'   #
#'   #Test Statistics:                 nu     = -6.992999
#'   #                                 var.nu = 17.203227
#'   #                                 z      = -1.686004
#'   #
#'   #P-value:                         0.09179512
#'
#'   #----------
#'
#'   # Compare the p-values produced by the Logrank, Gehan, Peto-Peto,
#'   # and Tarone-Ware tests using the hypergeometric variance.
#'   #-----------------------------------------------------------
#'
#'   twoSampleLinearRankTestCensored(x = Zn.AF, x.censored = Zn.AF.cen,
#'     y = Zn.BT, y.censored = Zn.BT.cen,
#'     test = "logrank")$p.value
#'   #[1] 0.09179512
#'
#'   twoSampleLinearRankTestCensored(x = Zn.AF, x.censored = Zn.AF.cen,
#'     y = Zn.BT, y.censored = Zn.BT.cen,
#'     test = "gehan")$p.value
#'   #[1] 0.0185445
#'
#'   twoSampleLinearRankTestCensored(x = Zn.AF, x.censored = Zn.AF.cen,
#'     y = Zn.BT, y.censored = Zn.BT.cen,
#'     test = "peto-peto")$p.value
#'   #[1] 0.009704529
#'
#'   twoSampleLinearRankTestCensored(x = Zn.AF, x.censored = Zn.AF.cen,
#'     y = Zn.BT, y.censored = Zn.BT.cen,
#'     test = "tarone-ware")$p.value
#'   #[1] 0.03457803
#'
#'   #----------
#'
#'   # Clean up
#'   #---------
#'
#'   rm(Cu.AF, Cu.AF.cen, Cu.BT, Cu.BT.cen,
#'      Zn.AF, Zn.AF.cen, Zn.BT, Zn.BT.cen)
#'
#'
#'   #==========
#'
#'   # Example 16.5 on pages 16-22 to 16.23 of USEPA (2009) shows how to perform
#'   # the Tarone-Ware two sample linear rank test based on censored data using
#'   # observations on tetrachloroethylene (PCE) (ppb) collected at one background
#'   # and one compliance well.  The data for this example are stored in
#'   # EPA.09.Ex.16.5.PCE.df.
#'
#'   EPA.09.Ex.16.5.PCE.df
#'
#'   #    Well.type PCE.Orig.ppb PCE.ppb Censored
#'   #1  Background           <4     4.0     TRUE
#'   #2  Background          1.5     1.5    FALSE
#'   #3  Background           <2     2.0     TRUE
#'   #4  Background          8.7     8.7    FALSE
#'   #5  Background          5.1     5.1    FALSE
#'   #6  Background           <5     5.0     TRUE
#'   #7  Compliance          6.4     6.4    FALSE
#'   #8  Compliance         10.9    10.9    FALSE
#'   #9  Compliance            7     7.0    FALSE
#'   #10 Compliance         14.3    14.3    FALSE
#'   #11 Compliance          1.9     1.9    FALSE
#'   #12 Compliance           10    10.0    FALSE
#'   #13 Compliance          6.8     6.8    FALSE
#'   #14 Compliance           <5     5.0     TRUE
#'
#'   with(EPA.09.Ex.16.5.PCE.df,
#'     twoSampleLinearRankTestCensored(
#'       x = PCE.ppb[Well.type == "Compliance"],
#'       x.censored = Censored[Well.type == "Compliance"],
#'       y = PCE.ppb[Well.type == "Background"],
#'       y.censored = Censored[Well.type == "Background"],
#'       test = "tarone-ware", alternative = "greater"))
#'
#'   #Results of Hypothesis Test
#'   #Based on Censored Data
#'   #--------------------------
#'   #
#'   #Null Hypothesis:                 Fy(t) = Fx(t)
#'   #
#'   #Alternative Hypothesis:          Fy(t) > Fx(t) for at least one t
#'   #
#'   #Test Name:                       Two-Sample Linear Rank Test:
#'   #                                 Tarone-Ware Test
#'   #                                 with Hypergeometric Variance
#'   #
#'   #Censoring Side:                  left
#'   #
#'   #Censoring Level(s):              x = 5
#'   #                                 y = 2 4 5
#'   #
#'   #Data:                            x = PCE.ppb[Well.type == "Compliance"]
#'   #                                 y = PCE.ppb[Well.type == "Background"]
#'   #
#'   #Censoring Variable:              x = Censored[Well.type == "Compliance"]
#'   #                                 y = Censored[Well.type == "Background"]
#'   #
#'   #Sample Sizes:                    nx = 8
#'   #                                 ny = 6
#'   #
#'   #Percent Censored:                x = 12.5%
#'   #                                 y = 50.0%
#'   #
#'   #Test Statistics:                 nu     =  8.458912
#'   #                                 var.nu = 20.912407
#'   #                                 z      =  1.849748
#'   #
#'   #P-value:                         0.03217495
#'
#'   # Compare the p-value for the Tarone-Ware test with p-values from
#'   # the logrank, Gehan, and Peto-Peto tests
#'   #-----------------------------------------------------------------
#'
#'   with(EPA.09.Ex.16.5.PCE.df,
#'     twoSampleLinearRankTestCensored(
#'       x = PCE.ppb[Well.type == "Compliance"],
#'       x.censored = Censored[Well.type == "Compliance"],
#'       y = PCE.ppb[Well.type == "Background"],
#'       y.censored = Censored[Well.type == "Background"],
#'       test = "tarone-ware", alternative = "greater"))$p.value
#'   #[1] 0.03217495
#'
#'
#'   with(EPA.09.Ex.16.5.PCE.df,
#'     twoSampleLinearRankTestCensored(
#'       x = PCE.ppb[Well.type == "Compliance"],
#'       x.censored = Censored[Well.type == "Compliance"],
#'       y = PCE.ppb[Well.type == "Background"],
#'       y.censored = Censored[Well.type == "Background"],
#'       test = "logrank", alternative = "greater"))$p.value
#'   #[1] 0.02752793
#'
#'
#'   with(EPA.09.Ex.16.5.PCE.df,
#'     twoSampleLinearRankTestCensored(
#'       x = PCE.ppb[Well.type == "Compliance"],
#'       x.censored = Censored[Well.type == "Compliance"],
#'       y = PCE.ppb[Well.type == "Background"],
#'       y.censored = Censored[Well.type == "Background"],
#'       test = "gehan", alternative = "greater"))$p.value
#'   #[1] 0.03656224
#'
#'   with(EPA.09.Ex.16.5.PCE.df,
#'     twoSampleLinearRankTestCensored(
#'       x = PCE.ppb[Well.type == "Compliance"],
#'       x.censored = Censored[Well.type == "Compliance"],
#'       y = PCE.ppb[Well.type == "Background"],
#'       y.censored = Censored[Well.type == "Background"],
#'       test = "peto-peto", alternative = "greater"))$p.value
#'   #[1] 0.03127296
#'
#'   #==========
#'
#'   # The results shown in Table 9 of Millard and Deverel (1988, p.2097) are correct
#'   # only for the hypergeometric variance and the modified MWW tests; the other
#'   # results were computed as if there were no ties.  Re-compute the correct
#'   # z-statistics and p-values for the copper and zinc data.
#'
#'   test <- c(rep(c("gehan", "logrank", "peto-peto"), 2), "peto-peto",
#'     "normal.scores.1", "normal.scores.2", "normal.scores.2")
#'
#'   variance <- c(rep("permutation", 3), rep("hypergeometric", 3),
#'     "asymptotic", rep("permutation", 2), "hypergeometric")
#'
#'   stats.mat <- matrix(as.numeric(NA), ncol = 4, nrow = 10)
#'
#'   for(i in 1:10) {
#'     dum.list <- with(Millard.Deverel.88.df,
#'         twoSampleLinearRankTestCensored(
#'           x = Cu[Zone == "Basin.Trough"],
#'           x.censored = Cu.censored[Zone == "Basin.Trough"],
#'           y = Cu[Zone == "Alluvial.Fan"],
#'           y.censored = Cu.censored[Zone == "Alluvial.Fan"],
#'           test = test[i], variance = variance[i]))
#'     stats.mat[i, 1:2] <- c(dum.list$statistic["z"], dum.list$p.value)
#'
#'     dum.list <- with(Millard.Deverel.88.df,
#'         twoSampleLinearRankTestCensored(
#'           x = Zn[Zone == "Basin.Trough"],
#'           x.censored = Zn.censored[Zone == "Basin.Trough"],
#'           y = Zn[Zone == "Alluvial.Fan"],
#'           y.censored = Zn.censored[Zone == "Alluvial.Fan"],
#'           test = test[i], variance = variance[i]))
#'     stats.mat[i, 3:4] <- c(dum.list$statistic["z"], dum.list$p.value)
#'   }
#'
#'   dimnames(stats.mat) <- list(paste(test, variance, sep = "."),
#'     c("Cu.Z", "Cu.p.value", "Zn.Z", "Zn.p.value"))
#'
#'   round(stats.mat, 2)
#'   #                               Cu.Z Cu.p.value Zn.Z Zn.p.value
#'   #gehan.permutation              0.87       0.38 2.49       0.01
#'   #logrank.permutation            0.79       0.43 1.75       0.08
#'   #peto-peto.permutation          0.92       0.36 2.42       0.02
#'   #gehan.hypergeometric           0.71       0.48 2.35       0.02
#'   #logrank.hypergeometric         0.51       0.61 1.69       0.09
#'   #peto-peto.hypergeometric       1.03       0.30 2.59       0.01
#'   #peto-peto.asymptotic           0.90       0.37 2.37       0.02
#'   #normal.scores.1.permutation    0.94       0.34 2.37       0.02
#'   #normal.scores.2.permutation    0.98       0.33 2.39       0.02
#'   #normal.scores.2.hypergeometric 1.28       0.20 2.48       0.01
#'
#'   #----------
#'
#'   # Clean up
#'   #---------
#'   rm(test, variance, stats.mat, i, dum.list)
#' }
#' @rawRd
#' \keyword{htest}
#' @rawRd
#' \keyword{nonparametric}
#' @rawRd
#' \keyword{regression}

twoSampleLinearRankTestCensored <-
function (x, x.censored, y, y.censored, censoring.side = "left", 
    location.shift.null = 0, scale.shift.null = 1, alternative = "two.sided", 
    test = "logrank", variance = "hypergeometric", surv.est = "prentice", 
    shift.type = "location") 
{
    censoring.side <- match.arg(censoring.side, c("left", "right"))
    alternative <- match.arg(alternative, c("two.sided", "less", 
        "greater"))
    test <- match.arg(test, c("logrank", "tarone-ware", "gehan", 
        "peto-peto", "normal.scores.2", "normal.scores.1", "generalized.sign"))
    variance <- match.arg(variance, c("hypergeometric", "permutation", 
        "asymptotic"))
    surv.est <- match.arg(surv.est, c("prentice", "kaplan-meier", 
        "peto-peto", "altshuler"))
    if (test == "logrank") 
        surv.est <- "altshuler"
    shift.type <- match.arg(shift.type, c("location", "scale"))
    if (variance == "asymptotic" && (test != "peto-peto" || surv.est != 
        "prentice")) 
        stop(paste("The test based on the asymptotic variance is only available for", 
            "the 'peto-peto' test with the 'prentice' estimator of survival,", 
            "i.e., test='peto-peto' and surv.est='prentice'"))
    data.name <- c(deparse(substitute(x)), deparse(substitute(y)))
    names(data.name) <- c("x", "y")
    censoring.name <- c(deparse(substitute(x.censored)), deparse(substitute(y.censored)))
    names(censoring.name) <- c("x", "y")
    if (!is.vector(x, mode = "numeric") || is.factor(x)) 
        stop("'x' must be a numeric vector")
    if (!((is.vector(x.censored, mode = "numeric") && !is.factor(x.censored)) || 
        is.vector(x.censored, mode = "logical"))) 
        stop("'x.censored' must be a logical or numeric vector")
    if (length(x.censored) != length(x)) 
        stop("'x.censored' must be the same length as 'x'")
    if ((bad.obs.x <- sum(!(ok <- is.finite(x) & is.finite(as.numeric(x.censored))))) > 
        0) {
        x <- x[ok]
        x.censored <- x.censored[ok]
        warning(paste(bad.obs.x, "observations with NA/NaN/Inf in 'x' and/or 'x.censored' removed."))
    }
    if (is.numeric(x.censored)) {
        if (!all(x.censored == 0 | x.censored == 1)) 
            stop(paste("When 'x.censored' is a numeric vector, all values of", 
                "'x.censored' must be 0 (not censored) or 1 (censored)."))
        x.censored <- as.logical(x.censored)
    }
    n.x.cen <- sum(x.censored)
    if (!is.vector(y, mode = "numeric") || is.factor(y)) 
        stop("'y' must be a numeric vector")
    if (!((is.vector(y.censored, mode = "numeric") && !is.factor(y.censored)) || 
        is.vector(y.censored, mode = "logical"))) 
        stop("'y.censored' must be a logical or numeric vector")
    if (length(y.censored) != length(y)) 
        stop("'y.censored' must be the same length as 'y'")
    if ((bad.obs.y <- sum(!(ok <- is.finite(y) & is.finite(as.numeric(y.censored))))) > 
        0) {
        y <- y[ok]
        y.censored <- y.censored[ok]
        warning(paste(bad.obs.y, "observations with NA/NaN/Inf in 'y' and/or 'y.censored' removed."))
    }
    if (is.numeric(y.censored)) {
        if (!all(y.censored == 0 | y.censored == 1)) 
            stop(paste("When 'y.censored' is a numeric vector, all values of", 
                "'y.censored' must be 0 (not censored) or 1 (censored)."))
        y.censored <- as.logical(y.censored)
    }
    n.y.cen <- sum(y.censored)
    if (n.x.cen == 0 && n.y.cen == 0) 
        stop(paste("No censored values indicated by 'x.censored'", 
            "and 'y.censored';", "use \n\t\t\tthe function 'wilcox.test' or", 
            "'twoSampleLinearRankTest'"))
    Si.fcn <- function(ni, di, surv.est) {
        k <- length(ni)
        Si <- switch(surv.est, prentice = cumprod((ni - di + 
            1)/(ni + 1)), `kaplan-meier` = cumprod((ni - di)/ni), 
            `peto-peto` = {
                S.hat <- cumprod((ni - di)/ni)
                (S.hat + c(1, S.hat[1:(k - 1)]))/2
            }, altshuler = exp(-cumsum(di/ni)))
    }
    scores.fcn <- function(ni, di, Si, test) {
        k <- length(ni)
        Fi <- 1 - Si
        i <- 1:k
        switch(test, logrank = {
            ci <- -log(Si) - 1
            Ci <- -log(Si)
        }, gehan = {
            ci <- i - ni
            Ci <- i
        }, `tarone-ware` = {
            ci <- i - sqrt(ni)
            Ci <- i
        }, `peto-peto` = {
            ci <- 2 * Fi - 1
            Ci <- Fi
        }, normal.scores.1 = {
            ci <- qnorm(Fi)
            Ci <- dnorm(qnorm(Fi))/Si
        }, normal.scores.2 = {
            ci <- qnorm(Fi)
            Ci <- numeric(k)
            Ci[1] <- -ci[1]/(ni[1] - 1)
            for (j in 2:(k - 1)) Ci[j] <- (ni[j] * Ci[j - 1] - 
                ci[j])/(ni[j] - 1)
            if (ni[k] > 1) Ci[k] <- (ni[k] * Ci[k - 1] - ci[k])/(ni[k] - 
                1)
        }, generalized.sign = {
            ci <- sign(Fi - 0.5)
            Ci <- ifelse(Fi < 0.5, Fi/(1 - Fi), 1)
        })
        list(ci = ci, Ci = Ci)
    }
    cen.levels.x <- sort(unique(x[x.censored]))
    cen.levels.y <- sort(unique(y[y.censored]))
    if (shift.type == "location" && !missing(location.shift.null)) {
        if ((length(location.shift.null) != 1) || !is.finite(location.shift.null)) 
            stop("'location.shift.null' must be a single finite numeric value")
        x <- x - location.shift.null
    }
    if (shift.type == "scale" && !missing(scale.shift.null)) {
        if ((length(scale.shift.null) != 1) || !is.finite(scale.shift.null) || 
            scale.shift.null <= 0) 
            stop("'scale.shift.null' must be a single finite positive numeric value")
        x <- x/scale.shift.null
    }
    max.z <- max(x, y)
    if (censoring.side == "left") {
        x <- (-x) + max.z + 1
        y <- (-y) + max.z + 1
    }
    n.x <- length(x)
    n.y <- length(y)
    N <- n.x + n.y
    z <- c(x, y)
    x.cen <- x[x.censored]
    y.cen <- y[y.censored]
    z.cen <- c(x.cen, y.cen)
    x.no.cen <- x[!x.censored]
    y.no.cen <- y[!y.censored]
    z.no.cen <- c(x.no.cen, y.no.cen)
    n.x.no.cen <- n.x - n.x.cen
    n.y.no.cen <- n.y - n.y.cen
    ti <- sort(z.no.cen)
    k <- length(ti)
    if (variance == "hypergeometric") {
        ti <- unique(ti)
        k <- length(ti)
        ni <- sapply(1:k, function(i, z, ti) sum(ti[i] <= z), 
            z = z, ti = ti)
        n1i <- sapply(1:k, function(i, x, ti) sum(ti[i] <= x), 
            x = x, ti = ti)
        di <- table(z.no.cen)
        d1i <- numeric(k)
        tab <- table(x.no.cen)
        d1i[match(names(tab), as.character(ti))] <- tab
        Si <- Si.fcn(ni = ni, di = di, surv.est = surv.est)
        scores.list <- scores.fcn(ni = ni, di = di, Si = Si, 
            test = test)
        ci <- scores.list$ci
        Ci <- scores.list$Ci
        wi <- ci - Ci
        nu <- sum(wi * (d1i - (di * n1i)/ni))
        r <- n1i/ni
        term <- di * wi^2 * r * (1 - r) * ((ni - di)/(ni - 1))
        if (ni[k] == 1) 
            term[k] <- 0
        var.nu <- sum(term)
    }
    else {
        rle.ti <- rle(ti)
        uncensored.ties <- !all(rle.ti$lengths == 1)
        if (uncensored.ties) {
            diffs <- diff(sort(z))
            delta <- min(diffs[diffs > 0])/(2 * N)
            delta <- min(delta, 1e-08)
            tie.values <- rle.ti$values[rle.ti$lengths > 1]
            tie.values.n <- rle.ti$lengths[rle.ti$lengths > 1]
            n.tie.values <- length(tie.values)
            new.z.no.cen <- z.no.cen
            for (i in 1:n.tie.values) {
                index <- z.no.cen == tie.values[i]
                new.z.no.cen[index] <- z.no.cen[index] - ((tie.values.n[i] - 
                  1):0) * delta
            }
            new.x.no.cen <- new.z.no.cen[1:n.x.no.cen]
            new.y.no.cen <- new.z.no.cen[(n.x.no.cen + 1):k]
            new.ti <- sort(new.z.no.cen)
            new.x <- x
            new.x[!x.censored] <- new.x.no.cen
            new.y <- y
            new.y[!y.censored] <- new.y.no.cen
            new.z <- c(new.x, new.y)
            ni <- sapply(1:k, function(i, z, ti) sum(ti[i] <= 
                z), z = new.z, ti = new.ti)
            n1i <- sapply(1:k, function(i, x, ti) sum(ti[i] <= 
                x), x = new.x, ti = new.ti)
            di <- table(new.z.no.cen)
            d1i <- numeric(k)
            tab <- table(new.x.no.cen)
            d1i[match(names(tab), as.character(new.ti))] <- tab
            Si <- Si.fcn(ni = ni, di = di, surv.est = surv.est)
            scores.list <- scores.fcn(ni = ni, di = di, Si = Si, 
                test = test)
            ci <- scores.list$ci
            Ci <- scores.list$Ci
            ci <- sapply(split(ci, ti), mean)
            Ci <- sapply(split(Ci, ti), mean)
            if (variance == "asymptotic") {
                ai <- cumprod((ni + 1)/(ni + 2))
                ai <- sapply(split(ai, ti), mean)
                Si <- sapply(split(Si, ti), mean)
            }
            ti <- unique(ti)
            k <- length(ti)
            ni <- sapply(1:k, function(i, z, ti) sum(ti[i] <= 
                z), z = z, ti = ti)
            n1i <- sapply(1:k, function(i, x, ti) sum(ti[i] <= 
                x), x = x, ti = ti)
            di <- table(z.no.cen)
            d1i <- numeric(k)
            tab <- table(x.no.cen)
            d1i[match(names(tab), as.character(ti))] <- tab
            ei <- sapply(1:k, function(i, z, ti) sum(ti[i] <= 
                z & z < ti[i + 1]), z = z.cen, ti = c(ti, Inf))
            if (n.x.cen > 0) 
                e1i <- sapply(1:k, function(i, x, ti) sum(ti[i] <= 
                  x & x < ti[i + 1]), x = x.cen, ti = c(ti, Inf))
            else e1i <- rep(0, k)
            nu <- sum(d1i * ci + e1i * Ci)
            var.nu <- switch(variance, permutation = ((n.x * 
                n.y)/(N * (N - 1))) * sum(di * ci^2 + ei * Ci^2), 
                , asymptotic = {
                  bi <- 2 * d1i + e1i
                  dum <- sum(Si * bi) - cumsum(Si * bi)
                  sum(Si * (1 - ai) * bi - (ai - Si) * bi * (Si * 
                    bi + 2 * dum))
                })
        }
        else {
            ni <- sapply(1:k, function(i, z, ti) sum(ti[i] <= 
                z), z = z, ti = ti)
            n1i <- sapply(1:k, function(i, x, ti) sum(ti[i] <= 
                x), x = x, ti = ti)
            di <- table(z.no.cen)
            d1i <- numeric(k)
            tab <- table(x.no.cen)
            d1i[match(names(tab), as.character(ti))] <- tab
            ei <- sapply(1:k, function(i, z, ti) sum(ti[i] <= 
                z & z < ti[i + 1]), z = z.cen, ti = c(ti, Inf))
            if (n.x.cen > 0) 
                e1i <- sapply(1:k, function(i, x, ti) sum(ti[i] <= 
                  x & x < ti[i + 1]), x = x.cen, ti = c(ti, Inf))
            else e1i <- rep(0, k)
            Si <- Si.fcn(ni = ni, di = di, surv.est = surv.est)
            scores.list <- scores.fcn(ni = ni, di = di, Si = Si, 
                test = test)
            ci <- scores.list$ci
            Ci <- scores.list$Ci
            nu <- sum(d1i * ci + e1i * Ci)
            var.nu <- switch(variance, permutation = ((n.x * 
                n.y)/(N * (N - 1))) * sum(di * ci^2 + ei * Ci^2), 
                , asymptotic = {
                  ai <- cumprod((ni + 1)/(ni + 2))
                  bi <- 2 * d1i + e1i
                  dum <- sum(Si * bi) - cumsum(Si * bi)
                  sum(Si * (1 - ai) * bi - (ai - Si) * bi * (Si * 
                    bi + 2 * dum))
                })
        }
    }
    z <- nu/sqrt(var.nu)
    if (censoring.side == "left") {
        nu <- -nu
        z <- -z
    }
    p.value <- switch(alternative, two.sided = 2 * (1 - pnorm(abs(z))), 
        less = pnorm(z), greater = 1 - pnorm(z))
    stat <- c(nu, var.nu, z)
    names(stat) <- c("nu", "var.nu", "z")
    parameters <- NULL
    string <- switch(alternative, two.sided = "!=", less = "<", 
        greater = ">")
    null.value <- "Fx(t)"
    if (shift.type == "location") 
        names(null.value) <- ifelse(missing(location.shift.null), 
            "Fy(t)", paste("Fy(t - ", location.shift.null, ")", 
                sep = ""))
    else {
        names(null.value) <- ifelse(missing(scale.shift.null), 
            "Fy(t)", paste("Fy(t / ", scale.shift.null, ")", 
                sep = ""))
    }
    alternative <- paste(names(null.value), string, "Fx(t) for at least one t")
    sep.string <- paste("\n", space(33), sep = "")
    string0 <- paste(switch(surv.est, prentice = "Prentice", 
        `kaplan-meier` = "Kaplan-Meier", `peto-peto` = "Peto-Peto", 
        altshuler = "Altshuler"), "Survival Estimator")
    string1 <- switch(test, gehan = "Gehan's Test", logrank = "Logrank Test", 
        `tarone-ware` = "Tarone-Ware Test", `peto-peto` = paste("Peto-Peto Test Using", 
            string0, sep = sep.string), normal.scores.1 = paste("Normal Scores Test Using", 
            string0, "Based on Prentice (1978)", sep = sep.string), 
        normal.scores.2 = paste("Normal Scores Test Using", string0, 
            "Based on Prentice and Marek (1979)", sep = sep.string), 
        generalized.sign = "Generalized Sign Test")
    string2 <- switch(variance, permutation = "with Permutation Variance", 
        hypergeometric = "with Hypergeometric Variance", asymptotic = "with Asymptotic Variance")
    method <- paste("Two-Sample Linear Rank Test:", string1, 
        string2, sep = sep.string)
    ret.list <- list(statistic = stat, parameters = parameters, 
        p.value = p.value, estimate = NULL, null.value = null.value, 
        alternative = alternative, method = method, estimation.method = NULL, 
        sample.size = c(nx = n.x, ny = n.y), data.name = data.name, 
        bad.obs = c(x = bad.obs.x, y = bad.obs.y), censoring.side = censoring.side, 
        censoring.name = censoring.name, censoring.levels = list(x = cen.levels.x, 
            y = cen.levels.y), percent.censored = c(x = (100 * 
            n.x.cen)/n.x, y = (100 * n.y.cen)/n.y))
    oldClass(ret.list) <- "htestCensored"
    ret.list
}
