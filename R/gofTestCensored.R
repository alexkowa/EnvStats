#' Goodness-of-Fit Test for Normal or Lognormal Distribution Based on Censored Data
#' @description
#' Perform a goodness-of-fit test to determine whether a data set
#'   appears to come from a \link[stats:Normal]{normal distribution},
#'   \link[stats:Lognormal]{lognormal distribution}, or
#'   \link[=LognormalAlt]{lognormal distribution (alternative parameterization)}
#'   based on a sample of data that has been subjected to Type I or Type II
#'   censoring.
#' @usage
#' gofTestCensored(x, censored, censoring.side = "left", test = "sf",
#'     distribution = "norm", est.arg.list = NULL,
#'     prob.method = "hirsch-stedinger", plot.pos.con = 0.375,
#'     keep.data = TRUE, data.name = NULL, censoring.name = NULL)
#' @rawRd
#' \arguments{
#'   \item{x}{
#'   numeric vector of observations.
#'   Missing (\code{NA}), undefined (\code{NaN}),
#'   and infinite (\code{Inf}, \code{-Inf}) values are allowed but will be
#'   removed.
#' }
#'   \item{censored}{
#'   numeric or logical vector indicating which values of \code{x} are censored.
#'   This must be the same length as \code{x}.  If the mode of \code{censored} is
#'   \code{"logical"}, \code{TRUE} values correspond to elements of \code{x} that
#'   are censored, and \code{FALSE} values correspond to elements of \code{x} that
#'   are not censored.  If the mode of \code{censored} is \code{"numeric"},
#'   it must contain only \code{1}'s and \code{0}'s; \code{1} corresponds to
#'   \code{TRUE} and \code{0} corresponds to \code{FALSE}.  Missing (\code{NA})
#'   values are allowed but will be removed.
#' }
#'   \item{censoring.side}{
#'   character string indicating on which side the censoring occurs.  The possible
#'   values are \code{"left"} (the default) and \code{"right"}.
#' }
#'   \item{test}{
#'   character string defining which goodness-of-fit test to perform.  Possible values are:
#'   \code{"sw"} Shapiro-Wilk.
#'   \code{"sf"} Shapiro-Francia; the default.
#'   \code{"ppcc"} Probability Plot Correlation Coefficient.
#'
#'   The Shapiro-Wilk test is only available for singly censored data.
#'
#'   See the DETAILS section for more information.
#' }
#'   \item{distribution}{
#'   a character string denoting the abbreviation of the assumed distribution.
#'   Only continous distributions are allowed.  See the help file for
#'   \code{\link{Distribution.df}} for a list of distributions and their abbreviations.
#'   Examples of possible values are:\cr
#'   \code{distribution="norm"} (\link{Normal} distribution; the default), \cr
#'   \code{distribution="lnorm"} (\link{Lognormal} distribution), \cr
#'   \code{distribution="lnormAlt"} (\link[=LognormalAlt]{Lognormal distribution,
#'   alternative parameterization}, \cr
#'   \code{distribution="gamma"} (\link[=GammaDist]{Gamma} distribution), \cr
#'   \code{distribution="gammaAlt"} (\link[=GammaAlt]{Gamma} distribution,
#'   alternative parameterization).
#'
#'   The results for the goodness-of-fit test are
#'   identical for \code{distribution="lnorm"} and \code{distribution="lnormAlt"}, the
#'   only difference in ouput is that when \code{distribution="lnorm"} the
#'   returned estimated parameters are the mean and standard deviation based on the
#'   log-scale of the data, whereas when \code{distribution="lnormAlt"} the
#'   returned estimated parameters are the mean and coefficient of variation
#'   based on the original scale of the data.
#'
#'   Also, the results for the goodness-of-fit test are
#'   identical for \code{distribution="gamma"} and \code{distribution="gammaAlt"}, the
#'   only difference in ouput is that when \code{distribution="gamma"} the
#'   returned estimated parameters are the shape and scale, whereas when
#'   \code{distribution="lnormAlt"} the returned estimated parameters are the
#'   mean and coefficient of variation.
#' }
#'   \item{est.arg.list}{
#'   a list of arguments to be passed to the function estimating the distribution
#'   parameters.  For example, if \code{distribution="lnormAlt"} setting \cr
#'   \code{est.arg.list=list(method="bcmle")} indicates using the bias-corrected
#'   maximum likelihood estimators (see the help file for \code{\link{elnormAltCensored}}).
#'   The default value is \code{est.arg.list=NULL} so that all default values for the
#'   estimating function are used.  The estimated parameters are provided in the
#'   output merely for information, and the choice of the method of estimation has no
#'   effect on the goodness-of-fit test statistic or p-value.
#' }
#'   \item{prob.method}{
#'   character string indicating what method to use to compute the plotting positions
#'   (empirical probabilities) when \code{test="sf"} or \code{test="ppcc"}.
#'   Possible values are: \cr
#'   \code{"modified kaplan-meier"} (modification of product-limit method of Kaplan and Meier (1958)), \cr
#'   \code{"nelson"} (hazard plotting method of Nelson (1972)), \cr
#'   \code{"michael-schucany"} (generalization of the product-limit method due to Michael and Schucany (1986)), and \cr
#'   \code{"hirsch-stedinger"} (generalization of the product-limit method due to Hirsch and Stedinger (1987)). \cr
#'   The default value is \code{prob.method="hirsch-stedinger"}.
#'
#'   The \code{"nelson"} method is only available for \code{censoring.side="right"}, and
#'   the \code{"modified kaplan-meier"} method is only available for
#'   \code{censoring.side="left"}.  See the DETAILS section and the help file for
#'   \code{\link{ppointsCensored}} for more information.
#' }
#'   \item{plot.pos.con}{
#'   numeric scalar between 0 and 1 containing the value of the plotting position
#'   constant to use when \code{test="sf"} or \code{test="ppcc"}.  The default value is \cr
#'   \code{plot.pos.con=0.375}.  See the DETAILS section and the help file for \cr
#'   \code{\link{ppointsCensored}} for more information.
#' }
#'   \item{keep.data}{
#'   logical scalar indicating whether to return the original data.  The
#'   default value is \code{keep.data=TRUE}.
#' }
#'   \item{data.name}{
#'   optional character string indicating the name for the data used for argument \code{x}.
#' }
#'   \item{censoring.name}{
#'   optional character string indicating the name for the data used for argument \code{censored}.
#' }
#' }
#' @details
#' This help page has been shortened to keep function help focused on usage,
#' arguments, return values, and examples. Extended method details, formulas,
#' and background material are available in \code{vignette("extended-function-details", package = "EnvStats")}, section \code{gofTestCensored}.
#' @rawRd
#' \value{
#'   a list of class \code{"gofCensored"} containing the results of the goodness-of-fit
#'   test.  See the help files for \code{\link{gofCensored.object}} for details.
#' }
#' @rawRd
#' \references{
#'   Birnbaum, Z.W., and F.H. Tingey. (1951).
#'   One-Sided Confidence Contours for Probability Distribution Functions.
#'   \emph{Annals of Mathematical Statistics} \bold{22}, 592-596.
#'
#'   Blom, G. (1958). \emph{Statistical Estimates and Transformed Beta Variables}.
#'   John Wiley and Sons, New York.
#'
#'   Conover, W.J. (1980). \emph{Practical Nonparametric Statistics}. Second Edition.
#'   John Wiley and Sons, New York.
#'
#'   Dallal, G.E., and L. Wilkinson. (1986).
#'   An Analytic Approximation to the Distribution of Lilliefor's Test for Normality.
#'   \emph{The American Statistician} \bold{40}, 294-296.
#'
#'   D'Agostino, R.B. (1970). Transformation to Normality of the Null Distribution of \eqn{g1}.
#'   \emph{Biometrika} \bold{57}, 679-681.
#'
#'   D'Agostino, R.B. (1971). An Omnibus Test of Normality for Moderate and Large Size Samples.
#'   \emph{Biometrika} \bold{58}, 341-348.
#'
#'   D'Agostino, R.B. (1986b). Tests for the Normal Distribution. In: D'Agostino, R.B., and M.A. Stephens, eds.
#'   \emph{Goodness-of Fit Techniques}. Marcel Dekker, New York.
#'
#'   D'Agostino, R.B., and E.S. Pearson (1973). Tests for Departures from Normality.
#'   Empirical Results for the Distributions of \eqn{b2} and \eqn{\sqrt{b1}}.
#'   \emph{Biometrika} \bold{60}(3), 613-622.
#'
#'   D'Agostino, R.B., and G.L. Tietjen (1973). Approaches to the Null Distribution of \eqn{\sqrt{b1}}.
#'   \emph{Biometrika} \bold{60}(1), 169-173.
#'
#'   Fisher, R.A. (1950). \emph{Statistical Methods for Research Workers}. 11'th Edition.
#'   Hafner Publishing Company, New York, pp.99-100.
#'
#'   Gibbons, R.D., D.K. Bhaumik, and S. Aryal. (2009).
#'   \emph{Statistical Methods for Groundwater Monitoring}, Second Edition.
#'   John Wiley & Sons, Hoboken.
#'
#'   Kendall, M.G., and A. Stuart. (1991).
#'   \emph{The Advanced Theory of Statistics, Volume 2: Inference and Relationship}.
#'   Fifth Edition. Oxford University Press, New York.
#'
#'   Royston, J.P. (1992a). Approximating the Shapiro-Wilk W-Test for Non-Normality.
#'   \emph{Statistics and Computing} \bold{2}, 117-119.
#'
#'   Royston, J.P. (1992b).
#'   Estimation, Reference Ranges and Goodness of Fit for the Three-Parameter Log-Normal Distribution.
#'   \emph{Statistics in Medicine} \bold{11}, 897-912.
#'
#'   Royston, J.P. (1992c).
#'   A Pocket-Calculator Algorithm for the Shapiro-Francia Test of Non-Normality: An Application to Medicine.
#'   \emph{Statistics in Medicine} \bold{12}, 181-184.
#'
#'   Royston, P. (1993). A Toolkit for Testing for Non-Normality in Complete and Censored Samples.
#'   \emph{The Statistician} \bold{42}, 37-43.
#'
#'   Ryan, T., and B. Joiner. (1973). \emph{Normal Probability Plots and Tests for Normality}.
#'   Technical Report, Pennsylvannia State University, Department of Statistics.
#'
#'   Shapiro, S.S., and R.S. Francia. (1972). An Approximate Analysis of Variance Test for Normality.
#'   \emph{Journal of the American Statistical Association} \bold{67}(337), 215-219.
#'
#'   Shapiro, S.S., and M.B. Wilk. (1965). An Analysis of Variance Test for Normality (Complete Samples).
#'   \emph{Biometrika} \bold{52}, 591-611.
#'
#'   Verrill, S., and R.A. Johnson. (1987).
#'   The Asymptotic Equivalence of Some Modified Shapiro-Wilk Statistics -- Complete and Censored Sample Cases.
#'   \emph{The Annals of Statistics} \bold{15}(1), 413-419.
#'
#'   Verrill, S., and R.A. Johnson. (1988).
#'   Tables and Large-Sample Distribution Theory for Censored-Data Correlation Statistics for Testing Normality.
#'   \emph{Journal of the American Statistical Association} \bold{83}, 1192-1197.
#'
#'   Weisberg, S., and C. Bingham. (1975).
#'   An Approximate Analysis of Variance Test for Non-Normality Suitable for Machine Calculation.
#'   \emph{Technometrics} \bold{17}, 133-134.
#' }
#' @rawRd
#' \author{
#'   Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \note{
#'   The Shapiro-Wilk test (Shapiro and Wilk, 1965) and the Shapiro-Francia test
#'   (Shapiro and Francia, 1972) are probably the two most commonly used hypothesis tests to
#'   test departures from normality.  The Shapiro-Wilk test is most powerful at detecting
#'   short-tailed (platykurtic) and skewed distributions, and least powerful against
#'   symmetric, moderately long-tailed (leptokurtic) distributions.  Conversely, the
#'   Shapiro-Francia test is more powerful against symmetric long-tailed distributions and
#'   less powerful against short-tailed distributions (Royston, 1992b; 1993).
#'
#'   In practice, almost any goodness-of-fit test will \emph{not} reject the null hypothesis
#'   if the number of observations is relatively small.  Conversely, almost any goodness-of-fit
#'   test \emph{will} reject the null hypothesis if the number of observations is very large,
#'   since \dQuote{real} data are never distributed according to any theoretical distribution
#'   (Conover, 1980, p.367).  For most cases, however, the distribution of \dQuote{real} data
#'   is close enough to some theoretical distribution that fairly accurate results may be
#'   provided by assuming that particular theoretical distribution.  One way to asses the
#'   goodness of the fit is to use goodness-of-fit tests.  Another way is to look at
#'   quantile-quantile (Q-Q) plots (see \code{\link{qqPlotCensored}}).
#' }
#' @rawRd
#' \seealso{
#'   \code{\link{gofTest}}, \code{\link{gofCensored.object}},
#'   \code{\link{print.gofCensored}}, \code{\link{plot.gofCensored}},
#'   \code{\link{shapiro.test}}, \link{Normal}, \link{Lognormal},
#'   \code{\link{enormCensored}}, \code{\link{elnormCensored}},
#'   \code{\link{elnormAltCensored}}, \code{\link{qqPlotCensored}}.
#' }
#' @rawRd
#' \examples{
#'   # Generate 30 observations from a gamma distribution with
#'   # parameters mean=10 and cv=1 and then censor observations less than 5.
#'   # Then test the hypothesis that these data came from a gamma
#'   # distribution using the Shapiro-Wilk test.
#'   #
#'   # The p-value for the complete data is p = 0.86, while
#'   # the p-value for the censored data is p = 0.52.
#'   # (Note:  the call to set.seed lets you reproduce this example.)
#'
#'   set.seed(598)
#'
#'   dat <- sort(rgammaAlt(30, mean = 10, cv = 1))
#'   dat
#'   # [1]  0.5313509  1.4741833  1.9936208  2.7980636  3.4509840
#'   # [6]  3.7987348  4.5542952  5.5207531  5.5253596  5.7177872
#'   #[11]  5.7513827  9.1086375  9.8444090 10.6247123 10.9304922
#'   #[16] 11.7925398 13.3432689 13.9562777 14.6029065 15.0563342
#'   #[21] 15.8730642 16.0039936 16.6910715 17.0288922 17.8507891
#'   #[26] 19.1105522 20.2657141 26.3815970 30.2912797 42.8726101
#'
#'   dat.censored <- dat
#'   censored <- dat.censored < 5
#'   dat.censored[censored] <- 5
#'
#'   # Results for complete data:
#'   #---------------------------
#'   gofTest(dat, test = "sw", dist = "gammaAlt")
#'
#'   #Results of Goodness-of-Fit Test
#'   #-------------------------------
#'   #
#'   #Test Method:                     Shapiro-Wilk GOF Based on
#'   #                                 Chen & Balakrisnan (1995)
#'   #
#'   #Hypothesized Distribution:       Gamma
#'   #
#'   #Estimated Parameter(s):          mean = 12.4248552
#'   #                                 cv   =  0.7901752
#'   #
#'   #Estimation Method:               MLE
#'   #
#'   #Data:                            dat
#'   #
#'   #Sample Size:                     30
#'   #
#'   #Test Statistic:                  W = 0.981471
#'   #
#'   #Test Statistic Parameter:        n = 30
#'   #
#'   #P-value:                         0.8631802
#'   #
#'   #Alternative Hypothesis:          True cdf does not equal the
#'   #                                 Gamma Distribution.
#'
#'
#'   # Results for censored data:
#'   #---------------------------
#'   gof.list <- gofTestCensored(dat.censored, censored, test = "sw",
#'     distribution = "gammaAlt")
#'   gof.list
#'
#'   #Results of Goodness-of-Fit Test
#'   #Based on Type I Censored Data
#'   #-------------------------------
#'   #
#'   #Test Method:                     Shapiro-Wilk GOF
#'   #                                 (Singly Censored Data)
#'   #                                 Based on Chen & Balakrisnan (1995)
#'   #
#'   #Hypothesized Distribution:       Gamma
#'   #
#'   #Censoring Side:                  left
#'   #
#'   #Censoring Level(s):              5
#'   #
#'   #Estimated Parameter(s):          mean = 12.4911448
#'   #                                 cv   =  0.7617343
#'   #
#'   #Estimation Method:               MLE
#'   #
#'   #Data:                            dat.censored
#'   #
#'   #Censoring Variable:              censored
#'   #
#'   #Sample Size:                     30
#'   #
#'   #Percent Censored:                23.3%
#'   #
#'   #Test Statistic:                  W = 0.9613711
#'   #
#'   #Test Statistic Parameters:       N     = 30.0000000
#'   #                                 DELTA =  0.2333333
#'   #
#'   #P-value:                         0.522329
#'   #
#'   #Alternative Hypothesis:          True cdf does not equal the
#'   #                                 Gamma Distribution.
#'
#'   # Plot the results for the censored data
#'   #---------------------------------------
#'   dev.new()
#'   plot(gof.list)
#'
#'   #==========
#'
#'   # Continue the above example, but now test the hypothesis that
#'   # these data came from a lognormal distribution
#'   # (alternative parameterization) using the Shapiro-Wilk test.
#'   #
#'   # The p-value for the complete data is p = 0.056, while
#'   # the p-value for the censored data is p = 0.11.
#'
#'   # Results for complete data:
#'   #---------------------------
#'   gofTest(dat, test = "sw", dist = "lnormAlt")
#'
#'   #Results of Goodness-of-Fit Test
#'   #-------------------------------
#'   #
#'   #Test Method:                     Shapiro-Wilk GOF
#'   #
#'   #Hypothesized Distribution:       Lognormal
#'   #
#'   #Estimated Parameter(s):          mean = 13.757239
#'   #                                 cv   =  1.148872
#'   #
#'   #Estimation Method:               mvue
#'   #
#'   #Data:                            dat
#'   #
#'   #Sample Size:                     30
#'   #
#'   #Test Statistic:                  W = 0.9322226
#'   #
#'   #Test Statistic Parameter:        n = 30
#'   #
#'   #P-value:                         0.05626823
#'   #
#'   #Alternative Hypothesis:          True cdf does not equal the
#'   #                                 Lognormal Distribution.
#'
#'
#'   # Results for censored data:
#'   #---------------------------
#'   gof.list <- gofTestCensored(dat.censored, censored, test = "sw",
#'     distribution = "lnormAlt")
#'   gof.list
#'
#'   #Results of Goodness-of-Fit Test
#'   #Based on Type I Censored Data
#'   #-------------------------------
#'   #
#'   #Test Method:                     Shapiro-Wilk GOF
#'   #                                 (Singly Censored Data)
#'   #
#'   #Hypothesized Distribution:       Lognormal
#'   #
#'   #Censoring Side:                  left
#'   #
#'   #Censoring Level(s):              5
#'   #
#'   #Estimated Parameter(s):          mean = 13.0382221
#'   #                                 cv   =  0.9129512
#'   #
#'   #Estimation Method:               MLE
#'   #
#'   #Data:                            dat.censored
#'   #
#'   #Censoring Variable:              censored
#'   #
#'   #Sample Size:                     30
#'   #
#'   #Percent Censored:                23.3%
#'   #
#'   #Test Statistic:                  W = 0.9292406
#'   #
#'   #Test Statistic Parameters:       N     = 30.0000000
#'   #                                 DELTA =  0.2333333
#'   #
#'   #P-value:                         0.114511
#'   #
#'   #Alternative Hypothesis:          True cdf does not equal the
#'   #                                 Lognormal Distribution.
#'
#'   # Plot the results for the censored data
#'   #---------------------------------------
#'   dev.new()
#'   plot(gof.list)
#'
#'   #----------
#'
#'   # Redo the above example, but specify the quasi-minimum variance
#'   # unbiased estimator of the mean.  Note that the method of
#'   # estimating the parameters has no effect on the goodness-of-fit
#'   # test (see the DETAILS section above).
#'
#'   gofTestCensored(dat.censored, censored, test = "sw",
#'     distribution = "lnormAlt", est.arg.list = list(method = "qmvue"))
#'
#'   #Results of Goodness-of-Fit Test
#'   #Based on Type I Censored Data
#'   #-------------------------------
#'   #
#'   #Test Method:                     Shapiro-Wilk GOF
#'   #                                 (Singly Censored Data)
#'   #
#'   #Hypothesized Distribution:       Lognormal
#'   #
#'   #Censoring Side:                  left
#'   #
#'   #Censoring Level(s):              5
#'   #
#'   #Estimated Parameter(s):          mean = 12.8722749
#'   #                                 cv   =  0.8712549
#'   #
#'   #Estimation Method:               Quasi-MVUE
#'   #
#'   #Data:                            dat.censored
#'   #
#'   #Censoring Variable:              censored
#'   #
#'   #Sample Size:                     30
#'   #
#'   #Percent Censored:                23.3%
#'   #
#'   #Test Statistic:                  W = 0.9292406
#'   #
#'   #Test Statistic Parameters:       N     = 30.0000000
#'   #                                 DELTA =  0.2333333
#'   #
#'   #P-value:                         0.114511
#'   #
#'   #Alternative Hypothesis:          True cdf does not equal the
#'   #                                 Lognormal Distribution.
#'
#'   #----------
#'   # Clean up
#'
#'   rm(dat, dat.censored, censored, gof.list)
#'   graphics.off()
#'
#'   #==========
#'
#'   # Check the assumption that the silver data stored in Helsel.Cohn.88.silver.df
#'   # follows a lognormal distribution and plot the goodness-of-fit test results.
#'   # Note that the small p-value and the shape of the Q-Q plot
#'   # (an inverted S-shape) suggests that the log transformation is not quite strong
#'   # enough to "bring in" the tails (i.e., the log-transformed silver data has tails
#'   # that are slightly too long relative to a normal distribution).
#'   # Helsel and Cohn (1988, p.2002) note that the gross outlier of 560 mg/L tends to
#'   # make the shape of the data resemble a gamma distribution.
#'
#'   dum.list <- with(Helsel.Cohn.88.silver.df,
#'     gofTestCensored(Ag, Censored, test = "sf", dist = "lnorm"))
#'
#'   dum.list
#'   #Results of Goodness-of-Fit Test
#'   #Based on Type I Censored Data
#'   #-------------------------------
#'   #
#'   #Test Method:                     Shapiro-Francia GOF
#'   #                                 (Multiply Censored Data)
#'   #
#'   #Hypothesized Distribution:       Lognormal
#'   #
#'   #Censoring Side:                  left
#'   #
#'   #Censoring Level(s):               0.1  0.2  0.3  0.5  1.0  2.0  2.5  5.0
#'   #                                  6.0 10.0 20.0 25.0
#'   #
#'   #Estimated Parameter(s):          meanlog = -1.040572
#'   #                                 sdlog   =  2.354847
#'   #
#'   #Estimation Method:               MLE
#'   #
#'   #Data:                            Ag
#'   #
#'   #Censoring Variable:              Censored
#'   #
#'   #Sample Size:                     56
#'   #
#'   #Percent Censored:                60.7%
#'   #
#'   #Test Statistic:                  W = 0.8957198
#'   #
#'   #Test Statistic Parameters:       N     = 56.0000000
#'   #                                 DELTA =  0.6071429
#'   #
#'   #P-value:                         0.03490314
#'   #
#'   #Alternative Hypothesis:          True cdf does not equal the
#'   #                                 Lognormal Distribution.
#'
#'   dev.new()
#'   plot(dum.list)
#'
#'   #----------
#'
#'   # Clean up
#'   #---------
#'
#'   rm(dum.list)
#'   graphics.off()
#'
#'   #==========
#'
#'   # Chapter 15 of USEPA (2009) gives several examples of looking
#'   # at normal Q-Q plots and estimating the mean and standard deviation
#'   # for manganese concentrations (ppb) in groundwater at five background wells.
#'   # In EnvStats these data are stored in the data frame
#'   # EPA.09.Ex.15.1.manganese.df.
#'
#'   # Here we will test whether the data appear to come from a normal
#'   # distribution, then we will test to see whether they appear to come
#'   # from a lognormal distribution.
#'   #--------------------------------------------------------------------
#'
#'
#'   # First look at the data:
#'   #-----------------------
#'
#'   EPA.09.Ex.15.1.manganese.df
#'
#'   #   Sample   Well Manganese.Orig.ppb Manganese.ppb Censored
#'   #1       1 Well.1                 <5           5.0     TRUE
#'   #2       2 Well.1               12.1          12.1    FALSE
#'   #3       3 Well.1               16.9          16.9    FALSE
#'   #...
#'   #23      3 Well.5                3.3           3.3    FALSE
#'   #24      4 Well.5                8.4           8.4    FALSE
#'   #25      5 Well.5                 <2           2.0     TRUE
#'
#'   longToWide(EPA.09.Ex.15.1.manganese.df,
#'     "Manganese.Orig.ppb", "Sample", "Well",
#'     paste.row.name = TRUE)
#'
#'   #         Well.1 Well.2 Well.3 Well.4 Well.5
#'   #Sample.1     <5     <5     <5    6.3   17.9
#'   #Sample.2   12.1    7.7    5.3   11.9   22.7
#'   #Sample.3   16.9   53.6   12.6     10    3.3
#'   #Sample.4   21.6    9.5  106.3     <2    8.4
#'   #Sample.5     <2   45.9   34.5   77.2     <2
#'
#'
#'   # Now test whether the data appear to come from
#'   # a normal distribution.  Note that these data
#'   # are multiply censored, so we'll use the
#'   # Shapiro-Francia test.
#'   #----------------------------------------------
#'
#'   gof.normal <- with(EPA.09.Ex.15.1.manganese.df,
#'     gofTestCensored(Manganese.ppb, Censored, test = "sf"))
#'
#'   gof.normal
#'
#'   #Results of Goodness-of-Fit Test
#'   #Based on Type I Censored Data
#'   #-------------------------------
#'   #
#'   #Test Method:                     Shapiro-Francia GOF
#'   #                                 (Multiply Censored Data)
#'   #
#'   #Hypothesized Distribution:       Normal
#'   #
#'   #Censoring Side:                  left
#'   #
#'   #Censoring Level(s):              2 5
#'   #
#'   #Estimated Parameter(s):          mean = 15.23508
#'   #                                 sd   = 30.62812
#'   #
#'   #Estimation Method:               MLE
#'   #
#'   #Data:                            Manganese.ppb
#'   #
#'   #Censoring Variable:              Censored
#'   #
#'   #Sample Size:                     25
#'   #
#'   #Percent Censored:                24%
#'   #
#'   #Test Statistic:                  W = 0.8368016
#'   #
#'   #Test Statistic Parameters:       N     = 25.00
#'   #                                 DELTA =  0.24
#'   #
#'   #P-value:                         0.004662658
#'   #
#'   #Alternative Hypothesis:          True cdf does not equal the
#'   #                                 Normal Distribution.
#'
#'   # Plot the results:
#'   #------------------
#'
#'   dev.new()
#'   plot(gof.normal)
#'
#'   #----------
#'
#'   # Now test to see whether the data appear to come from
#'   # a lognormal distribuiton.
#'   #-----------------------------------------------------
#'
#'   gof.lognormal <- with(EPA.09.Ex.15.1.manganese.df,
#'     gofTestCensored(Manganese.ppb, Censored, test = "sf",
#'     distribution = "lnorm"))
#'
#'   gof.lognormal
#'
#'   #Results of Goodness-of-Fit Test
#'   #Based on Type I Censored Data
#'   #-------------------------------
#'   #
#'   #Test Method:                     Shapiro-Francia GOF
#'   #                                 (Multiply Censored Data)
#'   #
#'   #Hypothesized Distribution:       Lognormal
#'   #
#'   #Censoring Side:                  left
#'   #
#'   #Censoring Level(s):              2 5
#'   #
#'   #Estimated Parameter(s):          meanlog = 2.215905
#'   #                                 sdlog   = 1.356291
#'   #
#'   #Estimation Method:               MLE
#'   #
#'   #Data:                            Manganese.ppb
#'   #
#'   #Censoring Variable:              Censored
#'   #
#'   #Sample Size:                     25
#'   #
#'   #Percent Censored:                24%
#'   #
#'   #Test Statistic:                  W = 0.9864426
#'   #
#'   #Test Statistic Parameters:       N     = 25.00
#'   #                                 DELTA =  0.24
#'   #
#'   #P-value:                         0.9767731
#'   #
#'   #Alternative Hypothesis:          True cdf does not equal the
#'   #                                 Lognormal Distribution.
#'
#'   # Plot the results:
#'   #------------------
#'
#'   dev.new()
#'   plot(gof.lognormal)
#'
#'   #----------
#'
#'   # Clean up
#'   #---------
#'
#'   rm(gof.normal, gof.lognormal)
#'   graphics.off()
#' }
#' @rawRd
#' \keyword{ htest }
#' @rawRd
#' \keyword{ models }

gofTestCensored <-
function (x, censored, censoring.side = "left", test = "sf", 
    distribution = "norm", est.arg.list = NULL, prob.method = "hirsch-stedinger", 
    plot.pos.con = 0.375, keep.data = TRUE, data.name = NULL, 
    censoring.name = NULL) 
{
    distribution <- check.distribution.args(distribution, check.params = FALSE)$dist.abb
    if (!is.vector(x, mode = "numeric")) 
        stop("'x' must be a numeric vector")
    if (is.null(data.name)) 
        data.name <- deparse(substitute(x))
    if (!is.vector(censored, mode = "numeric") & !is.vector(censored, 
        mode = "logical")) 
        stop("'censored' must be a logical or numeric vector")
    if (length(censored) != length(x)) 
        stop("'censored' must be the same length as 'x'")
    if (is.numeric(censored)) {
        index <- is.finite(censored)
        if (!all(is.element(censored[index], 0:1))) 
            stop(paste("When 'censored' is a numeric vector, all non-missing values of", 
                "'censored' must be 0 (not censored) or 1 (censored)."))
    }
    if (is.null(censoring.name)) 
        censoring.name <- deparse(substitute(censored))
    censoring.side <- match.arg(censoring.side, c("left", "right"))
    test <- match.arg(test, c("sw", "sf", "ppcc"))
    if (test == "ppcc") 
        test <- "ppccNorm"
    if ((bad.obs <- sum(!(ok <- is.finite(x) & is.finite(as.numeric(censored))))) > 
        0) {
        is.not.finite.warning(x)
        is.not.finite.warning(as.numeric(censored))
        x <- x[ok]
        censored <- censored[ok]
        warning(paste(bad.obs, "observations with NA/NaN/Inf in 'x' and/or 'censored' removed."))
    }
    if (is.numeric(censored)) 
        censored <- as.logical(censored)
    n.cen <- sum(censored)
    if (n.cen == 0) {
        warning(paste("No censored values indicated by 'censored',", 
            "so the function 'gofTest' was called."))
        ret.list <- gofTest(y = x, test = test, distribution = distribution, 
            est.arg.list = est.arg.list)
        ret.list$data.name <- data.name
        ret.list$bad.obs <- bad.obs
        return(ret.list)
    }
    x.no.cen <- x[!censored]
    if (length(unique(x.no.cen)) < 2) 
        stop("'x' must contain at least 2 non-missing, uncensored, distinct values.")
    if (any(distribution == c("lnorm", "lnormAlt")) && any(x <= 
        0)) 
        stop("All non-missing values of 'x' must be positive for a lognormal distribution")
    multiple <- TRUE
    T.vec <- unique(x[censored])
    if (length(T.vec) == 1) {
        if (censoring.side == "left") {
            if (T.vec <= min(x.no.cen)) 
                multiple <- FALSE
        }
        else {
            if (T.vec >= max(x.no.cen)) 
                multiple <- FALSE
        }
    }
    if (multiple) {
        if (test == "sw") 
            stop(paste("Shapiro-Wilk test not available for multiply censored data.", 
                "Set test='sf' or test='ppcc'."))
        prob.method <- match.arg(prob.method, c("hirsch-stedinger", 
            "michael-schucany", "modified kaplan-meier", "nelson"))
        if (censoring.side == "left" & prob.method == "nelson") 
            stop("Nelson Method not available when censoring.side='left'")
        if (censoring.side == "right" & prob.method == "modified kaplan-meier") 
            stop("Modified Kaplan-Meier Method not available when censoring.side='right'")
        if (!is.vector(plot.pos.con, mode = "numeric") || length(plot.pos.con) != 
            1 || plot.pos.con < 0 || plot.pos.con > 1) 
            stop("'plot.pos.con' must be a numeric scalar between 0 and 1")
        censoring.type <- "MultiplyCensored"
    }
    else {
        censoring.type <- "SinglyCensored"
    }
    test.name <- paste(test, censoring.type, "GofTest", sep = "")
    if (!(distribution %in% c("norm", "lnorm", "lnormAlt"))) {
        efcn <- paste("e", distribution, sep = "")
        if (EnvStats::Distribution.df[distribution, "Type"] != 
            "Continuous" || !exists(efcn, where = "package:EnvStats")) 
            stop(paste("When the argument distribution is not equal to", 
                "'norm', 'lnorm', or 'lnormAlt',", "it must indicate a continuous distribution, and", 
                "there must exist an associated function", "to estimate the parameters in the presence of censored data.", 
                "See the help file for 'EnvStats::Distribution.df' for more information."))
        test.name <- paste(test, censoring.type, "GeneralGofTest", 
            sep = "")
    }
    arg.list <- list(x = x, censored = censored, censoring.side = censoring.side, 
        distribution = distribution, est.arg.list = est.arg.list)
    if (multiple) 
        arg.list <- c(arg.list, list(prob.method = prob.method, 
            plot.pos.con = plot.pos.con))
    ret.list <- do.call(test.name, args = arg.list)
    if (!keep.data) {
        ret.list <- ret.list[!(names(ret.list) %in% c("data", 
            "censored"))]
        oldClass(ret.list) <- "gofCensored"
    }
    ret.list$data.name <- data.name
    ret.list$censoring.name <- censoring.name
    if (any(bad.obs > 0)) 
        ret.list$bad.obs <- bad.obs
    else ret.list$bad.obs <- NULL
    ret.list
}
