#' Goodness-of-Fit Test
#' @aliases gofTest.default gofTest.formula
#' @description
#' Perform a goodness-of-fit test to determine whether a data set
#'   appears to come from a specified probability distribution or if two
#'   data sets appear to come from the same distribution.
#' @usage
#' gofTest(y, ...)
#'
#' \method{gofTest}{formula}(y, data = NULL, subset,
#'   na.action = na.pass, ...)
#'
#' \method{gofTest}{default}(y, x = NULL,
#'   test = ifelse(is.null(x), "sw", "ks"),
#'   distribution = "norm", est.arg.list = NULL,
#'   alternative = "two.sided", n.classes = NULL,
#'   cut.points = NULL, param.list = NULL,
#'   estimate.params = ifelse(is.null(param.list), TRUE, FALSE),
#'   n.param.est = NULL, correct = NULL, digits = .Options$digits,
#'   exact = NULL, ws.method = "normal scores", warn = TRUE, keep.data = TRUE,
#'   data.name = NULL, data.name.x = NULL, parent.of.data = NULL,
#'   subset.expression = NULL, ...)
#' @rawRd
#' \arguments{
#'   \item{y}{
#'   an object containing data for the goodness-of-fit test.  In the default
#'   method, the argument \code{y} must be numeric vector of observations.
#'   In the formula method, \code{y} must be a formula of the form \code{y ~ 1}
#'   or \code{y ~ x}.  The form \code{y ~ 1} indicates use the observations in
#'   the vector \code{y} for a one-sample goodness-of-fit test.  The form
#'   \code{y ~ x} is only relevant to the case of the two-sample
#'   Kolmogorov-Smirnov test (\code{test="ks"}) and indicates use the
#'   observations in the vector \code{y} as the second sample and use the
#'   observations in the vector \code{x} as the first sample.  Note that
#'   for the formula method, \code{x} and \code{y} must be the same length but
#'   this is not a requirement of the test and you can use vectors of different
#'   lengths via the default method.
#'   Missing (\code{NA}), undefined (\code{NaN}),
#'   and infinite (\code{Inf}, \code{-Inf}) values are allowed but will be
#'   removed.
#' }
#'   \item{data}{
#'   specifies an optional data frame, list or environment (or object coercible
#'   by \code{as.data.frame} to a data frame) containing the variables in the
#'   model.  If not found in \code{data}, the variables are taken from
#'   \code{environment(formula)}, typically the environment from which
#'   \code{gofTest} is called.
#' }
#'   \item{subset}{
#'   specifies an optional vector specifying a subset of observations to be used.
#' }
#'   \item{na.action}{
#'   specifies a function which indicates what should happen when the data contain \code{NA}s.
#'   The default is \code{\link{na.pass}}.
#' }
#'   \item{x}{
#'   numeric vector of values for the first sample in the case of a two-sample
#'   Kolmogorov-Smirnov goodness-of-fit test (\code{test="ks"}).
#'   Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf},
#'   \code{-Inf}) values are allowed but will be removed.
#' }
#'   \item{test}{
#'   character string defining which goodness-of-fit test to perform.  Possible values are:
#'   \itemize{
#'   \item \code{"sw"}. Shapiro-Wilk; the default when \code{x} is NOT supplied.
#'   \item \code{"sf"}. Shapiro-Francia.
#'   \item \code{"ppcc"}. Probability Plot Correlation Coefficient.
#'   \item \code{"ad"}.  Anderson-Darling.
#'   \item \code{"cmv"}. Cramer-von Mises.
#'   \item \code{"lillie"}. Lilliefor.
#'   \item \code{"skew"}. Zero-skew.
#'   \item \code{"chisq"}. Chi-squared.
#'   \item \code{"ks"}. Kolmogorov-Smirnov; the default when \code{x} IS supplied.
#'   \item \code{"ws"}. Wilk-Shapiro test for Uniform [0, 1] distribution.
#'   \item \code{"proucl.ad.gamma"}. Anderson-Darling test for a gamma distribution using
#'     ProUCL critical values.
#'   \item \code{"proucl.ks.gamma"}. Kolmogorov-Smirnov test for a gamma distribution using
#'     ProUCL critical values.
#'   }
#'
#'   When the argument \code{x} is supplied, you must set \code{test="ks"}, which is what \code{gofTest}
#'   does by default.
#' }
#'   \item{distribution}{
#'   a character string denoting the distribution abbreviation.  See the help file for
#'   \code{\link{Distribution.df}} for a list of distributions and their abbreviations.
#'   The default value is \code{distribution="norm"} (\link{Normal} distribution).
#'
#'   When \code{test="sw"}, \code{test="sf"}, or \code{test="ppcc"}, any continuous
#'   distribuiton is allowed (e.g., \code{"norm"} (normal), \code{"lnorm"} (lognormal),
#'   \code{"gamma"} (gamma), etc.), as well as mixed distributions involving the normal distribution
#'   (i.e., \code{"zmnorm"} (zero-modified normal), \code{"zmlnorm"} (zero-modified lognormal (delta)),
#'   and \cr
#'   \code{"zmlnormAlt"} (zero-modified lognormal with alternative parameterization)).
#'
#'   When \code{test="ad"}, \code{test="cvm"}, \code{test="lillie"}, or \code{test="skew"},
#'   only the values \code{"norm"} (normal), \code{"lnorm"} (lognormal),
#'   \code{"lnormAlt"} (lognormal with alternative parameterization),
#'   \code{"zmnorm"} (zero-modified normal), \code{"zmlnorm"} (zero-modified lognormal (delta)), and \cr
#'   \code{"zmlnormAlt"} (zero-modified lognormal with alternative parameterization) are allowed.
#'
#'   When \code{test="ks"}, any continuous distribution is allowed.
#'
#'   When \code{test="chisq"}, any distribuiton is allowed.
#'
#'   When \code{test="ws"}, this argument is ignored.
#'
#'   When \code{test="proucl.ad.gamma"} or \code{test="proucl.ks.gamma"}, you must set
#'   \code{distribution="gamma"} or \code{distribution="gammaAlt"}.
#' }
#'   \item{est.arg.list}{
#'   a list of arguments to be passed to the function estimating the distribution parameters.
#'   For example, if \code{test="sw"} and \code{distribution="gamma"}, setting \cr
#'   \code{est.arg.list=list(method="bcmle")} indicates using the bias-corrected \cr
#'   maximum-likelihood
#'   estimators of shape and scale (see the help file for \code{\link{egamma}}).
#'   See the help file
#'   \link{Estimating Distribution Parameters} for a list of estimating functions.
#'   The default value is \code{est.arg.list=NULL} so that all default values for the
#'   estimating function are used.  This argument is ignored if \cr
#'   \code{estimate.params=FALSE}.
#'
#'
#'   When \code{test="sw"}, \code{test="sf"}, \code{test="ppcc"},
#'   \code{test="ad"}, \code{test="cvm"}, \code{test="lillie"}, or \code{test="skew"},
#'   and you are testing for some form of normality (i.e., \link{Normal}, \link{Lognormal},
#'   \link[=Lognormal3]{Three-Parameter Lognormal},
#'   \link[=ZeroModifiedNormal]{Zero-Modified Normal}, or
#'   \link[=ZeroModifiedLognormal]{Zero-Modified Lognormal (Delta)}),
#'   the estimated parameters are provided in the
#'   output merely for information, and the choice of the method of estimation has no effect
#'   on the goodness-of-fit test statistic or p-value.
#'
#'
#'   When \code{test="ks"}, \code{x} is not supplied, and
#'   \code{estimate.params=TRUE}, the estimated parameters are used to
#'   specify the null hypothesis of which distribution
#'   the data are assumed to come from.
#'
#'
#'   When \code{test="chisq"} and \code{estimate.params=TRUE},
#'   the estimated parameters are used to specify the null hypothesis of which distribution
#'   the data are assumed to come from.
#'
#'   When \code{test="ws"}, \code{test="proucl.ad.gamma"}, or \code{test="proucl.ks.gamma"},
#'   this argument is ignored.
#' }
#'   \item{alternative}{
#'   for the case when \code{test="ks"}, \code{test="skew"}, or \code{test="ws"},
#'   character string specifying the alternative hypothesis.  When \code{test="ks"} or
#'   \code{test="skew"}, the possible values are \code{"two-sided"} (the default),
#'   \code{"greater"}, or \code{"less"}.  When \code{test="ws"}, the possible values are
#'   \code{"greater"} (the default), or \code{"less"}.  See the DETAILS section
#'   of the help file for \code{\link{ks.test}} for more explanation of the
#'   meaning of this argument.
#' }
#'   \item{n.classes}{
#'   for the case when \code{test="chisq"}, the number of cells into which the observations
#'   are to be allocated.  If the argument \code{cut.points} is supplied, then \code{n.classes}
#'   is set to \code{length(cut.points)-1}.  The default value is \cr
#'   \code{ceiling(2* (length(x)^(2/5)))} and is recommended by Moore (1986).
#' }
#'   \item{cut.points}{
#'   for the case when \code{test="chisq"}, a vector of cutpoints that defines the cells.
#'   The element \code{x[i]} is allocated to cell \code{j} if \cr
#'   \code{cut.points[j]} < \code{x[i]} \eqn{\le} \code{cut.points[j+1]}.
#'   If \code{x[i]} is less than or equal to the first cutpoint or
#'   greater than the last cutpoint, then \code{x[i]} is treated as missing.  If the
#'   hypothesized distribution is discrete, \code{cut.points} must be supplied.  The default
#'   value is \code{cut.points=NULL}, in which case the cutpoints are determined by
#'   \code{n.classes} equi-probable intervals.
#' }
#'   \item{param.list}{
#'   for the case when \code{test="ks"} and \code{x} is not supplied, or when
#'   \code{test="chisq"},
#'   a list with values for the parameters of the specified distribution.  See the help file
#'   for \code{\link{Distribution.df}} for the names and possible values of the parameters
#'   associated with each distribution.  The default value is \code{param.list=NULL}, which forces
#'   estimation of the distribution parameters.  This argument is ignored if
#'   \code{estimate.params=TRUE}.
#' }
#'   \item{estimate.params}{
#'   for the case when \code{test="ks"} and \code{x} is not supplied, or when
#'   \code{test="chisq"}, a logical scalar indicating whether to perform the goodness-of-fit test based on
#'   estimating the distribution parameters (\code{estimate.params=TRUE}) or using the
#'   user-supplied distribution parameters specified by \code{param.list} \cr
#'   (\code{estimate.params=FALSE}).  The default value of \code{estimate.params} is
#'   \code{TRUE} if \code{param.list=NULL}, otherwise it is \code{FALSE}.
#' }
#'   \item{n.param.est}{
#'   for the case when \code{test="ks"} and \code{x} is not supplied, or when
#'   \code{test="chisq"},
#'   an integer indicating the number of parameters estimated from the data.  \cr
#'   If \code{estimate.params=TRUE}, the default value is the number of parameters associated
#'   with the distribution specified by \code{distribution} (e.g., 2 for a normal distribution).
#'   If \code{estimate.params=FALSE}, the default value is \code{n.param.est=0}.
#' }
#'   \item{correct}{
#'   for the case when \code{test="chisq"}, a logical scalar indicating whether to use the
#'   continuity correction.  The default value is \code{correct=FALSE} unless \cr
#'   \code{n.classes=2}.
#' }
#'   \item{digits}{
#'   for the case when \code{test="ks"} and \code{x} is not supplied, or when
#'   \code{test="chisq"}, and \code{param.list} is supplied,
#'   a scalar indicating how many significant digits to print out for the parameters
#'   associated with the hypothesized distribution.  The default value is
#'   \code{.Options$digits}.
#' }
#'   \item{exact}{
#'   for the case when \code{test="ks"}, \code{exact=NULL} by default, but can be set to
#'   a logical scalar indicating whether an exact p-value should be computed.
#'   See the help file for \code{\link{ks.test}} for more information.
#' }
#'   \item{ws.method}{
#'   for the case when \code{test="ws"}, this argument specifies whether to perform the test
#'   based on normal scores (\code{ws.method="normal scores"}, the default) or
#'   chi-square scores (\code{ws.method="chi-square scores"}).  See the DETAILS section
#'   for more information.
#' }
#'   \item{warn}{
#'   logical scalar indicating whether to print a warning message when
#'   observations with \code{NA}s, \code{NaN}s, or \code{Inf}s in
#'   \code{y} or \code{x} are removed.  The default value is \code{warn=TRUE}.
#' }
#'   \item{keep.data}{
#'   logical scalar indicating whether to return the data used for the goodness-of-fit test.
#'   The default value is \code{keep.data=TRUE}.
#' }
#'   \item{data.name}{
#'   character string indicating the name of the data used for argument \code{y}.
#' }
#'   \item{data.name.x}{
#'   character string indicating the name of the data used for argument \code{x}.
#' }
#'   \item{parent.of.data}{
#'   character string indicating the source of the data used for the
#'   goodness-of-fit test.
#' }
#'   \item{subset.expression}{
#'   character string indicating the expression used to subset the data.
#' }
#'   \item{\dots}{
#'   additional arguments affecting the goodness-of-fit test.
#' }
#' }
#' @details
#' This help page has been shortened to keep function help focused on usage,
#' arguments, return values, and examples. Extended method details, formulas,
#' and background material are available in \code{vignette("extended-function-details", package = "EnvStats")}, section \code{gofTest}.
#' @rawRd
#' \value{
#'   a list of class \code{"gof"} containing the results of the goodness-of-fit test, unless
#'   the two-sample \cr
#'   Kolmogorov-Smirnov test is used, in which case the value is a list of
#'   class \code{"gofTwoSample"}.  Objects of class \code{"gof"} and \code{"gofTwoSample"}
#'   have special printing and plotting methods.  See the help files for \code{\link{gof.object}}
#'   and \code{\link{gofTwoSample.object}} for details.
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
#'   Kim, P.J., and R.I. Jennrich. (1973).
#'   Tables of the Exact Sampling Distribution of the Two Sample Kolmogorov-Smirnov Criterion.
#'   In Harter, H.L., and D.B. Owen, eds. \emph{Selected Tables in Mathematical Statistics, Vol. 1}.
#'   American Mathematical Society, Providence, Rhode Island, pp.79-170.
#'
#'   Kolmogorov, A.N. (1933). Sulla determinazione empirica di una legge di distribuzione.
#'   \emph{Giornale dell' Istituto Italiano degle Attuari} \bold{4}, 83-91.
#'
#'   Marsaglia, G., W.W. Tsang, and J. Wang. (2003). Evaluating Kolmogorov's distribution.
#'   \emph{Journal of Statistical Software}, \bold{8}(18).
#'   \doi{10.18637/jss.v008.i18}.
#'
#'   Moore, D.S. (1986). Tests of Chi-Squared Type. In D'Agostino, R.B., and M.A. Stephens, eds.
#'   \emph{Goodness-of Fit Techniques}. Marcel Dekker, New York, pp.63-95.
#'
#'   Pomeranz, J. (1973).
#'   Exact Cumulative Distribution of the Kolmogorov-Smirnov Statistic for Small Samples (Algorithm 487).
#'   \emph{Collected Algorithms from ACM} ??, ???-???.
#'
#'   Razali, N.M., and Y.B. Wah. (2011). Power Comparisons of Shapiro-Wilk, Kolmogorov-Smirnov,
#'   Lilliefors, and Anderson-Darling Tests.  \emph{Journal of Statistical Modeling and Analytics}
#'   \bold{2}(1), 21--33.
#'
#'   Romao, X., Delgado, R., and A. Costa. (2010). An Empirical Power Comparison of Univariate
#'   Goodness-of-Fit Tests for Normality.  \emph{Journal of Statistical Computation and Simulation}
#'   \bold{80}(5), 545--591.
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
#'   Smirnov, N.V. (1939).
#'   Estimate of Deviation Between Empirical Distribution Functions in Two Independent Samples.
#'   \emph{Bulletin Moscow University} \bold{2}(2), 3-16.
#'
#'   Smirnov, N.V. (1948). Table for Estimating the Goodness of Fit of Empirical Distributions.
#'   \emph{Annals of Mathematical Statistics} \bold{19}, 279-281.
#'
#'   Stephens, M.A. (1970).
#'   Use of the Kolmogorov-Smirnov, Cramer-von Mises and Related Statistics Without Extensive Tables.
#'   \emph{Journal of the Royal Statistical Society, Series B}, \bold{32}, 115-122.
#'
#'   Stephens, M.A. (1974). EDF Statistics for Goodness of Fit and Some Comparisons.
#'   \emph{Journal of the American Statistical Association} \bold{69}, 730-737.
#'
#'   Stephens, M.A. (1986a). Tests Based on EDF Statistics. In D'Agostino, R. B., and M.A. Stevens, eds.
#'   \emph{Goodness-of-Fit Techniques}. Marcel Dekker, New York.
#'
#'   Thode Jr., H.C. (2002). \emph{Testing for Normality}. Marcel Dekker, New York.
#'
#'   USEPA. (2015).  \emph{ProUCL Version 5.1.002 Technical Guide}.  EPA/600/R-07/041, October 2015.
#'   Office of Research and Development. U.S. Environmental Protection Agency, Washington, D.C.
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
#'
#'   Wilk, M.B., and S.S. Shapiro. (1968). The Joint Assessment of Normality of Several Independent
#'   Samples. \emph{Technometrics}, \bold{10}(4), 825-839.
#'
#'   Zar, J.H. (2010). \emph{Biostatistical Analysis}. Fifth Edition.
#'   Prentice-Hall, Upper Saddle River, NJ.
#' }
#' @rawRd
#' \author{
#'   Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#'
#'   Juergen Gross and Uwe Ligges for the Anderson-Darling, Carmer-von Mises, and Lilliefors tests called
#'   from the package \pkg{nortest}.
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
#'   In general, the Shapiro-Wilk and Shapiro-Francia tests outperform the Anderson-Darling
#'   test, which in turn outperforms the Cramer-von Mises test, which in turn
#'   outperforms the Lilliefors test (Stephens, 1986a; Razali and Wah, 2011; Romao et al., 2010).
#'
#'   The zero-skew goodness-of-fit test for normality is one of several tests that have
#'   been proposed to test the assumption of a normal distribution (D'Agostino, 1986b).
#'   This test has been included mainly because it is called by \code{\link{elnorm3}}.
#'   Ususally, the Shapiro-Wilk or Shapiro-Francia test is preferred to this test, unless
#'   the direction of the alternative to normality (e.g., positive skew) is known
#'   (D'Agostino, 1986b, pp. 405--406).
#'
#'   Kolmogorov (1933) introduced a goodness-of-fit test to test the hypothesis that a
#'   random sample of \eqn{n} observations \bold{x} comes from a specific hypothesized distribution
#'   with cumulative distribution function \eqn{H}.  This test is now usually called the
#'   one-sample Kolmogorov-Smirnov goodness-of-fit test.  Smirnov (1939) introduced a
#'   goodness-of-fit test to test the hypothesis that a random sample of \eqn{n}
#'   observations \bold{x} comes from the same distribution as a random sample of
#'   \eqn{m} observations \bold{y}.  This test is now usually called the two-sample
#'   Kolmogorov-Smirnov goodness-of-fit test.  Both tests are based on the maximum
#'   vertical distance between two cumulative distribution functions.  For the one-sample problem
#'   with a small sample size, the Kolmogorov-Smirnov test may be preferred over the chi-squared
#'   goodness-of-fit test since the KS-test is exact, while the chi-squared test is based on
#'   an asymptotic approximation.
#'
#'   The chi-squared test, introduced by Pearson in 1900, is the oldest and best known
#'   goodness-of-fit test.  The idea is to reduce the goodness-of-fit problem to a
#'   multinomial setting by comparing the observed cell counts with their expected values
#'   under the null hypothesis.  Grouping the data sacrifices information, especially if the
#'   hypothesized distribution is continuous.  On the other hand, chi-squared tests can be be
#'   applied to any type of variable: continuous, discrete, or a combination of these.
#'
#'   The Wilk-Shapiro (1968) tests for a Uniform [0, 1] distribution were introduced in the context
#'   of testing whether several independent samples all come from normal distributions, with
#'   possibly different means and variances.  The function \code{\link{gofGroupTest}} extends
#'   this idea to allow you to test whether several independent samples come from the same
#'   distribution (e.g., gamma, extreme value, etc.), with possibly different parameters.
#'
#'   In practice, almost any goodness-of-fit test will \emph{not} reject the null hypothesis
#'   if the number of observations is relatively small.  Conversely, almost any goodness-of-fit
#'   test \emph{will} reject the null hypothesis if the number of observations is very large,
#'   since \dQuote{real} data are never distributed according to any theoretical distribution
#'   (Conover, 1980, p.367).  For most cases, however, the distribution of \dQuote{real} data
#'   is close enough to some theoretical distribution that fairly accurate results may be
#'   provided by assuming that particular theoretical distribution.  One way to asses the
#'   goodness of the fit is to use goodness-of-fit tests.  Another way is to look at
#'   quantile-quantile (Q-Q) plots (see \code{\link{qqPlot}}).
#' }
#' @rawRd
#' \seealso{
#'   \code{\link{rosnerTest}}, \code{\link{gof.object}}, \code{\link{print.gof}},
#'   \code{\link{plot.gof}},
#'   \code{\link{shapiro.test}}, \code{\link{ks.test}}, \code{\link{chisq.test}},
#'   \link{Normal}, \link{Lognormal}, \link{Lognormal3},
#'   \link{Zero-Modified Normal}, \link{Zero-Modified Lognormal (Delta)},
#'   \code{\link{enorm}}, \code{\link{elnorm}}, \code{\link{elnormAlt}},
#'   \code{\link{elnorm3}}, \code{\link{ezmnorm}}, \code{\link{ezmlnorm}},
#'   \code{\link{ezmlnormAlt}}, \code{\link{qqPlot}}.
#' }
#' @rawRd
#' \examples{
#'   # Generate 20 observations from a gamma distribution with
#'   # parameters shape = 2 and scale = 3 then run various
#'   # goodness-of-fit tests.
#'   # (Note:  the call to set.seed lets you reproduce this example.)
#'
#'   set.seed(47)
#'   dat <- rgamma(20, shape = 2, scale = 3)
#'
#'   # Shapiro-Wilk generalized goodness-of-fit test
#'   #----------------------------------------------
#'   gof.list <- gofTest(dat, distribution = "gamma")
#'   gof.list
#'
#'   #Results of Goodness-of-Fit Test
#'   #-------------------------------
#'   #
#'   #Test Method:                     Shapiro-Wilk GOF Based on
#'   #                                 Chen & Balakrisnan (1995)
#'   #
#'   #Hypothesized Distribution:       Gamma
#'   #
#'   #Estimated Parameter(s):          shape = 1.909462
#'   #                                 scale = 4.056819
#'   #
#'   #Estimation Method:               mle
#'   #
#'   #Data:                            dat
#'   #
#'   #Sample Size:                     20
#'   #
#'   #Test Statistic:                  W = 0.9834958
#'   #
#'   #Test Statistic Parameter:        n = 20
#'   #
#'   #P-value:                         0.970903
#'   #
#'   #Alternative Hypothesis:          True cdf does not equal the
#'   #                                 Gamma Distribution.
#'
#'   dev.new()
#'   plot(gof.list)
#'
#'   #----------
#'
#'   # Redo the example above, but use the bias-corrected mle
#'
#'   gofTest(dat, distribution = "gamma",
#'     est.arg.list = list(method = "bcmle"))
#'
#'   #Results of Goodness-of-Fit Test
#'   #-------------------------------
#'   #
#'   #Test Method:                     Shapiro-Wilk GOF Based on
#'   #                                 Chen & Balakrisnan (1995)
#'   #
#'   #Hypothesized Distribution:       Gamma
#'   #
#'   #Estimated Parameter(s):          shape = 1.656376
#'   #                                 scale = 4.676680
#'   #
#'   #Estimation Method:               bcmle
#'   #
#'   #Data:                            dat
#'   #
#'   #Sample Size:                     20
#'   #
#'   #Test Statistic:                  W = 0.9834346
#'   #
#'   #Test Statistic Parameter:        n = 20
#'   #
#'   #P-value:                         0.9704046
#'   #
#'   #Alternative Hypothesis:          True cdf does not equal the
#'   #                                 Gamma Distribution.
#'
#'   #----------
#'
#'   # Komogorov-Smirnov goodness-of-fit test (pre-specified parameters)
#'   #------------------------------------------------------------------
#'
#'   gofTest(dat, test = "ks", distribution = "gamma",
#'     param.list = list(shape = 2, scale = 3))
#'
#'   #Results of Goodness-of-Fit Test
#'   #-------------------------------
#'   #
#'   #Test Method:                     Kolmogorov-Smirnov GOF
#'   #
#'   #Hypothesized Distribution:       Gamma(shape = 2, scale = 3)
#'   #
#'   #Data:                            dat
#'   #
#'   #Sample Size:                     20
#'   #
#'   #Test Statistic:                  ks = 0.2313878
#'   #
#'   #Test Statistic Parameter:        n = 20
#'   #
#'   #P-value:                         0.2005083
#'   #
#'   #Alternative Hypothesis:          True cdf does not equal the
#'   #                                 Gamma(shape = 2, scale = 3)
#'   #                                 Distribution.
#'
#'   #----------
#'
#'   # ProUCL Version of Komogorov-Smirnov goodness-of-fit test
#'   # for a Gamma Distribution (estimated parameters)
#'   #---------------------------------------------------------
#'
#'   gofTest(dat, test = "proucl.ks.gamma", distribution = "gamma")
#'
#'   #Results of Goodness-of-Fit Test
#'   #-------------------------------
#'   #
#'   #Test Method:                     ProUCL Kolmogorov-Smirnov Gamma GOF
#'   #
#'   #Hypothesized Distribution:       Gamma
#'   #
#'   #Estimated Parameter(s):          shape = 1.909462
#'   #                                 scale = 4.056819
#'   #
#'   #Estimation Method:               MLE
#'   #
#'   #Data:                            dat
#'   #
#'   #Sample Size:                     20
#'   #
#'   #Test Statistic:                  D = 0.0988692
#'   #
#'   #Test Statistic Parameter:        n = 20
#'   #
#'   #Critical Values:                 D.0.01 = 0.228
#'   #                                 D.0.05 = 0.196
#'   #                                 D.0.10 = 0.180
#'   #
#'   #P-value:                         >= 0.10
#'   #
#'   #Alternative Hypothesis:          True cdf does not equal the
#'   #                                 Gamma Distribution.
#'
#'   #----------
#'
#'   # Chi-squared goodness-of-fit test (estimated parameters)
#'   #--------------------------------------------------------
#'
#'   gofTest(dat, test = "chisq", distribution = "gamma", n.classes = 4)
#'
#'   #Results of Goodness-of-Fit Test
#'   #-------------------------------
#'   #
#'   #Test Method:                     Chi-square GOF
#'   #
#'   #Hypothesized Distribution:       Gamma
#'   #
#'   #Estimated Parameter(s):          shape = 1.909462
#'   #                                 scale = 4.056819
#'   #
#'   #Estimation Method:               mle
#'   #
#'   #Data:                            dat
#'   #
#'   #Sample Size:                     20
#'   #
#'   #Test Statistic:                  Chi-square = 1.2
#'   #
#'   #Test Statistic Parameter:        df = 1
#'   #
#'   #P-value:                         0.2733217
#'   #
#'   #Alternative Hypothesis:          True cdf does not equal the
#'   #                                 Gamma Distribution.
#'
#'   #----------
#'   # Clean up
#'
#'   rm(dat, gof.list)
#'   graphics.off()
#'
#'   #--------------------------------------------------------------------
#'
#'   # Example 10-2 of USEPA (2009, page 10-14) gives an example of
#'   # using the Shapiro-Wilk test to test the assumption of normality
#'   # for nickel concentrations (ppb) in groundwater collected over
#'   # 4 years.  The data for this example are stored in
#'   # EPA.09.Ex.10.1.nickel.df.
#'
#'   EPA.09.Ex.10.1.nickel.df
#'   #   Month   Well Nickel.ppb
#'   #1      1 Well.1       58.8
#'   #2      3 Well.1        1.0
#'   #3      6 Well.1      262.0
#'   #4      8 Well.1       56.0
#'   #5     10 Well.1        8.7
#'   #6      1 Well.2       19.0
#'   #7      3 Well.2       81.5
#'   #8      6 Well.2      331.0
#'   #9      8 Well.2       14.0
#'   #10    10 Well.2       64.4
#'   #11     1 Well.3       39.0
#'   #12     3 Well.3      151.0
#'   #13     6 Well.3       27.0
#'   #14     8 Well.3       21.4
#'   #15    10 Well.3      578.0
#'   #16     1 Well.4        3.1
#'   #17     3 Well.4      942.0
#'   #18     6 Well.4       85.6
#'   #19     8 Well.4       10.0
#'   #20    10 Well.4      637.0
#'
#'   # Test for a normal distribution:
#'   #--------------------------------
#'
#'   gof.list <- gofTest(Nickel.ppb ~ 1, data = EPA.09.Ex.10.1.nickel.df)
#'   gof.list
#'
#'   #Results of Goodness-of-Fit Test
#'   #-------------------------------
#'   #
#'   #Test Method:                     Shapiro-Wilk GOF
#'   #
#'   #Hypothesized Distribution:       Normal
#'   #
#'   #Estimated Parameter(s):          mean = 169.5250
#'   #                                 sd   = 259.7175
#'   #
#'   #Estimation Method:               mvue
#'   #
#'   #Data:                            Nickel.ppb
#'   #
#'   #Data Source:                     EPA.09.Ex.10.1.nickel.df
#'   #
#'   #Sample Size:                     20
#'   #
#'   #Test Statistic:                  W = 0.6788888
#'   #
#'   #Test Statistic Parameter:        n = 20
#'   #
#'   #P-value:                         2.17927e-05
#'   #
#'   #Alternative Hypothesis:          True cdf does not equal the
#'   #                                 Normal Distribution.
#'
#'   dev.new()
#'   plot(gof.list)
#'
#'   #----------
#'
#'   # Test for a lognormal distribution:
#'   #-----------------------------------
#'
#'   gofTest(Nickel.ppb ~ 1, data = EPA.09.Ex.10.1.nickel.df,
#'     dist = "lnorm")
#'
#'   #Results of Goodness-of-Fit Test
#'   #-------------------------------
#'   #
#'   #Test Method:                     Shapiro-Wilk GOF
#'   #
#'   #Hypothesized Distribution:       Lognormal
#'   #
#'   #Estimated Parameter(s):          meanlog = 3.918529
#'   #                                 sdlog   = 1.801404
#'   #
#'   #Estimation Method:               mvue
#'   #
#'   #Data:                            Nickel.ppb
#'   #
#'   #Data Source:                     EPA.09.Ex.10.1.nickel.df
#'   #
#'   #Sample Size:                     20
#'   #
#'   #Test Statistic:                  W = 0.978946
#'   #
#'   #Test Statistic Parameter:        n = 20
#'   #
#'   #P-value:                         0.9197735
#'   #
#'   #Alternative Hypothesis:          True cdf does not equal the
#'   #                                 Lognormal Distribution.
#'
#'   #----------
#'
#'   # Test for a lognormal distribution, but use the
#'   # Mean and CV parameterization:
#'   #-----------------------------------------------
#'
#'   gofTest(Nickel.ppb ~ 1, data = EPA.09.Ex.10.1.nickel.df,
#'     dist = "lnormAlt")
#'
#'   #Results of Goodness-of-Fit Test
#'   #-------------------------------
#'   #
#'   #Test Method:                     Shapiro-Wilk GOF
#'   #
#'   #Hypothesized Distribution:       Lognormal
#'   #
#'   #Estimated Parameter(s):          mean = 213.415628
#'   #                                 cv   =   2.809377
#'   #
#'   #Estimation Method:               mvue
#'   #
#'   #Data:                            Nickel.ppb
#'   #
#'   #Data Source:                     EPA.09.Ex.10.1.nickel.df
#'   #
#'   #Sample Size:                     20
#'   #
#'   #Test Statistic:                  W = 0.978946
#'   #
#'   #Test Statistic Parameter:        n = 20
#'   #
#'   #P-value:                         0.9197735
#'   #
#'   #Alternative Hypothesis:          True cdf does not equal the
#'   #                                 Lognormal Distribution.
#'
#'   #----------
#'   # Clean up
#'
#'   rm(gof.list)
#'   graphics.off()
#'
#'   #---------------------------------------------------------------------------
#'
#'   # Generate 20 observations from a normal distribution with mean=3 and sd=2, and
#'   # generate 10 observaions from a normal distribution with mean=2 and sd=2 then
#'   # test whether these sets of observations come from the same distribution.
#'   # (Note: the call to set.seed simply allows you to reproduce this example.)
#'
#'   set.seed(300)
#'   dat1 <- rnorm(20, mean = 3, sd = 2)
#'   dat2 <- rnorm(10, mean = 1, sd = 2)
#'   gofTest(x = dat1, y = dat2, test = "ks")
#'
#'   #Results of Goodness-of-Fit Test
#'   #-------------------------------
#'   #
#'   #Test Method:                     2-Sample K-S GOF
#'   #
#'   #Hypothesized Distribution:       Equal
#'   #
#'   #Data:                            x = dat1
#'   #                                 y = dat2
#'   #
#'   #Sample Sizes:                    n.x = 20
#'   #                                 n.y = 10
#'   #
#'   #Test Statistic:                  ks = 0.7
#'   #
#'   #Test Statistic Parameters:       n = 20
#'   #                                 m = 10
#'   #
#'   #P-value:                         0.001669561
#'   #
#'   #Alternative Hypothesis:          The cdf of 'dat1' does not equal
#'   #                                 the cdf of 'dat2'.
#'
#'   #----------
#'   # Clean up
#'
#'   rm(dat1, dat2)
#' }
#' @rawRd
#' \keyword{ htest }
#' @rawRd
#' \keyword{ models }

gofTest <-
function (y, ...) 
UseMethod("gofTest")
