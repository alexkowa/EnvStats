\name{kendallTrendTest}
\alias{kendallTrendTest}
\alias{kendallTrendTest.formula}
\alias{kendallTrendTest.default}
\title{
  Kendall's Nonparametric Test for Montonic Trend
}
\description{
  Perform a nonparametric test for a monotonic trend based on Kendall's
  tau statistic, and optionally compute a confidence interval for the
  slope.
}
\usage{
kendallTrendTest(y, ...)

\method{kendallTrendTest}{formula}(y, data = NULL, subset,
  na.action = na.pass, ...)

\method{kendallTrendTest}{default}(y, x = seq(along = y),
  alternative = "two.sided", correct = TRUE, ci.slope = TRUE,
  conf.level = 0.95, warn = TRUE, data.name = NULL, data.name.x = NULL,
  parent.of.data = NULL, subset.expression = NULL, ...)
}
\arguments{
  \item{y}{
  an object containing data for the trend test.  In the default method,
  the argument \code{y} must be numeric vector of observations.
  In the formula method, \code{y} must be a formula of the form \code{y ~ 1} or
  \code{y ~ x}.  The form \code{y ~ 1} indicates use the observations in the vector
  \code{y} for the test for trend and use the default value of the argument \code{x}
  in the call to \code{kendallTrendTest.default}.  The form \code{y ~ x} indicates
  use the observations in the vector \code{y} for the test for trend and use the
  specified value of the argument \code{x} in the call to
  \code{kendallTrendTest.default}.  Missing (\code{NA}), undefined (\code{NaN}),
  and infinite (\code{Inf}, \code{-Inf}) values are allowed but will be
  removed.
}
  \item{data}{
  specifies an optional data frame, list or environment (or object coercible by
  \code{as.data.frame} to a data frame) containing the variables in the model.
  If not found in \code{data}, the variables are taken from \code{environment(formula)},
  typically the environment from which \code{kendallTrendTest} is called.
}
  \item{subset}{
  specifies an optional vector specifying a subset of observations to be used.
}
  \item{na.action}{
  specifies a function which indicates what should happen when the data contain \code{NA}s.
  The default is \code{\link{na.pass}}.
}
  \item{x}{
  numeric vector of "predictor" values.  The length of \code{x} must equal the length of \code{y}.
  Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) values are
  allowed but will be removed.  The default value of \code{x} is the vector
  of numbers \eqn{1, 2, \dots, n} where \eqn{n} is the number of elements in
  \code{y}.
}
  \item{alternative}{
  character string indicating the kind of alternative hypothesis.  The
  possible values are \code{"two.sided"} (tau not equal to 0; the default),
  \code{"less"} (tau less than 0), and \code{"greater"} (tau greater than 0).
}
  \item{correct}{
  logical scalar indicating whether to use the correction for continuity in
  computing the \eqn{z}-statistic that is based on the test statistic \eqn{S}.
  The default value is \code{TRUE}.
}
  \item{ci.slope}{
  logical scalar indicating whether to compute a confidence interval for the
  slope.  The default value is \code{TRUE}.
}
  \item{conf.level}{
  numeric scalar between 0 and 1 indicating the confidence level associated
  with the confidence interval for the slope.  The default value is
  \code{0.95}.
}
  \item{warn}{
  logical scalar indicating whether to print a warning message when
  \code{y} does not contain at least two non-missing values,
  or when \code{x} does not contain at least two unique non-missing values.
  The default value is \code{TRUE}.
}
  \item{data.name}{
  character string indicating the name of the data used for the trend test.
  The default value is \code{deparse(substitute(y))}.
}
  \item{data.name.x}{
  character string indicating the name of the data used for the predictor variable x.
  If \code{x} is not supplied this argument is ignored.  When \code{x} is supplied,
  the default value is \code{deparse(substitute(x))}.
}
  \item{parent.of.data}{
  character string indicating the source of the data used for the trend test.
}
  \item{subset.expression}{
  character string indicating the expression used to subset the data.
}
  \item{\dots}{
  additional arguments affecting the test for trend.
}
}
\details{
  \code{kendallTrendTest} performs Kendall's nonparametric test for a monotonic trend,
  which is a special case of the test for independence based on Kendall's tau statistic
  (see \code{\link{cor.test}}).  The slope is estimated using the method of Theil (1950) and
  Sen (1968).  When \code{ci.slope=TRUE}, the confidence interval for the slope is
  computed using Gilbert's (1987) Modification of the Theil/Sen Method.

  Kendall's test for a monotonic trend is a special case of the test for independence
  based on Kendall's tau statistic.  The first section below explains the general case
  of testing for independence.  The second section explains the special case of
  testing for monotonic trend.  The last section explains how a simple linear
  regression model is a special case of a monotonic trend and how the slope may be
  estimated.
  \cr

  \bold{The General Case of Testing for Independence} \cr

  \emph{Definition of Kendall's Tau} \cr
  Let \eqn{X} and \eqn{Y} denote two continuous random variables with some joint
  (bivariate) distribution.  Let \eqn{(X_1, Y_1), (X_2, Y_2), \ldots, (X_n, Y_n)}
  denote a set of \eqn{n} bivariate observations from this distribution, and assume
  these bivariate observations are mutually independent.  Kendall (1938, 1975) proposed
  a test for the hypothesis that the \eqn{X} and \eqn{Y} random variables are
  independent based on estimating the following quantity:
  \deqn{\tau = \{ 2 Pr[(X_2 - X_1)(Y_2 - Y_1) > 0] \} - 1 \;\;\;\;\;\; (1)}
  The quantity in Equation (1) is called Kendall's tau, although this term is more
  often applied to the estimate of \eqn{\tau} (see Equation (2) below).
  If \eqn{X} and \eqn{Y} are independent, then \eqn{\tau=0}.  Furthermore, for most
  distributions of interest, if \eqn{\tau=0} then the random variables \eqn{X} and
  \eqn{Y} are independent.  (It can be shown that there exist some distributions for
  which \eqn{\tau=0} and the random variables \eqn{X} and \eqn{Y} are not independent;
  see Hollander and Wolfe (1999, p.364)).

  Note that Kendall's tau is similar to a correlation coefficient in that
  \eqn{-1 \le \tau \le 1}.  If \eqn{X} and \eqn{Y} always vary in the same direction,
  that is if \eqn{X_1 < X_2} always implies \eqn{Y_1 < Y_2}, then \eqn{\tau = 1}.
  If \eqn{X} and \eqn{Y} always vary in the opposite direction, that is if
  \eqn{X_1 < X_2} always implies \eqn{Y_1 > Y_2}, then \eqn{\tau = -1}.  If
  \eqn{\tau > 0}, this indicates \eqn{X} and \eqn{Y} are positively associated.
  If \eqn{\tau < 0}, this indicates \eqn{X} and \eqn{Y} are negatively associated.
  \cr

  \emph{Estimating Kendall's Tau} \cr
  The quantity in Equation (1) can be estimated by:
  \deqn{\hat{\tau} = \frac{2S}{n(n-1)} \;\;\;\;\;\; (2)}
  where
  \deqn{S = \sum_{i=1}^{n-1} \sum_{j=i+1}^{n} sign[(X_j - X_i)(Y_j - Y_i)] \;\;\;\;\;\; (3)}
  and \eqn{sign()} denotes the \code{\link{sign}} function:
  \tabular{lrl}{
                    \tab \eqn{-1} \tab \eqn{x < 0} \cr
    \eqn{sign(x) =} \tab \eqn{0}  \tab \eqn{x = 0 \;\;\;\;\;\; (4)} \cr
                    \tab \eqn{1}  \tab \eqn{x > 0}
  }
  (Hollander and Wolfe, 1999, Chapter 8; Conover, 1980, pp.256--260;
  Gilbert, 1987, Chapter 16; Helsel and Hirsch, 1992, pp.212--216;
  Gibbons et al., 2009, Chapter 11). The quantity defined in Equation (2) is called
  Kendall's rank correlation coefficient or more often Kendall's tau.

  Note that the quantity \eqn{S} defined in Equation (3) is equal to the number of
  concordant pairs minus the number of discordant pairs.  Hollander and Wolfe
  (1999, p.364) use the notation \eqn{K} instead of \eqn{S}, and Conover (1980, p.257)
  uses the notation \eqn{T}.
  \cr

  \emph{Testing the Null Hypothesis of Independence} \cr
  The null hypothesis \eqn{H_0: \tau = 0}, can be tested using the statistic \eqn{S}
  defined in Equation (3) above.  Tables of the distribution of \eqn{S} for small
  samples are given in Hollander and Wolfe (1999), Conover (1980, pp.458--459),
  Gilbert (1987, p.272), Helsel and Hirsch (1992, p.469), and Gibbons (2009, p.210).
  The function \code{kendallTrendTest} uses the large sample approximation to the
  distribution of \eqn{S} under the null hypothesis, which is given by:
  \deqn{z = \frac{S - E(S)}{\sqrt{Var(S)}} \;\;\;\;\;\; (5)}
  where
  \deqn{E(S) = 0 \;\;\;\;\;\; (6)}
  \deqn{Var(S) = \frac{n(n-1)(2n+5)}{18} \;\;\;\;\;\; (7)}
  Under the null hypothesis, the quantity \eqn{z} defined in Equation (5) is
  approximately distributed as a standard normal random variable.

  Both Kendall (1975) and Mann (1945) show that the normal approximation is excellent
  even for samples as small as \eqn{n=10}, provided that the following continuity
  correction is used:
  \deqn{z = \frac{S - sign(S)}{\sqrt{Var(S)}} \;\;\;\;\;\; (8)}
  The function \code{kendallTrendTest} performs the usual one-sample z-test using
  the statistic computed in Equation (8) or Equation (5).  The argument
  \code{correct} determines which equation is used to compute the z-statistic.
  By default, \code{correct=TRUE} so Equation (8) is used.

  In the case of tied observations in either the observed \eqn{X}'s and/or observed
  \eqn{Y}'s, the formula for the variance of \eqn{S} given in Equation (7) must be
  modified as follows:
  \tabular{ll}{
  \eqn{Var(S) =} \tab \eqn{\frac{n(n-1)(2n+5)}{18} -} \cr
                 \tab \cr
                 \tab \eqn{\frac{\sum_{i=1}^{g} t_i(t_i-1)(2t_i+5)}{18} - } \cr
                 \tab \cr
                 \tab \eqn{\frac{\sum_{j=1}^{h} u_j(u_j-1)(2u_j+5)}{18} + } \cr
                 \tab \cr
                 \tab \eqn{\frac{[\sum_{i=1}^{g} t_i(t_i-1)(t_i-2)][\sum_{j=1}^{h} u_j(u_j-1)(u_j-2)]}{9n(n-1)(n-2)} +} \cr
                 \tab \cr
                 \tab \eqn{\frac{[\sum_{i=1}^{g} t_i(t_i-1)][\sum_{j=1}^{h} u_j(u_j-1)]}{2n(n-1)} \;\;\;\;\;\; (9)}
  }
  where \eqn{g} is the number of tied groups in the \eqn{X} observations,
  \eqn{t_i} is the size of the \eqn{i}'th tied group in the \eqn{X} observations,
  \eqn{h} is the number of tied groups in the \eqn{Y} observations, and
  \eqn{u_j} is the size of the \eqn{j}'th tied group in the \eqn{Y} observations.
  In the case of no ties in either the \eqn{X} or \eqn{Y} observations, Equation (9)
  reduces to Equation (7).
  \cr

  \bold{The Special Case of Testing for Monotonic Trend} \cr
  Often in environmental sampling, observations are taken periodically over time
  (Hirsch et al., 1982; van Belle and Hughes, 1984; Hirsch and Slack, 1984).  In
  this case, the random variables \eqn{Y_1, Y_2, \ldots, Y_n} can be thought of as
  representing the observations, and the variables \eqn{X_1, X_2, \ldots, X_n}
  are no longer random but represent the time at which the \eqn{i}'th observation
  was taken.  If the observations are equally spaced over time, then it is useful to
  make the simplification \eqn{X_i = i} for \eqn{i = 1, 2, \ldots, n}.  This is in
  fact the default value of the argument \code{x} for the function
  \code{kendallTrendTest}.

  In the case where the \eqn{X}'s represent time and are all distinct, the test for
  independence between \eqn{X} and \eqn{Y} is equivalent to testing for a monotonic
  trend in \eqn{Y}, and the test statistic \eqn{S} simplifies to:
  \deqn{S = \sum_{i=1}^{n-1} \sum_{j=i+1}^{n} sign(Y_j - Y_i) \;\;\;\;\;\; (10)}
  Also, the formula for the variance of \eqn{S} in the presence of ties (under the
  null hypothesis \eqn{H_0: \tau = 0}) simplifies to:
  \deqn{Var(S) = \frac{n(n-1)(2n+5)}{18} - \frac{\sum_{j=1}^{h} u_j(u_j-1)(2u_j+5)}{18} \;\;\;\;\;\; (11)}
  A form of the test statistic \eqn{S} in Equation (10) was introduced by Mann (1945).
  \cr

  \bold{The Special Case of a Simple Linear Model:  Estimating the Slope} \cr
  Consider the simple linear regression model
  \deqn{Y_i = \beta_0 + \beta_1 X_i + \epsilon_i \;\;\;\;\;\; (12)}
  where \eqn{\beta_0} denotes the intercept, \eqn{\beta_1} denotes the slope,
  \eqn{i = 1, 2, \ldots, n}, and the \eqn{\epsilon}'s are assumed to be
  independent and identically distributed random variables from the same distribution.
  This is a special case of dependence between the \eqn{X}'s and the \eqn{Y}'s, and
  the null hypothesis of a zero slope can be tested using Kendall's test statistic
  \eqn{S} (Equation (3) or (10) above) and the associated z-statistic
  (Equation (5) or (8) above) (Hollander and Wolfe, 1999, pp.415--420).

  Theil (1950) proposed the following nonparametric estimator of the slope:
  \deqn{\hat{\beta}_1 = Median(\frac{Y_j - Y_i}{X_j - X_i}); \;\; i < j \;\;\;\;\;\; (13)}
  Note that the computation of the estimated slope involves computing
  \deqn{N = {n \choose 2} = \frac{n(n-1)}{2} \;\;\;\;\;\; (14)}
  \dQuote{two-point} estimated slopes (assuming no tied \eqn{X} values), and taking
  the median of these N values.

  Sen (1968) generalized this estimator to the case where there are possibly tied
  observations in the \eqn{X}'s.  In this case, Sen simply ignores the two-point
  estimated slopes where the \eqn{X}'s are tied and computes the median based on the
  remaining \eqn{N'} two-point estimated slopes.  That is, Sen's estimator is given by:
  \deqn{\hat{\beta}_1 = Median(\frac{Y_j - Y_i}{X_j - X_i}); \;\; i < j, X_i \ne X_j  \;\;\;\;\;\; (15)}
  (Hollander and Wolfe, 1999, pp.421--422).

  Conover (1980, p. 267) suggests the following estimator for the intercept:
  \deqn{\hat{\beta}_0 = Y_{0.5} - \hat{\beta}_1 X_{0.5} \;\;\;\;\;\; (16)}
  where \eqn{X_{0.5}} and \eqn{Y_{0.5}} denote the sample medians of the \eqn{X}'s
  and \eqn{Y}'s, respectively.  With these estimators of slope and intercept, the
  estimated regression line passes through the point \eqn{(X_{0.5}, Y_{0.5})}.

  \bold{NOTE:} The function \code{kendallTrendTest} always returns estimates of
  slope and intercept assuming a linear model (Equation (12)), while the p-value
  is based on Kendall's tau, which is testing for the broader alternative of any
  kind of dependence between the \eqn{X}'s and \eqn{Y}'s.
  \cr

  \emph{Confidence Interval for the Slope} \cr
  Theil (1950) and Sen (1968) proposed methods to compute a confidence interval for
  the true slope, assuming the linear model of Equation (12) (see
  Hollander and Wolfe, 1999, pp.421-422).  Gilbert (1987, p.218) illustrates a
  simpler method than the one given by Sen (1968) that is based on a normal
  approximation.  Gilbert's (1987) method is an extension of the one given in
  Hollander and Wolfe (1999, p.424) that allows for ties and/or multiple
  observations per time period.  This method is valid for a sample size as small as
  \eqn{n=10} unless there are several tied observations.

  Let \eqn{N'} denote the number of defined two-point estimated slopes that are used
  in Equation (15) above (if there are no tied \eqn{X} values then \eqn{N' = N}), and
  let \eqn{\hat{\beta}_{1_{(1)}}, \hat{\beta}_{1_{(2)}}, \ldots, \hat{\beta}_{1_{(N')}}}
  denote the \eqn{N'} ordered slopes.  For Gilbert's (1987) method, a
  \eqn{100(1-\alpha)\%} two-sided confidence interval for the true slope is given by:
  \deqn{[\hat{\beta}_{1_{(M1)}}, \hat{\beta}_{1_{(M2+1)}}] \;\;\;\;\;\; (17)}
  where
  \deqn{M1 = \frac{N' - C_{\alpha}}{2} \;\;\;\;\;\; (18)}
  \deqn{M2 = \frac{N' + C_{\alpha}}{2} \;\;\;\;\;\; (19)}
  \deqn{C_\alpha = z_{1 - \alpha/2} \sqrt{Var(S)} \;\;\;\;\;\; (20)}
  \eqn{Var(S)} is defined in Equations (7), (9), or (11), and
  \eqn{z_p} denotes the \eqn{p}'th quantile of the standard normal distribution.
  One-sided confidence intervals may computed in a similar fashion.

  Usually the quantities \eqn{M1} and \eqn{M2} will not be integers.
  Gilbert (1987, p.219) suggests interpolating between adjacent values in this case,
  which is what the function \code{kendallTrendTest} does.
}
\value{
  A list of class \code{"htestEnvStats"} containing the results of the hypothesis
  test.  See the help file for \code{\link{htestEnvStats.object}} for details.
  In addition, the following components are part of the list returned by
  \code{kendallTrendTest}:

  \item{S}{The value of the Kendall S-statistic.}
  \item{var.S}{The variance of the Kendall S-statistic.}
  \item{slopes}{A numeric vector of all possible two-point slope estimates.
    This component is used by the function \code{\link{kendallSeasonalTrendTest}}.}
}
\references{
  Bradley, J.V. (1968). \emph{Distribution-Free Statistical Tests}.
  Prentice-Hall, Englewood Cliffs, NJ.

  Conover, W.J. (1980). \emph{Practical Nonparametric Statistics}. Second Edition.
  John Wiley and Sons, New York, pp.256-272.

  Gibbons, R.D., D.K. Bhaumik, and S. Aryal. (2009).
  \emph{Statistical Methods for Groundwater Monitoring}, Second Edition.
  John Wiley & Sons, Hoboken.

  Gilbert, R.O. (1987). \emph{Statistical Methods for Environmental Pollution Monitoring}.
  Van Nostrand Reinhold, New York, NY, Chapter 16.

  Helsel, D.R. and R.M. Hirsch. (1988). Discussion of Applicability of the t-test for Detecting Trends
  in Water Quality Variables. \emph{Water Resources Bulletin} \bold{24}(1), 201-204.

  Helsel, D.R., and R.M. Hirsch. (1992). \emph{Statistical Methods in Water Resources Research}.
  Elsevier, NY.

  Helsel, D.R., and R. M. Hirsch. (2002). \emph{Statistical Methods in Water Resources}.
  Techniques of Water Resources Investigations, Book 4, chapter A3. U.S. Geological Survey.
  Available on-line at \url{https://pubs.usgs.gov/tm/04/a03/tm4a3.pdf}.

  Hirsch, R.M., J.R. Slack, and R.A. Smith. (1982). Techniques of Trend Analysis for Monthly Water Quality
  Data. \emph{Water Resources Research} \bold{18}(1), 107-121.

  Hirsch, R.M. and J.R. Slack. (1984). A Nonparametric Trend Test for Seasonal Data with Serial Dependence.
  \emph{Water Resources Research} \bold{20}(6), 727-732.

  Hirsch, R.M., R.B. Alexander, and R.A. Smith. (1991). Selection of Methods for the Detection and
  Estimation of Trends in Water Quality. \emph{Water Resources Research} \bold{27}(5), 803-813.

  Hollander, M., and D.A. Wolfe. (1999). \emph{Nonparametric Statistical Methods,
  Second Edition}.  John Wiley and Sons, New York.

  Kendall, M.G. (1938). A New Measure of Rank Correlation. \emph{Biometrika} \bold{30}, 81-93.

  Kendall, M.G. (1975). \emph{Rank Correlation Methods}. Charles Griffin, London.

  Mann, H.B. (1945). Nonparametric Tests Against Trend. \emph{Econometrica} \bold{13}, 245-259.

  Millard, S.P., and Neerchal, N.K. (2001). \emph{Environmental Statistics with S-PLUS}.
  CRC Press, Boca Raton, Florida.

  Sen, P.K. (1968). Estimates of the Regression Coefficient Based on Kendall's Tau.
  \emph{Journal of the American Statistical Association} \bold{63}, 1379-1389.

  Theil, H. (1950). A Rank-Invariant Method of Linear and Polynomial Regression Analysis, I-III.
  \emph{Proc. Kon. Ned. Akad. v. Wetensch. A.} \bold{53}, 386-392, 521-525, 1397-1412.

  USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
  EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.
  U.S. Environmental Protection Agency, Washington, D.C.

  USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
  EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
  U.S. Environmental Protection Agency, Washington, D.C.

  van Belle, G., and J.P. Hughes. (1984). Nonparametric Tests for Trend in Water Quality.
  \emph{Water Resources Research} \bold{20}(1), 127-136.
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  Kendall's test for independence or trend is a nonparametric test.  No
  assumptions are made about the distribution of the \eqn{X} and \eqn{Y}
  variables.  Hirsch et al. (1982) introduced the "seasonal Kendall test" to
  test for trend within each season.  They note that Kendall's test for trend
  is easy to compute, even in the presence of missing values, and can also be
  used with censored values.

  van Belle and Hughes (1984) note that Kendall's test for trend is slightly
  less powerful than the test based on Spearman's rho, but it converges to
  normality faster.  Also, Bradley (1968, p.288) shows that for the case of a
  linear model with normal (Gaussian) errors, the asymptotic relative
  efficiency of Kendall's test for trend versus the parametric test for a
  zero slope is 0.98.

  The results of the function \code{kendallTrendTest} are similar to the
  results of the built-in \R function \code{\link{cor.test}} with the
  argument \code{method="kendall"} except that \code{\link{cor.test}}
  1) computes exact p-values when the number of pairs is less than 50 and
  there are no ties, and 2) does not return a confidence interval for
  the slope.
}
\seealso{
  \code{\link{cor.test}}, \code{\link{kendallSeasonalTrendTest}}, \code{\link{htestEnvStats.object}}.
}
\examples{
  # Reproduce Example 17-6 on page 17-33 of USEPA (2009).  This example
  # tests for trend in sulfate concentrations (ppm) collected at various
  # months between 1989 and 1996.

  head(EPA.09.Ex.17.6.sulfate.df)
  #  Sample.No Year Month Sampling.Date       Date Sulfate.ppm
  #1         1   89     6          89.6 1989-06-01         480
  #2         2   89     8          89.8 1989-08-01         450
  #3         3   90     1          90.1 1990-01-01         490
  #4         4   90     3          90.3 1990-03-01         520
  #5         5   90     6          90.6 1990-06-01         485
  #6         6   90     8          90.8 1990-08-01         510


  # Plot the data
  #--------------
  dev.new()
  with(EPA.09.Ex.17.6.sulfate.df,
    plot(Sampling.Date, Sulfate.ppm, pch = 15, ylim = c(400, 900),
    xlab = "Sampling Date", ylab = "Sulfate Conc (ppm)",
    main = "Figure 17-6. Time Series Plot of \nSulfate Concentrations (ppm)")
  )
  Sulfate.fit <- lm(Sulfate.ppm ~ Sampling.Date,
    data = EPA.09.Ex.17.6.sulfate.df)
  abline(Sulfate.fit, lty = 2)


  # Perform the Kendall test for trend
  #-----------------------------------
  kendallTrendTest(Sulfate.ppm ~ Sampling.Date,
    data = EPA.09.Ex.17.6.sulfate.df)

  #Results of Hypothesis Test
  #--------------------------
  #
  #Null Hypothesis:                 tau = 0
  #
  #Alternative Hypothesis:          True tau is not equal to 0
  #
  #Test Name:                       Kendall's Test for Trend
  #                                 (with continuity correction)
  #
  #Estimated Parameter(s):          tau       =     0.7667984
  #                                 slope     =    26.6666667
  #                                 intercept = -1909.3333333
  #
  #Estimation Method:               slope:      Theil/Sen Estimator
  #                                 intercept:  Conover's Estimator
  #
  #Data:                            y = Sulfate.ppm
  #                                 x = Sampling.Date
  #
  #Data Source:                     EPA.09.Ex.17.6.sulfate.df
  #
  #Sample Size:                     23
  #
  #Test Statistic:                  z = 5.107322
  #
  #P-value:                         3.267574e-07
  #
  #Confidence Interval for:         slope
  #
  #Confidence Interval Method:      Gilbert's Modification
  #                                 of Theil/Sen Method
  #
  #Confidence Interval Type:        two-sided
  #
  #Confidence Level:                95%
  #
  #Confidence Interval:             LCL = 20.00000
  #                                 UCL = 35.71182


  # Clean up
  #---------
  rm(Sulfate.fit)
  graphics.off()
}
\keyword{htestEnvStats}
\keyword{nonparametric}
\keyword{regression}
