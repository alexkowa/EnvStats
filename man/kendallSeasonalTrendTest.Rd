\name{kendallSeasonalTrendTest}
\alias{kendallSeasonalTrendTest}
\alias{kendallSeasonalTrendTest.default}
\alias{kendallSeasonalTrendTest.formula}
\alias{kendallSeasonalTrendTest.data.frame}
\alias{kendallSeasonalTrendTest.matrix}
\title{
  Nonparametric Test for Monotonic Trend Within Each Season Based on Kendall's Tau Statistic
}
\description{
  Perform a nonparametric test for a monotonic trend within each season based on Kendall's tau
  statistic, and optionally compute a confidence interval for the slope across all seasons.
}
\usage{
kendallSeasonalTrendTest(y, ...)

\method{kendallSeasonalTrendTest}{formula}(y, data = NULL, subset,
  na.action = na.pass, ...)

\method{kendallSeasonalTrendTest}{default}(y, season, year,
  alternative = "two.sided", correct = TRUE, ci.slope = TRUE, conf.level = 0.95,
  independent.obs = TRUE, data.name = NULL, season.name = NULL, year.name = NULL,
  parent.of.data = NULL, subset.expression = NULL, ...)

\method{kendallSeasonalTrendTest}{data.frame}(y, ...)

\method{kendallSeasonalTrendTest}{matrix}(y, ...)
}
\arguments{
  \item{y}{
  an object containing data for the trend test.  In the default method,
  the argument \code{y} must be numeric vector of observations.
  When \code{y} is a data frame, all columns must be numeric.
  When \code{y} is a matrix, it must be a numeric matrix.
  In the formula method, \code{y} must be a formula of the form
  \code{y ~ season + year}, where \code{y}, \code{season}, and \code{year}
  specify what variables to use for the these arguments in the call to
  \code{kendallSeasonalTrendTest.default}.  Missing (\code{NA}), undefined (\code{NaN}),
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
  \item{season}{
  numeric or character vector or a factor indicating the seasons in which the observations in
  \code{y} were taken.  The length of \code{season} must equal the length of \code{y}.
}
  \item{year}{
  numeric vector indicating the years in which the observations in \code{y} were taken.
  The length of \code{year} must equal the length of \code{y}.
}
  \item{alternative}{
  character string indicating the kind of alternative hypothesis.  The
  possible values are \code{"two.sided"} (tau not equal to 0; the default),
  \code{"less"} (tau less than 0), and \code{"greater"} (tau greater than 0).
}
  \item{correct}{
  logical scalar indicating whether to use the correction for continuity in
  computing the \eqn{z}-statistic that is based on the test statistic \eqn{S'}.
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
  \item{independent.obs}{
  logical scalar indicating whether to assume the observations in \code{y} are seially independent.
  The default value is \code{TRUE}.
}
  \item{data.name}{
  character string indicating the name of the data used for the trend test.
  The default value is \code{deparse(substitute(y))}.
}
  \item{season.name}{
  character string indicating the name of the data used for the season.
  The default value is \code{deparse(substitute(season))}.
}
  \item{year.name}{
  character string indicating the name of the data used for the year.
  The default value is \code{deparse(substitute(year))}.
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
  Hirsch et al. (1982) introduced a modification of Kendall's test for trend
  (see \code{\link{kendallTrendTest}}) that allows for seasonality in observations collected over time.
  They call this test the \emph{seasonal Kendall test}.  Their test is appropriate for testing for
  trend in each season when the trend is always in the same direction across all seasons.
  van Belle and Hughes (1984) introduced a heterogeneity test for trend which is appropriate for testing
  for trend in any direction in any season.  Hirsch and Slack (1984) proposed an extension to the seasonal
  Kendall test that allows for serial dependence in the observations.  The function
  \code{\link{kendallSeasonalTrendTest}} includes all of these tests, as well as an extension of the
  van Belle-Hughes heterogeneity test to the case of serial dependence.

  \bold{Testing for Trend Assuming Serial Independence} \cr

  \emph{The Model} \cr
  Assume observations are taken over two or more years, and assume a single year
  can be divided into two or more seasons.  Let \eqn{p} denote the number of seasons.
  Let \eqn{X} and \eqn{Y} denote two continuous random variables with some joint
  (bivariate) distribution (which may differ from season to season).  Let \eqn{N_j}
  denote the number of bivariate observations taken in the \eqn{j}'th season (over two
  or more years) (\eqn{j = 1, 2, \ldots, p}), so that
  \deqn{(X_{1j}, Y_{1j}), (X_{2j}, Y_{2j}), \ldots, (X_{N_jj}, Y_{N_jj})}
  denote the \eqn{N_j} bivariate observations from this distribution for season
  \eqn{j}, assume these bivariate observations are mutually independent, and let
  \deqn{\tau_j = \{ 2 Pr[(X_{2j} - X_{1j})(Y_{2j} - Y_{1j}) > 0] \} - 1 \;\;\;\;\;\; (1)}
  denote the value of Kendall's tau for that season (see \code{\link{kendallTrendTest}}).
  Also, assume all of the \eqn{Y} observations are independent.

  The function \code{kendallSeasonalTrendTest} assumes that the \eqn{X} values always
  denote the year in which the observation was taken.  Note that within any season,
  the \eqn{X} values need not be unique.  That is, there may be more than one
  observation within the same year within the same season.  In this case, the
  argument \code{y} must be a numeric vector, and you must supply the additional
  arguments \code{season} and \code{year}.

  If there is only one observation per season per year (missing values allowed), it is
  usually easiest to supply the argument \code{y} as an \eqn{n \times p} matrix or
  data frame, where \eqn{n} denotes the number of years. In this case
  \deqn{N_1 = N_2 = \cdots = N_p = n \;\;\;\;\;\; (2)}
  and
  \deqn{X_{ij} = i \;\;\;\;\;\; (3)}
  for \eqn{i = 1, 2, \ldots, n} and \eqn{j = 1, 2, \ldots, p}, so if \eqn{Y} denotes
  the \eqn{n \times p} matrix of observed \eqn{Y}'s and \eqn{X} denotes the
  \eqn{n \times p} matrix of the \eqn{X}'s, then
  \tabular{llllll}{
                        \tab \eqn{Y_{11}} \tab \eqn{Y_{12}} \tab \eqn{\cdots} \tab \eqn{Y_{1p}} \tab \cr
                        \tab \eqn{Y_{21}} \tab \eqn{Y_{22}} \tab \eqn{\cdots} \tab \eqn{Y_{2p}} \tab \cr
  \eqn{\underline{Y} =} \tab .            \tab              \tab              \tab              \tab \eqn{\;\;\;\;\;\; (4)} \cr
                        \tab .            \tab              \tab              \tab              \tab \cr
                        \tab .            \tab              \tab              \tab              \tab \cr
                        \tab \eqn{Y_{n1}} \tab \eqn{Y_{n2}} \tab \eqn{\cdots} \tab \eqn{Y_{np}} \tab
  }
  \tabular{llllll}{
                        \tab \eqn{1} \tab \eqn{1} \tab \eqn{\cdots} \tab \eqn{1} \tab \cr
                        \tab \eqn{2} \tab \eqn{2} \tab \eqn{\cdots} \tab \eqn{2} \tab \cr
  \eqn{\underline{X} =} \tab .       \tab         \tab              \tab         \tab \eqn{\;\;\;\;\;\; (5)} \cr
                        \tab .       \tab         \tab              \tab         \tab \cr
                        \tab .       \tab         \tab              \tab         \tab \cr
                        \tab \eqn{n} \tab \eqn{n} \tab \eqn{\cdots} \tab \eqn{n} \tab
  }
  The null hypothesis is that within each season the \eqn{X} and \eqn{Y} random
  variables are independent; that is, within each season there is no trend in the
  \eqn{Y} observations over time.  This null hypothesis can be expressed as:
  \deqn{H_0: \tau_1 = \tau_2 = \cdots = \tau_p = 0 \;\;\;\;\;\; (6)}
  \cr

  \emph{The Seasonal Kendall Test for Trend} \cr
  Hirsch et al.'s (1982) extension of Kendall's tau statistic to test the null
  hypothesis (6) is based on simply summing together the Kendall \eqn{S}-statistics
  for each season and computing the following statistic:
  \deqn{z = \frac{S'}{\sqrt{Var(S')}} \;\;\;\;\;\; (7)}
  or, using the correction for continuity,
  \deqn{z = \frac{S' - sign(S')}{\sqrt{Var(S')}} \;\;\;\;\;\; (8)}
  where
  \deqn{S' = \sum_{j=1}^p S_j \;\;\;\;\;\; (9)}
  \deqn{S_j = \sum_{i=1}^{N_j-1} \sum_{k=i+1}^{N_j} sign[(X_{kj} - X_{ij})(Y_{kj} - Y_{ij})] \;\;\;\;\;\; (10)}
  and \eqn{sign()} denotes the \code{\link{sign}} function:
  \tabular{lrl}{
                    \tab \eqn{-1} \tab \eqn{x < 0} \cr
    \eqn{sign(x) =} \tab \eqn{0}  \tab \eqn{x = 0 \;\;\;\;\;\; (11)} \cr
                    \tab \eqn{1}  \tab \eqn{x > 0}
  }
  Note that the quantity in Equation (10) is simply the Kendall \eqn{S}-statistic for
  season \eqn{j} (\eqn{j = 1, 2, \ldots, p}) (see Equation (3) in the help file for
  \code{\link{kendallTrendTest}}).

  For each season, if the predictor variables (the \eqn{X}'s) are strictly increasing
  (e.g., Equation (3) above), then Equation (10) simplifies to
  \deqn{S_j = \sum_{i=1}^{N_j-1} \sum_{k=i+1}^{N_j} sign[(Y_{kj} - Y_{ij})] \;\;\;\;\;\; (12)}
  Under the null hypothesis (6), the quantity \eqn{z} defined in Equation (7) or (8)
  is approximately distributed as a standard normal random variable.

  Note that there may be missing values in the observations, so let \eqn{n_j}
  denote the number of \eqn{(X,Y)} pairs without missing values for season \eqn{j}.

  The statistic \eqn{S'} in Equation (9) has mean and variance given by:
  \deqn{E(S') = \sum_{j = 1}^p E(S_j) \;\;\;\;\;\; (13)}
  \deqn{Var(S') = \sum_{j = 1}^p Var(S_j) + \sum_{g=1}^{p-1} \sum_{h=g+1}^{p} 2 Cov(S_g, S_h) \;\;\;\;\;\; (14)}
  Since all the observations are assumed to be mutually independent,
  \deqn{\sigma_{gh} = Cov(S_g, S_h) = 0, \;\; g \ne h, \;\; g,h = 1, 2, \ldots, p \;\;\;\;\;\; (15)}
  Furthermore, under the null hypothesis (6),
  \deqn{E(S_j) = 0, \;\; j = 1, 2, \ldots, p \;\;\;\;\;\; (16)}
  and, in the case of no tied observations,
  \deqn{Var(S_j) = \frac{n_j(n_j-1)(2n_j+5)}{18} \;\;\;\;\;\; (17)}
  for \eqn{j=1,2, \ldots, p} (see equation (7) in the help file for
  \code{\link{kendallTrendTest}}).

  In the case of tied observations,
  \tabular{ll}{
  \eqn{Var(S_j) =} \tab \eqn{\frac{n_j(n_j-1)(2n_j+5)}{18} -} \cr
                 \tab \cr
                 \tab \eqn{\frac{\sum_{i=1}^{g} t_i(t_i-1)(2t_i+5)}{18} - } \cr
                 \tab \cr
                 \tab \eqn{\frac{\sum_{k=1}^{h} u_k(u_k-1)(2u_k+5)}{18} + } \cr
                 \tab \cr
                 \tab \eqn{\frac{[\sum_{i=1}^{g} t_i(t_i-1)(t_i-2)][\sum_{k=1}^{h} u_k(u_k-1)(u_k-2)]}{9n_k(n_k-1)(n_k-2)} +} \cr
                 \tab \cr
                 \tab \eqn{\frac{[\sum_{i=1}^{g} t_i(t_i-1)][\sum_{k=1}^{h} u_k(u_k-1)]}{2n_k(n_k-1)} \;\;\;\;\;\; (18)}
  }
  where \eqn{g} is the number of tied groups in the \eqn{X} observations for
  season \eqn{j}, \eqn{t_i} is the size of the \eqn{i}'th tied group in the \eqn{X}
  observations for season \eqn{j}, \eqn{h} is the number of tied groups in the \eqn{Y}
  observations for season \eqn{j}, and \eqn{u_k} is the size of the \eqn{k}'th tied
  group in the \eqn{Y} observations for season \eqn{j}
  (see Equation (9) in the help file for \code{\link{kendallTrendTest}}).
  \cr

  \emph{Estimating \eqn{\tau}, Slope, and Intercept} \cr
  The function \code{kendall.SeasonalTrendTest} returns estimated values of
  Kendall's \eqn{\tau}, the slope, and the intercept for each season, as well as a
  single estimate for each of these three quantities combined over all seasons.
  The overall estimate of \eqn{\tau} is the weighted average of the p seasonal
  \eqn{\tau}'s:
  \deqn{\hat{\tau} = \frac{\sum_{j=1}^p n_j \hat{\tau}_j}{\sum_{j=1}^p n_j} \;\;\;\;\;\; (19)}
  where
  \deqn{\hat{\tau}_j = \frac{2S_j}{n_j(n_j-1)} \;\;\;\;\;\; (20)}
  (see Equation (2) in the help file for \code{\link{kendallTrendTest}}).

  We can compute the estimated slope for season \eqn{j} as:
  \deqn{\hat{\beta}_{1_j} = Median(\frac{Y_{kj} - Y_{ij}}{X_{kj} - X_{ij}}); \;\; i < k; \;\; X_{kj} \ne X_{ik} \;\;\;\;\;\; (21)}
  for \eqn{j = 1, 2, \ldots, p}.  The overall estimate of slope, however, is
  \bold{not} the median of these \eqn{p} estimates of slope; instead,
  following Hirsch et al. (1982, p.117), the overall estimate of slope is the median
  of all two-point slopes computed within each season:
  \deqn{\hat{\beta}_1 = Median(\frac{Y_{kj} - Y_{ij}}{X_{kj} - X_{ij}}); \;\; i < k; \;\; X_{kj} \ne X_{ik}; \;\; j = 1, 2, \ldots, p \;\;\;\;\;\; (22)}
  (see Equation (15) in the help file for \code{\link{kendallTrendTest}}).

  The overall estimate of intercept is the median of the \eqn{p} seasonal estimates of
  intercept:
  \deqn{\hat{\beta}_0 = Median(\hat{\beta}_{0_1}, \hat{\beta}_{0_2}, \ldots, \hat{\beta}_{0_p}) \;\;\;\;\;\; (23)}
  where
  \deqn{\hat{\beta}_{0_j} = Y_{0.5_j} - \hat{\beta}_{1_j} X_{0.5_j} \;\;\;\;\;\; (24)}
  and \eqn{X_{0.5_j}} and \eqn{Y_{0.5_j}} denote the sample medians of the \eqn{X}'s
  and \eqn{Y}'s, respectively, for season \eqn{j}
  (see Equation (16) in the help file for \code{\link{kendallTrendTest}}).
  \cr

  \emph{Confidence Interval for the Slope} \cr
  Gilbert (1987, p.227-228) extends his method of computing a confidence interval for
  the slope to the case of seasonal observations.  Let \eqn{N'} denote the number of
  defined two-point estimated slopes that are used in Equation (22) above and let
  \deqn{\hat{\beta}_{1_{(1)}}, \hat{\beta}_{1_{(2)}}, \ldots, \hat{\beta}_{1_{(N')}}}
  denote the \eqn{N'} ordered slopes.  For Gilbert's (1987) method, a
  \eqn{100(1-\alpha)\%} two-sided confidence interval for the true over-all
  slope across all seasons is given by:
  \deqn{[\hat{\beta}_{1_{(M1)}}, \hat{\beta}_{1_{(M2+1)}}] \;\;\;\;\;\; (25)}
  where
  \deqn{M1 = \frac{N' - C_{\alpha}}{2} \;\;\;\;\;\; (26)}
  \deqn{M2 = \frac{N' + C_{\alpha}}{2} \;\;\;\;\;\; (27)}
  \deqn{C_\alpha = z_{1 - \alpha/2} \sqrt{Var(S')} \;\;\;\;\;\; (28)}
  \eqn{Var(S')} is defined in Equation (14), and
  \eqn{z_p} denotes the \eqn{p}'th quantile of the standard normal distribution.
  One-sided confidence intervals may computed in a similar fashion.

  Usually the quantities \eqn{M1} and \eqn{M2} will not be integers.
  Gilbert (1987, p.219) suggests interpolating between adjacent values in this case,
  which is what the function \code{kendallSeasonalTrendTest} does.
  \cr

  \emph{The Van Belle-Hughes Heterogeneity Test for Trend} \cr
  The seasonal Kendall test described above is appropriate for testing the null
  hypothesis (6) against the alternative hypothesis of a trend in at least one season.
  All of the trends in each season should be in the same direction.

  The seasonal Kendall test is not appropriate for testing for trend when there are
  trends in a positive direction in one or more seasons and also negative trends in
  one or more seasons.  For example, for the following set of observations, the
  seasonal Kendall statistic \eqn{S'} is 0 with an associated two-sided p-value of 1,
  even though there is clearly a positive trend in season 1 and a negative trend in
  season 2.
  \tabular{ccc}{
    \bold{Year} \tab \bold{Season 1} \tab \bold{Season 2} \cr
    1           \tab 5               \tab 8 \cr
    2           \tab 6               \tab 7 \cr
    3           \tab 7               \tab 6 \cr
    4           \tab 8               \tab 5
  }
  Van Belle and Hughes (1984) suggest using the following statistic to test for
  heterogeneity in trend prior to applying the seasonal Kendall test:
  \deqn{\chi_{het}^2 = \sum_{j=1}^p Z_j^2 - p \bar{Z}^2 \;\;\;\;\;\; (29)}
  where
  \deqn{Z_j = \frac{S_j}{Var(S_j)} \;\;\;\;\;\; (30)}
  \deqn{\bar{Z} = \frac{1}{p} \sum_{j=1}^p Z_j \;\;\;\;\;\; (31)}
  Under the null hypothesis (6), the statistic defined in Equation (29) is
  approximately distributed as a \link[=Chisquare]{chi-square random variable} with
  \eqn{p-1} degrees of freedom.  Note that the continuity correction is not used to
  compute the \eqn{Z_j}'s defined in Equation (30) since using it results in an
  unacceptably conservative test (van Belle and Hughes, 1984, p.132).  Van Belle and
  Hughes (1984) actually call the statistic in (29) a homogeneous chi-square statistic.
  Here it is called a heterogeneous chi-square statistic after the alternative
  hypothesis it is meant to test.

  Van Belle and Hughes (1984) imply that the heterogeneity statistic defined in
  Equation (29) may be used to test the null hypothesis:
  \deqn{H_0: \tau_1 = \tau_2 = \cdots = \tau_p = \tau \;\;\;\;\;\; (32)}
  where \eqn{\tau} is some arbitrary number between -1 and 1.  For this case, however,
  the distribution of the test statistic in Equation (29) is unknown since it depends
  on the unknown value of \eqn{\tau} (Equations (16)-(18) above assume
  \eqn{\tau = 0} and are not correct if \eqn{\tau \ne 0}). The heterogeneity
  chi-square statistic of Equation (29) may be assumed to be approximately
  distributed as chi-square with \eqn{p-1} degrees of freedom under the null
  hypothesis (32), but further study is needed to determine how well this
  approximation works.
  \cr

  \bold{Testing for Trend Assuming Serial Dependence} \cr

  \emph{The Model} \cr
  Assume the same model as for the case of serial independence, except now the
  observed \eqn{Y}'s are not assumed to be independent of one another, but are
  allowed to be serially correlated over time.  Furthermore, assume one observation
  per season per year (Equations (2)-(5) above).
  \cr

  \emph{The Seasonal Kendall Test for Trend Modified for Serial Dependence} \cr
  Hirsch and Slack (1984) introduced a modification of the seasonal Kendall test that
  is robust against serial dependence (in terms of Type I error) except when the
  observations have a very strong long-term persistence (very large autocorrelation) or
  when the sample sizes are small (e.g., 5 years of monthly data).  This modification
  is based on a multivariate test introduced by Dietz and Killeen (1981).

  In the case of serial dependence, Equation (15) is no longer true, so an estimate of
  the correct value of \eqn{\sigma_{gh}} must be used to compute Var(S') in
  Equation (14).  Let \eqn{R} denote the \eqn{n \times p} matrix of ranks for the
  \eqn{Y} observations (Equation (4) above), where the \eqn{Y}'s are ranked within
  season:
  \tabular{llllll}{
                        \tab \eqn{R_{11}} \tab \eqn{R_{12}} \tab \eqn{\cdots} \tab \eqn{R_{1p}} \tab \cr
                        \tab \eqn{R_{21}} \tab \eqn{R_{22}} \tab \eqn{\cdots} \tab \eqn{R_{2p}} \tab \cr
  \eqn{\underline{R} =} \tab .            \tab              \tab              \tab              \tab \eqn{\;\;\;\;\;\; (33)} \cr
                        \tab .            \tab              \tab              \tab              \tab \cr
                        \tab .            \tab              \tab              \tab              \tab \cr
                        \tab \eqn{R_{n1}} \tab \eqn{R_{n2}} \tab \eqn{\cdots} \tab \eqn{R_{np}} \tab
  }
  where
  \deqn{R_{ij} = \frac{1}{2} [n_j + 1 \sum_{k=1}^{n_j} sign(Y_{ij} - Y_{kj})] \;\;\;\;\;\; (34)}
  the \eqn{sign} function is defined in Equation (11) above, and as before \eqn{n_j} denotes
  the number of \eqn{(X,Y)} pairs without missing values for season \eqn{j}.  Note that
  by this definition, missing values are assigned the mid-rank of the non-missing
  values.

  Hirsch and Slack (1984) suggest using the following formula, given by Dietz and
  Killeen (1981), in the case where there are no missing values:
  \deqn{\hat{\sigma}_{gh} = \frac{1}{3} [K_{gh} + 4 \sum_{i=1}^n R_{ig}R_{ih} - n(n+1)^2] \;\;\;\;\;\; (35)}
  where
  \deqn{K_{gh} = \sum_{i=1}^{n-1} \sum_{j=i+1}^n sign[(Y_{jg} - Y_{ig})(Y_{jh} - Y_{ih})] \;\;\;\;\;\; (36)}
  Note that the quantity defined in Equation (36) is Kendall's tau for season \eqn{g}
  vs. season \eqn{h}.

  For the case of missing values, Hirsch and Slack (1984) derive the following
  modification of Equation (35):
  \deqn{\hat{\sigma}_{gh} = \frac{1}{3} [K_{gh} + 4 \sum_{i=1}^n R_{ig}R_{ih} - n(n_g + 1)(n_h + 1)] \;\;\;\;\;\; (37)}
  Technically, the estimates in Equations (35) and (37) are not correct estimators of
  covariance, and Equations (17) and (18) are not correct estimators of variance,
  because the model Dietz and Killeen (1981) use assumes that observations within the
  rows of \eqn{Y} (Equation (4) above) may be correlated, but observations between
  rows are independent.  Serial dependence induces correlation between all of the
  \eqn{Y}'s.  In most cases, however, the serial dependence shows an exponential decay
  in correlation across time and so these estimates work fairly well (see more
  discussion in the BACKGROUND section below).
  \cr

  \emph{Estimates and Confidence Intervals} \cr
  The seasonal and over-all estimates of \eqn{\tau}, slope, and intercept are computed
  using the same methods as in the case of serial independence.  Also, the method for
  computing the confidence interval for the slope is the same as in the case of serial
  independence.  Note that the serial dependence is accounted for in the term
  \eqn{Var(S')} in Equation (28).
  \cr

  \emph{The Van Belle-Hughes Heterogeneity Test for Trend Modified for Serial Dependence} \cr
  Like its counterpart in the case of serial independence, the seasonal Kendall test
  modified for serial dependence described above is appropriate for testing the null
  hypothesis (6) against the alternative hypothesis of a trend in at least one season.
  All of the trends in each season should be in the same direction.

  The modified seasonal Kendall test is not appropriate for testing for trend when
  there are trends in a positive direction in one or more seasons and also negative
  trends in one or more seasons.  This section describes a modification of the
  van Belle-Hughes heterogeneity test for trend in the presence of serial dependence.

  Let \eqn{\underline{S}} denote the \eqn{p \times 1} vector of Kendall \eqn{S}-statistics for
  each season:
  \tabular{lll}{
                        \tab \eqn{S_1} \tab \cr
                        \tab \eqn{S_2} \tab \cr
  \eqn{\underline{S} =} \tab .         \tab \eqn{\;\;\;\;\;\; (38)} \cr
                        \tab .         \tab \cr
                        \tab .         \tab \cr
                        \tab \eqn{S_p} \tab
  }
  The distribution of \eqn{\underline{S}} is approximately multivariate normal with
  \tabular{lll}{
                                             \tab \eqn{\mu_1} \tab \cr
                                             \tab \eqn{\mu_2} \tab \cr
  \eqn{E(\underline{S}) = \underline{\mu} =} \tab .           \tab \eqn{\;\;\;\;\;\; (39)} \cr
                                             \tab .           \tab \cr
                                             \tab .           \tab \cr
                                             \tab \eqn{\mu_p}
  }
  \tabular{llllll}{
                                      \tab \eqn{\sigma_1^2}  \tab \eqn{\sigma_{12}} \tab \eqn{\cdots} \tab \eqn{\sigma_{1p}} \tab \cr
                                      \tab \eqn{\sigma_{21}} \tab \eqn{\sigma_2^2}  \tab \eqn{\cdots} \tab \eqn{\sigma_{2p}} \tab \cr
  \eqn{Var(\underline{S}) = \Sigma =} \tab .                 \tab                   \tab              \tab                   \tab \eqn{\;\;\;\;\;\; (40)} \cr
                                      \tab .                 \tab                   \tab              \tab                   \tab \cr
                                      \tab .                 \tab                   \tab              \tab                   \tab \cr
                                      \tab \eqn{\sigma_{n1}} \tab \eqn{\sigma_{n2}} \tab \eqn{\cdots} \tab \eqn{\sigma_n^2}  \tab
  }
  where
  \deqn{\mu_j = \frac{n_j(n_j - 1)}{2} \tau_j, \;\; j = 1, 2, \ldots, p \;\;\;\;\;\; (41)}
  Define the \eqn{p \times p} matrix \eqn{\underline{m}} as
  \tabular{llllll}{
                        \tab \eqn{\frac{2}{n_1(n_1 - 1)}} \tab \eqn{0}                       \tab \eqn{\cdots} \tab \eqn{0} \tab \cr
                        \tab \eqn{0}                      \tab \eqn{\frac{2}{n_2(n_2 - 1)}}  \tab \eqn{\cdots} \tab \eqn{0} \tab \cr
  \eqn{\underline{m} =} \tab .                            \tab                               \tab              \tab         \tab \eqn{\;\;\;\;\;\; (42)} \cr
                        \tab .                            \tab                               \tab              \tab         \tab \cr
                        \tab .                            \tab                               \tab              \tab         \tab \cr
                        \tab \eqn{0}                      \tab \eqn{0}                       \tab \eqn{\cdots} \tab \eqn{\frac{2}{n_p(n_p - 1)}}
  }
  Then the vector of the seasonal estimates of \eqn{\tau} can be written as:
  \tabular{lllll}{
                                 \tab \eqn{\hat{\tau}_1} \tab         \tab \eqn{2S_1/[n_1(n_1-1)]} \tab                                     \cr
                                 \tab \eqn{\hat{\tau}_2} \tab         \tab \eqn{2S_2/[n_2(n_2-1)]} \tab                                     \cr
  \eqn{\underline{\hat{\tau}} =} \tab .                  \tab \eqn{=} \tab .                       \tab \eqn{= \underline{m} \; \underline{S} \;\;\;\;\; (43)} \cr
                                 \tab .                  \tab         \tab .                       \tab                                     \cr
                                 \tab .                  \tab         \tab .                       \tab                                     \cr
                                 \tab \eqn{\hat{\tau}_p} \tab         \tab \eqn{2S_p/[n_p(n_p-1)]} \tab
  }
  so the distribution of the vector in Equation (43) is approximately multivariate
  normal with
  \tabular{llllll}{
                                    \tab                                        \tab                                       \tab                          \tab \eqn{\tau_1} \tab \cr
                                    \tab                                        \tab                                       \tab                          \tab \eqn{\tau_2} \tab \cr
  \eqn{E(\underline{\hat{\tau}}) =} \tab \eqn{E(\underline{m} \underline{S}) =} \tab \eqn{\underline{m} \underline{\mu} =} \tab \eqn{\underline{\tau} =} \tab .            \tab \eqn{\;\;\;\;\;\; (44)} \cr
                                    \tab                                        \tab                                       \tab                          \tab .            \tab \cr
                                    \tab                                        \tab                                       \tab                          \tab .            \tab \cr
                                    \tab                                        \tab                                       \tab                          \tab \eqn{\tau_p}
  }
  \deqn{Var(\underline{\hat{\tau}}) = Var(\underline{m} \; \underline{S}) = \underline{m} \Sigma \underline{m}^T = \Sigma_{\hat{\tau}} \;\;\;\;\;\; (45)}
  where \eqn{^T} denotes the transpose operator.
  Let \eqn{\underline{C}} denote the \eqn{(p-1) \times p} contrast matrix
  \deqn{\underline{C} = [\; \underline{1} \; | \; -I_p] \;\;\;\;\;\; (46)}
  where \eqn{I_p} denotes the \eqn{p \times p} identity matrix.  That is,
  \tabular{lrrrrr}{
                        \tab \eqn{1} \tab \eqn{-1} \tab \eqn{0}  \tab \eqn{\cdots} \tab \eqn{0}  \cr
                        \tab \eqn{1} \tab \eqn{0}  \tab \eqn{-1} \tab \eqn{\cdots} \tab \eqn{0}  \cr
  \eqn{\underline{C} =} \tab .       \tab          \tab          \tab              \tab .        \cr
                        \tab .       \tab          \tab          \tab              \tab .        \cr
                        \tab .       \tab          \tab          \tab              \tab .        \cr
                        \tab \eqn{1} \tab \eqn{0}  \tab \eqn{0}  \tab \eqn{\cdots} \tab \eqn{-1}
  }
  Then the null hypothesis (32) is equivalent to the null hypothesis:
  \deqn{H_0: \underline{C} \underline{\tau} = 0 \;\;\;\;\;\; (47)}
  Based on theory for samples from a multivariate normal distribution
  (Johnson and Wichern, 2007), under the null hypothesis (47) the quantity
  \deqn{\chi_{het}^2 = (\underline{C} \; \underline{\hat{\tau}})^T (\underline{C} \hat{\Sigma}_{\hat{\tau}} \underline{C}^T)^{-1} (\underline{C} \; \underline{\hat{\tau}}) \;\;\;\;\;\; (48)}
  has approximately a chi-square distribution with \eqn{p-1} degrees of freedom for
  \dQuote{large} values of seasonal sample sizes, where
  \deqn{\hat{\Sigma}_{\hat{\tau}} = \underline{m} \hat{\Sigma} \underline{m}^T \;\;\;\;\;\; (49)}
  The estimate of \eqn{\Sigma} in Equation (49) can be computed using the same formulas
  that are used for the modified seasonal Kendall test (i.e., Equation (35) or (37)
  for the off-diagonal elements and Equation (17) or (18) for the diagonal elements).
  As previously noted, the formulas for the variances are actually only valid if
  \eqn{t = 0} and there is no correlation between the rows of \eqn{Y}.  The same is
  true of the formulas for the covariances.  More work is needed to determine the
  goodness of the chi-square approximation for the test statistic in (48).  The
  pseudo-heterogeneity test statistic of Equation (48), however, should provide some
  guidance as to whether the null hypothesis (32) (or equivalently (47)) appears to be
  true.
}
\value{
  A list of class \code{"htestEnvStats"} containing the results of the hypothesis
  test.  See the help file for \code{\link{htestEnvStats.object}} for details.
  In addition, the following components are part of the list returned by \cr
  \code{kendallSeasonalTrendTest}:

  \item{seasonal.S}{numeric vector.  The value of the Kendall S-statistic for each season.}
  \item{var.seasonal.S}{numeric vector.  The variance of the Kendall S-statistic for each season.
    This component only appears when \code{independent.obs=TRUE}.}
  \item{var.cov.seasonal.S}{numeric matrix.  The estimated variance-covariance matrix of the Kendall
    S-statistics for each season. This component only appears when \cr
    \code{independent.obs=FALSE}.}
  \item{seasonal.estimates}{numeric matrix.  The estimated Kendall's tau, slope, and intercept for
    each season.}
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

  Hollander, M., and D.A. Wolfe. (1999). \emph{Nonparametric Statistical Methods, Second Edition}.
  John Wiley and Sons, New York.

  Johnson, R.A., and D.W. Wichern. (2007).  \emph{Applied Multivariate Statistical
  Analysis}, Sixth Edition.  Pearson Prentice Hall, Upper Saddle River, NJ.

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
  Kendall's test for independence or trend is a nonparametric test.  No assumptions are made about the
  distribution of the \eqn{X} and \eqn{Y} variables.  Hirsch et al. (1982) introduced the seasonal
  Kendall test to test for trend within each season.  They note that Kendall's test for trend is easy to
  compute, even in the presence of missing values, and can also be used with censored values.

  van Belle and Hughes (1984) note that the seasonal Kendall test introduced by Hirsch et al. (1982) is
  similar to a multivariate extension of the sign test proposed by Jonckheere (1954).  Jonckheeere's test
  statistic is based on the unweighted sum of the seasonal tau statistics, while Hirsch et al.'s test is
  based on the weighted sum (weighted by number of observations within a season) of the seasonal tau
  statistics.

  van Belle and Hughes (1984) also note that Kendall's test for trend is slightly less powerful than the test
  based on Spearman's rho, but it converges to normality faster.  Also, Bradley (1968, p.288) shows that for
  the case of a linear model with normal (Gaussian) errors, the asymptotic relative efficiency of
  Kendall's test for trend versus the parametric test for a zero slope is 0.98.

  Based on the work of Dietz and Killeen (1981), Hirsch and Slack (1984) describe a modified version of the
  seasonal Kendall test that allows for serial dependence in the observations.  They performed a Monte Carlo
  study to determine the empirical significance level and power of this modified test vs. the test that
  assumes independent observations and found a trade-off between power and the correct significance level.
  For \eqn{p = 12} seasons, they found the modified test gave correct significance levels for \eqn{n \geq 10}
  as long as the lag-one autocorrelation was 0.6 or less, while the original test that assumes independent
  observations yielded highly inflated significance levels.  On the other hand, if in fact the observations
  are serially independent, the original test is more powerful than the modified test.

  Hirsch and Slack (1984) also looked at the performance of the test for trend introduced by
  Dietz and Killeen (1981), which is a weighted sums of squares of the seasonal Kendall S-statistics,
  where the matrix of weights is the inverse of the covariance matrix.  The Dietz-Killeen test statistic,
  unlike the one proposed by Hirsh and Slack (1984), tests for trend in either direction in any season,
  and is asymptotically distributed as a chi-square random variable with \eqn{p} (number of seasons)
  degrees of freedom.  Hirsch and Slack (1984), however, found that the test based on this statistic is
  quite conservative (i.e., the significance level is much smaller than the assumed significance level)
  and has poor power even for moderate sample sizes.  The chi-square approximation becomes reasonably
  close only when \eqn{n > 40} if \eqn{p = 12}, \eqn{n > 30} if \eqn{p = 4}, and \eqn{n > 20} if
  \eqn{p = 2}.

  Lettenmaier (1988) notes the poor power of the test proposed by Dietz and Killeen (1981) and states the
  poor power apparently results from an upward bias in the estimated variance of the statistic, which can
  be traced to the inversion of the estimated covariance matrix.  He suggests an alternative test statistic
  (to test trend in either direction in any season) that is the sum of the squares of the scaled seasonal
  Kendall S-statistics (scaled by their standard deviations).  Note that this test statistic ignores
  information about the covariance between the seasonal Kendall S-statistics, although its distribution
  depends on these covariances.  In the case of no serial dependence, Lettenmaier's test statistic is
  exactly the same as the Dietz-Killeen test statistic.  In the case of serial dependence,
  Lettenmaier (1988) notes his test statistic is a quadratic form of a multivariate normal random variable
  and therefore all the moments of this random variable are easily computed.  Lettenmaier (1988)
  approximates the distribution of his test statistic as a scaled non-central chi-square distribution
  (with fractional degrees of freedom).  Based on extensive Monte Carlo studies, Lettenmaier (1988) shows
  that for the case when the trend is the same in all seasons, the seasonal Kendall's test of
  Hirsch and Slack (1984) is superior to his test and far superior to the Dietz-Killeen test.
  The power of Lettenmaier's test approached that of the seasonal Kendall test for large trend magnitudes.
}
\seealso{
  \code{\link{kendallTrendTest}}, \code{\link{htestEnvStats.object}}, \code{\link{cor.test}}.
}
\examples{
  # Reproduce Example 14-10 on page 14-38 of USEPA (2009).  This example
  # tests for trend in analyte concentrations (ppm) collected monthly
  # between 1983 and 1985.

  head(EPA.09.Ex.14.8.df)
  #     Month Year Unadj.Conc Adj.Conc
  #1  January 1983       1.99     2.11
  #2 February 1983       2.10     2.14
  #3    March 1983       2.12     2.10
  #4    April 1983       2.12     2.13
  #5      May 1983       2.11     2.12
  #6     June 1983       2.15     2.12

  tail(EPA.09.Ex.14.8.df)
  #       Month Year Unadj.Conc Adj.Conc
  #31      July 1985       2.31     2.23
  #32    August 1985       2.32     2.24
  #33 September 1985       2.28     2.23
  #34   October 1985       2.22     2.24
  #35  November 1985       2.19     2.25
  #36  December 1985       2.22     2.23


  # Plot the data
  #--------------
  Unadj.Conc <- EPA.09.Ex.14.8.df$Unadj.Conc
  Adj.Conc   <- EPA.09.Ex.14.8.df$Adj.Conc
  Month      <- EPA.09.Ex.14.8.df$Month
  Year       <- EPA.09.Ex.14.8.df$Year
  Time       <- paste(substring(Month, 1, 3), Year - 1900, sep = "-")
  n          <- length(Unadj.Conc)
  Three.Yr.Mean <- mean(Unadj.Conc)

  dev.new()
  par(mar = c(7, 4, 3, 1) + 0.1, cex.lab = 1.25)
  plot(1:n, Unadj.Conc, type = "n", xaxt = "n",
	xlab = "Time (Month)",
	ylab = "ANALYTE CONCENTRATION (mg/L)",
	main = "Figure 14-15. Seasonal Time Series Over a Three Year Period",
	cex.main = 1.1)
  axis(1, at = 1:n, labels = rep("", n))
  at <- rep(c(1, 5, 9), 3) + rep(c(0, 12, 24), each = 3)
  axis(1, at = at, labels = Time[at])
  points(1:n, Unadj.Conc, pch = 0, type = "o", lwd = 2)
  points(1:n, Adj.Conc, pch = 3, type = "o", col = 8, lwd = 2)
  abline(h = Three.Yr.Mean, lwd = 2)
  legend("topleft", c("Unadjusted", "Adjusted", "3-Year Mean"), bty = "n",
    pch = c(0, 3, -1), lty = c(1, 1, 1), lwd = 2, col = c(1, 8, 1),
    inset = c(0.05, 0.01))


  # Perform the seasonal Kendall trend test
  #----------------------------------------

  kendallSeasonalTrendTest(Unadj.Conc ~ Month + Year,
    data = EPA.09.Ex.14.8.df)

  #Results of Hypothesis Test
  #--------------------------
  #
  #Null Hypothesis:                 All 12 values of tau = 0
  #
  #Alternative Hypothesis:          The seasonal taus are not all equal
  #                                 (Chi-Square Heterogeneity Test)
  #                                 At least one seasonal tau != 0
  #                                 and all non-zero tau's have the
  #                                 same sign (z Trend Test)
  #
  #Test Name:                       Seasonal Kendall Test for Trend
  #                                 (with continuity correction)
  #
  #Estimated Parameter(s):          tau       =    0.9722222
  #                                 slope     =    0.0600000
  #                                 intercept = -131.7350000
  #
  #Estimation Method:               tau:        Weighted Average of
  #                                             Seasonal Estimates
  #                                 slope:      Hirsch et al.'s
  #                                             Modification of
  #                                             Thiel/Sen Estimator
  #                                 intercept:  Median of
  #                                             Seasonal Estimates
  #
  #Data:                            y      = Unadj.Conc
  #                                 season = Month
  #                                 year   = Year
  #
  #Data Source:                     EPA.09.Ex.14.8.df
  #
  #Sample Sizes:                    January   =  3
  #                                 February  =  3
  #                                 March     =  3
  #                                 April     =  3
  #                                 May       =  3
  #                                 June      =  3
  #                                 July      =  3
  #                                 August    =  3
  #                                 September =  3
  #                                 October   =  3
  #                                 November  =  3
  #                                 December  =  3
  #                                 Total     = 36
  #
  #Test Statistics:                 Chi-Square (Het) = 0.1071882
  #                                 z (Trend)        = 5.1849514
  #
  #Test Statistic Parameter:        df = 11
  #
  #P-values:                        Chi-Square (Het) = 1.000000e+00
  #                                 z (Trend)        = 2.160712e-07
  #
  #Confidence Interval for:         slope
  #
  #Confidence Interval Method:      Gilbert's Modification of
  #                                 Theil/Sen Method
  #
  #Confidence Interval Type:        two-sided
  #
  #Confidence Level:                95%
  #
  #Confidence Interval:             LCL = 0.05786914
  #                                 UCL = 0.07213086

  #==========

  # Clean up
  #---------

  rm(Unadj.Conc, Adj.Conc, Month, Year, Time, n, Three.Yr.Mean, at)
  graphics.off()
}
\keyword{htestEnvStats}
\keyword{nonparametric}
\keyword{regression}
