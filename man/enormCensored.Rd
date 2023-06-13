\name{enormCensored}
\alias{enormCensored}
\title{
  Estimate Parameters for a Normal Distribution Based on Type I Censored Data
}
\description{
  Estimate the mean and standard deviation of a
  \link[stats:Normal]{normal (Gaussian) distribution} given a
  sample of data that has been subjected to Type I censoring, and optionally
  construct a confidence interval for the mean.
}
\usage{
  enormCensored(x, censored, method = "mle", censoring.side = "left",
    ci = FALSE, ci.method = "profile.likelihood", ci.type = "two-sided",
    conf.level = 0.95, n.bootstraps = 1000, pivot.statistic = "z",
    nmc = 1000, seed = NULL, ...)
}
\arguments{
  \item{x}{
  numeric vector of observations.  Missing (\code{NA}), undefined (\code{NaN}), and
  infinite (\code{Inf}, \code{-Inf}) values are allowed but will be removed.
}
  \item{censored}{
  numeric or logical vector indicating which values of \code{x} are censored.
  This must be the same length as \code{x}.  If the mode of \code{censored} is
  \code{"logical"}, \code{TRUE} values correspond to elements of \code{x} that
  are censored, and \code{FALSE} values correspond to elements of \code{x} that
  are not censored.  If the mode of \code{censored} is \code{"numeric"},
  it must contain only \code{1}'s and \code{0}'s; \code{1} corresponds to
  \code{TRUE} and \code{0} corresponds to \code{FALSE}.  Missing (\code{NA})
  values are allowed but will be removed.
}
  \item{method}{
  character string specifying the method of estimation.

  For singly censored data, the possible values are:  \cr
  \code{"mle"} (maximum likelihood; the default), \cr
  \code{"bcmle"} (bias-corrected maximum likelihood), \cr
  \code{"ROS"} or \code{"qq.reg"} (quantile-quantile regression; also called
    \emph{regression on order statistics} and abbreviated ROS), \cr
  \code{"qq.reg.w.cen.level"} (quantile-quantile regression including the
    censoring level), \cr
  \code{"rROS"} or \code{"impute.w.qq.reg"} (moment estimation based on imputation using
    quantile-quantile regression; also called \emph{robust regression on order statistics}
    and abbreviated rROS), \cr
  \code{"impute.w.qq.reg.w.cen.level"} (moment estimation based on imputation
    using the \code{qq.reg.w.cen.level} method), \cr
  \code{"impute.w.mle"} (moment estimation based on imputation using the mle), \cr
  \code{"iterative.impute.w.qq.reg"} (moment estimation based on iterative
    imputation using the \code{qq.reg} method), \cr
  \code{"m.est"} (robust M-estimation), and \cr
  \code{"half.cen.level"} (moment estimation based on setting the censored
    observations to half the censoring level).

  For multiply censored data, the possible values are: \cr
  \code{"mle"} (maximum likelihood; the default), \cr
  \code{"ROS"} or \code{"qq.reg"} (quantile-quantile regression; also called
    \emph{regression on order statistics} and abbreviated ROS), \cr
  \code{"rROS"} or \code{"impute.w.qq.reg"} (moment estimation based on imputation using
    quantile-quantile regression; also called \emph{robust regression on order statistics}
    and abbreviated rROS), and \cr
  \code{"half.cen.level"} (moment estimation based on setting the censored
    observations to half the censoring level).

  See the DETAILS section for more information.
}
  \item{censoring.side}{
  character string indicating on which side the censoring occurs.  The possible
  values are \code{"left"} (the default) and \code{"right"}.
}
  \item{ci}{
  logical scalar indicating whether to compute a confidence interval for the
  mean or variance.  The default value is \code{ci=FALSE}.
}
  \item{ci.method}{
  character string indicating what method to use to construct the confidence interval
  for the mean.  The possible values are: \cr
  \code{"profile.likelihood"} (profile likelihood; the default), \cr
  \code{"normal.approx"} (normal approximation), \cr
  \code{"normal.approx.w.cov"} (normal approximation taking into account the
  covariance between the estimated mean and standard deviation; only available for
  singly censored data), \cr
  \code{"gpq"} (generalized pivotal quantity), and \cr
  \code{"bootstrap"} (based on bootstrapping).

  See the DETAILS section for more information. This argument is ignored if \cr
  \code{ci=FALSE}.
}
  \item{ci.type}{
  character string indicating what kind of confidence interval to compute.  The
  possible values are \code{"two-sided"} (the default), \code{"lower"}, and
  \code{"upper"}.  This argument is ignored if \code{ci=FALSE}.
}
  \item{conf.level}{
  a scalar between 0 and 1 indicating the confidence level of the confidence interval.
  The default value is \code{conf.level=0.95}. This argument is ignored if
  \code{ci=FALSE}.
}
  \item{n.bootstraps}{
  numeric scalar indicating how many bootstraps to use to construct the
  confidence interval for the mean when \code{ci.type="bootstrap"}.  This
  argument is ignored if \code{ci=FALSE} and/or \code{ci.method} does not
  equal \code{"bootstrap"}.
}
  \item{pivot.statistic}{
  character string indicating which pivot statistic to use in the construction
  of the confidence interval for the mean when \code{ci.method="normal.approx"} or
  \code{ci.method="normal.approx.w.cov"} (see the DETAILS section).  The possible
  values are \code{pivot.statistic="z"} (the default) and \code{pivot.statistic="t"}.
  When \code{pivot.statistic="t"} you may supply the argument
  \code{ci.sample size} (see below).  The argument \code{pivot.statistic} is
  ignored if \code{ci=FALSE}.
}
  \item{nmc}{
  numeric scalar indicating the number of Monte Carlo simulations to run when
  \code{ci.method="gpq"}.  The default is \code{nmc=1000}.  This argument is ignored if \cr
  \code{ci=FALSE}.
}
  \item{seed}{
  integer supplied to the function \code{\link[base]{set.seed}} and used when
  \code{ci.method="bootstrap"} or \code{ci.method="gpq"}.  The default value is
  \code{seed=NULL}, in which case the current value of \code{.Random.seed} is used.
  This argument is ignored when \code{ci=FALSE}.
}
  \item{\dots}{
  additional arguments to pass to other functions.
  \itemize{
  \item \code{prob.method}.  Character string indicating what method to use to
    compute the plotting positions (empirical probabilities) when \code{method}
    is one of \code{"ROS"}, \code{"qq.reg"}, \code{"qq.reg.w.cen.level"}, \code{"rROS"},
    \code{"impute.w.qq.reg"}, \code{"impute.w.qq.reg.w.cen.level"}, \code{"impute.w.mle"}, or \cr
    \code{"iterative.impute.w.qq.reg"}.  Possible values are: \cr
    \code{"kaplan-meier"} (product-limit method of Kaplan and Meier (1958)), \cr
    \code{"nelson"} (hazard plotting method of Nelson (1972)), \cr
    \code{"michael-schucany"} (generalization of the product-limit method due to Michael and Schucany (1986)), and \cr
    \code{"hirsch-stedinger"} (generalization of the product-limit method due to Hirsch and Stedinger (1987)).

    The default value is \code{prob.method="hirsch-stedinger"}.  The \code{"nelson"}
    method is only available for \code{censoring.side="right"}.
    See the DETAILS section and the help file for \code{\link{ppointsCensored}}
    for more information.

  \item \code{plot.pos.con}.  Numeric scalar between 0 and 1 containing the
    value of the plotting position constant to use when \code{method} is one of
    \code{"ROS"}, \code{"qq.reg"}, \code{"qq.reg.w.cen.level"},
    \code{"rROS"}, \code{"impute.w.qq.reg"}, \cr
    \code{"impute.w.qq.reg.w.cen.level"}, \code{"impute.w.mle"}, or \cr
    \code{"iterative.impute.w.qq.reg"}.  The default value is \code{plot.pos.con=0.375}.
    See the DETAILS section and the help file for \code{\link{ppointsCensored}}
    for more information.

  \item \code{ci.sample.size}.  Numeric scalar indicating what sample size to
    assume to construct the confidence interval for the mean if
    \code{pivot.statistic="t"} and \code{ci.method="normal.approx"} or
    \code{ci.method="normal.approx.w.cov"}.  When \code{method} equals
    \code{"mle"} or \code{"bcmle"}, the default value is the expected number of
    uncensored observations, otherwise it is the observed number of
    uncensored observations.

  \item \code{lb.impute}.  Numeric scalar indicating the lower bound for imputed
    observations when method is one of \code{"rROS"}, \code{"impute.w.qq.reg"}, \cr
    \code{"impute.w.qq.reg.w.cen.level"}, \code{"impute.w.mle"}, or \cr
    \code{"iterative.impute.w.qq.reg"}.  Imputed values smaller than this
    value will be set to this value.  The default is \code{lb.impute=-Inf}.

  \item \code{ub.impute}.  Numeric scalar indicating the upper bound for imputed
    observations when method is one of \code{"rROS"}, \code{"impute.w.qq.reg"}, \cr
    \code{"impute.w.qq.reg.w.cen.level"}, \code{"impute.w.mle"}, or \cr
    \code{"iterative.impute.w.qq.reg"}.  Imputed values larger than this value
    will be set to this value.  The default is \code{ub.impute=Inf}.

  \item \code{convergence}.  Character string indicating the kind of convergence
    criterion when \code{method="iterative.impute.w.qq.reg"}.  The possible values
    are \code{"relative"} (the default) and \code{"absolute"}.  See the DETAILS
    section for more information.

  \item \code{tol}.  Numeric scalar indicating the convergence tolerance when \cr
    \code{method="iterative.impute.w.qq.reg"}.  The default value is \code{tol=1e-6}.
    If \code{convergence="relative"}, then the relative difference in the old and
    new estimates of the mean and the relative difference in the old and new estimates
    of the standard deviation must be less than \code{tol} for convergence to be
    achieved.  If \code{convergence="absolute"}, then the absolute difference in the
    old and new estimates of the mean and the absolute difference in the old and new
    estimates of the standard deviation must be less than \code{tol} for convergence
    to be achieved.

  \item \code{max.iter}.  Numeric scalar indicating the maximum number of iterations
    when \code{method="iterative.impute.w.qq.reg"}.

  \item \code{t.df}.  Numeric scalar greater than or equal to 1 that determines the
    robustness and efficiency properties of the estimator when \code{method="m.est"}.
    The default value is \code{t.df=3}.
  }
}
}
\details{
  If \code{x} or \code{censored} contain any missing (\code{NA}), undefined (\code{NaN}) or
  infinite (\code{Inf}, \code{-Inf}) values, they will be removed prior to
  performing the estimation.

  Let \eqn{\underline{x}} denote a vector of \eqn{N} observations from a
  \link[stats:Normal]{normal distribution} with mean \eqn{\mu} and
  standard deviation \eqn{\sigma}.  Assume \eqn{n} (\eqn{0 < n < N}) of these
  observations are known and \eqn{c} (\eqn{c=N-n}) of these observations are
  all censored below (left-censored) or all censored above (right-censored) at
  \eqn{k} fixed censoring levels
  \deqn{T_1, T_2, \ldots, T_k; \; k \ge 1 \;\;\;\;\;\; (1)}
  For the case when \eqn{k \ge 2}, the data are said to be Type I
  \bold{\emph{multiply censored}}.  For the case when \eqn{k=1},
  set \eqn{T = T_1}.  If the data are left-censored
  and all \eqn{n} known observations are greater
  than or equal to \eqn{T}, or if the data are right-censored and all \eqn{n}
  known observations are less than or equal to \eqn{T}, then the data are
  said to be Type I \bold{\emph{singly censored}} (Nelson, 1982, p.7), otherwise
  they are considered to be Type I multiply censored.

  Let \eqn{c_j} denote the number of observations censored below or above censoring
  level \eqn{T_j} for \eqn{j = 1, 2, \ldots, k}, so that
  \deqn{\sum_{i=1}^k c_j = c \;\;\;\;\;\; (2)}
  Let \eqn{x_{(1)}, x_{(2)}, \ldots, x_{(N)}} denote the \dQuote{ordered} observations,
  where now \dQuote{observation} means either the actual observation (for uncensored
  observations) or the censoring level (for censored observations).  For
  right-censored data, if a censored observation has the same value as an
  uncensored one, the uncensored observation should be placed first.
  For left-censored data, if a censored observation has the same value as an
  uncensored one, the censored observation should be placed first.

  Note that in this case the quantity \eqn{x_{(i)}} does not necessarily represent
  the \eqn{i}'th \dQuote{largest} observation from the (unknown) complete sample.

  Finally, let \eqn{\Omega} (omega) denote the set of \eqn{n} subscripts in the
  \dQuote{ordered} sample that correspond to uncensored observations.
  \cr

  \bold{ESTIMATION} \cr

  \bold{\emph{Estimation Methods for Multiply and Singly Censored Data}} \cr
  The following methods are available for multiply and singly censored data.

  \emph{Maximum Likelihood Estimation} (\code{method="mle"}) \cr
  For Type I left censored data, the likelihood function is given by:
  \deqn{L(\mu, \sigma | \underline{x}) = {N \choose c_1 c_2 \ldots c_k n} \prod_{j=1}^k [F(T_j)]^{c_j} \prod_{i \in \Omega} f[x_{(i)}] \;\;\;\;\;\; (3)}
  where \eqn{f} and \eqn{F} denote the probability density function (pdf) and
  cumulative distribution function (cdf) of the population. That is,
  \deqn{f(t) = \phi(\frac{t-\mu}{\sigma}) \;\;\;\;\;\; (4)}
  \deqn{F(t) = \Phi(\frac{t-\mu}{\sigma}) \;\;\;\;\;\; (5)}
  where \eqn{\phi} and \eqn{\Phi} denote the pdf and cdf of the standard normal
  distribution, respectively (Cohen, 1963; 1991, pp.6, 50).  For left singly
  censored data, Equation (3) simplifies to:
  \deqn{L(\mu, \sigma | \underline{x}) = {N \choose c} [F(T)]^{c} \prod_{i = c+1}^n f[x_{(i)}] \;\;\;\;\;\; (6)}

  Similarly, for Type I right censored data, the likelihood function is given by:
  \deqn{L(\mu, \sigma | \underline{x}) = {N \choose c_1 c_2 \ldots c_k n} \prod_{j=1}^k [1 - F(T_j)]^{c_j} \prod_{i \in \Omega} f[x_{(i)}] \;\;\;\;\;\; (7)}
  and for right singly censored data this simplifies to:
  \deqn{L(\mu, \sigma | \underline{x}) = {N \choose c} [1 - F(T)]^{c} \prod_{i = 1}^n f[x_{(i)}] \;\;\;\;\;\; (8)}

  The maximum likelihood estimators are computed by maximizing the likelihood
  function.  For right-censored data, Cohen (1963; 1991, pp.50-51) shows that
  taking partial derivatives of the log-likelihood function with respect to
  \eqn{\mu} and \eqn{\sigma} and setting these to 0 produces the following two
  simultaneous equations:
  \deqn{\bar{x} - \mu = -\sigma \sum_{i=1}^k (\frac{c_j}{n}) Q_j \;\;\;\;\;\; (9)}
  \deqn{s^2 + (\bar{x} - \mu)^2 = \sigma^2[1 - \sum_{j=1}^k \zeta_j (\frac{c_j}{n}) Q_j] \;\;\;\;\;\; (10)}
  where
  \deqn{\bar{x} = \frac{1}{n} \sum_{i \in \Omega} x_{(i)} \;\;\;\;\;\; (11)}
  \deqn{s^2 = \frac{1}{n} \sum_{i \in \Omega} (x_{(i)} - \bar{x})^2 \;\;\;\;\;\; (12)}
  \deqn{Q_j = Q(\zeta_j) \;\;\;\;\;\; (13)}
  \deqn{\zeta_j = \frac{T_j - \mu}{\sigma} \;\;\;\;\;\; (14)}
  \deqn{Q(t) = \frac{\phi(t)}{1 - \Phi(t)} \;\;\;\;\;\; (15)}
  Note that the quantity defined in Equation (11) is simply the mean of the uncensored
  observations, the quantity defined in Equation (12) is simply the method of
  moments estimator of variance based on the uncensored observations, and the
  function \eqn{Q()} defined in Equation (15) is the hazard function for the
  standard normal distribution.

  For left-censored data, Equations (9) and (10) stay the same, except \eqn{\zeta} is
  replaced with \eqn{-\zeta}.

  The function \code{enormCensored} computes the maximum likelihood estimators by
  solving Equations (9) and (10) and uses the quantile-quantile regression
  estimators (see below) as initial values.
  \cr

  \emph{Regression on Order Statistics} (\code{method="ROS"}) or \cr
  \emph{Quantile-Quantile Regression} (\code{method="qq.reg"}) \cr
  This method is sometimes called the \emph{probability plot method}
  (Nelson, 1982, Chapter 3; Gilbert, 1987, pp.134-136;
  Helsel and Hirsch, 1992, p. 361), and more recently also called
  \emph{parametric regression on order statistics} or \emph{ROS}
  (USEPA, 2009; Helsel, 2012).  In the case of no censoring, it is well known
  (e.g., Nelson, 1982, p.113; Cleveland, 1993, p.31) that for the standard
  normal (Gaussian) quantile-quantile plot (i.e., the plot of the sorted observations
  (empirical quantiles) versus standard normal quantiles; see \code{\link{qqPlot}}),
  the intercept and slope of the fitted least-squares line estimate the mean and
  standard deviation, respectively.  Specifically, the estimates of \eqn{\mu} and
  \eqn{\sigma} are found by computing the least-squares estimates in the following
  model:
  \deqn{x_{(i)} = \mu + \sigma \Phi^{-1}(p_i) + \epsilon_i, \; i = 1, 2, \ldots, N \;\;\;\;\;\; (16)}
  where
  \deqn{p_i = \frac{i-a}{N - 2a + 1} \;\;\;\;\;\; (17)}
  denotes the plotting position associated with the \eqn{i}'th largest value,
  \eqn{a} is a constant such that \eqn{0 \le a \le 1} (the plotting position constant),
  and \eqn{\Phi} denotes the cumulative distribution function (cdf) of the standard
  normal distribution.  The default value of \eqn{a} is 0.375 (see below).

  This method can be adapted to the case of left (right) singly censored data
  as follows.  Plot the \eqn{n} uncensored observations against the \eqn{n}
  largest (smallest) normal quantiles, where the normal quantiles are computed
  based on a sample size of \eqn{N}, fit the least-squares line to this plot, and
  estimate the mean and standard deviation from the intercept and slope, respectively.
  That is, use Equations (16) and (17), but for right singly censored data use
  \eqn{i = 1, 2, \ldots, n}, and for left singly censored data use
  \eqn{i = (c+1), (c+2), \ldots,  N}.

  The argument \code{plot.pos.con} (see the entry for \dots in the ARGUMENTS
  section above) determines the value of the plotting positions computed in
  Equation (18).  The default value is \code{plot.pos.con=0.375}.
  See the help file for \code{\link{qqPlot}} for more information.

  This method is discussed by Haas and Scheff (1990).  In the context of
  lognormal data, Travis and Land (1990) suggest exponentiating the
  predicted 50'th percentile from this fit to estimate the geometric mean
  (i.e., the median of the lognormal distribution).

  This method is easily extended to multiply censored data.  Equation (16) becomes
  \deqn{x_{(i)} = \mu + \sigma \Phi^{-1}(p_i) + \epsilon_i, \; i \in \Omega \;\;\;\;\;\; (18)}
  where \eqn{\Omega} denotes the set of \eqn{n} subscripts associated with the
  uncensored observations in the ordered sample.  The plotting positions are
  computed by calling the \pkg{EnvStats} function \code{\link{ppointsCensored}}.
  The argument \code{prob.method} determines the method of computing the plotting
  positions (default is \code{prob.method="hirsch-stedinger"}), and the argument
  \code{plot.pos.con} determines the plotting position constant (default is
  \code{plot.pos.con=0.375}).  (See the entry for \dots in the ARGUMENTS section above.)
  Both Helsel (2012) and USEPA (2009) also use the Hirsch-Stedinger probability
  method but set the plotting position constant to 0.
  \cr

  \emph{Robust Regression on Order Statistics} (\code{method="rROS"}) or \cr
  \emph{Imputation Using Quantile-Quantile Regression} (\code{method="impute.w.qq.reg"}) \cr
  This is the robust Regression on Order Statistics (rROS) method discussed in
  USEPA (2009) and Helsel (2012).  It involves using the quantile-quantile regression
  method (\code{method="qq.reg"} or \code{method="ROS"}) to fit a regression line
  (and thus initially estimate the mean and standard deviation), and then imputing the
  values of the censored observations by predicting them from the regression equation.
  The final estimates of the mean and standard deviation are then computed using
  the usual formulas (see \code{\link{enorm}}) based on the observed and imputed
  values.

  The imputed values are computed as:
  \deqn{\hat{x}_{(i)} = \hat{\mu}_{qqreg} + \hat{\sigma}_{qqreg} \Phi^{-1}(p_i), \; i \not \in \Omega \;\;\;\;\;\; (19)}
  See the help file for \code{\link{ppointsCensored}} for information on how the
  plotting positions for the censored observations are computed.

  The argument \code{prob.method} determines the method of computing the plotting
  positions (default is \code{prob.method="hirsch-stedinger"}), and the argument
  \code{plot.pos.con} determines the plotting position constant (default is
  \code{plot.pos.con=0.375}).  (See the entry for \dots in the ARGUMENTS section above.)
  Both Helsel (2012) and USEPA (2009) also use the Hirsch-Stedinger probability
  method but set the plotting position constant to 0.

  The arguments \code{lb.impute} and \code{ub.impute} determine the lower and upper
  bounds for the imputed values.  Imputed values smaller than \code{lb.impute} are
  set to this value.  Imputed values larger than \code{ub.impute} are set to this
  value.  The default values are \code{lb.impute=-Inf} and \code{ub.impute=Inf}.
  See the entry for \dots in the ARGUMENTS section above.

  For singly censored data, this is the NR method of Gilliom and Helsel
  (1986, p. 137).  In the context of lognormal data, this method is discussed by
  Hashimoto and Trussell (1983), Gilliom and Helsel (1986), and El-Shaarawi (1989),
  and is referred to as the LR or Log-Probability Method.

  For multiply censored data, this method was developed in the context of
  lognormal data by Helsel and Cohn (1988) using the formulas for plotting
  positions given in Hirsch and Stedinger (1987) and Weibull plotting positions
  (i.e., \code{prob.method="hirsch-stedinger"} and \code{plot.pos.con=0}).
  \cr

  \emph{Setting Censored Observations to Half the Censoring Level} (\code{method="half.cen.level"}) \cr
  This method is applicable only to left censored data that is bounded below by 0.
  This method involves simply replacing all the censored observations with half their
  detection limit, and then computing the mean and standard deviation with the usual
  formulas (see \code{\link{enorm}}).

  This method is included only to allow comparison of this method to other methods.
  \bold{\emph{Setting left-censored observations to half the censoring level is not
  recommended}}.

  For singly censored data, this method is discussed by Gleit (1985),
  Haas and Scheff (1990), and El-Shaarawi and Esterby (1992).
  El-Shaarawi and Esterby (1992) show that these estimators are biased and
  inconsistent (i.e., the bias remains even as the sample size increases).

  For multiply censored data, this method was studied by Helsel and Cohn
  (1988).
  \cr

  \bold{\emph{Estimation Methods for Singly Censored Data}} \cr
  The following methods are available only for singly censored data.

  \emph{Bias-Corrected Maximum Likelihood Estimation} (\code{method="bcmle"}) \cr
  The maximum likelihood estimates of \eqn{\mu} and \eqn{\sigma} are biased.
  The bias tends to 0 as the sample size increases, but it can be considerable
  for small sample sizes, especially in the case of a large percentage of
  censored observations (Saw, 1961b).  Schmee et al. (1985) note that bias and
  variances of the mle's are of the order \eqn{1/N} (see for example,
  Bain and Engelhardt, 1991), and that for 90\% censoring the bias is negligible
  if \eqn{N} is at least 100.  (For less intense censoring, even fewer
  observations are needed.)

  The exact bias of each estimator is extremely difficult to compute.
  Saw (1961b), however, derived the first-order term (i.e., the term of order
  \eqn{1/N}) in the bias of the mle's of \eqn{\mu} and \eqn{\sigma} and
  proposed bias-corrected mle's.  His bias-corrected estimators were derived for
  the case of Type II singly censored data.  Schneider (1986, p.110) and
  Haas and Scheff (1990), however, state that this bias correction should
  reduce the bias of the estimators in the case of Type I censoring as well.

  Based on the tables of bias-correction terms given in Saw (1961b),
  Schneider (1986, pp.107-110) performed a least-squares fit to produce the
  following computational formulas for right-censored data:
  \deqn{B_{\mu} = -exp[2.692 - 5.493 \frac{n}{N+1}] \;\;\;\;\;\; (20)}
  \deqn{B_{\sigma} = -[0.312 + 0.859 \frac{n}{N+1}]^{-2} \;\;\;\;\;\; (21)}
  \deqn{\hat{\mu}_{bcmle} = \hat{\mu}_{mle} - \frac{\hat{\sigma}_{mle}}{N+1} B_{\mu} \;\;\;\;\;\; (22)}
  \deqn{\hat{\sigma}_{bcmle} = \hat{\sigma}_{mle} - \frac{\hat{\sigma}_{mle}}{N+1} B_{\sigma} \;\;\;\;\;\; (23)}

  For left-censored data, Equation (22) becomes:
  \deqn{\hat{\mu}_{bcmle} = \hat{\mu}_{mle} + \frac{\hat{\sigma}_{mle}}{N+1} B_{\mu} \;\;\;\;\;\; (22)}
  \cr

  \emph{Quantile-Quantile Regression Including the Censoring Level} (\code{method="qq.reg.w.cen.level"}) \cr
  This is a modification of the quantile-quantile regression method and was proposed
  by El-Shaarawi (1989) in the context of lognormal data.  El-Shaarawi's idea is to
  include the censoring level and an associated plotting position, along with the
  uncensored observations and their associated plotting positions, in order to include
  information about the value of the censoring level \eqn{T}.

  For left singly censored data, the modification involves adding the point
  \eqn{[\Phi^{-1}(p_c), T]} to the plot before fitting the least-squares line.
  For right singly censored data, the point  \eqn{[\Phi^{-1}(p_{n+1}), T]}
  is added to the plot before fitting the least-squares line.

  El-Shaarawi (1989) also proposed replacing the estimated normal quantiles with the
  exact expected values of normal order statistics, and using the values in their
  variance-covariance matrix to perform a weighted least least-squared regression.
  These last two modifications are not incorporated here.
  \cr

  \emph{Imputation Using Quantile-Quantile Regression Including the Censoring Level} \cr
  (\code{method ="impute.w.qq.reg.w.cen.level"}) \cr
  This is exactly the same method as imputation using quantile-quantile regression \cr
  (\code{method="impute.w.qq.reg"}), except that the quantile-quantile regression
  including the censoring level method (\code{method="qq.reg.w.cen.level"}) is used
  to fit the regression line.  In the context of lognormal data, this method is
  discussed by El-Shaarawi (1989), which he denotes as the Modified LR Method.
  \cr

  \emph{Imputation Using Maximum Likelihood} (\code{method ="impute.w.mle"}) \cr
  This is exactly the same method as imputation with quantile-quantile regression \cr
  (\code{method="impute.w.qq.reg"}), except that the maximum likelihood method
  (\code{method="mle"}) is used to compute the initial estimates of the mean and
  standard deviation.  In the context of lognormal data, this method is discussed
  by El-Shaarawi (1989), which he denotes as the Modified Maximum Likelihood Method.
  \cr

  \emph{Iterative Imputation Using Quantile-Quantile Regression} (\code{method="iterative.impute.w.qq.reg"}) \cr
  This method is similar to the imputation with quantile-quantile regression method \cr
  (\code{method="impute.w.qq.reg"}), but iterates until the estimates of the mean
  and standard deviation converge.  The algorithm is:
  \enumerate{
    \item Compute the initial estimates of \eqn{\mu} and \eqn{\sigma} using the
      \code{"impute.w.qq.reg"} method. (Actually, any suitable estimates will do.)
    \item Using the current values of \eqn{\mu} and \eqn{\sigma} and Equation (19),
      compute new imputed values of the censored observations.
    \item Use the new imputed values along with the uncensored observations to
      compute new estimates of \eqn{\mu} and \eqn{\sigma} based on the usual
      formulas (see \code{\link{enorm}}).
    \item Repeat Steps 2 and 3 until the estimates converge (the convergence
      criterion is determined by the arguments \code{tol} and \code{convergence};
      see the entry for \dots in the ARGUMENTS section above).
  }

  This method is discussed by Gleit (1985), which he denotes as
  \dQuote{Fill-In with Expected Values}.
  \cr

  \emph{M-Estimators} (\code{method="m.est"}) \cr
  This method was contributed by Leo R. Korn (Korn and Tyler, 2001).
  This method finds location and scale estimates that are consistent at the
  normal model and robust to deviations from the normal model, including both
  outliers on the right and outliers on the left above and below the limit of
  detection.  The estimates are found by solving the simultaneous equations:
  \deqn{\sum_{i=1}^c h_{\nu} (\frac{T-\mu}{\sigma}) + \sum_{i=c+1}^N \psi_{\nu} (\frac{x_i - \mu}{\sigma}) = 0 \;\;\;\;\;\; (23)}
  \deqn{\sum_{i=1}^c \lambda_{\nu} (\frac{T-\mu}{\sigma}) + \sum_{i=c+1}^N \chi_{\nu} (\frac{x_i - \mu}{\sigma}) = 0 \;\;\;\;\;\; (24)}
  where
  \deqn{H_{\nu}(r) = -log[F_{\nu}(r)] \;\;\;\;\;\; (25)}
  \deqn{h_{\nu}(r) = \frac{d}{dr} H_{\nu}(r) = H'_{\nu}(r) \;\;\;\;\;\; (26)}
  \deqn{\rho_{\nu}(r) = -log[f_{\nu}(r)] \;\;\;\;\;\; (27)}
  \deqn{\psi_{\nu}(r) = \frac{d}{dr} \rho_{\nu}(r) = \rho'_{\nu}(r) \;\;\;\;\;\; (28)}
  \deqn{\lambda_{\nu}(r) = r h_{\nu}(r) \;\;\;\;\;\; (29)}
  \deqn{\chi_{\nu}(r) = r \psi_{\nu}(r) - 1 \;\;\;\;\;\; (30)}
  and \eqn{f_{\nu}} and \eqn{F_{\nu}} denote the probability density function
  (pdf) and cumulative distribution function (cdf) of
  \link[stats:TDist]{Student's t-distribution} with \eqn{\nu} degrees of freedom.

  This results in an M-estimating equation based on the t-density function
  (Korn and Tyler., 2001).  Since the t-density has heavier tails than the normal
  density, this M-estimator will tend to down-weight values that are far away from
  the center of the data.  When censoring is present, neither the location nor the
  scale estimates are consistent at the normal model.  A computational correction
  is performed that converts the above M-estimator to another M-estimator that is
  consistent at the normal model, even under censoring.

  The degrees of freedom parameter \eqn{\nu} is set by the argument \code{t.df} and
  may be viewed as a tuning parameter that will determine the robustness and
  efficiency properties.  When \code{t.df} is large, the estimator is similar to
  the usual mle and the output will then be very close to that when \code{method="mle"}.
  As \code{t.df} decreases, the efficiency will decline and the outlier rejection
  property will increase in strength.  Choosing \code{t.df=3} (the default) provides
  a good combination of efficiency and robustness.  A reasonable strategy is to
  transform the data so that they are approximately symmetric (often the log
  transformation for environmental data is appropriate) and then apply the
  M-estimator using \code{t.df=3}.
  \cr \cr

  \bold{CONFIDENCE INTERVALS} \cr
  This section explains how confidence intervals for the mean \eqn{\mu} are
  computed.
  \cr

  \bold{\emph{Likelihood Profile}} (\code{ci.method="profile.likelihood"}) \cr
  This method was proposed by Cox (1970, p.88), and Venzon and Moolgavkar (1988)
  introduced an efficient method of computation.  This method is also discussed by
  Stryhn and Christensen (2003) and Royston (2007).
  The idea behind this method is to invert the likelihood-ratio test to obtain a
  confidence interval for the mean \eqn{\mu} while treating the standard deviation
  \eqn{\sigma} as a nuisance parameter.  Equation (3) above
  shows the form of the likelihood function \eqn{L(\mu, \sigma | \underline{x})} for
  multiply left-censored data, and Equation (7) shows the function for
  multiply right-censored data.

  Following Stryhn and Christensen (2003), denote the maximum likelihood estimates
  of the mean and standard deviation by \eqn{(\mu^*, \sigma^*)}.  The likelihood
  ratio test statistic (\eqn{G^2}) of the hypothesis \eqn{H_0: \mu = \mu_0}
  (where \eqn{\mu_0} is a fixed value) equals the drop in \eqn{2 log(L)} between the
  \dQuote{full} model and the reduced model with \eqn{\mu} fixed at \eqn{\mu_0}, i.e.,
  \deqn{G^2 = 2 \{log[L(\mu^*, \sigma^*)] - log[L(\mu_0, \sigma_0^*)]\} \;\;\;\;\;\; (30)}
  where \eqn{\sigma_0^*} is the maximum likelihood estimate of \eqn{\sigma} for the
  reduced model (i.e., when \eqn{\mu = \mu_0}).  Under the null hypothesis,
  the test statistic \eqn{G^2} follows a
  \link[stats:Chisquare]{chi-squared distribution} with 1 degree of freedom.

  Alternatively, we may
  express the test statistic in terms of the profile likelihood function \eqn{L_1}
  for the mean \eqn{\mu}, which is obtained from the usual likelihood function by
  maximizing over the parameter \eqn{\sigma}, i.e.,
  \deqn{L_1(\mu) = max_{\sigma} L(\mu, \sigma) \;\;\;\;\;\; (31)}
  Then we have
  \deqn{G^2 = 2 \{log[L_1(\mu^*)] - log[L_1(\mu_0)]\} \;\;\;\;\;\; (32)}
  A two-sided \eqn{(1-\alpha)100\%} confidence interval for the mean \eqn{\mu}
  consists of all values of \eqn{\mu_0} for which the test is not significant at
  level \eqn{alpha}:
  \deqn{\mu_0: G^2 \le \chi^2_{1, {1-\alpha}} \;\;\;\;\;\; (33)}
  where \eqn{\chi^2_{\nu, p}} denotes the \eqn{p}'th quantile of the
  \link[stats:Chisquare]{chi-squared distribution} with \eqn{\nu} degrees of freedom.
  One-sided lower and one-sided upper confidence intervals are computed in a similar
  fashion, except that the quantity \eqn{1-\alpha} in Equation (33) is replaced with
  \eqn{1-2\alpha}.
  \cr

  \bold{\emph{Normal Approximation}} (\code{ci.method="normal.approx"}) \cr
  This method constructs approximate \eqn{(1-\alpha)100\%} confidence intervals for
  \eqn{\mu} based on the assumption that the estimator of \eqn{\mu} is
  approximately normally distributed.  That is, a two-sided \eqn{(1-\alpha)100\%}
  confidence interval for \eqn{\mu} is constructed as:
  \deqn{[\hat{\mu} - t_{1-\alpha/2, m-1}\hat{\sigma}_{\hat{\mu}}, \; \hat{\mu} + t_{1-\alpha/2, m-1}\hat{\sigma}_{\hat{\mu}}] \;\;\;\; (34)}
  where \eqn{\hat{\mu}} denotes the estimate of \eqn{\mu},
  \eqn{\hat{\sigma}_{\hat{\mu}}} denotes the estimated asymptotic standard
  deviation of the estimator of \eqn{\mu}, \eqn{m} denotes the assumed sample
  size for the confidence interval, and \eqn{t_{p,\nu}} denotes the \eqn{p}'th
  quantile of \link[stats:TDist]{Student's t-distribuiton} with \eqn{\nu}
  degrees of freedom.  One-sided confidence intervals are computed in a
  similar fashion.

  The argument \code{ci.sample.size} determines the value of \eqn{m} (see
  see the entry for \dots in the ARGUMENTS section above).
  When \code{method} equals
  \code{"mle"} or \code{"bcmle"}, the default value is the expected number of
  uncensored observations, otherwise it is the observed number of
  uncensored observations.  This is simply an ad-hoc method of constructing
  confidence intervals and is not based on any published theoretical results.

  When \code{pivot.statistic="z"}, the \eqn{p}'th quantile from the
  \link[stats:Normal]{standard normal distribution} is used in place of the
  \eqn{p}'th quantile from Student's t-distribution.

  \emph{Approximate Confidence Interval Based on Maximum Likelihood Estimators} \cr
  When \code{method="mle"}, the standard deviation of the mle of \eqn{\mu} is
  estimated based on the inverse of the Fisher Information matrix.  The estimated
  variance-covariance matrix for the estimates of \eqn{\mu} and \eqn{\sigma}
  are based on the observed information matrix, formulas for which are given in
  Cohen (1991).

  \emph{Approximate Confidence Interval Based on Bias-Corrected Maximum Likelihood Estimators} \cr
  When \code{method="bcmle"} (available only for singly censored data),
  the same procedures are used to construct the
  confidence interval as for \code{method="mle"}.  The true variance of the
  bias-corrected mle of \eqn{\mu} is necessarily larger than the variance of the
  mle of \eqn{\mu} (although the differences in the variances goes to 0 as the
  sample size gets large).  Hence this method of constructing a confidence interval
  leads to intervals that are too short for small sample sizes, but these intervals
  should be better centered about the true value of \eqn{\mu}.

  \emph{Approximate Confidence Interval Based on Other Estimators} \cr
  When method is some value other than \code{"mle"}, the standard deviation of the
  estimated mean is approximated by
  \deqn{\hat{\sigma}_{\hat{\mu}} = \frac{\hat{\sigma}}{\sqrt{m}} \;\;\;\;\;\; (35)}
  where, as already noted, \eqn{m} denotes the assumed sample size.  This is simply
  an ad-hoc method of constructing confidence intervals and is not based on any
  published theoretical results.
  \cr

  \bold{\emph{Normal Approximation Using Covariance}} (\code{ci.method="normal.approx.w.cov"})
  This method is only available for singly censored data and only applicable when
  \code{method="mle"} or \code{method="bcmle"}.  It was proposed by Schneider
  (1986, pp. 191-193) for the case of Type II censoring, but is applicable to any
  situation where the estimated mean and standard deviation are consistent estimators
  and are correlated.  In particular, the mle's of \eqn{\mu} and \eqn{\sigma} are
  correlated under Type I censoring as well.

  Schneider's idea is to determine two positive quantities \eqn{z_1, z_2} such that
  \deqn{Pr(\hat{\mu} + z_1\hat{\sigma} < \mu) = \frac{\alpha}{2} \;\;\;\;\;\; (36)}
  \deqn{Pr(\hat{\mu} - z_2\hat{\sigma} > \mu) = \frac{\alpha}{2} \;\;\;\;\;\; (37)}
  so that
  \deqn{[\hat{\mu} - z_2\hat{\sigma}, \; \hat{\mu} + z_1\hat{\sigma}] \;\;\;\;\;\; (38)}
  is a \eqn{(1-\alpha)100\%} confidence interval for \eqn{\mu}.

  For cases where the estimators of \eqn{\mu} and \eqn{\sigma} are independent
  (e.g., complete samples), it is well known that setting
  \deqn{z_1 = z_2 = \frac{t_{1-\alpha/2, N}}{\sqrt{N}} \;\;\;\;\;\; (39)}
  yields an exact confidence interval and setting
  \deqn{z_1 = z_2 = \frac{z_{1-\alpha/2}}{\sqrt{N}} \;\;\;\;\;\; (40)}
  where \eqn{z_p} denotes the \eqn{p}'th quantile of the standard normal distribution
  yields an approximate confidence interval that is asymptotically correct.

  For the general case, Schneider (1986) considers the random variable
  \deqn{W(z) = \hat{\mu} + z\hat{\sigma} \;\;\;\;\;\; (41)}
  and provides formulas for \eqn{z_1} and \eqn{z_2}.

  Note that the resulting confidence interval for the mean is not symmetric about
  the estimated mean.  Also note that the quantity \eqn{m} is a random variable for
  Type I censoring, while Schneider (1986) assumed it to be fixed since he derived
  the result for Type II censoring (in which case \eqn{m=n}).
  \cr

  \bold{\emph{Bootstrap and Bias-Corrected Bootstrap Approximation}} (\code{ci.method="bootstrap"}) \cr
  The bootstrap is a nonparametric method of estimating the distribution
  (and associated distribution parameters and quantiles) of a sample statistic,
  regardless of the distribution of the population from which the sample was drawn.
  The bootstrap was introduced by Efron (1979) and a general reference is
  Efron and Tibshirani (1993).

  In the context of deriving an approximate \eqn{(1-\alpha)100\%} confidence interval
  for the population mean \eqn{\mu}, the bootstrap can be broken down into the
  following steps:
  \enumerate{
    \item Create a bootstrap sample by taking a random sample of size \eqn{N} from
      the observations in \eqn{\underline{x}}, where sampling is done with
      replacement.  Note that because sampling is done with replacement, the same
      element of \eqn{\underline{x}} can appear more than once in the bootstrap
      sample.  Thus, the bootstrap sample will usually not look exactly like the
      original sample (e.g., the number of censored observations in the bootstrap
      sample will often differ from the number of censored observations in the
      original sample).

    \item Estimate \eqn{\mu} based on the bootstrap sample created in Step 1, using
      the same method that was used to estimate \eqn{\mu} using the original
      observations in \eqn{\underline{x}}. Because the bootstrap sample usually
      does not match the original sample, the estimate of \eqn{\mu} based on the
      bootstrap sample will usually differ from the original estimate based on
      \eqn{\underline{x}}.

    \item Repeat Steps 1 and 2 \eqn{B} times, where \eqn{B} is some large number.
      For the function \code{enormCensored}, the number of bootstraps \eqn{B} is
      determined by the argument \code{n.bootstraps} (see the section ARGUMENTS above).
      The default value of \code{n.bootstraps} is \code{1000}.

    \item Use the \eqn{B} estimated values of \eqn{\mu} to compute the empirical
      cumulative distribution function of this estimator of \eqn{\mu} (see
      \code{\link{ecdfPlot}}), and then create a confidence interval for \eqn{\mu}
      based on this estimated cdf.
  }

  The two-sided percentile interval (Efron and Tibshirani, 1993, p.170) is computed as:
  \deqn{[\hat{G}^{-1}(\frac{\alpha}{2}), \; \hat{G}^{-1}(1-\frac{\alpha}{2})] \;\;\;\;\;\; (42)}
  where \eqn{\hat{G}(t)} denotes the empirical cdf evaluated at \eqn{t} and thus
  \eqn{\hat{G}^{-1}(p)} denotes the \eqn{p}'th empirical quantile, that is,
  the \eqn{p}'th quantile associated with the empirical cdf.  Similarly, a one-sided lower
  confidence interval is computed as:
  \deqn{[\hat{G}^{-1}(\alpha), \; \infty] \;\;\;\;\;\; (43)}
  and a one-sided upper confidence interval is computed as:
  \deqn{[-\infty, \; \hat{G}^{-1}(1-\alpha)] \;\;\;\;\;\; (44)}
  The function \code{enormCensored} calls the \R function \code{\link[stats]{quantile}}
  to compute the empirical quantiles used in Equations (42)-(44).

  The percentile method bootstrap confidence interval is only first-order
  accurate (Efron and Tibshirani, 1993, pp.187-188), meaning that the probability
  that the confidence interval will contain the true value of \eqn{\mu} can be
  off by \eqn{k/\sqrt{N}}, where \eqn{k} is some constant.  Efron and Tibshirani
  (1993, pp.184-188) proposed a bias-corrected and accelerated interval that is
  second-order accurate, meaning that the probability that the confidence interval
  will contain the true value of \eqn{\mu} may be off by \eqn{k/N} instead of
  \eqn{k/\sqrt{N}}.  The two-sided bias-corrected and accelerated confidence interval is
  computed as:
  \deqn{[\hat{G}^{-1}(\alpha_1), \; \hat{G}^{-1}(\alpha_2)] \;\;\;\;\;\; (45)}
  where
  \deqn{\alpha_1 = \Phi[\hat{z}_0 + \frac{\hat{z}_0 + z_{\alpha/2}}{1 - \hat{a}(z_0 + z_{\alpha/2})}] \;\;\;\;\;\; (46)}
  \deqn{\alpha_2 = \Phi[\hat{z}_0 + \frac{\hat{z}_0 + z_{1-\alpha/2}}{1 - \hat{a}(z_0 + z_{1-\alpha/2})}] \;\;\;\;\;\; (47)}
  \deqn{\hat{z}_0 = \Phi^{-1}[\hat{G}(\hat{\mu})] \;\;\;\;\;\; (48)}
  \deqn{\hat{a} = \frac{\sum_{i=1}^N (\hat{\mu}_{(\cdot)} - \hat{\mu}_{(i)})^3}{6[\sum_{i=1}^N (\hat{\mu}_{(\cdot)} - \hat{\mu}_{(i)})^2]^{3/2}} \;\;\;\;\;\; (49)}
  where the quantity \eqn{\hat{\mu}_{(i)}} denotes the estimate of \eqn{\mu} using
  all the values in \eqn{\underline{x}} except the \eqn{i}'th one, and
  \deqn{\hat{\mu}{(\cdot)} = \frac{1}{N} \sum_{i=1}^N \hat{\mu_{(i)}} \;\;\;\;\;\; (50)}
  A one-sided lower confidence interval is given by:
  \deqn{[\hat{G}^{-1}(\alpha_1), \; \infty] \;\;\;\;\;\; (51)}
  and a one-sided upper confidence interval is given by:
  \deqn{[-\infty, \; \hat{G}^{-1}(\alpha_2)] \;\;\;\;\;\; (52)}
  where \eqn{\alpha_1} and \eqn{\alpha_2} are computed as for a two-sided confidence
  interval, except \eqn{\alpha/2} is replaced with \eqn{\alpha} in Equations (46) and (47).

  The constant \eqn{\hat{z}_0} incorporates the bias correction, and the constant
  \eqn{\hat{a}} is the acceleration constant.  The term \dQuote{acceleration} refers
  to the rate of change of the standard error of the estimate of \eqn{\mu} with
  respect to the true value of \eqn{\mu} (Efron and Tibshirani, 1993, p.186).  For a
  normal (Gaussian) distribution, the standard error of the estimate of \eqn{\mu}
  does not depend on the value of \eqn{\mu}, hence the acceleration constant is not
  really necessary.

  When \code{ci.method="bootstrap"}, the function \code{enormCensored} computes both
  the percentile method and bias-corrected and accelerated method
  bootstrap confidence intervals.

  This method of constructing confidence intervals for censored data was studied by
  Shumway et al. (1989).
  \cr

  \bold{\emph{Generalized Pivotal Quantity}} (\code{ci.method="gpq"}) \cr
  This method was introduced by Schmee et al. (1985) and is discussed by
  Krishnamoorthy and Mathew (2009).  The idea is essentially to use a parametric
  bootstrap to estimate the correct pivotal quantities \eqn{z_1} and \eqn{z_2} in
  Equation (38) above.  For singly censored data, these quantities are computed as
  follows:
  \enumerate{
    \item Generate a random sample of \eqn{N} observations from a standard normal
      (i.e., N(0,1)) distribution and let \eqn{z_{(1)}, z_{(2)}, \ldots, z_{(N)}}
      denote the ordered (sorted) observations.
    \item Set the smallest \code{c} observations to be censored.
    \item Compute the estimates of \eqn{\mu} and \eqn{\sigma} using the method
      specified by the \code{method} argument, and denote these estimates as
      \eqn{\hat{\mu}^*, \; \hat{\sigma}^*}.
    \item Compute the t-like pivotal quantity
      \eqn{\hat{t} = \hat{\mu}^*/\hat{\sigma}^*}.
    \item Repeat steps 1-4 \code{nmc} times to produce an empirical distribution of
      the t-like pivotal quantity.
  }
  The function \code{enormCensored} calls the function
  \code{\link{gpqCiNormSinglyCensored}} to generate the distribution of
  pivotal quantities in the case of singly censored data.
  A two-sided \eqn{(1-\alpha)100\%} confidence interval for \eqn{\mu} is then
  computed as:
  \deqn{[\hat{\mu} - \hat{t}_{1-(\alpha/2)} \hat{\sigma}, \; \hat{\mu} - \hat{t}_{\alpha/2} \hat{\sigma}] \;\;\;\;\;\; (49)}
  where \eqn{\hat{t}_p} denotes the \eqn{p}'th empirical quantile of the
  \code{nmc} generated \eqn{\hat{t}} values.

  Schmee at al. (1985) derived this method in the context of Type II singly censored
  data (for which these limits are exact within Monte Carlo error), but state that
  according to Regal (1982) this method produces confidence intervals that are
  close apporximations to the correct limits for Type I censored data.

  For multiply censored data, this method has been extended as follows.  The
  algorithm stays the same, except that Step 2 becomes:\cr

  2. Set the \eqn{i}'th ordered generated observation to be censored or not censored
  according to whether the \eqn{i}'th observed observation in the original data
  is censored or not censored.

  The function \code{enormCensored} calls the function
  \code{\link{gpqCiNormMultiplyCensored}} to generate the distribution of
  pivotal quantities in the case of multiply censored data.
}
\value{
  a list of class \code{"estimateCensored"} containing the estimated parameters
  and other information.  See \code{\link{estimateCensored.object}} for details.
}
\references{
  Bain, L.J., and M. Engelhardt. (1991).  \emph{Statistical Analysis of
  Reliability and Life-Testing Models}.  Marcel Dekker, New York, 496pp.

  Cohen, A.C. (1959).  Simplified Estimators for the Normal Distribution When
  Samples are Singly Censored or Truncated.  \emph{Technometrics} \bold{1}(3), 217--237.

  Cohen, A.C. (1963).  Progressively Censored Samples in Life Testing.
  \emph{Technometrics} \bold{5}, 327--339

  Cohen, A.C. (1991).  \emph{Truncated and Censored Samples}.  Marcel Dekker,
  New York, New York, 312pp.

  Cox, D.R. (1970).  \emph{Analysis of Binary Data}.  Chapman & Hall, London.  142pp.

  Efron, B. (1979).  Bootstrap Methods: Another Look at the Jackknife.
  \emph{The Annals of Statistics} \bold{7}, 1--26.

  Efron, B., and R.J. Tibshirani. (1993).  \emph{An Introduction to the Bootstrap}.
  Chapman and Hall, New York, 436pp.

  El-Shaarawi, A.H. (1989).  Inferences About the Mean from Censored Water Quality
  Data.  \emph{Water Resources Research} \bold{25}(4) 685--690.

  El-Shaarawi, A.H., and D.M. Dolan. (1989).  Maximum Likelihood Estimation of
  Water Quality Concentrations from Censored Data.
  \emph{Canadian Journal of Fisheries and Aquatic Sciences} \bold{46}, 1033--1039.

  El-Shaarawi, A.H., and S.R. Esterby. (1992).  Replacement of Censored Observations
  by a Constant: An Evaluation.  \emph{Water Research} \bold{26}(6), 835--844.

  El-Shaarawi, A.H., and A. Naderi. (1991).  Statistical Inference from Multiply
  Censored Environmental Data.
  \emph{Environmental Monitoring and Assessment} \bold{17}, 339--347.

  Gibbons, R.D., D.K. Bhaumik, and S. Aryal. (2009).
  \emph{Statistical Methods for Groundwater Monitoring}, Second Edition.
  John Wiley & Sons, Hoboken.

  Gilliom, R.J., and D.R. Helsel. (1986).  Estimation of Distributional Parameters for
  Censored Trace Level Water Quality Data: 1. Estimation Techniques.
  \emph{Water Resources Research} \bold{22}, 135--146.

  Gleit, A. (1985).  Estimation for Small Normal Data Sets with Detection Limits.
  \emph{Environmental Science and Technology} \bold{19}, 1201--1206.

  Haas, C.N., and P.A. Scheff. (1990).  Estimation of Averages in Truncated Samples.
  \emph{Environmental Science and Technology} \bold{24}(6), 912--919.

  Hashimoto, L.K., and R.R. Trussell. (1983).  Evaluating Water Quality Data Near
  the Detection Limit.  Paper presented at the Advanced Technology Conference,
  American Water Works Association, Las Vegas, Nevada, June 5-9, 1983.

  Helsel, D.R. (1990).  Less than Obvious: Statistical Treatment of Data Below the
  Detection Limit.  \emph{Environmental Science and Technology} \bold{24}(12),
  1766--1774.

  Helsel, D.R. (2012). \emph{Statistics for Censored Environmental Data Using Minitab and R,
  Second Edition}.  John Wiley & Sons, Hoboken, New Jersey.

  Helsel, D.R., and T.A. Cohn. (1988).  Estimation of Descriptive Statistics for
  Multiply Censored Water Quality Data.  \emph{Water Resources Research}
  \bold{24}(12), 1997--2004.

  Hirsch, R.M., and J.R. Stedinger. (1987).  Plotting Positions for Historical
  Floods and Their Precision.  \emph{Water Resources Research} \bold{23}(4), 715--727.

  Korn, L.R., and D.E. Tyler. (2001).  Robust Estimation for Chemical Concentration
  Data Subject to Detection Limits.  In Fernholz, L., S. Morgenthaler, and W. Stahel,
  eds.  \emph{Statistics in Genetics and in the Environmental Sciences}.
  Birkhauser Verlag, Basel, pp.41--63.

  Krishnamoorthy K., and T. Mathew. (2009).
  \emph{Statistical Tolerance Regions: Theory, Applications, and Computation}.
  John Wiley and Sons, Hoboken.

  Michael, J.R., and W.R. Schucany. (1986).  Analysis of Data from Censored Samples.
  In D'Agostino, R.B., and M.A. Stephens, eds. \emph{Goodness-of Fit Techniques}.
  Marcel Dekker, New York, 560pp, Chapter 11, 461--496.

  Millard, S.P., P. Dixon, and N.K. Neerchal. (2014; in preparation).
  \emph{Environmental Statistics with R}.  CRC Press, Boca Raton, Florida.

  Nelson, W. (1982).  \emph{Applied Life Data Analysis}.
  John Wiley and Sons, New York, 634pp.

  Newman, M.C., P.M. Dixon, B.B. Looney, and J.E. Pinder. (1989).  Estimating Mean
  and Variance for Environmental Samples with Below Detection Limit Observations.
  \emph{Water Resources Bulletin} \bold{25}(4), 905--916.

  Pettitt, A. N. (1983).  Re-Weighted Least Squares Estimation with Censored and
  Grouped Data: An Application of the EM Algorithm.
  \emph{Journal of the Royal Statistical Society, Series B} \bold{47}, 253--260.

  Regal, R. (1982).  Applying Order Statistic Censored Normal Confidence Intervals
  to Time Censored Data.  Unpublished manuscript, University of Minnesota, Duluth,
  Department of Mathematical Sciences.

  Royston, P. (2007).  Profile Likelihood for Estimation and Confdence Intervals.
  \emph{The Stata Journal} \bold{7}(3), pp. 376--387.

  Saw, J.G. (1961b).  The Bias of the Maximum Likelihood Estimators of Location and
  Scale Parameters Given a Type II Censored Normal Sample.
  \emph{Biometrika} \bold{48}, 448--451.

  Schmee, J., D.Gladstein, and W. Nelson. (1985).  Confidence Limits for Parameters
  of a Normal Distribution from Singly Censored Samples, Using Maximum Likelihood.
  \emph{Technometrics} \bold{27}(2) 119--128.

  Schneider, H. (1986).  \emph{Truncated and Censored Samples from Normal Populations}.
  Marcel Dekker, New York, New York, 273pp.

  Shumway, R.H., A.S. Azari, and P. Johnson. (1989).  Estimating Mean Concentrations
  Under Transformations for Environmental Data With Detection Limits.
  \emph{Technometrics} \bold{31}(3), 347--356.

  Singh, A., R. Maichle, and S. Lee. (2006).  \emph{On the Computation of a 95\%
  Upper Confidence Limit of the Unknown Population Mean Based Upon Data Sets
  with Below Detection Limit Observations}.  EPA/600/R-06/022, March 2006.
  Office of Research and Development, U.S. Environmental Protection Agency,
  Washington, D.C.

  Stryhn, H., and J. Christensen. (2003).  \emph{Confidence Intervals by the Profile
  Likelihood Method, with Applications in Veterinary Epidemiology}.  Contributed paper
  at ISVEE X (November 2003, Chile).
  \url{https://gilvanguedes.com/wp-content/uploads/2019/05/Profile-Likelihood-CI.pdf}.

  Travis, C.C., and M.L. Land. (1990).  Estimating the Mean of Data Sets with
  Nondetectable Values.  \emph{Environmental Science and Technology} \bold{24}, 961--962.

  USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
  EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.
  U.S. Environmental Protection Agency, Washington, D.C. Chapter 15.

  USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
  EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
  U.S. Environmental Protection Agency, Washington, D.C.

  Venzon, D.J., and S.H. Moolgavkar. (1988).  A Method for Computing
  Profile-Likelihood-Based Confidence Intervals.  \emph{Journal of the Royal
  Statistical Society, Series C (Applied Statistics)} \bold{37}(1), pp. 87--94.
}
\author{
    Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  A sample of data contains censored observations if some of the observations are
  reported only as being below or above some censoring level.  In environmental
  data analysis, Type I left-censored data sets are common, with values being
  reported as \dQuote{less than the detection limit} (e.g., Helsel, 2012).  Data
  sets with only one censoring level are called \emph{singly censored}; data sets with
  multiple censoring levels are called \emph{multiply} or \emph{progressively censored}.

  Statistical methods for dealing with censored data sets have a long history in the
  field of survival analysis and life testing.  More recently, researchers in the
  environmental field have proposed alternative methods of computing estimates and
  confidence intervals in addition to the classical ones such as maximum likelihood
  estimation.

  Helsel (2012, Chapter 6) gives an excellent review of past studies of the
  properties of various estimators based on censored environmental data.

  In practice, it is better to use a confidence interval for the mean or a
  joint confidence region for the mean and standard deviation, rather than rely on a
  single point-estimate of the mean.  Since confidence intervals and regions depend
  on the properties of the estimators for both the mean and standard deviation, the
  results of studies that simply evaluated the performance of the mean and standard
  deviation separately cannot be readily extrapolated to predict the performance of
  various methods of constructing confidence intervals and regions.  Furthermore,
  for several of the methods that have been proposed to estimate the mean based on
  type I left-censored data, standard errors of the estimates are not available,
  hence it is not possible to construct confidence intervals
  (El-Shaarawi and Dolan, 1989).

  Few studies have been done to evaluate the performance of methods for constructing
  confidence intervals for the mean or joint confidence regions for the mean and
  standard deviation when data are subjected to single or multiple censoring.
  See, for example, Singh et al. (2006).

  Schmee et al. (1985) studied Type II censoring for a normal distribution and
  noted that the bias and variances of the maximum likelihood estimators are of the
  order \eqn{1/N}, and that the bias is negligible for \eqn{N=100} and as much as
  90\% censoring.  (If the proportion of censored observations is less than 90\%,
  the bias becomes negligible for smaller sample sizes.)  For small samples with
  moderate to high censoring, however, the bias of the mle's causes confidence
  intervals based on them using a normal approximation (e.g., \code{method="mle"}
  and \code{ci.method="normal.approx"}) to be too short.  Schmee et al. (1985)
  provide tables for exact confidence intervals for sample sizes up to \eqn{N=100}
  that were created based on Monte Carlo simulation.  Schmee et al. (1985) state
  that these tables should work well for Type I censored data as well.

  Shumway et al. (1989) evaluated the coverage of 90\% confidence intervals for the
  mean based on using a Box-Cox transformation to induce normality, computing the
  mle's based on the normal distribution, then computing the mean in the original
  scale.  They considered three methods of constructing confidence intervals:  the
  delta method, the bootstrap, and the bias-corrected bootstrap.
  Shumway et al. (1989) used three parent distributions in their study:
  Normal(3,1), the square of this distribuiton, and the exponentiation of this
  distribution (i.e., a lognormal distribution).  Based on sample sizes of 10 and 50
  with a censoring level at the 10'th or 20'th percentile, Shumway et al. (1989)
  found that the delta method performed quite well and was superior to the
  bootstrap method.

  Millard et al. (2015; in preparation) show that the coverage of the profile likelihood
  method is excellent.
}
\seealso{
  \link[stats]{Normal}, \code{\link{enorm}}, \code{\link{estimateCensored.object}}.
}
\examples{
  # Chapter 15 of USEPA (2009) gives several examples of estimating the mean
  # and standard deviation of a lognormal distribution on the log-scale using
  # manganese concentrations (ppb) in groundwater at five background wells.
  # In EnvStats these data are stored in the data frame
  # EPA.09.Ex.15.1.manganese.df.

  # Here we will estimate the mean and standard deviation using the MLE,
  # Q-Q regression (also called parametric regression on order statistics
  # or ROS; e.g., USEPA, 2009 and Helsel, 2012), and imputation with Q-Q
  # regression (also called robust regression on order statistics or rROS).

  # We will log-transform the original observations and then call
  # enormCensored.  Alternatively, we could have more simply called
  # elnormCensored.

  # First look at the data:
  #-----------------------

  EPA.09.Ex.15.1.manganese.df

  #   Sample   Well Manganese.Orig.ppb Manganese.ppb Censored
  #1       1 Well.1                 <5           5.0     TRUE
  #2       2 Well.1               12.1          12.1    FALSE
  #3       3 Well.1               16.9          16.9    FALSE
  #...
  #23      3 Well.5                3.3           3.3    FALSE
  #24      4 Well.5                8.4           8.4    FALSE
  #25      5 Well.5                 <2           2.0     TRUE

  longToWide(EPA.09.Ex.15.1.manganese.df,
    "Manganese.Orig.ppb", "Sample", "Well",
    paste.row.name = TRUE)

  #         Well.1 Well.2 Well.3 Well.4 Well.5
  #Sample.1     <5     <5     <5    6.3   17.9
  #Sample.2   12.1    7.7    5.3   11.9   22.7
  #Sample.3   16.9   53.6   12.6     10    3.3
  #Sample.4   21.6    9.5  106.3     <2    8.4
  #Sample.5     <2   45.9   34.5   77.2     <2


  # Now estimate the mean and standard deviation on the log-scale
  # using the MLE:
  #---------------------------------------------------------------

  with(EPA.09.Ex.15.1.manganese.df,
    enormCensored(log(Manganese.ppb), Censored))

  #Results of Distribution Parameter Estimation
  #Based on Type I Censored Data
  #--------------------------------------------
  #
  #Assumed Distribution:            Normal
  #
  #Censoring Side:                  left
  #
  #Censoring Level(s):              0.6931472 1.6094379
  #
  #Estimated Parameter(s):          mean = 2.215905
  #                                 sd   = 1.356291
  #
  #Estimation Method:               MLE
  #
  #Data:                            log(Manganese.ppb)
  #
  #Censoring Variable:              Censored
  #
  #Sample Size:                     25
  #
  #Percent Censored:                24%

  # Now compare the MLE with the estimators based on
  # Q-Q regression and imputation with Q-Q regression
  #--------------------------------------------------

  with(EPA.09.Ex.15.1.manganese.df,
    enormCensored(log(Manganese.ppb), Censored))$parameters
  #    mean       sd
  #2.215905 1.356291

  with(EPA.09.Ex.15.1.manganese.df,
    enormCensored(log(Manganese.ppb), Censored,
    method = "ROS"))$parameters
  #    mean       sd
  #2.293742 1.283635

  with(EPA.09.Ex.15.1.manganese.df,
    enormCensored(log(Manganese.ppb), Censored,
    method = "rROS"))$parameters
  #    mean       sd
  #2.298656 1.238104

  #----------

  # The method used to estimate quantiles for a Q-Q plot is
  # determined by the argument prob.method.  For the functions
  # enormCensored and elnormCensored, for any estimation
  # method that involves Q-Q regression, the default value of
  # prob.method is "hirsch-stedinger" and the default value for the
  # plotting position constant is plot.pos.con=0.375.

  # Both Helsel (2012) and USEPA (2009) also use the Hirsch-Stedinger
  # probability method but set the plotting position constant to 0.

  with(EPA.09.Ex.15.1.manganese.df,
    enormCensored(log(Manganese.ppb), Censored,
    method = "rROS", plot.pos.con = 0))$parameters
  #    mean       sd
  #2.277175 1.261431

  #----------

  # Using the same data as above, compute a confidence interval
  # for the mean on the log-scale using the profile-likelihood
  # method.

  with(EPA.09.Ex.15.1.manganese.df,
    enormCensored(log(Manganese.ppb), Censored, ci = TRUE))

  #Results of Distribution Parameter Estimation
  #Based on Type I Censored Data
  #--------------------------------------------
  #
  #Assumed Distribution:            Normal
  #
  #Censoring Side:                  left
  #
  #Censoring Level(s):              0.6931472 1.6094379
  #
  #Estimated Parameter(s):          mean = 2.215905
  #                                 sd   = 1.356291
  #
  #Estimation Method:               MLE
  #
  #Data:                            log(Manganese.ppb)
  #
  #Censoring Variable:              Censored
  #
  #Sample Size:                     25
  #
  #Percent Censored:                24%
  #
  #Confidence Interval for:         mean
  #
  #Confidence Interval Method:      Profile Likelihood
  #
  #Confidence Interval Type:        two-sided
  #
  #Confidence Level:                95%
  #
  #Confidence Interval:             LCL = 1.595062
  #                                 UCL = 2.771197
}
\keyword{ distribution }
\keyword{ htest }

