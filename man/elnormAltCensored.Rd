\name{elnormAltCensored}
\alias{elnormAltCensored}
\title{
  Estimate Parameters for a Lognormal Distribution (Original Scale) Based on Type I Censored Data
}
\description{
  Estimate the mean and coefficient of variation of a
  \link[=LognormalAlt]{lognormal distribution} given a
  sample of data that has been subjected to Type I censoring,
  and optionally construct a confidence interval for the mean.
}
\usage{
  elnormAltCensored(x, censored, method = "mle", censoring.side = "left",
    ci = FALSE, ci.method = "profile.likelihood", ci.type = "two-sided",
    conf.level = 0.95, n.bootstraps = 1000, pivot.statistic = "z", ...)
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

  For singly censored data, the possible values are: \cr
  \code{"mle"} (maximum likelihood; the default), \cr
  \code{"qmvue"} (quasi minimum variance unbiased estimation), \cr
  \code{"bcmle"} (bias-corrected maximum likelihood), \cr
  \code{"rROS"} or \code{"impute.w.qq.reg"} (moment estimation based on imputation using
    quantile-quantile regression; also called \emph{robust regression on order statistics}
    and abbreviated rROS), \cr
  \code{"impute.w.qq.reg.w.cen.level"} (moment estimation based on imputation
    using the \code{qq.reg.w.cen.level method}), \cr
  \code{"impute.w.mle"} (moment estimation based on imputation using the mle), and \cr
  \code{"half.cen.level"} (moment estimation based on setting the censored
    observations to half the censoring level).

  For multiply censored data, the possible values are: \cr
  \code{"mle"} (maximum likelihood; the default), \cr
  \code{"qmvue"} (quasi minimum variance unbiased estimation), \cr
  \code{"bcmle"} (bias-corrected maximum likelihood), \cr
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
  for the mean.  The possible values are
  \code{"profile.likelihood"} (profile likelihood; the default), \cr
  \code{"cox"} (Cox's approximation), \cr
  \code{"delta"} (normal approximation based on the delta method), \cr
  \code{"normal.approx"} (normal approximation), and \cr
  \code{"bootstrap"} (based on bootstrapping).

  The confidence interval method \code{"profile.likelihood"} is valid only
  when \code{method="mle"}. \cr
  The confidence interval methods \code{"delta"} and \code{"cox"} are valid only
  when \code{method} is one of \code{"mle"}, \code{"bcmle"}, or \code{"qmvue"}.  \cr
  The confidence interval method \code{"normal.approx"} is valid only when
  \code{method} is one of \code{"rROS"}, \code{"impute.w.qq.reg"},
  \code{"impute.w.qq.reg.w.cen.level"}, \code{"impute.w.mle"}, or
  \code{"half.cen.level"}.

  See the DETAILS section for more information.
  This argument is ignored if \code{ci=FALSE}.
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
  of the confidence interval for the mean when \code{ci.method} is equal to
  \code{"delta"}, \code{"cox"}, or \code{"normal.approx"} (see the DETAILS section).
  The possible
  values are \code{pivot.statistic="z"} (the default) and \code{pivot.statistic="t"}.
  When \code{pivot.statistic="t"} you may supply the argument
  \code{ci.sample size} (see below).  The argument \code{pivot.statistic} is
  ignored if \code{ci=FALSE}.
}
  \item{\dots}{
  additional arguments to pass to other functions.
  \itemize{
  \item \code{prob.method}.  Character string indicating what method to use to
    compute the plotting positions (empirical probabilities) when \code{method}
    is one of \code{"rROS"}, \code{"impute.w.qq.reg"}, \code{"impute.w.qq.reg.w.cen.level"}, or
    \code{"impute.w.mle"}.  Possible values are
    \code{"kaplan-meier"} (product-limit method of Kaplan and Meier (1958)),
    \code{"nelson"} (hazard plotting method of Nelson (1972)),
    \code{"michael-schucany"} (generalization of the product-limit method due to Michael and Schucany (1986)), and
    \code{"hirsch-stedinger"} (generalization of the product-limit method due to Hirsch and Stedinger (1987)).
    The default value is \code{prob.method="hirsch-stedinger"}.  The \code{"nelson"}
    method is only available for \code{censoring.side="right"}.
    See the DETAILS section and the help file for \code{\link{ppointsCensored}}
    for more information.

  \item \code{plot.pos.con}.  Numeric scalar between 0 and 1 containing the
    value of the plotting position constant to use when \code{method} is one of
    \code{"rROS"}, \cr
    \code{"impute.w.qq.reg"}, \code{"impute.w.qq.reg.w.cen.level"}, or
    \code{"impute.w.mle"}.  The default value is \code{plot.pos.con=0.375}.
    See the DETAILS section and the help file for \code{\link{ppointsCensored}}
    for more information.

  \item \code{ci.sample.size}.  Numeric scalar indicating what sample size to
    assume to construct the confidence interval for the mean if
    \code{pivot.statistic="t"} and \code{ci.method} is equal to
    \code{"delta"}, \code{"cox"}, or \code{"normal.approx"}.
    When \code{method} equals
    \code{"mle"}, \code{"bcmle"}, or \code{"qmvue"}, the default value is the
    expected number of
    uncensored observations, otherwise it is the observed number of
    uncensored observations.

  \item \code{lb.impute}.  Numeric scalar indicating the lower bound for imputed
    observations when method is one of \code{"rROS"}, \code{"impute.w.qq.reg"}, \cr
    \code{"impute.w.qq.reg.w.cen.level"}, or \code{"impute.w.mle"}.
    Imputed values smaller than this
    value will be set to this value.  The default is \code{lb.impute=-Inf}.

  \item \code{ub.impute}.  Numeric scalar indicating the upper bound for imputed
    observations when method is one of \code{"rROS"}, \code{"impute.w.qq.reg"}, \cr
    \code{"impute.w.qq.reg.w.cen.level"}, or \code{"impute.w.mle"}.
    Imputed values larger than this value
    will be set to this value.  The default is \code{ub.impute=Inf}.
  }
}
}
\details{
  If \code{x} or \code{censored} contain any missing (\code{NA}), undefined (\code{NaN}) or
  infinite (\code{Inf}, \code{-Inf}) values, they will be removed prior to
  performing the estimation.

  Let \eqn{\underline{x}} be a vector of \eqn{n} observations from a
  \link[=LognormalAlt]{lognormal distribution} with
  parameters \code{mean=}\eqn{\theta} and \code{cv=}\eqn{\tau}.  Let \eqn{\eta} denote the
  standard deviation of this distribution, so that \eqn{\eta = \theta \tau}.  Set
  \eqn{\underline{y} = log(\underline{x})}.  Then \eqn{\underline{y}} is a vector of observations
  from a normal distribution with parameters \code{mean=}\eqn{\mu} and \code{sd=}\eqn{\sigma}.
  See the help file for \link{LognormalAlt} for the relationship between
  \eqn{\theta, \tau, \eta, \mu}, and \eqn{\sigma}.

  Let \eqn{\underline{x}} denote a vector of \eqn{N} observations from a
  \link[=LognormalAlt]{lognormal distribution} with parameters
  \code{mean=}\eqn{\theta} and \code{cv=}\eqn{\tau}.  Let \eqn{\eta} denote the
  standard deviation of this distribution, so that \eqn{\eta = \theta \tau}.  Set
  \eqn{\underline{y} = log(\underline{x})}.  Then \eqn{\underline{y}} is a
  vector of observations from a normal distribution with parameters
  \code{mean=}\eqn{\mu} and \code{sd=}\eqn{\sigma}.  See the help file for
  \link{LognormalAlt} for the relationship between
  \eqn{\theta, \tau, \eta, \mu}, and \eqn{\sigma}.

  Assume \eqn{n} (\eqn{0 < n < N}) of the \eqn{N} observations are known and
  \eqn{c} (\eqn{c=N-n}) of the observations are all censored below (left-censored)
  or all censored above (right-censored) at \eqn{k} fixed censoring levels
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
  This section explains how each of the estimators of \code{mean=}\eqn{\theta} and
  \code{cv=}\eqn{\tau} are computed.  The approach is to first compute estimates of
  \eqn{\theta} and \eqn{\eta^2} (the mean and variance of the lognormal distribution),
  say \eqn{\hat{\theta}} and \eqn{\hat{\eta}^2}, then compute the estimate of the cv
  \eqn{\tau} by \eqn{\hat{\tau} = \hat{\eta}/\hat{\theta}}.

  \emph{Maximum Likelihood Estimation} (\code{method="mle"}) \cr
  The maximum likelihood estimators of \eqn{\theta}, \eqn{\tau}, and \eqn{\eta} are
  computed as:
  \deqn{\hat{\theta}_{mle} = exp(\hat{\mu}_{mle} + \frac{\hat{\sigma}^2_{mle}}{2}) \;\;\;\;\;\; (3)}
  \deqn{\hat{\tau}_{mle} = [exp(\hat{\sigma}^2_{mle})  - 1]^{1/2} \;\;\;\;\;\; (4)}
  \deqn{\hat{\eta}_{mle} = \hat{\theta}_{mle} \; \hat{\tau}_{mle} \;\;\;\;\;\; (5)}
  where \eqn{\hat{\mu}_{mle}} and \eqn{\hat{\sigma}_{mle}} denote the maximum
  likelihood estimators of \eqn{\mu} and \eqn{\sigma}.  See the help for for
  \code{\link{enormCensored}} for information on how \eqn{\hat{\mu}_{mle}} and
  \eqn{\hat{\sigma}_{mle}} are computed.
  \cr

  \emph{Quasi Minimum Variance Unbiased Estimation Based on the MLE's} (\code{method="qmvue"}) \cr
  The maximum likelihood estimators of \eqn{\theta} and \eqn{\eta^2} are biased.
  Even for complete (uncensored) samples these estimators are biased
  (see equation (12) in the help file for \code{\link{elnormAlt}}).
  The bias tends to 0 as the sample size increases, but it can be considerable for
  small sample sizes.
  (Cohn et al., 1989, demonstrate the bias for complete data sets.)
  For the case of complete samples, the minimum variance unbiased estimators (mvue's)
  of \eqn{\theta} and \eqn{\eta^2} were derived by Finney (1941) and are discussed in
  Gilbert (1987, pp.164-167) and Cohn et al. (1989).  These estimators are computed as:
  \deqn{\hat{\theta}_{mvue} = e^{\bar{y}} g_{n-1}(\frac{s^2}{2}) \;\;\;\;\;\; (6)}
  \deqn{\hat{\eta}^2_{mvue} = e^{2 \bar{y}} \{g_{n-1}(2s^2) - g_{n-1}[\frac{(n-2)s^2}{n-1}]\} \;\;\;\;\;\; (7)}
  where
  \deqn{\bar{y} = \frac{1}{n} \sum_{i=1}^n y_i \;\;\;\;\;\; (8)}
  \deqn{s^2 = \frac{1}{n-1} \sum_{i=1}^n (y_i - \bar{y})^2 \;\;\;\;\;\; (9)}
  \deqn{g_m(z) = \sum_{i=0}^\infty \frac{m^i (m+2i)}{m(m+2) \cdots (m+2i)} (\frac{m}{m+1})^i (\frac{z^i}{i!}) \;\;\;\;\;\; (10)}
  (see the help file for \code{\link{elnormAlt}}).

  For Type I censored samples, the quasi minimum variance unbiased estimators
  (qmvue's) of \eqn{\theta} and \eqn{\eta^2} are computed using equations (6) and (7)
  and estimating \eqn{\mu} and \eqn{\sigma} with their mle's (see
  \code{\link{elnormCensored}}).

  For singly censored data, this is apparently the LM method of Gilliom and Helsel
  (1986, p.137) (it is not clear from their description on page 137 whether their
  LM method is the straight \code{method="mle"} described above or
  \code{method="qmvue"} described here).  This method was also used by
  Newman et al. (1989, p.915, equations 10-11).

  For multiply censored data, this is apparently the MM method of Helsel and Cohn
  (1988, p.1998).  (It is not clear from their description on page 1998 and the
  description in Gilliom and Helsel, 1986, page 137 whether Helsel and Cohn's (1988)
  MM method is the straight \code{method="mle"} described above or \code{method="qmvue"}
  described here.)
  \cr

  \emph{Bias-Corrected Maximum Likelihood Estimation} (\code{method="bcmle"}) \cr
  This method was derived by El-Shaarawi (1989) and can be applied to complete or
  censored data sets.  For complete data, the exact relative bias of the mle of
  the mean \eqn{\theta} is given as:
  \deqn{B_{mle} = \frac{E[\hat{\theta}_{mle}]}{\theta} = exp[\frac{-(n-1)\sigma^2}{2n}] (1 - \frac{\sigma^2}{n})^{-(n-1)/2} \;\;\;\;\;\; (11)}
  (see equation (12) in the help file for \code{\link{elnormAlt}}).

  For the case of complete or censored data, El-Shaarawi (1989) proposed the
  following \dQuote{bias-corrected} maximum likelihood estimator:
  \deqn{\hat{\theta}_{bcmle} = \frac{\hat{\theta}_{mle}}{\hat{B}_{mle}} \;\;\;\;\;\; (12)}
  where
  \deqn{\hat{B}_{mle} = exp[\frac{1}{2}(\hat{V}_{11} + 2\hat{\sigma}_{mle} \hat{V}_{12} + \hat{\sigma}^2_{mle} \hat{V}_{22})] \;\;\;\;\;\; (13)}
  and \eqn{V} denotes the asymptotic variance-covariance of the mle's of \eqn{\mu}
  and \eqn{\sigma}, which is based on the observed information matrix, formulas for
  which are given in Cohen (1991).  El-Shaarawi (1989) does not propose a
  bias-corrected estimator of the variance \eqn{\eta^2}, so the mle of \eqn{\eta}
  is computed when \code{method="bcmle"}.
  \cr

  \emph{Robust Regression on Order Statistics} (\code{method="rROS"}) or \cr
  \emph{Imputation Using Quantile-Quantile Regression} (\code{method ="impute.w.qq.reg"}) \cr
  This is the robust Regression on Order Statistics (rROS) method discussed in USEPA (2009)
  and Helsel (2012).  This method involves using quantile-quantile regression on the
  log-transformed observations to fit a regression line (and thus initially estimate the mean
  \eqn{\mu} and standard deviation \eqn{\sigma} in log-space), imputing the
  log-transformed values of the \eqn{c} censored observations by predicting them
  from the regression equation, transforming the log-scale imputed values back to
  the original scale, and then computing the method of moments estimates of the
  mean and standard deviation based on the observed and imputed values.

  The steps are:
  \enumerate{
    \item Estimate \eqn{\mu} and \eqn{\sigma} by computing the least-squares
      estimates in the following model:
      \deqn{y_{(i)} = \mu + \sigma \Phi^{-1}(p_i) + \epsilon_i, \; i \in \Omega \;\;\;\;\;\; (14)}
      where \eqn{p_i} denotes the plotting position associated with the \eqn{i}'th
      largest value, \eqn{a} is a constant such that \eqn{0 \le a \le 1}
      (the default value is 0.375), \eqn{\Phi} denotes the cumulative
      distribution function (cdf) of the standard normal distribution and
      \eqn{\Omega} denotes the set of \eqn{n} subscripts associated with the
      uncensored observations in the ordered sample.  The plotting positions are
      computed by calling the function \code{\link{ppointsCensored}}.
      \cr
    \item Compute the log-scale imputed values as:
      \deqn{\hat{y}_{(i)} = \hat{\mu}_{qqreg} + \hat{\sigma}_{qqreg} \Phi^{-1}(p_i), \; i \not \in \Omega \;\;\;\;\;\; (15)}
      \cr
    \item Retransform the log-scale imputed values:
      \deqn{\hat{x}_{(i)} = exp[\hat{y}_{(i)}], \; i \not \in \Omega  \;\;\;\;\;\; (16)}
      \cr
    \item Compute the usual method of moments estimates of the mean and variance.
      \deqn{\hat{\theta} = \frac{1}{N} [\sum_{i \not \in \Omega} \hat{x}_{(i)} + \sum_{i \in \Omega} x_{(i)}] \;\;\;\;\;\; (17)}
      \deqn{\hat{\eta}^2 = \frac{1}{N-1} [\sum_{i \not \in \Omega} (\hat{x}_{(i)} - \hat{\theta}^2) + \sum_{i \in \Omega} (x_{(i)} - \hat{\theta}^2)] \;\;\;\;\;\; (18)}
      Note that the estimate of variance is actually the usual unbiased one
      (not the method of moments one) in the case of complete data.
  }
  For sinlgy censored data, this method is discussed by Hashimoto and Trussell (1983),
  Gilliom and Helsel (1986), and El-Shaarawi (1989), and is referred to as the LR
  (Log-Regression) or Log-Probability Method.

  For multiply censored data, this is the MR method of Helsel and Cohn (1988, p.1998).
  They used it with the probability method of Hirsch and Stedinger (1987) and
  Weibull plotting positions (i.e., \code{prob.method="hirsch-stedinger"} and
  \code{plot.pos.con=0}).

  The argument \code{plot.pos.con} (see the entry for \dots in the ARGUMENTS
  section above) determines the value of the plotting positions computed in
  equations (14) and (15) when \code{method} equals \code{"hirsch-stedinger"} or
  \code{"michael-schucany"}.  The default value is \code{plot.pos.con=0.375}.
  See the help file for \code{\link{ppointsCensored}} for more information.

  The arguments \code{lb.impute} and \code{ub.impute} (see the entry for \dots in
  the ARGUMENTS section above) determine the lower and upper bounds for the
  imputed values.  Imputed values smaller than \code{lb.impute} are set to this
  value.  Imputed values larger than \code{ub.impute} are set to this value.
  The default values are \code{lb.impute=0} and \code{ub.impute=Inf}.
  \cr

  \emph{Imputation Using Quantile-Quantile Regression Including the Censoring Level}\cr
       (\code{method ="impute.w.qq.reg.w.cen.level"}) \cr
  \bold{This method is only available for sinlgy censored data}. This method was
  proposed by El-Shaarawi (1989), which he denoted as the Modified LR Method.
  It is exactly the same method as imputation
  using quantile-quantile regression (\code{method="impute.w.qq.reg"}), except that
  the quantile-quantile regression includes the censoring level.  For left singly
  censored data, the modification involves adding the point
  \eqn{[\Phi^{-1}(p_c), T]} to the plot before fitting the least-squares line.
  For right singly censored data, the point  \eqn{[\Phi^{-1}(p_{n+1}), T]}
  is added to the plot before fitting the least-squares line.
  \cr

  \emph{Imputation Using Maximum Likelihood} (\code{method ="impute.w.mle"}) \cr
  \bold{This method is only available for sinlgy censored data}.
  This is exactly the same method as robust Regression on Order Statistics (i.e.,
  the same as using \code{method="rROS"} or \cr
  \code{method="impute.w.qq.reg"}),
  except that the maximum likelihood method (\code{method="mle"}) is used to compute
  the initial estimates of the mean and standard deviation.
  In the context of lognormal data, this method is discussed
  by El-Shaarawi (1989), which he denotes as the Modified Maximum Likelihood Method.
  \cr

  \emph{Setting Censored Observations to Half the Censoring Level} (\code{method="half.cen.level"}) \cr
  This method is applicable only to left censored data that is bounded below by 0.
  This method involves simply replacing all the censored observations with half their
  detection limit, and then computing the usual moment estimators of the mean and
  variance.  That is, all censored observations are imputed to be half the detection
  limit, and then Equations (17) and (18) are used to estimate the mean and varaince.

  This method is included only to allow comparison of this method to other methods.
  \bold{\emph{Setting left-censored observations to half the censoring level is not
  recommended}}.  In particular, El-Shaarawi and Esterby (1992) show that these
  estimators are biased and inconsistent (i.e., the bias remains even as the sample
  size increases).
  \cr \cr

  \bold{CONFIDENCE INTERVALS} \cr
  This section explains how confidence intervals for the mean \eqn{\theta} are
  computed.
  f\cr

  \emph{Likelihood Profile} (\code{ci.method="profile.likelihood"}) \cr
  This method was proposed by Cox (1970, p.88), and Venzon and Moolgavkar (1988)
  introduced an efficient method of computation.  This method is also discussed by
  Stryhn and Christensen (2003) and Royston (2007).
  The idea behind this method is to invert the likelihood-ratio test to obtain a
  confidence interval for the mean \eqn{\theta} while treating the coefficient of
  variation \eqn{\tau} as a nuisance parameter.

  For Type I left censored data, the likelihood function is given by:
  \deqn{L(\theta, \tau | \underline{x}) = {N \choose c_1 c_2 \ldots c_k n} \prod_{j=1}^k [F(T_j)]^{c_j} \prod_{i \in \Omega} f[x_{(i)}] \;\;\;\;\;\; (19)}
  where \eqn{f} and \eqn{F} denote the probability density function (pdf) and
  cumulative distribution function (cdf) of the population. That is,
  \deqn{f(t) = \phi(\frac{t-\mu}{\sigma}) \;\;\;\;\;\; (20)}
  \deqn{F(t) = \Phi(\frac{t-\mu}{\sigma}) \;\;\;\;\;\; (21)}
  where
  \deqn{\mu = log(\frac{\theta}{\sqrt{\tau^2 + 1}}) \;\;\;\;\;\; (22)}
  \deqn{\sigma = [log(\tau^2 + 1)]^{1/2} \;\;\;\;\;\; (23)}
  and \eqn{\phi} and \eqn{\Phi} denote the pdf and cdf of the standard normal
  distribution, respectively (Cohen, 1963; 1991, pp.6, 50).  For left singly
  censored data, equation (3) simplifies to:
  \deqn{L(\mu, \sigma | \underline{x}) = {N \choose c} [F(T)]^{c} \prod_{i = c+1}^n f[x_{(i)}] \;\;\;\;\;\; (24)}

  Similarly, for Type I right censored data, the likelihood function is given by:
  \deqn{L(\mu, \sigma | \underline{x}) = {N \choose c_1 c_2 \ldots c_k n} \prod_{j=1}^k [1 - F(T_j)]^{c_j} \prod_{i \in \Omega} f[x_{(i)}] \;\;\;\;\;\; (25)}
  and for right singly censored data this simplifies to:
  \deqn{L(\mu, \sigma | \underline{x}) = {N \choose c} [1 - F(T)]^{c} \prod_{i = 1}^n f[x_{(i)}] \;\;\;\;\;\; (26)}

  Following Stryhn and Christensen (2003), denote the maximum likelihood estimates
  of the mean and coefficient of variation by \eqn{(\theta^*, \tau^*)}.
  The likelihood ratio test statistic (\eqn{G^2}) of the hypothesis
  \eqn{H_0: \theta = \theta_0} (where \eqn{\theta_0} is a fixed value) equals the
  drop in \eqn{2 log(L)} between the \dQuote{full} model and the reduced model with
  \eqn{\theta} fixed at \eqn{\theta_0}, i.e.,
  \deqn{G^2 = 2 \{log[L(\theta^*, \tau^*)] - log[L(\theta_0, \tau_0^*)]\} \;\;\;\;\;\; (27)}
  where \eqn{\tau_0^*} is the maximum likelihood estimate of \eqn{\tau} for the
  reduced model (i.e., when \eqn{\theta = \theta_0}).  Under the null hypothesis,
  the test statistic \eqn{G^2} follows a
  \link[stats:Chisquare]{chi-squared distribution} with 1 degree of freedom.

  Alternatively, we may
  express the test statistic in terms of the profile likelihood function \eqn{L_1}
  for the mean \eqn{\theta}, which is obtained from the usual likelihood function by
  maximizing over the parameter \eqn{\tau}, i.e.,
  \deqn{L_1(\theta) = max_{\tau} L(\theta, \tau) \;\;\;\;\;\; (28)}
  Then we have
  \deqn{G^2 = 2 \{log[L_1(\theta^*)] - log[L_1(\theta_0)]\} \;\;\;\;\;\; (29)}
  A two-sided \eqn{(1-\alpha)100\%} confidence interval for the mean \eqn{\theta}
  consists of all values of \eqn{\theta_0} for which the test is not significant at
  level \eqn{alpha}:
  \deqn{\theta_0: G^2 \le \chi^2_{1, {1-\alpha}} \;\;\;\;\;\; (30)}
  where \eqn{\chi^2_{\nu, p}} denotes the \eqn{p}'th quantile of the
  \link[stats:Chisquare]{chi-squared distribution} with \eqn{\nu} degrees of freedom.
  One-sided lower and one-sided upper confidence intervals are computed in a similar
  fashion, except that the quantity \eqn{1-\alpha} in Equation (30) is replaced with
  \eqn{1-2\alpha}.
  \cr

  \bold{\emph{Direct Normal Approximations}} (\code{ci.method="delta"} or \code{ci.method="normal.approx"}) \cr
  An approximate \eqn{(1-\alpha)100\%} confidence interval for \eqn{\theta} can be
  constructed assuming the distribution of the estimator of \eqn{\theta} is
  approximately normally distributed.  That is, a two-sided \eqn{(1-\alpha)100\%}
  confidence interval for \eqn{\theta} is constructed as:
  \deqn{[\hat{\theta} - t_{1-\alpha/2, m-1}\hat{\sigma}_{\hat{\theta}}, \; \hat{\theta} + t_{1-\alpha/2, m-1}\hat{\sigma}_{\hat{\theta}}] \;\;\;\;\;\; (31)}
  where \eqn{\hat{\theta}} denotes the estimate of \eqn{\theta},
  \eqn{\hat{\sigma}_{\hat{\theta}}} denotes the estimated asymptotic standard
  deviation of the estimator of \eqn{\theta}, \eqn{m} denotes the assumed sample
  size for the confidence interval, and \eqn{t_{p,\nu}} denotes the \eqn{p}'th
  quantile of \link[stats:TDist]{Student's t-distribuiton} with \eqn{\nu}
  degrees of freedom.  One-sided confidence intervals are computed in a
  similar fashion.

  The argument \code{ci.sample.size} determines the value of \eqn{m} (see
  see the entry for \dots in the ARGUMENTS section above).
  When \code{method} equals \code{"mle"}, \code{"qmvue"}, or \code{"bcmle"}
  and the data are singly censored, the default value is the
  expected number of uncensored observations, otherwise it is \eqn{n},
  the observed number of uncensored observations.  This is simply an ad-hoc
  method of constructing confidence intervals and is not based on any
  published theoretical results.

  When \code{pivot.statistic="z"}, the \eqn{p}'th quantile from the
  \link[stats:Normal]{standard normal distribution} is used in place of the
  \eqn{p}'th quantile from Student's t-distribution.

  \emph{Direct Normal Approximation Based on the Delta Method} (\code{ci.method="delta"}) \cr
  This method is usually applied with the maximum likelihood estimators
  (\code{method="mle"}).  It should also work approximately for the quasi minimum
  variance unbiased estimators (\code{method="qmvue"}) and the bias-corrected maximum
  likelihood estimators (\code{method="bcmle"}).

  When \code{method="mle"}, the variance of the mle of \eqn{\theta} can be estimated
  based on the variance-covariance matrix of the mle's of \eqn{\mu} and \eqn{\sigma}
  (denoted \eqn{V}), and the delta method:
  \deqn{\hat{\sigma}^2_{\hat{\theta}} = (\frac{\partial \theta}{\partial \underline{\lambda}})^{'}_{\hat{\underline{\lambda}}} \hat{V} (\frac{\partial \theta}{\partial \underline{\lambda}})_{\hat{\underline{\lambda}}} \;\;\;\;\;\; (32)}
  where
  \deqn{\underline{\lambda}' = (\mu, \sigma) \;\;\;\;\;\; (33)}
  \deqn{\frac{\partial \theta}{\partial \mu} = exp(\mu + \frac{\sigma^2}{2}) \;\;\;\;\;\; (34)}
  \deqn{\frac{\partial \theta}{\partial \sigma} = \sigma exp(\mu + \frac{\sigma^2}{2}) \;\;\;\;\;\; (35)}
  (Shumway et al., 1989).  The variance-covariance matrix \eqn{V} of the mle's of
  \eqn{\mu} and \eqn{\sigma} is estimated based on the inverse of the observed Fisher
  Information matrix, formulas for which are given in Cohen (1991).
  \cr

  \emph{Direct Normal Approximation Based on the Moment Estimators} (\code{ci.method="normal.approx"}) \cr
  This method is valid only for the moment estimators based on imputed values \cr
  (i.e., \code{method="impute.w.qq.reg"} or \code{method="half.cen.level"}).  For
  these cases, the standard deviation of the estimated mean is assumed to be
  approximated by
  \deqn{\hat{\sigma}_{\hat{\theta}} = \frac{\hat{\eta}}{\sqrt{m}} \;\;\;\;\;\; (36)}
  where, as already noted, \eqn{m} denotes the assumed sample size.
  This is simply an ad-hoc method of constructing confidence intervals and is not
  based on any published theoretical results.
  \cr

  \emph{Cox's Method} (\code{ci.method="cox"}) \cr
  This method may be applied with the maximum likelihood estimators
  (\code{method="mle"}), the quasi minimum variance unbiased estimators
  (\code{method="qmvue"}), and the bias-corrected maximum likelihood estimators
  (\code{method="bcmle"}).

  This method was proposed by El-Shaarawi (1989) and is an extension of the
  method derived by Cox and presented in Land (1972) for the case of
  complete data (see the explanation of \code{ci.method="cox"} in the help file
  for \code{\link{elnormAlt}}).  The idea is to construct an approximate
  \eqn{(1-\alpha)100\%} confidence interval for the quantity
  \deqn{\beta = exp(\mu + \frac{\sigma^2}{2}) \;\;\;\;\;\; (37)}
  assuming the estimate of \eqn{\beta}
  \deqn{\hat{\beta} = exp(\hat{\mu} + \frac{\hat{\sigma}^2}{2}) \;\;\;\;\;\; (38)}
  is approximately normally distributed, and then exponentiate the confidence limits.
  That is, a two-sided \eqn{(1-\alpha)100\%} confidence interval for \eqn{\theta}
  is constructed as:
  \deqn{[ exp(\hat{\beta} - h), \; exp(\hat{\beta} + h) ]\;\;\;\;\;\; (39)}
  where
  \deqn{h = t_{1-\alpha/2, m-1}\hat{\sigma}_{\hat{\beta}} \;\;\;\;\;\; (40)}
  and \eqn{\hat{\sigma}_{\hat{\beta}}} denotes the estimated asymptotic standard
  deviation of the estimator of \eqn{\beta}, \eqn{m} denotes the assumed sample
  size for the confidence interval, and \eqn{t_{p,\nu}} denotes the \eqn{p}'th
  quantile of \link[stats:TDist]{Student's t-distribuiton} with \eqn{\nu}
  degrees of freedom.

  El-Shaarawi (1989) shows that the standard deviation of the mle of \eqn{\beta} can
  be estimated by:
  \deqn{\hat{\sigma}_{\hat{\beta}} = \sqrt{ \hat{V}_{11} + 2 \hat{\sigma} \hat{V}_{12} + \hat{\sigma}^2 \hat{V}_{22} } \;\;\;\;\;\; (41)}
  where \eqn{V} denotes the variance-covariance matrix of the mle's of \eqn{\mu} and
  \eqn{\sigma} and is estimated based on the inverse of the Fisher Information matrix.

  One-sided confidence intervals are computed in a similar fashion.
  \cr

  \bold{\emph{Bootstrap and Bias-Corrected Bootstrap Approximation}} (\code{ci.method="bootstrap"}) \cr
  The bootstrap is a nonparametric method of estimating the distribution
  (and associated distribution parameters and quantiles) of a sample statistic,
  regardless of the distribution of the population from which the sample was drawn.
  The bootstrap was introduced by Efron (1979) and a general reference is
  Efron and Tibshirani (1993).

  In the context of deriving an approximate \eqn{(1-\alpha)100\%} confidence interval
  for the population mean \eqn{\theta}, the bootstrap can be broken down into the
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

    \item Estimate \eqn{\theta} based on the bootstrap sample created in Step 1, using
      the same method that was used to estimate \eqn{\theta} using the original
      observations in \eqn{\underline{x}}. Because the bootstrap sample usually
      does not match the original sample, the estimate of \eqn{\theta} based on the
      bootstrap sample will usually differ from the original estimate based on
      \eqn{\underline{x}}.

    \item Repeat Steps 1 and 2 \eqn{B} times, where \eqn{B} is some large number.
      The number of bootstraps \eqn{B} is determined by the argument
      \code{n.bootstraps} (see the section ARGUMENTS above).
      The default value of \code{n.bootstraps} is \code{1000}.

    \item Use the \eqn{B} estimated values of \eqn{\theta} to compute the empirical
      cumulative distribution function of this estimator of \eqn{\theta} (see
      \code{\link{ecdfPlot}}), and then create a confidence interval for \eqn{\theta}
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
  \deqn{[0, \; \hat{G}^{-1}(1-\alpha)] \;\;\;\;\;\; (44)}
  The function \code{elnormAltCensored} calls the \R function \code{\link[stats]{quantile}}
  to compute the empirical quantiles used in Equations (42)-(44).

  The percentile method bootstrap confidence interval is only first-order
  accurate (Efron and Tibshirani, 1993, pp.187-188), meaning that the probability
  that the confidence interval will contain the true value of \eqn{\theta} can be
  off by \eqn{k/\sqrt{N}}, where \eqn{k}is some constant.  Efron and Tibshirani
  (1993, pp.184-188) proposed a bias-corrected and accelerated interval that is
  second-order accurate, meaning that the probability that the confidence interval
  will contain the true value of \eqn{\theta} may be off by \eqn{k/N} instead of
  \eqn{k/\sqrt{N}}.  The two-sided bias-corrected and accelerated confidence interval is
  computed as:
  \deqn{[\hat{G}^{-1}(\alpha_1), \; \hat{G}^{-1}(\alpha_2)] \;\;\;\;\;\; (45)}
  where
  \deqn{\alpha_1 = \Phi[\hat{z}_0 + \frac{\hat{z}_0 + z_{\alpha/2}}{1 - \hat{a}(z_0 + z_{\alpha/2})}] \;\;\;\;\;\; (46)}
  \deqn{\alpha_2 = \Phi[\hat{z}_0 + \frac{\hat{z}_0 + z_{1-\alpha/2}}{1 - \hat{a}(z_0 + z_{1-\alpha/2})}] \;\;\;\;\;\; (47)}
  \deqn{\hat{z}_0 = \Phi^{-1}[\hat{G}(\hat{\theta})] \;\;\;\;\;\; (48)}
  \deqn{\hat{a} = \frac{\sum_{i=1}^N (\hat{\theta}_{(\cdot)} - \hat{\theta}_{(i)})^3}{6[\sum_{i=1}^N (\hat{\theta}_{(\cdot)} - \hat{\theta}_{(i)})^2]^{3/2}} \;\;\;\;\;\; (49)}
  where the quantity \eqn{\hat{\theta}_{(i)}} denotes the estimate of \eqn{\theta} using
  all the values in \eqn{\underline{x}} except the \eqn{i}'th one, and
  \deqn{\hat{\theta}{(\cdot)} = \frac{1}{N} \sum_{i=1}^N \hat{\theta_{(i)}} \;\;\;\;\;\; (50)}
  A one-sided lower confidence interval is given by:
  \deqn{[\hat{G}^{-1}(\alpha_1), \; \infty] \;\;\;\;\;\; (51)}
  and a one-sided upper confidence interval is given by:
  \deqn{[0, \; \hat{G}^{-1}(\alpha_2)] \;\;\;\;\;\; (52)}
  where \eqn{\alpha_1} and \eqn{\alpha_2} are computed as for a two-sided confidence
  interval, except \eqn{\alpha/2} is replaced with \eqn{\alpha} in Equations (51) and (52).

  The constant \eqn{\hat{z}_0} incorporates the bias correction, and the constant
  \eqn{\hat{a}} is the acceleration constant.  The term \dQuote{acceleration} refers
  to the rate of change of the standard error of the estimate of \eqn{\theta} with
  respect to the true value of \eqn{\theta} (Efron and Tibshirani, 1993, p.186).  For a
  normal (Gaussian) distribution, the standard error of the estimate of \eqn{\theta}
  does not depend on the value of \eqn{\theta}, hence the acceleration constant is not
  really necessary.

  When \code{ci.method="bootstrap"}, the function \code{elnormAltCensored} computes both
  the percentile method and bias-corrected and accelerated method bootstrap confidence
  intervals.

  This method of constructing confidence intervals for censored data was studied by
  Shumway et al. (1989).
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
  standard deviation \bold{on the original scale, not the log-scale,} when data are
  subjected to single or multiple censoring.  See, for example, Singh et al. (2006).
}
\seealso{
  \code{\link{LognormalAlt}}, \code{\link{elnormAlt}},
  \code{\link{elnormCensored}}, \code{\link{enormCensored}},
  \code{\link{estimateCensored.object}}.
}
\examples{
  # Chapter 15 of USEPA (2009) gives several examples of estimating the mean
  # and standard deviation of a lognormal distribution on the log-scale using
  # manganese concentrations (ppb) in groundwater at five background wells.
  # In EnvStats these data are stored in the data frame
  # EPA.09.Ex.15.1.manganese.df.

  # Here we will estimate the mean and coefficient of variation
  # ON THE ORIGINAL SCALE using the MLE, QMVUE,
  # and robust ROS (imputation with Q-Q regression).

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


  # Now estimate the mean and coefficient of variation
  # using the MLE:
  #---------------------------------------------------

  with(EPA.09.Ex.15.1.manganese.df,
    elnormAltCensored(Manganese.ppb, Censored))

  #Results of Distribution Parameter Estimation
  #Based on Type I Censored Data
  #--------------------------------------------
  #
  #Assumed Distribution:            Lognormal
  #
  #Censoring Side:                  left
  #
  #Censoring Level(s):              2 5
  #
  #Estimated Parameter(s):          mean = 23.003987
  #                                 cv   =  2.300772
  #
  #Estimation Method:               MLE
  #
  #Data:                            Manganese.ppb
  #
  #Censoring Variable:              Censored
  #
  #Sample Size:                     25
  #
  #Percent Censored:                24%

  # Now compare the MLE with the QMVUE and the
  # estimator based on robust ROS
  #-------------------------------------------

  with(EPA.09.Ex.15.1.manganese.df,
    elnormAltCensored(Manganese.ppb, Censored))$parameters
  #     mean        cv
  #23.003987  2.300772

  with(EPA.09.Ex.15.1.manganese.df,
    elnormAltCensored(Manganese.ppb, Censored,
    method = "qmvue"))$parameters
  #     mean        cv
  #21.566945  1.841366

  with(EPA.09.Ex.15.1.manganese.df,
    elnormAltCensored(Manganese.ppb, Censored,
    method = "rROS"))$parameters
  #     mean        cv
  #19.886180  1.298868

  #----------

  # The method used to estimate quantiles for a Q-Q plot is
  # determined by the argument prob.method.  For the function
  # elnormCensoredAlt, for any estimation method that involves
  # Q-Q regression, the default value of prob.method is
  # "hirsch-stedinger" and the default value for the
  # plotting position constant is plot.pos.con=0.375.

  # Both Helsel (2012) and USEPA (2009) also use the Hirsch-Stedinger
  # probability method but set the plotting position constant to 0.

  with(EPA.09.Ex.15.1.manganese.df,
    elnormAltCensored(Manganese.ppb, Censored,
    method = "rROS", plot.pos.con = 0))$parameters
  #     mean        cv
  #19.827673  1.304725

  #----------

  # Using the same data as above, compute a confidence interval
  # for the mean using the profile-likelihood method.

  with(EPA.09.Ex.15.1.manganese.df,
    elnormAltCensored(Manganese.ppb, Censored, ci = TRUE))

  #Results of Distribution Parameter Estimation
  #Based on Type I Censored Data
  #--------------------------------------------
  #
  #Assumed Distribution:            Lognormal
  #
  #Censoring Side:                  left
  #
  #Censoring Level(s):              2 5
  #
  #Estimated Parameter(s):          mean = 23.003987
  #                                 cv   =  2.300772
  #
  #Estimation Method:               MLE
  #
  #Data:                            Manganese.ppb
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
  #Confidence Interval:             LCL = 12.37629
  #                                 UCL = 69.87694
}
\keyword{ distribution }
\keyword{ htest }
