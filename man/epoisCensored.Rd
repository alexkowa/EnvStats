\name{epoisCensored}
\alias{epoisCensored}
\title{
  Estimate Mean of a Poisson Distribution Based on Type I Censored Data
}
\description{
  Estimate the mean of a \link[stats:Poisson]{Poisson distribution} given a
  sample of data that has been subjected to Type I censoring, and optionally
  construct a confidence interval for the mean.
}
\usage{
  epoisCensored(x, censored, method = "mle", censoring.side = "left",
    ci = FALSE, ci.method = "profile.likelihood", ci.type = "two-sided",
    conf.level = 0.95, n.bootstraps = 1000, pivot.statistic = "z",
    ci.sample.size = sum(!censored))
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
  character string specifying the method of estimation.  The possible values are:
  \code{"mle"} (maximum likelihood; the default), and
  \code{"half.cen.level"} (moment estimation based on setting the censored
  observations to half the censoring level).
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
  for the mean.  The possible values are \code{"profile.likelihood"}
  (profile likelihood; the default),
  \code{"normal.approx"} (normal approximation),
  and
  \code{"bootstrap"} (based on bootstrapping).
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
  of the confidence interval for the mean when \code{ci.method="normal.approx"}
  (see the DETAILS section).  The possible
  values are \code{pivot.statistic="z"} (the default) and \code{pivot.statistic="t"}.
  When \code{pivot.statistic="t"} you may supply the argument
  \code{ci.sample size} (see below).  The argument \code{pivot.statistic} is
  ignored if \code{ci=FALSE}.
}
  \item{ci.sample.size}{
  numeric scalar indicating what sample size to assume to construct the
  confidence interval for the mean if \code{pivot.statistic="t"} and
  \code{ci.method="normal.approx"}.  The default value is the number of
  uncensored observations.
}
}
\details{
  If \code{x} or \code{censored} contain any missing (\code{NA}), undefined (\code{NaN}) or
  infinite (\code{Inf}, \code{-Inf}) values, they will be removed prior to
  performing the estimation.

  Let \eqn{\underline{x}} denote a vector of \eqn{N} observations from a
  \link[stats:Poisson]{Poisson distribution} with mean
  \code{lambda=}\eqn{\lambda}.

  Assume \eqn{n} (\eqn{0 < n < N}) of these
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

  \bold{Estimation} \cr

  \emph{Maximum Likelihood Estimation} (\code{method="mle"}) \cr
  For Type I left censored data, the likelihood function is given by:
  \deqn{L(\lambda | \underline{x}) = {N \choose c_1 c_2 \ldots c_k n} \prod_{j=1}^k [F(T_j)]^{c_j} \prod_{i \in \Omega} f[x_{(i)}] \;\;\;\;\;\; (3)}
  where \eqn{f} and \eqn{F} denote the probability density function (pdf) and
  cumulative distribution function (cdf) of the population
  (Cohen, 1963; Cohen, 1991, pp.6, 50).  That is,
  \deqn{f(t) =  \frac{e^{-\lambda} \lambda^t}{t!}, \; x = 0, 1, 2, \ldots \;\;\;\;\;\; (4)}
  \deqn{F(t) = \sum_{i=0}^t f(i) = \sum_{i=0}^t \frac{e^{-\lambda} \lambda^i}{i!} \;\;\;\;\;\; (5)}
  (Johnson et al., 1992, p.151).  For left singly censored data, equation (3)
  simplifies to:
  \deqn{L(\lambda | \underline{x}) = {N \choose c} [F(T)]^{c} \prod_{i = c+1}^n f[x_{(i)}] \;\;\;\;\;\; (6)}

  Similarly, for Type I right censored data, the likelihood function is given by:
  \deqn{L(\lambda | \underline{x}) = {N \choose c_1 c_2 \ldots c_k n} \prod_{j=1}^k [1 - F(T_j)]^{c_j} \prod_{i \in \Omega} f[x_{(i)}] \;\;\;\;\;\; (7)}
  and for right singly censored data this simplifies to:
  \deqn{L(\lambda | \underline{x}) = {N \choose c} [1 - F(T)]^{c} \prod_{i = 1}^n f[x_{(i)}] \;\;\;\;\;\; (8)}

  The maximum likelihood estimators are computed by maximizing the likelihood function.
  For right-censored data, taking the derivative of the log-likelihood function
  with respect to \eqn{\lambda} and setting this to 0 produces the following equation:
  \deqn{\bar{x} = \lambda \{1 - \sum_{j=1}^K \frac{c_j}{n} [\frac{f(T_j)}{1-F(T_j)}] \} \;\;\;\;\;\; (9)}
  where
  \deqn{\bar{x} = \frac{1}{n} \sum{i \in \Omega} x_i \;\;\;\;\;\; (10)}
  Note that the quantity defined in equation (10) is simply the mean of the uncensored observations.

  For left-censored data, taking the derivative of the log-likelihood function with
  respect to \eqn{\lambda} and setting this to 0 produces the following equation:
  \deqn{\bar{x} = \lambda \{1 + \sum_{j=1}^K \frac{c_j}{n} [\frac{f(T_j - 1)}{F(T_j - 1)}] \} \;\;\;\;\;\; (11)}

  The function \code{epoisCensored} computes the maximum likelihood estimator
  of \eqn{\lambda} by solving Equation (9) (right-censored data) or
  Equation (11) (left-censored data); it uses the sample mean of
  the uncensored observations as the initial value.
  \cr

  \emph{Setting Censored Observations to Half the Censoring Level} (\code{method="half.cen.level"}) \cr
  This method is applicable only to left censored data.
  This method involves simply replacing all the censored observations with half their
  detection limit, and then computing the mean and standard deviation with the usual
  formulas (see \code{\link{epois}}).

  This method is included only to allow comparison of this method to other methods.
  \bold{\emph{Setting left-censored observations to half the censoring level is not
  recommended}}.
  \cr
  \cr

  \bold{Confidence Intervals} \cr
  This section explains how confidence intervals for the mean \eqn{\lambda} are
  computed.
  \cr

  \emph{Likelihood Profile} (\code{ci.method="profile.likelihood"}) \cr
  This method was proposed by Cox (1970, p.88), and Venzon and Moolgavkar (1988)
  introduced an efficient method of computation.  This method is also discussed by
  Stryhn and Christensen (2003) and Royston (2007).
  The idea behind this method is to invert the likelihood-ratio test to obtain a
  confidence interval for the mean \eqn{\lambda}.  Equation (3) above
  shows the form of the likelihood function \eqn{L(\lambda | \underline{x})} for
  multiply left-censored data, and Equation (7) shows the function for
  multiply right-censored data.

  Following Stryhn and Christensen (2003), denote the maximum likelihood estimate
  of the mean by \eqn{\lambda^*}.  The likelihood
  ratio test statistic (\eqn{G^2}) of the hypothesis \eqn{H_0: \lambda = \lambda_0}
  (where \eqn{\lambda_0} is a fixed value) equals the drop in \eqn{2 log(L)} between the
  \dQuote{full} model and the reduced model with \eqn{\lambda} fixed at \eqn{\mu_0}, i.e.,
  \deqn{G^2 = 2 \{log[L(\lambda^*)] - log[L(\lambda_0)]\} \;\;\;\;\;\; (11)}.
  Under the null hypothesis, the test statistic \eqn{G^2} follows a
  \link[stats:Chisquare]{chi-squared distribution} with 1 degree of freedom.

  A two-sided \eqn{(1-\alpha)100\%} confidence interval for the mean \eqn{\lambda}
  consists of all values of \eqn{\lambda_0} for which the test is not significant at
  level \eqn{alpha}:
  \deqn{\lambda_0: G^2 \le \chi^2_{1, {1-\alpha}} \;\;\;\;\;\; (12)}
  where \eqn{\chi^2_{\nu, p}} denotes the \eqn{p}'th quantile of the
  \link[stats:Chisquare]{chi-squared distribution} with \eqn{\nu} degrees of freedom.
  One-sided lower and one-sided upper confidence intervals are computed in a similar
  fashion, except that the quantity \eqn{1-\alpha} in Equation (12) is replaced with
  \eqn{1-2\alpha}.
  \cr

  \emph{Normal Approximation} (\code{ci.method="normal.approx"}) \cr
  This method constructs approximate \eqn{(1-\alpha)100\%} confidence intervals for
  \eqn{\lambda} based on the assumption that the estimator of \eqn{\lambda} is
  approximately normally distributed.  That is, a two-sided \eqn{(1-\alpha)100\%}
  confidence interval for \eqn{\lambda} is constructed as:
  \deqn{[\hat{\lambda} - t_{1-\alpha/2, m-1}\hat{\sigma}_{\hat{\lambda}}, \; \hat{\lambda} + t_{1-\alpha/2, m-1}\hat{\sigma}_{\hat{\lambda}}] \;\;\;\; (13)}
  where \eqn{\hat{\lambda}} denotes the estimate of \eqn{\lambda},
  \eqn{\hat{\sigma}_{\hat{\lambda}}} denotes the estimated asymptotic standard
  deviation of the estimator of \eqn{\lambda}, \eqn{m} denotes the assumed sample
  size for the confidence interval, and \eqn{t_{p,\nu}} denotes the \eqn{p}'th
  quantile of \link[stats:TDist]{Student's t-distribuiton} with \eqn{\nu}
  degrees of freedom.  One-sided confidence intervals are computed in a
  similar fashion.

  The argument \code{ci.sample.size} determines the value of \eqn{m} and by
  default is equal to the number of uncensored observations.
  This is simply an ad-hoc method of constructing
  confidence intervals and is not based on any published theoretical results.

  When \code{pivot.statistic="z"}, the \eqn{p}'th quantile from the
  \link[stats:Normal]{standard normal distribution} is used in place of the
  \eqn{p}'th quantile from Student's t-distribution.

  When \eqn{\lambda} is estimated with the maximum likelihood estimator
  (\code{method="mle"}), the variance of \eqn{\hat{\lambda}} is
  estimated based on the inverse of the Fisher Information matrix.  When
  \eqn{\lambda} is estimated using the half-censoring-level method
  (\code{method="half.cen.level"}), the variance of \eqn{\hat{\lambda}} is
  estimated as:
  \deqn{\hat{\sigma}^2_{\hat{\lambda}} = \frac{\hat{\lambda}}{m} \;\;\;\;\;\; (14)}
  where \eqn{m} denotes the assumed sample size (see above).
  \cr

  \emph{Bootstrap and Bias-Corrected Bootstrap Approximation} (\code{ci.method="bootstrap"}) \cr
  The bootstrap is a nonparametric method of estimating the distribution
  (and associated distribution parameters and quantiles) of a sample statistic,
  regardless of the distribution of the population from which the sample was drawn.
  The bootstrap was introduced by Efron (1979) and a general reference is
  Efron and Tibshirani (1993).

  In the context of deriving an approximate \eqn{(1-\alpha)100\%} confidence interval
  for the population mean \eqn{\lambda}, the bootstrap can be broken down into the
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

    \item Estimate \eqn{\lambda} based on the bootstrap sample created in Step 1, using
      the same method that was used to estimate \eqn{\lambda} using the original
      observations in \eqn{\underline{x}}. Because the bootstrap sample usually
      does not match the original sample, the estimate of \eqn{\lambda} based on the
      bootstrap sample will usually differ from the original estimate based on
      \eqn{\underline{x}}.

    \item Repeat Steps 1 and 2 \eqn{B} times, where \eqn{B} is some large number.
      For the function \code{epoisCensored}, the number of bootstraps \eqn{B} is
      determined by the argument \code{n.bootstraps} (see the section ARGUMENTS above).
      The default value of \code{n.bootstraps} is \code{1000}.

    \item Use the \eqn{B} estimated values of \eqn{\lambda} to compute the empirical
      cumulative distribution function of this estimator of \eqn{\lambda} (see
      \code{\link{ecdfPlot}}), and then create a confidence interval for \eqn{\lambda}
      based on this estimated cdf.
  }

  The two-sided percentile interval (Efron and Tibshirani, 1993, p.170) is computed as:
  \deqn{[\hat{G}^{-1}(\frac{\alpha}{2}), \; \hat{G}^{-1}(1-\frac{\alpha}{2})] \;\;\;\;\;\; (15)}
  where \eqn{\hat{G}(t)} denotes the empirical cdf evaluated at \eqn{t} and thus
  \eqn{\hat{G}^{-1}(p)} denotes the \eqn{p}'th empirical quantile, that is,
  the \eqn{p}'th quantile associated with the empirical cdf.  Similarly, a one-sided lower
  confidence interval is computed as:
  \deqn{[\hat{G}^{-1}(\alpha), \; \infty] \;\;\;\;\;\; (16)}
  and a one-sided upper confidence interval is computed as:
  \deqn{[0, \; \hat{G}^{-1}(1-\alpha)] \;\;\;\;\;\; (17)}
  The function \code{epoisCensored} calls the \R function \code{\link[stats]{quantile}}
  to compute the empirical quantiles used in Equations (15)-(17).

  The percentile method bootstrap confidence interval is only first-order
  accurate (Efron and Tibshirani, 1993, pp.187-188), meaning that the probability
  that the confidence interval will contain the true value of \eqn{\lambda} can be
  off by \eqn{k/\sqrt{N}}, where \eqn{k}is some constant.  Efron and Tibshirani
  (1993, pp.184-188) proposed a bias-corrected and accelerated interval that is
  second-order accurate, meaning that the probability that the confidence interval
  will contain the true value of \eqn{\lambda} may be off by \eqn{k/N} instead of
  \eqn{k/\sqrt{N}}.  The two-sided bias-corrected and accelerated confidence interval is
  computed as:
  \deqn{[\hat{G}^{-1}(\alpha_1), \; \hat{G}^{-1}(\alpha_2)] \;\;\;\;\;\; (18)}
  where
  \deqn{\alpha_1 = \Phi[\hat{z}_0 + \frac{\hat{z}_0 + z_{\alpha/2}}{1 - \hat{a}(z_0 + z_{\alpha/2})}] \;\;\;\;\;\; (19)}
  \deqn{\alpha_2 = \Phi[\hat{z}_0 + \frac{\hat{z}_0 + z_{1-\alpha/2}}{1 - \hat{a}(z_0 + z_{1-\alpha/2})}] \;\;\;\;\;\; (20)}
  \deqn{\hat{z}_0 = \Phi^{-1}[\hat{G}(\hat{\lambda})] \;\;\;\;\;\; (21)}
  \deqn{\hat{a} = \frac{\sum_{i=1}^N (\hat{\lambda}_{(\cdot)} - \hat{\lambda}_{(i)})^3}{6[\sum_{i=1}^N (\hat{\lambda}_{(\cdot)} - \hat{\lambda}_{(i)})^2]^{3/2}} \;\;\;\;\;\; (22)}
  where the quantity \eqn{\hat{\lambda}_{(i)}} denotes the estimate of \eqn{\lambda} using
  all the values in \eqn{\underline{x}} except the \eqn{i}'th one, and
  \deqn{\hat{\lambda}{(\cdot)} = \frac{1}{N} \sum_{i=1}^N \hat{\lambda_{(i)}} \;\;\;\;\;\; (23)}
  A one-sided lower confidence interval is given by:
  \deqn{[\hat{G}^{-1}(\alpha_1), \; \infty] \;\;\;\;\;\; (24)}
  and a one-sided upper confidence interval is given by:
  \deqn{[0, \; \hat{G}^{-1}(\alpha_2)] \;\;\;\;\;\; (25)}
  where \eqn{\alpha_1} and \eqn{\alpha_2} are computed as for a two-sided confidence
  interval, except \eqn{\alpha/2} is replaced with \eqn{\alpha} in Equations (19) and (20).

  The constant \eqn{\hat{z}_0} incorporates the bias correction, and the constant
  \eqn{\hat{a}} is the acceleration constant.  The term \dQuote{acceleration} refers
  to the rate of change of the standard error of the estimate of \eqn{\lambda} with
  respect to the true value of \eqn{\lambda} (Efron and Tibshirani, 1993, p.186).  For a
  normal (Gaussian) distribution, the standard error of the estimate of \eqn{\lambda}
  does not depend on the value of \eqn{\lambda}, hence the acceleration constant is not
  really necessary.

  When \code{ci.method="bootstrap"}, the function \code{epoisCensored} computes both
  the percentile method and bias-corrected and accelerated method bootstrap confidence
  intervals.
}
\value{
  a list of class \code{"estimateCensored"} containing the estimated parameters
  and other information.  See \code{\link{estimateCensored.object}} for details.
}
\references{
  Cohen, A.C. (1991).  \emph{Truncated and Censored Samples}.  Marcel Dekker,
  New York, New York, 312pp.

  Cox, D.R. (1970).  \emph{Analysis of Binary Data}.  Chapman & Hall, London.  142pp.

  Efron, B. (1979).  Bootstrap Methods: Another Look at the Jackknife.
  \emph{The Annals of Statistics} \bold{7}, 1--26.

  Efron, B., and R.J. Tibshirani. (1993).  \emph{An Introduction to the Bootstrap}.
  Chapman and Hall, New York, 436pp.

  Forbes, C., M. Evans, N. Hastings, and B. Peacock. (2011).
  \emph{Statistical Distributions, Fourth Edition}.
  John Wiley and Sons, Hoboken, NJ.

  Helsel, D.R. (2012). \emph{Statistics for Censored Environmental Data Using Minitab and R,
  Second Edition}.  John Wiley & Sons, Hoboken, New Jersey.

  Johnson, N. L., S. Kotz, and A. Kemp. (1992).  \emph{Univariate Discrete
  Distributions, Second Edition}.  John Wiley and Sons, New York, Chapter 4.

  Millard, S.P., P. Dixon, and N.K. Neerchal. (2014; in preparation).
  \emph{Environmental Statistics with R}.  CRC Press, Boca Raton, Florida.

  Nelson, W. (1982).  \emph{Applied Life Data Analysis}.
  John Wiley and Sons, New York, 634pp.

  Royston, P. (2007).  Profile Likelihood for Estimation and Confdence Intervals.
  \emph{The Stata Journal} \bold{7}(3), pp. 376--387.

  Stryhn, H., and J. Christensen. (2003).  \emph{Confidence Intervals by the Profile
  Likelihood Method, with Applications in Veterinary Epidemiology}.  Contributed paper
  at ISVEE X (November 2003, Chile).
  \url{https://gilvanguedes.com/wp-content/uploads/2019/05/Profile-Likelihood-CI.pdf}.

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
  estimation.  Helsel (2012, Chapter 6) gives an excellent review of past studies
  of the properties of various estimators for parameters of a normal or lognormal
  distribution based on censored environmental data.

  In practice, it is better to use a confidence interval for the mean or a
  joint confidence region for the mean and standard deviation (or coefficient of
  variation), rather than rely on a single point-estimate of the mean.
  Few studies have been done to evaluate the performance of methods for constructing
  confidence intervals for the mean or joint confidence regions for the mean and
  coefficient of variation of a Poisson distribution when data are subjected to
  single or multiple censoring.
}
\seealso{
  \link[stats]{Poisson}, \code{\link{epois}}, \code{\link{estimateCensored.object}}.
}
\examples{
  # Generate 20 observations from a Poisson distribution with
  # parameter lambda=10, and censor the values less than 10.
  # Then generate 20 more observations from the same distribution
  # and censor the values less than 20.  Then estimate the mean
  # using the maximum likelihood method.
  # (Note: the call to set.seed simply allows you to reproduce this example.)

  set.seed(300)
  dat.1 <- rpois(20, lambda=10)
  censored.1 <- dat.1 < 10
  dat.1[censored.1] <- 10

  dat.2 <- rpois(20, lambda=10)
  censored.2 <- dat.2 < 20
  dat.2[censored.2] <- 20

  dat <- c(dat.1, dat.2)
  censored <- c(censored.1, censored.2)

  epoisCensored(dat, censored, ci = TRUE)

  #Results of Distribution Parameter Estimation
  #Based on Type I Censored Data
  #--------------------------------------------
  #
  #Assumed Distribution:            Poisson
  #
  #Censoring Side:                  left
  #
  #Censoring Level(s):              10 20
  #
  #Estimated Parameter(s):          lambda = 11.05402
  #
  #Estimation Method:               MLE
  #
  #Data:                            dat
  #
  #Censoring Variable:              censored
  #
  #Sample Size:                     40
  #
  #Percent Censored:                65%
  #
  #Confidence Interval for:         lambda
  #
  #Confidence Interval Method:      Profile Likelihood
  #
  #Confidence Interval Type:        two-sided
  #
  #Confidence Level:                95%
  #
  #Confidence Interval:             LCL =  9.842894
  #                                 UCL = 12.846484

  #----------

  # Clean up
  #---------
  rm(dat.1, censored.1, dat.2, censored.2, dat, censored)
}
\keyword{ distribution }
\keyword{ htest }

