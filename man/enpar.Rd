\name{enpar}
\alias{enpar}
\title{
  Estimate Mean, Standard Deviation, and Standard Error Nonparametrically
}
\description{
  Estimate the mean, standard deviation, and standard error of the mean
  nonparametrically given a sample of data, and optionally construct
  a confidence interval for the mean.
}
\usage{
  enpar(x, ci = FALSE, ci.method = "bootstrap", ci.type = "two-sided", 
      conf.level = 0.95, pivot.statistic = "z", n.bootstraps = 1000, seed = NULL)
}
\arguments{
  \item{x}{
  numeric vector of observations.
  Missing (\code{NA}), undefined (\code{NaN}), and
  infinite (\code{Inf}, \code{-Inf}) values are allowed but will be removed.
}
  \item{ci}{
  logical scalar indicating whether to compute a confidence interval for the
  mean.  The default value is \code{ci=FALSE}.
}
  \item{ci.method}{
  character string indicating what method to use to construct the confidence interval
  for the mean.  The possible values are
  \code{"bootstrap"} (based on bootstrapping; the default), and 
  \code{"normal.approx"} (normal approximation).  
  See the \bold{DETAILS} section for more information.
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
  \item{pivot.statistic}{
  character string indicating which statistic to use for the confidence interval
  for the mean when \code{ci.method="normal.approx"}.  Possible values are
  \code{"z"} (confidence interval based on the z-statistic; the default), and
  \code{"t"} (confidence interval based on the t-statistic).
  This argument is ignored if \code{ci=FALSE} or \cr
  \code{ci.method="bootstrap"}.
}
  \item{n.bootstraps}{
  numeric scalar indicating how many bootstraps to use to construct the
  confidence interval for the mean.  This argument is ignored if 
  \code{ci=FALSE} or \cr
  \code{ci.method="normal.approx"}.
}
  \item{seed}{
  integer supplied to the function \code{\link[base]{set.seed}} and used when \cr
  \code{ci=TRUE} and \code{ci.method="bootstrap"}.  The default value is
  \code{seed=NULL}, in which case the current value of \code{.Random.seed} 
  is used.  This argument is ignored if \code{ci=FALSE} or 
  \code{ci.method="normal.approx"}.  This argument is necessary to create 
  reproducible results for the bootstrapped confidence intervals 
  (see the \bold{EXAMPLES} section).
}
}
\details{
  Let \eqn{\underline{x} = (x_1, x_2, \ldots, x_N)} denote a vector of \eqn{N}
  observations from some distribution with mean \eqn{\mu} and standard 
  deviation \eqn{\sigma}.
  \cr

  \bold{Estimation} \cr
  Unbiased and consistent estimators of the mean and variance are:
  \deqn{\hat{\mu} = \bar{x} = \frac{1}{n} \sum_{i=1}^n x_i \;\;\;\; (1)}
  \deqn{\hat{\sigma}^2 = s^2 = \frac{1}{n-1} \sum_{i=1}^n (x_i - \bar{x})^2 \;\;\;\; (2)}
  A consistent (but not unbiased) estimate of the standard deviation is given by 
  the square root of the estimated variance above:
  \deqn{\hat{\sigma} = s \;\;\;\; (3)}
  It can be shown that the variance of the sample mean is given by:
  \deqn{\sigma^2_{\hat{\mu}} = \sigma^2_{\bar{x}} = \frac{\sigma^2}{n} \;\;\;\; (4)}
  so the standard deviation of the sample mean (usually called the \emph{standard error}) 
  can be estimated by:
  \deqn{\hat{\sigma}_{\hat{\mu}} = \hat{\sigma}_{\bar{x}} = \frac{s}{\sqrt{n}} \;\;\;\; (5)}
  \cr

  \bold{Confidence Intervals} \cr
  This section explains how confidence intervals for the mean \eqn{\mu} are
  computed.
  \cr

  \emph{Normal Approximation} (\code{ci.method="normal.approx"}) \cr
  This method constructs approximate \eqn{(1-\alpha)100\%} confidence intervals for
  \eqn{\mu} based on the assumption that the estimator of \eqn{\mu}, i.e., the 
  sample mean, is approximately normally distributed.  That is, a two-sided 
  \eqn{(1-\alpha)100\%} confidence interval for \eqn{\mu} is constructed as:
  \deqn{[\hat{\mu} - t_{1-\alpha/2, m-1}\hat{\sigma}_{\hat{\mu}}, \; \hat{\mu} + t_{1-\alpha/2, m-1}\hat{\sigma}_{\hat{\mu}}] \;\;\;\; (6)}
  where \eqn{\hat{\mu}} denotes the estimate of \eqn{\mu},
  \eqn{\hat{\sigma}_{\hat{\mu}}} denotes the estimated asymptotic standard
  deviation of the estimator of \eqn{\mu}, \eqn{m} denotes the assumed sample
  size for the confidence interval, and \eqn{t_{p,\nu}} denotes the \eqn{p}'th
  quantile of \link[stats:TDist]{Student's t-distribuiton} with \eqn{\nu}
  degrees of freedom.  One-sided confidence intervals are computed in a
  similar fashion.

  When \code{pivot.statistic="z"}, the \eqn{p}'th quantile from the
  \link[stats:Normal]{standard normal distribution} is used in place of the
  \eqn{p}'th quantile from Student's t-distribution.
  \cr

  \emph{Bootstrap and Bias-Corrected Bootstrap Approximation} (\code{ci.method="bootstrap"}) \cr
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
      original sample.

    \item Estimate \eqn{\mu} based on the bootstrap sample created in Step 1, using
      the same method that was used to estimate \eqn{\mu} using the original
      observations in \eqn{\underline{x}}. Because the bootstrap sample usually
      does not match the original sample, the estimate of \eqn{\mu} based on the
      bootstrap sample will usually differ from the original estimate based on
      \eqn{\underline{x}}.  For the bootstrap-t method (see below), this step also
      involves estimating the standard error of the estimate of the mean and
      computing the statistic 
      \eqn{T = (\hat{\mu}_B - \hat{\mu}) / \hat{\sigma}_{\hat{\mu}_B}}
      where \eqn{\hat{\mu}} denotes the estimate of the mean based on the original sample,
      and \eqn{\hat{\mu}_B} and \eqn{\hat{\sigma}_{\hat{\mu}_B}} denote the estimate of
      the mean and estimate of the standard error of the estimate of the mean based on
      the bootstrap sample.

    \item Repeat Steps 1 and 2 \eqn{B} times, where \eqn{B} is some large number.
      For the function \code{enpar}, the number of bootstraps \eqn{B} is
      determined by the argument \code{n.bootstraps} (see the section \bold{ARGUMENTS} above).
      The default value of \code{n.bootstraps} is \code{1000}.

    \item Use the \eqn{B} estimated values of \eqn{\mu} to compute the empirical
      cumulative distribution function of the estimator of \eqn{\mu} or to compute
      the empirical cumulative distribution function of the statistic \eqn{T}
      (see \code{\link{ecdfPlot}}), and then create a confidence interval for \eqn{\mu}
      based on this estimated cdf.
  }

  The two-sided percentile interval (Efron and Tibshirani, 1993, p.170) is computed as:
  \deqn{[\hat{G}^{-1}(\frac{\alpha}{2}), \; \hat{G}^{-1}(1-\frac{\alpha}{2})] \;\;\;\;\;\; (7)}
  where \eqn{\hat{G}(t)} denotes the empirical cdf of \eqn{\hat{\mu}_B} evaluated at \eqn{t}
  and thus \eqn{\hat{G}^{-1}(p)} denotes the \eqn{p}'th empirical quantile of the
  distribution of \eqn{\hat{\mu}_B}, that is, the \eqn{p}'th quantile associated with the
  empirical cdf.  Similarly, a one-sided lower
  confidence interval is computed as:
  \deqn{[\hat{G}^{-1}(\alpha), \; \infty] \;\;\;\;\;\; (8)}
  and a one-sided upper confidence interval is computed as:
  \deqn{[-\infty, \; \hat{G}^{-1}(1-\alpha)] \;\;\;\;\;\; (9)}
  The function \code{enpar} calls the \R function \code{\link[stats]{quantile}}
  to compute the empirical quantiles used in Equations (7)-(9).

  The percentile method bootstrap confidence interval is only first-order
  accurate (Efron and Tibshirani, 1993, pp.187-188), meaning that the probability
  that the confidence interval will contain the true value of \eqn{\mu} can be
  off by \eqn{k/\sqrt{N}}, where \eqn{k} is some constant.  Efron and Tibshirani
  (1993, pp.184--188) proposed a bias-corrected and accelerated interval that is
  second-order accurate, meaning that the probability that the confidence interval
  will contain the true value of \eqn{\mu} may be off by \eqn{k/N} instead of
  \eqn{k/\sqrt{N}}.  The two-sided bias-corrected and accelerated confidence interval is
  computed as:
  \deqn{[\hat{G}^{-1}(\alpha_1), \; \hat{G}^{-1}(\alpha_2)] \;\;\;\;\;\; (10)}
  where
  \deqn{\alpha_1 = \Phi[\hat{z}_0 + \frac{\hat{z}_0 + z_{\alpha/2}}{1 - \hat{a}(z_0 + z_{\alpha/2})}] \;\;\;\;\;\; (11)}
  \deqn{\alpha_2 = \Phi[\hat{z}_0 + \frac{\hat{z}_0 + z_{1-\alpha/2}}{1 - \hat{a}(z_0 + z_{1-\alpha/2})}] \;\;\;\;\;\; (12)}
  \deqn{\hat{z}_0 = \Phi^{-1}[\hat{G}(\hat{\mu})] \;\;\;\;\;\; (13)}
  \deqn{\hat{a} = \frac{\sum_{i=1}^N (\hat{\mu}_{(\cdot)} - \hat{\mu}_{(i)})^3}{6[\sum_{i=1}^N (\hat{\mu}_{(\cdot)} - \hat{\mu}_{(i)})^2]^{3/2}} \;\;\;\;\;\; (14)}
  where the quantity \eqn{\hat{\mu}_{(i)}} denotes the estimate of \eqn{\mu} using
  all the values in \eqn{\underline{x}} except the \eqn{i}'th one, and
  \deqn{\hat{\mu}{(\cdot)} = \frac{1}{N} \sum_{i=1}^N \hat{\mu_{(i)}} \;\;\;\;\;\; (15)}
  A one-sided lower confidence interval is given by:
  \deqn{[\hat{G}^{-1}(\alpha_1), \; \infty] \;\;\;\;\;\; (16)}
  and a one-sided upper confidence interval is given by:
  \deqn{[-\infty, \; \hat{G}^{-1}(\alpha_2)] \;\;\;\;\;\; (17)}
  where \eqn{\alpha_1} and \eqn{\alpha_2} are computed as for a two-sided confidence
  interval, except \eqn{\alpha/2} is replaced with \eqn{\alpha} in Equations (11) and (12).

  The constant \eqn{\hat{z}_0} incorporates the bias correction, and the constant
  \eqn{\hat{a}} is the acceleration constant.  The term \dQuote{acceleration} refers
  to the rate of change of the standard error of the estimate of \eqn{\mu} with
  respect to the true value of \eqn{\mu} (Efron and Tibshirani, 1993, p.186).  For a
  normal (Gaussian) distribution, the standard error of the estimate of \eqn{\mu}
  does not depend on the value of \eqn{\mu}, hence the acceleration constant is not
  really necessary.

  For the bootstrap-t method, the two-sided confidence interval
  (Efron and Tibshirani, 1993, p.160) is computed as:
  \deqn{[\hat{\mu} - t_{1-\alpha/2}\hat{\sigma}_{\hat{\mu}}, \; \hat{\mu} - t_{\alpha/2}\hat{\sigma}_{\hat{\mu}}] \;\;\;\;\;\; (18)}
  where \eqn{\hat{\mu}} and \eqn{\hat{\sigma}_{\hat{\mu}}} denote the estimate of the mean
  and standard error of the estimate of the mean based on the original sample, and
  \eqn{t_p} denotes the \eqn{p}'th empirical quantile of the bootstrap distribution of
  the statistic \eqn{T}.  Similarly, a one-sided lower confidence interval is computed as:
  \deqn{[\hat{\mu} - t_{1-\alpha}\hat{\sigma}_{\hat{\mu}}, \; \infty] \;\;\;\;\;\; (19)}
  and a one-sided upper confidence interval is computed as:
  \deqn{[-\infty, \; \hat{\mu} - t_{\alpha}\hat{\sigma}_{\hat{\mu}}] \;\;\;\;\;\; (20)}

  When \code{ci.method="bootstrap"}, the function \code{enpar} computes
  the percentile method, bias-corrected and accelerated method, and bootstrap-t
  bootstrap confidence intervals.  The percentile method is transformation respecting,
  but not second-order accurate.  The bootstrap-t method is second-order accurate, but not
  transformation respecting.  The bias-corrected and accelerated method is both
  transformation respecting and second-order accurate (Efron and Tibshirani, 1993, p.188).
}
\value{
  a list of class \code{"estimate"} containing the estimated parameters
  and other information.  See \code{\link{estimate.object}} for details.
}
\references{
  Efron, B. (1979).  Bootstrap Methods: Another Look at the Jackknife.
  \emph{The Annals of Statistics} \bold{7}, 1--26.

  Efron, B., and R.J. Tibshirani. (1993).  \emph{An Introduction to the Bootstrap}.
  Chapman and Hall, New York, 436pp.
}
\author{
    Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  The function \code{enpar} is related to the companion function 
  \code{\link{enparCensored}} for censored data.  To estimate the median and 
  compute a confidence interval, use \code{\link{eqnpar}}.

  The result of the call to \code{enpar} with \code{ci.method="normal.approx"} 
  and \code{pivot.statistic="t"} produces the same result as the call to 
  \code{\link{enorm}} with \code{ci.param="mean"}.
}
\seealso{
  \code{\link{enparCensored}}, \code{\link{eqnpar}}, \code{\link{enorm}}, 
  \code{\link{mean}}, \code{\link{sd}}, \code{\link{estimate.object}}.
}
\examples{
  # The data frame ACE.13.TCE.df contains observations on
  # Trichloroethylene (TCE) concentrations (mg/L) at
  # 10 groundwater monitoring wells before and after remediation.
  #
  # Compute the mean concentration for each period along with
  # a 95% bootstrap BCa confidence interval for the mean.
  #
  # NOTE: Use of the argument "seed" is necessary to reproduce this example.
  #
  # Before remediation: 21.6 [14.2, 30.1]
  # After remediation:   3.6 [ 1.6,  5.7]

  with(ACE.13.TCE.df,
    enpar(TCE.mg.per.L[Period=="Before"], ci = TRUE, seed = 476))

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            None
  #
  #Estimated Parameter(s):          mean    = 21.62400
  #                                 sd      = 13.51134
  #                                 se.mean =  4.27266
  #
  #Estimation Method:               Sample Mean
  #
  #Data:                            TCE.mg.per.L[Period == "Before"]
  #
  #Sample Size:                     10
  #
  #Confidence Interval for:         mean
  #
  #Confidence Interval Method:      Bootstrap
  #
  #Number of Bootstraps:            1000
  #
  #Confidence Interval Type:        two-sided
  #
  #Confidence Level:                95%
  #
  #Confidence Interval:             Pct.LCL = 13.95560
  #                                 Pct.UCL = 29.79510
  #                                 BCa.LCL = 14.16080
  #                                 BCa.UCL = 30.06848
  #                                 t.LCL   = 12.41945
  #                                 t.UCL   = 32.47306
  
  #----------

  with(ACE.13.TCE.df, 
    enpar(TCE.mg.per.L[Period=="After"], ci = TRUE, seed = 543))

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            None
  #
  #Estimated Parameter(s):          mean    = 3.632900
  #                                 sd      = 3.554419
  #                                 se.mean = 1.124006
  #
  #Estimation Method:               Sample Mean
  #
  #Data:                            TCE.mg.per.L[Period == "After"]
  #
  #Sample Size:                     10
  #
  #Confidence Interval for:         mean
  #
  #Confidence Interval Method:      Bootstrap
  #
  #Number of Bootstraps:            1000
  #
  #Confidence Interval Type:        two-sided
  #
  #Confidence Level:                95%
  #
  #Confidence Interval:             Pct.LCL = 1.833843
  #                                 Pct.UCL = 5.830230
  #                                 BCa.LCL = 1.631655
  #                                 BCa.UCL = 5.677514
  #                                 t.LCL   = 1.683791
  #                                 t.UCL   = 8.101829
}
\keyword{ distribution }
\keyword{ htest }
