\name{enparCensored}
\alias{enparCensored}
\title{
  Estimate Mean, Standard Deviation, and Standard Error Nonparametrically Based on Censored Data
}
\description{
  Estimate the mean, standard deviation, and standard error of the mean
  nonparametrically given a sample of data from a positive-valued distribution
  that has been subjected to left- or right-censoring, and optionally construct
  a confidence interval for the mean.
}
\usage{
  enparCensored(x, censored, censoring.side = "left", correct.se = TRUE, 
    restricted = FALSE, left.censored.min = "Censoring Level", 
    right.censored.max = "Censoring Level", ci = FALSE, 
    ci.method = "normal.approx", ci.type = "two-sided", conf.level = 0.95, 
    pivot.statistic = "t", ci.sample.size = "Total", n.bootstraps = 1000, 
    seed = NULL, warn = FALSE)
}
\arguments{
  \item{x}{
  numeric vector of positive-valued observations.
  Missing (\code{NA}), undefined (\code{NaN}), and
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
  \item{censoring.side}{
  character string indicating on which side the censoring occurs.  The possible
  values are \code{"left"} (the default) and \code{"right"}.
}
  \item{correct.se}{
  logical scalar indicating whether to multiply the estimated standard error
  by a factor to correct for bias.  The default value is \code{correct.se=TRUE}.
  See the DETAILS section below.
}
  \item{restricted}{
  logical scalar indicating whether to compute the restricted mean in the case when 
  the smallest censored value is less than or equal to the smallest uncensored value 
  (left-censored data) or the largest censored value is greater than or equal to the 
  largest uncensored value (right-censored data).  The default value is 
  \code{restricted=FALSE}.  See the DETAILS section for more information.
}
  \item{left.censored.min}{
  Only relevant for the case when \code{censoring.side="left"}, the smallest
  censored value is less than or equal to the smallest uncensored value, and \cr
  \code{restricted=TRUE}.  In this case, \code{left.censored.min} must be the 
  character string \code{"Censoring Level"}, or else a numeric scalar between 
  0 and the smallest censored value.  The default value is 
  \code{left.censored.min="Censoring Level"}.
  See the DETAILS section for more information.
}
  \item{right.censored.max}{
  Only relevant for the case when \code{censoring.side="right"}, the largest
  censored value is greater than or equal to the largest uncensored value, and \cr
  \code{restricted=TRUE}.  In this case, \code{right.censored.max} must be the 
  character string \code{"Censoring Level"}, or else a numeric scalar greater 
  than or equal to the largest censored value. The default value is 
  \code{right.censored.max="Censoring Level"}. 
  See the DETAILS section for more information.
}
  \item{ci}{
  logical scalar indicating whether to compute a confidence interval for the
  mean or variance.  The default value is \code{ci=FALSE}.
}
  \item{ci.method}{
  character string indicating what method to use to construct the confidence interval
  for the mean.  The possible values are
  \code{"normal.approx"} (normal approximation; the default), and
  \code{"bootstrap"} (based on bootstrapping).
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
  \code{"t"} (confidence interval based on the t-statistic; the default), and 
  \code{"z"} (confidence interval based on the z-statistic).  When 
  \code{pivot.statistic="t"} you may supply the argument \code{ci.sample size} 
  (see below).  This argument is ignored if \code{ci=FALSE}.
}
  \item{ci.sample.size}{
  character string indicating what sample size to assume when
  computing the confidence interval for the mean when \code{ci.method="normal.approx"} \cr
  and \code{pivot.statistic="t"}.  Possible values are \code{ci.sample.size="Total"} 
  (the total number of observations; the default), and \cr
  \code{ci.sample.size="Uncensored"} (the number of uncensored observations). 
  This argument is ignored if \code{ci=FALSE}, \code{ci.method="bootstrap"}, 
  or \code{pivot.statistic="z"}.
}
  \item{n.bootstraps}{
  numeric scalar indicating how many bootstraps to use to construct the
  confidence interval for the mean when \code{ci.type="bootstrap"}.  This
  argument is ignored if \code{ci=FALSE} or \code{ci.method="normal.approx"}.
}
  \item{seed}{
  integer supplied to the function \code{\link[base]{set.seed}} and used when 
  \code{ci=TRUE} and \cr
  \code{ci.method="bootstrap"}.  The default value is
  \code{seed=NULL}, in which case the current value of \code{.Random.seed} is used.
  This argument is ignored if \code{ci=FALSE} or \code{ci.method="normal.approx"}.  
  The \code{seed} argument is necessary in order to create reproducible results for 
  the bootstrapped confidence intervals (see the \bold{EXAMPLES} section).
}
  \item{warn}{
  logical scalar indicating whether to issue a notification in the case when a 
  restricted mean will be estimated, but setting the smallest censored value(s) 
  to an uncensored value (left-censored data) or setting the largest censored 
  value(s) to an uncensored value (right-censored data) results in no censored 
  values in the data.  In this case, the function \code{\link{enpar}} 
  is called.
}
}
\details{
  Let \eqn{\underline{x} = (x_1, x_2, \ldots, x_N)} denote a vector of \eqn{N}
  observations from some positive-valued distribution with mean
  \eqn{\mu} and standard deviation \eqn{\sigma}.
  Assume \eqn{n} (\eqn{0 < n < N}) of these
  observations are known and \eqn{c} (\eqn{c=N-n}) of these observations are
  all censored below (left-censored) or all censored above (right-censored) at
  \eqn{k} censoring levels
  \deqn{T_1, T_2, \ldots, T_k; \; k \ge 1 \;\;\;\;\;\; (1)}
  Let \eqn{y_1, y_2, \ldots, y_n} denote the \eqn{n} ordered uncensored
  observations, and let \eqn{r_1, r_2, \ldots, r_n} denote the order of these 
  uncensored observations within the context of all the observations (censored and 
  uncensored).  For example, if the left-censored data are \{<10, 14, 14, <15, 20\}, then 
  \eqn{y_1 = 14, y_2 = 14, y_3 = 20}, and \eqn{r_1 = 2, r_2 = 3, r_3 = 5}.  

  Let \eqn{y_1', y_2', \ldots, y_p'} denote the \eqn{p} ordered \emph{distinct} 
  uncensored observations, let \eqn{m_j} denote the number of detects at 
  \eqn{y_j'} (\eqn{j = 1, 2, \ldots, p}), and let \eqn{r_j'} denote the number of 
  \eqn{x_i \le y_j'}, i.e., the number of observations (censored and uncensored) 
  less than or equal to \eqn{y_j'} (\eqn{j = 1, 2, \ldots, p}).  For example, 
  if the left-censored data are \{<10, 14, 14, <15, 20\}, then 
  \eqn{y_1' = 14, y_2' = 20}, \eqn{m_1 = 2, m_2 = 1}, and \eqn{r_1' = 3, r_2' = 5}.
  \cr

  \bold{Estimation} \cr
  This section explains how the mean \eqn{\mu}, standard deviation \eqn{\sigma}, 
  and standard error of the mean \eqn{\hat{\sigma}_{\hat{\mu}}} are estimated, as well as 
  the restricted mean.
  \cr

  \emph{Estimating the Mean} \cr
  It can be shown that the mean of a positive-valued distribution is equal to the
  area under the survival curve (Klein and Moeschberger, 2003, p.33):
  \deqn{\mu = \int_0^\infty [1 - F(t)] dt = \int_0^\infty S(t) dt \;\;\;\;\;\; (2)}
  where \eqn{F(t)} denotes the cumulative distribution function evaluated at \eqn{t}
  and \eqn{S(t) = 1 - F(t)} denotes the survival function evaluated at \eqn{t}.
  When the Kaplan-Meier estimator is used to construct the survival function,
  you can use the area under this curve to estimate the mean of the distribution,
  and the estimator can be as efficient or more efficient than
  parametric estimators of the mean (Meier, 2004; Helsel, 2012; Lee and Wang, 2003).
  Let \eqn{\hat{F}(t)} denote the Kaplan-Meier estimator of the empirical
  cumulative distribution function (ecdf) evaluated at \eqn{t}, and let
  \eqn{\hat{S}(t) = 1 - \hat{F}(t)} denote the estimated survival function evaluated
  at \eqn{t}.  (See the help files for \code{\link{ecdfPlotCensored}} and
  \code{\link{qqPlotCensored}} for an explanation of how the Kaplan-Meier
  estimator of the ecdf is computed.)

  The formula for the estimated mean is given by (Lee and Wang, 2003, p. 74):
  \deqn{\hat{\mu} = \sum_{i=1}^{n} \hat{S}(y_{i-1}) (y_{i} - y_{i-1}) \;\;\;\;\;\; (3)}
  where \eqn{y_{0} = 0} and \eqn{\hat{S}(y_{0}) = 1} by definition.  It can be
  shown that this formula is eqivalent to:
  \deqn{\hat{\mu} = \sum_{i=1}^n y_{i} [\hat{F}(y_{i}) - \hat{F}(y_{i-1})] \;\;\;\;\;\; (4)}
  where \eqn{\hat{F}(y_{0}) = \hat{F}(0) = 0} by definition, and this is equivalent to:
  \deqn{\hat{\mu} = \sum_{i=1}^p y_i' [\hat{F}(y_i') - \hat{F}(y_{i-1}')] \;\;\;\;\;\; (5)}
  (USEPA, 2009, pp. 15--7 to 15--12; Beal, 2010; USEPA, 2022, pp. 128--129).
  \cr

  \emph{Estimating the Standard Deviation} \cr
  The formula for the estimated standard deviation is:
  \deqn{\hat{\sigma} = \{\sum_{i=1}^n (y_{i} - \hat{\mu})^2 [\hat{F}(y_{i}) - \hat{F}(y_{i-1})]\}^{1/2} \;\;\;\;\; (6)}
  which is equivalent to:
  \deqn{\hat{\sigma} = \{\sum_{i=1}^p (y_i' - \hat{\mu})^2 [\hat{F}(y_i') - \hat{F}(y_{i-1}')]\}^{1/2} \;\;\;\;\; (7)}
  (USEPA, 2009, p. 15-10; Beal, 2010).
  \cr

  \emph{Estimating the Standard Error of the Mean} \cr
  For left-censored data, the formula for the estimated standard error of the
  mean is:
  \deqn{\hat{\sigma}_{\hat{\mu}} = [\sum_{i=j}^{p-1} A_j^2 \frac{m_{j+1}}{r_{j+1}'(r_{j+1}' - m_{j+1})}]^{1/2} \;\;\;\;\;\; (8)}
  where
  \deqn{A_j = \sum_{i=1}^{j} (y_{i+1}' - y_i') \hat{F}(y_i') \;\;\;\;\;\; (9)}
  (Beal, 2010; USEPA, 2022, pp. 128--129).

  For rigth-censored data, the formula for the estimated standard error of the
  mean is:
  \deqn{\hat{\sigma}_{\hat{\mu}} = [\sum_{r=1}^{n-1} \frac{A_r^2}{(N-r)(N-r+1)}]^{1/2} \;\;\;\;\;\; (10)}
  where
  \deqn{A_r = \sum_{i=r}^{n-1} (y_{i+1} - y_{i}) \hat{S}(y_{i}) \;\;\;\;\;\; (11)}
  (Lee and Wang, 2003, p. 74).  

  Kaplan and Meier suggest using a bias correction of
  \eqn{n/(n-1)} for the estimated variance of the mean (Lee and Wang, 2003, p.75):
  \deqn{\hat{\sigma}_{\hat{\mu}, BC} = \sqrt{\frac{n}{n-1}} \;\; \hat{\sigma}_{\hat{\mu}} \;\;\;\;\;\; (12)}
  When \code{correct.se=TRUE} (the default), Equation (12) is used.  Beal (2010), 
  ProUCL 5.2.0 (USEPA, 2022), and the \code{kmms} function in the \bold{STAND} package 
  (Frome and Frome, 2015) all compute the bias-corrected estimate of the standard 
  error of the mean as well.
  \cr

  \emph{Estimating the Restricted Mean} \cr
  If the smallest value for left-censored data is censored and less than or equal to 
  the smallest uncensored value, then the estimated mean will be biased high, and
  if the largest value for right-censored data is censored and greater than or equal to
  the largest uncensored value, then the estimated mean will be biased low.  One solution 
  to this problem is to instead estimate what is called the \bold{\emph{restricted mean}} 
  (Miller, 1981; Lee and Wang, 2003, p. 74; Meier, 2004; Barker, 2009).

  To compute the restricted mean (\code{restricted=TRUE}), for left-censored data, 
  the smallest censored observation(s) are treated as observed, and set to the 
  smallest censoring level \cr
  (\code{left.censored.min="Censoring Level"}) or some other
  value less than the smallest censoring level and greater than 0, and then applying 
  the above formulas.  To compute the restricted mean for right-censored data, 
  the largest censored observation(s) are treated as observed and set to the 
  censoring level (\code{right.censored.max="Censoring Level"}) or some value 
  greater than the largest censoring level.

  ProUCL 5.2.0 (USEPA, 2022, pp. 128--129) and Beal (2010) do not compute the restricted 
  mean in cases where it could be applied, whereas USEPA (2009, pp. 15--7 to 15--12) and 
  the \code{kmms} function in Version 2.0 of the R package \bold{STAND} 
  (Frome and Frome, 2015) do compute the restricted mean and set the smallest 
  censored observation(s) equal to the censoring level (i.e., what 
  \code{enparCensored} does when \code{restricted=TRUE} and 
  \code{left.censored.min="Censoring Level"}). 

  To be consistent with ProUCL 5.2.0, by default the function \code{enparCensored} 
  does not compute the restricted mean (i.e., \code{restricted=FALSE}).  Note that there  
  are instances where ProUCL 5.2.0 does not agree with \code{enparCensored}, and it is 
  not clear why. It should 
  be noted that when the restricted mean is computed, the number of uncensored 
  observations increases because the smallest (left-censored) or largest 
  (right-censored) censored observation(s) is/are set to a specified value and 
  treated as uncensored.  The \code{kmms} function in Version 2.0 of the 
  \bold{STAND} package (Frome and Frome, 2015) is inconsistent in how it treats the 
  number of uncensored observations when computing estimates associated with the 
  restricted mean.  Although \code{kmms} sets the smallest censored observations to the 
  observed censoring level and treats them as not censored, when it computes 
  the bias correction factor for the standard error of the mean, it assumes those 
  observations are still censored (see the EXAMPLES section below).

  In the unusual case when a restricted mean will be estimated and setting the 
  smallest censored value(s) to an uncensored value (left-censored data), or 
  setting the largest censored value(s) to an uncensored value 
  (right-censored data), results in no censored values in the data, the Kaplan-Meier 
  estimate of the mean reduces to the sample mean, so the function 
  \code{\link{enpar}} is called and, if \code{warn=TRUE}, a warning is returned.
  \cr

  \bold{Confidence Intervals} \cr
  This section explains how confidence intervals for the mean \eqn{\mu} are
  computed.
  \cr

  \emph{Normal Approximation} (\code{ci.method="normal.approx"}) \cr
  This method constructs approximate \eqn{(1-\alpha)100\%} confidence intervals for
  \eqn{\mu} based on the assumption that the estimator of \eqn{\mu} is
  approximately normally distributed.  That is, a two-sided \eqn{(1-\alpha)100\%}
  confidence interval for \eqn{\mu} is constructed as:
  \deqn{[\hat{\mu} - t_{1-\alpha/2, v-1}\hat{\sigma}_{\hat{\mu}}, \; \hat{\mu} + t_{1-\alpha/2, v-1}\hat{\sigma}_{\hat{\mu}}] \;\;\;\; (13)}
  where \eqn{\hat{\mu}} denotes the estimate of \eqn{\mu},
  \eqn{\hat{\sigma}_{\hat{\mu}}} denotes the estimated asymptotic standard
  deviation of the estimator of \eqn{\mu}, \eqn{v} denotes the assumed sample
  size for the confidence interval, and \eqn{t_{p,\nu}} denotes the \eqn{p}'th
  quantile of \link[stats:TDist]{Student's t-distribuiton} with \eqn{\nu}
  degrees of freedom.  One-sided confidence intervals are computed in a
  similar fashion.

  The argument \code{ci.sample.size} determines the value of \eqn{v}.  
  The possible values are the total number of observations, \eqn{N} 
  (\code{ci.sample.size="Total"}), or the number of uncensored observations, 
  \eqn{n} (\code{ci.sample.size="Uncensored"}).  To be consistent with ProUCL 5.2.0, 
  in \code{enparCensored} the default value is the total number of observations.  
  The \code{kmms} function in the \bold{STAND} package, on the other hand, 
  uses the number of uncensored observations.

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
      original sample (e.g., the number of censored observations in the bootstrap
      sample will often differ from the number of censored observations in the
      original sample).

    \item Estimate \eqn{\mu} based on the bootstrap sample created in Step 1, using
      the same method that was used to estimate \eqn{\mu} using the original
      observations in \eqn{\underline{x}}. Because the bootstrap sample usually
      does not match the original sample, the estimate of \eqn{\mu} based on the
      bootstrap sample will usually differ from the original estimate based on
      \eqn{\underline{x}}.  For the bootstrap-t method (see below), this step also
      involves estimating the standard error of the estimate of the mean and
      computing the statistic \eqn{T = (\hat{\mu}_B - \hat{\mu}) / \hat{\sigma}_{\hat{\mu}_B}}
      where \eqn{\hat{\mu}} denotes the estimate of the mean based on the original sample,
      and \eqn{\hat{\mu}_B} and \eqn{\hat{\sigma}_{\hat{\mu}_B}} denote the estimate of
      the mean and estimate of the standard error of the estimate of the mean based on
      the bootstrap sample.

    \item Repeat Steps 1 and 2 \eqn{B} times, where \eqn{B} is some large number.
      For the function \code{enparCensored}, the number of bootstraps \eqn{B} is
      determined by the argument \code{n.bootstraps} (see the section \bold{ARGUMENTS} 
      above).
      The default value of \code{n.bootstraps} is \code{1000}.

    \item Use the \eqn{B} estimated values of \eqn{\mu} to compute the empirical
      cumulative distribution function of the estimator of \eqn{\mu} or to compute
      the empirical cumulative distribution function of the statistic \eqn{T}
      (see \code{\link{ecdfPlot}}), and then create a confidence interval for \eqn{\mu}
      based on this estimated cdf.
  }

  The two-sided percentile interval (Efron and Tibshirani, 1993, p.170) is computed as:
  \deqn{[\hat{G}^{-1}(\frac{\alpha}{2}), \; \hat{G}^{-1}(1-\frac{\alpha}{2})] \;\;\;\;\;\; (14)}
  where \eqn{\hat{G}(t)} denotes the empirical cdf of \eqn{\hat{\mu}_B} evaluated at \eqn{t}
  and thus \eqn{\hat{G}^{-1}(p)} denotes the \eqn{p}'th empirical quantile of the
  distribution of \eqn{\hat{\mu}_B}, that is, the \eqn{p}'th quantile associated with the
  empirical cdf.  Similarly, a one-sided lower
  confidence interval is computed as:
  \deqn{[\hat{G}^{-1}(\alpha), \; \infty] \;\;\;\;\;\; (15)}
  and a one-sided upper confidence interval is computed as:
  \deqn{[-\infty, \; \hat{G}^{-1}(1-\alpha)] \;\;\;\;\;\; (16)}
  The function \code{enparCensored} calls the \R function \code{\link[stats]{quantile}}
  to compute the empirical quantiles used in Equations (14)-(16).

  The percentile method bootstrap confidence interval is only first-order
  accurate (Efron and Tibshirani, 1993, pp.187-188), meaning that the probability
  that the confidence interval will contain the true value of \eqn{\mu} can be
  off by \eqn{k/\sqrt{N}}, where \eqn{k} is some constant.  Efron and Tibshirani
  (1993, pp.184--188) proposed a bias-corrected and accelerated interval that is
  second-order accurate, meaning that the probability that the confidence interval
  will contain the true value of \eqn{\mu} may be off by \eqn{k/N} instead of
  \eqn{k/\sqrt{N}}.  The two-sided bias-corrected and accelerated confidence interval is
  computed as:
  \deqn{[\hat{G}^{-1}(\alpha_1), \; \hat{G}^{-1}(\alpha_2)] \;\;\;\;\;\; (17)}
  where
  \deqn{\alpha_1 = \Phi[\hat{z}_0 + \frac{\hat{z}_0 + z_{\alpha/2}}{1 - \hat{a}(z_0 + z_{\alpha/2})}] \;\;\;\;\;\; (18)}
  \deqn{\alpha_2 = \Phi[\hat{z}_0 + \frac{\hat{z}_0 + z_{1-\alpha/2}}{1 - \hat{a}(z_0 + z_{1-\alpha/2})}] \;\;\;\;\;\; (19)}
  \deqn{\hat{z}_0 = \Phi^{-1}[\hat{G}(\hat{\mu})] \;\;\;\;\;\; (20)}
  \deqn{\hat{a} = \frac{\sum_{i=1}^N (\hat{\mu}_{(\cdot)} - \hat{\mu}_{(i)})^3}{6[\sum_{i=1}^N (\hat{\mu}_{(\cdot)} - \hat{\mu}_{(i)})^2]^{3/2}} \;\;\;\;\;\; (21)}
  where the quantity \eqn{\hat{\mu}_{(i)}} denotes the estimate of \eqn{\mu} using
  all the values in \eqn{\underline{x}} except the \eqn{i}'th one, and
  \deqn{\hat{\mu}{(\cdot)} = \frac{1}{N} \sum_{i=1}^N \hat{\mu_{(i)}} \;\;\;\;\;\; (22)}
  A one-sided lower confidence interval is given by:
  \deqn{[\hat{G}^{-1}(\alpha_1), \; \infty] \;\;\;\;\;\; (23)}
  and a one-sided upper confidence interval is given by:
  \deqn{[-\infty, \; \hat{G}^{-1}(\alpha_2)] \;\;\;\;\;\; (24)}
  where \eqn{\alpha_1} and \eqn{\alpha_2} are computed as for a two-sided confidence
  interval, except \eqn{\alpha/2} is replaced with \eqn{\alpha} in Equations (18) and (19).

  The constant \eqn{\hat{z}_0} incorporates the bias correction, and the constant
  \eqn{\hat{a}} is the acceleration constant.  The term \dQuote{acceleration} refers
  to the rate of change of the standard error of the estimate of \eqn{\mu} with
  respect to the true value of \eqn{\mu} (Efron and Tibshirani, 1993, p.186).  For a
  normal (Gaussian) distribution, the standard error of the estimate of \eqn{\mu}
  does not depend on the value of \eqn{\mu}, hence the acceleration constant is not
  really necessary.

  For the bootstrap-t method, the two-sided confidence interval
  (Efron and Tibshirani, 1993, p.160) is computed as:
  \deqn{[\hat{\mu} - t_{1-\alpha/2}\hat{\sigma}_{\hat{\mu}}, \; \hat{\mu} - t_{\alpha/2}\hat{\sigma}_{\hat{\mu}}] \;\;\;\;\;\; (25)}
  where \eqn{\hat{\mu}} and \eqn{\hat{\sigma}_{\hat{\mu}}} denote the estimate of the mean
  and standard error of the estimate of the mean based on the original sample, and
  \eqn{t_p} denotes the \eqn{p}'th empirical quantile of the bootstrap distribution of
  the statistic \eqn{T}.  Similarly, a one-sided lower confidence interval is computed as:
  \deqn{[\hat{\mu} - t_{1-\alpha}\hat{\sigma}_{\hat{\mu}}, \; \infty] \;\;\;\;\;\; (26)}
  and a one-sided upper confidence interval is computed as:
  \deqn{[-\infty, \; \hat{\mu} - t_{\alpha}\hat{\sigma}_{\hat{\mu}}] \;\;\;\;\;\; (27)}

  When \code{ci.method="bootstrap"}, the function \code{enparCensored} computes
  the percentile method, bias-corrected and accelerated method, and bootstrap-t
  bootstrap confidence intervals.  The percentile method is transformation respecting,
  but not second-order accurate.  The bootstrap-t method is second-order accurate, but not
  transformation respecting.  The bias-corrected and accelerated method is both
  transformation respecting and second-order accurate (Efron and Tibshirani, 1993, p.188).
}
\value{
  a list of class \code{"estimateCensored"} containing the estimated parameters
  and other information.  See \code{\link{estimateCensored.object}} for details.
}
\references{
  Barker, C. (2009).  The Mean, Median, and Confidence Intervals of the
  Kaplan-Meier Survival Estimate -- Computations and Applications.
  \emph{The American Statistician} \bold{63}(1), 78--80.

  Beal, D. (2010).  \emph{A Macro for Calculating Summary Statistics on
  Left Censored Environmental Data Using the Kaplan-Meier Method}.
  Paper SDA-09, presented at Southeast SAS Users Group 2010, September 26-28,
  Savannah, GA.  \url{https://analytics.ncsu.edu/sesug/2010/SDA09.Beal.pdf}.

  Efron, B. (1979).  Bootstrap Methods: Another Look at the Jackknife.
  \emph{The Annals of Statistics} \bold{7}, 1--26.

  Efron, B., and R.J. Tibshirani. (1993).  \emph{An Introduction to the Bootstrap}.
  Chapman and Hall, New York, 436pp.

  El-Shaarawi, A.H., and D.M. Dolan. (1989).  Maximum Likelihood Estimation of
  Water Quality Concentrations from Censored Data.
  \emph{Canadian Journal of Fisheries and Aquatic Sciences} \bold{46}, 1033--1039.

  Frome E.L., and D.P. Frome (2015). \emph{STAND: Statistical Analysis of
  Non-Detects}. R package version 2.0, \url{https://CRAN.R-project.org/package=STAND}.

  Gillespie, B.W., Q. Chen, H. Reichert, A. Franzblau, E. Hedgeman, J. Lepkowski,
  P. Adriaens, A. Demond, W. Luksemburg, and D.H. Garabrant. (2010).  Estimating Population
  Distributions When Some Data Are Below a Limit of Detection by Using a Reverse
  Kaplan-Meier Estimator.  \emph{Epidemiology} \bold{21}(4), S64--S70.

  Helsel, D.R. (2012). \emph{Statistics for Censored Environmental Data Using Minitab and R,
  Second Edition}.  John Wiley & Sons, Hoboken, New Jersey.

  Irwin, J.O. (1949).  The Standard Error of an Estimate of Expectation of Life,
  with Special Reference to Expectation of Tumourless Life in Experiments with Mice.
  \emph{Journal of Hygiene} \bold{47}, 188--189.

  Kaplan, E.L., and P. Meier. (1958). Nonparametric Estimation From Incomplete Observations.
  \emph{Journal of the American Statistical Association} \bold{53}, 457-481.

  Klein, J.P., and M.L. Moeschberger. (2003).  \emph{Survival Analysis:
  Techniques for Censored and Truncated Data, Second Edition}.  Springer, New York,
  537pp.

  Lee, E.T., and J.W. Wang. (2003).
  \emph{Statistical Methods for Survival Data Analysis, Third Edition}.
  John Wiley & Sons, Hoboken, New Jersey, 513pp.

  Meier, P., T. Karrison, R. Chappell, and H. Xie. (2004).  The Price of Kaplan-Meier.
  \emph{Journal of the American Statistical Association} \bold{99}(467), 890--896.

  Miller, R.G. (1981).  \emph{Survival Analysis}. John Wiley and Sons, New York.

  Nelson, W. (1982).  \emph{Applied Life Data Analysis}.
  John Wiley and Sons, New York, 634pp.

  Singh, A., R. Maichle, and S. Lee. (2006).  \emph{On the Computation of a 95\%
  Upper Confidence Limit of the Unknown Population Mean Based Upon Data Sets
  with Below Detection Limit Observations}.  EPA/600/R-06/022, March 2006.
  Office of Research and Development, U.S. Environmental Protection Agency,
  Washington, D.C.

  Singh, A., N. Armbya, and A. Singh. (2010).
  \emph{ProUCL Version 4.1.00 Technical Guide (Draft)}. EPA/600/R-07/041, May 2010.
  Office of Research and Development, U.S. Environmental Protection Agency,
  Washington, D.C.

  USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at 
  RCRA Facilities, Unified Guidance}.  EPA 530/R-09-007, March 2009.  Office of 
  Resource Conservation and Recovery Program Implementation and Information Division.
  U.S. Environmental Protection Agency, Washington, D.C.

  USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
  EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, 
  Program Information and Implementation Division.  U.S. Environmental 
  Protection Agency, Washington, D.C.

  USEPA. (2022).  \emph{ProUCL Version 5.2.0 Technical Guide: 
  Statistical Software for Environmental Applications for Data Sets with and 
  without Nondetect Observations}.  Prepared by:  Neptune and Company, Inc., 
  1435 Garrison Street, Suite 201, Lakewood, CO 80215.  pp. 128--129, 143.  
  \url{https://www.epa.gov/land-research/proucl-software}.
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
}
\seealso{
  \code{\link{ppointsCensored}}, \code{\link{ecdfPlotCensored}}, 
  \code{\link{qqPlotCensored}},\code{\link{estimateCensored.object}}, 
  \code{\link{enpar}}.
}
\examples{
  # Using the lead concentration data from soil samples shown in 
  # Beal (2010), compute the Kaplan-Meier estimators of the mean, 
  # standard deviation, and standard error of the mean, as well as 
  # a 95% upper confidence limit for the mean.  Compare these 
  # results to those given in Beal (2010), and also to the results 
  # produced by ProUCL 5.2.0.

  # First look at the data:
  #-----------------------

  head(Beal.2010.Pb.df)
  #  Pb.char  Pb Censored
  #1      <1 1.0     TRUE
  #2      <1 1.0     TRUE
  #3       2 2.0    FALSE
  #4     2.5 2.5    FALSE
  #5     2.8 2.8    FALSE
  #6      <3 3.0     TRUE

  tail(Beal.2010.Pb.df)
  #   Pb.char   Pb Censored
  #24     <10   10     TRUE
  #25      10   10    FALSE
  #26      15   15    FALSE
  #27      49   49    FALSE
  #28     200  200    FALSE
  #29    9060 9060    FALSE

  # enparCensored Results:
  #-----------------------
  Beal.unrestricted <- with(Beal.2010.Pb.df, 
    enparCensored(x = Pb, censored = Censored, ci = TRUE, 
      ci.type = "upper"))

  Beal.unrestricted

  #Results of Distribution Parameter Estimation
  #Based on Type I Censored Data
  #--------------------------------------------
  #
  #Assumed Distribution:            None
  #
  #Censoring Side:                  left
  #
  #Censoring Level(s):               1  3  4  6  9 10 
  #
  #Estimated Parameter(s):          mean    =  325.3396
  #                                 sd      = 1651.0950
  #                                 se.mean =  315.0023
  #
  #Estimation Method:               Kaplan-Meier
  #                                 (Bias-corrected se.mean)
  #
  #Data:                            Pb
  #
  #Censoring Variable:              Censored
  #
  #Sample Size:                     29
  #
  #Percent Censored:                34.48276%
  #
  #Confidence Interval for:         mean
  #
  #Assumed Sample Size:             29
  #
  #Confidence Interval Method:      Normal Approximation
  #                                 (t Distribution)
  #
  #Confidence Interval Type:        upper
  #
  #Confidence Level:                95%
  #
  #Confidence Interval:             LCL =   0.0000
  #                                 UCL = 861.1996

  c(Beal.unrestricted$parameters, Beal.unrestricted$interval$limits)
  #     mean        sd   se.mean       LCL       UCL 
  # 325.3396 1651.0950  315.0023    0.0000  861.1996

  # Beal (2010) published results:
  #-------------------------------
  #   Mean   Std. Dev.  SE of Mean
  # 325.34     1651.09      315.00

  # ProUCL 5.2.0 results:
  #----------------------
  #   Mean   Std. Dev.  SE of Mean  95% UCL
  # 325.2      1651         315       861.1

  #----------

  # Now compute the restricted mean and associated quantities, 
  # and compare these results with those produced by the 
  # kmms() function in the STAND package.
  #----------------------------------------------------------- 

  Beal.restricted <- with(Beal.2010.Pb.df, 
    enparCensored(x = Pb, censored = Censored, restricted = TRUE, 
      ci = TRUE, ci.type = "upper"))

  Beal.restricted 

  #Results of Distribution Parameter Estimation
  #Based on Type I Censored Data
  #--------------------------------------------
  #
  #Assumed Distribution:            None
  #
  #Censoring Side:                  left
  #
  #Censoring Level(s):               1  3  4  6  9 10 
  #
  #Estimated Parameter(s):          mean    =  325.2011
  #                                 sd      = 1651.1221
  #                                 se.mean =  314.1774
  #
  #Estimation Method:               Kaplan-Meier (Restricted Mean)
  #                                 Smallest censored value(s)
  #                                   set to Censoring Level
  #                                 (Bias-corrected se.mean)
  #
  #Data:                            Pb
  #
  #Censoring Variable:              Censored
  #
  #Sample Size:                     29
  #
  #Percent Censored:                34.48276%
  #
  #Confidence Interval for:         mean
  #
  #Assumed Sample Size:             29
  #
  #Confidence Interval Method:      Normal Approximation
  #                                 (t Distribution)
  #
  #Confidence Interval Type:        upper
  #
  #Confidence Level:                95%
  #
  #Confidence Interval:             LCL =   0.000
  #                                 UCL = 859.658

  c(Beal.restricted$parameters, Beal.restricted$interval$limits)
  #     mean        sd   se.mean       LCL       UCL 
  # 325.2011 1651.1221  314.1774    0.0000  859.6580

  # kmms() results:
  #----------------
  #  KM.mean    KM.LCL    KM.UCL     KM.se     gamma 
  # 325.2011 -221.0419  871.4440  315.0075    0.9500 
  
  # NOTE: as pointed out above, the kmms() function treats the 
  #       smallest censored observations (<1 and <1) as NOT 
  #       censored when computing the mean and uncorrected 
  #       standard error of the mean, but assumes these 
  #       observations ARE censored when computing the corrected 
  #       standard error of the mean.
  #--------------------------------------------------------------

  Beal.restricted$parameters["se.mean"] * sqrt((20/21)) * sqrt((19/18))
  #  se.mean 
  # 315.0075

  #==========

  # Repeat the above example, estimating the unrestricted mean and 
  # computing an upper confidence limit based on the bootstrap 
  # instead of on the normal approximation with a t pivot statistic.
  # Compare results to those from ProUCL 5.2.0.
  # Note:  Setting the seed argument lets you reproduce this example.
  #------------------------------------------------------------------

  Beal.unrestricted.boot <- with(Beal.2010.Pb.df, 
    enparCensored(x = Pb, censored = Censored, ci = TRUE, 
      ci.type = "upper", ci.method = "bootstrap", seed = 923))

  Beal.unrestricted.boot

  #Results of Distribution Parameter Estimation
  #Based on Type I Censored Data
  #--------------------------------------------
  #
  #Assumed Distribution:            None
  #
  #Censoring Side:                  left
  #
  #Censoring Level(s):               1  3  4  6  9 10 
  #
  #Estimated Parameter(s):          mean    =  325.3396
  #                                 sd      = 1651.0950
  #                                 se.mean =  315.0023
  #
  #Estimation Method:               Kaplan-Meier
  #                                 (Bias-corrected se.mean)
  #
  #Data:                            Pb
  #
  #Censoring Variable:              Censored
  #
  #Sample Size:                     29
  #
  #Percent Censored:                34.48276%
  #
  #Confidence Interval for:         mean
  #
  #Assumed Sample Size:             29
  #
  #Confidence Interval Method:      Bootstrap
  #
  #Number of Bootstraps:            1000
  #
  #Number of Bootstrap Samples
  #With No Censored Values:         0
  #
  #Number of Times Bootstrap
  #Repeated Because Too Few
  #Uncensored Observations:         0
  #
  #Confidence Interval Type:        upper
  #
  #Confidence Level:                95%
  #
  #Confidence Interval:             Pct.LCL =     0.0000
  #                                 Pct.UCL =   948.7342
  #                                 BCa.LCL =     0.0000
  #                                 BCa.UCL =   942.6596
  #                                 t.LCL   =     0.0000
  #                                 t.UCL   = 62121.8909

  c(Beal.unrestricted.boot$interval$limits)
  #   Pct.LCL    Pct.UCL    BCa.LCL    BCa.UCL      t.LCL      t.UCL 
  #    0.0000   948.7342     0.0000   942.6596     0.0000 62121.8909

  # ProUCL 5.2.0 results:
  #----------------------
  #   Pct.LCL    Pct.UCL    BCa.LCL    BCa.UCL      t.LCL      t.UCL 
  #    0.0000   944.3        0.0000   947.8        0.0000 62169

  #==========

  # Clean up
  #---------
  rm(Beal.unrestricted, Beal.restricted, Beal.unrestricted.boot)
}
\keyword{ distribution }
\keyword{ htest }

