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
  enparCensored(x, censored, censoring.side = "left", correct.se = FALSE, 
    left.censored.min = "DL", right.censored.max = "DL", ci = FALSE, 
    ci.method = "normal.approx", ci.type = "two-sided", conf.level = 0.95, 
    pivot.statistic = "z", ci.sample.size = NULL, n.bootstraps = 1000)
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
  by a factor to correct for bias.  The default value is \code{correct.se=FALSE}.  
  See the DETAILS section below.
}
  \item{left.censored.min}{
  Only relevant for the case when \code{censoring.side="left"} and the smallest 
  censored value is less than the smallest uncensored value.  In this 
  case, \code{left.censored.min} must be a character string with the possible values 
  \code{"DL"} (detection limit; the default), \code{"DL/2"} (half the detection limit), 
  or \code{"Ignore"}, or else a numeric scalar between 0 and the smallest censored 
  value.  See the DETAILS section for more information.
}
  \item{right.censored.max}{
  Only relevant for the case when \code{censoring.side="right"} and the largest 
  censored value is greater than the largest uncensored value.  In this 
  case, \code{right.censored.max} must be a character string with the possible values 
  \code{"DL"} (detection limit; the default) or \code{"Ignore"}, or else a numeric 
  scalar greater than or equal to the largest censored value.  
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
  \item{pivot.statistic}{
  character string indicating which statistic to use for the confidence interval 
  for the mean when \code{ci.method="normal.approx"}.  Possible values are 
  \code{"z"} (confidence interval based on the z-statistic; the default), and 
  \code{"t"} (confidence interval based on the t-statistic).  
  When \code{pivot.statistic="t"} you may supply the argument 
  \code{ci.sample size} (see below).
  This argument is ignored if \code{ci=FALSE}.
}
  \item{ci.sample.size}{
  numeric scalar or a \code{NULL} object indicating what sample size to assume when 
  computing the confidence interval for the mean when \code{ci.method="normal.approx"} 
  and \code{pivot.statistic="t"}.  The default value is \code{ci.sample.size=NULL}, 
  in which case \code{ci.sample.size} is equal to the number of uncensored observations.  
  This argument is ignored if \code{ci=FALSE}.
}
  \item{n.bootstraps}{
  numeric scalar indicating how many bootstraps to use to construct the 
  confidence interval for the mean when \code{ci.type="bootstrap"}.  This 
  argument is ignored if \code{ci=FALSE} and/or \code{ci.method} does not 
  equal \code{"bootstrap"}.
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
  Finally, let \eqn{y_1, y_2, \ldots, y_n} denote the \eqn{n} ordered uncensored 
  observations.
  \cr
 
  \bold{Estimation} \cr
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
  where \eqn{\hat{F}(y_{0}) = \hat{F}(0) = 0} by definition 
  (USEPA, 2009, p. 15-10; Singh et al., 2010, pp. 109--111; Beal, 2010).

  The formula for the estimated standard deviation is:
  \deqn{\hat{\sigma} = \{\sum_{i=1}^n (y_{i} - \hat{\mu})^2 [\hat{F}(y_{i}) - \hat{F}(y_{i-1})]\}^{1/2} \;\;\;\;\; (5)}
  (USEPA, 2009, p. 15-10), and the formula for the estimated standard error of the 
  mean is:
  \deqn{\hat{\sigma}_{\hat{\mu}} = [\sum_{r=1}^{n-1} \frac{A_r^2}{(N-r)(N-r+1)}]^{1/2} \;\;\;\;\;\; (6)}
  where
  \deqn{A_r = \sum_{i=r}^{n-1} \hat{S}(y_{i}) (y_{i+1} - y_{i}) \;\;\;\;\;\; (7)}
  (Lee and Wang, 2003, p. 74).  Kaplan and Meier suggest using a bias correction of 
  \eqn{n/(n-1)} (Lee and Wang, 2003, p.75):
  \deqn{\hat{\sigma}_{\hat{\mu}, BC} = \frac{n}{n-1} \hat{\sigma}_{\hat{\mu}} \;\;\;\;\;\; (8)}
  When \code{correct.se=TRUE}, Equation (8) is used instead of Equation (6).

  If the smallest value for left-censored data is censored and less than or equal to  
  the smallest uncensored value then the estimated mean will be biased high, and 
  if the largest value for right-censored data is censored and greater than or equal to 
  the largest uncensored value, the the estimated mean will be biased low.  In these 
  cases, the above formulas can and should be modified 
  (Barker, 2009; Lee and Wang, 2003, p. 74).  
  For left-censored data, the smallest censored observation can be treated as 
  observed and set to the smallest censoring level (\code{left.censored.min="DL"}), 
  half of the smallest censoring level (\code{left.censored.min="DL/2"}), or some other 
  value greater than 0 and the smallest censoring level.  Setting 
  \code{left.censored.min="Ignore"} uses the formulas given above (biased in this case) 
  and is what is presented in Singh et al. (2010, pp. 109--111) and Beal (2010).  
  USEPA (2009, pp. 15--7 to 15-10) on the other hand uses the method associated with 
  \code{left.censored.min="DL"}.  For right-censored data, the largest censored 
  observation can be treated as observed and set to the censoring level 
  (\code{right.censored.max="DL"}) or some value greater than the largest censoring 
  level.  In the survival analysis literature, this method of dealing with this 
  situation is called estimating the \bold{\emph{restricted mean}} 
  (Miller, 1981; Meier, 2004; Barker, 2009).
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
  \deqn{[\hat{\mu} - t_{1-\alpha/2, m-1}\hat{\sigma}_{\hat{\mu}}, \; \hat{\mu} + t_{1-\alpha/2, m-1}\hat{\sigma}_{\hat{\mu}}] \;\;\;\; (9)}
  where \eqn{\hat{\mu}} denotes the estimate of \eqn{\mu}, 
  \eqn{\hat{\sigma}_{\hat{\mu}}} denotes the estimated asymptotic standard 
  deviation of the estimator of \eqn{\mu}, \eqn{m} denotes the assumed sample 
  size for the confidence interval, and \eqn{t_{p,\nu}} denotes the \eqn{p}'th 
  quantile of \link[stats:TDist]{Student's t-distribuiton} with \eqn{\nu} 
  degrees of freedom.  One-sided confidence intervals are computed in a 
  similar fashion.

  The argument \code{ci.sample.size} determines the value of \eqn{m}.  
  By default, it is the observed number of uncensored observations.  
  This is simply an ad-hoc method of constructing 
  confidence intervals and is not based on any published theoretical results.

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
      computing the statistic \eqn{T = (\hat{\mu}_B - \hat{mu}) / \hat{\sigma}_{\hat{\mu}_B}} 
      where \eqn{\hat{\mu}} denotes the estimate of the mean based on the original sample, 
      and \eqn{\hat{\mu}_B} and \eqn{\hat{\sigma}_{\hat{\mu}_B}} denote the estimate of 
      the mean and estimate of the standard error of the estimate of the mean based on 
      the bootstrap sample.

    \item Repeat Steps 1 and 2 \eqn{B} times, where \eqn{B} is some large number.  
      For the function \code{enparCensored}, the number of bootstraps \eqn{B} is 
      determined by the argument \code{n.bootstraps} (see the section ARGUMENTS above).  
      The default value of \code{n.bootstraps} is \code{1000}.

    \item Use the \eqn{B} estimated values of \eqn{\mu} to compute the empirical 
      cumulative distribution function of the estimator of \eqn{\mu} or to compute 
      the empirical cumulative distribution function of the statistic \eqn{T}  
      (see \code{\link{ecdfPlot}}), and then create a confidence interval for \eqn{\mu} 
      based on this estimated cdf.  
  }

  The two-sided percentile interval (Efron and Tibshirani, 1993, p.170) is computed as:
  \deqn{[\hat{G}^{-1}(\frac{\alpha}{2}), \; \hat{G}^{-1}(1-\frac{\alpha}{2})] \;\;\;\;\;\; (10)}
  where \eqn{\hat{G}(t)} denotes the empirical cdf of \eqn{\hat{\mu}_B} evaluated at \eqn{t} 
  and thus \eqn{\hat{G}^{-1}(p)} denotes the \eqn{p}'th empirical quantile of the 
  distribution of \eqn{\hat{\mu}_B}, that is, the \eqn{p}'th quantile associated with the 
  empirical cdf.  Similarly, a one-sided lower 
  confidence interval is computed as:
  \deqn{[\hat{G}^{-1}(\alpha), \; \infty] \;\;\;\;\;\; (11)}
  and a one-sided upper confidence interval is computed as:
  \deqn{[-\infty, \; \hat{G}^{-1}(1-\alpha)] \;\;\;\;\;\; (12)}
  The function \code{enparCensored} calls the \R function \code{\link[stats]{quantile}} 
  to compute the empirical quantiles used in Equations (10)-(12).

  The percentile method bootstrap confidence interval is only first-order 
  accurate (Efron and Tibshirani, 1993, pp.187-188), meaning that the probability 
  that the confidence interval will contain the true value of \eqn{\mu} can be 
  off by \eqn{k/\sqrt{N}}, where \eqn{k} is some constant.  Efron and Tibshirani 
  (1993, pp.184--188) proposed a bias-corrected and accelerated interval that is 
  second-order accurate, meaning that the probability that the confidence interval 
  will contain the true value of \eqn{\mu} may be off by \eqn{k/N} instead of 
  \eqn{k/\sqrt{N}}.  The two-sided bias-corrected and accelerated confidence interval is 
  computed as:
  \deqn{[\hat{G}^{-1}(\alpha_1), \; \hat{G}^{-1}(\alpha_2)] \;\;\;\;\;\; (13)}
  where
  \deqn{\alpha_1 = \Phi[\hat{z}_0 + \frac{\hat{z}_0 + z_{\alpha/2}}{1 - \hat{a}(z_0 + z_{\alpha/2})}] \;\;\;\;\;\; (14)}
  \deqn{\alpha_2 = \Phi[\hat{z}_0 + \frac{\hat{z}_0 + z_{1-\alpha/2}}{1 - \hat{a}(z_0 + z_{1-\alpha/2})}] \;\;\;\;\;\; (15)}
  \deqn{\hat{z}_0 = \Phi^{-1}[\hat{G}(\hat{\mu})] \;\;\;\;\;\; (16)}
  \deqn{\hat{a} = \frac{\sum_{i=1}^N (\hat{\mu}_{(\cdot)} - \hat{\mu}_{(i)})^3}{6[\sum_{i=1}^N (\hat{\mu}_{(\cdot)} - \hat{\mu}_{(i)})^2]^{3/2}} \;\;\;\;\;\; (17)}
  where the quantity \eqn{\hat{\mu}_{(i)}} denotes the estimate of \eqn{\mu} using 
  all the values in \eqn{\underline{x}} except the \eqn{i}'th one, and
  \deqn{\hat{\mu}{(\cdot)} = \frac{1}{N} \sum_{i=1}^N \hat{\mu_{(i)}} \;\;\;\;\;\; (18)}
  A one-sided lower confidence interval is given by:
  \deqn{[\hat{G}^{-1}(\alpha_1), \; \infty] \;\;\;\;\;\; (19)}
  and a one-sided upper confidence interval is given by:
  \deqn{[-\infty, \; \hat{G}^{-1}(\alpha_2)] \;\;\;\;\;\; (20)}
  where \eqn{\alpha_1} and \eqn{\alpha_2} are computed as for a two-sided confidence 
  interval, except \eqn{\alpha/2} is replaced with \eqn{\alpha} in Equations (14) and (15).

  The constant \eqn{\hat{z}_0} incorporates the bias correction, and the constant 
  \eqn{\hat{a}} is the acceleration constant.  The term \dQuote{acceleration} refers 
  to the rate of change of the standard error of the estimate of \eqn{\mu} with 
  respect to the true value of \eqn{\mu} (Efron and Tibshirani, 1993, p.186).  For a 
  normal (Gaussian) distribution, the standard error of the estimate of \eqn{\mu} 
  does not depend on the value of \eqn{\mu}, hence the acceleration constant is not 
  really necessary.

  For the bootstrap-t method, the two-sided confidence interval 
  (Efron and Tibshirani, 1993, p.160) is computed as:
  \deqn{[\hat{\mu} - t_{1-\alpha/2}\hat{\sigma}_{\hat{\mu}}, \; \hat{\mu} - t_{\alpha/2}\hat{\sigma}_{\hat{\mu}}] \;\;\;\;\;\; (21)}
  where \eqn{\hat{\mu}} and \eqn{\hat{\sigma}_{\hat{\mu}}} denote the estimate of the mean 
  and standard error of the estimate of the mean based on the original sample, and 
  \eqn{t_p} denotes the \eqn{p}'th empirical quantile of the bootstrap distribution of 
  the statistic \eqn{T}.  Similarly, a one-sided lower confidence interval is computed as:
  \deqn{[\hat{\mu} - t_{1-\alpha}\hat{\sigma}_{\hat{\mu}}, \; \infty] \;\;\;\;\;\; (22)}
  and a one-sided upper confidence interval is computed as:
  \deqn{[-\infty, \; \hat{\mu} - t_{\alpha}\hat{\sigma}_{\hat{\mu}}] \;\;\;\;\;\; (23)}

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
  Savannah, GA.

  Efron, B. (1979).  Bootstrap Methods: Another Look at the Jackknife.  
  \emph{The Annals of Statistics} \bold{7}, 1--26.

  Efron, B., and R.J. Tibshirani. (1993).  \emph{An Introduction to the Bootstrap}.  
  Chapman and Hall, New York, 436pp.

  El-Shaarawi, A.H., and D.M. Dolan. (1989).  Maximum Likelihood Estimation of 
  Water Quality Concentrations from Censored Data.  
  \emph{Canadian Journal of Fisheries and Aquatic Sciences} \bold{46}, 1033--1039.

  Gillespie, B.W., Q. Chen, H. Reichert, A. Franzblau, E. Hedgeman, J. Lepkowski, 
  P. Adriaens, A. Demond, W. Luksemburg, and D.H. Garabrant. (2010).  Estimating Population 
  Distributions When Some Data Are Below a Limit of Detection by Using a Reverse 
  Kaplan-Meier Estimator.  \emph{Epidemiology} \bold{21}(4), S64--S70.

  Helsel, D.R. (2012). \emph{Statistics for Censored Environmental Data Using Minitab and R, 
  Second Edition}.  John Wiley \& Sons, Hoboken, New Jersey.

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
  John Wiley \& Sons, Hoboken, New Jersey, 513pp.

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

  USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
  EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.  
  U.S. Environmental Protection Agency, Washington, D.C.

  USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
  EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
  U.S. Environmental Protection Agency, Washington, D.C.
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
  \code{\link{ecdfPlotCensored}}, \code{\link{qqPlotCensored}}, 
  \code{\link{estimateCensored.object}}.
}
\examples{
  # Example 15-1 of USEPA (2009, page 15-10) gives an example of
  # estimating the mean and standard deviation nonparametrically 
  # using the Kaplan-Meier estimators based on censored manganese 
  # concentrations (ppb) in groundwater collected at 5 monitoring
  # wells.  The data for this example are stored in 
  # EPA.09.Ex.15.1.manganese.df.

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

  #----------

  # Following Example 15-1 in USEPA (2009, p.15-10), 
  # estimate the log-scale mean and standard deviation 
  # nonparametrically using the Kaplan-Meier method
  #------------------------------------------------
  with(EPA.09.Ex.15.1.manganese.df, 
    enparCensored(log(Manganese.ppb), Censored, ci = TRUE))

  #Results of Distribution Parameter Estimation
  #Based on Type I Censored Data
  #--------------------------------------------
  #
  #Assumed Distribution:            None
  #
  #Censoring Side:                  left
  #
  #Censoring Level(s):              0.6931472 1.6094379 
  #
  #Estimated Parameter(s):          mean    = 2.3092890
  #                                 sd      = 1.1816102
  #                                 se.mean = 0.1682862
  #
  #Estimation Method:               Kaplan-Meier
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
  #Confidence Interval Method:      Normal Approximation
  #
  #Confidence Interval Type:        two-sided
  #
  #Confidence Level:                95%
  #
  #Confidence Interval:             LCL = 1.979454
  #                                 UCL = 2.639124

  #----------

  # Now estimate the mean and standard deviation on the 
  # original scale nonparametrically using the 
  # Kaplan-Meier method.
  #-----------------------------------------------------

  with(EPA.09.Ex.15.1.manganese.df, 
    enparCensored(Manganese.ppb, Censored, ci = TRUE))

  #Results of Distribution Parameter Estimation
  #Based on Type I Censored Data
  #--------------------------------------------
  #
  #Assumed Distribution:            None
  #
  #Censoring Side:                  left
  #
  #Censoring Level(s):              2 5 
  #
  #Estimated Parameter(s):          mean    = 19.867000
  #                                 sd      = 25.317737
  #                                 se.mean =  4.689888
  #
  #Estimation Method:               Kaplan-Meier
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
  #Confidence Interval Method:      Normal Approximation
  #
  #Confidence Interval Type:        two-sided
  #
  #Confidence Level:                95%
  #
  #Confidence Interval:             LCL = 10.67499
  #                                 UCL = 29.05901
}
\keyword{ distribution }
\keyword{ htest }

