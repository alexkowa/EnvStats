\name{elnormAlt}
\alias{elnormAlt}
\title{
  Estimate Parameters of a Lognormal Distribution (Original Scale)
}
\description{
  Estimate the mean and coefficient of variation of a 
  \link[=LognormalAlt]{lognormal distribution}, and optionally construct a 
  confidence interval for the mean.
}
\usage{
  elnormAlt(x, method = "mvue", ci = FALSE, ci.type = "two-sided", 
    ci.method = "land", conf.level = 0.95, parkin.list = NULL)
}
\arguments{
  \item{x}{
  numeric vector of positive observations.
}
  \item{method}{
  character string specifying the method of estimation.  Possible values are 
  \code{"mvue"} (minimum variance unbiased; the default), \code{"qmle"} 
  (quasi maximum likelihood), \code{"mle"} (maximum likelihood), \code{"mme"} 
  (method of moments), and \code{"mmue"} (method of moments based on the unbiased 
  estimate of variance).  See the DETAILS section for more information on these 
  estimation methods.
}
  \item{ci}{
  logical scalar indicating whether to compute a confidence interval for the 
  mean.  The default value is \code{FALSE}.
}
  \item{ci.type}{
  character string indicating what kind of confidence interval to compute.  The 
  possible values are \code{"two-sided"} (the default), \code{"lower"}, and 
  \code{"upper"}.  This argument is ignored if \code{ci=FALSE}.
}
  \item{ci.method}{
  character string indicating what method to use to construct the confidence interval 
  for the mean.  The possible values are \code{"land"} (Land's method; the default), 
  \code{zou} (Zou et al.'s method), \code{"parkin"} (Parkin et al.'s method), 
  \code{"cox"} (Cox's approximation), and \code{"normal.approx"} (normal approximation).  
  See the DETAILS section for more information.  This argument is ignored if 
  \code{ci=FALSE}.
}
  \item{conf.level}{
  a scalar between 0 and 1 indicating the confidence level of the confidence interval.  
  The default value is \code{conf.level=0.95}. This argument is ignored if 
  \code{ci=FALSE}.
}
  \item{parkin.list}{
  a list containing arguments for the function \code{\link{eqnpar}}.  The components 
  of this list are \code{lcl.rank} (set to \code{NULL} by default), \code{ucl.rank} 
  (set to \code{NULL} by default), \code{ci.method} (set to \code{"exact"} if the 
  sample size is \eqn{\le 20}, otherwise set to \cr
  \code{"normal.approx"}), and 
  \code{approx.conf.level} (set to the value of \code{conf.level}).  This argument is 
  ignored unless \code{ci=TRUE} and \code{ci.method="parkin"}. 
}
}
\details{
  If \code{x} contains any missing (\code{NA}), undefined (\code{NaN}) or 
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

  \bold{Estimation} \cr
  This section explains how each of the estimators of \code{mean=}\eqn{\theta} and 
  \code{cv=}\eqn{\tau} are computed.  The approach is to first compute estimates of 
  \eqn{\theta} and \eqn{\eta^2} (the mean and variance of the lognormal distribution), 
  say \eqn{\hat{\theta}} and \eqn{\hat{\eta}^2}, then compute the estimate of the cv 
  \eqn{\tau} by \eqn{\hat{\tau} = \hat{\eta}/\hat{\theta}}.

  \emph{Minimum Variance Unbiased Estimation} (\code{method="mvue"}) \cr
  The minimum variance unbiased estimators (mvue's) of \eqn{\theta} and \eqn{\eta^2} were derived 
  by Finney (1941) and are discussed in Gilbert (1987, pp. 164-167) and Cohn et al. (1989).  These 
  estimators are computed as:
  \deqn{\hat{\theta}_{mvue} = e^{\bar{y}} g_{n-1}(\frac{s^2}{2}) \;\;\;\; (1)}
  \deqn{\hat{\eta}^2_{mvue} = e^{2 \bar{y}} \{g_{n-1}(2s^2) - g_{n-1}[\frac{(n-2)s^2}{n-1}]\} \;\;\;\; (2)}
  where
  \deqn{\bar{y} = \frac{1}{n} \sum_{i=1}^n y_i \;\;\;\; (3)}
  \deqn{s^2 = \frac{1}{n-1} \sum_{i=1}^n (y_i - \bar{y})^2 \;\;\;\; (4)}
  \deqn{g_m(z) = \sum_{i=0}^\infty \frac{m^i (m+2i)}{m(m+2) \cdots (m+2i)} (\frac{m}{m+1})^i (\frac{z^i}{i!}) \;\;\;\; (5)}

  The expected value and variance of the mvue of \eqn{\theta} are 
  (Bradu and Mundlak, 1970; Cohn et al., 1989):
  \deqn{E[\hat{\theta}_{mvue}] = \theta \;\;\;\; (6)}
  \deqn{Var[\hat{\theta}_{mvue}] = e^{2\mu} \{e^{[(2+n-1)\sigma^2]/n} g_{n-1}(\frac{\sigma^4}{4n}) - e^{\sigma^2} \} \;\;\;\; (7)}

  \emph{Maximum Likelihood Estimation} (\code{method="mle"}) \cr
  The maximum likelihood estimators (mle's) of \eqn{\theta} and \eqn{\eta^2} are given by:
  \deqn{\hat{\theta}_{mle} = exp(\bar{y} + \frac{\hat{\sigma}^2_{mle}}{2})  \;\;\;\; (8)}
  \deqn{\hat{\eta}^2_{mle} = \hat{\theta}^2_{mle} \hat{\tau}^2_{mle} \;\;\;\; (9)}
  where
  \deqn{\hat{\tau}^2_{mle} = exp(\hat{\sigma}^2_{mle}) - 1 \;\;\;\; (10)}
  \deqn{\hat{\sigma}^2_{mle} = \frac{n-1}{n} s^2 \;\;\;\; (11)}

  The expected value and variance of the mle of \eqn{\theta} are (after Cohn et al., 1989):
  \deqn{E[\hat{\theta}_{mle}] = \theta exp[\frac{-(n-1)\sigma^2}{2n}] (1 - \frac{\sigma^2}{n})^{-(n-1)/2} \;\;\;\; (12)}
  \deqn{Var[\hat{\theta}_{mle}] = exp(2\mu + \frac{\sigma^2}{n}) \{exp(\frac{\sigma^2}{n}) [1 - \frac{2\sigma^2}{n}]^{-(n-1)/2} - [1 - \frac{\sigma^2}{n}]^{-(n-1)} \} \;\;\;\; (13)}
  As can be seen from equation (12), the expected value of the mle of \eqn{\theta} does not exist 
  when \eqn{\sigma^2 > n}.  In general, the \eqn{p}'th moment of the mle of \eqn{\theta} does not 
  exist when \eqn{\sigma^2 > n/p}. 

  \emph{Quasi Maximum Likelihood Estimation} (\code{method="qmle"}) \cr 
  The quasi maximum likelihood estimators (qmle's; Cohn et al., 1989; Gilbert, 1987, p.167) of 
  \eqn{\theta} and \eqn{\eta^2} are the same as the mle's, except the mle of 
  \eqn{\sigma^2} in equations (8) and (10) is replaced with the more commonly used 
  mvue of \eqn{\sigma^2} shown in equation (4):
  \deqn{\hat{\theta}_{qmle} = exp(\bar{y} + \frac{s^2}{2})  \;\;\;\; (14)}
  \deqn{\hat{\eta}^2_{qmle} = \hat{\theta}^2_{qmle} \hat{\tau}^2_{qmle} \;\;\;\; (15)}
  \deqn{\hat{\tau}^2_{qmle} = exp(s^2) - 1 \;\;\;\; (16)}

  The expected value and variance of the qmle of \eqn{\theta} are (Cohn et al., 1989):
  \deqn{E[\hat{\theta}_{mle}] = \theta exp[\frac{-(n-1)\sigma^2}{2n}] (1 - \frac{\sigma^2}{n-1})^{-(n-1)/2} \;\;\;\; (17)}
  \deqn{Var[\hat{\theta}_{mle}] = exp(2\mu + \frac{\sigma^2}{n}) \{exp(\frac{\sigma^2}{n}) [1 - \frac{2\sigma^2}{n-1}]^{-(n-1)/2} - [1 - \frac{\sigma^2}{n-1}]^{-(n-1)} \} \;\;\;\; (18)}
  As can be seen from equation (17), the expected value of the qmle of \eqn{\theta} does not exist 
  when \eqn{\sigma^2 > (n - 1)}.  In general, the \eqn{p}'th moment of the mle of \eqn{\theta} does 
  not exist when \eqn{\sigma^2 > (n - 1)/p}.

  Note that Gilbert (1987, p. 167) incorrectly presents equation (12) rather than 
  equation (17) as the expected value of the qmle of \eqn{\theta}.  For large values 
  of \eqn{n} relative to \eqn{\sigma^2}, however, equations (12) and (17) are 
  virtually identical.

  \emph{Method of Moments Estimation} (\code{method="mme"}) \cr
  The method of moments estimators (mme's) of \eqn{\theta} and \eqn{\eta^2} are found by equating 
  the sample mean and variance with their population values:
  \deqn{\hat{\theta}_{mme} = \bar{x} = \frac{1}{n} \sum_{i=1}^n x_i \;\;\;\; (19)}
  \deqn{\hat{\eta}_{mme} = \frac{1}{n} \sum_{i=1}^n (x_i - \bar{x})^2 \;\;\;\; (20)}
  Note that the estimator of variance in equation (20) is biased.

  The expected value and variance of the mme of \eqn{\theta} are:
  \deqn{E[\hat{\theta}_{mme}] = \theta \;\;\;\; (21)}
  \deqn{Var[\hat{\theta}_{mme}] = \frac{\eta^2}{n} = \frac{1}{n} exp(2\mu + \sigma^2) [exp(\sigma^2)-1] \;\;\;\; (22)}

  \emph{Method of Moments Estimation Based on the Unbiased Estimate of Variance} (\code{method="mmue"}) \cr
  These estimators are exactly the same as the method of moments estimators described above, except 
  that the usual unbiased estimate of variance is used:
  \deqn{\hat{\theta}_{mmue} = \bar{x} = \frac{1}{n} \sum_{i=1}^n x_i \;\;\;\; (23)}
  \deqn{\hat{\eta}_{mmue} = \frac{1}{n-1} \sum_{i=1}^n (x_i - \bar{x})^2 \;\;\;\; (24)}
  Since the mmue of \eqn{\theta} is equivalent to the mme of \eqn{\theta}, so are its mean and varaince.
  \cr
 
  \bold{Confidence Intervals} \cr
  This section explains the different methods for constructing confidence intervals 
  for \eqn{\theta}, the mean of the lognormal distribution.

  \emph{Land's Method} (\code{ci.method="land"}) \cr
  Land (1971, 1975) derived a method for computing one-sided (lower or upper) 
  uniformly most accurate unbiased confidence intervals for \eqn{\theta}.  A 
  two-sided confidence interval can be constructed by combining an optimal lower 
  confidence limit with an optimal upper confidence limit.  This procedure for 
  two-sided confidence intervals is only asymptotically optimal, but for most 
  purposes should be acceptable (Land, 1975, p.387). 

  As shown in equation (3) in the help file for \link{LognormalAlt}, the mean 
  \eqn{\theta} of a lognormal random variable is related to the mean \eqn{\mu} and 
  standard deviation \eqn{\sigma} of the log-transformed random variable by the 
  following relationship:
  \deqn{\theta = e^{\beta} \;\;\;\; (25)}
  where
  \deqn{\beta = \mu + \frac{\sigma^2}{2} \;\;\;\; (26)}
  Land (1971) developed confidence bounds for the quantity \eqn{\beta}.  The mvue of 
  \eqn{\beta} is given by:
  \deqn{\hat{\beta}_{mvue} = \bar{y} + \frac{s^2}{2} \;\;\;\; (27)}
  Note that \eqn{\hat{\theta}_{qmle} = exp(\hat{\beta}_{mvue})}.  
  The \eqn{(1-\alpha)100\%} two-sided confidence interval for \eqn{\beta} is 
  given by:
  \deqn{[ \hat{\beta}_{mvue} + s \frac{C_{\alpha/2}}{\sqrt{n-1}}, \;  \hat{\beta}_{mvue} + s \frac{C_{1-\alpha/2}}{\sqrt{n-1}} ] \;\;\;\; (28)}
  the \eqn{(1-\alpha)100\%} one-sided upper confidence interval for \eqn{\beta} is 
  given by:
  \deqn{[ -\infty, \;  \hat{\beta}_{mvue} + s \frac{C_{1-\alpha}}{\sqrt{n-1}} ] \;\;\;\; (29)}
  and the \eqn{(1-\alpha)100\%} one-sided lower confidence interval for \eqn{\beta} is 
  given by:
  \deqn{[ \hat{\beta}_{mvue} + s \frac{C_{\alpha}}{\sqrt{n-1}}, \;  \infty ] \;\;\;\; (30)}
  where \eqn{s} is the estimate of \eqn{\sigma} (see equation (4) above), and the 
  factor \eqn{C} is given in tables in Land (1975).  

  Thus, by equations (25)-(30), the two-sided \eqn{(1-\alpha)100\%} confidence 
  interval for \eqn{\theta} is given by:
  \deqn{\{\hat{\theta}_{qmle} exp[s \frac{C_{\alpha/2}}{\sqrt{n-1}}], \;  \hat{\theta}_{qmle} exp[s \frac{C_{1-\alpha/2}}{\sqrt{n-1}}] \} \;\;\;\; (31)}
  the \eqn{(1-\alpha)100\%} one-sided upper confidence interval for \eqn{\theta} is 
  given by:
  \deqn{\{ 0, \;  \hat{\theta}_{qmle} exp[s \frac{C_{1-\alpha}}{\sqrt{n-1}}] \} \;\;\;\; (32)}
  and the \eqn{(1-\alpha)100\%} one-sided lower confidence interval for \eqn{\theta} 
  is given by:
  \deqn{\{\hat{\theta}_{qmle} exp[s \frac{C_{\alpha}}{\sqrt{n-1}} ], \;  \infty \} \;\;\;\; (33)}

  Note that Gilbert (1987, pp. 169-171, 264-265) denotes the quantity \eqn{C} above as 
  \eqn{H} and reproduces a subset of Land's (1975) tables.  Some guidance documents 
  (e.g., USEPA, 1992d) refer to this quantity as the \eqn{H}-statistic.
 
  \emph{Zou et al.'s Method} (\code{ci.method="zou"}) \cr
  Zou et al. (2009) proposed the following approximation for the two-sided 
  \eqn{(1-\alpha)100\%} confidence intervals for \eqn{\theta}.  The lower limit \eqn{LL} 
  is given by:
  \deqn{LL = \hat{\theta}_{qmle} exp\{ -[\frac{z^2_{1-\alpha/2}s^2}{n} + (\frac{s^2}{2} - \frac{(n-1)s^2}{2\chi^2_{1-\alpha/2, n-1}})^2]^{1/2}\} \;\;\;\; (34)}
  and the upper limit \eqn{UL} is given by:
  \deqn{UL = \hat{\theta}_{qmle} exp\{ [\frac{z^2_{1-\alpha/2}s^2}{n} + (\frac{(n-1)s^2}{2\chi^2_{\alpha/2, n-1}} - \frac{s^2}{2})^2]^{1/2}\} \;\;\;\; (35)}
  where \eqn{z_p} denotes the \eqn{p}'th quantile of the standard 
  \link[stats:Normal]{normal distribuiton}, and \eqn{\chi_{p, \nu}} denotes the 
  \eqn{p}'th quantile of the \link[stats:Chisquare]{chi-square distribution} with 
  \eqn{\nu} degrees of freedom.  The \eqn{(1-\alpha)100\%} one-sided lower confidence 
  limit and one-sided upper confidence limit are given by equations (34) and (35), 
  respectively, with \eqn{\alpha/2} replaced by \eqn{\alpha}.

  \emph{Parkin et al.'s Method} (\code{ci.method="parkin"}) \cr
  This method was developed by Parkin et al. (1990).  It can be shown that the 
  mean of a lognormal distribution corresponds to the \eqn{p}'th quantile, where
  \deqn{p = \Phi(\frac{\sigma}{2}) \;\;\;\; (36)}
  and \eqn{\Phi} denotes the cumulative distribution function of the standard 
  \link[stats:Normal]{normal distribution}.  Parkin et al. (1990) suggested 
  estimating \eqn{p} by replacing \eqn{\sigma} in equation (36) with the estimate 
  \eqn{s} as computed in equation (4).  Once an estimate of \eqn{p} is obtained, a 
  nonparametric confidence interval can be constructed for \eqn{p}, assuming \eqn{p} 
  is equal to its estimated value (see \code{\link{eqnpar}}). 
 
  \emph{Cox's Method} (\code{ci.method="cox"}) \cr
  This method was suggested by Professor D.R. Cox and is illustrated in Land (1972).  
  El-Shaarawi (1989) adapts this method to the case of censored water quality data.  
  Cox's idea is to construct an approximate \eqn{(1-\alpha)100\%} confidence interval 
  for the quantity \eqn{\beta} defined in equation (26) above assuming the estimate of 
  \eqn{\beta} is approximately normally distributed, and then exponentiate the 
  confidence limits.  That is, a two-sided \eqn{(1-\alpha)100\%} confidence interval 
  for \eqn{\theta} is constructed as:
  \deqn{[exp(\hat{\beta} - t_{1-\alpha/2, n-1}\hat{\sigma}_{\hat{\beta}}), \; exp(\hat{\beta} + t_{1-\alpha/2, n-1}\hat{\sigma}_{\hat{\beta}})] \;\;\;\; (37)}
  where \eqn{t(p, \nu)} denotes the \eqn{p}'th quantile of 
  \link[stats:TDist]{Student's t-distribution} with \eqn{\nu} degrees of freedom.  
  Note that this method, unlike the normal approximation method discussed below, 
  guarantees a positive value for the lower confidence limit.  One-sided confidence 
  intervals are computed in a similar fashion.

  Define an estimator of \eqn{\beta} by:
  \deqn{\hat{\beta} = \hat{\mu} + \frac{\hat{\sigma}^2}{2} \;\;\;\; (38)}
  Then the variance of this estimator is given by:
  \deqn{Var(\hat{\beta}) = Var(\hat{\mu}) + Cov(\hat{\mu}, \hat{\sigma}^2) + \frac{1}{4}Var(\hat{\sigma}^2) \;\;\;\; (39)}
  The function \code{elnormAlt} follows Land (1972) and uses the minimum variance 
  unbiased estimator for \eqn{\beta} shown in equation (27) above, so the variance and 
  estimated variance of this estimator are:
  \deqn{Var(\hat{\beta}_{mvue}) = \frac{\sigma^2}{n} + \frac{\sigma^4}{2(n-1)} \;\;\;\; (40)}
  \deqn{\hat{\sigma}^2_{\hat{\beta}} = \frac{s^2}{n} + \frac{s^4}{2(n+1)} \;\;\;\; (41)}
  Note that El-Shaarawi (1989, equation 5) simply replaces the value of \eqn{s^2} in 
  equation (41) with some estimator of \eqn{\sigma^2} (the mle or mvue of 
  \eqn{\sigma^2}), rather than using the mvue of the variance of \eqn{\beta} as shown 
  in equation (41).

  \emph{Normal Approximation} (\code{ci.method="normal.approx"})
  This method constructs approximate \eqn{(1-\alpha)100\%} confidence intervals for 
  \eqn{\theta} based on the assumption that the estimator of \eqn{\theta} is 
  approximately normally distributed.  That is, a two-sided \eqn{(1-\alpha)100\%} 
  confidence interval for \eqn{\theta} is constructed as:
  \deqn{[\hat{\theta} - t_{1-\alpha/2, n-1}\hat{\sigma}_{\hat{\theta}}, \; \hat{\theta} + t_{1-\alpha/2, n-1}\hat{\sigma}_{\hat{\theta}}] \;\;\;\; (42)}
  One-sided confidence intervals are computed in a similar fashion.

  When \code{method="mvue"} is used to estimate \eqn{\theta}, an unbiased estimate of 
  the variance of the estimator of \eqn{\theta} is used in equation (42) 
  (Bradu and Mundlak, 1970, equation 4.3; Gilbert, 1987, equation 13.5):
  \deqn{\hat{\sigma^2}_{\hat{\theta}} = e^{2\bar{y}} \{[g_{n-1}(\frac{s^2}{2})]^2 - g_{n-1}[\frac{s^2(n-2)}{n-1}] \} \;\;\;\; (43)}

  When \code{method="mle"} is used to estimate \eqn{\theta}, the estimate of the 
  variance of the estimator of \eqn{\theta} is computed by replacing \eqn{\mu} and 
  \eqn{\sigma^2} in equation (13) with their mle's:
  \deqn{\hat{\sigma}^2_{\hat{\theta}} = exp(2\bar{y} + \frac{\hat{\sigma}^2_{mle}}{n}) \{exp(\frac{\hat{\sigma}^2_{mle}}{n}) [1 - \frac{2\hat{\sigma}^2_{mle}}{n}]^{-(n-1)/2} - [1 - \frac{\hat{\sigma}^2_{mle}}{n}]^{-(n-1)} \} \;\;\;\; (44)}

  When \code{method="qmle"} is used to estimate \eqn{\theta}, the estimate of the 
  variance of the estimator of \eqn{\theta} is computed by replacing \eqn{\mu} and 
  \eqn{\sigma^2} in equation (18) with their mvue's:
  \deqn{\hat{\sigma}^2_{\hat{\theta}} = exp(2\bar{y} + \frac{s^2}{n}) \{exp(\frac{s^2}{n}) [1 - \frac{2 s^2}{n-1}]^{-(n-1)/2} - [1 - \frac{s^2}{n-1}]^{-(n-1)} \} \;\;\;\; (45)}
  Note that equation (45) is exactly the same as Gilbert's (1987, p. 167) equation 
  13.8a, except that Gilbert (1987) erroneously uses \eqn{n} where he should use 
  \eqn{n-1} instead.  For large values of \eqn{n} relative to \eqn{s^2}, however, 
  this makes little difference.

  When \code{method="mme"}, the estimate of the variance of the estimator of 
  \eqn{\theta} is computed by replacing \eqn{eta^2} in equation (22) with the 
  mme of \eqn{\eta^2} defined in equation (20):
  \deqn{\hat{\sigma}^2_{\hat{\theta}} = \frac{\hat{\eta}_{mme}^2}{n} = \frac{1}{n^2} \sum_{i=1}^n (x_i - \bar{x})^2 \;\;\;\; (46)}

  When \code{method="mmue"}, the estimate of the variance of the estimator of 
  \eqn{\theta} is computed by replacing \eqn{eta^2} in equation (22) with the 
  mmue of \eqn{\eta^2} defined in equation (24):
  \deqn{\hat{\sigma}^2_{\hat{\theta}} = \frac{\hat{\eta}_{mmue}^2}{n} = \frac{1}{n(n-1)} \sum_{i=1}^n (x_i - \bar{x})^2 \;\;\;\; (47)}
}
\value{
  a list of class \code{"estimate"} containing the estimated parameters and other information.  
  See \cr
  \code{\link{estimate.object}} for details.
}
\references{
  Aitchison, J., and J.A.C. Brown (1957).  \emph{The Lognormal Distribution 
  (with special references to its uses in economics)}.  Cambridge University Press, 
  London, Chapter 5.

  Armstrong, B.G. (1992).  Confidence Intervals for Arithmetic Means of Lognormally 
  Distributed Exposures.  \emph{American Industrial Hygiene Association Journal} 
  \bold{53}, 481--485.

  Bradu, D., and Y. Mundlak. (1970).  Estimation in Lognormal Linear Models.  
  \emph{Journal of the American Statistical Association} \bold{65}, 198--211.

  Cohn, T.A., L.L. DeLong, E.J. Gilroy, R.M. Hirsch, and D.K. Wells. (1989).  
  Estimating Constituent Loads.  \emph{Water Resources Research} \bold{25}(5), 
  937--942.

  Crow, E.L., and K. Shimizu. (1988).  \emph{Lognormal Distributions: Theory and 
  Applications}.  Marcel Dekker, New York, Chapter 2.

  El-Shaarawi, A.H., and J. Lin. (2007).  Interval Estimation for Log-Normal Mean 
  with Applications to Water Quality.  \emph{Environmetrics} \bold{18}, 1--10.

  El-Shaarawi, A.H., and R. Viveros. (1997).  Inference About the Mean in 
  Log-Regression with Environmental Applications.  \emph{Environmetrics} 
  \bold{8}, 569--582.

  Forbes, C., M. Evans, N. Hastings, and B. Peacock. (2011).  Statistical Distributions. 
  Fourth Edition. John Wiley and Sons, Hoboken, NJ.

  Finney, D.J. (1941).  On the Distribution of a Variate Whose Logarithm is 
  Normally Distributed.  \emph{Supplement to the Journal of the Royal Statistical 
  Society} \bold{7}, 155--161.

  Gilbert, R.O. (1987). \emph{Statistical Methods for Environmental Pollution Monitoring}. 
  Van Nostrand Reinhold, New York, NY.

  Johnson, N. L., S. Kotz, and N. Balakrishnan. (1994). 
  \emph{Continuous Univariate Distributions, Volume 1}. 
  Second Edition. John Wiley and Sons, New York.

  Krishnamoorthy, K., and T.P. Mathew. (2003).  Inferences on the Means of Lognormal 
  Distributions Using Generalized p-Values and Generalized Confidence Intervals.  
  \emph{Journal of Statistical Planning and Inference} \bold{115}, 103--121.

  Land, C.E. (1971).  Confidence Intervals for Linear Functions of the Normal Mean 
  and Variance.  \emph{The Annals of Mathematical Statistics} \bold{42}(4), 1187--1205.

  Land, C.E. (1972).  An Evaluation of Approximate Confidence Interval Estimation 
  Methods for Lognormal Means.  \emph{Technometrics} \bold{14}(1), 145--158.

  Land, C.E. (1973).  Standard Confidence Limits for Linear Functions of the Normal 
  Mean and Variance.  \emph{Journal of the American Statistical Association} 
  \bold{68}(344), 960--963.

  Land, C.E. (1975).  Tables of Confidence Limits for Linear Functions of the 
  Normal Mean and Variance, in 
  \emph{Selected Tables in Mathematical Statistics, Vol. III}.  
  American Mathematical Society, Providence, RI, pp. 385--419.

  Likes, J. (1980).  Variance of the MVUE for Lognormal Variance. 
  \emph{Technometrics} \bold{22}(2), 253--258.

  Limpert, E., W.A. Stahel, and M. Abbt. (2001).  Log-Normal Distributions Across the 
  Sciences:  Keys and Clues.  \emph{BioScience} \bold{51}, 341--352.

  Millard, S.P., and N.K. Neerchal. (2001). \emph{Environmental Statistics with S-PLUS}. 
  CRC Press, Boca Raton, FL.

  Ott, W.R. (1995). \emph{Environmental Statistics and Data Analysis}. 
  Lewis Publishers, Boca Raton, FL.

  Parkin, T.B., J.J. Meisinger, S.T. Chester, J.L. Starr, and J.A. Robinson. (1988).  
  Evaluation of Statistical Estimation Methods for Lognormally Distributed Variables.  
  \emph{Journal of the Soil Science Society of America} \bold{52}, 323--329.

  Parkin, T.B., S.T. Chester, and J.A. Robinson. (1990).  Calculating Confidence 
  Intervals for the Mean of a Lognormally Distributed Variable.  
  \emph{Journal of the Soil Science Society of America} \bold{54}, 321--326.

  Singh, A., A.K. Singh, and R.J. Iaci. (2002). 
  \emph{Estimation of the Exposure Point Concentration Term Using a Gamma Distribution}.  
  EPA/600/R-02/084. October 2002. Technology Support Center for Monitoring and 
  Site Characterization, Office of Research and Development, Office of Solid Waste and 
  Emergency Response, U.S. Environmental Protection Agency, Washington, D.C.

  Singh, A., R. Maichle, and N. Armbya. (2010a). 
  \emph{ProUCL Version 4.1.00 User Guide (Draft)}. EPA/600/R-07/041, May 2010. 
  Office of Research and Development, U.S. Environmental Protection Agency, Washington, D.C.

  Singh, A., N. Armbya, and A. Singh. (2010b). 
  \emph{ProUCL Version 4.1.00 Technical Guide (Draft)}. EPA/600/R-07/041, May 2010.  
  Office of Research and Development, U.S. Environmental Protection Agency, Washington, D.C.

  USEPA. (1992d).  \emph{Supplemental Guidance to RAGS: Calculating the Concentration Term}.  
  Publication 9285.7-081, May 1992.  Intermittenet Bulletin, Volume 1, Number 1.  
  Office of Emergency and Remedial Response, Hazardous Site Evaluation Division, 
  OS-230. Office of Solid Waste and Emergency Response, 
  U.S. Environmental Protection Agency, Washington, D.C.

  USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
  EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.  
  U.S. Environmental Protection Agency, Washington, D.C.

  Zou, G.Y., C.Y. Huo, and J. Taleban. (2009).  Simple Confidence Intervals for 
  Lognormal Means and their Differences with Environmental Applications.  
  \emph{Environmetrics} \bold{20}, 172--180.
}
\author{
    Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  The normal and lognormal distribution are probably the two most frequently used 
  distributions to model environmental data.  In order to make any kind of 
  probability statement about a normally-distributed population (of chemical 
  concentrations for example), you have to first estimate the mean and standard 
  deviation (the population parameters) of the distribution.  Once you estimate 
  these parameters, it is often useful to characterize the uncertainty in the 
  estimate of the mean or variance.  This is done with confidence intervals.

  Some EPA guidance documents (e.g., Singh et al., 2002; Singh et al., 2010a,b) 
  strongly recommend against using a lognormal model for environmental data and 
  recommend trying a gamma distribuiton instead.

  USEPA (1992d) directs persons involved in risk assessment for Superfund sites to 
  use Land's (1971, 1975) method (\code{ci.method="land"}) for computing the upper 
  95\% confidence interval for the mean, assuming the data follow a lognormal 
  distribution (the guidance document cites Gilbert (1987) as a source of descriptions 
  and tables for this method).  The last example in the EXAMPLES section below 
  reproduces an example from this guidance document.

  In the past, some authors suggested using the geometric mean, also called the 
  "rating curve" estimator (Cohn et al., 1989), as the estimator of the mean, 
  \eqn{\theta}.  This estimator is computed as:
  \deqn{\hat{\theta}_{rc} = e^{\bar{y}} \;\;\;\; (48)}
  Cohn et al. (1989) cite several authors who have pointed out this estimator is 
  biased and is not even a consistent estimator of the mean.  In fact, it is the 
  maximum likelihood estimator of the median of the distribution 
  (see \code{\link{eqlnorm}}.)

  Finney (1941) computed the efficiency of the method of moments estimators of the 
  mean (\eqn{\theta}) and variance (\eqn{\eta^2}) of the lognormal distribution 
  (equations (19)-(20)) relative to the mvue's (equations (1)-(2)) as a function of 
  \eqn{\sigma^2} (the variance of the log-transformed observations), and found that 
  while the mme of \eqn{\theta} is reasonably efficient compared to the mvue of 
  \eqn{\theta}, the mme of \eqn{\eta^2} performs quite poorly relative to the 
  mvue of \eqn{\eta^2}.

  Cohn et al. (1989) and Parkin et al. (1988) have shown that the qmle and the mle 
  of the mean can be severely biased for typical environmental data, and suggest 
  always using the mvue.

  Parkin et al. (1990) studied the performance of various methods for constructing a 
  confidence interval for the mean via Monte Carlo simulation.  They compared 
  approximate methods to Land's optimal method (\code{ci.method="land"}).  They used 
  four parent lognormal distributions to generate observations; all had mean 10, but 
  differed in coefficient of variation: 50, 100, 200, and 500\%.  They also generated 
  sample sizes from 6 to 100 in increments of 2.  For each combination of parent 
  distribution and sample size, they generated 25,000 Monte Carlo trials.  
  Parkin et al. found that for small sample sizes (\eqn{n < 20}), none of the 
  approximate methods (\code{"parkin"}, \code{"cox"}, \code{"normal.approx"}) worked 
  very well. For \eqn{n > 20}, their method (\code{"parkin"}) provided reasonably 
  accurate coverage.  Cox's method (\code{"cox"}) worked well for \eqn{n > 60}, and 
  performed slightly better than Parkin et al.'s method (\code{"parkin"}) for highly 
  skewed populations.

  Zou et al. (2009) used Monte Carlo simulation to compare the performance of their 
  method with the CGI method of Krishnamoorthy and Mathew (2003) and 
  the modified Cox method of Armstrong (1992) and El-Shaarawi and Lin (2007).  
  Performance was assessed based on 1) percentage of times the interval contained the 
  parameter value (coverage\%), 2) balance between left and right tail errors, and 
  3) confidence interval width.  All three methods showed acceptable coverage 
  percentages.  The modified Cox method showed unbalanced tail errors, and Zou 
  et al.'s method showed consistently narrower average width.
}
\seealso{
  \link{LognormalAlt}, \link[stats]{Lognormal}, \link[stats]{Normal}.
}
\examples{
  # Using the Reference area TcCB data in the data frame EPA.94b.tccb.df, 
  # estimate the mean and coefficient of variation, 
  # and construct a 95% confidence interval for the mean.

  with(EPA.94b.tccb.df, elnormAlt(TcCB[Area == "Reference"], ci = TRUE))  

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            Lognormal
  #
  #Estimated Parameter(s):          mean = 0.5989072
  #                                 cv   = 0.4899539
  #
  #Estimation Method:               mvue
  #
  #Data:                            TcCB[Area == "Reference"]
  #
  #Sample Size:                     47
  #
  #Confidence Interval for:         mean
  #
  #Confidence Interval Method:      Land
  #
  #Confidence Interval Type:        two-sided
  #
  #Confidence Level:                95%
  #
  #Confidence Interval:             LCL = 0.5243787
  #                                 UCL = 0.7016992

  #----------

  # Compare the different methods of estimating the distribution parameters using the 
  # Reference area TcCB data.

  with(EPA.94b.tccb.df, elnormAlt(TcCB[Area == "Reference"], method = "mvue"))$parameters
  #     mean        cv 
  #0.5989072 0.4899539

  with(EPA.94b.tccb.df, elnormAlt(TcCB[Area == "Reference"], method = "qmle"))$parameters
  #     mean        cv 
  #0.6004468 0.4947791 
 
  with(EPA.94b.tccb.df, elnormAlt(TcCB[Area == "Reference"], method = "mle"))$parameters
  #     mean        cv 
  #0.5990497 0.4888968 
 
  with(EPA.94b.tccb.df, elnormAlt(TcCB[Area == "Reference"], method = "mme"))$parameters
  #     mean        cv 
  #0.5985106 0.4688423 
 
  with(EPA.94b.tccb.df, elnormAlt(TcCB[Area == "Reference"], method = "mmue"))$parameters
  #     mean        cv 
  #0.5985106 0.4739110

  #----------

  # Compare the different methods of constructing the confidence interval for
  # the mean using the Reference area TcCB data.
 
  with(EPA.94b.tccb.df, elnormAlt(TcCB[Area == "Reference"], 
    method = "mvue", ci = TRUE, ci.method = "land"))$interval$limits
  #      LCL       UCL 
  #0.5243787 0.7016992

  with(EPA.94b.tccb.df, elnormAlt(TcCB[Area == "Reference"], 
    method = "mvue", ci = TRUE, ci.method = "zou"))$interval$limits
  #      LCL       UCL 
  #0.5230444 0.6962071 

  with(EPA.94b.tccb.df, elnormAlt(TcCB[Area == "Reference"], 
    method = "mvue", ci = TRUE, ci.method = "parkin"))$interval$limits
  # LCL  UCL 
  #0.50 0.74 
 
  with(EPA.94b.tccb.df, elnormAlt(TcCB[Area == "Reference"], 
     method = "mvue", ci = TRUE, ci.method = "cox"))$interval$limits
  #      LCL       UCL 
  #0.5196213 0.6938444 
 
  with(EPA.94b.tccb.df, elnormAlt(TcCB[Area == "Reference"], 
     method = "mvue", ci = TRUE, ci.method = "normal.approx"))$interval$limits
  #      LCL       UCL 
  #0.5130160 0.6847984 

  #----------

  # Reproduce the example in Highlights 7 and 8 of USEPA (1992d).  This example shows 
  # how to compute the upper 95% confidence limit of the mean of a lognormal distribution 
  # and compares it to the result of computing the upper 95% confidence limit assuming a 
  # normal distribution. The data for this example are chromium concentrations (mg/kg) in 
  # soil samples collected randomly over a Superfund site, and are stored in the data frame 
  # EPA.92d.chromium.vec.

  # First look at the data 

  EPA.92d.chromium.vec
  # [1]   10   13   20   36   41   59   67  110  110  136  140  160  200  230 1300

  stripChart(EPA.92d.chromium.vec, ylab = "Chromium (mg/kg)")

  # Note there is one very large "outlier" (1300).  
  # Perform a goodness-of-fit test to determine whether a lognormal distribution 
  # is appropriate:

  gof.list <- gofTest(EPA.92d.chromium.vec, dist = 'lnormAlt') 
  gof.list 

  #Results of Goodness-of-Fit Test 
  #------------------------------- 
  #
  #Test Method:                     Shapiro-Wilk GOF
  #
  #Hypothesized Distribution:       Lognormal 
  #
  #Estimated Parameter(s):          mean = 159.855185
  #                                 cv   =   1.493994
  #
  #Estimation Method:               mvue
  #
  #Data:                            EPA.92d.chromium.vec
  #
  #Sample Size:                     15
  #
  #Test Statistic:                  W = 0.9607179
  #
  #Test Statistic Parameter:        n = 15
  #
  #P-value:                         0.7048747
  #
  #Alternative Hypothesis:          True cdf does not equal the
  #                                 Lognormal Distribution. 

  plot(gof.list, digits = 2)

  # The lognormal distribution seems to provide an adequate fit, although the largest 
  # observation (1300) is somewhat suspect, and given the small sample size there is 
  # not much power to detect any kind of mild deviation from a lognormal distribution.
  
  # Now compute the one-sided 95\% upper confidence limit for the mean.  
  # Note that the value of 502 mg/kg shown in Hightlight 7 of USEPA (1992d) is a bit 
  # larger than the exact value of 496.6 mg/kg shown below.  
  # This is simply due to rounding error.

  elnormAlt(EPA.92d.chromium.vec, ci = TRUE, ci.type = "upper") 

  #Results of Distribution Parameter Estimation 
  #-------------------------------------------- 
  #
  #Assumed Distribution:          Lognormal 
  #
  #Estimated Parameter(s):        mean = 159.855185
  #                                 cv   =   1.493994 
  #
  #Estimation Method:             mvue 
  #
  #Data:                          EPA.92d.chromium.vec 
  #
  #Sample Size:                   15 
  #
  #Confidence Interval for:       mean 
  #
  #Confidence Interval Method:    Land 
  #
  #Confidence Interval Type:      upper 
  #
  #Confidence Level:              95% 
  #
  #Confidence Interval:           LCL =   0 
  #                               UCL = 496.6282 

  # Now compare this result with the upper 95\% confidence limit based on assuming 
  # a normal distribution.  Again note that the value of 325 mg/kg shown in 
  # Hightlight 8 is slightly larger than the exact value of 320.3 mg/kg shown below.  
  # This is simply due to rounding error.

  enorm(EPA.92d.chromium.vec, ci = TRUE, ci.type = "upper") 

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            Normal
  #
  #Estimated Parameter(s):          mean = 175.4667
  #                                 sd   = 318.5440
  #
  #Estimation Method:               mvue
  #
  #Data:                            EPA.92d.chromium.vec
  #
  #Sample Size:                     15
  #
  #Confidence Interval for:         mean
  #
  #Confidence Interval Method:      Exact
  #
  #Confidence Interval Type:        upper
  #
  #Confidence Level:                95%
  #
  #Confidence Interval:             LCL =     -Inf
  #                                 UCL = 320.3304

  #----------

  # Clean up
  #---------

  rm(gof.list)
}
\keyword{ distribution }
\keyword{ htest }
