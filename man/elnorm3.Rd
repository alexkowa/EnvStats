\name{elnorm3}
\alias{elnorm3}
\title{
  Estimate Parameters of a Three-Parameter Lognormal Distribution (Log-Scale)
}
\description{
  Estimate the mean, standard deviation, and threshold parameters for a 
  \link[=Lognormal3]{three-parameter lognormal distribution}, and optionally 
  construct a confidence interval for the threshold or the median of the distribution.
}
\usage{
  elnorm3(x, method = "lmle", ci = FALSE, ci.parameter = "threshold", 
    ci.method = "avar", ci.type = "two-sided", conf.level = 0.95, 
    threshold.lb.sd = 100, evNormOrdStats.method = "royston")
}
\arguments{
  \item{x}{
  numeric vector of observations.
}
  \item{method}{
  character string specifying the method of estimation.  Possible values are:
  \itemize{ 
    \item \code{"lmle"} (local maximum likelihood; the default)
    \item \code{"mme"} (method of moments)
    \item \code{"mmue"} (method of moments using an unbaised estimate of variance)
    \item \code{"mmme"} (modified method of moments due to Cohen and Whitten (1980))
    \item \code{"zero.skew"} (zero-skewness estimator due to Griffiths (1980))
    \item \code{"royston.skew"} (estimator based on Royston's (1992b) index of skewness).  
  }
  See the DETAILS section for more information.
}
  \item{ci}{
  logical scalar indicating whether to compute a confidence interval for either 
  the threshold or median of the distribution.  The default value is \code{FALSE}.
}
  \item{ci.parameter}{
  character string indicating the parameter for which the confidence interval is 
  desired.  The possible values are \code{"threshold"} (the default) and 
  \code{"median"}.  This argument is ignored if \code{ci=FALSE}.
}
  \item{ci.method}{
  character string indicating the method to use to construct the confidence interval.  
  The possible values are \code{"avar"} (asymptotic variance; the default), \cr
  \code{"likelihood.profile"}, and \code{"skewness"} (method suggested by Royston 
  (1992b) for \code{method="zero.skew"}).  This argument is ignored if \code{ci=FALSE}.
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
  \item{threshold.lb.sd}{
  a positive numeric scalar specifying the range over which to look for the 
  local maximum likelihood (\code{method="lmle"}) or zero-skewness \cr
  (\code{method="zero.skewness"}) estimator of threshold.  The range is set to \cr
  \code{[ mean(x) - threshold.lb.sd * sd(x), min(x) ]}.  If you receive a warning 
  message that \code{elnorm3} is unable to find an acceptable estimate of threshold 
  in this range, it may be because of convergence problems specific to the data in 
  \code{x}.  When this occurs, try changing the value of \code{threshold.lb.sd}.  This 
  same range is used in constructing confidence intervals for the threshold parameter.  
  The default value is \code{threshold.lb.sd=100}.  This argument is relevant only if 
  \code{method="lmle"}, \code{method="zero.skew"}, 
  \code{ci.method="likelihood.profile"}, and/or \code{ci.method="skewness"}.
}
  \item{evNormOrdStats.method}{
  character string indicating which method to use in the call to 
  \code{link{evNormOrdStatsScalar}} when \code{method="mmme"}.  See the DETAILS 
  section for more information. 
}
}
\details{
  If \code{x} contains any missing (\code{NA}), undefined (\code{NaN}) or 
  infinite (\code{Inf}, \code{-Inf}) values, they will be removed prior to 
  performing the estimation.

  Let \eqn{X} denote a random variable from a 
  \link[=Lognormal3]{three-parameter lognormal distribution} with 
  parameters \code{meanlog=}\eqn{\mu}, \code{sdlog=}\eqn{\sigma}, and 
  \code{threshold=}\eqn{\gamma}.  Let \eqn{\underline{x}} denote a vector of 
  \eqn{n} observations from this distribution.  Furthermore, let \eqn{x_{(i)}} denote 
  the \eqn{i}'th order statistic in the sample, so that \eqn{x_{(1)}} denotes the 
  smallest value and \eqn{x_{(n)}} denote the largest value in \eqn{\underline{x}}.  
  Finally, denote the sample mean and variance by:
  \deqn{\bar{x} = \frac{1}{n} \sum_{i=1}^n x_i \;\;\;\; (1)}
  \deqn{s^2 = \frac{1}{n-1} \sum_{i=1}^n (x_i - \bar{x})^2 \;\;\;\; (2)}
  Note that the sample variance is the unbiased version.  Denote the method of 
  moments estimator of variance by:
  \deqn{s^2_m = \frac{1}{n} \sum_{i=1}^n (x_i - \bar{x})^2 \;\;\;\; (3)}

  \bold{Estimation} \cr

  \emph{Local Maximum Likelihood Estimation} (\code{method="lmle"}) \cr
  Hill (1963) showed that the likelihood function approaches infinity as \eqn{\gamma} 
  approaches \eqn{x_{(1)}}, so that the global maximum likelihood estimators of 
  \eqn{(\mu, \sigma, \gamma)} are \eqn{(-\infty, \infty, x_{(1)})}, which are 
  inadmissible, since \eqn{\gamma} must be smaller than \eqn{x_{(1)}}.  Cohen (1951) 
  suggested using local maximum likelihood estimators (lmle's), derived by equating 
  partial derivatives of the log-likelihood function to zero.  These estimators were 
  studied by Harter and Moore (1966), Calitz (1973), Cohen and Whitten (1980), and 
  Griffiths (1980), and appear to possess most of the desirable properties ordinarily 
  associated with maximum likelihood estimators.

  Cohen (1951) showed that the lmle of \eqn{\gamma} is given by the solution to the 
  following equation:
  \deqn{[\sum_{i=1}^n \frac{1}{w_i}] \, \{\sum_{i=1}^n y_i - \sum_{i=1}^n y_i^2 + \frac{1}{n}[\sum_{i=1}^n y_i]^2 \} - n \sum_{i=1}^n \frac{y_i}{w_i} = 0 \;\;\;\; (4)}
  where
  \deqn{w_i = x_i - \hat{\gamma} \;\;\;\; (5)}
  \deqn{y_i = log(x_i - \hat{\gamma}) = log(w_i) \;\;\;\; (6)}
  and that the lmle's of \eqn{\mu} and \eqn{\sigma} then follow as:
  \deqn{\hat{\mu} = \frac{1}{n} \sum_{i=1}^n y_i = \bar{y} \;\;\;\; (7)}
  \deqn{\hat{\sigma}^2 = \frac{1}{n} \sum_{i=1}^n (y_i - \bar{y})^2 \;\;\;\; (8)}
  Unfortunately, while equation (4) simplifies the task of computing the lmle's, 
  for certain data sets there still may be convergence problems (Calitz, 1973), and 
  occasionally multiple roots of equation (4) may exist.  When multiple roots to 
  equation (4) exisit, Cohen and Whitten (1980) recommend using the one that results 
  in closest agreement between the mle of \eqn{\mu} (equation (7)) and the sample 
  mean (equation (1)).

  On the other hand, Griffiths (1980) showed that for a given value of the threshold 
  parameter \eqn{\gamma}, the maximized value of the log-likelihood (the 
  \dQuote{profile likelihood} for \eqn{\gamma}) is given by:
  \deqn{log[L(\gamma)] = \frac{-n}{2} [1 + log(2\pi) + 2\hat{\mu} + log(\hat{\sigma}^2) ] \;\;\;\; (9)}
  where the estimates of \eqn{\mu} and \eqn{\sigma} are defined in equations (7) 
  and (8), so the lmle of \eqn{\gamma} reduces to an iterative search over the values 
  of \eqn{\gamma}.  Griffiths (1980) noted that the distribution of the lmle of 
  \eqn{\gamma} is far from normal and that \eqn{log[L(\gamma)]} is not quadratic 
  near the lmle of \eqn{\gamma}.  He suggested a better parameterization based on
  \deqn{\eta = -log(x_{(1)} - \gamma) \;\;\;\; (10)}
  Thus, once the lmle of \eqn{\eta} is found using equations (9) and (10), the lmle of 
  \eqn{\gamma} is given by:
  \deqn{\hat{\gamma} = x_{(1)} - exp(-\hat{\eta}) \;\;\;\; (11)}
  When \code{method="lmle"}, the function \code{elnorm3} uses the function 
  \code{\link{nlminb}} to search for the minimum of \eqn{-2log[L(\eta)]}, using the 
  modified method of moments estimator (\code{method="mmme"}; see below) as the 
  starting value for \eqn{\gamma}.  Equation (11) is then used to solve for the 
  lmle of \eqn{\gamma}, and equation (4) is used to \dQuote{fine tune} the estimated 
  value of \eqn{\gamma}.  The lmle's of \eqn{\mu} and \eqn{\sigma} are then computed 
  using equations (6)-(8).
  \cr

  \emph{Method of Moments Estimation} (\code{method="mme"}) \cr  
  Denote the \eqn{r}'th sample central moment by:
  \deqn{m_r = \frac{1}{n} \sum_{i=1}^n (x_i - \bar{x})^r \;\;\;\; (12)}
  and note that
  \deqn{s^2_m = m_2 \;\;\;\; (13)}
  Equating the sample first moment (the sample mean) with its population value 
  (the population mean), and equating the second and third sample central moments 
  with their population values yields (Johnson et al., 1994, p.228):
  \deqn{\bar{x} = \gamma + \beta \sqrt{\omega} \;\;\;\; (14)}
  \deqn{m_2 = s^2_m = \beta^2 \omega (\omega - 1) \;\;\;\; (15)}
  \deqn{m_3 = \beta^3 \omega^{3/2} (\omega - 1)^2 (\omega + 2) \;\;\;\; (16)}
  where
  \deqn{\beta = exp(\mu) \;\;\;\; (17)}
  \deqn{\omega = exp(\sigma^2) \;\;\;\; (18)}
  Combining equations (15) and (16) yields:
  \deqn{b_1 = \frac{m_3}{m_2^{3/2}} = (\omega + 2) \sqrt{\omega - 1} \;\;\;\; (19)}
  The quantity on the left-hand side of equation (19) is the usual estimator of 
  skewness.  Solving equation (19) for \eqn{\omega} yields:
  \deqn{\hat{\omega} = (d + h)^{1/3} + (d - h)^{1/3} - 1 \;\;\;\; (20)}
  where
  \deqn{d = 1 + \frac{b_1}{2} \;\;\;\; (21)}
  \deqn{h = sqrt{d^2 - 1} \;\;\;\; (22)}
  Using equation (18), the method of moments estimator of \eqn{\sigma} is then 
  computed as:
  \deqn{\hat{\sigma}^2 = log(\hat{\omega}) \;\;\;\; (23)}
  Combining equations (15) and (17), the method of moments estimator of \eqn{\mu} 
  is computed as:
  \deqn{\hat{\mu} = \frac{1}{2} log[\frac{s^2_m}{\hat{omega}(\hat{\omega} - 1)}] \;\;\;\; (24)}
  Finally, using equations (14), (17), and (18), the method of moments estimator of 
  \eqn{\gamma} is computed as:
  \deqn{\bar{x} - exp(\hat{mu} + \frac{\hat{\sigma}^2}{2}) \;\;\;\; (25)}
  There are two major problems with using method of moments estimators for the 
  three-parameter lognormal distribution.  First, they are subject to very large 
  sampling error due to the use of second and third sample moments 
  (Cohen, 1988, p.121; Johnson et al., 1994, p.228).  Second, Heyde (1963) showed 
  that the lognormal distribution is not uniquely determined by its moments.
  \cr

  \emph{Method of Moments Estimators Using an Unbiased Estimate of Variance} (\code{method="mmue"}) \cr
  This method of estimation is exactly the same as the method of moments 
  (\code{method="mme"}), except that the unbiased estimator of variance (equation (3)) 
  is used in place of the method of moments one (equation (4)).  This modification is 
  given in Cohen (1988, pp.119-120).
  \cr

  \emph{Modified Method of Moments Estimation} (\code{method="mmme"}) \cr
  This method of estimation is described by Cohen (1988, pp.125-132).  It was 
  introduced by Cohen and Whitten (1980; their MME-II with r=1) and was further 
  investigated by Cohen et al. (1985).  It is motivated by the fact that the first 
  order statistic in the sample, \eqn{x_{(1)}}, contains more information about 
  the threshold parameter \eqn{\gamma} than any other observation and often more 
  information than all of the other observations combined (Cohen, 1988, p.125).

  The first two sets of equations are the same as for the modified method of moments 
  estimators (\code{method="mmme"}), i.e., equations (14) and (15) with the 
  unbiased estimator of variance (equation (3)) used in place of the method of 
  moments one (equation (4)).  The third equation replaces equation (16) 
  by equating a function of the first order statistic with its expected value:
  \deqn{log(x_{(1)} - \gamma) = \mu + \sigma E[Z_{(1,n)}] \;\;\;\; (26)}
  where \eqn{E[Z_{(i,n)}]} denotes the expected value of the \eqn{i}'th order 
  statistic in a random sample of \eqn{n} observations from a standard normal 
  distribution.  (See the help file for \code{\link{evNormOrdStats}} for information 
  on how \eqn{E[Z_{(i,n)}]} is computed.)  Using equations (17) and (18), 
  equation (26) can be rewritten as:
  \deqn{x_{(1)} = \gamma + \beta exp\{\sqrt{log(\omega)} \, E[Z_{(i,n)}] \} \;\;\;\; (27)}
  Combining equations (14), (15), (17), (18), and (27) yields the following equation 
  for the estimate of \eqn{\omega}:
  \deqn{\frac{s^2}{[\bar{x} - x_{(1)}]^2} = \frac{\hat{\omega}(\hat{\omega} - 1)}{[\sqrt{\hat{\omega}} - exp\{\sqrt{log(\omega)} \, E[Z_{(i,n)}] \} ]^2} \;\;\;\; (28)}
  After equation (28) is solved for \eqn{\hat{\omega}}, the estimate of \eqn{\sigma} 
  is again computed using equation (23), and the estimate of \eqn{\mu} is computed 
  using equation (24), where the unbiased estimate of variaince is used in place of 
  the biased one (just as for \code{method="mmue"}).
  \cr

  \emph{Zero-Skewness Estimation} (\code{method="zero.skew"}) \cr
  This method of estimation was introduced by Griffiths (1980), and elaborated upon 
  by Royston (1992b).  The idea is that if the threshold parameter \eqn{\gamma} were 
  known, then the distribution of:
  \deqn{Y = log(X - \gamma) \;\;\;\; (29)}
  is normal, so the skew of \eqn{Y} is 0.  Thus, the threshold parameter \eqn{\gamma} 
  is estimated as that value that forces the sample skew (defined in equation (19)) of 
  the observations defined in equation (6) to be 0.  That is, the zero-skewness 
  estimator of \eqn{\gamma} is the value that satisfies the following equation:
  \deqn{0 = \frac{\frac{1}{n} \sum_{i=1}^n (y_i - \bar{y})^3}{[\frac{1}{n} \sum_{i=1}^n (y_i - \bar{y})^2]^{3/2}} \;\;\;\; (30)}
  where
  \deqn{y_i = log(x_i - \hat{\gamma}) \;\;\;\; (31)}
  Note that since the denominator in equation (30) is always positive (assuming 
  there are at least two unique values in \eqn{\underline{x}}), only the numerator 
  needs to be used to determine the value of \eqn{\hat{\gamma}}.

  Once the value of \eqn{\hat{\gamma}} has been determined, \eqn{\mu} and \eqn{\sigma} 
  are estimated using equations (7) and (8), except the unbiased estimator of variance 
  is used in equation (8).

  Royston (1992b) developed a modification of the Shaprio-Wilk goodness-of-fit test 
  for normality based on tranforming the data using equation (6) and the zero-skewness 
  estimator of \eqn{\gamma} (see \code{\link{gofTest}}).
  \cr
 
  \emph{Estimators Based on Royston's Index of Skewness} (\code{method="royston.skew"}) \cr
  This method of estimation is discussed by Royston (1992b), and is similar to the 
  zero-skewness method discussed above, except a different measure of skewness is used.  
  Royston's (1992b) index of skewness is given by:
  \deqn{q = \frac{y_{(n)} - \tilde{y}}{\tilde{y} - y_{(1)}} \;\;\;\; (32)}
  where \eqn{y_{(i)}} denotes the \eqn{i}'th order statistic of \eqn{y} and \eqn{y} 
  is defined in equation (31) above, and \eqn{\tilde{y}} denotes the median of \eqn{y}.  
  Royston (1992b) shows that the value of \eqn{\gamma} that yields a value of 
  \eqn{q=0} is given by:
  \deqn{\hat{\gamma} = \frac{y_{(1)}y_{(n)} - \tilde{y}^2}{y_{(1)} + y_{(n)} - 2\tilde{y}} \;\;\;\; (33)}
  Again, as for the zero-skewness method, once the value of \eqn{\hat{\gamma}} has 
  been determined, \eqn{\mu} and \eqn{\sigma} are estimated using equations (7) and (8), 
  except the unbiased estimator of variance is used in equation (8).

  Royston (1992b) developed this estimator as a quick way to estimate \eqn{\gamma}.
  \cr

  \bold{Confidence Intervals} \cr
  This section explains three different methods for constructing confidence intervals 
  for the threshold parameter \eqn{\gamma}, or the median of the three-parameter 
  lognormal distribution, which is given by:
  \deqn{Med[X] = \gamma + exp(\mu) = \gamma + \beta \;\;\;\; (34)}
  \cr

  \emph{Normal Approximation Based on Asymptotic Variances and Covariances} (\code{ci.method="avar"}) \cr
  Formulas for asymptotic variances and covariances for the three-parameter lognormal 
  distribution, based on the information matrix, are given in Cohen (1951), Cohen and 
  Whitten (1980), Cohen et al., (1985), and Cohen (1988).  The relevant quantities for 
  \eqn{\gamma} and the median are:
  \deqn{Var(\hat{\gamma}) = \sigma^2_{\hat{\gamma}} = \frac{\sigma^2}{n} \, (\frac{\beta^2}{\omega}) H \;\;\;\; (35)}
  \deqn{Var(\hat{\beta}) = \sigma^2_{\hat{\beta}} = \frac{\sigma^2}{n} \, \beta^2 (1 + H) \;\;\;\; (36)}
  \deqn{Cov(\hat{\gamma}, \hat{\beta}) = \sigma_{\hat{\gamma}, \hat{\beta}} = \frac{-\sigma^3}{n} \, (\frac{\beta^2}{\sqrt{\omega}}) H \;\;\;\; (37)}
  where 
  \deqn{H = [\omega (1 + \sigma^2) - 2\sigma^2 - 1]^{-1} \;\;\;\; (38)}
  A two-sided \eqn{(1-\alpha)100\%} confidence interval for \eqn{\gamma} is computed as:
  \deqn{\hat{\gamma} - t_{n-2, 1-\alpha/2} \hat{\sigma}_{\hat{\gamma}}, \, \hat{\gamma} + t_{n-2, 1-\alpha/2} \hat{\sigma}_{\hat{\gamma}} \;\;\;\; (39)}
  where \eqn{t_{\nu, p}} denotes the \eqn{p}'th quantile of 
  \link[stats:TDist]{Student's t-distribution} with \eqn{n} degrees of freedom, and the 
  quantity \eqn{\hat{\sigma}_{\hat{\gamma}}} is computed using equations (35) and (38) 
  and substituting estimated values of \eqn{\beta}, \eqn{\omega}, and \eqn{\sigma}.  
  One-sided confidence intervals are computed in a similar manner.

  A two-sided \eqn{(1-\alpha)100\%} confidence interval for the median (see equation 
  (34) above) is computed as:
  \deqn{\hat{\gamma} + \hat{\beta} - t_{n-2, 1-\alpha/2} \hat{\sigma}_{\hat{\gamma} + \hat{\beta}}, \, \hat{\gamma} + \hat{\beta} + t_{n-2, 1-\alpha/2} \hat{\sigma}_{\hat{\gamma} + \hat{\beta}} \;\;\;\; (40)}
  where 
  \deqn{\hat{\sigma}^2_{\hat{\gamma} + \hat{\beta}} = \hat{\sigma}^2_{\hat{\gamma}} + \hat{\sigma}^2_{\hat{\beta}} + \hat{\sigma}_{\hat{\gamma}, \hat{\beta}} \;\;\;\; (41)}
  is computed using equations (35)-(38) and substituting estimated values of 
  \eqn{\beta}, \eqn{\omega}, and \eqn{\sigma}.  One-sided confidence intervals are 
  computed in a similar manner.

  This method of constructing confidence intervals is analogous to using the Wald test 
  (e.g., Silvey, 1975, pp.115-118) to test hypotheses on the parameters.

  Because of the regularity problems associated with the global maximum likelihood 
  estimators, it is questionble whether the asymptotic variances and covariances shown 
  above apply to local maximum likelihood estimators.  Simulation studies, however, 
  have shown that these estimates of variance and covariance perform reasonably well 
  (Harter and Moore, 1966; Cohen and Whitten, 1980).

  Note that this method of constructing confidence intervals can be used with 
  estimators other than the lmle's.  Cohen and Whitten (1980) and Cohen et al. (1985) 
  found that the asymptotic variances and covariances are reasonably close to 
  corresponding simulated variances and covariances for the modified method of moments 
  estimators (\code{method="mmme"}).
  \cr

  \emph{Likelihood Profile} (\code{ci.method="likelihood.profile"}) \cr
  Griffiths (1980) suggested constructing confidence intervals for the threshold 
  parameter \eqn{\gamma} based on the profile likelihood function given in equations 
  (9) and (10).  Royston (1992b) further elaborated upon this procedure.  A 
  two-sided \eqn{(1-\alpha)100\%} confidence interval for \eqn{\eta} is constructed as:
  \deqn{[\eta_{LCL}, \eta_{UCL}] \;\;\;\; (42)}
  by finding the two values of \eqn{\eta} (one larger than the lmle of \eqn{\eta} and 
  one smaller than the lmle of \eqn{\eta}) that satisfy:
  \deqn{log[L(\eta)] = log[L(\hat{\eta}_{lmle})] - \frac{1}{2} \chi^2_{1, \alpha/2} \;\;\;\; (43)}
  where \eqn{\chi^2_{\nu, p}} denotes the \eqn{p}'th quantile of the 
  \link[stats:Chisquare]{chi-square distribution} with \eqn{\nu} degrees of freedom.  
  Once these values are found, the two-sided confidence for \eqn{\gamma} is computed as:
  \deqn{[\gamma_{LCL}, \gamma_{UCL}] \;\;\;\; (44)}
  where
  \deqn{\gamma_{LCL} = x_{(1)} - exp(-\eta_{LCL}) \;\;\;\; (45)}
  \deqn{\gamma_{UCL} = x_{(1)} - exp(-\eta_{UCL}) \;\;\;\; (46)}
  One-sided intervals are construced in a similar manner.
 
  This method of constructing confidence intervals is analogous to using the 
  likelihood-ratio test (e.g., Silvey, 1975, pp.108-115) to test hypotheses on the 
  parameters.

  To construct a two-sided \eqn{(1-\alpha)100\%} confidence interval for the median 
  (see equation (34)), Royston (1992b) suggested the following procedure:
  \enumerate{
    \item Construct a confidence interval for \eqn{\gamma} using the likelihood 
      profile procedure.
  \item Construct a confidence interval for \eqn{\beta} as: 
    \deqn{[\beta_{LCL}, \beta_{UCL}] = [exp(\hat{\mu} - t_{n-2, 1-\alpha/2} \frac{\hat{\sigma}}{n}), \, exp(\hat{\mu} + t_{n-2, 1-\alpha/2} \frac{\hat{\sigma}}{n})] \;\;\;\; (47)}
  \item Construct the confidence interval for the median as: 
    \deqn{[\gamma_{LCL} + \beta_{LCL}, \gamma_{UCL} + \beta_{UCL}] \;\;\;\; (48)}
  } 

  Royston (1992b) actually suggested using the quantile from the standard normal 
  distribution instead of Student's t-distribution in step 2 above.  The function 
  \code{elnorm3}, however, uses the Student's t quantile.

  Note that this method of constructing confidence intervals can be used with 
  estimators other than the lmle's.
  \cr
 
  \emph{Royston's Confidence Interval Based on Significant Skewness} (\code{ci.method="skewness"}) \cr
  Royston (1992b) suggested constructing confidence intervals for the threshold 
  parameter \eqn{\gamma} based on the idea behind the zero-skewness estimator 
  (\code{method="zero.skew"}).  A two-sided \eqn{(1-\alpha)100\%} confidence interval 
  for \eqn{\gamma} is constructed by finding the two values of \eqn{\gamma} that yield 
  a p-value of \eqn{\alpha/2} for the test of zero-skewness on the observations 
  \eqn{\underline{y}} defined in equation (6) (see \code{\link{gofTest}}).  One-sided 
  confidence intervals are constructed in a similar manner.

  To construct \eqn{(1-\alpha)100\%} confidence intervals for the median 
  (see equation (34)), the exact same procedure is used as for 
  \code{ci.method="likelihood.profile"}, except that the confidence interval for 
  \eqn{\gamma} is based on the zero-skewness method just described instead of the 
  likelihood profile method.
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

  Calitz, F. (1973).  Maximum Likelihood Estimation of the Parameters of the 
  Three-Parameter Lognormal Distribution--a Reconsideration.  \emph{Australian 
  Journal of Statistics} \bold{15}(3), 185--190.

  Cohen, A.C. (1951).  Estimating Parameters of Logarithmic-Normal Distributions by 
  Maximum Likelihood.  \emph{Journal of the American Statistical Association} 
  \bold{46}, 206--212.

  Cohen, A.C. (1988).  Three-Parameter Estimation.  In Crow, E.L., and K. Shimizu, eds. 
  \emph{Lognormal Distributions: Theory and Applications}.  Marcel Dekker, New York, 
  Chapter 4.

  Cohen, A.C., and B.J. Whitten. (1980).  Estimation in the Three-Parameter Lognormal 
  Distribution.  \emph{Journal of the American Statistical Association} \bold{75}, 
  399--404.

  Cohen, A.C., B.J. Whitten, and Y. Ding. (1985).  Modified Moment Estimation for the 
  Three-Parameter Lognormal Distribution.  \emph{Journal of Quality Technology} 
  \bold{17}, 92--99.

  Crow, E.L., and K. Shimizu. (1988).  \emph{Lognormal Distributions: Theory and 
  Applications}.  Marcel Dekker, New York, Chapter 2.

  Griffiths, D.A. (1980).  Interval Estimation for the Three-Parameter Lognormal 
  Distribution via the Likelihood Function.  \emph{Applied Statistics} \bold{29}, 
  58--68.

  Harter, H.L., and A.H. Moore. (1966).  Local-Maximum-Likelihood Estimation of the 
  Parameters of Three-Parameter Lognormal Populations from Complete and Censored 
  Samples.  \emph{Journal of the American Statistical Association} \bold{61}, 842--851.

  Heyde, C.C. (1963).  On a Property of the Lognormal Distribution.  \emph{Journal of 
  the Royal Statistical Society, Series B} \bold{25}, 392--393.

  Hill, .B.M. (1963).  The Three-Parameter Lognormal Distribution and Bayesian 
  Analysis of a Point-Source Epidemic.  \emph{Journal of the American Statistical 
  Association} \bold{58}, 72--84.

  Hoshi, K., J.R. Stedinger, and J. Burges. (1984).  Estimation of Log-Normal 
  Quantiles: Monte Carlo Results and First-Order Approximations.  \emph{Journal of 
  Hydrology} \bold{71}, 1--30.

  Johnson, N. L., S. Kotz, and N. Balakrishnan. (1994). 
  \emph{Continuous Univariate Distributions, Volume 1}. 
  Second Edition. John Wiley and Sons, New York.

  Royston, J.P. (1992b).  Estimation, Reference Ranges and Goodness of Fit for the 
  Three-Parameter Log-Normal Distribution.  \emph{Statistics in Medicine} \bold{11}, 
  897--912.

  Stedinger, J.R. (1980).  Fitting Lognormal Distributions to Hydrologic Data. 
  \emph{Water Resources Research} \bold{16}(3), 481--490.
}
\author{
    Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  The problem of estimating the parameters of a three-parameter lognormal distribution 
  has been extensively discussed by Aitchison and Brown (1957, Chapter 6), 
  Calitz (1973), Cohen (1951), Cohen (1988), Cohen and Whitten (1980), 
  Cohen et al. (1985), Griffiths (1980), Harter and Moore (1966), Hill (1963), and 
  Royston (1992b).  Stedinger (1980) and Hoshi et al. (1984) discuss fitting the 
  three-parameter lognormal distribution to hydrologic data.

  The global maximum likelihood estimates are inadmissible.  In the past, several 
  researchers have found that the local maximum likelihood estimates (lmle's) 
  occasionally fail because of convergence problems, but they were not using the 
  likelihood profile and reparameterization of Griffiths (1980).  Cohen (1988) 
  recommends the modified methods of moments estimators over lmle's because they are 
  easy to compute, they are unbiased with respect to \eqn{\mu} and \eqn{\sigma^2} (the 
  mean and standard deviation on the log-scale), their variances are minimal or near 
  minimal, and they do not suffer from regularity problems.

  Because the distribution of the lmle of the threshold parameter \eqn{\gamma} is far 
  from normal for moderate sample sizes (Griffiths, 1980), it is questionable whether 
  confidence intervals for \eqn{\gamma} or the median based on asymptotic variances 
  and covariances will perform well.  Cohen and Whitten (1980) and Cohen et al. (1985), 
  however, found that the asymptotic variances and covariances are reasonably close to 
  corresponding simulated variances and covariances for the modified method of moments 
  estimators (\code{method="mmme"}).  In a simulation study (5000 monte carlo trials), 
  Royston (1992b) found that the coverage of confidence intervals for \eqn{\gamma} 
  based on the likelihood profile (\code{ci.method="likelihood.profile"}) was very 
  close the nominal level (94.1\% for a nominal level of 95\%), although not 
  symmetric.  Royston (1992b) also found that the coverage of confidence intervals 
  for \eqn{\gamma} based on the skewness method (\code{ci.method="skewness"}) was also 
  very close (95.4\%) and symmetric.
}
\seealso{
  \link{Lognormal3}, \link[stats]{Lognormal}, \link{LognormalAlt}, 
  \link[stats]{Normal}.
}
\examples{
  # Generate 20 observations from a 3-parameter lognormal distribution 
  # with parameters meanlog=1.5, sdlog=1, and threshold=10, then use 
  # Cohen and Whitten's (1980) modified moments estimators to estimate 
  # the parameters, and construct a confidence interval for the 
  # threshold based on the estimated asymptotic variance. 
  # (Note: the call to set.seed simply allows you to reproduce this example.)

  set.seed(250) 
  dat <- rlnorm3(20, meanlog = 1.5, sdlog = 1, threshold = 10) 
  elnorm3(dat, method = "mmme", ci = TRUE)

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            3-Parameter Lognormal
  #
  #Estimated Parameter(s):          meanlog   = 1.5206664
  #                                 sdlog     = 0.5330974
  #                                 threshold = 9.6620403
  #
  #Estimation Method:               mmme
  #
  #Data:                            dat
  #
  #Sample Size:                     20
  #
  #Confidence Interval for:         threshold
  #
  #Confidence Interval Method:      Normal Approximation
  #                                 Based on Asymptotic Variance
  #
  #Confidence Interval Type:        two-sided
  #
  #Confidence Level:                95%
  #
  #Confidence Interval:             LCL =  6.985258
  #                                 UCL = 12.338823

  #----------

  # Repeat the above example using the other methods of estimation 
  # and compare. 

  round(elnorm3(dat, "lmle")$parameters, 1) 
  #meanlog     sdlog threshold 
  #    1.3       0.7      10.5 
 
  round(elnorm3(dat, "mme")$parameters, 1) 
  #meanlog     sdlog threshold 
  #    2.1       0.3       6.0 
 
  round(elnorm3(dat, "mmue")$parameters, 1) 
  #meanlog     sdlog threshold 
  #    2.2       0.3       5.8 
  
  round(elnorm3(dat, "mmme")$parameters, 1) 
  #meanlog     sdlog threshold 
  #    1.5       0.5       9.7 
  
  round(elnorm3(dat, "zero.skew")$parameters, 1) 
  #meanlog     sdlog threshold 
  #    1.3       0.6      10.3 
 
  round(elnorm3(dat, "royston")$parameters, 1)
  #meanlog     sdlog threshold 
  #    1.4       0.6      10.1 

  #----------

  # Compare methods for computing a two-sided 95\% confidence interval 
  # for the threshold: 
  # modified method of moments estimator using asymptotic variance, 
  # lmle using asymptotic variance, 
  # lmle using likelihood profile, and 
  # zero-skewness estimator using the skewness method.

  elnorm3(dat, method = "mmme", ci = TRUE, 
    ci.method = "avar")$interval$limits 
  #      LCL       UCL 
  # 6.985258 12.338823 
 
  elnorm3(dat, method = "lmle", ci = TRUE, 
    ci.method = "avar")$interval$limits 
  #       LCL       UCL 
  #  9.017223 11.980107 
 
  elnorm3(dat, method = "lmle", ci = TRUE, 
    ci.method="likelihood.profile")$interval$limits 
  #      LCL       UCL 
  # 3.699989 11.266029 
 

  elnorm3(dat, method = "zero.skew", ci = TRUE, 
    ci.method = "skewness")$interval$limits 
  #      LCL       UCL 
  #-25.18851  11.18652

  #----------

  # Now construct a confidence interval for the median of the distribution 
  # based on using the modified method of moments estimator for threshold 
  # and the asymptotic variances and covariances.  Note that the true median 
  # is given by threshold + exp(meanlog) = 10 + exp(1.5) = 14.48169.

  elnorm3(dat, method = "mmme", ci = TRUE, ci.parameter = "median") 

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            3-Parameter Lognormal
  #
  #Estimated Parameter(s):          meanlog   = 1.5206664
  #                                 sdlog     = 0.5330974
  #                                 threshold = 9.6620403
  #
  #Estimation Method:               mmme
  #
  #Data:                            dat
  #
  #Sample Size:                     20
  #
  #Confidence Interval for:         median
  #
  #Confidence Interval Method:      Normal Approximation
  #                                 Based on Asymptotic Variance
  #
  #Confidence Interval Type:        two-sided
  #
  #Confidence Level:                95%
  #
  #Confidence Interval:             LCL = 11.20541
  #                                 UCL = 17.26922

  #----------

  # Compare methods for computing a two-sided 95\% confidence interval 
  # for the median: 
  # modified method of moments estimator using asymptotic variance, 
  # lmle using asymptotic variance, 
  # lmle using likelihood profile, and 
  # zero-skewness estimator using the skewness method.

  elnorm3(dat, method = "mmme", ci = TRUE, ci.parameter = "median", 
    ci.method = "avar")$interval$limits 
  #     LCL      UCL 
  #11.20541 17.26922 
 
  elnorm3(dat, method = "lmle", ci = TRUE, ci.parameter = "median", 
    ci.method = "avar")$interval$limits 
  #     LCL      UCL 
  #12.28326 15.87233 

  elnorm3(dat, method = "lmle", ci = TRUE, ci.parameter = "median", 
    ci.method = "likelihood.profile")$interval$limits 
  #      LCL       UCL 
  # 6.314583 16.165525 

  elnorm3(dat, method = "zero.skew", ci = TRUE, ci.parameter = "median", 
    ci.method = "skewness")$interval$limits 
  #      LCL       UCL 
  #-22.38322  16.33569

  #----------

  # Clean up
  #---------

  rm(dat)

}
\keyword{ distribution }
\keyword{ htest }
