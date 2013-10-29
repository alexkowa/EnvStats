\name{HoskingEtAl1985}
\alias{HoskingEtAl1985}
\alias{Hosking et al 1985}
\concept{Hosking}
\concept{Abstract}
\title{
  Abstract:  Hosking et al. (1985)
}
\description{
  Detailed abstract of the manuscript: \cr \cr
  Hosking, J.R.M., J.R. Wallis, and E.F. Wood. (1985).  Estimation of the 
  Generalized Extreme-Value Distribution by the Method of Probability-Weighted 
  Moments.  \emph{Technometrics} \bold{27}(3), 251--261.
}
\details{
  \bold{Abstract} \cr
  Hosking et al. (1985) use the method of probability-weighted moments, 
  introduced by Greenwood et al. (1979), to estimate the parameters of the 
  \link[=GEVD]{generalized extreme value distribution} (GEVD) with parameters 
  \code{location=}\eqn{\eta}, \code{scale=}\eqn{\theta}, and 
  \code{shape=}\eqn{\kappa}.  Hosking et al. (1985) derive the asymptotic 
  distributions of the probability-weighted moment estimators (PWME), and compare 
  the asymptotic and small-sample statistical properties (via computer simulation) 
  of the PWME with maximum likelihood estimators (MLE) and Jenkinson's (1969) 
  method of sextiles estimators (JSE).  They also compare the statistical 
  properties of quantile estimators (which are based on the distribution parameter 
  estimators).  Finally, they derive a test of the null hypothesis that the 
  shape parameter is zero, and assess its performance via computer simulation.

  Hosking et al. (1985) note that when \eqn{\kappa \le -1}, the moments and 
  probability-weighted moments of the GEVD do not exist.  They also note that in 
  practice the shape parameter usually lies between -1/2 and 1/2.

  Hosking et al. (1985) found that the asymptotic efficiency of the PWME 
  (the limit as the sample size approaches infinity of the ratio of the 
  variance of the MLE divided by the variance of the PWME) tends to 0 as the 
  shape parameter approaches 1/2 or -1/2.  For values of \eqn{\kappa} within the 
  range \eqn{[-0.2, 0.2]}, however, the efficiency of the estimator of location 
  is close to 100%, and the efficiency of the estimators of scale and shape 
  are greater than 70%.  With respect to estimating large quantiles, 
  Hosking et al. (1985) found that the asymptotic efficiency of the PWME is poor for 
  \eqn{\kappa} outside the range \eqn{[-0.2, 0.2]}.

  For the small sample results, Hosking et al. (1985) considered several possible 
  forms of the PWME (see equations (8)-(10) below).  The best overall results were 
  given by the plotting-position PWME defined by equations (9) and (10) with 
  \eqn{a=0.35} and \eqn{b=0}.

  Small sample results for estimating the parameters show that for \eqn{n \ge 50} 
  all three methods give almost identical results.  For \eqn{n < 50} the results 
  for the different estimators are a bit different, but not dramatically so.  The 
  MLE tends to be slightly less biased than the other two methods.  For estimating 
  the shape parameter, the MLE has a slightly larger standard deviation, and the 
  PWME has consistently the smallest standard deviation.

  Small sample results for estimating large quantiles show that for \eqn{n \ge 100} 
  all three methods are comparable.  For \eqn{n < 100} the PWME and JSE are 
  comparable and in general have much smaller standard deviations than the MLE.  
  All three methods are very inaccurate for estimating large quantiles in small 
  samples, especially when \eqn{\kappa < 0}.

  Hosking et al. (1985) derive a test of the null hypothesis \eqn{H_0: \kappa=0} 
  based on the PWME of \eqn{\kappa}. The test is performed by computing the 
  statistic:
  \deqn{z = \frac{\hat{\kappa_{pwme}}}{\sqrt{0.5663/n}} \;\;\;\; (1)} 
  and comparing \eqn{z} to a standard normal distribution (see 
  \code{\link{zTestGevdShape}}).  Based on computer simulations using the 
  plotting-position PWME, they found that a sample size of \eqn{n \ge 25} ensures 
  an adequate normal approximation.  They also found this test has power comparable 
  to the modified likelihood-ratio test, which was found by Hosking (1984) to be 
  the best overall test of \eqn{H_0: \kappa=0} of the thirteen tests he considered.
  \cr

  \bold{More Details}

  \emph{Probability-Weighted Moments and Parameters of the GEVD} \cr
  The definition of a \link[=pwMoment]{probability-weighted moment}, introduced by 
  Greenwood et al. (1979), is as follows.  Let \eqn{X} denote a random variable 
  with cdf \eqn{F}, and let \eqn{x(p)} denote the \eqn{p}'th quantile of the 
  distribution.  Then the \eqn{ijk}'th probability-weighted moment is given by:
  \deqn{M(i, j, k) = E[X^i F^j (1 - F)^k] = \int^1_0 [x(F)]^i F^j (1 - F)^k \, dF \;\;\;\; (2)}
  where \eqn{i}, \eqn{j}, and \eqn{k} are real numbers.

  Hosking et al. (1985) set
  \deqn{\beta_j = M(i, j, 0) \;\;\;\; (3)}
  and Greenwood et al. (1979) show that
  \deqn{\beta_j = \frac{1}{j+1} E[X_{j+1:j+1}] \;\;\;\; (4)}
  where 
  \deqn{E[X_{j+1:j+1}]}
  denotes the expected value of the \eqn{j+1}'th order statistic (i.e., the maximum) 
  in a sample of size \eqn{j+1}.  Hosking et al. (1985) show that if \eqn{X} has a 
  GEVD with parameters \code{location=}\eqn{\eta}, \code{scale=}\eqn{\theta}, and 
  \code{shape=}\eqn{\kappa}, where \eqn{\kappa \ne 0}, then
  \deqn{\beta_j = \frac{1}{j+1} \{\eta + \frac{\theta [1 - (j+1)^{-\kappa} \Gamma(1+\kappa)]}{\kappa} \} \;\;\;\; (5)}
  for \eqn{\kappa > -1}, where \eqn{\Gamma()} denotes the 
  \link[base:Special]{gamma function}.  Thus,
  \deqn{\beta_0 = \eta + \frac{\theta [1 - \Gamma(1+\kappa)]}{\kappa} \;\;\;\; (6)}
  \deqn{2\beta_1 - \beta_0 = \frac{\theta [\Gamma(1+\kappa)] (1 - 2^{-\kappa})}{\kappa} \;\;\;\; (7)}
  \deqn{\frac{3\beta_2 - \beta_0}{2\beta_1 - \beta_0} = \frac{1 - 3^{-\kappa}}{1 - 2^{-kappa}} \;\;\;\; (8)}
  
  \emph{Estimating Distribution Parameters} \cr
  Using the results of Landwehr et al. (1979), Hosking et al. (1985) show that 
  given a random sample of \eqn{n} values from some arbitrary distribution, an 
  unbiased, distribution-free, and parameter-free estimator of the 
  probability-weighted moment \eqn{\beta_j = M(i, j, 0)} defined above is given by:
  \deqn{b_j = \frac{1}{n} \sum^n_{i=j+1} x_{i,n} \frac{{i-1 \choose j}}{{n-1 \choose j}} \;\;\;\; (9)}
  where the quantity \eqn{x_{i,n}} denotes the \eqn{i}'th order statistic in the 
  random sample of size \eqn{n}.  Hosking et al. (1985) note that this estimator is 
  closely related to U-statistics (Hoeffding, 1948; Lehmann, 1975, pp. 362-371). 

  An alternative \dQuote{plotting position} estimator is given by:
  \deqn{\hat{\beta}_j[p_{i,n}] = \frac{1}{n} \sum^n_{i=1} p^j_{i,n} x_{i,n} \;\;\;\; (10)}
  where 
  \deqn{p_{i,n} = \hat{F}(x_{i,n}) \;\;\;\; (11)}
  denotes the plotting position of the \eqn{i}'th order statistic in the random 
  sample of size \eqn{n}, that is, a distribution-free estimate of the cdf of 
  \eqn{X} evaluated at the \eqn{i}'th order statistic.  Typically, plotting 
  positions have the form:
  \deqn{p_{i,n} = \frac{i-a}{n+b} \;\;\;\; (12)}
  where \eqn{b > -a > -1}.  For this form of plotting position, the 
  plotting-position estimators in (10) are asymptotically equivalent to the 
  U-statistic estimators in (9).

  Although the unbiased and plotting position estimators are asymptotically 
  equivalent (Hosking, 1990), Hosking and Wallis (1995) recommend using the 
  unbiased estimator for almost all applications because of its superior 
  performance in small and moderate samples.

  Using equations (6)-(8) above, i.e., the three equations involving 
  \eqn{\beta_0}, \eqn{\beta_1}, and \eqn{\beta_2}, Hosking et al. (1985) define 
  the probability-weighted moment estimators of 
  \eqn{\eta}, \eqn{\theta}, and \eqn{\kappa} as the solutions to these three 
  simultaneous equations, with the values of the probability-weighted moments 
  replaced by their estimated values (using either the unbiased or plotting posistion 
  estiamtors in (9) and (10) above).  Hosking et al. (1985) note that the third 
  equation (equation (8)) must be solved iteratively for the PWME of \eqn{\kappa}.  
  Using the unbiased estimators of the PWMEs to solve for \eqn{\kappa}, the PWMEs 
  of \eqn{\eta} and \eqn{\theta} are given by:
  \deqn{\hat{\eta}_{pwme} = b_0 + \frac{\hat{\theta}_{pwme} [\Gamma(1 + \hat{\kappa}_{pwme}) - 1]}{\hat{\kappa}_{pwme}} \;\;\;\; (13)}
  \deqn{\hat{\theta}_{pwme} = \frac{(2b_1 - b_0)\hat{\kappa}_{pwme}}{\Gamma(1 + \hat{\kappa}_{pwme}) (1 - 2^{-\hat{\kappa}_{pwme}})} \;\;\;\; (14)}
  Hosking et al. (1985) show that when the unbiased estimates of the PWMEs are used 
  to estimate the probability-weighted moments, the estimates of \eqn{\theta} and 
  \eqn{\kappa} satisfy the feasibility criteria
  \deqn{\hat{\theta}_{pwme} > 0; \, \hat{\kappa}_{pwme} > -1}
  almost surely.

  Hosking et al. (1985) show that the asymptotic distribution of the PWME is 
  multivariate normal with mean equal to \eqn{(\eta, \theta, \kappa)}, and they 
  derive the formula for the asymptotic variance-covariance matrix as:
  \deqn{V_{\hat{\eta}, \hat{\theta}, \hat{\kappa}} = \frac{1}{n} G V_{\hat{\beta}_0, \hat{\beta}_1, \hat{\beta}_2} G^T \;\;\;\; (15)}
  where
  \deqn{V_{\hat{\beta}_0, \hat{\beta}_1, \hat{\beta}_2}}
  denotes the variance-covariance matrix of the estimators of the probability-weighted 
  moments defined in either equation (9) or (10) above (recall that these two 
  estimators are asymptotically equivalent), and the matrix \eqn{G} is defined by:
  \deqn{G_{i1} = \frac{\partial \eta}{\partial \beta_{i-1}}, \, G_{i2} = \frac{\partial \theta}{\partial \beta_{i-1}}, \, G_{i3} = \frac{\partial \kappa}{\partial \beta_{i-1}} \;\;\;\; (16)}
  for \eqn{i = 1, 2, 3}.  Hosking et al. (1985) provide formulas for the matrix
  \deqn{V_{\hat{\beta}_0, \hat{\beta}_1, \hat{\beta}_2}}
  in Appendix C of their manuscript.  Note that there is a typographical error in 
  equation (C.11) (Jon Hosking, personal communication, 1996).  In the second line 
  of this equation, the quantity \eqn{-(r+s)^{-k}} should be replaced with 
  \eqn{-(r+s)^{-2k}}.

  The matrix \eqn{G} in equation (16) is not easily computed.  Its inverse, however, 
  is easy to compute and then can be inverted numerically (Jon Hosking, 1996, 
  personal communication). The inverse of \eqn{G} is given by:
  \deqn{G^{-1}_{i1} = \frac{\partial \beta_{i-1}{\partial \eta}}, \, G^{-1}_{i2} = \frac{\partial \beta_{i-1}{\partial \theta}}, \, G^{-1}_{i3} = \frac{\partial \beta_{i-1}{\partial \kappa}} \;\;\;\; (17)}  
  and by equation (5) above it can be shown that:
  \deqn{\frac{\partial \beta_j}{\partial \eta} = \frac{1}{j+1} \;\;\;\; (18)}
  \deqn{\frac{\partial \beta_j}{\partial \theta} =\frac{1 - (j+1)^{-\kappa}\Gamma(1+\kappa)}{(j+1)\kappa} \;\;\;\; (19)}
  \deqn{\frac{\partial \beta_j}{\partial \kappa} = \frac{\theta}{j+1} \{ \frac{(j+1)^{-\kappa}[log(j+1)\Gamma(1+\kappa)-\Gamma^{'}(1+\kappa)]}{\kappa} - \frac{1 - (j+1)^{-\kappa}\Gamma(1+\kappa)}{\kappa^2} \} \;\;\;\; (20)}
  for \eqn{i = 1, 2, 3}.

  \emph{Estimating Distribution Quantiles} \cr
  If \eqn{X} has a GEVD with parameters \code{location=}\eqn{\eta}, 
  \code{scale=}\eqn{\theta}, and \code{shape=}\eqn{\kappa}, where \eqn{\kappa \ne 0}, 
  then the \eqn{p}'th quantile of the distribution is given by:
  \deqn{x(p) = \eta + \frac{\theta \{1 - [-log(p)]^{\kappa} \}}{\kappa} \;\;\;\; (21)}
  \eqn{(0 \le p \le 1)}.  Given estimated values of the location, scale, and shape 
  parameters, the \eqn{p}'th quantile of the distribution is estimated as:
  \deqn{\hat{x}(p) = \hat{\eta} + \frac{\hat{\theta} \{1 - [-log(p)]^{\hat{\kappa}} \}}{\hat{\kappa}} \;\;\;\; (22)}
}
\references{
  Forbes, C., M. Evans, N. Hastings, and B. Peacock. (2011).  Statistical Distributions. 
  Fourth Edition. John Wiley and Sons, Hoboken, NJ.

  Greenwood, J.A., J.M. Landwehr, N.C. Matalas, and J.R. Wallis. (1979).  
  Probability Weighted Moments: Definition and Relation to Parameters of Several 
  Distributions Expressible in Inverse Form.  \emph{Water Resources Research} 
  \bold{15}(5), 1049--1054.

  Hoeffding, W. (1948).  A Class of Statistics with Asymptotically Normal 
  Distribution.  \emph{Annals of Mathematical Statistics} \bold{19}, 293--325.

  Hosking, J.R.M. (1985).  Algorithm AS 215: Maximum-Likelihood Estimation of the 
  Parameters of the Generalized Extreme-Value Distribution.  
  \emph{Applied Statistics} \bold{34}(3), 301--310.

  Hosking, J.R.M. (1990).  \eqn{L}-Moments:  Analysis and Estimation of 
  Distributions Using Linear Combinations of Order Statistics.  \emph{Journal of 
  the Royal Statistical Society, Series B} \bold{52}(1), 105--124.

  Hosking, J.R.M., and J.R. Wallis (1995).  A Comparison of Unbiased and 
  Plotting-Position Estimators of \eqn{L} Moments.  \emph{Water Resources 
  Research} \bold{31}(8), 2019--2025.

  Jenkinson, A.F. (1969).  Statistics of Extremes. \emph{Technical Note 98}, 
  World Meteorological Office, Geneva.

  Johnson, N. L., S. Kotz, and A.W. Kemp. (1992).  
  \emph{Univariate Discrete Distributions}.  Second Edition.  
  John Wiley and Sons, New York, pp.4-8.

  Johnson, N. L., S. Kotz, and N. Balakrishnan. (1995). 
  \emph{Continuous Univariate Distributions, Volume 2}. 
  Second Edition. John Wiley and Sons, New York.

  Lehmann, E.L. (1975).  \emph{Nonparametrics:  Statistical Methods Based on Ranks}.  
  Holden-Day, Oakland, CA, 457pp.
}
\author{
    Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\seealso{
  \link[=GEVD]{Generalized Extreme Value Distribution}, \code{\link{egevd}}.
}
\keyword{ distribution }
