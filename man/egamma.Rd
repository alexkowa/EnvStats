\name{egamma}
\alias{egamma}
\alias{egammaAlt}
\title{
  Estimate Parameters of Gamma Distribution
}
\description{
  Estimate the shape and scale parameters (or the mean and coefficient of 
  variation) of a \link[=GammaDist]{Gamma} distribution.
}
\usage{
  egamma(x, method = "mle", ci = FALSE, 
    ci.type = "two-sided", ci.method = "normal.approx", 
    normal.approx.transform = "kulkarni.powar", conf.level = 0.95)

  egammaAlt(x, method = "mle", ci = FALSE, 
    ci.type = "two-sided", ci.method = "normal.approx", 
    normal.approx.transform = "kulkarni.powar", conf.level = 0.95)
}
\arguments{
  \item{x}{
  numeric vector of non-negative observations. 
  Missing (\code{NA}), undefined (\code{NaN}), and 
  infinite (\code{Inf}, \code{-Inf}) values are allowed but will be removed.
}
  \item{method}{
  character string specifying the method of estimation.  The possible values are: \cr 
  \code{"mle"} (maximum likelihood; the default), \cr
  \code{"bcmle"} (bias-corrected mle), \cr
  \code{"mme"} (method of moments), and \cr
  \code{"mmue"} (method of moments based on the unbiased estimator of variance). \cr 
  See the DETAILS section for more information.
}
  \item{ci}{
  logical scalar indicating whether to compute a confidence interval for the mean.  
  The default value is \code{ci=FALSE}.
}
  \item{ci.type}{
  character string indicating what kind of confidence interval to compute.  
  The possible values are 
  \code{"two-sided"} (the default), \code{"lower"}, and \code{"upper"}.  
  This argument is ignored if \code{ci=FALSE}.
}
  \item{ci.method}{
  character string indicating which method to use to construct the confidence interval.  
  Possible values are \code{"normal.approx"} (the default), 
  \code{"profile.likelihood"}, \code{"chisq.approx"}, and \code{"chisq.adj"}.  
  This argument is ignored if \code{ci=FALSE}.
}
  \item{normal.approx.transform}{
  character string indicating which power transformation to use when \cr
  \code{ci.method="normal.approx"}.  Possible values are \cr
  \code{"kulkarni.powar"} (the default), \code{"cube.root"}, and 
  \code{"fourth.root"}.  See the DETAILS section for more informaiton.  
  This argument is ignored if \code{ci=FALSE} or \code{ci.method="chisq.approx"}.
}
  \item{conf.level}{
  a scalar between 0 and 1 indicating the confidence level of the confidence interval.  The default 
  value is \code{conf.level=0.95}.  This argument is ignored if \code{ci=FALSE}.
}
}
\details{
  If \code{x} contains any missing (\code{NA}), undefined (\code{NaN}) or 
  infinite (\code{Inf}, \code{-Inf}) values, they will be removed prior to 
  performing the estimation.

  Let \eqn{\underline{x} = x_1, x_2, \ldots, x_n} denote a random sample of 
  \eqn{n} observations from a \link[stats:GammaDist]{gamma distribution} 
  with parameters \code{shape=}\eqn{\kappa} and \code{scale=}\eqn{\theta}.  
  The relationship between these parameters and the mean (\code{mean=}\eqn{\mu}) 
  and coefficient of variation (\code{cv=}\eqn{\tau}) of this distribution is given by:
  \deqn{\kappa = \tau^{-2} \;\;\;\;\;\; (1)}
  \deqn{\theta = \mu/\kappa \;\;\;\;\;\; (2)}
  \deqn{\mu = \kappa \; \theta \;\;\;\;\;\; (3)}
  \deqn{\tau = \kappa^{-1/2} \;\;\;\;\;\; (4)}

  The function \code{egamma} returns estimates of the shape and scale parameters.  
  The function \code{egammaAlt} returns estimates of the mean (\eqn{\mu}) and 
  coefficient of variation (\eqn{cv}) based on the estimates of the shape and 
  scale parameters.

  \bold{Estimation} \cr

  \emph{Maximum Likelihood Estimation} (\code{method="mle"}) \cr
  The maximum likelihood estimators (mle's) of the shape and scale parameters 
  \eqn{\kappa} and \eqn{\theta} are solutions of the simultaneous equations:
  \deqn{\hat{\kappa}_{mle} =  \frac{1}{n}\sum_{i=1}^n log(x_i) - log(\bar{x}) = \psi(\hat{\kappa}_{mle}) - log(\hat{\kappa}_{mle}) \ \;\;\;\;\;\; (5)}
  \deqn{\hat{\theta}_{mle} = \bar{x} / \hat{\kappa}_{mle} \;\;\;\;\;\; (6)}
  where \eqn{\psi} denotes the \code{\link[base:Special]{digamma function}}, 
  and \eqn{\bar{x}} denotes the sample mean:
  \deqn{\bar{x} = \frac{1}{n}\sum_{i=1}^n x_i \;\;\;\;\;\; (7)} 
  (Forbes et al., 2011, chapter 22; Johnson et al., 1994, chapter 17).  
  \cr

  \emph{Bias-Corrected Maximum Likelihood Estimation} (\code{method="bcmle"}) \cr
  The \dQuote{bias-corrected} maximum likelihood estimator of 
  the shape parameter is based on the suggestion of Anderson and Ray (1975; 
  see also Johnon et al., 1994, p.366 and Singh et al., 2010b, p.48), who noted that 
  the bias of the maximum likelihood estimator of the shape parameter can be 
  considerable when the sample size is small.  This estimator is given by:
  \deqn{\hat{\kappa}_{bcmle} = \frac{n-3}{n}\hat{\kappa}_{mle} + \frac{2}{3n} \;\;\;\;\;\; (8)}
  When \code{method="bcmle"}, Equation (6) above is modified so that the estimate of the 
  scale paramter is based on the \dQuote{bias-corrected} maximum likelihood estimator 
  of the shape parameter:
  \deqn{\hat{\theta}_{bcmle} = \bar{x} / \hat{\kappa}_{bcmle} \;\;\;\;\;\; (9)}
  \cr

  \emph{Method of Moments Estimation} (\code{method="mme"}) \cr
  The method of moments estimators (mme's) of the shape and scale parameters 
  \eqn{\kappa} and \eqn{\theta} are:
  \deqn{\hat{\kappa}_{mme} = (\bar{x}/s_m)^2 \;\;\;\;\;\; (10)}
  \deqn{\hat{\theta}_{mme} = s_m^2 / \bar{x} \;\;\;\;\;\; (11)}
  where \eqn{s_m^2} denotes the method of moments estimator of variance:
  \deqn{s_m^2 = \frac{1}{n} \sum_{i=1}^n (x_i - \bar{x})^2 \;\;\;\;\;\; (12)}
  \cr
 
  \emph{Method of Moments Estimation Based on the Unbiased Estimator of Variance} (\code{method="mmue"}) \cr  
  The method of moments estimators based on the unbiased estimator of variance 
  are exactly the same as the method of moments estimators, 
  except that the method of moments estimator of variance is replaced with the 
  unbiased estimator of variance:
  \deqn{\hat{\kappa}_{mmue} = (\bar{x}/s)^2 \;\;\;\;\;\; (13)}
  \deqn{\hat{\theta}_{mmue} = s^2 / \bar{x} \;\;\;\;\;\; (14)}
  where \eqn{s^2} denotes the unbiased estimator of variance:
  \deqn{s^2 = \frac{1}{n-1} \sum_{i=1}^n (x_i - \bar{x})^2 \;\;\;\;\;\; (15)}
  \cr

  \bold{Confidence Intervals} \cr
  This section discusses how confidence intervals for the mean \eqn{\mu} are computed.

  \emph{Normal Approximation} (\code{ci.method="normal.approx"}) \cr
  The normal approximation method is based on the method of Kulkarni and Powar (2010), 
  who use a power transformation of the the original data to approximate a sample 
  from a normal distribuiton, compute the confidence interval for the mean on the 
  transformed scale using the usual formula for a confidence interval for the 
  mean of a normal distribuiton, and then tranform the limits back to the original 
  space using equations based on the expected value of a gamma random variable 
  raised to a power.  

  The particular power used for the normal approximation is defined by the argument \cr
  \code{normal.approx.transform}.  The value 
  \code{normal.approx.transform="cube.root"} uses the cube root transformation 
  suggested by Wilson and Hilferty (1931), and the value \cr
  \code{"fourth.root"} uses the fourth root transformation suggested 
  by Hawkins and Wixley (1986).  The default value \code{"kulkarni.powar"} 
  uses the \dQuote{Optimum Power Normal Approximation Method} of Kulkarni and Powar 
  (2010), who show this method performs the best in terms of maintining coverage 
  and minimizing confidence interval width compared to eight other methods.  
  The \dQuote{optimum} power \eqn{p} is determined by:
  \tabular{lll}{
  \eqn{p = -0.0705 - 0.178\hat{\kappa} + 0.475 \sqrt{\hat{\kappa}}} \tab if \eqn{\hat{\kappa} \le 1.5} \tab \cr
  \eqn{p = 0.246} \tab if \eqn{\hat{\kappa} > 1.5} \tab (16)
  }
  where \eqn{\hat{\kappa}} denotes the estimate of the shape parameter.  
  Kulkarni and Powar (2010) 
  derived this equation by determining what power transformation yields a skew closest to 0 and 
  a kurtosis closest to 3 for a gamma random variable with a given shape parameter.  
  Although Kulkarni and Powar (2010) use the maximum likelihood estimate of shape to 
  determine the power to use to induce approximate normality, for the functions 
  \code{egamma} and \code{egammaAlt} the power is based on whatever estimate of 
  shape is used (e.g., \code{method="mle"}, \code{method="bcmle"}, etc.). 
  \cr

  \emph{Likelihood Profile} (\code{ci.method="profile.likelihood"}) \cr
  This method was proposed by Cox (1970, p.88), and Venzon and Moolgavkar (1988) 
  introduced an efficient method of computation.  This method is also discussed by  
  Stryhn and Christensen (2003) and Royston (2007).  
  The idea behind this method is to invert the likelihood-ratio test to obtain a 
  confidence interval for the mean \eqn{\mu} while treating the coefficient of 
  variation \eqn{\tau} as a nuisance parameter.

  The likelihood function is given by:
  \deqn{L(\mu, \tau | \underline{x}) = \prod_{i=1}^n \frac{x_i^{\kappa-1} e^{-x_i/\theta}}{\theta^\kappa \Gamma(\kappa)} \;\;\;\;\;\; (17)}
  where \eqn{\kappa}, \eqn{\theta}, \eqn{\mu}, and \eqn{\tau} are defined in 
  Equations (1)-(4) above, and 
  \eqn{\Gamma(t)} denotes the \link[base:Special]{Gamma function} evaluated at \eqn{t}. 
  
  Following Stryhn and Christensen (2003), denote the maximum likelihood estimates 
  of the mean and coefficient of variation by \eqn{(\mu^*, \tau^*)}.  
  The likelihood ratio test statistic (\eqn{G^2}) of the hypothesis 
  \eqn{H_0: \mu = \mu_0} (where \eqn{\mu_0} is a fixed value) equals the 
  drop in \eqn{2 log(L)} between the \dQuote{full} model and the reduced model with 
  \eqn{\mu} fixed at \eqn{\mu_0}, i.e.,
  \deqn{G^2 = 2 \{log[L(\mu^*, \tau^*)] - log[L(\mu_0, \tau_0^*)]\} \;\;\;\;\;\; (18)}
  where \eqn{\tau_0^*} is the maximum likelihood estimate of \eqn{\tau} for the 
  reduced model (i.e., when \eqn{\mu = \mu_0}).  Under the null hypothesis, 
  the test statistic \eqn{G^2} follows a 
  \link[stats:Chisquare]{chi-squared distribution} with 1 degree of freedom.

  Alternatively, we may 
  express the test statistic in terms of the profile likelihood function \eqn{L_1} 
  for the mean \eqn{\mu}, which is obtained from the usual likelihood function by 
  maximizing over the parameter \eqn{\tau}, i.e.,
  \deqn{L_1(\mu) = max_{\tau} L(\mu, \tau) \;\;\;\;\;\; (19)}
  Then we have 
  \deqn{G^2 = 2 \{log[L_1(\mu^*)] - log[L_1(\mu_0)]\} \;\;\;\;\;\; (20)}
  A two-sided \eqn{(1-\alpha)100\%} confidence interval for the mean \eqn{\mu} 
  consists of all values of \eqn{\mu_0} for which the test is not significant at 
  level \eqn{alpha}:
  \deqn{\mu_0: G^2 \le \chi^2_{1, {1-\alpha}} \;\;\;\;\;\; (21)}
  where \eqn{\chi^2_{\nu, p}} denotes the \eqn{p}'th quantile of the 
  \link[stats:Chisquare]{chi-squared distribution} with \eqn{\nu} degrees of freedom.  
  One-sided lower and one-sided upper confidence intervals are computed in a similar 
  fashion, except that the quantity \eqn{1-\alpha} in Equation (21) is replaced with 
  \eqn{1-2\alpha}.
  \cr

  \emph{Chi-Square Approximation} (\code{ci.method="chisq.approx"}) \cr
  This method is based on the relationship between the sample mean of the gamma 
  distribution and the chi-squared distribution (Grice and Bain, 1980):
  \deqn{\frac{2n\bar{x}}{\theta} \sim \chi^2_{2n\kappa} \;\;\;\;\;\; (22)}
  Therefore, an exact one-sided upper \eqn{(1-\alpha)100\%} confidence interval 
  for the mean \eqn{\mu} is given by:
  \deqn{[0, \, \frac{2n\bar{x}\kappa}{\chi^2_{2n\kappa, \alpha}}] \;\;\;\;\;\; (23)}
  an exact one-sided lower \eqn{(1-\alpha)100\%} confidence interval 
  is given by:
  \deqn{[\frac{2n\bar{x}\kappa}{\chi^2_{2n\kappa, 1-\alpha}}, \, \infty] \;\;\;\;\;\; (24)}
  and a two-sided \eqn{(1-\alpha)100\%} confidence interval is given by:
  \deqn{[\frac{2n\bar{x}\kappa}{\chi^2_{2n\kappa, 1-\alpha/2}}, 
    \, \frac{2n\bar{x}\kappa}{\chi^2_{2n\kappa, \alpha/2}}] \;\;\;\;\;\; (25)}

  Because this method is exact only when the shape parameter \eqn{\kappa} is known, the 
  method used here is called the \dQuote{chi-square approximation} method 
  because the estimate of the shape parameter, \eqn{\hat{\kappa}}, is used in place 
  of \eqn{\kappa} in Equations (23)-(25) above.  The Chi-Square Approximation method 
  is \bold{not} the method proposed by Grice and Bain (1980) in which the 
  confidence interval is adjusted based on adjusting for the fact that the shape 
  parameter \eqn{\kappa} is estimated (see the explanation of the 
  Chi-Square Adjusted method below).  The Chi-Square Approximation method used 
  by \code{egamma} and \code{egammaAlt} is equivalent to the \dQuote{approximate gamma} 
  method of \emph{ProUCL} (USEPA, 2015, equation (2-34), p.62).
  \cr

  \emph{Chi-Square Adjusted} (\code{ci.method="chisq.adj"}) \cr
  This is the same method as the Chi-Square Approximation method discussed above, 
  execpt that the value of \eqn{\alpha} is adjusted to account for the fact that 
  the shape parameter \eqn{\kappa} is estimated rather than known.  Grice and Bain (1980) 
  performed Monte Carlo simulations to determine how to adjust \eqn{\alpha} and the 
  values in their Table 2 are given in the matrix \code{\link{Grice.Bain.80.mat}}.  
  This method requires that the sample size \eqn{n} is at least 5 and the confidence level 
  is between 75\% and 99.5\% (except when \eqn{n=5}, in which case the confidence level 
  must be less than 99\%).  For values of the sample size \eqn{n} and/or \eqn{\alpha} 
  that are not listed in the table, linear interpolation is used (when the sample 
  size \eqn{n} is greater than 40, linear interpolation on \eqn{1/n} is used, as 
  recommended by Grice and Bain (1980)).  The Chi-Square Adjusted method used 
  by \code{egamma} and \code{egammaAlt} is equivalent to the \dQuote{adjusted gamma} 
  method of \emph{ProUCL} (USEPA, 2015, equation (2-35), p.63).
}
\value{
  a list of class \code{"estimate"} containing the estimated parameters and other information.  
  See \cr
  \code{\link{estimate.object}} for details.
}
\references{
  Anderson, C.W., and W.D. Ray. (1975). Improved Maximum Likelihood Estimators 
  for the Gamma Distribution. \emph{Communications in Statistics}, \bold{4}, 437--448.

  Forbes, C., M. Evans, N. Hastings, and B. Peacock. (2011).  
  \emph{Statistical Distributions, Fourth Edition}. 
  John Wiley and Sons, Hoboken, NJ.

  Grice, J.V., and L.J. Bain. (1980). Inferences Concerning the Mean of the Gamma Distribution. 
  \emph{Journal of the American Statistician}, \bold{75}, 929-933.

  Hawkins, D. M., and R.A.J. Wixley. (1986). A Note on the Transformation of 
  Chi-Squared Variables to Normality. \emph{The American Statistician},
  \bold{40}, 296--298.

  Johnson, N.L., S. Kotz, and N. Balakrishnan. (1994). 
  \emph{Continuous Univariate Distributions, Volume 1}. Second Edition. 
  John Wiley and Sons, New York, Chapter 17.

  Kulkarni, H.V., and S.K. Powar. (2010). A New Method for Interval Estimation of the Mean 
  of the Gamma Distribution. \emph{Lifetime Data Analysis}, \bold{16}, 431--447.

  Singh, A., A.K. Singh, and R.J. Iaci. (2002). 
  \emph{Estimation of the Exposure Point Concentration Term Using a Gamma Distribution}.  
  EPA/600/R-02/084. October 2002. Technology Support Center for Monitoring and 
  Site Characterization, Office of Research and Development, Office of Solid Waste and 
  Emergency Response, U.S. Environmental Protection Agency, Washington, D.C.

  USEPA. (2015).  \emph{ProUCL Version 5.1.002 Technical Guide}.  EPA/600/R-07/041, October 2015.  
  Office of Research and Development. U.S. Environmental Protection Agency, Washington, D.C.

  Wilson, E.B., and M.M. Hilferty. (1931). The Distribution of Chi-Squares. 
  \emph{Proceedings of the National Academy of Sciences}, \bold{17}, 684--688.
}
\author{
    Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  The gamma distribution takes values on the positive real line. 
  Special cases of the gamma are the \link[=Exponential]{exponential} distribution and 
  the \link[=Chisquare]{chi-square} distributions. Applications of the gamma include 
  life testing, statistical ecology, queuing theory, inventory control, and precipitation 
  processes. A gamma distribution starts to resemble a normal distribution as the 
  shape parameter a tends to infinity.

  Some EPA guidance documents (e.g., Singh et al., 2002; Singh et al., 2010a,b) strongly recommend 
  against using a lognormal model for environmental data and recommend trying a gamma distribuiton 
  instead.
}
\section{Warning}{
  When \code{ci=TRUE} and \code{ci.method="normal.approx"}, it is possible for the 
  lower confidence limit based on the transformed data to be less than 0.  
  In this case, the lower confidence limit on the original scale is set to 0 and a warning is 
  issued stating that the normal approximation is not accurate in this case.
}
\seealso{
  \code{\link{GammaDist}}, \code{\link{estimate.object}}, \code{\link{eqgamma}}, 
  \code{\link{predIntGamma}}, \code{\link{tolIntGamma}}.
}
\examples{
  # Generate 20 observations from a gamma distribution with parameters 
  # shape=3 and scale=2, then estimate the parameters. 
  # (Note: the call to set.seed simply allows you to reproduce this 
  # example.)

  set.seed(250) 
  dat <- rgamma(20, shape = 3, scale = 2) 
  egamma(dat, ci = TRUE)

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            Gamma
  #
  #Estimated Parameter(s):          shape = 2.203862
  #                                 scale = 2.174928
  #
  #Estimation Method:               mle
  #
  #Data:                            dat
  #
  #Sample Size:                     20
  #
  #Confidence Interval for:         mean
  #
  #Confidence Interval Method:      Optimum Power Normal Approximation
  #                                 of Kulkarni & Powar (2010)
  #                                 using mle of 'shape'
  #
  #Normal Transform Power:          0.246
  #
  #Confidence Interval Type:        two-sided
  #
  #Confidence Level:                95%
  #
  #Confidence Interval:             LCL = 3.361652
  #                                 UCL = 6.746794

  # Clean up
  rm(dat)

  #====================================================================

  # Using the reference area TcCB data in EPA.94b.tccb.df, assume a 
  # gamma distribution, estimate the parameters based on the 
  # bias-corrected mle of shape, and compute a one-sided upper 90% 
  # confidence interval for the mean.

  #----------
  # First test to see whether the data appear to follow a gamma 
  # distribution.

  with(EPA.94b.tccb.df, 
    gofTest(TcCB[Area == "Reference"], dist = "gamma", 
      est.arg.list = list(method = "bcmle"))
  )

  #Results of Goodness-of-Fit Test
  #-------------------------------
  #
  #Test Method:                     Shapiro-Wilk GOF Based on 
  #                                 Chen & Balakrisnan (1995)
  #
  #Hypothesized Distribution:       Gamma
  #
  #Estimated Parameter(s):          shape = 4.5695247
  #                                 scale = 0.1309788
  #
  #Estimation Method:               bcmle
  #
  #Data:                            TcCB[Area == "Reference"]
  #
  #Sample Size:                     47
  #
  #Test Statistic:                  W = 0.9703827
  #
  #Test Statistic Parameter:        n = 47
  #
  #P-value:                         0.2739512
  #
  #Alternative Hypothesis:          True cdf does not equal the
  #                                 Gamma Distribution.

  #----------
  # Now estimate the paramters and compute the upper confidence limit.

  with(EPA.94b.tccb.df, 
    egamma(TcCB[Area == "Reference"], method = "bcmle", ci = TRUE, 
      ci.type = "upper", conf.level = 0.9) 
  )

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            Gamma
  #
  #Estimated Parameter(s):          shape = 4.5695247
  #                                 scale = 0.1309788
  #
  #Estimation Method:               bcmle
  #
  #Data:                            TcCB[Area == "Reference"]
  #
  #Sample Size:                     47
  #
  #Confidence Interval for:         mean
  #
  #Confidence Interval Method:      Optimum Power Normal Approximation
  #                                 of Kulkarni & Powar (2010)
  #                                 using bcmle of 'shape'
  #
  #Normal Transform Power:          0.246
  #
  #Confidence Interval Type:        upper
  #
  #Confidence Level:                90%
  #
  #Confidence Interval:             LCL = 0.0000000
  #                                 UCL = 0.6561838

  #------------------------------------------------------------------

  # Repeat the above example but use the alternative parameterization.

  with(EPA.94b.tccb.df, 
    egammaAlt(TcCB[Area == "Reference"], method = "bcmle", ci = TRUE, 
      ci.type = "upper", conf.level = 0.9) 
  )

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            Gamma
  #
  #Estimated Parameter(s):          mean = 0.5985106
  #                                 cv   = 0.4678046
  #
  #Estimation Method:               bcmle of 'shape'
  #
  #Data:                            TcCB[Area == "Reference"]
  #
  #Sample Size:                     47
  #
  #Confidence Interval for:         mean
  #
  #Confidence Interval Method:      Optimum Power Normal Approximation
  #                                 of Kulkarni & Powar (2010)
  #                                 using bcmle of 'shape'
  #
  #Normal Transform Power:          0.246
  #
  #Confidence Interval Type:        upper
  #
  #Confidence Level:                90%
  #
  #Confidence Interval:             LCL = 0.0000000
  #                                 UCL = 0.6561838

  #------------------------------------------------------------------

  # Compare the upper confidence limit based on 
  # 1) the default method: 
  #    normal approximation method based on Kulkarni and Powar (2010)
  # 2) Profile Likelihood 
  # 3) Chi-Square Approximation
  # 4) Chi-Square Adjusted

  # Default Method 
  #---------------
  with(EPA.94b.tccb.df, 
    egamma(TcCB[Area == "Reference"], method = "bcmle", ci = TRUE, 
      ci.type = "upper", conf.level = 0.9)$interval$limits["UCL"]
  )

  #      UCL 
  #0.6561838

  # Profile Likelihood 
  #-------------------
  with(EPA.94b.tccb.df, 
    egamma(TcCB[Area == "Reference"], method = "mle", ci = TRUE, 
      ci.type = "upper", conf.level = 0.9,
      ci.method = "profile.likelihood")$interval$limits["UCL"]
  )

  #      UCL 
  #0.6527009


  # Chi-Square Approximation
  #-------------------------
  with(EPA.94b.tccb.df, 
    egamma(TcCB[Area == "Reference"], method = "mle", ci = TRUE, 
      ci.type = "upper", conf.level = 0.9, 
      ci.method = "chisq.approx")$interval$limits["UCL"]
  )

  #      UCL 
  #0.6532188


  # Chi-Square Adjusted
  #--------------------
  with(EPA.94b.tccb.df, 
    egamma(TcCB[Area == "Reference"], method = "mle", ci = TRUE, 
      ci.type = "upper", conf.level = 0.9, 
      ci.method = "chisq.adj")$interval$limits["UCL"]
  )

  #    UCL 
  #0.65467

}
\keyword{ distribution }
\keyword{ htest }
