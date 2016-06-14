\name{egevd}
\alias{egevd}
\title{
  Estimate Parameters of a Generalized Extreme Value Distribution
}
\description{
  Estimate the location, scale and shape parameters of a 
  \link[=GEVD]{generalized extreme value distribution}, and optionally construct a 
  confidence interval for one of the parameters.
}
\usage{
  egevd(x, method = "mle", pwme.method = "unbiased", tsoe.method = "med", 
    plot.pos.cons = c(a = 0.35, b = 0), ci = FALSE, ci.parameter = "location", 
    ci.type = "two-sided", ci.method = "normal.approx", information = "observed", 
    conf.level = 0.95)
}
\arguments{
  \item{x}{
  numeric vector of observations.
}
  \item{method}{
  character string specifying the method of estimation.  Possible values are 
  \code{"mle"} (maximum likelihood; the default), 
  \code{"pwme"} (probability-weighted moments), and 
  \code{"tsoe"} (two-stage order-statistics estimator of Castillo and Hadi (1994)).  
  See the DETAILS section for more information on these estimation methods.
}
  \item{pwme.method}{
  character string specifying what method to use to compute the 
  probability-weighted moments when \code{method="pwme"}.  The possible values are 
  \code{"ubiased"} (method based on the U-statistic; the default), or 
  \code{"plotting.position"} (method based on the plotting position formula).  
  See the DETAILS section in this help file and the help file for \code{\link{pwMoment}} 
  for more information.  This argument is ignored if \code{method} is not equal to 
  \code{"pwme"}.
}
  \item{tsoe.method}{
  character string specifying the robust function to apply in the second stage of 
  the two-stage order-statistics estimator when \code{method="tsoe"}.  Possible 
  values are \code{"med"} (median; the default), and \code{"lms"} 
  (least median of squares).  See the DETAILS section for more information on 
  these estimation methods.  This argument is ignored if \code{method} is not 
  equal to \code{"tsoe"}.
}
  \item{plot.pos.cons}{
  numeric vector of length 2 specifying the constants used in the formula for the 
  plotting positions when \code{method="pwme"} and \cr
  \code{pwme.method="plotting.position"}.  The default value is \cr
  \code{plot.pos.cons=c(a=0.35, b=0)}.  If this vector has a names attribute with 
  the value \code{c("a","b")} or \code{c("b","a")}, then the elements will be 
  matched by name in the formula for computing the plotting positions.  Otherwise, 
  the first element is mapped to the name \code{"a"} and the second element to the 
  name \code{"b"}.  See the DETAILS section in this help file and the help file 
  for \code{\link{pwMoment}} for more information.  This argument is used only if 
  \code{method="tsoe"}, or if both \code{method="pwme"} and 
  \code{pwme.method="plotting.position"}.
}
  \item{ci}{
  logical scalar indicating whether to compute a confidence interval for the 
  location, scale, or shape parameter.  The default value is \code{FALSE}.
}
  \item{ci.parameter}{
  character string indicating the parameter for which the confidence interval is 
  desired.  The possible values are \code{"location"} (the default), \code{"scale"}, 
  or \code{"shape"}.  This argument is ignored if \code{ci=FALSE}.
}
  \item{ci.type}{
  character string indicating what kind of confidence interval to compute.  The 
  possible values are \code{"two-sided"} (the default), \code{"lower"}, and 
  \code{"upper"}.  This argument is ignored if \code{ci=FALSE}.
}
  \item{ci.method}{
  character string indicating what method to use to construct the confidence interval 
  for the location or scale parameter.  Currently, the only possible value is 
  \code{"normal.approx"} (the default).  See the DETAILS section for more information.  
  This argument is ignored if \code{ci=FALSE}.
}
  \item{information}{
  character string indicating which kind of Fisher information to use when 
  computing the variance-covariance matrix of the maximum likelihood estimators.  
  The possible values are \code{"observed"} (the default) and \code{"expected"}.  
  See the DETAILS section for more information.  This argument is used only when 
  \code{method="mle"} and \code{ci=TRUE}.
}
  \item{conf.level}{
  a scalar between 0 and 1 indicating the confidence level of the confidence interval.  
  The default value is \code{conf.level=0.95}. This argument is ignored if 
  \code{ci=FALSE}.
}
}
\details{
  If \code{x} contains any missing (\code{NA}), undefined (\code{NaN}) or 
  infinite (\code{Inf}, \code{-Inf}) values, they will be removed prior to 
  performing the estimation.

  Let \eqn{\underline{x} = (x_1, x_2, \ldots, x_n)} be a vector of 
  \eqn{n} observations from a \link[=GEVD]{generalized extreme value distribution} with 
  parameters \code{location=}\eqn{\eta}, \code{scale=}\eqn{\theta}, and 
  \code{shape=}\eqn{\kappa}.

  \bold{Estimation}

  \emph{Maximum Likelihood Estimation} (\code{method="mle"}) \cr 
  The log likelihood function is given by:
  \deqn{L(\eta, \theta, \kappa) = -n \, log(\theta) - (1 - \kappa) \sum^n_{i=1} y_i - \sum^n_{i=1} e^{y_i}}
  where
  \deqn{y_i = -\frac{1}{\kappa} log[\frac{1 - \kappa(x_i - \eta)}{\theta}]}
  (see, for example, Jenkinson, 1969; Prescott and Walden, 1980; Prescott and Walden, 
  1983; Hosking, 1985; MacLeod, 1989).  The maximum likelihood estimators (MLE's) of 
  \eqn{\eta}, \eqn{\theta}, and \eqn{\kappa} are those values that maximize the 
  likelihood function, subject to the following constraints:
  \deqn{\theta > 0}
  \deqn{\kappa \le 1}
  \deqn{x_i < \eta + \frac{\theta}{\kappa} \; if \kappa > 0}
  \deqn{x_i > \eta + \frac{\theta}{\kappa} \; if \kappa < 0}
  Although in theory the value of \eqn{\kappa} may lie anywhere in the interval 
  \eqn{(-\infty, \infty)} (see \link{GEVD}), the constraint \eqn{\kappa \le 1} is 
  imposed because when \eqn{\kappa > 1} the likelihood can be made infinite and 
  thus the MLE does not exist (Castillo and Hadi, 1994).  Hence, \bold{this method of 
  estimation is not valid when the true value of \eqn{\kappa} is larger than 1}.  
  Hosking (1985) and Hosking et al. (1985) note that in practice the value of 
  \eqn{\kappa} tends to lie in the interval \eqn{-1/2 < \kappa < 1/2}.

  The value of \eqn{-L} is minimized using the \R function \code{\link{nlminb}}.  
  Prescott and Walden (1983) give formulas for the gradient and Hessian.  Only 
  the gradient is supplied in the call to \code{\link{nlminb}}.  The values of 
  the PWME (see below) are used as the starting values.  If the starting value of 
  \eqn{\kappa} is less than 0.001 in absolute value, it is reset to 
  \code{sign(k) * 0.001}, as suggested by Hosking (1985).

  \emph{Probability-Weighted Moments Estimation} (\code{method="pwme"})\cr
  The idea of probability-weighted moments was introduced by Greenwood et al. (1979).  
  Landwehr et al. (1979) derived probability-weighted moment estimators (PWME's) for 
  the parameters of the \link[=EVD]{Type I (Gumbel) extreme value distribution}.  
  Hosking et al. (1985) extended these results to the generalized extreme value 
  distribution.  See the \link[=HoskingEtAl1985]{abstract for Hosking et al. (1985)} 
  for details on how these estimators are computed.

  \emph{Two-Stage Order Statistics Estimation} (\code{method="tsoe"})\cr
  The two-stage order statistics estimator (TSOE) was introduced by 
  Castillo and Hadi (1994) as an alternative to the MLE and PWME.  Unlike the 
  MLE and PWME, the TSOE of \eqn{\kappa} exists for all combinations of sample 
  values and possible values of \eqn{\kappa}.  See the 
  \link[=CastilloAndHadi1994]{abstract for Castillo and Hadi (1994)} for details 
  on how these estimators are computed.  In the second stage, 
  Castillo and Hadi (1984) suggest using either the median or the least median of 
  squares as the robust function.  The function \code{egevd} allows three options 
  for the robust function:  median (\code{tsoe.method="med"}; see the \R help file for 
  \code{\link{median}}), least median of squares (\code{tsoe.method="lms"}; 
  see the help file for \code{\link[MASS]{lmsreg}} in the package \pkg{MASS}), 
  and least trimmed squares (\code{tsoe.method="lts"}; see the help file for 
  \code{\link[MASS]{ltsreg}} in the package \pkg{MASS}).


  \bold{Confidence Intervals} \cr
  When \code{ci=TRUE}, an approximate \eqn{(1-\alpha)}100\% confidence intervals 
  for \eqn{\eta} can be constructed assuming the distribution of the estimator of 
  \eqn{\eta} is approximately normally distributed.  A two-sided confidence 
  interval is constructed as:
  \deqn{[\hat{\eta} - t(n-1, 1-\alpha/2) \hat{\sigma}_{\hat{\eta}}, \, \hat{\eta} + t(n-1, 1-\alpha/2) \hat{\sigma}_{\hat{\eta}}]}
  where \eqn{t(\nu, p)} is the \eqn{p}'th quantile of Student's t-distribution with 
  \eqn{\nu} degrees of freedom, and the quantity 
  \deqn{\hat{\sigma}_{\hat{\eta}}}
  denotes the estimated asymptotic standard deviation of the estimator of \eqn{\eta}.

  Similarly, a two-sided confidence interval for \eqn{\theta} is constructed as:
  \deqn{[\hat{\theta} - t(n-1, 1-\alpha/2) \hat{\sigma}_{\hat{\theta}}, \, \hat{\theta} + t(n-1, 1-\alpha/2) \hat{\sigma}_{\hat{\theta}}]}
  and a two-sided confidence interval for \eqn{\kappa} is constructed as:
  \deqn{[\hat{\kappa} - t(n-1, 1-\alpha/2) \hat{\sigma}_{\hat{\kappa}}, \, \hat{\kappa} + t(n-1, 1-\alpha/2) \hat{\sigma}_{\hat{\kappa}}]}

  One-sided confidence intervals for \eqn{\eta}, \eqn{\theta}, and \eqn{\kappa} are 
  computed in a similar fashion.

  \emph{Maximum Likelihood Estimator} (\code{method="mle"}) \cr
  Prescott and Walden (1980) derive the elements of the Fisher information matrix 
  (the expected information).  The inverse of this matrix, evaluated at the values 
  of the MLE, is the estimated asymptotic variance-covariance matrix of the MLE.  
  This method is used to estimate the standard deviations of the estimated 
  distribution parameters when \code{information="expected"}.  The necessary 
  regularity conditions hold for \eqn{\kappa < 1/2}.  Thus, \bold{this method of 
  constructing confidence intervals is not valid when the true value of 
  \eqn{\kappa} is greater than or equal to 1/2}.

  Prescott and Walden (1983) derive expressions for the observed information matrix 
  (i.e., the Hessian).  This matrix is used to compute the estimated asymptotic 
  variance-covariance matrix of the MLE when \code{information="observed"}.

  In computer simulations, Prescott and Walden (1983) found that the 
  variance-covariance matrix based on the observed information gave slightly more 
  accurate estimates of the variance of MLE of \eqn{\kappa} compared to the 
  estimated variance based on the expected information.

  \emph{Probability-Weighted Moments Estimator} (\code{method="pwme"}) \cr
  Hosking et al. (1985) show that these estimators are asymptotically multivariate 
  normal and derive the asymptotic variance-covariance matrix.  See the 
  \link[=HoskingEtAl1985]{abstract for Hosking et al. (1985)} for details on how 
  this matrix is computed.

  \emph{Two-Stage Order Statistics Estimator} (\code{method="tsoe"}) \cr
  Currently there is no built-in method in \pkg{EnvStats} for computing confidence 
  intervals when \cr
  \code{method="tsoe"}.  \link[=CastilloAndHadi1994]{Castillo and Hadi (1994)} suggest 
  using the bootstrap or jackknife method.
}
\value{
  a list of class \code{"estimate"} containing the estimated parameters and other information.  
  See \cr
  \code{\link{estimate.object}} for details.
}
\references{
  Castillo, E., and A. Hadi. (1994).  Parameter and Quantile Estimation for the 
  Generalized Extreme-Value Distribution.  \emph{Environmetrics} \bold{5}, 417--432.

  Forbes, C., M. Evans, N. Hastings, and B. Peacock. (2011).  Statistical Distributions. 
  Fourth Edition. John Wiley and Sons, Hoboken, NJ.

  Greenwood, J.A., J.M. Landwehr, N.C. Matalas, and J.R. Wallis. (1979).  
  Probability Weighted Moments: Definition and Relation to Parameters of Several 
  Distributions Expressible in Inverse Form.  \emph{Water Resources Research} 
  \bold{15}(5), 1049--1054.

  Hosking, J.R.M. (1984).  Testing Whether the Shape Parameter is Zero in the 
  Generalized Extreme-Value Distribution.  \emph{Biometrika} \bold{71}(2), 367--374.

  Hosking, J.R.M. (1985).  Algorithm AS 215: Maximum-Likelihood Estimation of the 
  Parameters of the Generalized Extreme-Value Distribution.  
  \emph{Applied Statistics} \bold{34}(3), 301--310.

  Hosking, J.R.M., J.R. Wallis, and E.F. Wood. (1985).  Estimation of the 
  Generalized Extreme-Value Distribution by the Method of 
  Probability-Weighted Moments.  \emph{Technometrics} \bold{27}(3), 251--261.

  Jenkinson, A.F. (1969).  Statistics of Extremes. \emph{Technical Note 98}, 
  World Meteorological Office, Geneva.

  Johnson, N. L., S. Kotz, and N. Balakrishnan. (1995). 
  \emph{Continuous Univariate Distributions, Volume 2}. 
  Second Edition. John Wiley and Sons, New York.

  Landwehr, J.M., N.C. Matalas, and J.R. Wallis. (1979).  Probability Weighted 
  Moments Compared With Some Traditional Techniques in Estimating Gumbel 
  Parameters and Quantiles.  \emph{Water Resources Research} \bold{15}(5), 
  1055--1064.

  Macleod, A.J. (1989).  Remark AS R76: A Remark on Algorithm AS 215: 
  Maximum Likelihood Estimation of the Parameters of the Generalized 
  Extreme-Value Distribution.  \emph{Applied Statistics} \bold{38}(1), 198--199.

  Prescott, P., and A.T. Walden. (1980).  Maximum Likelihood Estimation of the 
  Parameters of the Generalized Extreme-Value Distribution.  
  \emph{Biometrika} \bold{67}(3), 723--724.

  Prescott, P., and A.T. Walden. (1983).  Maximum Likelihood Estimation of the 
  Three-Parameter Generalized Extreme-Value Distribution from Censored Samples.  
  \emph{Journal of Statistical Computing and Simulation} \bold{16}, 241--250.
}
\author{
    Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  Two-parameter \link[=EVD]{extreme value distributions} (EVD) have been 
  applied extensively since the 1930's to several fields of study, including 
  the distributions of hydrological and meteorological variables, human lifetimes, 
  and strength of materials.  The three-parameter 
  \link[=GEVD]{generalized extreme value distribution} (GEVD) was introduced by 
  Jenkinson (1955) to model annual maximum and minimum values of meteorological 
  events.  Since then, it has been used extensively in the hydological and 
  meteorological fields.

  The three families of EVDs are all special kinds of GEVDs.  When the shape 
  parameter \eqn{\kappa=0}, the GEVD reduces to the Type I extreme value (Gumbel) 
  distribution.  (The function \code{\link{zTestGevdShape}} allows you to test 
  the null hypothesis \eqn{H_0: \kappa=0}.)  When \eqn{\kappa > 0}, the GEVD is 
  the same as the Type II extreme value distribution, and when \eqn{\kappa < 0} 
  it is the same as the Type III extreme value distribution.

  Hosking et al. (1985) compare the asymptotic and small-sample statistical 
  properties of the PWME with the MLE and Jenkinson's (1969) method of sextiles.  
  Castillo and Hadi (1994) compare the small-sample statistical properties of the 
  MLE, PWME, and TSOE.  Hosking and Wallis (1995) compare the small-sample properties 
  of unbaised \eqn{L}-moment estimators vs. plotting-position \eqn{L}-moment 
  estimators.  (PWMEs can be written as linear combinations of \eqn{L}-moments and 
  thus have equivalent statistical properties.)  Hosking and Wallis (1995) conclude 
  that unbiased estimators should be used for almost all applications.
}
\seealso{
  \link[=GEVD]{Generalized Extreme Value Distribution}, 
  \code{\link{zTestGevdShape}}, \link[=EVD]{Extreme Value Distribution}, 
  \code{\link{eevd}}.
}
\examples{
  # Generate 20 observations from a generalized extreme value distribution 
  # with parameters location=2, scale=1, and shape=0.2, then compute the 
  # MLE and construct a 90% confidence interval for the location parameter. 
  # (Note: the call to set.seed simply allows you to reproduce this example.)

  set.seed(498) 
  dat <- rgevd(20, location = 2, scale = 1, shape = 0.2) 
  egevd(dat, ci = TRUE, conf.level = 0.9)

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            Generalized Extreme Value
  #
  #Estimated Parameter(s):          location = 1.6144631
  #                                 scale    = 0.9867007
  #                                 shape    = 0.2632493
  #
  #Estimation Method:               mle
  #
  #Data:                            dat
  #
  #Sample Size:                     20
  #
  #Confidence Interval for:         location
  #
  #Confidence Interval Method:      Normal Approximation
  #                                 (t Distribution) based on
  #                                 observed information
  #
  #Confidence Interval Type:        two-sided
  #
  #Confidence Level:                90%
  #
  #Confidence Interval:             LCL = 1.225249
  #                                 UCL = 2.003677

  #----------

  # Compare the values of the different types of estimators:

  egevd(dat, method = "mle")$parameters 
  # location     scale     shape 
  #1.6144631 0.9867007 0.2632493 

  egevd(dat, method = "pwme")$parameters
  # location     scale     shape 
  #1.5785779 1.0187880 0.2257948 

  egevd(dat, method = "pwme", pwme.method = "plotting.position")$parameters 
  # location     scale     shape 
  #1.5509183 0.9804992 0.1657040

  egevd(dat, method = "tsoe")$parameters 
  # location     scale     shape 
  #1.5372694 1.0876041 0.2927272 

  egevd(dat, method = "tsoe", tsoe.method = "lms")$parameters 
  #location    scale    shape 
  #1.519469 1.081149 0.284863

  egevd(dat, method = "tsoe", tsoe.method = "lts")$parameters 
  # location     scale     shape 
  #1.4840198 1.0679549 0.2691914 

  #----------

  # Clean up
  #---------
  rm(dat)
}
\keyword{ distribution }
\keyword{ htest }
