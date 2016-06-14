\name{eevd}
\alias{eevd}
\title{
  Estimate Parameters of an Extreme Value (Gumbel) Distribution
}
\description{
  Estimate the location and scale parameters of an 
  \link[=EVD]{extreme value distribution}, and optionally construct a 
  confidence interval for one of the parameters.
}
\usage{
  eevd(x, method = "mle", pwme.method = "unbiased", 
    plot.pos.cons = c(a = 0.35, b = 0), ci = FALSE, 
    ci.parameter = "location", ci.type = "two-sided", 
    ci.method = "normal.approx", conf.level = 0.95)
}
\arguments{
  \item{x}{
  numeric vector of observations.
}
  \item{method}{
  character string specifying the method of estimation.  Possible values are 
  \code{"mle"} (maximum likelihood; the default), \code{"mme"} (methods of moments), 
  \code{"mmue"} (method of moments based on the unbiased estimator of variance), and 
  \code{"pwme"} (probability-weighted moments).  See the DETAILS section for more 
  information on these estimation methods. 
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
  \item{plot.pos.cons}{
  numeric vector of length 2 specifying the constants used in the formula for the 
  plotting positions when \code{method="pwme"} and \cr
  \code{pwme.method="plotting.position"}.  The default value is \cr
  \code{plot.pos.cons=c(a=0.35, b=0)}.  If this vector has a names attribute with 
  the value \code{c("a","b")} or \code{c("b","a")}, then the elements will be 
  matched by name in the formula for computing the plotting positions.  Otherwise, 
  the first element is mapped to the name \code{"a"} and the second element to the 
  name \code{"b"}.  See the DETAILS section in this help file and the help file 
  for \code{\link{pwMoment}} for more information.  This argument is ignored if 
  \code{method} is not equal to \code{"pwme"} or if \code{pwme.method="ubiased"}.
}
  \item{ci}{
  logical scalar indicating whether to compute a confidence interval for the 
  location or scale parameter.  The default value is \code{FALSE}.
}
  \item{ci.parameter}{
  character string indicating the parameter for which the confidence interval is 
  desired.  The possible values are \code{"location"} (the default) and \code{"scale"}.  
  This argument is ignored if \code{ci=FALSE}.
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
  \eqn{n} observations from an \link[=EVD]{extreme value distribution} with 
  parameters \code{location=}\eqn{\eta} and \code{scale=}\eqn{\theta}.

  \bold{Estimation} \cr

  \emph{Maximum Likelihood Estimation} (\code{method="mle"}) \cr
  The maximum likelihood estimators (mle's) of \eqn{\eta} and \eqn{\theta} are 
  the solutions of the simultaneous equations (Forbes et al., 2011):
  \deqn{\hat{\eta}_mle = \hat{\theta}_mle \, log[\frac{1}{n} \sum_{i=1}^{n} exp(\frac{-x_i}{\hat{\theta}_mle})]}
  \deqn{\hat{\theta}_mle = \bar{x} - \frac{\sum_{i=1}^{n} x_i exp(\frac{-x_i}{\hat{\theta}_mle})}{\sum_{i=1}^{n} exp(\frac{-x_i}{\hat{\theta}_mle})}}
  where 
  \deqn{\bar{x} = \frac{1}{n} \sum_{i=1}^n x_i}.

  \emph{Method of Moments Estimation} (\code{method="mme"}) \cr
  The method of moments estimators (mme's) of \eqn{\eta} and \eqn{\theta} are 
  given by (Johnson et al., 1995, p.27):
  \deqn{\hat{\eta}_{mme} = \bar{x} - \epsilon \hat{\theta}_{mme}}
  \deqn{\hat{\theta}_{mme} = \frac{\sqrt{6}}{\pi} s_m}
  where \eqn{\epsilon} denotes \link[=EulersConstant]{Euler's constant} and 
  \eqn{s_m} denotes the square root of the method of moments estimator of variance:
  \deqn{s_m^2 = \frac{1}{n} \sum_{i=1}^n (x_i - \bar{x})^2}
  
  \emph{Method of Moments Estimators Based on the Unbiased Estimator of Variance} (\code{method="mmue"}) \cr
  These estimators are the same as the method of moments estimators except that 
  the method of moments estimator of variance is replaced with the unbiased estimator 
  of variance:
  \deqn{s^2 = \frac{1}{n-1} \sum_{i=1}^n (x_i - \bar{x})^2}

  \emph{Probability-Weighted Moments Estimation} (\code{method="pwme"})\cr
  Greenwood et al. (1979) show that the relationship between the distribution 
  parameters \eqn{\eta} and \eqn{\theta} and the probability-weighted moments 
  is given by:
  \deqn{\eta = M(1, 0, 0) - \epsilon \theta}
  \deqn{\theta = \frac{M(1, 0, 0) - 2M(1, 0, 1)}{log(2)}}
  where \eqn{M(i, j, k)} denotes the \eqn{ijk}'th probability-weighted moment and 
  \eqn{\epsilon} denotes \link[=EulersConstant]{Euler's constant}.  
  The probability-weighted moment estimators (pwme's) of \eqn{\eta} and 
  \eqn{\theta} are computed by simply replacing the \eqn{M(i,j,k)}'s in the 
  above two equations with estimates of the \eqn{M(i,j,k)}'s (and for the 
  estimate of \eqn{\eta}, replacing \eqn{\theta} with its estimated value).  
  See the help file for \code{\link{pwMoment}} for more information on how to 
  estimate the \eqn{M(i,j,k)}'s.  Also, see Landwehr et al. (1979) for an example 
  of this method of estimation using the unbiased (U-statistic type) 
  probability-weighted moment estimators.  Hosking et al. (1985) note that this 
  method of estimation using the U-statistic type probability-weighted moments 
  is equivalent to Downton's (1966) linear estimates with linear coefficients. \cr

  \bold{Confidence Intervals} \cr
  When \code{ci=TRUE}, an approximate \eqn{(1-\alpha)}100\% confidence intervals 
  for \eqn{\eta} can be constructed assuming the distribution of the estimator of 
  \eqn{\eta} is approximately normally distributed.  A two-sided confidence 
  interval is constructed as:
  \deqn{[\hat{\eta} - t(n-1, 1-\alpha/2) \hat{\sigma}_{\hat{\eta}}, \, \hat{\eta} + t(n-1, 1-\alpha/2) \hat{\sigma}_{\hat{\eta}}]}
  where \eqn{t(\nu, p)} is the \eqn{p}'th quantile of 
  \link[stats:TDist]{Student's t-distribution} with 
  \eqn{\nu} degrees of freedom, and the quantity 
  \deqn{\hat{\sigma}_{\hat{\eta}}}
  denotes the estimated asymptotic standard deviation of the estimator of \eqn{\eta}.

  Similarly, a two-sided confidence interval for \eqn{\theta} is constructed as:
  \deqn{[\hat{\theta} - t(n-1, 1-\alpha/2) \hat{\sigma}_{\hat{\theta}}, \, \hat{\theta} + t(n-1, 1-\alpha/2) \hat{\sigma}_{\hat{\theta}}]}

  One-sided confidence intervals for \eqn{\eta} and \eqn{\theta} are computed in 
  a similar fashion.

  \emph{Maximum Likelihood} (\code{method="mle"}) \cr
  Downton (1966) shows that the estimated asymptotic variances of the mle's of 
  \eqn{\eta} and \eqn{\theta} are given by:
  \deqn{\hat{\sigma}_{\hat{\eta}_mle}^2 = \frac{\hat{\theta}_mle^2}{n} [1 + \frac{6(1 - \epsilon)^2}{\pi^2}] = \frac{1.10867 \hat{\theta}_mle^2}{n}}
  \deqn{\hat{\sigma}_{\hat{\theta}_mle}^2 = \frac{6}{\pi^2} \frac{\hat{\theta}_mle^2}{n} = \frac{0.60793 \hat{\theta}_mle^2}{n}}
  where \eqn{\epsilon} denotes \link[=EulersConstant]{Euler's constant}.

  \emph{Method of Moments} (\code{method="mme"} or \code{method="mmue"}) \cr
  Tiago de Oliveira (1963) and Johnson et al. (1995, p.27) show that the 
  estimated asymptotic variance of the mme's of \eqn{\eta} and \eqn{\theta} 
  are given by:
  \deqn{\hat{\sigma}_{\hat{\eta}_mme}^2 = \frac{\hat{\theta}_mme^2}{n} [\frac{\pi^2}{6} + \frac{\epsilon^2}{4}(\beta_2 - 1) - \frac{\pi \epsilon \sqrt{\beta_1}}{\sqrt{6}}] = \frac{1.1678 \hat{\theta}_mme^2}{n}}
  \deqn{\hat{\sigma}_{\hat{\theta}_mme}^2 = \frac{\hat{\theta}_mle^2}{n} \frac{(\beta_2 - 1)}{4} = \frac{1.1 \hat{\theta}_mme^2}{n}}
  where the quantities 
  \deqn{\sqrt{\beta_1}, \; \beta_2}
  denote the skew and kurtosis of the distribution, and \eqn{\epsilon} 
  denotes \link[=EulersConstant]{Euler's constant}. 

  The estimated asymptotic variances of the mmue's of \eqn{\eta} and \eqn{\theta} 
  are the same, except replace the mme of \eqn{\theta} in the above equations with 
  the mmue of \eqn{\theta}.

  \emph{Probability-Weighted Moments} (\code{method="pwme"}) \cr
  As stated above, Hosking et al. (1985) note that this method of estimation using 
  the U-statistic type probability-weighted moments is equivalent to 
  Downton's (1966) linear estimates with linear coefficients.  Downton (1966) 
  provides exact values of the variances of the estimates of location and scale 
  parameters for the smallest extreme value distribution.  For the largest extreme 
  value distribution, the formula for the estimate of scale is the same, but the 
  formula for the estimate of location must be modified.  Thus, Downton's (1966) 
  equation (3.4) is modified to:
  \deqn{\hat{\eta}_pwme = \frac{(n-1)log(2) + (n+1)\epsilon}{n(n-1)log(2)} v - \frac{2 \epsilon}{n(n-1)log(2)} w}
  where \eqn{\epsilon} denotes \link[=EulersConstant]{Euler's constant}, and 
  \eqn{v} and \eqn{w} are defined in Downton (1966, p.8).  Using 
  Downton's (1966) equations (3.9)-(3.12), the exact variance of the pwme of 
  \eqn{\eta} can be derived.  Note that when \code{method="pwme"} and 
  \code{pwme.method="plotting.position"}, these are only the asymptotically correct 
  variances.
}
\value{
  a list of class \code{"estimate"} containing the estimated parameters and other information.  
  See \cr
  \code{\link{estimate.object}} for details.
}
\references{
  Castillo, E. (1988).  \emph{Extreme Value Theory in Engineering}.  
  Academic Press, New York, pp.184--198.

  Downton, F. (1966).  Linear Estimates of Parameters in the Extreme Value 
  Distribution.  \emph{Technometrics} \bold{8}(1), 3--17.

  Forbes, C., M. Evans, N. Hastings, and B. Peacock. (2011).  Statistical Distributions. 
  Fourth Edition. John Wiley and Sons, Hoboken, NJ.

  Greenwood, J.A., J.M. Landwehr, N.C. Matalas, and J.R. Wallis. (1979).  
  Probability Weighted Moments: Definition and Relation to Parameters of Several 
  Distributions Expressible in Inverse Form.  \emph{Water Resources Research} 
  \bold{15}(5), 1049--1054.

  Hosking, J.R.M., J.R. Wallis, and E.F. Wood. (1985).  Estimation of the 
  Generalized Extreme-Value Distribution by the Method of 
  Probability-Weighted Moments.  \emph{Technometrics} \bold{27}(3), 251--261.

  Johnson, N. L., S. Kotz, and N. Balakrishnan. (1995). 
  \emph{Continuous Univariate Distributions, Volume 2}. 
  Second Edition. John Wiley and Sons, New York.

  Landwehr, J.M., N.C. Matalas, and J.R. Wallis. (1979).  Probability Weighted 
  Moments Compared With Some Traditional Techniques in Estimating Gumbel 
  Parameters and Quantiles.  \emph{Water Resources Research} \bold{15}(5), 
  1055--1064.

  Tiago de Oliveira, J. (1963).  Decision Results for the Parameters of the 
  Extreme Value (Gumbel) Distribution Based on the Mean and Standard Deviation.  
  \emph{Trabajos de Estadistica} \bold{14}, 61--81.
}
\author{
    Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  There are three families of extreme value distributions.  The one 
  described here is the \link[=EVD]{Type I, also called the Gumbel extreme value 
  distribution or simply Gumbel distribution}.  The name 
  \dQuote{extreme value} comes from the fact that this distribution is 
  the limiting distribution (as \eqn{n} approaches infinity) of the 
  greatest value among \eqn{n} independent random variables each 
  having the same continuous distribution.

  The Gumbel extreme value distribution is related to the 
  \link[stats:Exponential]{exponential distribution} as follows. 
  Let \eqn{Y} be an \link[stats:Exponential]{exponential random variable} 
  with parameter \code{rate=}\eqn{\lambda}.  Then \eqn{X = \eta - log(Y)} 
  has an extreme value distribution with parameters 
  \code{location=}\eqn{\eta} and \code{scale=}\eqn{1/\lambda}.

  The distribution described above and assumed by \code{eevd} is the 
  \emph{largest} extreme value distribution.  The smallest extreme value 
  distribution is the limiting distribution (as \eqn{n} approaches infinity) 
  of the smallest value among 
  \eqn{n} independent random variables each having the same continuous distribution. 
  If \eqn{X} has a largest extreme value distribution with parameters 
  \code{location=}\eqn{\eta} and \code{scale=}\eqn{\theta}, then 
  \eqn{Y = -X} has a smallest extreme value distribution with parameters 
  \code{location=}\eqn{-\eta} and \code{scale=}\eqn{\theta}.  The smallest 
  extreme value distribution is related to the \link[stats:Weibull]{Weibull distribution} 
  as follows.  Let \eqn{Y} be a \link[stats:Weibull]{Weibull random variable} with 
  parameters 
  \code{shape=}\eqn{\beta} and \code{scale=}\eqn{\alpha}.  Then \eqn{X = log(Y)} 
  has a smallest extreme value distribution with parameters \code{location=}\eqn{log(\alpha)} 
  and \code{scale=}\eqn{1/\beta}.

  The extreme value distribution has been used extensively to model the distribution 
  of streamflow, flooding, rainfall, temperature, wind speed, and other 
  meteorological variables, as well as material strength and life data.
}
\seealso{
  \link[=EVD]{Extreme Value Distribution}, \link[=EulersConstant]{Euler's Constant}.
}
\examples{
  # Generate 20 observations from an extreme value distribution with 
  # parameters location=2 and scale=1, then estimate the parameters 
  # and construct a 90% confidence interval for the location parameter. 
  # (Note: the call to set.seed simply allows you to reproduce this example.)

  set.seed(250) 
  dat <- revd(20, location = 2) 
  eevd(dat, ci = TRUE, conf.level = 0.9) 

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            Extreme Value
  #
  #Estimated Parameter(s):          location = 1.9684093
  #                                 scale    = 0.7481955
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
  #                                 (t Distribution)
  #
  #Confidence Interval Type:        two-sided
  #
  #Confidence Level:                90%
  #
  #Confidence Interval:             LCL = 1.663809
  #                                 UCL = 2.273009

  #----------

  #Compare the values of the different types of estimators: 

  eevd(dat, method = "mle")$parameters 
  # location     scale 
  #1.9684093 0.7481955

  eevd(dat, method = "mme")$parameters 
  # location     scale 
  #1.9575980 0.8339256 

  eevd(dat, method = "mmue")$parameters 
  # location     scale 
  #1.9450932 0.8555896 

  eevd(dat, method = "pwme")$parameters 
  # location     scale 
  #1.9434922 0.8583633

  #----------

  # Clean up
  #---------
  rm(dat)
}
\keyword{ distribution }
\keyword{ htest }
