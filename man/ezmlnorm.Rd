\name{ezmlnorm}
\alias{ezmlnorm}
\alias{ezmlnormAlt}
\title{
  Estimate Parameters of a Zero-Modified Lognormal (Delta) Distribution
}
\description{
  Estimate the parameters of a 
  \link[=ZeroModifiedLognormal]{zero-modified lognormal distribution} or a 
  \link[=ZeroModifiedLognormalAlt]{zero-modified lognormal distribution (alternative parameterization)}, 
  and optionally construct a confidence interval for the mean.
}
\usage{
  ezmlnorm(x, method = "mvue", ci = FALSE, ci.type = "two-sided", 
    ci.method = "normal.approx", conf.level = 0.95)

  ezmlnormAlt(x, method = "mvue", ci = FALSE, ci.type = "two-sided", 
    ci.method = "normal.approx", conf.level = 0.95)
}
\arguments{
  \item{x}{
  numeric vector of observations.  Missing (\code{NA}), undefined (\code{NaN}), 
  and infinite (\code{Inf}, \code{-Inf}) values are allowed but will be removed.
}
  \item{method}{
  character string specifying the method of estimation.  The only possible value is 
  \code{"mvue"} (minimum variance unbiased; the default).  See the DETAILS section for 
  more information on this estimation method. 
}
  \item{ci}{
  logical scalar indicating whether to compute a confidence interval for the 
  mean.  The default value is \code{FALSE}.  If \code{ci=TRUE} and there are less 
  than three non-missing observations in \code{x}, or if all observations are 
  zeros, a warning will be issued and no confidence interval will be computed.
}
  \item{ci.type}{
  character string indicating what kind of confidence interval to compute.  The 
  possible values are \code{"two-sided"} (the default), \code{"lower"}, and 
  \code{"upper"}.  This argument is ignored if \code{ci=FALSE}.
}
  \item{ci.method}{
  character string indicating what method to use to construct the confidence 
  interval for the mean.  The only possible value is \code{"normal.approx"} 
  (the default).  See the DETAILS section for more information. 
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
  \eqn{n} observations from a 
  \link[=ZeroModifiedLognormal]{zero-modified lognormal distribution} with 
  parameters \code{meanlog=}\eqn{\mu}, \code{sdlog=}\eqn{\sigma}, and 
  \code{p.zero=}\eqn{p}.  Alternatively, let 
  \eqn{\underline{x} = (x_1, x_2, \ldots, x_n)} be a vector of 
  \eqn{n} observations from a 
  \link[=ZeroModifiedLognormalAlt]{zero-modified lognormal distribution 
  (alternative parameterization)} with parameters \code{mean=}\eqn{\theta}, 
  \code{cv=}\eqn{\tau}, and \code{p.zero=}\eqn{p}.

  Let \eqn{r} denote the number of observations in \eqn{\underline{x}} that are equal 
  to 0, and order the observations so that \eqn{x_1, x_2, \ldots, x_r} denote 
  the \eqn{r} zero observations and \eqn{x_{r+1}, x_{r+2}, \ldots, x_n} denote 
  the \eqn{n-r} non-zero observations.

  Note that \eqn{\theta} is \emph{not} the mean of the zero-modified lognormal 
  distribution; it is the mean of the lognormal part of the distribution.  Similarly, 
  \eqn{\tau} is \emph{not} the coefficient of variation of the zero-modified 
  lognormal distribution; it is the coefficient of variation of the lognormal 
  part of the distribution.

  Let \eqn{\gamma}, \eqn{\delta}, and \eqn{\phi} denote the mean, standard deviation, 
  and coefficient of variation of the overall zero-modified lognormal (delta) 
  distribution.  Let \eqn{\eta} denote the standard deviation of the lognormal 
  part of the distribution, so that \eqn{\eta = \theta \tau}.  Aitchison (1955) 
  shows that:
  \deqn{\gamma = (1 - p) \theta  \;\;\;\; (1)}
  \deqn{\delta^2 = (1 - p) \eta^2 + p (1 - p) \theta^2 \;\;\;\; (2)}
  so that
  \deqn{\phi = \frac{\delta}{\gamma} = \frac{\sqrt{\tau^2 + p}}{\sqrt{1-p}} \;\;\;\; (3)}
  \cr

  \bold{Estimation} \cr

  \emph{Minimum Variance Unbiased Estimation} (\code{method="mvue"}) \cr
  Aitchison (1955) shows that the minimum variance unbiased estimators (mvue's) of 
  \eqn{\gamma} and \eqn{\delta} are:
  \tabular{lll}{
    \eqn{\hat{\gamma}_{mvue} =} \tab \eqn{(1-\frac{r}{n}) e^{\bar{y}} g_{n-r-1}(\frac{s^2}{2})} \tab if \eqn{r < n - 1}, \cr
                                \tab \eqn{x_n / n} \tab if \eqn{r = n - 1}, \cr
                                \tab \eqn{0} \tab if \eqn{r = n \;\;\;\; (4)} \cr
                                \tab         \tab \cr
    \eqn{\hat{\delta}^2_{mvue} =} \tab \eqn{(1-\frac{r}{n}) e^{2\bar{y}} \{g_{n-r-1}(2s^2) - \frac{n-r-1}{n-1} g_{n-r-1}[\frac{(n-r-2)s^2}{n-r-1}] \} } \tab if \eqn{r < n - 1}, \cr
                                  \tab \eqn{x_n^2 / n} \tab if \eqn{r = n - 1}, \cr
                                  \tab \eqn{0} \tab if \eqn{r = n \;\;\;\; (5)}
  }
  where 
  \deqn{y_i = log(x_i), \; r = r+1, r+2, \ldots, n \;\;\;\; (6)}
  \deqn{\bar{y} = \frac{1}{n-r} \sum_{i=r+1}^n y_i \;\;\;\; (7)}
  \deqn{s^2 = \frac{1}{n-r-1} \sum_{i=r+1}^n (y_i - \bar{y})^2 \;\;\;\; (8)}
  \deqn{g_m(z) = \sum_{i=0}^\infty \frac{m^i (m+2i)}{m(m+2) \cdots (m+2i)} (\frac{m}{m+1})^i (\frac{z^i}{i!}) \;\;\;\; (9)}

  Note that when \eqn{r=n-1} or \eqn{r=n}, the estimator of \eqn{\gamma} is simply the 
  sample mean for all observations (including zero values), and the estimator for 
  \eqn{\delta^2} is simply the sample variance for all observations.

  The expected value and asymptotic variance of the mvue of \eqn{\gamma} are 
  (Aitchison and Brown, 1957, p.99; Owen and DeRouen, 1980):
  \deqn{E(\hat{\gamma}_{mvue}) = \gamma \;\;\;\; (10)}
  \deqn{AVar(\hat{\gamma}_{mvue}) = \frac{1}{n} exp(2\mu + \sigma^2) (1-p) (p + \frac{2\sigma^2 + \sigma^4}{2}) \;\;\;\; (11)}
  \cr

  \bold{Confidence Intervals} \cr

  \emph{Based on Normal Approximation} (\code{ci.method="normal.approx"}) \cr
  An approximate \eqn{(1-\alpha)100\%} confidence interval for \eqn{\gamma} is 
  constructed based on the assumption that the estimator of \eqn{\gamma} is 
  approximately normally distributed.  Thus, an approximate two-sided 
  \eqn{(1-\alpha)100\%} confidence interval for \eqn{\gamma} is constructed as:
  \deqn{[ \hat{\gamma}_{mvue} - t_{n-2, 1-\alpha/2} \hat{\sigma}_{\hat{\gamma}}, \; \hat{\gamma}_{mvue} + t_{n-2, 1-\alpha/2} \hat{\sigma}_{\hat{\gamma}} ] \;\;\;\; (12)}
  where \eqn{t_{\nu, p}} is the \eqn{p}'th quantile of 
  \link[stats:TDist]{Student's t-distribution} with \eqn{\nu} degrees of freedom, and 
  the quantity \eqn{\hat{\sigma}_{\hat{\gamma}}} is the estimated standard deviation 
  of the mvue of \eqn{\gamma}, and is computed by replacing the values of 
  \eqn{\mu}, \eqn{\sigma}, and \eqn{p} in equation (11) above with their estimated 
  values and taking the square root.

  Note that there must be at least 3 non-missing observations (\eqn{n \ge 3}) and 
  at least one observation must be non-zero (\eqn{r \le n-1}) in order to construct 
  a confidence interval.

  One-sided confidence intervals are computed in a similar fashion.
}
\value{
  a list of class \code{"estimate"} containing the estimated parameters and other information.  
  See \cr
  \code{\link{estimate.object}} for details.

  For the function \code{ezmlnorm}, the component called \code{parameters} is a 
  numeric vector with the following estimated parameters: 
  \tabular{ll}{
    \bold{Parameter Name} \tab \bold{Explanation} \cr
    \code{meanlog} \tab mean of the log of the lognormal part of the distribution. \cr
    \code{sdlog}   \tab standard deviation of the log of the lognormal part of the distribution. \cr
    \code{p.zero}  \tab probability that an observation will be 0. \cr
    \code{mean.zmlnorm} \tab mean of the overall zero-modified lognormal (delta) distribution. \cr
    \code{sd.zmlnorm} \tab standard deviation of the overall zero-modified lognormal (delta) distribution. \cr
  }

  For the function \code{ezmlnormAlt}, the component called \code{parameters} is a 
  numeric vector with the following estimated parameters: 
  \tabular{ll}{
    \bold{Parameter Name} \tab \bold{Explanation} \cr
    \code{mean} \tab mean of the lognormal part of the distribution. \cr
    \code{cv}   \tab coefficient of variation of the lognormal part of the distribution. \cr
    \code{p.zero}  \tab probability that an observation will be 0. \cr
    \code{mean.zmlnorm} \tab mean of the overall zero-modified lognormal (delta) distribution. \cr
    \code{sd.zmlnorm} \tab standard deviation of the overall zero-modified lognormal (delta) distribution. \cr
  }
}
\references{
  Aitchison, J. (1955).  On the Distribution of a Positive Random Variable Having 
  a Discrete Probability Mass at the Origin.  \emph{Journal of the American 
  Statistical Association} \bold{50}, 901--908.

  Aitchison, J., and J.A.C. Brown (1957).  \emph{The Lognormal Distribution 
  (with special reference to its uses in economics)}.  Cambridge University Press, 
  London. pp.94-99.

  Crow, E.L., and K. Shimizu. (1988).  \emph{Lognormal Distributions: 
  Theory and Applications}.  Marcel Dekker, New York, pp.47--51.

  Gibbons, RD., D.K. Bhaumik, and S. Aryal. (2009).  \emph{Statistical Methods 
  for Groundwater Monitoring}.  Second Edition.  John Wiley and Sons, Hoboken, NJ.

  Gilliom, R.J., and D.R. Helsel. (1986).  Estimation of Distributional Parameters 
  for Censored Trace Level Water Quality Data: 1. Estimation Techniques.  
  \emph{Water Resources Research} \bold{22}, 135--146.

  Helsel, D.R. (2012).  \emph{Statistics for Censored Environmental Data Using 
  Minitab and R}.  Second Edition.  John Wiley and Sons, Hoboken, NJ, Chapter 1.

  Johnson, N. L., S. Kotz, and A.W. Kemp. (1992).  \emph{Univariate Discrete Distributions}. 
  Second Edition. John Wiley and Sons, New York, p.312.

  Owen, W., and T. DeRouen. (1980).  Estimation of the Mean for Lognormal Data 
  Containing Zeros and Left-Censored Values, with Applications to the Measurement 
  of Worker Exposure to Air Contaminants.  \emph{Biometrics} \bold{36}, 707--719.

  USEPA (1992c).  \emph{Statistical Analysis of Ground-Water Monitoring Data at 
  RCRA Facilities: Addendum to Interim Final Guidance}.  Office of Solid Waste, 
  Permits and State Programs Division, US Environmental Protection Agency, 
  Washington, D.C.

  USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
  EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.  
  U.S. Environmental Protection Agency, Washington, D.C.
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  The zero-modified lognormal (delta) distribution is sometimes used to 
  model chemical concentrations for which some observations are reported as 
  \dQuote{Below Detection Limit} (the nondetects are assumed equal to 0).  
  See, for example, Gilliom and Helsel (1986), Owen and DeRouen (1980), and 
  Gibbons et al. (2009, Chapter 12).  USEPA (2009, Chapter 15) recommends this 
  strategy only in specific situations, and Helsel (2012, Chapter 1) strongly 
  discourages this approach to dealing with non-detects.

  A variation of the zero-modified lognormal (delta) distribution is the 
  \link[=ZeroModifiedNormal]{zero-modified normal distribution}, in which a 
  normal distribution is mixed with a positive probability mass at 0. 

  One way to try to assess whether a zero-modified lognormal (delta), 
  zero-modified normal, censored normal, or censored lognormal is the best 
  model for the data is to construct both censored and detects-only probability 
  plots (see \code{\link{qqPlotCensored}}).
}
\seealso{
  \link[=ZeroModifiedLognormal]{Zero-Modified Lognormal}, 
  \link[=ZeroModifiedNormal]{Zero-Modified Normal}, \link[stats:Lognormal]{Lognormal}.
}
\examples{
  # Generate 100 observations from a zero-modified lognormal (delta) 
  # distribution with mean=2, cv=1, and p.zero=0.5, then estimate the 
  # parameters. According to equations (1) and (3) above, the overall mean 
  # is mean.zmlnorm=1 and the overall cv is cv.zmlnorm=sqrt(3). 
  # (Note: the call to set.seed simply allows you to reproduce this example.)

  set.seed(250) 
  dat <- rzmlnormAlt(100, mean = 2, cv = 1, p.zero = 0.5) 
  ezmlnormAlt(dat, ci = TRUE) 

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            Zero-Modified Lognormal (Delta)
  #
  #Estimated Parameter(s):          mean         = 1.9604561
  #                                 cv           = 0.9169411
  #                                 p.zero       = 0.4500000
  #                                 mean.zmlnorm = 1.0782508
  #                                 cv.zmlnorm   = 1.5307175
  #
  #Estimation Method:               mvue
  #
  #Data:                            dat
  #
  #Sample Size:                     100
  #
  #Confidence Interval for:         mean.zmlnorm
  #
  #Confidence Interval Method:      Normal Approximation
  #                                 (t Distribution)
  #
  #Confidence Interval Type:        two-sided
  #
  #Confidence Level:                95%
  #
  #Confidence Interval:             LCL = 0.748134
  #                                 UCL = 1.408368

  #----------

  # Clean up
  rm(dat)
}
\keyword{ distribution }
\keyword{ htest }
