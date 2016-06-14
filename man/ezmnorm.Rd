\name{ezmnorm}
\alias{ezmnorm}
\title{
  Estimate Parameters of a Zero-Modified Normal Distribution
}
\description{
  Estimate the mean and standard deviation of a 
  \link[=ZeroModifiedNormal]{zero-modified normal distribution}, and 
  optionally construct a confidence interval for the mean.
}
\usage{
  ezmnorm(x, method = "mvue", ci = FALSE, ci.type = "two-sided", 
    ci.method = "normal.approx", conf.level = 0.95)
}
\arguments{
  \item{x}{
  numeric vector of observations.
}
  \item{method}{
  character string specifying the method of estimation.  Currently, the only possible 
  value is \code{"mvue"} (minimum variance unbiased; the default).  See the DETAILS 
  section for more information.
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
  for the mean.  Currently the only possible value is \code{"normal.approx"} 
  (the default).  See the DETAILS section for more information.
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
  \link[=ZeroModifiedNormal]{zero-modified normal distribution} with 
  parameters \code{mean=}\eqn{\mu}, \code{sd=}\eqn{\sigma}, and \code{p.zero=}\eqn{p}.  
  Let \eqn{r} denote the number of observations in \eqn{\underline{x}} that are equal 
  to 0, and order the observations so that \eqn{x_1, x_2, \ldots, x_r} denote 
  the \eqn{r} zero observations, and \eqn{x_{r+1}, x_{r+2}, \ldots, x_n} denote the 
  \eqn{n-r} non-zero observations.

  Note that \eqn{\mu} is \emph{not} the mean of the zero-modified normal distribution; 
  it is the mean of the normal part of the distribution.  Similarly, \eqn{\sigma} is 
  \emph{not} the standard deviation of the zero-modified normal distribution; it is 
  the standard deviation of the normal part of the distribution.

  Let \eqn{\gamma} and \eqn{\delta} denote the mean and standard deviation of the 
  overall zero-modified normal distribution.  Aitchison (1955) shows that:
  \deqn{\gamma = (1 - p) \mu  \;\;\;\; (1)}
  \deqn{\delta^2 = (1 - p) \sigma^2 + p (1 - p) \mu^2 \;\;\;\; (2)}
  \cr

  \bold{Estimation} \cr

  \emph{Minimum Variance Unbiased Estimation} (\code{method="mvue"}) \cr
  Aitchison (1955) shows that the minimum variance unbiased estimators (mvue's) of 
  \eqn{\gamma} and \eqn{\delta} are:
  \deqn{\hat{\gamma}_{mvue} = \bar{x} \;\;\;\; (3)}
  \tabular{lll}{
    \eqn{\hat{\delta}^2_{mvue} =} \tab \eqn{\frac{n-r-1}{n-1} (s^*)^2 + \frac{r}{n} (\frac{n-r}{n-1}) (\bar{x}^*)^2} \tab if \eqn{r < n - 1}, \cr
                                  \tab \eqn{x_n^2 / n} \tab if \eqn{r = n - 1}, \cr
                                  \tab \eqn{0} \tab if \eqn{r = n \;\;\;\; (4)}
  }
  where 
  \deqn{\bar{x} = \frac{1}{n} \sum_{i=1}^n x_i \;\;\;\; (5)}
  \deqn{\bar{x}^* = \frac{1}{n-r} \sum_{i=r+1}^n x_i \;\;\;\; (6)}
  \deqn{(s^*)^2 = \frac{1}{n-r-1} \sum_{i=r+1}^n (x_i - \bar{x}^*)^2 \;\;\;\; (7)}
  Note that the quantity in equation (5) is the sample mean of all observations 
  (including 0 values), the quantity in equation (6) is the sample mean of all non-zero 
  observations, and the quantity in equation (7) is the sample variance of all 
  non-zero observations.  Also note that for \eqn{r=n-1} or \eqn{r=n}, the estimator 
  of \eqn{\delta^2} is the sample variance for all observations (including 0 values).
  \cr

  \bold{Confidence Intervals} \cr

  \emph{Based on Normal Approximation} (\code{ci.method="normal.approx"}) \cr
  An approximate \eqn{(1-\alpha)100\%} confidence interval for \eqn{\gamma} is 
  constructed based on the assumption that the estimator of \eqn{\gamma} is 
  approximately normally distributed.  Aitchison (1955) shows that 
  \deqn{Var(\hat{\gamma}_{mvue}) = Var(\bar{x}) = \frac{\delta^2}{n} \;\;\;\; (8)}
  Thus, an approximate two-sided \eqn{(1-\alpha)100\%} confidence interval for 
  \eqn{\gamma} is constructed as:
  \deqn{[ \hat{\gamma}_{mvue} - t_{n-2, 1-\alpha/2} \frac{\hat{\delta}_{mvue}}{\sqrt{n}}, \; \hat{\gamma}_{mvue} + t_{n-2, 1-\alpha/2} \frac{\hat{\delta}_{mvue}}{\sqrt{n}} ] \;\;\;\; (9)}
  where \eqn{t_{\nu, p}} is the \eqn{p}'th quantile of 
  \link[stats:TDist]{Student's t-distribution} with \eqn{\nu} degrees of freedom.

  One-sided confidence intervals are computed in a similar fashion.
}
\value{
  a list of class \code{"estimate"} containing the estimated parameters and other information.  
  See \cr
  \code{\link{estimate.object}} for details.

  The component called \code{parameters} is a numeric vector with the following 
  estimated parameters: 
  \tabular{ll}{
    \bold{Parameter Name} \tab \bold{Explanation} \cr
    \code{mean} \tab mean of the normal (Gaussian) part of the distribution. \cr
    \code{sd}   \tab standard deviation of the normal (Gaussian) part of the distribution. \cr
    \code{p.zero}  \tab probability that an observation will be 0. \cr
    \code{mean.zmnorm} \tab mean of the overall zero-modified normal distribution. \cr
    \code{sd.zmnorm} \tab standard deviation of the overall normal distribution. \cr
  }
}
\references{
  Aitchison, J. (1955).  On the Distribution of a Positive Random Variable Having 
  a Discrete Probability Mass at the Origin.  \emph{Journal of the American 
  Statistical Association} \bold{50}, 901--908.

  Gilliom, R.J., and D.R. Helsel. (1986).  Estimation of Distributional Parameters 
  for Censored Trace Level Water Quality Data: 1. Estimation Techniques.  
  \emph{Water Resources Research} \bold{22}, 135--146.

  Owen, W., and T. DeRouen. (1980).  Estimation of the Mean for Lognormal Data 
  Containing Zeros and Left-Censored Values, with Applications to the Measurement 
  of Worker Exposure to Air Contaminants.  \emph{Biometrics} \bold{36}, 707--719.

  USEPA (1992c).  \emph{Statistical Analysis of Ground-Water Monitoring Data at 
  RCRA Facilities: Addendum to Interim Final Guidance}.  Office of Solid Waste, 
  Permits and State Programs Division, US Environmental Protection Agency, 
  Washington, D.C.
}
\author{
    Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  The \link[=ZeroModifiedNormal]{zero-modified normal distribution} is sometimes 
  used to model chemical concentrations for which some observations are reported 
  as \dQuote{Below Detection Limit}.  See, for example USEPA (1992c, pp.27-34).  
  In most cases, however, the zero-modified lognormal (delta) distribution will be 
  more appropriate, since chemical concentrations are bounded below at 0 
  (e.g., Gilliom and Helsel, 1986; Owen and DeRouen, 1980).

  Once you estimate the parameters of the zero-modified normal distribution, it is 
  often useful to characterize the uncertainty in the estimate of the mean.  This is 
  done with a confidence interval.

  One way to try to assess whether a 
  \link[=ZeroModifiedLognormal]{zero-modified lognormal (delta)}, 
  \link[=ZeroModifiedNormal]{zero-modified normal}, censored normal, or 
  censored lognormal is the best model for the data is to construct both 
  censored and detects-only probability plots (see \code{\link{qqPlotCensored}}).
}
\seealso{
  \link{ZeroModifiedNormal}, \link[stats]{Normal}, 
  \code{\link{ezmlnorm}}, \link{ZeroModifiedLognormal}, \code{\link{estimate.object}}.
}
\examples{
  # Generate 100 observations from a zero-modified normal distribution 
  # with mean=4, sd=2, and p.zero=0.5, then estimate the parameters.  
  # According to equations (1) and (2) above, the overall mean is 
  # mean.zmnorm=2 and the overall standard deviation is sd.zmnorm=sqrt(6).  
  # (Note: the call to set.seed simply allows you to reproduce this example.)

  set.seed(250) 
  dat <- rzmnorm(100, mean = 4, sd = 2, p.zero = 0.5) 
  ezmnorm(dat, ci = TRUE) 

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            Zero-Modified Normal
  #
  #Estimated Parameter(s):          mean        = 4.037732
  #                                 sd          = 1.917004
  #                                 p.zero      = 0.450000
  #                                 mean.zmnorm = 2.220753
  #                                 sd.zmnorm   = 2.465829
  #
  #Estimation Method:               mvue
  #
  #Data:                            dat
  #
  #Sample Size:                     100
  #
  #Confidence Interval for:         mean.zmnorm
  #
  #Confidence Interval Method:      Normal Approximation
  #                                 (t Distribution)
  #
  #Confidence Interval Type:        two-sided
  #
  #Confidence Level:                95%
  #
  #Confidence Interval:             LCL = 1.731417
  #                                 UCL = 2.710088

  #----------

  # Following Example 9 on page 34 of USEPA (1992c), compute an 
  # estimate of the mean of the zinc data, assuming a 
  # zero-modified normal distribution. The data are stored in 
  # EPA.92c.zinc.df.

  head(EPA.92c.zinc.df) 
  #  Zinc.orig  Zinc Censored Sample Well
  #1        <7  7.00     TRUE      1    1
  #2     11.41 11.41    FALSE      2    1
  #3        <7  7.00     TRUE      3    1
  #4        <7  7.00     TRUE      4    1
  #5        <7  7.00     TRUE      5    1
  #6     10.00 10.00    FALSE      6    1

  New.Zinc <- EPA.92c.zinc.df$Zinc 
  New.Zinc[EPA.92c.zinc.df$Censored] <- 0 
  ezmnorm(New.Zinc, ci = TRUE) 

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            Zero-Modified Normal
  #
  #Estimated Parameter(s):          mean        = 11.891000
  #                                 sd          =  1.594523
  #                                 p.zero      =  0.500000
  #                                 mean.zmnorm =  5.945500
  #                                 sd.zmnorm   =  6.123235
  #
  #Estimation Method:               mvue
  #
  #Data:                            New.Zinc
  #
  #Sample Size:                     40
  #
  #Confidence Interval for:         mean.zmnorm
  #
  #Confidence Interval Method:      Normal Approximation
  #                                 (t Distribution)
  #
  #Confidence Interval Type:        two-sided
  #
  #Confidence Level:                95%
  #
  #Confidence Interval:             LCL = 3.985545
  #                                 UCL = 7.905455

  #----------

  # Clean up
  rm(dat, New.Zinc)
}
\keyword{ distribution }
\keyword{ htest }
