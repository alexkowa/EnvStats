\name{enorm}
\alias{enorm}
\title{
  Estimate Parameters of a Normal (Gaussian) Distribution
}
\description{
  Estimate the mean and standard deviation parameters of a 
  \link[stats:Normal]{normal (Gaussian) distribution}, and optionally construct a 
  confidence interval for the mean or the variance.
}
\usage{
  enorm(x, method = "mvue", ci = FALSE, ci.type = "two-sided", 
    ci.method = "exact", conf.level = 0.95, ci.param = "mean")
}
\arguments{
  \item{x}{
  numeric vector of observations.
}
  \item{method}{
  character string specifying the method of estimation.  Possible values are 
  \code{"mvue"} (minimum variance unbiased; the default), and \code{"mle/mme"} 
  (maximum likelihood/method of moments).    
  See the DETAILS section for more information on these estimation methods.
}
  \item{ci}{
  logical scalar indicating whether to compute a confidence interval for the 
  mean or variance.  The default value is \code{FALSE}.
}
  \item{ci.type}{
  character string indicating what kind of confidence interval to compute.  The 
  possible values are \code{"two-sided"} (the default), \code{"lower"}, and 
  \code{"upper"}.  This argument is ignored if \code{ci=FALSE}.
}
  \item{ci.method}{
  character string indicating what method to use to construct the confidence interval 
  for the mean or variance.  The only possible value is \code{"exact"} (the default).  
  See the DETAILS section for more information.  This argument is ignored if 
  \code{ci=FALSE}.
}
  \item{conf.level}{
  a scalar between 0 and 1 indicating the confidence level of the confidence interval.  
  The default value is \code{conf.level=0.95}. This argument is ignored if 
  \code{ci=FALSE}.
}
  \item{ci.param}{
  character string indicating which parameter to create a confidence interval for.  
  The possible values are \code{ci.param="mean"} (the default) and \cr
  \code{ci.param="variance"}.  This argument is ignored if \code{ci=FALSE}.
}
}
\details{
  If \code{x} contains any missing (\code{NA}), undefined (\code{NaN}) or 
  infinite (\code{Inf}, \code{-Inf}) values, they will be removed prior to 
  performing the estimation.

  Let \eqn{\underline{x} = (x_1, x_2, \ldots, x_n)} be a vector of 
  \eqn{n} observations from an \link[stats:Normal]{normal (Gaussian) distribution} with 
  parameters \code{mean=}\eqn{\mu} and \code{sd=}\eqn{\sigma}.

  \bold{Estimation} \cr

  \emph{Minimum Variance Unbiased Estimation} (\code{method="mvue"}) \cr
  The minimum variance unbiased estimators (mvue's) of the mean and variance are:
  \deqn{\hat{\mu}_{mvue} = \bar{x} = \frac{1}{n} \sum_{i=1}^n x_i \;\;\;\; (1)}
  \deqn{\hat{\sigma}^2_{mvue} = s^2 = \frac{1}{n-1} \sum_{i=1}^n (x_i - \bar{x})^2 \;\;\;\; (2)}
  (Johnson et al., 1994; Forbes et al., 2011).  Note that when \code{method="mvue"}, 
  the estimated standard deviation is the square root of the mvue of the variance, 
  but is not itself an mvue.

  \emph{Maximum Likelihood/Method of Moments Estimation} (\code{method="mle/mme"}) \cr
  The maximum likelihood estimator (mle) and method of moments estimator (mme) of the 
  mean are both the same as the mvue of the mean given in equation (1) above.  The 
  mle and mme of the variance is given by:
  \deqn{\hat{\sigma}^2_{mle} = s^2_m = \frac{n-1}{n} s^2 = \frac{1}{n} \sum_{i=1}^n (x_i - \bar{x})^2 \;\;\;\; (3)}
  When \code{method="mle/mme"}, the estimated standard deviation is the square root of 
  the mle of the variance, and is itself an mle.
  \cr

  \bold{Confidence Intervals} \cr

  \emph{Confidence Interval for the Mean} (\code{ci.param="mean"}) \cr
  When \code{ci=TRUE} and \code{ci.param = "mean"}, the usual confidence interval 
  for \eqn{\mu} is constructed as follows.  If \code{ci.type="two-sided"}, a 
  the \eqn{(1-\alpha)}100\% confidence interval for \eqn{\mu} is given by:
  \deqn{[\hat{\mu} - t(n-1, 1-\alpha/2) \frac{\hat{\sigma}}{\sqrt{n}}, \, \hat{\mu} + t(n-1, 1-\alpha/2) \frac{\hat{\sigma}}{\sqrt{n}}] \;\;\;\; (4)}
  where \eqn{t(\nu, p)} is the \eqn{p}'th quantile of 
  \link[stats:TDist]{Student's t-distribution} with \eqn{\nu} degrees of freedom 
  (Zar, 2010; Gilbert, 1987; Ott, 1995; Helsel and Hirsch, 1992).

  If \code{ci.type="lower"}, the \eqn{(1-\alpha)}100\% confidence interval for 
  \eqn{\mu} is given by:
  \deqn{[\hat{\mu} - t(n-1, 1-\alpha) \frac{\hat{\sigma}}{\sqrt{n}}, \, \infty] \;\;\;\; (5)}
  and if \code{ci.type="upper"}, the confidence interval is given by:
  \deqn{[-\infty, \, \hat{\mu} + t(n-1, 1-\alpha/2) \frac{\hat{\sigma}}{\sqrt{n}}] \;\;\;\; (6)}

  \emph{Confidence Interval for the Variance} (\code{ci.param="variance"}) \cr
  When \code{ci=TRUE} and \code{ci.param = "variance"}, the usual confidence interval 
  for \eqn{\sigma^2} is constructed as follows.  A two-sided 
  \eqn{(1-\alpha)}100\% confidence interval for \eqn{\sigma^2} is given by:
  \deqn{[ \frac{(n-1)s^2}{\chi^2_{n-1,1-\alpha/2}}, \, \frac{(n-1)s^2}{\chi^2_{n-1,\alpha/2}} ] \;\;\;\; (7)}
  Similarly, a one-sided upper \eqn{(1-\alpha)}100\% confidence interval for the 
  population variance is given by:
  \deqn{[ 0, \, \frac{(n-1)s^2}{\chi^2_{n-1,\alpha}} ] \;\;\;\; (8)}
  and a one-sided lower \eqn{(1-\alpha)}100\% confidence interval for the population 
  variance is given by:
  \deqn{[ \frac{(n-1)s^2}{\chi^2_{n-1,1-\alpha}}, \, \infty ] \;\;\;\; (9)}
  (van Belle et al., 2004; Zar, 2010).
}
\value{
  a list of class \code{"estimate"} containing the estimated parameters and other information.  
  See \cr
  \code{\link{estimate.object}} for details.
}
\references{
  Berthouex, P.M., and L.C. Brown. (2002). 
  \emph{Statistics for Environmental Engineers}.  Second Edition.   
  Lewis Publishers, Boca Raton, FL.

  Forbes, C., M. Evans, N. Hastings, and B. Peacock. (2011).  Statistical Distributions. 
  Fourth Edition. John Wiley and Sons, Hoboken, NJ.

  Gilbert, R.O. (1987). \emph{Statistical Methods for Environmental Pollution Monitoring}. 
  Van Nostrand Reinhold, New York, NY.

  Helsel, D.R., and R.M. Hirsch. (1992). 
  \emph{Statistical Methods in Water Resources Research}. 
  Elsevier, New York, NY, Chapter 7.

  Johnson, N. L., S. Kotz, and N. Balakrishnan. (1994). 
  \emph{Continuous Univariate Distributions, Volume 1}. 
  Second Edition. John Wiley and Sons, New York.

  Millard, S.P., and N.K. Neerchal. (2001). \emph{Environmental Statistics with S-PLUS}. 
  CRC Press, Boca Raton, FL.

  Ott, W.R. (1995). \emph{Environmental Statistics and Data Analysis}. 
  Lewis Publishers, Boca Raton, FL.

  USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
  EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.  
  U.S. Environmental Protection Agency, Washington, D.C.

  van Belle, G., L.D. Fisher, Heagerty, P.J., and Lumley, T. (2004). 
  \emph{Biostatistics: A Methodology for the Health Sciences, 2nd Edition}. 
  John Wiley & Sons, New York.

  Zar, J.H. (2010). \emph{Biostatistical Analysis}. Fifth Edition. 
  Prentice-Hall, Upper Saddle River, NJ.
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
}
\seealso{
  \link[stats]{Normal}.
}
\examples{
  # Generate 20 observations from a N(3, 2) distribution, then estimate 
  # the parameters and create a 95\% confidence interval for the mean. 
  # (Note: the call to set.seed simply allows you to reproduce this example.)

  set.seed(250) 
  dat <- rnorm(20, mean = 3, sd = 2) 
  enorm(dat, ci = TRUE) 

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            Normal
  #
  #Estimated Parameter(s):          mean = 2.861160
  #                                 sd   = 1.180226
  #
  #Estimation Method:               mvue
  #
  #Data:                            dat
  #
  #Sample Size:                     20
  #
  #Confidence Interval for:         mean
  #
  #Confidence Interval Method:      Exact
  #
  #Confidence Interval Type:        two-sided
  #
  #Confidence Level:                95%
  #
  #Confidence Interval:             LCL = 2.308798
  #                                 UCL = 3.413523

  #----------

  # Using the same data, construct an upper 90% confidence interval for
  # the variance.

  enorm(dat, ci = TRUE, ci.type = "upper", ci.param = "variance")$interval

  #Confidence Interval for:         variance
  #
  #Confidence Interval Method:      Exact
  #
  #Confidence Interval Type:        upper
  #
  #Confidence Level:                95%
  #
  #Confidence Interval:             LCL = 0.000000
  #                                 UCL = 2.615963

  #----------

  # Clean up
  #---------
  rm(dat)

  #----------

  # Using the Reference area TcCB data in the data frame EPA.94b.tccb.df, 
  # estimate the mean and standard deviation of the log-transformed data, 
  # and construct a 95% confidence interval for the mean.

  with(EPA.94b.tccb.df, enorm(log(TcCB[Area == "Reference"]), ci = TRUE))  

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            Normal
  #
  #Estimated Parameter(s):          mean = -0.6195712
  #                                 sd   =  0.4679530
  #
  #Estimation Method:               mvue
  #
  #Data:                            log(TcCB[Area == "Reference"])
  #
  #Sample Size:                     47
  #
  #Confidence Interval for:         mean
  #
  #Confidence Interval Method:      Exact
  #
  #Confidence Interval Type:        two-sided
  #
  #Confidence Level:                95%
  #
  #Confidence Interval:             LCL = -0.7569673
  #                                 UCL = -0.4821751
}
\keyword{ distribution }
\keyword{ htest }
