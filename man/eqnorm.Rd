\name{eqnorm}
\alias{eqnorm}
\title{
  Estimate Quantiles of a Normal Distribution
}
\description{
  Estimate quantiles of a \link[stats:Normal]{normal distribution}, and optionally construct a 
  confidence interval for a quantile.
}
\usage{
  eqnorm(x, p = 0.5, method = "qmle", ci = FALSE, 
    ci.method = "exact", ci.type = "two-sided", conf.level = 0.95, 
    digits = 0, warn = TRUE)
}
\arguments{
  \item{x}{
  a numeric vector of observations, or an object resulting from a call to an 
  estimating function that assumes a normal (Gaussian) distribution 
  (i.e., \code{\link{enorm}}, \code{\link{enormCensored}}). 
  If \code{x} is a numeric vector, 
  missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) 
  values are allowed but will be removed.
}
  \item{p}{
  numeric vector of probabilities for which quantiles will be estimated.  
  All values of \code{p} must be between 0 and 1.  When \code{ci=TRUE}, \code{p} 
  must be a scalar. The default value is \code{p=0.5}.
}
  \item{method}{
  character string indicating what method to use to estimate the quantile(s).  
  Currently the only possible value is \code{method="qmle"} (quasi maximum likelihood).  
  See the DETAILS section for more information.
}
  \item{ci}{
  logical scalar indicating whether to compute a confidence interval for the quantile.  
  The default value is \code{ci=FALSE}.
}
  \item{ci.method}{
  character string indicating what method to use to construct the confidence interval 
  for the quantile.  The possible values are \code{"exact"} (exact method; the default) 
  and \code{"normal.approx"} (normal approximation).  See the DETAILS section for more 
  information.
}
  \item{ci.type}{
  character string indicating what kind of confidence interval for the quantile to compute.  
  The possible values are \code{"two-sided"} (the default), \code{"lower"}, and 
  \code{"upper"}.  This argument is ignored if \code{ci=FALSE}.
}
  \item{conf.level}{
  a scalar between 0 and 1 indicating the confidence level of the confidence interval.  
  The default value is \code{conf.level=0.95}.  This argument is ignored if \code{ci=FALSE}.
}
  \item{digits}{
  an integer indicating the number of decimal places to round to when printing out 
  the value of \code{100*p}. The default value is \code{digits=0}.
}
  \item{warn}{
  logical scalar indicating whether to warn in the case when \code{ci=TRUE}, \cr
  \code{ci.method="exact"}, and the supplied object \code{x} is of class 
  \code{"estimate"} but did not use \code{method="mvue"} for estimation.
}
}
\details{
  If \code{x} contains any missing (\code{NA}), undefined (\code{NaN}) or 
  infinite (\code{Inf}, \code{-Inf}) values, they will be removed prior to 
  performing the estimation.

  Quantiles are estimated by 1) estimating the mean and standard deviation parameters by 
  calling \code{\link{enorm}} with \code{method="mvue"}, and then 
  2) calling the function \code{\link[=Normal]{qnorm}} and using the estimated values 
  for mean and standard deviation.  This estimator of the \eqn{p}'th quantile is 
  sometimes called the quasi-maximum likelihood estimator (qmle; Cohn et al., 1989) 
  because if the maximum likelihood estimator of standard deviation were used 
  in place of the minimum variaince unbiased one, then this estimator of the quantile 
  would be the mle of the \eqn{p}'th quantile.

  When \code{ci=TRUE} and \code{ci.method="exact"}, the confidence interval for a 
  quantile is computed by using the relationship between a confidence interval for 
  a quantile and a tolerance interval.  Specifically, it can be shown 
  (e.g., Conover, 1980, pp.119-121) that an upper confidence interval for the 
  \eqn{p}'th quantile with confidence level \eqn{100(1-\alpha)\%} is equivalent to 
  an upper \eqn{\beta}-content tolerance interval with coverage \eqn{100p\%} and 
  confidence level \eqn{100(1-\alpha)\%}.  Also, a lower confidence interval for 
  the \eqn{p}'th quantile with confidence level \eqn{100(1-\alpha)\%} is equivalent 
  to a lower \eqn{\beta}-content tolerance interval with coverage \eqn{100(1-p)\%} and 
  confidence level \eqn{100(1-\alpha)\%}.  See the help file for \code{\link{tolIntNorm}} 
  for information on tolerance intervals for a normal distribution.

  When \code{ci=TRUE} and \code{ci.method="normal.approx"}, the confidence interval for a 
  quantile is computed by assuming the estimated quantile has an approximately normal 
  distribution and using the asymptotic variance to construct the confidence interval 
  (see Stedinger, 1983; Stedinger et al., 1993).
}
\value{
  If \code{x} is a numeric vector, \code{eqnorm} returns a list of class 
  \code{"estimate"} containing the estimated quantile(s) and other information.  
  See \code{\link{estimate.object}} for details.

  If \code{x} is the result of calling an estimation function, \code{eqnorm} 
  returns a list whose class is the same as \code{x}.  The list contains the same 
  components as \code{x}, as well as components called \code{quantiles} and 
  \code{quantile.method}.  In addition, if \code{ci=TRUE}, the returned list 
  contains a component called \code{interval} containing the confidence interval 
  information. If \code{x} already has a component called \code{interval}, this 
  component is replaced with the confidence interval information.
}
\references{
  Berthouex, P.M., and L.C. Brown. (2002). \emph{Statistics for Environmental Engineers}. 
  Lewis Publishers, Boca Raton.

  Conover, W.J. (1980). \emph{Practical Nonparametric Statistics}. Second Edition. 
  John Wiley and Sons, New York.

  Gibbons, R.D., D.K. Bhaumik, and S. Aryal. (2009). 
  \emph{Statistical Methods for Groundwater Monitoring}, Second Edition.  
  John Wiley & Sons, Hoboken.

  Gilbert, R.O. (1987). \emph{Statistical Methods for Environmental Pollution Monitoring}. 
  Van Nostrand Reinhold, New York, NY, pp.132-136.

  Helsel, D.R., and R.M. Hirsch. (1992). \emph{Statistical Methods in Water Resources Research}. 
  Elsevier, New York, NY, pp.88-90.

  Johnson, N.L., and B.L. Welch. (1940). Applications of the Non-Central t-Distribution. 
  \emph{Biometrika} \bold{31}, 362-389.

  Millard, S.P., and Neerchal, N.K. (2001). \emph{Environmental Statistics with S-PLUS}. 
  CRC Press, Boca Raton, Florida.

  Owen, D.B. (1962). \emph{Handbook of Statistical Tables}. Addison-Wesley, Reading, MA.

  Stedinger, J. (1983). Confidence Intervals for Design Events. 
  \emph{Journal of Hydraulic Engineering} \bold{109}(1), 13-27.

  Stedinger, J.R., R.M. Vogel, and E. Foufoula-Georgiou. (1993). 
  Frequency Analysis of Extreme Events. In: Maidment, D.R., ed. \emph{Handbook of Hydrology}. 
  McGraw-Hill, New York, Chapter 18, pp.29-30.

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
  Percentiles are sometimes used in environmental standards and regulations.  
  For example, Berthouex and Brown (2002, p.71) note that England has water 
  quality limits based on the 90th and 95th percentiles of monitoring data not 
  exceeding specified levels.  They also note that the U.S. EPA has specifications 
  for air quality monitoring, aquatic standards on toxic chemicals, and maximum 
  daily limits for industrial effluents that are all based on percentiles.  Given 
  the importance of these quantities, it is essential to characterize the amount 
  of uncertainty associated with the estimates of these quantities.  This is done 
  with confidence intervals.
}
\seealso{
  \code{\link{enorm}}, \code{\link{tolIntNorm}}, \code{\link{Normal}},  
  \code{\link{estimate.object}}.
}
\examples{
  # Generate 20 observations from a normal distribution with 
  # parameters mean=10 and sd=2, then estimate the 90th 
  # percentile and create a one-sided upper 95% confidence interval 
  # for that percentile. 
  # (Note: the call to set.seed simply allows you to reproduce this 
  # example.)

  set.seed(47) 
  dat <- rnorm(20, mean = 10, sd = 2) 
  eqnorm(dat, p = 0.9, ci = TRUE, ci.type = "upper")

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            Normal
  #
  #Estimated Parameter(s):          mean = 9.792856
  #                                 sd   = 1.821286
  #
  #Estimation Method:               mvue
  #
  #Estimated Quantile(s):           90'th %ile = 12.12693
  #
  #Quantile Estimation Method:      qmle
  #
  #Data:                            dat
  #
  #Sample Size:                     20
  #
  #Confidence Interval for:         90'th %ile
  #
  #Confidence Interval Method:      Exact
  #
  #Confidence Interval Type:        upper
  #
  #Confidence Level:                95%
  #
  #Confidence Interval:             LCL =     -Inf
  #                                 UCL = 13.30064

  #----------
  # Compare these results with the true 90'th percentile:

  qnorm(p = 0.9, mean = 10, sd = 2)
  #[1] 12.56310

  #----------

  # Clean up
  rm(dat)

  #==========

  # Example 21-4 of USEPA (2009, p. 21-13) shows how to construct a 
  # 99% lower confidence limit for the 95th percentile using chrysene 
  # data and assuming a lognormal distribution.  The data for this 
  # example are stored in EPA.09.Ex.21.1.aldicarb.df.

  # The facility permit has established an ACL of 30 ppb that should not 
  # be exceeded more than 5% of the time.  Thus, if the lower confidence limit 
  # for the 95th percentile is greater than 30 ppb, the well is deemed to be 
  # out of compliance.

  # Look at the data
  #-----------------

  head(EPA.09.Ex.21.1.aldicarb.df)
  #  Month   Well Aldicarb.ppb
  #1     1 Well.1         19.9
  #2     2 Well.1         29.6
  #3     3 Well.1         18.7
  #4     4 Well.1         24.2
  #5     1 Well.2         23.7
  #6     2 Well.2         21.9

  longToWide(EPA.09.Ex.21.1.aldicarb.df, 
    "Aldicarb.ppb", "Month", "Well", paste.row.name = TRUE)
  #        Well.1 Well.2 Well.3
  #Month.1   19.9   23.7    5.6
  #Month.2   29.6   21.9    3.3
  #Month.3   18.7   26.9    2.3
  #Month.4   24.2   26.1    6.9

  # Estimate the 95th percentile and compute the lower 
  # 99% confidence limit for Well 1.
  #---------------------------------------------------

  with(EPA.09.Ex.21.1.aldicarb.df, 
    eqnorm(Aldicarb.ppb[Well == "Well.1"], p = 0.95, ci = TRUE, 
      ci.type = "lower", conf.level = 0.99))

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            Normal
  #
  #Estimated Parameter(s):          mean = 23.10000
  #                                 sd   =  4.93491
  #
  #Estimation Method:               mvue
  #
  #Estimated Quantile(s):           95'th %ile = 31.2172
  #
  #Quantile Estimation Method:      qmle
  #
  #Data:                            Aldicarb.ppb[Well == "Well.1"]
  #
  #Sample Size:                     4
  #
  #Confidence Interval for:         95'th %ile
  #
  #Confidence Interval Method:      Exact
  #
  #Confidence Interval Type:        lower
  #
  #Confidence Level:                99%
  #
  #Confidence Interval:             LCL = 25.2855
  #                                 UCL =     Inf
 

  # Now compute the 99% lower confidence limit for each of the three 
  # wells all at once.
  #------------------------------------------------------------------

  LCLs <- with(EPA.09.Ex.21.1.aldicarb.df, 
    sapply(split(Aldicarb.ppb, Well), 
      function(x) eqnorm(x, p = 0.95, method = "qmle", ci = TRUE, 
      ci.type = "lower", conf.level = 0.99)$interval$limits["LCL"]))

  round(LCLs, 2)
  #Well.1.LCL Well.2.LCL Well.3.LCL 
  #     25.29      25.66       5.46 

  LCLs > 30
  #Well.1.LCL Well.2.LCL Well.3.LCL 
  #     FALSE      FALSE      FALSE


  # Clean up
  #---------

  rm(LCLs)

  
  #==========

  # Example 17-3 of USEPA (2009, p. 17-17) shows how to construct a 
  # beta-content upper tolerance limit with 95% coverage and 95% 
  # confidence using chrysene data and assuming a lognormal 
  # distribution.

  # A beta-content upper tolerance limit with 95% coverage and 95% 
  # confidence is equivalent to the 95% upper confidence limit for the 
  # 95th percentile.

  # Here we will construct a 95% upper confidence limit for the 95th 
  # percentile based on the log-transformed data, then exponentiate the 
  # result to get the confidence limit on the original scale.  Note that 
  # it is easier to just use the function eqlnorm with the original data 
  # to achieve the same result.

  attach(EPA.09.Ex.17.3.chrysene.df)
  log.Chrysene <- log(Chrysene.ppb[Well.type == "Background"])
  eqnorm(log.Chrysene, p = 0.95, ci = TRUE, ci.type = "upper")

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            Normal
  #
  #Estimated Parameter(s):          mean = 2.5085773
  #                                 sd   = 0.6279479
  #
  #Estimation Method:               mvue
  #
  #Estimated Quantile(s):           95'th %ile = 3.54146
  #
  #Quantile Estimation Method:      qmle
  #
  #Data:                            log.Chrysene
  #
  #Sample Size:                     8
  #
  #Confidence Interval for:         95'th %ile
  #
  #Confidence Interval Method:      Exact
  #
  #Confidence Interval Type:        upper
  #
  #Confidence Level:                95%
  #
  #Confidence Interval:             LCL =     -Inf
  #                                 UCL = 4.510032

  exp(4.510032)
  #[1] 90.92473

  #----------
  # Clean up

  rm(log.Chrysene)
  detach("EPA.09.Ex.17.3.chrysene.df")
}
\keyword{ distribution }
\keyword{ htest }
