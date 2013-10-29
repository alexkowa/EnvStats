\name{eqlnorm}
\alias{eqlnorm}
\title{
  Estimate Quantiles of a Lognormal Distribution
}
\description{
  Estimate quantiles of a \link[stats:Lognormal]{lognormal distribution}, 
  and optionally construct a confidence interval for a quantile.
}
\usage{
  eqlnorm(x, p = 0.5, method = "qmle", ci = FALSE, 
    ci.method = "exact", ci.type = "two-sided", conf.level = 0.95, 
    digits = 0)
}
\arguments{
  \item{x}{
  a numeric vector of positive observations, or an object resulting from a call to an 
  estimating function that assumes a lognormal distribution 
  (i.e., \code{\link{elnorm}}, \code{\link{elnormCensored}}). 
  You \emph{cannot} use objects resulting 
  from a call to estimating functions that use the alternative parameterization 
  such as \code{elnormAlt}.  If \code{x} is a numeric vector, 
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
  The possible values are \code{"qmle"} (quasi maximum likelihood; the default) and \cr
  \code{"mvue"} (minimum variance unbiased).  The method \code{"mvue"} is available only 
  when \code{p=0.5} (i.e., when you are estimating the median).  See the DETAILS section 
  for more information.
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
}
\details{
  If \code{x} contains any missing (\code{NA}), undefined (\code{NaN}) or 
  infinite (\code{Inf}, \code{-Inf}) values, they will be removed prior to 
  performing the estimation.

  Quantiles and their associated confidence intervals are constructed by calling 
  the function \code{\link{eqnorm}} using the log-transformed data and then 
  exponentiating the quantiles and confidence limits.

  In the special case when \code{p=0.5} and \code{method="mvue"}, the estimated 
  median is computed using the method given in Gilbert (1987, p.172) and 
  Bradu and Mundlak (1970).
}
\value{
  If \code{x} is a numeric vector, \code{eqlnorm} returns a list of class 
  \code{"estimate"} containing the estimated quantile(s) and other information.  
  See \code{\link{estimate.object}} for details.

  If \code{x} is the result of calling an estimation function, \code{eqlnorm} 
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

  Bradu, D., and Y. Mundlak. (1970). Estimation in Lognormal Linear Models. 
  \emph{Journal of the American Statistical Association} \bold{65}, 198-211.

  Conover, W.J. (1980). \emph{Practical Nonparametric Statistics}. Second Edition. 
  John Wiley and Sons, New York.

  Gibbons, R.D., D.K. Bhaumik, and S. Aryal. (2009). 
  \emph{Statistical Methods for Groundwater Monitoring}, Second Edition.  
  John Wiley & Sons, Hoboken.

  Gilbert, R.O. (1987). \emph{Statistical Methods for Environmental Pollution Monitoring}. 
  Van Nostrand Reinhold, New York.

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
  Percentiles are sometimes used in environmental standards and regulations.  For example, 
  Berthouex and Brown (2002, p.71) note that England has water quality limits based on 
  the 90th and 95th percentiles of monitoring data not exceeding specified levels.  They also 
  note that the U.S. EPA has specifications for air quality monitoring, aquatic standards on 
  toxic chemicals, and maximum daily limits for industrial effluents that are all based on 
  percentiles.  Given the importance of these quantities, it is essential to characterize 
  the amount of uncertainty associated with the estimates of these quantities.  
  This is done with confidence intervals.
}
\seealso{
  \code{\link{eqnorm}}, \code{\link{Lognormal}}, \code{\link{elnorm}}, 
  \code{\link{estimate.object}}.
}
\examples{
  # Generate 20 observations from a lognormal distribution with 
  # parameters meanlog=3 and sdlog=0.5, then estimate the 90th 
  # percentile and create a one-sided upper 95% confidence interval 
  # for that percentile. 
  # (Note: the call to set.seed simply allows you to reproduce this 
  # example.)

  set.seed(47) 
  dat <- rlnorm(20, meanlog = 3, sdlog = 0.5) 
  eqlnorm(dat, p = 0.9, ci = TRUE, ci.type = "upper")

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            Lognormal
  #
  #Estimated Parameter(s):          meanlog = 2.9482139
  #                                 sdlog   = 0.4553215
  #
  #Estimation Method:               mvue
  #
  #Estimated Quantile(s):           90'th %ile = 34.18312
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
  #Confidence Interval:             LCL =  0.00000
  #                                 UCL = 45.84008

  #----------
  # Compare these results with the true 90'th percentile:

  qlnorm(p = 0.9, meanlog = 3, sdlog = 0.5)
  #[1] 38.1214

  #----------

  # Clean up
  rm(dat)
  
  #--------------------------------------------------------------------

  # Example 17-3 of USEPA (2009, p. 17-17) shows how to construct a 
  # beta-content upper tolerance limit with 95% coverage and 95% 
  # confidence using chrysene data and assuming a lognormal 
  # distribution.

  # A beta-content upper tolerance limit with 95% coverage and 95% 
  # confidence is equivalent to the 95% upper confidence limit for the 
  # 95th percentile.

  attach(EPA.09.Ex.17.3.chrysene.df)
  Chrysene <- Chrysene.ppb[Well.type == "Background"]
  eqlnorm(Chrysene, p = 0.95, ci = TRUE, ci.type = "upper")

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            Lognormal
  #
  #Estimated Parameter(s):          meanlog = 2.5085773
  #                                 sdlog   = 0.6279479
  #
  #Estimation Method:               mvue
  #
  #Estimated Quantile(s):           95'th %ile = 34.51727
  #
  #Quantile Estimation Method:      qmle
  #
  #Data:                            Chrysene
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
  #Confidence Interval:             LCL =  0.0000
  #                                 UCL = 90.9247

  #----------
  # Clean up

  rm(Chrysene)
  detach("EPA.09.Ex.17.3.chrysene.df")
}
\keyword{ distribution }
\keyword{ htest }
