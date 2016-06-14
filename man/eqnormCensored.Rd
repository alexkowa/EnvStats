\name{eqnormCensored}
\alias{eqnormCensored}
\title{
  Estimate Quantiles of a Normal Distribution Based on Type I Censored Data
}
\description{
  Estimate quantiles of a \link[stats:Normal]{normal distribution} given a 
  sample of data that has been subjected to Type I censoring, and optionally 
  construct a confidence interval for a quantile.
}
\usage{
  eqnormCensored(x, censored, censoring.side = "left", p = 0.5, method = "mle", 
    ci = FALSE, ci.method = "exact.for.complete", ci.type = "two-sided", 
    conf.level = 0.95, digits = 0, nmc = 1000, seed = NULL)
}
\arguments{
  \item{x}{
  a numeric vector of observations.  
  Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) 
  values are allowed but will be removed.
}
  \item{censored}{
  numeric or logical vector indicating which values of \code{x} are censored.  
  This must be the same length as \code{x}.  If the mode of \code{censored} is 
  \code{"logical"}, \code{TRUE} values correspond to elements of \code{x} that 
  are censored, and \code{FALSE} values correspond to elements of \code{x} that 
  are not censored.  If the mode of \code{censored} is \code{"numeric"}, 
  it must contain only \code{1}'s and \code{0}'s; \code{1} corresponds to 
  \code{TRUE} and \code{0} corresponds to \code{FALSE}.  Missing (\code{NA}) 
  values are allowed but will be removed.
}
  \item{censoring.side}{
  character string indicating on which side the censoring occurs.  The possible 
  values are \code{"left"} (the default) and \code{"right"}. 
}
  \item{p}{
  numeric vector of probabilities for which quantiles will be estimated.  
  All values of \code{p} must be between 0 and 1.  When \code{ci=TRUE}, \code{p} 
  must be a scalar. The default value is \code{p=0.5}.
}
  \item{method}{
  character string specifying the method of estimating the mean and standard deviation.  

  For singly censored data, the possible values are: \cr
  \code{"mle"} (maximum likelihood; the default), \cr
  \code{"bcmle"} (bias-corrected maximum likelihood), \cr 
  \code{"qq.reg"} (quantile-quantile regression), \cr
  \code{"qq.reg.w.cen.level"} (quantile-quantile regression including the 
  censoring level), \cr
  \code{"impute.w.qq.reg"} (moment estimation based on imputation using the 
  \code{qq.reg method}), \cr
  \code{"impute.w.qq.reg.w.cen.level"} (moment estimation based on imputation 
  using the \code{qq.reg.w.cen.level method}), \cr
  \code{"impute.w.mle"} (moment estimation based on imputation using the mle), \cr
  \code{"iterative.impute.w.qq.reg"} (moment estimation based on iterative 
  imputation using the \code{qq.reg method}), \cr
  \code{"m.est"} (robust M-estimation), and \cr
  \code{"half.cen.level"} (moment estimation based on setting the censored 
  observations to half the censoring level).  \cr

  For multiply censored data, the possible values are: \cr
  \code{"mle"} (maximum likelihood; the default), \cr
  \code{"qq.reg"} (quantile-quantile regression), \cr
  \code{"impute.w.qq.reg"} (moment estimation based on imputation using the 
  \code{qq.reg method}), and \cr
  \code{"half.cen.level"} (moment estimation based on setting the censored 
  observations to half the censoring level). \cr

  See the DETAILS section for more information.
}
  \item{ci}{
  logical scalar indicating whether to compute a confidence interval for the quantile.  
  The default value is \code{ci=FALSE}.
}
  \item{ci.method}{
  character string indicating what method to use to construct the confidence interval 
  for the quantile.  The possible values are: \cr 
  \code{"exact.for.complete"} (exact method for complete (i.e., uncensored) data; the default), \cr
  \code{"gpq"} (method based on generalized pivotal quantities), and \cr
  \code{"normal.approx"} (normal approximation).  

  See the DETAILS section for more information.  This argument is ignored if \cr
  \code{ci=FALSE}.
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
  \item{nmc}{
  numeric scalar indicating the number of Monte Carlo simulations to run when 
  \code{ci.method="gpq"}.  The default is \code{nmc=1000}.  This argument is ignored 
  if \code{ci=FALSE}.
}
  \item{seed}{
  integer supplied to the function \code{\link[base]{set.seed}} and used when 
  \code{ci.method="gpq"}.  The default value is 
  \code{seed=NULL}, in which case the current value of \code{.Random.seed} is used.  
  This argument is ignored when \code{ci=FALSE}.
}
}
\details{
  \bold{Estimating Quantiles} \cr
  Quantiles are estimated by:
  \enumerate{
    \item estimating the mean and standard deviation parameters by 
      calling \code{\link{enormCensored}}, and then 
    \item calling the function \code{\link[=Normal]{qnorm}} and using the estimated 
      values for the mean and standard deviation.  
  }
  The estimated quantile thus depends on the method of estimating the mean and 
  standard deviation.
  \cr

  \bold{Confidence Intervals for Quantiles} \cr

  \emph{Exact Method When Data are Complete} (\code{ci.method="exact.for.complete"}) \cr
  When \code{ci.method="exact.for.complete"}, the function \code{eqnormCensored} 
  calls the function \code{\link{eqnorm}}, supplying it with the estimated mean 
  and standard deviation, and setting the argument \cr
  \code{ci.method="exact"}.  Thus, this 
  is the exact method for computing a confidence interval for a quantile had the data 
  been complete.  Because the data have been subjected to Type I censoring, this method 
  of constructing a confidence interval for the quantile is an approximation.
  \cr

  \emph{Normal Approximation} (\code{ci.method="normal.approx"}) \cr
  When \code{ci.method="normal.approx"}, the function \code{eqnormCensored} 
  calls the function \code{\link{eqnorm}}, supplying it with the estimated mean 
  and standard deviation, and setting the argument \cr 
  \code{ci.method="normal.approx"}.  
  Thus, this is the normal approximation method for computing a confidence interval 
  for a quantile had the data been complete.  Because the data have been subjected 
  to Type I censoring, this method of constructing a confidence interval for the 
  quantile is an approximation both because of the normal approximation and because 
  the estimates of the mean and standard devation are based on censored, instead of 
  complete, data.
  \cr

  \emph{Generalized Pivotal Quantity} (\code{ci.method="gpq"}) \cr
  When \code{ci.method="gpq"}, the function \code{eqnormCensored} uses the 
  relationship between confidence intervals for quantiles and tolerance intervals 
  and calls the function \code{\link{tolIntNormCensored}} with the argument 
  \code{ti.method="gpq"} to construct the confidence interval.  
  Specifically, it can be shown 
  (e.g., Conover, 1980, pp.119-121) that an upper confidence interval for the 
  \eqn{p}'th quantile with confidence level \eqn{100(1-\alpha)\%} is equivalent to 
  an upper \eqn{\beta}-content tolerance interval with coverage \eqn{100p\%} and 
  confidence level \eqn{100(1-\alpha)\%}.  Also, a lower confidence interval for 
  the \eqn{p}'th quantile with confidence level \eqn{100(1-\alpha)\%} is equivalent 
  to a lower \eqn{\beta}-content tolerance interval with coverage \eqn{100(1-p)\%} and 
  confidence level \eqn{100(1-\alpha)\%}.
}
\value{
  \code{eqnormCensored} returns a list of class \code{"estimateCensored"} 
  containing the estimated quantile(s) and other information.  
  See \code{\link{estimateCensored.object}} for details.
}
\references{
  Berthouex, P.M., and L.C. Brown. (2002). \emph{Statistics for Environmental Engineers}. 
  Lewis Publishers, Boca Raton.

  Caudill, S.P., L.-Y. Wong, W.E. Turner, R. Lee, A. Henderson, D. G. Patterson Jr. 
  (2007).  Percentile Estimation Using Variable Censored Data.  
  \emph{Chemosphere} \bold{68}, 169--180.

  Conover, W.J. (1980). \emph{Practical Nonparametric Statistics}. Second Edition. 
  John Wiley and Sons, New York.

  Draper, N., and H. Smith. (1998). \emph{Applied Regression Analysis}. Third Edition. 
  John Wiley and Sons, New York.

  Ellison, B.E. (1964). On Two-Sided Tolerance Intervals for a Normal Distribution. 
  \emph{Annals of Mathematical Statistics} \bold{35}, 762-772.

  Gibbons, R.D., D.K. Bhaumik, and S. Aryal. (2009). 
  \emph{Statistical Methods for Groundwater Monitoring}, Second Edition.  
  John Wiley & Sons, Hoboken.

  Gilbert, R.O. (1987). \emph{Statistical Methods for Environmental Pollution Monitoring}. 
  Van Nostrand Reinhold, New York, NY, pp.132-136.

  Guttman, I. (1970). \emph{Statistical Tolerance Regions: Classical and Bayesian}. 
  Hafner Publishing Co., Darien, CT.

  Hahn, G.J. (1970b). Statistical Intervals for a Normal Population, Part I: Tables, Examples 
  and Applications. \emph{Journal of Quality Technology} \bold{2}(3), 115-125.

  Hahn, G.J. (1970c). Statistical Intervals for a Normal Population, Part II: Formulas, Assumptions, 
  Some Derivations. \emph{Journal of Quality Technology} \bold{2}(4), 195-206.

  Hahn, G.J., and W.Q. Meeker. (1991). \emph{Statistical Intervals: A Guide for Practitioners}. 
  John Wiley and Sons, New York.

  Helsel, D.R., and R.M. Hirsch. (1992). \emph{Statistical Methods in Water Resources Research}. 
  Elsevier, New York, NY, pp.88-90.

  Hewett, P., and G.H. Ganser. (2007).  A Comparison of Several Methods for Analyzing 
  Censored Data.  \emph{Annals of Occupational Hygiene} \bold{51}(7), 611--632.

  Johnson, N.L., and B.L. Welch. (1940). Applications of the Non-Central t-Distribution. 
  \emph{Biometrika} \bold{31}, 362-389.

  Krishnamoorthy K., and T. Mathew. (2009). 
  \emph{Statistical Tolerance Regions: Theory, Applications, and Computation}. 
  John Wiley and Sons, Hoboken.

  Kroll, C.N., and J.R. Stedinger. (1996).  Estimation of Moments and Quantiles 
  Using Censored Data.  \emph{Water Resources Research} \bold{32}(4), 1005--1012.

  Millard, S.P., and N.K. Neerchal. (2001). \emph{Environmental Statistics with S-PLUS}. 
  CRC Press, Boca Raton.

  Odeh, R.E., and D.B. Owen. (1980). \emph{Tables for Normal Tolerance Limits, Sampling Plans, 
  and Screening}. Marcel Dekker, New York.

  Owen, D.B. (1962). \emph{Handbook of Statistical Tables}. Addison-Wesley, Reading, MA.

  Serasinghe, S.K. (2010).  \emph{A Simulation Comparison of Parametric and 
  Nonparametric Estimators of Quantiles from Right Censored Data}.  A Report 
  submitted in partial fulfillment of the requirements for the degree Master of 
  Science, Department of Statistics, College of Arts and Sciences, Kansas State 
  University, Manhattan, Kansas.


  Singh, A., R. Maichle, and S. Lee. (2006).  \emph{On the Computation of a 95\% 
  Upper Confidence Limit of the Unknown Population Mean Based Upon Data Sets 
  with Below Detection Limit Observations}.  EPA/600/R-06/022, March 2006.  
  Office of Research and Development, U.S. Environmental Protection Agency, 
  Washington, D.C.

  Singh, A., R. Maichle, and N. Armbya. (2010a). 
  \emph{ProUCL Version 4.1.00 User Guide (Draft)}. EPA/600/R-07/041, May 2010. 
  Office of Research and Development, U.S. Environmental Protection Agency, Washington, D.C.

  Singh, A., N. Armbya, and A. Singh. (2010b). 
  \emph{ProUCL Version 4.1.00 Technical Guide (Draft)}. EPA/600/R-07/041, May 2010.  
  Office of Research and Development, U.S. Environmental Protection Agency, Washington, D.C.

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

  Wald, A., and J. Wolfowitz. (1946). Tolerance Limits for a Normal Distribution. 
  \emph{Annals of Mathematical Statistics} \bold{17}, 208-215.
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

  A sample of data contains censored observations if some of the observations are 
  reported only as being below or above some censoring level.  In environmental 
  data analysis, Type I left-censored data sets are common, with values being 
  reported as \dQuote{less than the detection limit} (e.g., Helsel, 2012).  Data 
  sets with only one censoring level are called \emph{singly censored}; data sets with 
  multiple censoring levels are called \emph{multiply} or \emph{progressively censored}.

  Statistical methods for dealing with censored data sets have a long history in the 
  field of survival analysis and life testing.  More recently, researchers in the 
  environmental field have proposed alternative methods of computing estimates and 
  confidence intervals in addition to the classical ones such as maximum likelihood 
  estimation.

  Helsel (2012, Chapter 6) gives an excellent review of past studies of the 
  properties of various estimators based on censored environmental data.

  In practice, it is better to use a confidence interval for a percentile, rather 
  than rely on a single point-estimate of percentile.  Confidence intervals 
  for percentiles of a normal distribution depend on the properties of the estimators 
  for both the mean and standard deviation.

  Few studies have been done to evaluate the performance of methods for constructing 
  confidence intervals for the mean or joint confidence regions for the mean and 
  standard deviation when data are subjected to single or multiple censoring (see, 
  for example, Singh et al., 2006).  Studies to evaluate the performance of a 
  confidence interval for a percentile include:  Caudill et al. (2007), Hewett and 
  Ganner (2007), Kroll and Stedinger (1996), and Serasinghe (2010).
}
\seealso{
  \code{\link{enormCensored}}, \code{\link{tolIntNormCensored}}, \code{\link{Normal}},  
  \code{\link{estimateCensored.object}}.
}
\examples{
  # Generate 15 observations from a normal distribution with 
  # parameters mean=10 and sd=2, and censor observations less than 8. 
  # Then generate 15 more observations from this distribution and  censor 
  # observations less than 7.
  # Then estimate the 90th percentile and create a one-sided upper 95% 
  # confidence interval for that percentile. 
  # (Note: the call to set.seed simply allows you to reproduce this example.)

  set.seed(47) 

  x.1 <- rnorm(15, mean = 10, sd = 2) 
  sort(x.1)
  # [1]  6.343542  7.068499  7.828525  8.029036  8.155088  9.436470
  # [7]  9.495908 10.030262 10.079205 10.182946 10.217551 10.370811
  #[13] 10.987640 11.422285 13.989393
  censored.1 <- x.1 < 8
  x.1[censored.1] <- 8

  x.2 <- rnorm(15, mean = 10, sd = 2) 
  sort(x.2)
  # [1]  5.355255  6.065562  6.783680  6.867676  8.219412  8.593224
  # [7]  9.319168  9.347066  9.837844  9.918844 10.055054 10.498296
  #[13] 10.834382 11.341558 12.528482
  censored.2 <- x.2 < 7
  x.2[censored.2] <- 7

  x <- c(x.1, x.2)
  censored <- c(censored.1, censored.2)

  eqnormCensored(x, censored, p = 0.9, ci = TRUE, ci.type = "upper")

  #Results of Distribution Parameter Estimation
  #Based on Type I Censored Data
  #--------------------------------------------
  #
  #Assumed Distribution:            Normal
  #
  #Censoring Side:                  left
  #
  #Censoring Level(s):              7 8 
  #
  #Estimated Parameter(s):          mean = 9.390624
  #                                 sd   = 1.827156
  #
  #Estimation Method:               MLE
  #
  #Estimated Quantile(s):           90'th %ile = 11.73222
  #
  #Quantile Estimation Method:      Quantile(s) Based on
  #                                 MLE Estimators
  #
  #Data:                            x
  #
  #Censoring Variable:              censored
  #  
  #Sample Size:                     30
  #
  #Percent Censored:                16.66667%
  #
  #Confidence Interval for:         90'th %ile
  #
  #Assumed Sample Size:             30
  #
  #Confidence Interval Method:      Exact for
  #                                 Complete Data
  #
  #Confidence Interval Type:        upper
  #
  #Confidence Level:                95%
  #
  #Confidence Interval:             LCL =     -Inf
  #                                 UCL = 12.63808

  #----------

  # Compare these results with the true 90'th percentile:

  qnorm(p = 0.9, mean = 10, sd = 2)
  #[1] 12.56310

  #----------

  # Clean up
  rm(x.1, censored.1, x.2, censored.2, x, censored)
  
  #==========

  # Chapter 15 of USEPA (2009) gives several examples of estimating the mean
  # and standard deviation of a lognormal distribution on the log-scale using 
  # manganese concentrations (ppb) in groundwater at five background wells. 
  # In EnvStats these data are stored in the data frame 
  # EPA.09.Ex.15.1.manganese.df.

  # Here we will estimate the mean and standard deviation using the MLE, 
  # and then construct an upper 95% confidence limit for the 90th percentile. 

  # We will log-transform the original observations and then call 
  # eqnormCensored.  Alternatively, we could have more simply called 
  # eqlnormCensored.

  # First look at the data:
  #-----------------------

  EPA.09.Ex.15.1.manganese.df

  #   Sample   Well Manganese.Orig.ppb Manganese.ppb Censored
  #1       1 Well.1                 <5           5.0     TRUE
  #2       2 Well.1               12.1          12.1    FALSE
  #3       3 Well.1               16.9          16.9    FALSE
  #...
  #23      3 Well.5                3.3           3.3    FALSE
  #24      4 Well.5                8.4           8.4    FALSE
  #25      5 Well.5                 <2           2.0     TRUE

  longToWide(EPA.09.Ex.15.1.manganese.df, 
    "Manganese.Orig.ppb", "Sample", "Well",
    paste.row.name = TRUE)  

  #         Well.1 Well.2 Well.3 Well.4 Well.5
  #Sample.1     <5     <5     <5    6.3   17.9
  #Sample.2   12.1    7.7    5.3   11.9   22.7
  #Sample.3   16.9   53.6   12.6     10    3.3
  #Sample.4   21.6    9.5  106.3     <2    8.4
  #Sample.5     <2   45.9   34.5   77.2     <2


  # Now estimate the mean, standard deviation, and 90th percentile 
  # on the log-scale using the MLE, and construct an upper 95% 
  # confidence limit for the 90th percentile on the log-scale:
  #---------------------------------------------------------------

  est.list <- with(EPA.09.Ex.15.1.manganese.df,
    eqnormCensored(log(Manganese.ppb), Censored, 
      p = 0.9, ci = TRUE, ci.type = "upper"))

  est.list

  #Results of Distribution Parameter Estimation
  #Based on Type I Censored Data
  #--------------------------------------------
  #
  #Assumed Distribution:            Normal
  #
  #Censoring Side:                  left
  #
  #Censoring Level(s):              0.6931472 1.6094379 
  #
  #Estimated Parameter(s):          mean = 2.215905
  #                                 sd   = 1.356291
  #
  #Estimation Method:               MLE
  #
  #Estimated Quantile(s):           90'th %ile = 3.954062
  #
  #Quantile Estimation Method:      Quantile(s) Based on
  #                                 MLE Estimators
  #
  #Data:                            log(Manganese.ppb)
  #
  #Censoring Variable:              censored
  #
  #Sample Size:                     25
  #
  #Percent Censored:                24%
  #
  #Confidence Interval for:         90'th %ile
  #
  #Assumed Sample Size:             25
  #
  #Confidence Interval Method:      Exact for
  #                                 Complete Data
  #
  #Confidence Interval Type:        upper
  #
  #Confidence Level:                95%
  #
  #Confidence Interval:             LCL =     -Inf
  #                                 UCL = 4.708904

  # To estimate the 90th percentile on the original scale, 
  # we need to exponentiate the results
  #-------------------------------------------------------
  exp(est.list$quantiles)
  #90'th %ile 
  #  52.14674 

  exp(est.list$interval$limits)
  #     LCL      UCL 
  #  0.0000 110.9305

  #----------

  # Clean up
  #---------
  rm(est.list)
}
\keyword{ distribution }
\keyword{ htest }
