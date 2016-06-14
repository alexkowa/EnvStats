\name{tolIntLnorm}
\alias{tolIntLnorm}
\alias{tolIntLnormAlt}
\title{
  Tolerance Interval for a Lognormal Distribution
}
\description{
  Estimate the mean and standard deviation on the log-scale for a 
  \link[stats:Lognormal]{lognormal distribution}, or estimate the mean 
  and coefficient of variation for a 
  \link[=LognormalAlt]{lognormal distribution (alternative parameterization)}, 
  and construct a \eqn{\beta}-content or \eqn{\beta}-expectation tolerance 
  interval.
}
\usage{
  tolIntLnorm(x, coverage = 0.95, cov.type = "content", ti.type = "two-sided", 
    conf.level = 0.95, method = "exact")

  tolIntLnormAlt(x, coverage = 0.95, cov.type = "content", ti.type = "two-sided", 
    conf.level = 0.95, method = "exact", est.method = "mvue")
}
\arguments{
  \item{x}{
  For \code{tolIntLnorm}, \code{x} can be a numeric vector of positive observations, 
  or an object resulting from a call to an estimating function that assumes a 
  lognormal distribution (i.e., \code{\link{elnorm}} or \code{\link{elnormCensored}}).  
  You \emph{cannot} supply objects resulting from a call to estimating functions that 
  use the alternative parameterization such as \code{\link{elnormAlt}} or 
  \code{\link{elnormAltCensored}}.

  For \code{tolIntLnormAlt}, a numeric vector of positive observations.  

  If \code{x} is a numeric vector, 
  missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) 
  values are allowed but will be removed.
}
  \item{coverage}{
  a scalar between 0 and 1 indicating the desired coverage of the tolerance interval.  
  The default value is \code{coverage=0.95}.  If \code{cov.type="expectation"}, 
  this argument is ignored.
}
  \item{cov.type}{
  character string specifying the coverage type for the tolerance interval.  
  The possible values are \code{"content"} (\eqn{\beta}-content; the default), and 
  \code{"expectation"} (\eqn{\beta}-expectation).  See the DETAILS section for more 
  information.
}
  \item{ti.type}{
  character string indicating what kind of tolerance interval to compute.  
  The possible values are \code{"two-sided"} (the default), \code{"lower"}, and 
  \code{"upper"}.
}
  \item{conf.level}{
  a scalar between 0 and 1 indicating the confidence level associated with the 
  tolerance interval.  The default value is \code{conf.level=0.95}.
}
  \item{method}{
  for the case of a two-sided tolerance interval, a character string specifying the 
  method for constructing the tolerance interval.  This argument is ignored if 
  \code{ti.type="lower"} or \code{ti.type="upper"}.  The possible values are \cr
  \code{"exact"} (the default) and \code{"wald.wolfowitz"} (the Wald-Wolfowitz 
  approximation).  See the DETAILS section in this help file and the DETAILS section 
  in the help file for \code{\link{tolIntNorm}} for more information.
}
  \item{est.method}{
  for \code{tolIntLnormAlt}, a character string specifying the method of estimating 
  the mean and coefficient of variation.  \emph{This argument has no effect on the 
  method of constructing the tolerance interval}.  Possible values are 
  \code{"mvue"} (minimum variance unbiased; the default), 
  \code{"qmle"} (quasi maximum likelihood), \code{"mle"} (maximum likelihood), 
  \code{"mme"} (method of moments), and \code{"mmue"} 
  (method of moments based on the unbiased estimate of variance).  See the DETAILS 
  section of \code{\link{elnormAlt}} for more information on these estimation methods.
}
}
\details{
  The function \code{tolIntLnorm} returns a tolerance interval as well as 
  estimates of the meanlog and sdlog parameters.  
  The function \code{tolIntLnormAlt} returns a tolerance interval as well as 
  estimates of the mean and coefficient of variation.

  A tolerance interval for a lognormal distribution is constructed by taking the 
  natural logarithm of the observations and constructing a tolerance interval 
  based on the normal (Gaussian) distribution by calling \code{\link{tolIntNorm}}.  
  These tolerance limits are then exponentiated to produce a tolerance interval on 
  the original scale of the data.
}
\value{
  If \code{x} is a numeric vector, a list of class 
  \code{"estimate"} containing the estimated parameters, a component called 
  \code{interval} containing the tolerance interval information, and other 
  information.  See \code{\link{estimate.object}} for details.

  If \code{x} is the result of calling an estimation function, a list whose 
  class is the same as \code{x}.  The list contains the same 
  components as \code{x}.  If \code{x} already has a component called 
  \code{interval}, this component is replaced with the tolerance interval 
  information.
}
\references{
  Berthouex, P.M., and L.C. Brown. (2002). \emph{Statistics for Environmental Engineers}. 
  Lewis Publishers, Boca Raton.

  Draper, N., and H. Smith. (1998). \emph{Applied Regression Analysis}. Third Edition. 
  John Wiley and Sons, New York.

  Ellison, B.E. (1964). On Two-Sided Tolerance Intervals for a Normal Distribution. 
  \emph{Annals of Mathematical Statistics} \bold{35}, 762-772.

  Gibbons, R.D., D.K. Bhaumik, and S. Aryal. (2009). 
  \emph{Statistical Methods for Groundwater Monitoring}, Second Edition.  
  John Wiley & Sons, Hoboken.

  Guttman, I. (1970). \emph{Statistical Tolerance Regions: Classical and Bayesian}. 
  Hafner Publishing Co., Darien, CT.

  Hahn, G.J. (1970b). Statistical Intervals for a Normal Population, Part I: Tables, Examples 
  and Applications. \emph{Journal of Quality Technology} \bold{2}(3), 115-125.

  Hahn, G.J. (1970c). Statistical Intervals for a Normal Population, Part II: Formulas, Assumptions, 
  Some Derivations. \emph{Journal of Quality Technology} \bold{2}(4), 195-206.

  Hahn, G.J., and W.Q. Meeker. (1991). \emph{Statistical Intervals: A Guide for Practitioners}. 
  John Wiley and Sons, New York.

  Krishnamoorthy K., and T. Mathew. (2009). 
  \emph{Statistical Tolerance Regions: Theory, Applications, and Computation}. 
  John Wiley and Sons, Hoboken.

  Millard, S.P., and N.K. Neerchal. (2001). \emph{Environmental Statistics with S-PLUS}. 
  CRC Press, Boca Raton.

  Odeh, R.E., and D.B. Owen. (1980). \emph{Tables for Normal Tolerance Limits, Sampling Plans, 
  and Screening}. Marcel Dekker, New York.

  Owen, D.B. (1962). \emph{Handbook of Statistical Tables}. Addison-Wesley, Reading, MA.

  Singh, A., R. Maichle, and N. Armbya. (2010a). 
  \emph{ProUCL Version 4.1.00 User Guide (Draft)}. EPA/600/R-07/041, May 2010. 
  Office of Research and Development, U.S. Environmental Protection Agency, Washington, D.C.

  Singh, A., N. Armbya, and A. Singh. (2010b). 
  \emph{ProUCL Version 4.1.00 Technical Guide (Draft)}. EPA/600/R-07/041, May 2010.  
  Office of Research and Development, U.S. Environmental Protection Agency, Washington, D.C.

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
  Tolerance intervals have long been applied to quality control and 
  life testing problems (Hahn, 1970b,c; Hahn and Meeker, 1991; 
  Krishnamoorthy and Mathew, 2009).  References that discuss tolerance intervals 
  in the context of environmental monitoring include:  
  Berthouex and Brown (2002, Chapter 21), Gibbons et al. (2009), 
  Millard and Neerchal (2001, Chapter 6), Singh et al. (2010b), and USEPA (2009).
}
\seealso{
  \code{\link{tolIntNorm}}, \code{\link{Lognormal}}, \code{\link{LognormalAlt}}, 
  \code{\link{estimate.object}}, \code{\link{elnorm}}, \code{\link{elnormAlt}}, 
  \code{\link{eqlnorm}}, \cr
  \code{\link{predIntLnorm}}, 
  \link{Tolerance Intervals}, \link{Estimating Distribution Parameters}, 
  \link{Estimating Distribution Quantiles}.
}
\examples{
  # Generate 20 observations from a lognormal distribution with parameters 
  # meanlog=0 and sdlog=1.  Use tolIntLnorm to estimate 
  # the mean and standard deviation of the log of the true distribution, and 
  # construct a two-sided 90\% beta-content tolerance interval with associated 
  # confidence level 95\%. 
  # (Note: the call to set.seed simply allows you to reproduce this example.)

  set.seed(250) 
  dat <- rlnorm(20) 
  tolIntLnorm(dat, coverage = 0.9) 

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            Lognormal
  #
  #Estimated Parameter(s):          meanlog = -0.06941976
  #                                 sdlog   =  0.59011300
  #
  #Estimation Method:               mvue
  #
  #Data:                            dat
  #
  #Sample Size:                     20
  #
  #Tolerance Interval Coverage:     90%
  #
  #Coverage Type:                   content
  #
  #Tolerance Interval Method:       Exact
  #
  #Tolerance Interval Type:         two-sided
  #
  #Confidence Level:                95%
  #
  #Tolerance Interval:              LTL = 0.237457
  #                                 UTL = 3.665369


  # The exact two-sided interval that contains 90\% of this distribution 
  # is given by: [0.193, 5.18].  

  qlnorm(p = c(0.05, 0.95))
  #[1] 0.1930408 5.1802516

  # Clean up
  rm(dat)

  #==========

  # Example 17-3 of USEPA (2009, p. 17-17) shows how to construct a 
  # beta-content upper tolerance limit with 95% coverage and 95% 
  # confidence  using chrysene data and assuming a lognormal distribution.  
  # The data for this example are stored in EPA.09.Ex.17.3.chrysene.df, 
  # which contains chrysene concentration data (ppb) found in water 
  # samples obtained from two background wells (Wells 1 and 2) and 
  # three compliance wells (Wells 3, 4, and 5).  The tolerance limit 
  # is based on the data from the background wells.

  head(EPA.09.Ex.17.3.chrysene.df)
  #  Month   Well  Well.type Chrysene.ppb
  #1     1 Well.1 Background         19.7
  #2     2 Well.1 Background         39.2
  #3     3 Well.1 Background          7.8
  #4     4 Well.1 Background         12.8
  #5     1 Well.2 Background         10.2
  #6     2 Well.2 Background          7.2

  longToWide(EPA.09.Ex.17.3.chrysene.df, "Chrysene.ppb", "Month", "Well")
  #  Well.1 Well.2 Well.3 Well.4 Well.5
  #1   19.7   10.2   68.0   26.8   47.0
  #2   39.2    7.2   48.9   17.7   30.5
  #3    7.8   16.1   30.1   31.9   15.0
  #4   12.8    5.7   38.1   22.2   23.4

  with(EPA.09.Ex.17.3.chrysene.df, 
    tolIntLnorm(Chrysene.ppb[Well.type == "Background"], 
    ti.type = "upper", coverage = 0.95, conf.level = 0.95))

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
  #Data:                            Chrysene.ppb[Well.type == "Background"]
  #
  #Sample Size:                     8
  #
  #Tolerance Interval Coverage:     95%
  #
  #Coverage Type:                   content
  #
  #Tolerance Interval Method:       Exact
  #
  #Tolerance Interval Type:         upper
  #
  #Confidence Level:                95%
  #
  #Tolerance Interval:              LTL =  0.0000
  #                                 UTL = 90.9247

  #----------

  # Repeat the above example, but estimate the mean and 
  # coefficient of variation on the original scale
  #-----------------------------------------------

  with(EPA.09.Ex.17.3.chrysene.df, 
    tolIntLnormAlt(Chrysene.ppb[Well.type == "Background"], 
    ti.type = "upper", coverage = 0.95, conf.level = 0.95))

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            Lognormal
  #
  #Estimated Parameter(s):          mean = 14.5547353
  #                                 cv   =  0.6390825
  #
  #Estimation Method:               mvue
  #
  #Data:                            Chrysene.ppb[Well.type == "Background"]
  #
  #Sample Size:                     8
  #
  #Tolerance Interval Coverage:     95%
  #
  #Coverage Type:                   content
  #
  #Tolerance Interval Method:       Exact
  #
  #Tolerance Interval Type:         upper
  #
  #Confidence Level:                95%
  #
  #Tolerance Interval:              LTL =  0.0000
  #                                 UTL = 90.9247
}
\keyword{ distribution }
\keyword{ htest }
