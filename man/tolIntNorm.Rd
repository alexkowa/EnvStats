\name{tolIntNorm}
\alias{tolIntNorm}
\title{
  Tolerance Interval for a Normal Distribution
}
\description{
  Construct a \eqn{\beta}-content or \eqn{\beta}-expectation tolerance 
  interval for a \link[stats:Normal]{normal distribution}.
}
\usage{
  tolIntNorm(x, coverage = 0.95, cov.type = "content", 
    ti.type = "two-sided", conf.level = 0.95, method = "exact")
}
\arguments{
  \item{x}{
  numeric vector of observations, or an object resulting from a call to an 
  estimating function that assumes a normal (Gaussian) distribution 
  (i.e., \code{\link{enorm}} or \code{\link{enormCensored}}).  
  If \code{x} is a numeric vector, 
  missing (\code{NA}), undefined (\code{NaN}), and 
  infinite (\code{Inf}, \code{-Inf}) values are allowed but will be removed.
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
  a scalar between 0 and 1 indicating the confidence level associated with the tolerance 
  interval.  The default value is \code{conf.level=0.95}.
}
  \item{method}{
  for the case of a two-sided tolerance interval, a character string specifying the method for 
  constructing the tolerance interval.  This argument is ignored if \code{ti.type="lower"} or 
  \code{ti.type="upper"}.  The possible values are \cr
  \code{"exact"} (the default) and \code{"wald.wolfowitz"} (the Wald-Wolfowitz approximation).  
  See the DETAILS section for more information.
}
}
\details{
  If \code{x} contains any missing (\code{NA}), undefined (\code{NaN}) or 
  infinite (\code{Inf}, \code{-Inf}) values, they will be removed prior to 
  performing the estimation.

  A tolerance interval for some population is an interval on the real line constructed so as to 
  contain \eqn{100 \beta \%} of the population (i.e., \eqn{100 \beta \%} of all 
  future observations), where \eqn{0 < \beta < 1}.  The quantity \eqn{100 \beta \%} is called 
  the \emph{coverage}.

  There are two kinds of tolerance intervals (Guttman, 1970):
  \itemize{ 
    \item A \eqn{\beta}-content tolerance interval with confidence level \eqn{100(1-\alpha)\%} is 
    constructed so that it contains at least \eqn{100 \beta \%} of the population (i.e., the 
    coverage is at least \eqn{100 \beta \%}) with probability \eqn{100(1-\alpha)\%}, where 
    \eqn{0 < \alpha < 1}. The quantity \eqn{100(1-\alpha)\%} is called the confidence level or 
    confidence coefficient associated with the tolerance interval.

    \item A \eqn{\beta}-expectation tolerance interval is constructed so that the \emph{average} coverage of 
    the interval is \eqn{100 \beta \%}.
  } 

  \bold{Note:} A \eqn{\beta}-expectation tolerance interval with coverage \eqn{100 \beta \%} is 
  equivalent to a prediction interval for one future observation with associated confidence level 
  \eqn{100 \beta \%}.  Note that there is no explicit confidence level associated with a 
  \eqn{\beta}-expectation tolerance interval.  If a \eqn{\beta}-expectation tolerance interval is 
  treated as a \eqn{\beta}-content tolerance interval, the confidence level associated with this 
  tolerance interval is usually around 50\% (e.g., Guttman, 1970, Table 4.2, p.76).  

  For a normal distribution, the form of a two-sided \eqn{100(1-\alpha)\%} tolerance 
  interval is: 
  \deqn{[\bar{x} - Ks, \, \bar{x} + Ks]} 
  where \eqn{\bar{x}} denotes the sample 
  mean, \eqn{s} denotes the sample standard deviation, and \eqn{K} denotes a constant 
  that depends on the sample size \eqn{n}, the coverage, and, for a \eqn{\beta}-content 
  tolerance interval (but not a \eqn{\beta}-expectation tolerance interval), the 
  confidence level.  

  Similarly, the form of a one-sided lower tolerance interval is: 
  \deqn{[\bar{x} - Ks, \, \infty]} 
  and the form of a one-sided upper tolerance interval is:
  \deqn{[-\infty, \, \bar{x} + Ks]} 
  but \eqn{K} differs for one-sided versus two-sided tolerance intervals.  
  The derivation of the constant \eqn{K} is explained in the help file for 
  \code{\link{tolIntNormK}}.
}
\value{
  If \code{x} is a numeric vector, \code{tolIntNorm} returns a list of class 
  \code{"estimate"} containing the estimated parameters, a component called 
  \code{interval} containing the tolerance interval information, and other 
  information.  See \code{\link{estimate.object}} for details.

  If \code{x} is the result of calling an estimation function, \code{tolIntNorm} 
  returns a list whose class is the same as \code{x}.  The list contains the same 
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
  life testing problems (Hahn, 1970b,c; Hahn and Meeker, 1991; Krishnamoorthy and Mathew, 2009).  
  References that discuss tolerance intervals in the context of environmental monitoring include:  
  Berthouex and Brown (2002, Chapter 21), Gibbons et al. (2009), 
  Millard and Neerchal (2001, Chapter 6), Singh et al. (2010b), and USEPA (2009).
}
\seealso{
  \code{\link{tolIntNormK}}, \code{\link{tolIntLnorm}}, \code{\link{Normal}}, 
  \code{\link{estimate.object}}, \code{\link{enorm}}, \code{\link{eqnorm}}, 
  \code{\link{predIntNorm}}, \link{Tolerance Intervals}, 
  \link{Estimating Distribution Parameters}, \link{Estimating Distribution Quantiles}.
}
\examples{
  # Generate 20 observations from a normal distribution with parameters 
  # mean=10 and sd=2, then create a tolerance interval. 
  # (Note: the call to set.seed simply allows you to reproduce this 
  # example.)

  set.seed(250) 
  dat <- rnorm(20, mean = 10, sd = 2) 
  tolIntNorm(dat)

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            Normal
  #
  #Estimated Parameter(s):          mean = 9.861160
  #                                 sd   = 1.180226
  #
  #Estimation Method:               mvue
  #
  #Data:                            dat
  #
  #Sample Size:                     20
  #
  #Tolerance Interval Coverage:     95%
  #
  #Coverage Type:                   content
  #
  #Tolerance Interval Method:       Exact
  #
  #Tolerance Interval Type:         two-sided
  #
  #Confidence Level:                95%
  #
  #Tolerance Interval:              LTL =  6.603328
  #                                 UTL = 13.118993

  #----------

  # Clean up
  rm(dat)
  
  #--------------------------------------------------------------------

  # Example 17-3 of USEPA (2009, p. 17-17) shows how to construct a 
  # beta-content upper tolerance limit with 95% coverage and 95% 
  # confidence  using chrysene data and assuming a lognormal distribution.  
  # The data for this example are stored in EPA.09.Ex.17.3.chrysene.df, 
  # which contains chrysene concentration data (ppb) found in water 
  # samples obtained from two background wells (Wells 1 and 2) and 
  # three compliance wells (Wells 3, 4, and 5).  The tolerance limit 
  # is based on the data from the background wells.

  # Here we will first take the log of the data and  
  # then construct the tolerance interval; note however that it is 
  # easier to call the function tolIntLnorm instead using the 
  # original data.

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

  tol.int.list <- with(EPA.09.Ex.17.3.chrysene.df, 
    tolIntNorm(log(Chrysene.ppb[Well.type == "Background"]), 
    ti.type = "upper", coverage = 0.95, conf.level = 0.95))

  tol.int.list

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
  #Data:                            log(Chrysene.ppb[Well.type == "Background"])
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
  #Tolerance Interval:              LTL =     -Inf
  #                                 UTL = 4.510032

  # Compute the upper tolerance interaval on the original scale
  # by exponentiating the upper tolerance limit:

  exp(tol.int.list$interval$limits["UTL"])
  #    UTL 
  #90.9247

  #----------

  # Clean up

  rm(tol.int.list)
}
\keyword{ distribution }
\keyword{ htest }
