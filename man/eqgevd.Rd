\name{eqgevd}
\alias{eqgevd}
\title{
  Estimate Quantiles of a Generalized Extreme Value Distribution
}
\description{
  Estimate quantiles of a \link[=GEVD]{generalized extreme value distribution}.
}
\usage{
  eqgevd(x, p = 0.5, method = "mle", pwme.method = "unbiased", 
    tsoe.method = "med", plot.pos.cons = c(a = 0.35, b = 0), digits = 0)
}
\arguments{
  \item{x}{
  a numeric vector of observations, or an object resulting from a call to an 
  estimating function that assumes a generalized extreme value distribution 
  (e.g., \code{\link{egevd}}). If \code{x} is a numeric vector, 
  missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) 
  values are allowed but will be removed.
}
  \item{p}{
  numeric vector of probabilities for which quantiles will be estimated.  
  All values of \code{p} must be between 0 and 1.  The default value is \code{p=0.5}.
}
  \item{method}{
  character string specifying the method to use to estimate the location, scale, and 
  threshold parameters.  Possible values are 
  \code{"mle"} (maximum likelihood; the default), 
  \code{"pwme"} (probability-weighted moments), and 
  \code{"tsoe"} (two-stage order-statistics estimator of Castillo and Hadi (1994)).  
  See the DETAILS section of the help file for \code{\link{egevd}} for more 
  information on these estimation methods.
}
  \item{pwme.method}{
  character string specifying what method to use to compute the 
  probability-weighted moments when \code{method="pwme"}.  The possible values are 
  \code{"ubiased"} (method based on the U-statistic; the default), or 
  \code{"plotting.position"} (method based on the plotting position formula).  
  See the DETAILS section of the help file for \code{\link{egevd}}  for more 
  information.  This argument is ignored if \code{method} is not equal to 
  \code{"pwme"}.
}
  \item{tsoe.method}{
  character string specifying the robust function to apply in the second stage of 
  the two-stage order-statistics estimator when \code{method="tsoe"}.  Possible 
  values are \code{"med"} (median; the default), and \code{"lms"} 
  (least median of squares).  See the DETAILS section of the help file for 
  \code{\link{egevd}} for more information on these estimation methods.  
  This argument is ignored if \code{method} is not equal to \code{"tsoe"}.
}
  \item{plot.pos.cons}{
  numeric vector of length 2 specifying the constants used in the formula for the 
  plotting positions when \code{method="pwme"} and \cr
  \code{pwme.method="plotting.position"}.  The default value is \cr
  \code{plot.pos.cons=c(a=0.35, b=0)}.  If this vector has a names attribute with 
  the value \code{c("a","b")} or \code{c("b","a")}, then the elements will be 
  matched by name in the formula for computing the plotting positions.  Otherwise, 
  the first element is mapped to the name \code{"a"} and the second element to the 
  name \code{"b"}.  See the DETAILS section of the help file for \code{\link{egevd}} 
  for more information.  This argument is used only if \code{method="tsoe"}, or if 
  both \code{method="pwme"} and \cr
  \code{pwme.method="plotting.position"}.
}
  \item{digits}{
  an integer indicating the number of decimal places to round to when printing out 
  the value of \code{100*p}. The default value is \code{digits=0}.
}
}
\details{
  The function \code{eqgevd} returns estimated quantiles as well as 
  estimates of the location, scale and threshold parameters.  

  Quantiles are estimated by 1) estimating the location, scale, and threshold 
  parameters by calling \code{\link{egevd}}, and then 2) calling the function 
  \code{\link[=GEVD]{qgevd}} and using the estimated values for 
  location, scale, and threshold.
}
\value{
  If \code{x} is a numeric vector, \code{eqevd} returns a 
  list of class \code{"estimate"} containing the estimated quantile(s) and other 
  information. See \code{\link{estimate.object}} for details.

  If \code{x} is the result of calling an estimation function, \code{eqevd} 
  returns a list whose class is the same as \code{x}.  The list 
  contains the same components as \code{x}, as well as components called 
  \code{quantiles} and \code{quantile.method}.
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
  \code{\link{egevd}}, \link[=GEVD]{Generalized Extreme Value Distribution}, 
  \link[=EVD]{Extreme Value Distribution}, \code{\link{eevd}}, \cr
  \code{\link{estimate.object}}.
}
\examples{
  # Generate 20 observations from a generalized extreme value distribution 
  # with parameters location=2, scale=1, and shape=0.2, then compute the 
  # MLEs of location, shape,and threshold, and estimate the 90th percentile. 
  # (Note: the call to set.seed simply allows you to reproduce this example.)

  set.seed(498) 
  dat <- rgevd(20, location = 2, scale = 1, shape = 0.2) 
  eqgevd(dat, p = 0.9)

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
  #Estimated Quantile(s):           90'th %ile = 3.289912
  #
  #Quantile Estimation Method:      Quantile(s) Based on
  #                                 mle Estimators
  #
  #Data:                            dat
  #
  #Sample Size:                     20

  #----------

  # Clean up
  #---------
  rm(dat)
}
\keyword{ distribution }
\keyword{ htest }
