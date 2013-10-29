\name{eqzmnorm}
\alias{eqzmnorm}
\title{
  Estimate Quantiles of a Zero-Modified Normal Distribution
}
\description{
  Estimate quantiles of a 
  \link[=ZeroModifiedNormal]{zero-modified normal distribution}.
}
\usage{
  eqzmnorm(x, p = 0.5, method = "mvue", digits = 0)
}
\arguments{
  \item{x}{
  a numeric vector of observations, or an object resulting from a call to an 
  estimating function that assumes a zero-modified normal distribution 
  (e.g., \code{\link{ezmnorm}}).  If \code{x} is a numeric vector, 
  missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) 
  values are allowed but will be removed.
}
  \item{p}{
  numeric vector of probabilities for which quantiles will be estimated.  
  All values of \code{p} must be between 0 and 1.  The default value is \code{p=0.5}.
}
  \item{method}{
  character string specifying the method of estimating the disribution parameters.  
  Currently, the only possible 
  value is \code{"mvue"} (minimum variance unbiased; the default).  See the DETAILS 
  section of the help file for \code{\link{ezmnorm}} for more information.
}
  \item{digits}{
  an integer indicating the number of decimal places to round to when printing out 
  the value of \code{100*p}. The default value is \code{digits=0}.
}
}
\details{
  The function \code{eqzmnorm} returns estimated quantiles as well as 
  estimates of the distribution parameters.  

  Quantiles are estimated by 1) estimating the distribution parameters by 
  calling \code{\link{ezmnorm}}, and then 2) calling the function 
  \code{\link[=ZeroModifiedNormal]{qzmnorm}} and using the estimated values for 
  the distribution parameters.
}
\value{
  If \code{x} is a numeric vector, \code{eqzmnorm} returns a 
  list of class \code{"estimate"} containing the estimated quantile(s) and other 
  information. See \code{\link{estimate.object}} for details.

  If \code{x} is the result of calling an estimation function, \code{eqzmnorm} 
  returns a list whose class is the same as \code{x}.  The list 
  contains the same components as \code{x}, as well as components called 
  \code{quantiles} and \code{quantile.method}.
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
  # with mean=4, sd=2, and p.zero=0.5, then estimate the parameters and 
  # the 80th and 90th percentiles.  
  # (Note: the call to set.seed simply allows you to reproduce this example.)

  set.seed(250) 
  dat <- rzmnorm(100, mean = 4, sd = 2, p.zero = 0.5) 
  eqzmnorm(dat, p = c(0.8, 0.9)) 

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
  #Estimated Quantile(s):           80'th %ile = 4.706298
  #                                 90'th %ile = 5.779250
  #
  #Quantile Estimation Method:      Quantile(s) Based on
  #                                 mvue Estimators
  #
  #Data:                            dat
  #
  #Sample Size:                     100

  #----------

  # Compare the estimated quantiles with the true quantiles

  qzmnorm(mean = 4, sd = 2, p.zero = 0.5, p = c(0.8, 0.9))
  #[1] 4.506694 5.683242

  #----------

  # Clean up
  rm(dat)
}
\keyword{ distribution }
\keyword{ htest }
