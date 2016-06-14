\name{eqevd}
\alias{eqevd}
\title{
  Estimate Quantiles of an Extreme Value (Gumbel) Distribution
}
\description{
  Estimate quantiles of an \link[=EVD]{extreme value distribution}.
}
\usage{
  eqevd(x, p = 0.5, method = "mle", pwme.method = "unbiased", 
    plot.pos.cons = c(a = 0.35, b = 0), digits = 0)
}
\arguments{
  \item{x}{
  a numeric vector of observations, or an object resulting from a call to an 
  estimating function that assumes an extreme value distribution 
  (e.g., \code{\link{eevd}}).  If \code{x} is a numeric vector, 
  missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) 
  values are allowed but will be removed.
}
  \item{p}{
  numeric vector of probabilities for which quantiles will be estimated.  
  All values of \code{p} must be between 0 and 1.  The default value is \code{p=0.5}.
}
  \item{method}{
  character string specifying the method to use to estimate the location and scale 
  parameters.  Possible values are 
  \code{"mle"} (maximum likelihood; the default), \code{"mme"} (methods of moments), 
  \code{"mmue"} (method of moments based on the unbiased estimator of variance), and 
  \code{"pwme"} (probability-weighted moments).  See the DETAILS section of the 
  help file for \code{\link{eevd}} for more information on these estimation methods. 
}
  \item{pwme.method}{
  character string specifying what method to use to compute the 
  probability-weighted moments when \code{method="pwme"}.  The possible values are 
  \code{"ubiased"} (method based on the U-statistic; the default), or 
  \code{"plotting.position"} (method based on the plotting position formula).  
  See the DETAILS section of the help file for \code{\link{eevd}} for more information.  
  This argument is ignored if \code{method} is not equal to \code{"pwme"}.
}
  \item{plot.pos.cons}{
  numeric vector of length 2 specifying the constants used in the formula for the 
  plotting positions when \code{method="pwme"} and \cr
  \code{pwme.method="plotting.position"}.  The default value is \cr
  \code{plot.pos.cons=c(a=0.35, b=0)}.  If this vector has a names attribute with 
  the value \code{c("a","b")} or \code{c("b","a")}, then the elements will be 
  matched by name in the formula for computing the plotting positions.  Otherwise, 
  the first element is mapped to the name \code{"a"} and the second element to the 
  name \code{"b"}.  See the DETAILS section of the help file for \code{\link{eevd}} 
  for more information.  This argument is ignored if 
  \code{method} is not equal to \code{"pwme"} or if \code{pwme.method="ubiased"}.
}
  \item{digits}{
  an integer indicating the number of decimal places to round to when printing out 
  the value of \code{100*p}. The default value is \code{digits=0}.
}
}
\details{
  The function \code{eqevd} returns estimated quantiles as well as 
  estimates of the location and scale parameters.  

  Quantiles are estimated by 1) estimating the location and scale parameters by 
  calling \code{\link{eevd}}, and then 2) calling the function 
  \code{\link[=EVD]{qevd}} and using the estimated values for 
  location and scale.
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
  Castillo, E. (1988).  \emph{Extreme Value Theory in Engineering}.  
  Academic Press, New York, pp.184--198.

  Downton, F. (1966).  Linear Estimates of Parameters in the Extreme Value 
  Distribution.  \emph{Technometrics} \bold{8}(1), 3--17.

  Forbes, C., M. Evans, N. Hastings, and B. Peacock. (2011).  Statistical Distributions. 
  Fourth Edition. John Wiley and Sons, Hoboken, NJ.

  Greenwood, J.A., J.M. Landwehr, N.C. Matalas, and J.R. Wallis. (1979).  
  Probability Weighted Moments: Definition and Relation to Parameters of Several 
  Distributions Expressible in Inverse Form.  \emph{Water Resources Research} 
  \bold{15}(5), 1049--1054.

  Hosking, J.R.M., J.R. Wallis, and E.F. Wood. (1985).  Estimation of the 
  Generalized Extreme-Value Distribution by the Method of 
  Probability-Weighted Moments.  \emph{Technometrics} \bold{27}(3), 251--261.

  Johnson, N. L., S. Kotz, and N. Balakrishnan. (1995). 
  \emph{Continuous Univariate Distributions, Volume 2}. 
  Second Edition. John Wiley and Sons, New York.

  Landwehr, J.M., N.C. Matalas, and J.R. Wallis. (1979).  Probability Weighted 
  Moments Compared With Some Traditional Techniques in Estimating Gumbel 
  Parameters and Quantiles.  \emph{Water Resources Research} \bold{15}(5), 
  1055--1064.

  Tiago de Oliveira, J. (1963).  Decision Results for the Parameters of the 
  Extreme Value (Gumbel) Distribution Based on the Mean and Standard Deviation.  
  \emph{Trabajos de Estadistica} \bold{14}, 61--81.
}
\author{
    Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  There are three families of extreme value distributions.  The one 
  described here is the \link[=EVD]{Type I, also called the Gumbel extreme value 
  distribution or simply Gumbel distribution}.  The name 
  \dQuote{extreme value} comes from the fact that this distribution is 
  the limiting distribution (as \eqn{n} approaches infinity) of the 
  greatest value among \eqn{n} independent random variables each 
  having the same continuous distribution.

  The Gumbel extreme value distribution is related to the 
  \link[stats:Exponential]{exponential distribution} as follows. 
  Let \eqn{Y} be an \link[stats:Exponential]{exponential random variable} 
  with parameter \code{rate=}\eqn{\lambda}.  Then \eqn{X = \eta - log(Y)} 
  has an extreme value distribution with parameters 
  \code{location=}\eqn{\eta} and \code{scale=}\eqn{1/\lambda}.

  The distribution described above and assumed by \code{eevd} is the 
  \emph{largest} extreme value distribution.  The smallest extreme value 
  distribution is the limiting distribution (as \eqn{n} approaches infinity) 
  of the smallest value among 
  \eqn{n} independent random variables each having the same continuous distribution. 
  If \eqn{X} has a largest extreme value distribution with parameters 
  \code{location=}\eqn{\eta} and \code{scale=}\eqn{\theta}, then 
  \eqn{Y = -X} has a smallest extreme value distribution with parameters 
  \code{location=}\eqn{-\eta} and \code{scale=}\eqn{\theta}.  The smallest 
  extreme value distribution is related to the \link[stats:Weibull]{Weibull distribution} 
  as follows.  Let \eqn{Y} be a \link[stats:Weibull]{Weibull random variable} with 
  parameters 
  \code{shape=}\eqn{\beta} and \code{scale=}\eqn{\alpha}.  Then \eqn{X = log(Y)} 
  has a smallest extreme value distribution with parameters \code{location=}\eqn{log(\alpha)} 
  and \code{scale=}\eqn{1/\beta}.

  The extreme value distribution has been used extensively to model the distribution 
  of streamflow, flooding, rainfall, temperature, wind speed, and other 
  meteorological variables, as well as material strength and life data.
}
\seealso{
  \code{\link{eevd}}, \link[=EVD]{Extreme Value Distribution}, 
  \code{\link{estimate.object}}.
}
\examples{
  # Generate 20 observations from an extreme value distribution with 
  # parameters location=2 and scale=1, then estimate the parameters 
  # and estimate the 90'th percentile.
  # (Note: the call to set.seed simply allows you to reproduce this example.)

  set.seed(250) 
  dat <- revd(20, location = 2) 
  eqevd(dat, p = 0.9) 

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            Extreme Value
  #
  #Estimated Parameter(s):          location = 1.9684093
  #                                 scale    = 0.7481955
  #
  #Estimation Method:               mle
  #
  #Estimated Quantile(s):           90'th %ile = 3.652124
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
