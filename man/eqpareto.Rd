\name{eqpareto}
\alias{eqpareto}
\title{
  Estimate Quantiles of a Pareto Distribution
}
\description{
  Estimate quantiles of a \link[=Pareto]{Pareto distribution}.
}
\usage{
  eqpareto(x, p = 0.5, method = "mle", plot.pos.con = 0.375, digits = 0)
}
\arguments{
  \item{x}{
  a numeric vector of observations, or an object resulting from a call to an 
  estimating function that assumes a Pareto distribution 
  (e.g., \code{\link{epareto}}).  If \code{x} is a numeric vector, 
  missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) 
  values are allowed but will be removed.
}
  \item{p}{
  numeric vector of probabilities for which quantiles will be estimated.  
  All values of \code{p} must be between 0 and 1.  The default value is \code{p=0.5}.
}
  \item{method}{
  character string specifying the method of estimating the distribution parameters.  
  Possible values are 
  \code{"mle"} (maximum likelihood; the default), and \code{"lse"} (least-squares).  
  See the DETAILS section of the help file for \code{\link{epareto}} for more 
  information on these estimation methods. 
}
  \item{plot.pos.con}{
  numeric scalar between 0 and 1 containing the value of the plotting position 
  constant used to construct the values of the empirical cdf.  The default value is 
  \code{plot.pos.con=0.375}.  This argument is used only when \code{method="lse"}.
}
  \item{digits}{
  an integer indicating the number of decimal places to round to when printing out 
  the value of \code{100*p}. The default value is \code{digits=0}.
}
}
\details{
  The function \code{eqpareto} returns estimated quantiles as well as 
  estimates of the location and scale parameters.  

  Quantiles are estimated by 1) estimating the location and scale parameters by 
  calling \code{\link{epareto}}, and then 2) calling the function 
  \code{\link[=Pareto]{qpareto}} and using the estimated values for 
  location and scale.
}
\value{
  If \code{x} is a numeric vector, \code{eqpareto} returns a 
  list of class \code{"estimate"} containing the estimated quantile(s) and other 
  information. See \code{\link{estimate.object}} for details.

  If \code{x} is the result of calling an estimation function, \code{eqpareto} 
  returns a list whose class is the same as \code{x}.  The list 
  contains the same components as \code{x}, as well as components called 
  \code{quantiles} and \code{quantile.method}.
}
\references{
  Forbes, C., M. Evans, N. Hastings, and B. Peacock. (2011).  Statistical Distributions. 
  Fourth Edition. John Wiley and Sons, Hoboken, NJ.

  Johnson, N. L., S. Kotz, and N. Balakrishnan. (1994). 
  \emph{Continuous Univariate Distributions, Volume 1}. 
  Second Edition. John Wiley and Sons, New York.
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  The Pareto distribution is named after Vilfredo Pareto (1848-1923), a professor 
  of economics.  It is derived from Pareto's law, which states that the number of 
  persons \eqn{N} having income \eqn{\ge x} is given by:
  \deqn{N = A x^{-\theta}}
  where \eqn{\theta} denotes Pareto's constant and is the shape parameter for the 
  probability distribution.

  The Pareto distribution takes values on the positive real line.  All values must be 
  larger than the \dQuote{location} parameter \eqn{\eta}, which is really a threshold 
  parameter.  There are three kinds of Pareto distributions.  The one described here 
  is the Pareto distribution of the first kind.  Stable Pareto distributions have 
  \eqn{0 < \theta < 2}.  Note that the \eqn{r}'th moment only exists if 
  \eqn{r < \theta}.

  The Pareto distribution is related to the 
  \link[stats:Exponential]{exponential distribution} and 
  \link[stats:Logistic]{logistic distribution} as follows.  
  Let \eqn{X} denote a Pareto random variable with \code{location=}\eqn{\eta} and 
  \code{shape=}\eqn{\theta}.  Then \eqn{log(X/\eta)} has an exponential distribution 
  with parameter \code{rate=}\eqn{\theta}, and \eqn{-log\{ [(X/\eta)^\theta] - 1 \}} 
  has a logistic distribution with parameters \code{location=}\eqn{0} and 
  \code{scale=}\eqn{1}.

  The Pareto distribution has a very long right-hand tail.  It is often applied in 
  the study of socioeconomic data, including the distribution of income, firm size, 
  population, and stock price fluctuations.
}
\seealso{
  \code{\link{epareto}}, \link{Pareto}, \code{\link{estimate.object}}.
}
\examples{
  # Generate 30 observations from a Pareto distribution with 
  # parameters location=1 and shape=1 then estimate the parameters 
  # and the 90'th percentile.
  # (Note: the call to set.seed simply allows you to reproduce this example.)

  set.seed(250) 
  dat <- rpareto(30, location = 1, shape = 1) 
  eqpareto(dat, p = 0.9) 

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            Pareto
  #
  #Estimated Parameter(s):          location = 1.009046
  #                                 shape    = 1.079850
  #
  #Estimation Method:               mle
  #
  #Estimated Quantile(s):           90'th %ile = 8.510708
  #
  #Quantile Estimation Method:      Quantile(s) Based on
  #                                 mle Estimators
  #
  #Data:                            dat
  #
  #Sample Size:                     30

  #----------

  # Clean up
  #---------
  rm(dat)
}
\keyword{ distribution }
\keyword{ htest }

