\name{cv}
\alias{cv}
\alias{CV}
\alias{coefficient of variation}
\alias{Coefficient of Variation}
\title{
  Sample Coefficient of Variation.
}
\description{
  Compute the sample coefficient of variation.
}
\usage{
  cv(x, method = "moments", sd.method = "sqrt.unbiased", 
    l.moment.method = "unbiased", plot.pos.cons = c(a = 0.35, b = 0), 
    na.rm = FALSE)
}
\arguments{
  \item{x}{
  numeric vector of observations.
}
  \item{method}{
  character string specifying what method to use to compute the sample coefficient 
  of variation.  The possible values are \code{"moments"} 
  (product moment ratio estimator; the default), or \code{"l.moments"} 
  (L-moment ratio estimator).
}
  \item{sd.method}{
  character string specifying what method to use to compute the sample standard 
  deviation when \code{method="moments"}.  The possible values are 
  \code{"sqrt.ubiased"} (the square root of the unbiased estimate of variance; 
  the default), or \code{"moments"} (the method of moments estimator).
}
  \item{l.moment.method}{
  character string specifying what method to use to compute the 
  \eqn{L}-moments when \code{method="l.moments"}.  The possible values are 
  \code{"ubiased"} (method based on the \eqn{U}-statistic; the default), or 
  \code{"plotting.position"} (method based on the plotting position formula). 
}
  \item{plot.pos.cons}{
  numeric vector of length 2 specifying the constants used in the formula for 
  the plotting positions when \code{method="l.moments"} and \cr
  \code{l.moment.method="plotting.position"}.  The default value is \cr
  \code{plot.pos.cons=c(a=0.35, b=0)}.  If this vector has a names attribute 
  with the value \code{c("a","b")} or \code{c("b","a")}, then the elements will 
  be matched by name in the formula for computing the plotting positions.  
  Otherwise, the first element is mapped to the name \code{"a"} and the second 
  element to the name \code{"b"}. 
}
  \item{na.rm}{
  logical scalar indicating whether to remove missing values from \code{x}.  
  If \cr
  \code{na.rm=FALSE} (the default) and \code{x} contains missing values, 
  then a missing value (\code{NA}) is returned.  If \code{na.rm=TRUE}, 
  missing values are removed from \code{x} prior to computing the coefficient 
  of variation.
}
}
\details{
  Let \eqn{\underline{x}} denote a random sample of \eqn{n} observations from 
  some distribution with mean \eqn{\mu} and standard deviation \eqn{\sigma}.

  \emph{Product Moment Coefficient of Variation} (\code{method="moments"}) \cr
  The coefficient of variation (sometimes denoted CV) of a distribution is 
  defined as the ratio of the standard deviation to the mean. That is:
  \deqn{CV = \frac{\sigma}{\mu} \;\;\;\;\;\; (1)}
  The coefficient of variation measures how spread out the distribution is 
  relative to the size of the mean.  It is usually used to characterize positive, 
  right-skewed distributions such as the lognormal distribution.

  When \code{sd.method="sqrt.unbiased"}, the coefficient of variation is estimated 
  using the sample mean and the square root of the unbaised estimator of variance:
  \deqn{\widehat{CV} = \frac{s}{\bar{x}} \;\;\;\;\;\; (2)}
  where
  \deqn{\bar{x} = \frac{1}{n} \sum_{i=1}^n x_i \;\;\;\;\;\; (3)}
  \deqn{s = [\frac{1}{n-1} \sum_{i=1}^n (x_i - \bar{x})^2]^{1/2} \;\;\;\;\;\; (4)}
  Note that the estimator of standard deviation in equation (4) is not unbiased. 
 
  When \code{sd.method="moments"}, the coefficient of variation is estimated using 
  the sample mean and the square root of the method of moments estimator of variance:
  \deqn{\widehat{CV} = \frac{s_m}{\bar{x}} \;\;\;\;\;\; (5)}
  \deqn{s = [\frac{1}{n} \sum_{i=1}^n (x_i - \bar{x})^2]^{1/2} \;\;\;\;\;\; (6)}
  \cr

  \emph{L-Moment Coefficient of Variation} (\code{method="l.moments"}) \cr
  Hosking (1990) defines an \eqn{L}-moment analog of the 
  coefficient of variation (denoted the \eqn{L}-CV) as:
  \deqn{\tau = \frac{l_2}{l_1} \;\;\;\;\;\; (7)}
  that is, the second \eqn{L}-moment divided by the first \eqn{L}-moment.  
  He shows that for a positive-valued random variable, the \eqn{L}-CV lies in the 
  interval (0, 1).

  When \code{l.moment.method="unbiased"}, the \eqn{L}-CV is estimated by:
  \deqn{t = \frac{l_2}{l_1} \;\;\;\;\;\; (8)}
  that is, the unbiased estimator of the second \eqn{L}-moment divided by 
  the unbiased estimator of the first \eqn{L}-moment.

  When \code{l.moment.method="plotting.position"}, the \eqn{L}-CV is estimated by:
  \deqn{\tilde{t} = \frac{\tilde{l_2}}{\tilde{l_1}} \;\;\;\;\;\; (9)}
  that is, the plotting-position estimator of the second \eqn{L}-moment divided by 
  the plotting-position estimator of the first \eqn{L}-moment.

  See the help file for \code{\link{lMoment}} for more information on 
  estimating \eqn{L}-moments.
}
\value{
  A numeric scalar -- the sample coefficient of variation.
}
\references{
  Berthouex, P.M., and L.C. Brown. (2002). 
  \emph{Statistics for Environmental Engineers, Second Edition}. 
  Lewis Publishers, Boca Raton, FL.

  Gilbert, R.O. (1987). \emph{Statistical Methods for Environmental Pollution 
  Monitoring}. Van Nostrand Reinhold, NY.

  Ott, W.R. (1995). \emph{Environmental Statistics and Data Analysis}. 
  Lewis Publishers, Boca Raton, FL.

  Taylor, J.K. (1990). \emph{Statistical Techniques for Data Analysis}.  
  Lewis Publishers, Boca Raton, FL.

  Vogel, R.M., and N.M. Fennessey. (1993).  \eqn{L} Moment Diagrams Should Replace 
  Product Moment Diagrams.  \emph{Water Resources Research} \bold{29}(6), 1745--1752.

  Zar, J.H. (2010). \emph{Biostatistical Analysis}. Fifth Edition. 
  Prentice-Hall, Upper Saddle River, NJ.
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  Traditionally, the coefficient of variation has been estimated using 
  product moment estimators.  Hosking (1990) introduced the idea of 
  \eqn{L}-moments and the \eqn{L}-CV.  Vogel and Fennessey (1993) argue that 
  \eqn{L}-moment ratios should replace product moment ratios because of their 
  superior performance (they are nearly unbiased and better for discriminating 
  between distributions).
}
\seealso{
  \link{Summary Statistics}, \code{\link{summaryFull}}, \code{\link{var}}, 
  \code{\link{sd}}, \code{\link{skewness}}, \code{\link{kurtosis}}.
}
\examples{
  # Generate 20 observations from a lognormal distribution with 
  # parameters mean=10 and cv=1, and estimate the coefficient of variation. 
  # (Note: the call to set.seed simply allows you to reproduce this example.)

  set.seed(250) 
  dat <- rlnormAlt(20, mean = 10, cv = 1) 

  cv(dat) 
  #[1] 0.5077981

  cv(dat, sd.method = "moments") 
  #[1] 0.4949403
 
  cv(dat, method = "l.moments") 
  #[1] 0.2804148

  #----------
  # Clean up
  rm(dat)
}
\keyword{ univar }
