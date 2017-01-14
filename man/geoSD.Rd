\name{geoSD}
\alias{geoSD}
\alias{Geometric Standard Deviation}
\alias{geometric standard deviation}
\alias{geometric SD}
\alias{Geometric SD}
\title{
  Geometric Standard Deviation.
}
\description{
  Compute the sample geometric standard deviation.
}
\usage{
  geoSD(x, na.rm = FALSE, sqrt.unbiased = TRUE)
}
\arguments{
  \item{x}{
  numeric vector of observations.
}
  \item{na.rm}{
  logical scalar indicating whether to remove missing values from \code{x}. \cr
  If \code{na.rm=FALSE} (the default) and \code{x} contains missing values, 
  then a missing value (\code{NA}) is returned.  If \code{na.rm=TRUE}, 
  missing values are removed from \code{x} prior to computing the coefficient 
  of variation.
}
  \item{sqrt.unbiased}{
  logical scalar specifying what method to use to compute the sample standard 
  deviation of the log-transformed observations.  If \code{sqrt.unbiased=TRUE} 
  (the default), the square root of the unbiased estimator of variance is used, 
  otherwise the method of moments estimator of standard deviation is used.  
  See the DETAILS section for more information.
}
}
\details{
  If \code{x} contains any non-positive values (values less than or equal to 0), 
  \code{geoMean} returns \code{NA} and issues a warning.

  Let \eqn{\underline{x}} denote a vector of \eqn{n} observations from some 
  distribution.  The sample geometric standard deviation is a measure of variability.  
  It is defined as:
  \deqn{s_G = exp(s_y) \;\;\;\;\;\; (1)}
  where
  \deqn{s_y = [\frac{1}{n-1} \sum_{i=1}^n (y_i - \bar{y})^2]^{1/2} \;\;\;\;\;\; (2)}
  \deqn{y_i = log(x_i), \;\; i = 1, 2, \ldots, n \;\;\;\;\;\; (3)}
  That is, the sample geometric standard deviation is the antilog of the sample 
  standard deviation of the log-transformed observations.

  The sample standard deviation of the log-transformed observations shown in 
  Equation (2) is the square root of the unbiased estimator of variance.  
  (Note that this estimator of standard deviation is not an unbiased estimator.)  
  Sometimes, the square root of the method of moments estimator of variance is used 
  instead:
  \deqn{s_y = [\frac{1}{n} \sum_{i=1}^n (y_i - \bar{y})^2]^{1/2} \;\;\;\;\;\; (4)}
  This is the estimator used in Equation (1) when \code{sqrt.unbiased=FALSE}.
}
\value{
  A numeric scalar -- the sample geometric standard deviation.
}
\references{
  Berthouex, P.M., and L.C. Brown. (2002). 
  \emph{Statistics for Environmental Engineers, Second Edition}. 
  Lewis Publishers, Boca Raton, FL.

  Gilbert, R.O. (1987). \emph{Statistical Methods for Environmental Pollution 
  Monitoring}. Van Nostrand Reinhold, NY.

  Leidel, N.A., K.A. Busch, and J.R. Lynch. (1977).  \emph{Occupational 
  Exposure Sampling Strategy Manual}.  U.S. Department of Health, Education, 
  and Welfare, Public Health Service, Center for Disease Control, 
  National Institute for Occupational Safety and Health, Cincinnati, 
  Ohio 45226, January, 1977, pp.102--103.

  Ott, W.R. (1995). \emph{Environmental Statistics and Data Analysis}. 
  Lewis Publishers, Boca Raton, FL.

  Taylor, J.K. (1990). \emph{Statistical Techniques for Data Analysis}.  
  Lewis Publishers, Boca Raton, FL.

  Zar, J.H. (2010). \emph{Biostatistical Analysis}. Fifth Edition. 
  Prentice-Hall, Upper Saddle River, NJ.
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  The geometric standard deviation is only defined for positive observations.  
  It is usually computed only for observations that are assumed to have come 
  from a \link[=LognormalAlt]{lognormal distribution}. 
}
\seealso{
  \code{\link{geoMean}}, \link{Lognormal}, \code{\link{elnorm}}, 
  \code{\link{summaryFull}}, \code{\link{Summary Statistics}}.
}
\examples{
  # Generate 2000 observations from a lognormal distribution with parameters 
  # mean=10 and cv=1, which implies the standard deviation (on the original 
  # scale) is 10.  Compute the mean, geometric mean, standard deviation, 
  # and geometric standard deviation. 
  # (Note: the call to set.seed simply allows you to reproduce this example.)

  set.seed(250) 
  dat <- rlnormAlt(2000, mean = 10, cv = 1) 

  mean(dat) 
  #[1] 10.23417
 
  geoMean(dat) 
  #[1] 7.160154
 
  sd(dat) 
  #[1] 9.786493
 
  geoSD(dat) 
  #[1] 2.334358

  #----------
  # Clean up
  rm(dat)
}
\keyword{ univar }
