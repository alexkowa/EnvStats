\name{geoMean}
\alias{geoMean}
\alias{geometric mean}
\alias{Geometric Mean}
\title{
  Geometric Mean
}
\description{
  Compute the sample geometric mean.
}
\usage{
  geoMean(x, na.rm = FALSE)
}
\arguments{
  \item{x}{
  numeric vector of observations.
}
  \item{na.rm}{
  logical scalar indicating whether to remove missing values from \code{x}.  
  If \code{na.rm=FALSE} (the default) and \code{x} contains missing values, 
  then a missing value (\code{NA}) is returned.  If \code{na.rm=TRUE}, 
  missing values are removed from \code{x} prior to computing the coefficient 
  of variation.
}
}
\details{
  If \code{x} contains any non-positive values (values less than or equal to 0), 
  \code{geoMean} returns \code{NA} and issues a warning.

  Let \eqn{\underline{x}} denote a vector of \eqn{n} observations from some 
  distribution.  The sample geometric mean is a measure of central tendency.  
  It is defined as:
  \deqn{\bar{x}_G = \sqrt[n]{x_1 x_2 \ldots x_n} = [\prod_{i=1}^n x_i]^{1/n} \;\;\;\;\;\; (1)}
  that is, it is the \eqn{n}'th root of the product of all \eqn{n} observations.

  An equivalent way to define the geometric mean is by:
  \deqn{\bar{x}_G = exp[\frac{1}{n} \sum_{i=1}^n log(x_i)] = e^{\bar{y}} \;\;\;\;\;\; (2)}
  where 
  \deqn{\bar{y} = \frac{1}{n} \sum_{i=1}^n y_i  \;\;\;\;\;\; (3)}
  \deqn{y_i = log(x_i), \;\; i = 1, 2, \ldots, n \;\;\;\;\;\; (4)}
  That is, the sample geometric mean is antilog of the sample mean of the 
  log-transformed observations.

  The geometric mean is only defined for positive observations.  
  It can be shown that the geometric mean is less than or equal to the 
  sample arithmetic mean with equality only when all of the observations are 
  the same value.
}
\value{
  A numeric scalar -- the sample geometric mean.
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

  Zar, J.H. (2010). \emph{Biostatistical Analysis}. Fifth Edition. 
  Prentice-Hall, Upper Saddle River, NJ.
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  The geometric mean is sometimes used to average ratios and percent changes 
  (Zar, 2010).  For the lognormal distribution, the geometric mean is the 
  maximum likelihood estimator of the \bold{\emph{median}} of the distribution, 
  although it is sometimes used incorrectly to estimate the mean of the 
  distribution (see the NOTE section in the help file for \code{\link{elnormAlt}}).
}
\seealso{
  \code{\link{geoSD}}, \code{\link{summaryFull}}, \code{\link{Summary Statistics}}, 
  \code{\link{mean}}, \code{\link{median}}.
}
\examples{
  # Generate 20 observations from a lognormal distribution with parameters 
  # mean=10 and cv=2, and compute the mean, median, and geometric mean. 
  # (Note: the call to set.seed simply allows you to reproduce this example.)

  set.seed(250) 
  dat <- rlnormAlt(20, mean = 10, cv = 2) 

  mean(dat) 
  #[1] 5.339273

  median(dat) 
  #[1] 3.692091
 
  geoMean(dat) 
  #[1] 4.095127
 
  #----------
  # Clean up
  rm(dat)
}
\keyword{ univar }
