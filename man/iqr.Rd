\name{iqr}
\alias{iqr}
\alias{Interquartile Range}
\title{
  Interquartile Range
}
\description{
  Compute the interquartile range for a set of data.
}
\usage{
  iqr(x, na.rm = FALSE)
}
\arguments{
  \item{x}{
  numeric vector of observations.
}
  \item{na.rm}{
  logical scalar indicating whether to remove missing values from \code{x}. If \cr
  \code{na.rm=FALSE} (the default) and \code{x} contains missing values, 
  then a missing value (\code{NA}) is returned.  If \code{na.rm=TRUE}, 
  missing values are removed from \code{x} prior to computing the coefficient 
  of variation.
}
}
\details{
  Let \eqn{\underline{x}} denote a random sample of \eqn{n} observations from 
  some distribution associated with a random variable \eqn{X}.  The sample 
  interquartile range is defined as:
  \deqn{IQR = \hat{X}_{0.75} - \hat{X}_{0.25} \;\;\;\;\;\; (1)}
  where \eqn{X_p} denotes the \eqn{p}'th quantile of the distribution and 
  \eqn{\hat{X}_p} denotes the estimate of this quantile (i.e., the sample 
  \eqn{p}'th quantile).

  See the \R help file for \code{\link{quantile}} for information on how sample 
  quantiles are computed.
}
\value{
  A numeric scalar -- the interquartile range.
}
\references{
  Chambers, J.M., W.S. Cleveland, B. Kleiner, and P.A. Tukey. (1983).  
  \emph{Graphical Methods for Data Analysis}. Duxbury Press, Boston, MA.

  Cleveland, W.S. (1993).  \emph{Visualizing Data}.  Hobart Press, Summit, 
  New Jersey.

  Helsel, D.R., and R.M. Hirsch. (1992). 
  \emph{Statistical Methods in Water Resources Research}. 
  Elsevier, New York, NY.

  Hirsch, R.M., D.R. Helsel, T.A. Cohn, and E.J. Gilroy. (1993).  
  Statistical Analysis of Hydrologic Data. In: Maidment, D.R., ed. 
  \emph{Handbook of Hydrology}. McGraw-Hill, New York, Chapter 17, pp.5--7.

  Zar, J.H. (2010). \emph{Biostatistical Analysis}. Fifth Edition. 
  Prentice-Hall, Upper Saddle River, NJ.
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  The interquartile range is a robust estimate of the spread of the 
  distribution.  It is the distance between the two ends of a boxplot 
  (see the \R help file for \code{\link{boxplot}}).  For a normal distribution 
  with standard deviation \eqn{\sigma} it can be shown that:
  \deqn{IQR = 1.34898 \sigma \;\;\;\;\;\; (2)}
}
\seealso{
  \link{Summary Statistics}, \code{\link{summaryFull}}, 
  \code{\link{var}}, \code{\link{sd}}.
}
\examples{
  # Generate 20 observations from a normal distribution with parameters 
  # mean=10 and sd=2, and compute the standard deviation and 
  # interquartile range. 
  # (Note: the call to set.seed simply allows you to reproduce this example.)

  set.seed(250) 
  dat <- rnorm(20, mean=10, sd=2) 

  sd(dat) 
  #[1] 1.180226
 
  iqr(dat) 
  #[1] 1.489932
 
  #----------

  # Repeat the last example, but add a couple of large "outliers" to the 
  # data.  Note that the estimated standard deviation is greatly affected 
  # by the outliers, while the interquartile range is not.

  summaryStats(dat, quartiles = TRUE) 
  #     N   Mean     SD Median    Min     Max 1st Qu. 3rd Qu.
  #dat 20 9.8612 1.1802 9.6978 7.6042 11.8756  9.1618 10.6517
 
  new.dat <- c(dat, 20, 50) 

  sd(dat) 
  #[1] 1.180226
 
  sd(new.dat) 
  #[1] 8.79796
 
  iqr(dat) 
  #[1] 1.489932
 
  iqr(new.dat) 
  #[1] 1.851472

  #----------
  # Clean up
  rm(dat, new.dat)
}
\keyword{ univar }
