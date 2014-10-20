\name{print.gofOutlier}
\alias{print.gofOutlier}
\title{
  Print Output of Goodness-of-Fit Outlier Tests
}
\description{
  Formats and prints the results of performing a goodness-of-fit test for outliers.  
  This method is automatically called by \code{\link[base]{print}} when given an object of 
  class \code{"gofOutlier"}.  The names of the functions that perform goodness-of-fit tests 
  for outliers and that produce objects of class \code{"gofOutlier"} are listed under 
  \link[=FcnsByCatGOFTests]{Tests for Outliers}.
}
\usage{
\method{print}{gofOutlier}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  an object of class \code{"gofOutlier"}.  See \code{\link{gofOutlier.object}} for details.
}
  \item{\dots}{
  arguments that can be supplied to the \code{\link[base]{format}} function.
}
}
\details{
  This is the \code{"gofOutlier"} method for the generic function \code{\link[base]{print}}.  
  Prints name of the test, hypothesized distribution, data name, sample size, 
  value of the test statistic, parameters associated with the null distribution of the 
  test statistic, Type I error, critical values, and the alternative hypothesis.
}
\value{
  Invisibly returns the input \code{x}.
}
\references{
  Chambers, J. M. and Hastie, T. J. (1992).  \emph{Statistical Models in S}.  
  Wadsworth & Brooks/Cole. 
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\seealso{
  \link[=FcnsByCatGOFTests]{Tests for Outliers}, \code{\link{gofOutlier.object}}, 
  \code{\link[base]{print}}.
}
\keyword{print}