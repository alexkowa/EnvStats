\name{print.summaryStats}
\alias{print.summaryStats}
\title{
  Print Summary Statistics
}
\description{
  Formats and prints the results of calling \code{\link{summaryStats}} or 
  \code{\link{summaryFull}}.  This method is automatically called by 
  \code{\link[base]{print}} when given an object of class \code{"summaryStats"}.
}
\usage{
\method{print}{summaryStats}(x, ...)
}
\arguments{
  \item{x}{
  an object of class \code{"summaryStats"}.  See \code{\link{summaryStats.object}} for 
  details.
}
  \item{\dots}{
  arguments that can be supplied to the \code{\link[base]{format}} function.
}
}
\details{
  This is the \code{"summaryStats"} method for the generic function 
  \code{\link[base]{print}}.  Prints summary statistics.
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
  \code{\link{summaryStats}}, \code{\link{summaryFull}},   
  \code{\link{summaryStats.object}}, \code{\link[base]{print}}.
}
\keyword{print}