\name{print.boxcox}
\alias{print.boxcox}
\title{
  Print Output of Objective for Box-Cox Power Transformations
}
\description{
  Formats and prints the results of calling the function \code{\link{boxcox}}.  
  This method is automatically called by \code{\link{print}} when given an 
  object of class \code{"boxcox"}.  The names of other functions involved in 
  Box-Cox transformations are listed under \link{Data Transformations}.
}
\usage{
\method{print}{boxcox}(x, ...)
}
\arguments{
  \item{x}{
  an object of class \code{"boxcox"}.  See \code{\link{boxcox.object}} for details.
}
  \item{\dots}{
  arguments that can be supplied to the \code{\link[base]{format}} function.
}
}
\details{
  This is the \code{"boxcox"} method for the generic function \code{\link[base]{print}}.  
  Prints the objective name, the name of the data object used, the sample size, 
  the values of the powers, and the values of the objective.  In the case of 
  optimization, also prints the range of powers over which the optimization 
  took place.
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
  \code{\link{boxcox}}, \code{\link{boxcox.object}}, \code{\link{plot.boxcox}}, 
  \link{Data Transformations}, \code{\link[base]{print}}.
}
\keyword{print}
