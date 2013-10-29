\name{print.boxcoxLm}
\alias{print.boxcoxLm}
\title{
  Print Output of Objective for Box-Cox Power Transformations for an "lm" Object
}
\description{
  Formats and prints the results of calling the function \code{\link{boxcox}} 
  when the argument \code{x} supplied to \code{\link{boxcox}} is an object of 
  class \code{"lm"}.  This method is automatically called by \code{\link{print}} 
  when given an object of class \code{"boxcoxLm"}.  The names of other functions 
  involved in Box-Cox transformations are listed under \link{Data Transformations}.
}
\usage{
\method{print}{boxcoxLm}(x, ...)
}
\arguments{
  \item{x}{
  an object of class \code{"boxcoxLm"}.  See \code{\link{boxcoxLm.object}} for details.
}
  \item{\dots}{
  arguments that can be supplied to the \code{\link[base]{format}} function.
}
}
\details{
  This is the \code{"boxcoxLm"} method for the generic function \code{\link[base]{print}}.  
  Prints the objective name,  the details of the \code{"lm"} object used, 
  the sample size, 
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
  \code{\link{boxcox}}, \code{\link{boxcoxLm.object}}, \code{\link{plot.boxcoxLm}}, 
  \link{Data Transformations}, \code{\link[base]{print}}.
}
\keyword{print}
