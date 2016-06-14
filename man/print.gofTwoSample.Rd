\name{print.gofTwoSample}
\alias{print.gofTwoSample}
\title{
  Print Output of Two-Sample Goodness-of-Fit Tests
}
\description{
  Formats and prints the results of performing a two-sample goodness-of-fit test.  
  This method is automatically called by \code{\link[base]{print}} when given an 
  object of class \code{\link[=gofTwoSample.object]{"gofTwoSample"}}.  Currently, 
  the only \pkg{EnvStats} function that performs a two-sample goodness-of-fit test 
  that produces an object of class \code{\link[=gofTwoSample.object]{"gofTwoSample"}} 
  is \code{\link{gofTest}}.
}
\usage{
\method{print}{gofTwoSample}(x, ...)
}
\arguments{
  \item{x}{
  an object of class \code{"gofTwoSample"}.  See \code{\link{gofTwoSample.object}} 
  for details.
}
  \item{\dots}{
  arguments that can be supplied to the \code{\link[base]{format}} function.
}
}
\details{
  This is the \code{"gofTwoSample"} method for the generic function 
  \code{\link[base]{print}}.  
  See the help file for \cr
  \code{\link{gofTwoSample.object}} for information on the 
  information contained in this kind of object.
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
  \link[=FcnsByCatGOFTests]{Goodness-of-Fit Tests}, \code{\link{gofTwoSample.object}}, 
  \code{\link[base]{print}}.
}
\keyword{print}