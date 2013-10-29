\name{print.gofGroup}
\alias{print.gofGroup}
\title{
  Print Output of Group Goodness-of-Fit Tests
}
\description{
  Formats and prints the results of performing a group goodness-of-fit test.  
  This method is automatically called by \code{\link[base]{print}} when given an 
  object of class \code{\link[=gofGroup.object]{"gofGroup"}}.  Currently, 
  the only \pkg{EnvStats} function that performs a group goodness-of-fit test 
  that produces an object of class \code{\link[=gofGroup.object]{"gofGroup"}} 
  is \code{\link{gofGroupTest}}.
}
\usage{
\method{print}{gofGroup}(x, ...)
}
\arguments{
  \item{x}{
  an object of class \code{"gofGroup"}.  
  See \code{\link{gofGroup.object}} for details.
}
  \item{\dots}{
  arguments that can be supplied to the \code{\link[base]{format}} function.
}
}
\details{
  This is the \code{"gofGroup"} method for the generic function 
  \code{\link[base]{print}}.  
  See the help file for \code{\link{gofGroup.object}} for information 
  on the information contained in this kind of object.
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
  \link[=FcnsByCatGOFTests]{Goodness-of-Fit Tests}, 
  \code{\link{gofGroup.object}}, 
  \code{\link[base]{print}}.
}
\keyword{print}
