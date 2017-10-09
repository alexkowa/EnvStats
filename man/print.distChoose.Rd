\name{print.distChoose}
\alias{print.distChoose}
\title{
  Print Output of Goodness-of-Fit Tests
}
\description{
  Formats and prints the results of calling the function \code{\link{distChoose}}, which 
  uses a series of goodness-of-fit tests to choose among candidate distributions. 
  This method is automatically called by \code{\link[base]{print}} when given an 
  object of class \code{"distChoose"}.
}
\usage{
\method{print}{distChoose}(x, ...)
}
\arguments{
  \item{x}{
  an object of class \code{"distChoose"}.  See \code{\link{distChoose.object}} for details.
}
  \item{\dots}{
  arguments that can be supplied to the \code{\link[base]{format}} function.
}
}
\details{
  This is the \code{"distChoose"} method for the generic function \code{\link[base]{print}}.  
  Prints the candidate distributions, method used to choose among the candidate distributions, 
  chosen distribution, Type I error associated with each goodness-of-fit test, 
  estimated population parameter(s) associated with the chosen distribution, 
  estimation method, goodness-of-fit test results for each candidate distribution, 
  and the data name.
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
  \code{\link{distChoose}}, \code{\link{distChoose.object}}, 
  \link[=FcnsByCatGOFTests]{Goodness-of-Fit Tests}, \code{\link[base]{print}}.
}
\keyword{print}