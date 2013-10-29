\name{print.gof}
\alias{print.gof}
\title{
  Print Output of Goodness-of-Fit Tests
}
\description{
  Formats and prints the results of performing a goodness-of-fit test.  This method is 
  automatically called by \code{\link[base]{print}} when given an object of class \code{"gof"}.  
  The names of the functions that perform goodness-of-fit tests and that produce objects of class 
  \code{"gof"} are listed under \link[=FcnsByCatGOFTests]{Goodness-of-Fit Tests}.
}
\usage{
\method{print}{gof}(x, ...)
}
\arguments{
  \item{x}{
  an object of class \code{"gof"}.  See \code{\link{gof.object}} for details.
}
  \item{\dots}{
  arguments that can be supplied to the \code{\link[base]{format}} function.
}
}
\details{
  This is the \code{"gof"} method for the generic function \code{\link[base]{print}}.  
  Prints name of the test, hypothesized distribution, estimated population parameter(s), 
  estimation method, data name, sample size, value of the test statistic, parameters 
  associated with the null distribution of the test statistic, p-value associated with the 
  test statistic, and the alternative hypothesis.
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
  \link[=FcnsByCatGOFTests]{Goodness-of-Fit Tests}, \code{\link{gof.object}}, 
  \code{\link[base]{print}}.
}
\keyword{print}