\name{print.htest}
\alias{print.htest}
\title{
  Print Output of Hypothesis Tests
}
\description{
  This is a modification of the \R function \code{print.htest} that formats and 
  prints the results of performing a hypothesis test.  This method is 
  automatically called by the \pkg{EnvStats} generic function \code{\link{print}} when 
  given an object of class \code{"htest"}.  The names of the \pkg{EnvStats} functions 
  that perform hypothesis tests and that produce objects of class 
  \code{"htest"} are listed under \link{Hypothesis Tests}.
}
\usage{
\method{print}{htest}(x, ...)
}
\arguments{
  \item{x}{
  an object of class \code{"htest"}.  See \code{\link{htest.object}} for details.
}
  \item{\dots}{
  arguments that can be supplied to the \code{\link[base]{format}} function.
}
}
\details{
  This is the \code{"htest"} method for the \pkg{EnvStats} generic function 
  \code{\link{print}}.  
  Prints null and alternative hypotheses, name of the test, estimated population 
  parameter(s) involved in the null hypothesis, estimation method (if present), 
  data name, sample size (if present), number of missing observations removed 
  prior to performing the test (if present), value of the test statistic, 
  parameters associated with the null distribution of the test statistic, 
  p-value associated with the test statistic, and confidence interval for the 
  population parameter (if present).
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
  \link{Hypothesis Tests}, \code{\link{htest.object}}, 
  \code{\link{print}}.
}
\keyword{print}