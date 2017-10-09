\name{print.gofCensored}
\alias{print.gofCensored}
\title{
  Print Output of Goodness-of-Fit Tests Based on Censored Data
}
\description{
  Formats and prints the results of performing a goodness-of-fit test.  This method is 
  automatically called by \code{\link[base]{print}} when given an object of class 
  \code{"gofCensored"}.  Currently, the only function that produces an object of 
  this class is \code{\link{gofTestCensored}}.
}
\usage{
\method{print}{gofCensored}(x, show.cen.levels = TRUE, 
  pct.censored.digits = .Options$digits, ...)
}
\arguments{
  \item{x}{
  an object of class \code{"gofCensored"}.  See \code{\link{gofCensored.object}} for 
  details.
}
  \item{show.cen.levels}{
  logical scalar indicating whether to print the censoring levels.  The default is 
  \code{show.cen.levels=TRUE}.
}
  \item{pct.censored.digits}{
  numeric scalar indicating the number of significant digits to print for the 
  percent of censored observations.
}
  \item{\dots}{
  arguments that can be supplied to the \code{\link[base]{format}} function.
}
}
\details{
  This is the \code{"gofCensored"} method for the generic function 
  \code{\link[base]{print}}.  
  Prints name of the test, hypothesized distribution, estimated population parameter(s), 
  estimation method, data name, sample size, censoring information, value of the test 
  statistic, parameters associated with the null distribution of the test statistic, 
  p-value associated with the test statistic, and the alternative hypothesis.
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
  \link[=FcnsByCatCensoredData]{Censored Data}, \code{\link{gofCensored.object}}, 
  \code{\link[base]{print}}.
}
\keyword{print}