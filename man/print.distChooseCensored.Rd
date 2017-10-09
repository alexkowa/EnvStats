\name{print.distChooseCensored}
\alias{print.distChooseCensored}
\title{
  Print Output of Goodness-of-Fit Tests
}
\description{
  Formats and prints the results of calling the function \code{\link{distChooseCensored}}, which 
  uses a series of goodness-of-fit tests to choose among candidate distributions based on  
  censored data. 
  This method is automatically called by \code{\link[base]{print}} when given an 
  object of class \code{"distChooseCensored"}.
}
\usage{
\method{print}{distChooseCensored}(x, show.cen.levels = TRUE, 
  pct.censored.digits = .Options$digits, ...)
}
\arguments{
  \item{x}{
  an object of class \code{"distChoose"}.  See \code{\link{distChoose.object}} for details.
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
  This is the \code{"distChooseCensored"} method for the generic function 
  \code{\link[base]{print}}.  
  Prints the candidate distributions, 
  method used to choose among the candidate distributions, 
  chosen distribution, Type I error associated with each goodness-of-fit test, 
  estimated population parameter(s) associated with the chosen distribution, 
  estimation method, goodness-of-fit test results for each candidate distribution, 
  and the data name and censoring variable.
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
  \code{\link{distChooseCensored}}, \code{\link{distChooseCensored.object}}, 
  \link{Censored Data}, \code{\link[base]{print}}.
}
\keyword{print}