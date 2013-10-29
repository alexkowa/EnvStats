\name{print.htestCensored}
\alias{print.htestCensored}
\title{
  Print Output of Hypothesis Tests Based on Censored Data
}
\description{
  Formats and prints the results of performing a hypothesis test based on censored data.  
  This method is automatically called by \code{\link[base]{print}} when given an 
  object of class \code{"htestCensored"}.  The names of the \pkg{EnvStats} functions 
  that perform hypothesis tests based on censored data and that produce objects of 
  class \code{"htestCensored"} are listed in the section \bold{Hypothesis Tests}  
  in the help file 
  \link[=FcnsByCatCensoredData]{EnvStats Functions for Censored Data}.  
  Currently, the only function listed is 
  \code{\link{twoSampleLinearRankTestCensored}}.
}
\usage{
\method{print}{htestCensored}(x, show.cen.levels = TRUE, ...)
}
\arguments{
  \item{x}{
  an object of class \code{"htestCensored"}.  
  See \code{\link{htestCensored.object}} for details.
}
  \item{show.cen.levels}{
  logical scalar indicating whether to print the censoring levels.  The default value 
  is \code{show.cen.levels=TRUE}.
}
  \item{\dots}{
  arguments that can be supplied to the \code{\link[base]{format}} function.
}
}
\details{
  This is the \code{"htestCensored"} method for the generic function 
  \code{\link[base]{print}}.  
  Prints null and alternative hypotheses, name of the test, censoring side, 
  estimated population parameter(s) involved in the null hypothesis, 
  estimation method (if present), 
  data name, censoring variable, sample size (if present), 
  percent of observations that are censored, 
  number of missing observations removed prior to performing the test (if present), 
  value of the test statistic, 
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
  \link{Censored Data}, \code{\link{htestCensored.object}}, 
  \code{\link[base]{print}}.
}
\keyword{print}