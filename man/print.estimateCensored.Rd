\name{print.estimateCensored}
\alias{print.estimateCensored}
\title{
  Print Objects of Class "estimateCensored"
}
\description{
  Formats and prints the results of \pkg{EnvStats} functions that estimate 
  the parameters or quantiles of a probability distribution and optionally 
  construct confidence, prediction, or tolerance intervals based on a sample 
  of Tyep I censored data assumed to come from that distribution.  
  This method is automatically called by \code{\link{print}} when given an 
  object of class \code{"estimateCensored"}.

  See the subsections \emph{Estimating Distribution Parameters} and 
  \emph{Estimating Distribution Quantiles} in the help file 
  \link{Censored Data} for lists of functions that estimate 
  distribution parameters and quantiles based on Type I censored data.

  See the subsection \emph{Prediction and Tolerance Intervals}  
  in the help file \link{Censored Data} for lists of functions 
  that create prediction and tolerance intervals.
}
\usage{
\method{print}{estimateCensored}(x, show.cen.levels = TRUE, 
  pct.censored.digits = .Options$digits, 
  conf.cov.sig.digits = .Options$digits, limits.sig.digits = .Options$digits, 
  ...) 
}
\arguments{
  \item{x}{
  an object of class \code{"estimateCensored"}.  See 
  \code{\link{estimateCensored.object}} for details.
}
  \item{show.cen.levels}{
  logical scalar indicating whether to print the censoring levels.  The default is 
  \code{show.cen.levels=TRUE}.
}
  \item{pct.censored.digits}{
  numeric scalar indicating the number of significant digits to print for the 
  percent of censored observations.
}
  \item{conf.cov.sig.digits}{
  numeric scalar indicating the number of significant digits to print for the 
  confidence level or coverage of a confidence, prediction, or tolerance interval.
}
  \item{limits.sig.digits}{
  numeric scalar indicating the number of significant digits to print for the upper 
  and lower limits of a confidence, prediction, or tolerance interval.
}
  \item{\dots}{
  arguments that can be supplied to the \code{\link[base]{format}} function.
}
}
\details{
  This is the \code{"estimateCensored"} method for the generic function 
  \code{\link[base]{print}}.  
  Prints estimated parameters and, if present in the object, information regarding 
  confidence, prediction, or tolerance intervals.
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
  \code{\link{estimateCensored.object}}, 
  \link{Censored Data}, \code{\link[base]{print}}.
}
\keyword{print}
