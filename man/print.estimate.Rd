\name{print.estimate}
\alias{print.estimate}
\title{
  Print Objects of Class "estimate"
}
\description{
  Formats and prints the results of \pkg{EnvStats} functions that estimate 
  the parameters or quantiles of a probability distribution and optionally 
  construct confidence, prediction, or tolerance intervals based on a sample 
  of data assumed to come from that distribution.  
  This method is automatically called by \code{\link{print}} when given an 
  object of class \code{"estimate"}.

  See the help files 
  \link{Estimating Distribution Parameters} and 
  \link{Estimating Distribution Quantiles} 
  for lists of functions that estimate distribution parameters 
  and quantiles.  See the help files \link{Prediction Intervals} 
  and \link{Tolerance Intervals} for lists of functions 
  that create prediction and tolerance intervals.
}
\usage{
\method{print}{estimate}(x, conf.cov.sig.digits = .Options$digits, 
  limits.sig.digits = .Options$digits, ...) 
}
\arguments{
  \item{x}{
  an object of class \code{"estimate"}.  See \code{\link{estimate.object}} for details.
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
  This is the \code{"estimate"} method for the generic function 
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
  \code{\link{estimate.object}}, 
  \link{Estimating Distribution Parameters}, 
  \link{Estimating Distribution Quantiles}, 
  \link{Prediction Intervals}, \link{Tolerance Intervals}, 
  \code{\link[base]{print}}.
}
\keyword{print}
