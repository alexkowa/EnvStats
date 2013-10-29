\name{print.boxcoxCensored}
\alias{print.boxcoxCensored}
\title{
  Print Output of Objective for Box-Cox Power Transformations Based on Type I Censored Data
}
\description{
  Formats and prints the results of calling the function \code{\link{boxcoxCensored}}.  
  This method is automatically called by \code{\link{print}} when given an 
  object of class \code{"boxcoxCensored"}.
}
\usage{
\method{print}{boxcoxCensored}(x, ...)
}
\arguments{
  \item{x}{
  an object of class \code{"boxcoxCensored"}.  See \code{\link{boxcoxCensored.object}} 
  for details.
}
  \item{\dots}{
  arguments that can be supplied to the \code{\link[base]{format}} function.
}
}
\details{
  This is the \code{"boxcoxCensored"} method for the generic function 
  \code{\link[base]{print}}.  
  Prints the objective name, the name of the data object used, the sample size, 
  the percentage of censored observations, the values of the powers, and the 
  values of the objective.  In the case of 
  optimization, also prints the range of powers over which the optimization 
  took place.
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
  \code{\link{boxcoxCensored}}, \code{\link{boxcoxCensored.object}}, 
  \code{\link{plot.boxcoxCensored}}, 
  \link{Data Transformations}, \code{\link[base]{print}}.
}
\keyword{print}
