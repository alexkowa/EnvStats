\name{calibrate.object}
\alias{calibrate.object}
\docType{class}
\title{
  S3 Class "calibrate"
}
\description{
  Objects of S3 class \code{"calibrate"} are returned by the \pkg{EnvStats} 
  function \code{\link{calibrate}}, which fits a calibration line or curve based 
  on linear regression.
}
\details{
  Objects of class \code{"calibrate"} are lists that inherit from 
  \code{\link{class}} \code{"\link{lm}"} and include a component called 
  \code{x} that stores the model matrix (the values of the predictor variables 
  for the final calibration model).
}
\section{Methods}{
  Generic functions that have methods for objects of class 
  \code{"calibrate"} include: \cr
  NONE AT PRESENT.
}
\value{
  See the help file for \code{\link{lm}}.  

  \strong{Required Components} \cr
  Besides the usual components in the list returned by the function \code{\link{lm}}, 
  the following components must be included in a legitimate list of 
  class \code{"calibrate"}.

  \item{x}{
    the model matrix from the linear model fit.}
}
\note{
  Since objects of class \code{"calibrate"} are lists, you may extract 
  their components with the \code{$} and \code{[[} operators.
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\seealso{
  \code{\link{calibrate}}, \code{\link{inversePredictCalibrate}}, 
  \code{\link{detectionLimitCalibrate}}.
}
\examples{
  # Create an object of class "calibrate", then print it out.

  # The data frame EPA.97.cadmium.111.df contains calibration data for 
  # cadmium at mass 111 (ng/L) that appeared in Gibbons et al. (1997b) 
  # and were provided to them by the U.S. EPA. 

  calibrate.list <- calibrate(Cadmium ~ Spike, data = EPA.97.cadmium.111.df) 
 
  names(calibrate.list)

  calibrate.list


  #----------

  # Clean up
  #---------
  rm(calibrate.list)
}
\keyword{classes}
