\name{EPA.09.Ex.17.6.sulfate.df}
\alias{EPA.09.Ex.17.6.sulfate.df}
\docType{data}
\title{
  Sulfate Concentrations from Example 17-6 of 2009 USEPA Guidance Document
}
\description{
  Sulfate concentrations (ppm) collected over several years. 
  The date of collection is simply indicated by month and year of collection.
  The column \code{Date} is a Date object where the day of the month has been arbitrarily set to 1.
}
\usage{EPA.09.Ex.17.6.sulfate.df}
\format{
  A data frame with 23 observations on the following 6 variables.
  \describe{
    \item{\code{Sample.No}}{a numeric vector indicating the sample number}
    \item{\code{Year}}{a numeric vector indicating the year of collection}
    \item{\code{Month}}{a numeric vector indicating the month of collection}
    \item{\code{Sampling.Date}}{a numeric vector indicating the year and month of collection}
    \item{\code{Date}}{a Date object indicating the date of collection, where the day of the month is arbitrarily set to 1}
    \item{\code{Sulfate.ppm}}{a numeric vector of sulfate concentrations (ppm)}
  }
}
\source{
  USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
  EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.  
  U.S. Environmental Protection Agency, Washington, D.C. p.17-33.

  USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
  EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
  U.S. Environmental Protection Agency, Washington, D.C. 
}
\keyword{datasets}
