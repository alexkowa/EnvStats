\name{EPA.09.Ex.21.6.nitrate.df}
\alias{EPA.09.Ex.21.6.nitrate.df}
\docType{data}
\title{
  Nitrate Concentrations from Example 21-6 of 2009 USEPA Guidance Document
}
\description{
  Nitrate concentrations (mg/L) at a well used for drinking water. 
}
\usage{EPA.09.Ex.21.6.nitrate.df}
\format{
  A data frame with 12 observations on the following 5 variables.
  \describe{
    \item{\code{Sampling.Date}}{a character vector indicating the sampling date}
    \item{\code{Date}}{a Date object indicating the sampling date}
    \item{\code{Nitrate.mg.per.l.orig}}{a character vector of original nitrate concentrations (mg/L)}
    \item{\code{Nitrate.mg.per.l}}{a numeric vector of nitrate concentrations (mg/L) with nondetects set to their detection limit}
    \item{\code{Censored}}{a logical vector indicating which observations are censored}
  }
}
\source{
  USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
  EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.  
  U.S. Environmental Protection Agency, Washington, D.C. p.21-22.

  USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
  EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
  U.S. Environmental Protection Agency, Washington, D.C. 
}
\keyword{datasets}
