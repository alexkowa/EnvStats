\name{EPA.09.Ex.17.2.toluene.df}
\alias{EPA.09.Ex.17.2.toluene.df}
\docType{data}
\title{
  Toluene Concentrations from Example 17-2 of 2009 USEPA Guidance Document
}
\description{
  Toluene concentrations (ppb) at two background and three compliance wells (five monthly measures at each well). 
}
\usage{EPA.09.Ex.17.2.toluene.df}
\format{
  A data frame with 25 observations on the following 6 variables.
  \describe{
    \item{\code{Month}}{a factor indicating the month of collection}
    \item{\code{Well}}{a factor indicating the well number}
    \item{\code{Well.type}}{a factor indicating the well type (background vs. compliance)}
    \item{\code{Toluene.ppb.orig}}{a character vector of original toluene concentrations (ppb)}
    \item{\code{Toluene.ppb}}{a numeric vector of toluene concentrations (ppb) with nondetects set to their detection limit}
    \item{\code{Censored}}{a logical vector indicating which observations are censored}
  }
}
\source{
  USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
  EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.  
  U.S. Environmental Protection Agency, Washington, D.C. p.17-13.

  USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
  EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
  U.S. Environmental Protection Agency, Washington, D.C. 
}
\keyword{datasets}
