\name{EPA.09.Ex.16.5.PCE.df}
\alias{EPA.09.Ex.16.5.PCE.df}
\docType{data}
\title{
  Tetrachloroethylene Concentrations from Example 16-5 of 2009 USEPA Guidance Document
}
\description{
  Tetrachloroethylene (PCE) concentrations (ppb) at one background well and one compliance well. 
}
\usage{EPA.09.Ex.16.5.PCE.df}
\format{
  A data frame with 14 observations on the following 4 variables.
  \describe{
    \item{\code{Well.type}}{a factor with levels \code{Background} \code{Compliance}}
    \item{\code{PCE.Orig.ppb}}{a character vector of original PCE concentrations (ppb)}
    \item{\code{PCE.ppb}}{a numeric vector of PCE concentrations (ppb) with nondetects set to their detection limit}
    \item{\code{Censored}}{a logical vector indicating which observations are censored}
  }
}
\source{
  USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
  EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.  
  U.S. Environmental Protection Agency, Washington, D.C. p.16-22.

  USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
  EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
  U.S. Environmental Protection Agency, Washington, D.C. 
}
\keyword{datasets}
