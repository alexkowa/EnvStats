\name{EPA.09.Ex.21.2.benzene.df}
\alias{EPA.09.Ex.21.2.benzene.df}
\docType{data}
\title{
  Benzene Concentrations from Example 21-2 of 2009 USEPA Guidance Document
}
\description{
  Benzene concentrations (ppb) collected at a landfill that previously handled smelter waste and is now undergoing remediation efforts.
}
\usage{EPA.09.Ex.21.2.benzene.df}
\format{
  A data frame with 8 observations on the following 4 variables.
  \describe{
    \item{\code{Month}}{a numeric vector indicating the month of collection}
    \item{\code{Benzene.ppb.orig}}{a character vector of original benzene concentrations (ppb)}
    \item{\code{Benzene.ppb}}{a numeric vector of benzene concentrations (ppb) with nondetects set to their detection limit}
    \item{\code{Censored}}{a logical vector indicating which observations are censored}
  }
}
\source{
  USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
  EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.  
  U.S. Environmental Protection Agency, Washington, D.C. p.21-7.

  USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
  EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
  U.S. Environmental Protection Agency, Washington, D.C. 
}
\keyword{datasets}
