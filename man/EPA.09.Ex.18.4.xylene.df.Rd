\name{EPA.09.Ex.18.4.xylene.df}
\alias{EPA.09.Ex.18.4.xylene.df}
\docType{data}
\title{
  Xylene Concentrations from Example 18-4 of 2009 USEPA Guidance Document
}
\description{
  Xylene concentrations (ppb) at three background wells and one compliance well.  Eight 
  monthly measures at each complaince well; three monthly measures at the compliance well. 
}
\usage{EPA.09.Ex.18.4.xylene.df}
\format{
  A data frame with 32 observations on the following 6 variables.
  \describe{
    \item{\code{Month}}{a factor indicating the month of collection}
    \item{\code{Well}}{a factor indicating the well number}
    \item{\code{Well.type}}{a factor indicating the well type (background vs. compliance)}
    \item{\code{Xylene.ppb.orig}}{a character vector of original xylene concentrations (ppb)}
    \item{\code{Xylene.ppb}}{a numeric vector of xylene concentrations (ppb) with nondetects set to their detection limit}
    \item{\code{Censored}}{a logical vector indicating which observations are censored}
  }
}
\source{
  USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
  EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.  
  U.S. Environmental Protection Agency, Washington, D.C. p.18-22.

  USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
  EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
  U.S. Environmental Protection Agency, Washington, D.C. 
}
\keyword{datasets}
