\name{EPA.09.Ex.15.1.manganese.df}
\alias{EPA.09.Ex.15.1.manganese.df}
\docType{data}
\title{
  Manganese Concentrations from Example 15-1 of 2009 USEPA Guidance Document
}
\description{
  Manganese concentrations (ppb) at five background wells (five measures at each well). 
}
\usage{EPA.09.Ex.15.1.manganese.df}
\format{
  A data frame with 25 observations on the following 5 variables.
  \describe{
    \item{\code{Sample}}{a numeric vector indicating the sample number (1-5)}
    \item{\code{Well}}{a factor indicating the well number}
    \item{\code{Manganese.Orig.ppb}}{a character vector of the original manganese concentrations (ppb)}
    \item{\code{Manganese.ppb}}{a numeric vector of manganese concentrations with non-detects coded to their detecion limit}
    \item{\code{Censored}}{a logical vector indicating which observations are censored}
  }
}
\source{
  USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
  EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.  
  U.S. Environmental Protection Agency, Washington, D.C. p.15-10.

  USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
  EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
  U.S. Environmental Protection Agency, Washington, D.C. 
}
\keyword{datasets}
