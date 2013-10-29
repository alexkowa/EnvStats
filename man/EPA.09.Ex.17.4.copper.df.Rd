\name{EPA.09.Ex.17.4.copper.df}
\alias{EPA.09.Ex.17.4.copper.df}
\docType{data}
\title{
  Copper Concentrations from Example 17-4 of 2009 USEPA Guidance Document
}
\description{
  Copper concentrations (ppb) at three background and two compliance wells 
  (eight monthly measures at the background wells, four monthly measures at the compliance wells). 
}
\usage{EPA.09.Ex.17.4.copper.df}
\format{
  A data frame with 40 observations on the following 6 variables.
  \describe{
    \item{\code{Month}}{a factor indicating the month of collection}
    \item{\code{Well}}{a factor indicating the well number}
    \item{\code{Well.type}}{a factor indicating the well type (background vs. compliance)}
    \item{\code{Copper.ppb.orig}}{a character vector of original copper concentrations (ppb)}
    \item{\code{Copper.ppb}}{a numeric vector of copper concentrations with nondetects set to their detection limit}
    \item{\code{Censored}}{a logical vector indicating which observations are censored}
  }
}
\source{
  USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
  EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.  
  U.S. Environmental Protection Agency, Washington, D.C. p.17-21.

  USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
  EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
  U.S. Environmental Protection Agency, Washington, D.C. 
}
\keyword{datasets}
