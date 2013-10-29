\name{EPA.09.Ex.19.5.mercury.df}
\alias{EPA.09.Ex.19.5.mercury.df}
\docType{data}
\title{
  Mercury Concentrations from Example 19-5 of 2009 USEPA Guidance Document
}
\description{
  Mercury concentrations (ppb) at four background and two compliance wells. 
}
\usage{EPA.09.Ex.19.5.mercury.df}
\format{
  A data frame with 36 observations on the following 6 variables.
  \describe{
    \item{\code{Event}}{a factor indicating the time of collection}
    \item{\code{Well}}{a factor indicating the well number}
    \item{\code{Well.type}}{a factor indicating the well type (background vs. compliance)}
    \item{\code{Mercury.ppb.orig}}{a character vector of original mercury concentrations (ppb)}
    \item{\code{Mercury.ppb}}{a numeric vector of mercury concentrations (ppb) with nondetects set to their detection limit}
    \item{\code{Censored}}{a logical vector indicating which observations are censored}
  }
}
\source{
  USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
  EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.  
  U.S. Environmental Protection Agency, Washington, D.C. p.19-33.

  USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
  EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
  U.S. Environmental Protection Agency, Washington, D.C. 
}
\keyword{datasets}
