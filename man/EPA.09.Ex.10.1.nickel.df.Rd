\name{EPA.09.Ex.10.1.nickel.df}
\alias{EPA.09.Ex.10.1.nickel.df}
\docType{data}
\title{
  Nickel Concentrations from Example 10-1 of 2009 USEPA Guidance Document
}
\description{
  Nickel concentrations (ppb) from four wells (five observations per year for each well).
  The Guidance Document has the label \dQuote{Year} instead of \dQuote{Well};  
  corrected in Errata.
}
\usage{EPA.09.Ex.10.1.nickel.df}
\format{
  A data frame with 20 observations on the following 3 variables.
  \describe{
    \item{\code{Month}}{a numeric vector indicating the month the sample was taken}
    \item{\code{Well}}{a factor indicating the well number}
    \item{\code{Nickel.ppb}}{a numeric vector of nickel concentrations (ppb)}
  }
}
\source{
  USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
  EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery, Program Implementation and Information Division.  
  U.S. Environmental Protection Agency, Washington, D.C., p.10-12.

  USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
  EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
  U.S. Environmental Protection Agency, Washington, D.C. 
}
\keyword{datasets}
