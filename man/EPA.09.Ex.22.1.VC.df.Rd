\name{EPA.09.Ex.22.1.VC.df}
\alias{EPA.09.Ex.22.1.VC.df}
\docType{data}
\title{
  Vinyl Chloride Concentrations from Example 22-1 of 2009 USEPA Guidance Document
}
\description{
  Vinyl Chloride (VC) concentrations (ppb) during detection monitoring for two 
  compliance wells.  Four years of quarterly measures at each well.  Compliance 
  monitoring began with Year 2 of the sampling record.
}
\usage{EPA.09.Ex.22.1.VC.df}
\format{
  A data frame with 32 observations on the following 5 variables.
  \describe{
    \item{\code{Year}}{a factor indicating the year of collection}
    \item{\code{Quarter}}{a factor indicating the quarter of collection}
    \item{\code{Period}}{a factor indicating the period (background vs. compliance)}
    \item{\code{Well}}{a factor indicating the well number}
    \item{\code{VC.ppb}}{a numeric vector of VC concentrations (ppb)}
  }
}
\source{
  USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
  EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.  
  U.S. Environmental Protection Agency, Washington, D.C. p.22-6.

  USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
  EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
  U.S. Environmental Protection Agency, Washington, D.C. 
}
\keyword{datasets}
