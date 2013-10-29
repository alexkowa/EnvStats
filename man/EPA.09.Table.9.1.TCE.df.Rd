\name{EPA.09.Table.9.1.TCE.df}
\alias{EPA.09.Table.9.1.TCE.df}
\docType{data}
\title{
  Trichloroethene concentrations in Table 9.1 of 2009 USEPA Guidance Document
}
\description{
  Time series of trichloroethene (TCE) concentrations (mg/L) taken at 2 separate 
  wells.  Some observations are annotated with a data qualifier of \code{U} (nondetect) 
  or \code{J} (estimated detected concentration). 
}
\usage{EPA.09.Table.9.1.TCE.df}
\format{
  A data frame with 30 observations on the following 5 variables.
  \describe{
    \item{\code{Date.Collected}}{a factor indicating the date of collection}
    \item{\code{Date}}{a Date object indicating the date of collection}
    \item{\code{Well}}{a factor indicating the well number}
    \item{\code{TCE.mg.per.L}}{a numeric vector indicating the TCE concnetrations (mg/L)}
    \item{\code{Data.Qualifier}}{a factor indicating the data qualifier}
  }
}
\source{
  USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
  EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.  
  U.S. Environmental Protection Agency, Washington, D.C. p.9-3.

  USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
  EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
  U.S. Environmental Protection Agency, Washington, D.C. 
}
\keyword{datasets}
