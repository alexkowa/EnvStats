\name{EPA.09.Ex.17.1.loglead.df}
\alias{EPA.09.Ex.17.1.loglead.df}
\docType{data}
\title{
  Log-transformed Lead Concentrations from Example 17-1 of 2009 USEPA Guidance Document
}
\description{
  Log-transformed lead concentrations (ppb) at two background and four compliance wells (four quarterly measures at each well). 
}
\usage{EPA.09.Ex.17.1.loglead.df}
\format{
  A data frame with 24 observations on the following 4 variables.
  \describe{
    \item{\code{Month}}{a factor indicating the month of collection; \code{1} = Jan, \code{2} = Apr, \code{3} = Jul, \code{4} = Oct}
    \item{\code{Well}}{a factor indicating the well number}
    \item{\code{Well.type}}{a factor indicating the well type (background vs. compliance)}
    \item{\code{LogLead}}{a numeric vector of log-transformed lead concentrations (ppb)}
  }
}
\source{
  USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
  EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.  
  U.S. Environmental Protection Agency, Washington, D.C. p.17-7.

  USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
  EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
  U.S. Environmental Protection Agency, Washington, D.C. 
}
\keyword{datasets}
