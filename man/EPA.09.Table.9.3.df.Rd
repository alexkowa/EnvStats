\name{EPA.09.Table.9.3.df}
\alias{EPA.09.Table.9.3.df}
\docType{data}
\title{
  Arsenic, Mercury and Strontium Concentrations in Table 9-3 of 2009 USEPA Guidance Document
}
\description{
  Arsenic, mercury, and strontium concentrations (mg/L) from a single well 
  collected approximately quarterly.  Nondetects are indicated by the 
  data qualifier \code{U}.
}
\usage{EPA.09.Table.9.3.df}
\format{
  A data frame with 15 observations on the following 8 variables.
  \describe{
    \item{\code{Date.Collected}}{a factor indicating the date of collection}
    \item{\code{Date}}{a Date object indicating the date of collection}
    \item{\code{Arsenic.mg.per.L}}{a numeric vector of arsenic concentrations (mg/L)}
    \item{\code{Arsenic.Data.Qualifier}}{a factor indicating the data qualifier for arsenic}
    \item{\code{Mercury.mg.per.L}}{a numeric vector of mercury concentrations (mg/L)}
    \item{\code{Mercury.Data.Qualifier}}{a factor indicating the data qualifier for mercury}
    \item{\code{Strontium.mg.per.L}}{a numeric vector of strontium concentrations}
    \item{\code{Strontium.Data.Qualifier}}{a factor indicating the data qualifier for strontium}
  }
}
\source{
  USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
  EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.  
  U.S. Environmental Protection Agency, Washington, D.C. p.9-13.

  USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
  EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
  U.S. Environmental Protection Agency, Washington, D.C. 
}
\keyword{datasets}
