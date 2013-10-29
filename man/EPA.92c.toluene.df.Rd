\name{EPA.92c.toluene.df}
\alias{EPA.92c.toluene.df}
\docType{data}
\title{
  Toluene Concentrations from 1992 USEPA Guidance Document
}
\description{
  Toluene concentrations (ppb) at two background and three compliance wells 
  (five monthly samples at each well).  Nondetects reported as \code{<5}. 
}
\usage{EPA.92c.toluene.df}
\format{
  A data frame with 25 observations on the following 6 variables.
  \describe{
    \item{\code{Toluene.orig}}{a character vector of original toluene concentrations (ppb)}
    \item{\code{Toluene}}{a numeric vector of toluene concentrations with \code{<5} coded as \code{5}}
    \item{\code{Censored}}{a logical vector indicating which observations are censored}
    \item{\code{Month}}{a factor indicating the month of collection}
    \item{\code{Well}}{a factor indicating the well number}
    \item{\code{Well.type}}{a factor indicating the well type (background vs. compliance)}
  }
}
\source{
  USEPA. (1992c). \emph{Statistical Analysis of Ground-Water Monitoring Data at RCRA Facilities: Addendum to Interim Final Guidance}. Office of Solid Waste, U.S. Environmental Protection Agency, Washington, D.C.  p.43.
}
\keyword{datasets}
