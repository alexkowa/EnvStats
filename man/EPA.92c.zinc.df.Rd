\name{EPA.92c.zinc.df}
\alias{EPA.92c.zinc.df}
\docType{data}
\title{
  Zinc Concentrations from 1992 USEPA Guidance Document
}
\description{
  Zinc concentrations (ppb) at five background wells 
  (eight samples for each well).  Nondetects reported as \code{<7}. 
}
\usage{EPA.92c.zinc.df}
\format{
  A data frame with 40 observations on the following 5 variables.
  \describe{
    \item{\code{Zinc.orig}}{a character vector of original zinc concentrations (ppb)}
    \item{\code{Zinc}}{a numeric vector of zinc concentrations with \code{<7} coded as \code{7}}
    \item{\code{Censored}}{a logical vector indicating which observations are censored}
    \item{\code{Sample}}{a factor indicating the sample number}
    \item{\code{Well}}{a factor indicating the well number}
  }
}
\source{
  USEPA. (1992c). \emph{Statistical Analysis of Ground-Water Monitoring Data at RCRA Facilities: Addendum to Interim Final Guidance}. Office of Solid Waste, U.S. Environmental Protection Agency, Washington, D.C.  p.30.
}
\keyword{datasets}
