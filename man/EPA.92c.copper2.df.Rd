\name{EPA.92c.copper2.df}
\alias{EPA.92c.copper2.df}
\docType{data}
\title{
  Copper Concentrations from 1992 USEPA Guidance Document
}
\description{
  Copper concentrations (ppb) at three background and two compliance wells 
  (eight monthly samples for each well; first four missing at compliance wells).  
  Nondetects reported as \code{<5}. 
}
\usage{EPA.92c.copper2.df}
\format{
  A data frame with 40 observations on the following 6 variables.
  \describe{
    \item{\code{Copper.orig}}{a character vector of original copper concentrations (ppb)}
    \item{\code{Copper}}{a numeric vector of copper concentrations with \code{<5} coded as \code{5}}
    \item{\code{Censored}}{a logical vector indicating which observations are censored}
    \item{\code{Month}}{a factor indicating the month of collection}
    \item{\code{Well}}{a factor indicating the well number}
    \item{\code{Well.type}}{a factor indicating the well type (background vs. compliance)}
  }
}
\source{
  USEPA. (1992c). \emph{Statistical Analysis of Ground-Water Monitoring Data at RCRA Facilities: Addendum to Interim Final Guidance}. Office of Solid Waste, U.S. Environmental Protection Agency, Washington, D.C.  p.55.
}
\keyword{datasets}
