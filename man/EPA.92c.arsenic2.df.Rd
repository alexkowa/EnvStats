\name{EPA.92c.arsenic2.df}
\alias{EPA.92c.arsenic2.df}
\docType{data}
\title{
  Arsenic Concentrations from 1992 USEPA Guidance Document
}
\description{
  Arsenic concentrations (ppb) at three background wells and one compliance well 
  (six monthly samples for each well; first four missing at compliance well).  Nondetects 
  reported as \code{<5}. 
}
\usage{EPA.92c.arsenic2.df}
\format{
  A data frame with 24 observations on the following 6 variables.
  \describe{
    \item{\code{Arsenic.orig}}{a character vector of original arsenic concentrations (ppb)}
    \item{\code{Arsenic}}{a numeric vector of arsenic concentrations with \code{<5} coded as \code{5}}
    \item{\code{Censored}}{a logical vector indicating which observations are censored}
    \item{\code{Month}}{a factor indicating the month of collection}
    \item{\code{Well}}{a factor indicating the well number}
    \item{\code{Well.type}}{a factor indicating the well type (background vs. compliance)}
  }
}
\source{
  USEPA. (1992c). \emph{Statistical Analysis of Ground-Water Monitoring Data at RCRA Facilities: Addendum to Interim Final Guidance}. Office of Solid Waste, U.S. Environmental Protection Agency, Washington, D.C.  p.60.
}
\keyword{datasets}
