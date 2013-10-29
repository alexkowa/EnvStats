\name{EPA.92c.benzene1.df}
\alias{EPA.92c.benzene1.df}
\docType{data}
\title{
  Benzene Concentrations from 1992 USEPA Guidance Document
}
\description{
  Benzene concentrations (ppb) at six background wells 
  (six monthly samples for each well).  Nondetects reported as \code{<2}. 
}
\usage{EPA.92c.benzene1.df}
\format{
  A data frame with 36 observations on the following 5 variables.
  \describe{
    \item{\code{Benzene.orig}}{a character vector of original benzene concentrations (ppb)}
    \item{\code{Benzene}}{a numeric vector of benzene concentrations with \code{<2} coded as \code{2}}
    \item{\code{Censored}}{a logical vector indicating which observations are censored}
    \item{\code{Month}}{a factor indicating the month of collection}
    \item{\code{Well}}{a factor indicating the well number}
  }
}
\source{
  USEPA. (1992c). \emph{Statistical Analysis of Ground-Water Monitoring Data at RCRA Facilities: Addendum to Interim Final Guidance}. Office of Solid Waste, U.S. Environmental Protection Agency, Washington, D.C.  p.36.
}
\keyword{datasets}
