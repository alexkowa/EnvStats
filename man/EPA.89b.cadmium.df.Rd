\name{EPA.89b.cadmium.df}
\alias{EPA.89b.cadmium.df}
\alias{Cadmium}
\docType{data}
\title{
  Cadmium Concentrations from 1989 USEPA Guidance Document
}
\description{
  Cadmium concentrations (mg/L) at one set of background and one set of compliance 
  wells.  Nondetects reported as "BDL".  Detection limit not given.
}
\usage{EPA.89b.cadmium.df}
\format{
  A data frame with 88 observations on the following 4 variables.
  \describe{
    \item{\code{Cadmium.orig}}{a character vector of the original cadmium observations (mg/L)}
    \item{\code{Cadmium}}{a numeric vector with \code{BDL} coded as \code{0}}
    \item{\code{Censored}}{a logical vector indicating which observations are censored}
    \item{\code{Well.type}}{a factor indicating the well type (background vs. compliance)}
  }
}
\source{
  USEPA. (1989b). \emph{Statistical Analysis of Ground-Water Monitoring Data at RCRA Facilities, Interim Final Guidance}. EPA/530-SW-89-026. Office of Solid Waste, U.S. Environmental Protection Agency, Washington, D.C. p.8-6.
}
\keyword{datasets}
