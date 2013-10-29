\name{EPA.89b.sulfate.df}
\alias{EPA.89b.sulfate.df}
\docType{data}
\title{
  Sulfate Concentrations from 1989 USEPA Guidance Document
}
\description{
  Sulfate concentrations (mg/L).  Nondetects reported as \code{<1450}. 
}
\usage{data(EPA.89b.sulfate.df)}
\format{
  A data frame with 24 observations on the following 3 variables.
  \describe{
    \item{\code{Sulfate.orig}}{a character vector of original sulfate concentration (mg/L)}
    \item{\code{Sulfate}}{a numeric vector of sulfate concentations with \code{<1450} coded as \code{1450}}
    \item{\code{Censored}}{a logical vector indicating which observations are censored}
  }
}
\source{
  USEPA. (1989b). \emph{Statistical Analysis of Ground-Water Monitoring Data at RCRA Facilities, Interim Final Guidance}. 
  EPA/530-SW-89-026. Office of Solid Waste, U.S. Environmental Protection Agency, Washington, D.C. p.8-9.
}
\keyword{datasets}
