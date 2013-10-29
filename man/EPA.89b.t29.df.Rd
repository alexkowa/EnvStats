\name{EPA.89b.t29.df}
\alias{EPA.89b.t29.df}
\docType{data}
\title{
  T-29 Concentrations from 1989 USEPA Guidance Document
}
\description{
  T-29 concentrations (ppm) at two compliance wells (four monthly samples at each well, 
  four replicates within each month).  Detection limit is not given. 
}
\usage{EPA.89b.t29.df}
\format{
  A data frame with 32 observations on the following 6 variables.
  \describe{
    \item{\code{T29.orig}}{a character vector of the original T-29 concentrations (ppm)}
    \item{\code{T29}}{a numeric vector of T-29 concentrations with \code{<?} coded as \code{0}}
    \item{\code{Censored}}{a logical vector indicating which observations are censored}
    \item{\code{Month}}{a factor indicating the month of collection}
    \item{\code{Replicate}}{a factor indicating the replicate number}
    \item{\code{Well}}{a factor indicating the well number}
  }
}
\source{
  USEPA. (1989b). \emph{Statistical Analysis of Ground-Water Monitoring Data at RCRA Facilities, Interim Final Guidance}. 
  EPA/530-SW-89-026. Office of Solid Waste, U.S. Environmental Protection Agency, Washington, D.C. p.6-10.
}
\keyword{datasets}
