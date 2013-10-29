\name{EPA.89b.benzene.df}
\alias{EPA.89b.benzene.df}
\alias{Benzene}
\docType{data}
\title{
  Benzene Concentrations from 1989 USEPA Guidance Document
}
\description{
  Benzene concentrations (ppm) at one background and five compliance wells (four monthly samples for each well).
}
\usage{EPA.89b.benzene.df}
\format{
  A data frame with 24 observations on the following 6 variables.
  \describe{
    \item{\code{Benzene.orig}}{a character vector of the original observations}
    \item{\code{Benzene}}{a numeric vector with \code{<1} observations coded as \code{1}}
    \item{\code{Censored}}{a logical vector indicating which observations are censored}
    \item{\code{Month}}{a factor indicating the month of collection}
    \item{\code{Well}}{a factor indicating the well number}
    \item{\code{Well.type}}{a factor indicating the well type (background vs. compliance)}
  }
}
\source{
  USEPA. (1989b). \emph{Statistical Analysis of Ground-Water Monitoring Data at RCRA Facilities, Interim Final Guidance}. EPA/530-SW-89-026. Office of Solid Waste, U.S. Environmental Protection Agency, Washington, D.C. p.5-18.
}
\keyword{datasets}
