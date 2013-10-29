\name{EPA.89b.loglead.df}
\alias{EPA.89b.loglead.df}
\docType{data}
\title{
  Log-transformed Lead Concentrations from 1989 USEPA Guidance Document
}
\description{
  Log-transformed lead concentrations (\eqn{\mu}g/L) at two background and four 
  compliance wells (four monthly samples for each well). 
}
\usage{EPA.89b.loglead.df}
\format{
  A data frame with 24 observations on the following 4 variables.
  \describe{
    \item{\code{LogLead}}{Natural logarithm of lead concentrations (\eqn{\mu}g/L)}
    \item{\code{Month}}{a factor indicating the month of collection}
    \item{\code{Well}}{a factor indicating the well number}
    \item{\code{Well.type}}{a factor indicating the well type (background vs. compliance)}
  }
}
\source{
  USEPA. (1989b). \emph{Statistical Analysis of Ground-Water Monitoring Data at RCRA Facilities, Interim Final Guidance}. EPA/530-SW-89-026. Office of Solid Waste, U.S. Environmental Protection Agency, Washington, D.C. p.5-11.
}
\keyword{datasets}
