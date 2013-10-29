\name{EPA.92c.benzene2.df}
\alias{EPA.92c.benzene2.df}
\docType{data}
\title{
  Benzene Concentrations from 1992 USEPA Guidance Document
}
\description{
  Benzene concentrations (ppb) at one background and one compliance well.  Four 
  observations per month for each well.  Background well sampled in months 1,2, and 3; 
  compliance well sampled in months 4 and 5.
}
\usage{EPA.92c.benzene2.df}
\format{
  A data frame with 20 observations on the following 3 variables.
  \describe{
    \item{\code{Benzene}}{a numeric vector of benzene concentrations (ppb)}
    \item{\code{Month}}{a factor indicating the month of collection}
    \item{\code{Well.type}}{a factor indicating the well type (background vs. compliance)}
  }
}
\source{
  USEPA. (1992c). \emph{Statistical Analysis of Ground-Water Monitoring Data at RCRA Facilities: Addendum to Interim Final Guidance}. Office of Solid Waste, U.S. Environmental Protection Agency, Washington, D.C.  p.56.
}
\keyword{datasets}
