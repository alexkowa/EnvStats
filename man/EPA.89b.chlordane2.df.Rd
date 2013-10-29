\name{EPA.89b.chlordane2.df}
\alias{EPA.89b.chlordane2.df}
\docType{data}
\title{
  Chlordane Concentrations from 1989 USEPA Guidance Document
}
\description{
  Chlordane concentrations (ppb) at one background and one compliance well.  Observations 
  taken during four separate months over two years.  Four replicates taken for each 
  \dQuote{month/year/well type} combination. 
}
\usage{data(EPA.89b.chlordane2.df)}
\format{
  A data frame with 32 observations on the following 5 variables.
  \describe{
    \item{\code{Chlordane}}{Chlordane concentration (ppb)}
    \item{\code{Month}}{a factor indicating the month of collection}
    \item{\code{Year}}{a numeric vector indicating the year of collection (85 or 86)}
    \item{\code{Replicate}}{a factor indicating the replicate number}
    \item{\code{Well.type}}{a factor indicating the well type (background vs. compliance)}
  }
}
\source{
  USEPA. (1989b). \emph{Statistical Analysis of Ground-Water Monitoring Data at RCRA Facilities, Interim Final Guidance}. EPA/530-SW-89-026. Office of Solid Waste, U.S. Environmental Protection Agency, Washington, D.C. p.5-27.
}
\keyword{datasets}
