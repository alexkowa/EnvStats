\name{EPA.94b.lead.df}
\alias{EPA.94b.lead.df}
\docType{data}
\title{
  Lead Concentrations from 1994 USEPA Guidance Document
}
\description{
  Lead concentrations (mg/Kg) in soil samples at a reference area and a 
  cleanup area.  Nondetects reported as \code{<39}.  There are 14 observations 
  for each area. 
}
\usage{EPA.94b.lead.df}
\format{
  A data frame with 28 observations on the following 4 variables.
  \describe{
    \item{\code{Lead.orig}}{a character vector of original lead concentrations (mg/Kg)}
    \item{\code{Lead}}{a numeric vector of lead concentrations with \code{<39} coded as \code{39}}
    \item{\code{Censored}}{a logical vector indicating which observations are censored}
    \item{\code{Area}}{a factor indicating the area (cleanup vs. reference)}
  }
}
\source{
  USEPA. (1994b). \emph{Statistical Methods for Evaluating the Attainment of Cleanup Standards, Volume 3: Reference-Based Standards for Soils and Solid Media}. 
  EPA/230-R-94-004. Office of Policy, Planning, and Evaluation, U.S. Environmental Protection Agency, Washington, D.C. pp.6.20--6.21. 
}
\keyword{datasets}
