\name{Millard.Deverel.88.df}
\alias{Millard.Deverel.88.df}
\docType{data}
\title{
 Copper and Zinc Concentrations in Shallow Ground Water
}
\description{
  Copper and zinc concentrations (mg/L) in shallow ground water from two geological 
  zones (Alluvial Fan and Basin-Trough) in the San Joaquin Valley, CA.  There are 68 
  samples from the Alluvial Fan zone and 50 from the Basin-Trough zone.  Some 
  observations are reported as <\eqn{DL}, where \eqn{DL} denotes a detection limit.  There 
  are multiple detection limits for both the copper and zinc data in each of the 
  geological zones.
}
\usage{Millard.Deverel.88.df}
\format{
  A data frame with 118 observations on the following 8 variables.
  \describe{
    \item{\code{Cu.orig}}{a character vector of original copper concentrations (mg/L)}
    \item{\code{Cu}}{a numeric vector of copper concentrations with nondetects coded to their detection limit}
    \item{\code{Cu.censored}}{a logical vector indicating which copper concentrations are censored}
    \item{\code{Zn.orig}}{a character vector of original zinc concentrations (mg/L)}
    \item{\code{Zn}}{a numeric vector of zinc concentrations with nondetects coded to their detection limit}
    \item{\code{Zn.censored}}{a logical vector indicating which zinc concentrations are censored}
    \item{\code{Zone}}{a factor indicating the zone (alluvial fan vs. basin trough)}
    \item{\code{Location}}{a numeric vector indicating the sampling location}
  }
}
\source{
  Millard, S.P., and S.J. Deverel. (1988). Nonparametric Statistical Methods for Comparing Two Sites Based on Data With Multiple Nondetect Limits. 
  \emph{Water Resources Research}, \bold{24}(12), 2087-2098.
}
\references{
  Deverel, S.J., R.J. Gilliom, R. Fujii, J.A. Izbicki, and J.C. Fields. (1984). 
  \emph{Areal Distribution of Selenium and Other Inorganic Constituents in Shallow Ground Water of the San Luis Drain Service Area, San Joaquin, California: A Preliminary Study}. 
  U.S. Geological Survey Water Resources Investigative Report 84-4319.
}
\keyword{datasets}
