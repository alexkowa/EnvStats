\name{Olympic.NH4.df}
\alias{Olympic.NH4.df}
\docType{data}
\title{
  Ammonium Concentration in Precipitation Measured at Olympic National Park Hoh Ranger Station
}
\description{
  Ammonium (NH\eqn{_4}) concentration (mg/L) in precipitation measured at
  Olympic National Park, Hoh Ranger Station (WA14), weekly or every other week
  from January 6, 2009 through December 20, 2011.
}
\usage{Olympic.NH4.df}
\format{
  A data frame with 102 observations on the following 6 variables.
  \describe{
    \item{\code{Date.On}}{Start of collection period.
      Date on which the sample bucket was installed on the collector.}
    \item{\code{Date.Off}}{End of collection period.
      Date on which the sample bucket was removed from the collector.}
    \item{\code{Week}}{a numeric vector indicating the cumulative week number
      starting from January 1, 2009.}
    \item{\code{NH4.Orig.mg.per.L}}{a character vector of the original NH\eqn{_4}
      concentrations reported either as the observed value or less than some
      detection limit.  For values reported as less than a detection limit,
      the value reported is the actual limit of detection or, in the case of a
      diluted sample, the product of the detection limit value and the
      dilution factor.}
    \item{\code{NH4.mg.per.L}}{a numeric vector of NH\eqn{_4} concentrations with
      non-detects coded to their detection limit.}
    \item{\code{Censored}}{a logical vector indicating which observations are censored.}
  }
}
\details{
  \describe{
  \item{Station}{Olympic National Park-Hoh Ranger Station (WA14)}
  \item{Location}{Jefferson County, Washington}
  \item{Latitude}{47.8597}
  \item{Longitude}{-123.9325}
  \item{Elevation}{182 meters}
  \item{USGS 1:24000 Map Name}{Owl Mountain}
  \item{Operating Agency}{Olympic National Park}
  \item{Sponsoring Agency}{NPS-Air Resources Division}
  }
}
\source{
  National Atmospheric Deposition Program, National Trends Network (NADP/NTN). \cr
  \url{https://nadp.slh.wisc.edu/sites/ntn-WA14/}
}
\keyword{datasets}
