\name{Skagit.NH3_N.df}
\alias{Skagit.NH3_N.df}
\docType{data}
\title{
  Ammonia Nitrogen Concentrations in the Skagit River, Marblemount, Washington
}
\description{
  Ammonia nitrogen (NH\eqn{_3}---N) concentration (mg/L) in the Skagit River
  measured monthly from January 1978 through December 2010 at the
  Marblemount, Washington monitoring station.
}
\usage{Skagit.NH3_N.df}
\format{
  A data frame with 396 observations on the following 6 variables.
  \describe{
    \item{\code{Date}}{Date of collection.}
    \item{\code{NH3_N.Orig.mg.per.L}}{a character vector of the ammonia
      nitrogen concentrations where values for non-detects are preceeded
      with the less-than sign (<).}
    \item{\code{NH3_N.mg.per.L}}{a numeric vector of ammonia nitrogen
      concentrations; non-detects have been coded to their detection limit.}
    \item{\code{DQ1}}{factor of data qualifier values.
      \itemize{
        \item \code{U} = The analyte was not detected at or above the reported result.
        \item \code{J} = The analyte was positively identified. The associated numerical result is an estimate.
        \item \code{UJ} = The analyte was not detected at or above the reported estimated result.
      }
    }
    \item{\code{DQ2}}{factor of data qualifier values.
      An asterisk (\code{*}) indicates a possible quality problem for the result.}
    \item{\code{Censored}}{a logical vector indicating which observations are censored.}
  }
}
\details{
  Station 04A100 - Skagit R \@ Marblemount.
  Located at the bridge on the Casdace River Road where
  Highway 20 (North Cascades Highway) turns 90 degrees in Marblemount.
}
\source{
  Washington State Deparment of Ecology. \cr
  \url{https://ecology.wa.gov/Research-Data/Monitoring-assessment/River-stream-monitoring/Water-quality-monitoring/Using-river-stream-water-quality-data}
}
\keyword{datasets}
