\name{Helsel.Cohn.88.silver.df}
\alias{Helsel.Cohn.88.silver.df}
\docType{data}
\title{
  Silver Concentrations From An Interlab Comparison
}
\description{
  Silver concentrations (mg/L) from an interlab comparison.  There are 34 observations 
  out of a total of 56 that are reported as <DL, where DL denotes a detection limit.  There 
  are 12 distinct detection limits.
}
\usage{Helsel.Cohn.88.silver.df}
\format{
  A data frame with 56 observations on the following 4 variables.
  \describe{
    \item{\code{Ag.orig}}{a character vector of original silver concentrations (mg/L)}
    \item{\code{Ag}}{a numeric vector with nondetects coded to the detection limit}
    \item{\code{Censored}}{a logical vector indicating which observations are censored}
    \item{\code{log.Ag}}{the natural logarithm of \code{Ag}}
  }
}
\source{
  Helsel, D.R., and T.A. Cohn. (1988). Estimation of Descriptive Statistics for Multiply Censored Water Quality Data. 
  \emph{Water Resources Research} \bold{24}(12), 1997--2004.
}
\references{
  Janzer, V.J. (1986). \emph{Report of the U.S. Geological Survey's Analytical Evaluation Program--Standard Reference Water Samples M6, M94, T95, N16, P8, and SED3}. 
  Technical Report, Branch of Quality Assurance, U.S. Geological Survey, Arvada, CO.
}
\keyword{datasets}
