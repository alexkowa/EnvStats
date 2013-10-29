\name{Refinery.CO.df}
\alias{Refinery.CO.df}
\docType{data}
\title{
  Carbon Monoxide Emissions from Oil Refinery.
}
\description{
  Carbon monoxide (CO) emissions (ppm) from an oil refinery near San Francisco.  
  The refinery submitted 31 daily measurements from its stack for the period 
  April 16, 1993 through May 16, 1993 to the Bay Area Air Quality Management 
  District (BAAQMD).  The BAAQMD made nine of its own indepent measurements for 
  the period September 11, 1990 through March 30, 1993.
}
\usage{data(Refinery.CO.df)}
\format{
  A data frame with 40 observations on the following 3 variables.
  \describe{
    \item{\code{CO.ppm}}{a numeric vector of CO emissions (ppm)}
    \item{\code{Source}}{a factor indicating the source of the measurment (\code{BAAQMD} or \code{refinery}}
    \item{\code{Date}}{a Date object indicating the date the measurement was taken}
  }
}
\source{
  Data and Story Library, http://lib.stat.cmu.edu/DASL/Datafiles/Refinery.html.
}
\references{
  Zou, G.Y., C.Y. Huo, and J. Taleban. (2009).  Simple Confidence Intervals for 
  Lognormal Means and their Differences with Environmental Applications.  
  \emph{Environmetrics}, \bold{20}, 172--180.
}
\keyword{datasets}
