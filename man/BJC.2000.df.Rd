\name{BJC.2000.df}
\alias{BJC.2000.df}
\docType{data}
\title{
  Randomly sampled measurements of an analyte in soil samples.
}
\description{
  Analyte concentrations (\eqn{\mu}g/g) in 11 discrete environmental soil samples.
}
\usage{
    BJC.2000.df
    data(BJC.2000.df)
}
\format{
  A data frame with 11 observations on the following 4 variables.
  \describe{
    \item{\code{Analyte.char}}{Character vector indicating lead concentrations.  
      Nondetects indicated with the letter U after the measure (e.g., 0.10U)}
    \item{\code{Analyte}}{numeric vector indicating analyte concentration.}
    \item{\code{Censored}}{logical vector indicating censoring status.}
    \item{\code{Detect}}{numeric vector of 0s (nondetects) and 1s (detects) 
     indicating censoring status.}
  }
}
\source{
  BJC. (2000).  \emph{Improved Methods for Calculating Concentrations Used 
  in Exposure Assessments}. BJC/OR-416, Prepared by the Lockheed Martin 
  Energy Research Corporation. Prepared for the U.S. Department of 
  Energy Office of Environmental Management. Bechtel Jacobs Company, LLC. 
  January, 2000. \url{https://rais.ornl.gov/documents/bjc_or416.pdf}.
}
\keyword{datasets}
