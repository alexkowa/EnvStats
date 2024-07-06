\name{Beal.2010.Pb.df}
\alias{Beal.2010.Pb.df}
\docType{data}
\title{
  Lead concentration in soil samples.
}
\description{
  Lead (Pb) concentrations (mg/kg) in 29 discrete environmental soil samples 
  from a site suspected to be contaminated with lead.
}
\usage{
    Beal.2010.Pb.df
    data(Beal.2010.Pb.df)
}
\format{
  A data frame with 29 observations on the following 3 variables.
  \describe{
    \item{\code{Pb.char}}{Character vector indicating lead concentrations.  
      Nondetects indicated with the less-than sign (e.g., <1)}
    \item{\code{Pb}}{numeric vector indicating lead concentration.}
    \item{\code{Censored}}{logical vector indicating censoring status.}
  }
}
\source{
  Beal, D. (2010).  \emph{A Macro for Calculating Summary Statistics on
  Left Censored Environmental Data Using the Kaplan-Meier Method}.
  Paper SDA-09, presented at Southeast SAS Users Group 2010, September 26-28,
  Savannah, GA. \url{https://analytics.ncsu.edu/sesug/2010/SDA09.Beal.pdf}.
}
\keyword{datasets}
