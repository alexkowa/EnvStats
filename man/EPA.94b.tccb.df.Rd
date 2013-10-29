\name{EPA.94b.tccb.df}
\alias{EPA.94b.tccb.df}
\docType{data}
\title{
  1,2,3,4-Tetrachlorobenzene Concentrations from 1994 USEPA Guidance Document
}
\description{
  1,2,3,4-Tetrachlorobenzene (TcCB) concentrations (ppb) in soil samples at a 
  reference area and a cleanup area.  There are 47 observations for the reference area 
  and 77 for the cleanup area.  There is only one nondetect in the dataset (it's in the 
  cleanup area), and it is reported as \code{ND}.  Here it is assumed the nondetect is 
  less than the smallest reported value, which is 0.09 ppb.  Note that on page 6.23 of 
  USEPA (1994b), a value of 25.5 for the Cleanup Unit was erroneously omitted. 
}
\usage{EPA.94b.tccb.df}
\format{
  A data frame with 124 observations on the following 4 variables.
  \describe{
    \item{\code{TcCB.orig}}{a character vector with the original tetrachlorobenzene concentrations (ppb)}
    \item{\code{TcCB}}{a numeric vector of tetrachlorobenzene with \code{<0.99} coded as \code{0.99}}
    \item{\code{Censored}}{a logical vector indicating which observations are censored}
    \item{\code{Area}}{a factor indicating the area (cleanup vs. reference)}
  }
}
\source{
  USEPA. (1994b). \emph{Statistical Methods for Evaluating the Attainment of Cleanup Standards, Volume 3: Reference-Based Standards for Soils and Solid Media}. 
  EPA/230-R-94-004. Office of Policy, Planning, and Evaluation, U.S. Environmental Protection Agency, Washington, D.C. pp.6.22-6.25. 
}
\keyword{datasets}
