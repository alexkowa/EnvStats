\name{Modified.TcCB.df}
\alias{Modified.TcCB.df}
\docType{data}
\title{
  Modified 1,2,3,4-Tetrachlorobenzene Data with Censored Values
}
\description{
  Artificial 1,2,3,4-Tetrachlorobenzene (TcCB) concentrations with censored values; 
  based on the reference area data stored in \code{\link{EPA.94b.tccb.df}}.  The data 
  frame \code{\link{EPA.94b.tccb.df}} contains TcCB concentrations (ppb) in soil samples 
  at a reference area and a cleanup area.  The data frame \cr
  \code{Modified.TcCB.df} contains a modified version of the data from the reference area.  
  For this data set, the concentrations of TcCB less than 0.5 ppb have been recoded as 
  \code{<0.5}.
}
\usage{Modified.TcCB.df}
\format{
  A data frame with 47 observations on the following 3 variables.
  \describe{
    \item{\code{TcCB.orig}}{a character vector of original TcCB concentrations (ppb)}
    \item{\code{TcCB}}{a numeric vector with censored observations set to their detection level}
    \item{\code{Censored}}{a logical vector indicating which observations are censored}
  }
}
\source{
  Millard, S.P., and N.K. Neerchal. (2001). \emph{Environmental Statistics with S-PLUS}. 
  CRC Press, Boca Raton, FL, p.595.
}
\references{
  USEPA. (1994b). \emph{Statistical Methods for Evaluating the Attainment of Cleanup Standards, Volume 3: Reference-Based Standards for Soils and Solid Media}. 
  EPA/230-R-94-004. Office of Policy, Planning, and Evaluation, U.S. Environmental Protection Agency, Washington, D.C.
}
\seealso{
  \code{\link{EPA.94b.tccb.df}}.
}
\keyword{datasets}
