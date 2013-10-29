\name{Helsel.Cohn.88.app.b.df}
\alias{Helsel.Cohn.88.app.b.df}
\docType{data}
\title{
  Example of Multiply Left-censored Data from Literature
}
\description{
  Made up multiply left-censored data.  There are 9 observations out of a total of 18 
  that are reported as <\eqn{DL}, where \eqn{DL} denotes a detection limit.  There are 
  2 distinct detection limits.
}
\usage{Helsel.Cohn.88.app.b.df}
\format{
  A data frame with 18 observations on the following 3 variables.
  \describe{
    \item{\code{Conc.orig}}{a character vector of original observations}
    \item{\code{Conc}}{a numeric vector of observations with censored values coded to censoring levels}
    \item{\code{Censored}}{a logical vector indicating which values are censored}
  }
}
\source{
  Helsel, D.R., and T.A. Cohn. (1988). Estimation of Descriptive Statistics for Multiply Censored Water Quality Data. 
  \emph{Water Resources Research} \bold{24}(12), 1997--2004, Appendix B.
}
\keyword{datasets}
