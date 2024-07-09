\name{ProUCL.5.2.TRS.df}
\alias{ProUCL.5.2.TRS.df}
\docType{data}
\title{
  Real dataset from ProUCL 5.2.0.
}
\description{
  A real data set of size n=55 with 18.8\% Nondetects (=10).  
  The name of the Excel file that comes with ProUCL 5.2.0 and  
  contains these data is \bold{TRS-Real-data-with-NDs.xls}.
}
\usage{
    ProUCL.5.2.TRS.df
    data(ProUCL.5.2.TRS.df)
}
\format{
  A data frame with 55 observations on the following 3 variables.
  \describe{
    \item{\code{Value}}{numeric vector indicating the concentration.}
    \item{\code{Detect}}{numeric vector of 0s (nondetects) and 1s (detects) 
     indicating censoring status.}
    \item{\code{Censored}}{logical vector indicating censoring status.}
  }
}
\source{
  USEPA. (2022a).  \emph{ProUCL Version 5.2.0 Technical Guide: 
  Statistical Software for Environmental Applications for Data Sets with and 
  without Nondetect Observations}.  Prepared by:  Neptune and Company, Inc., 
  1435 Garrison Street, Suite 201, Lakewood, CO 80215.  p. 143. 
  \url{https://www.epa.gov/land-research/proucl-software}.

  USEPA. (2022b).  \emph{ProUCL Version 5.2.0 User Guide: 
  Statistical Software for Environmental Applications for Data Sets with and 
  without Nondetect Observations}.  Prepared by:  Neptune and Company, Inc., 
  1435 Garrison Street, Suite 201, Lakewood, CO 80215.  p. 6-115. 
  \url{https://www.epa.gov/land-research/proucl-software}.
}
\keyword{datasets}
