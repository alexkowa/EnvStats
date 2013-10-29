\name{Ozone.NE.df}
\alias{Ozone.NE.df}
\docType{data}
\title{
  Ozone Concentrations in the Northeast U.S.
}
\description{
  Ozone concentrations in 41 U.S. cities based on daily maxima collected between June and August 1974.
}
\usage{Ozone.NE.df}
\format{
  A data frame with 41 observations on the following 5 variables.
  \describe{
    \item{\code{Median}}{median of daily maxima ozone concentration (ppb).}
    \item{\code{Quartile}}{Upper quartile (i.e., 75th percentile) of daily maxima ozone concentration (ppb).}
    \item{\code{City}}{a factor indicating the city}
    \item{\code{Longitude}}{negative longitude of the city}
    \item{\code{Latitude}}{latitude of the city}
  }
}
\source{
  Cleveland, W.S., Kleiner, B., McRae, J.E., Warner, J.L., and Pasceri, P.E. (1975). 
  \emph{The Analysis of Ground-Level Ozone Data from New Jersey, New York, Connecticut, and 
  Massachusetts: Data Quality Assessment and Temporal and Geographical Properties}. 
  Bell Laboratories Memorandum. 

  The original data were collected by the New Jersey Department of Environmental Protection, 
  the New York State Department of Environmental Protection, the Boyce Thompson Institute 
  (Yonkers, for New York data), the Connecticut Department of Environmental Protection, and 
  the Massachusetts Department of Public Health. 
}
\examples{
  summary(Ozone.NE.df)
  #     Median          Quartile               City      Longitude     
  # Min.   : 34.00   Min.   : 48.00   Asbury Park: 1   Min.   :-74.71  
  # 1st Qu.: 58.00   1st Qu.: 79.75   Babylon    : 1   1st Qu.:-73.74  
  # Median : 65.00   Median : 90.00   Bayonne    : 1   Median :-73.17  
  # Mean   : 68.15   Mean   : 95.10   Boston     : 1   Mean   :-72.94  
  # 3rd Qu.: 80.00   3rd Qu.:112.25   Bridgeport : 1   3rd Qu.:-72.08  
  # Max.   :100.00   Max.   :145.00   Cambridge  : 1   Max.   :-71.05  
  #                  NA's   :  1.00   (Other)    :35                   
  #    Latitude    
  # Min.   :40.22  
  # 1st Qu.:40.97  
  # Median :41.56  
  # Mean   :41.60  
  # 3rd Qu.:42.25  
  # Max.   :43.32 
}
\keyword{datasets}
