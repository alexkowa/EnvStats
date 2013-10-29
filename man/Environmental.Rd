\name{Environmental}
\alias{Environmental}
\alias{Environmental.df}
\alias{Air.df}
\docType{data}
\title{Atmospheric Environmental Conditions in New York City}
\description{Daily measurements of ozone concentration, wind speed, temperature, and solar radiation in New York City for 153 consecutive days between May 1 and September 30, 1973.}
\usage{
  Environmental.df
  Air.df
}
\format{
  The data frame \code{Environmental.df} has 153 observations on the following 4 variables.
  \describe{
    \item{\code{ozone}}{Average ozone concentration (of hourly measurements) of in parts per billion.}
    \item{\code{radiation}}{Solar radiation (from 08:00 to 12:00) in langleys.}
    \item{\code{temperature}}{Maximum daily temperature in degrees Fahrenheit.}
    \item{\code{wind}}{Average wind speed (at 07:00 and 10:00) in miles per hour.}
  }
  Row names are the dates the data were collected.

  The data frame \code{Air.df} is the same as \code{Environmental.df} except that the
  column \code{ozone} is the cube root of average ozone concentration.
}
\details{
  Data on ozone (ppb), solar radiation (langleys), temperature (degrees Fahrenheit), and wind speed (mph)
  for 153 consecutive days between May 1 and September 30, 1973.  These data are a superset of the data 
  contained in the data frame \code{environmental} in the package \pkg{lattice}.
}
\source{Chambers et al. (1983), pp. 347-349.}
\references{
  Chambers, J.M., W.S. Cleveland, B. Kleiner, and P.A. Tukey. (1983). \emph{Graphical Methods for Data Analysis}. Duxbury Press, Boston, MA, 395pp.

  Cleveland, W.S. (1993). \emph{Visualizing Data}. Hobart Press, Summit, New Jersey, 360pp.

  Cleveland, W.S. (1994). \emph{The Elements of Graphing Data}. Revised Edition. Hobart Press, Summit, New Jersey, 297pp.
}
\examples{
# Scatterplot matrix
pairs(Environmental.df)

pairs(Air.df)


# Time series plot for ozone
attach(Environmental.df)
dates <- as.Date(row.names(Environmental.df), format = "\%m/\%d/\%Y")
plot(dates, ozone, type = "l", 
    xlab = "Time (Year = 1973)", ylab = "Ozone (ppb)",
    main = "Time Series Plot of Daily Ozone Measures")
detach("Environmental.df")
rm(dates)
}
\keyword{datasets}
