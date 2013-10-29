\name{Benthic.df}
\alias{Benthic.df}
\docType{data}
\title{Benthic Data from Monitoring Program in Chesapeake Bay}
\description{
  Benthic data from a monitoring program in the Chesapeake Bay, 
  Maryland, covering July 1994 - December 1991.
}
\usage{Benthic.df}
\format{
  A data frame with 585 observations on the following 7 variables.
  \describe{
    \item{\code{Site.ID}}{Site ID}
    \item{\code{Stratum}}{Stratum Number (101-131)}
    \item{\code{Latitude}}{Latitude (degrees North)}
    \item{\code{Longitude}}{Longitude (negative values; degrees West)}
    \item{\code{Index}}{Benthic Index (between 1 and 5)}
    \item{\code{Salinity}}{Salinity (ppt)}
    \item{\code{Silt}}{Silt Content (\% clay in soil)}
  }
}
\details{
  Data from the Long Term Benthic Monitoring Program of the Chesapeake Bay. 
  The data consist of measurements of benthic characteristics and a computed 
  index of benthic health for several locations in the bay. Sampling methods 
  and designs of the program are discussed in Ranasinghe et al. (1992).  

  The data represent observations collected at 585 separate point locations (sites). 
  The sites are divided into 31 different strata, numbered 101 through 131, each 
  strata consisting of geographically close sites of similar degradation conditions. 
  The benthic index values range from 1 to 5 on a continuous scale, where high values 
  correspond to healthier benthos. Salinity was measured in parts per thousand (ppt), 
  and silt content is expressed as a percentage of clay in the soil with high numbers 
  corresponding to muddy areas.

  The United States Environmental Protection Agency (USEPA) established an initiative 
  for the Chesapeake Bay in partnership with the states bordering the bay in 1984. 
  The goal of the initiative is the restoration (abundance, health, and diversity) 
  of living resources to the bay by reducing nutrient loadings, reducing toxic 
  chemical impacts, and enhancing habitats.  USEPA's Chesapeake Bay Program Office 
  is responsible for implementing this initiative and has established an extensive 
  monitoring program that includes traditional water chemistry sampling, as well as 
  collecting data on living resources to measure progress towards meeting the 
  restoration goals.

  Sampling benthic invertebrate assemblages has been an integral part of the 
  Chesapeake Bay monitoring program due to their ecological importance and their 
  value as biological indicators. The condition of benthic assemblages is a measure 
  of the ecological health of the bay, including the effects of multiple types of 
  environmental stresses.  Nevertheless, regional-scale assessment of ecological 
  status and trends using benthic assemblages are limited by the fact that benthic 
  assemblages are strongly influenced by naturally variable habitat elements, such as 
  salinity, sediment type, and depth.  Also, different state agencies and USEPA programs 
  use different sampling methodologies, limiting the ability to integrate data into a 
  unified assessment.  To circumvent these limitations, USEPA has standardized benthic 
  data from several different monitoring programs into a single database, and from 
  that database developed a Restoration Goals Benthic Index that identifies whether 
  benthic restoration goals are being met.
}
\source{
  Ranasinghe, J.A., L.C. Scott, and R. Newport. (1992). 
  \emph{Long-term Benthic Monitoring and Assessment Program for the Maryland Portion of the Bay}, 
  Jul 1984-Dec 1991. Report prepared for the Maryland Department of the Environment and 
  the Maryland Department of Natural Resources by Versar, Inc., Columbia, MD.
}
\examples{
  attach(Benthic.df)

  # Show station locations
  #-----------------------
  dev.new()
  plot(Longitude, Latitude, 
      xlab = "-Longitude (Degrees West)",
      ylab = "Latitude",
      main = "Sampling Station Locations")


  # Scatterplot matrix of benthic index, salinity, and silt
  #--------------------------------------------------------
  dev.new()
  pairs(~ Index + Salinity + Silt, data = Benthic.df)


  # Contour and perspective plots based on loess fit
  # showing only predicted values within the convex hull
  # of station locations
  #-----------------------------------------------------
  library(sp)

  loess.fit <- loess(Index ~ Longitude * Latitude,
      data=Benthic.df, normalize=FALSE, span=0.25)
  lat <- Benthic.df$Latitude
  lon <- Benthic.df$Longitude
  Latitude <- seq(min(lat), max(lat), length=50)
  Longitude <- seq(min(lon), max(lon), length=50)
  predict.list <- list(Longitude=Longitude,
      Latitude=Latitude)
  predict.grid <- expand.grid(predict.list)
  predict.fit <- predict(loess.fit, predict.grid)
  index.chull <- chull(lon, lat)
  inside <- point.in.polygon(point.x = predict.grid$Longitude, 
      point.y = predict.grid$Latitude, 
      pol.x = lon[index.chull], 
      pol.y = lat[index.chull])
  predict.fit[inside == 0] <- NA

  dev.new()
  contour(Longitude, Latitude, predict.fit,
      levels=seq(1, 5, by=0.5), labcex=0.75,
      xlab="-Longitude (degrees West)",
      ylab="Latitude (degrees North)")
  title(main=paste("Contour Plot of Benthic Index",
      "Based on Loess Smooth", sep="\n"))

  dev.new()
  persp(Longitude, Latitude, predict.fit,
      xlim = c(-77.3, -75.9), ylim = c(38.1, 39.5), zlim = c(0, 6), 
      theta = -45, phi = 30, d = 0.5,
      xlab="-Longitude (degrees West)",
      ylab="Latitude (degrees North)",
      zlab="Benthic Index", ticktype = "detailed")
  title(main=paste("Surface Plot of Benthic Index",
      "Based on Loess Smooth", sep="\n"))

  detach("Benthic.df")

  rm(loess.fit, lat, lon, Latitude, Longitude, predict.list,
      predict.grid, predict.fit, index.chull, inside)
}
\keyword{datasets}
