\name{FcnsByCatSumStats}
\alias{FcnsByCatSumStats}
\alias{Summary Statistics}
\alias{Summary Plots}
\title{
  EnvStats Functions for Summary Statistics and Plots
}
\description{
  The \pkg{EnvStats} functions listed below create summary statistics and plots.
}
\details{
  \bold{Summary Statistics} \cr
  \R comes with several functions for computing summary statistics, including 
  \code{\link{mean}}, \code{\link{var}}, \code{\link{median}}, \code{\link{range}}, 
  \code{\link{quantile}}, and \code{\link{summary}}.  The following functions in 
  \pkg{EnvStats} complement these \R functions. 

  \tabular{ll}{
  \emph{Function Name}        \tab \emph{Description} \cr
  \code{\link{cv}}            \tab Coefficient of variation \cr
  \code{\link{geoMean}}       \tab Geometric mean \cr
  \code{\link{geoSD}}         \tab Geometric standard deviation \cr
  \code{\link{iqr}}           \tab Interquartile range \cr
  \code{\link{kurtosis}}      \tab Kurtosis \cr
  \code{\link{lMoment}}       \tab \eqn{L}-moments \cr
  \code{\link{pwMoment}}      \tab Probability-weighted moments \cr
  \code{\link{skewness}}      \tab Skew \cr       
  \code{\link{summaryFull}}   \tab Extensive summary statistics \cr
  \code{\link{summaryStats}}  \tab Summary statistics \cr
  } 


  \bold{Summary Plots} \cr
  \R comes with several functions for creating plots to summarize data, including 
  \code{\link[graphics]{hist}}, \code{\link[graphics]{barplot}}, \code{\link[graphics]{boxplot}}, 
  \code{\link[graphics]{dotchart}}, \code{\link[graphics]{stripchart}}, and numerous others.  

  The help file \link[=FcnsByCatPlotProbDists]{Plotting Probability Distributions} 
  lists several \pkg{EnvStats} functions useful for producing summary plots as well. 
 
  In addition, the \pkg{EnvStats} function \code{\link{stripChart}} is a modification 
  of \code{\link[graphics]{stripchart}} that allows you to include summary statistics 
  on the plot itself.

  Finally, the help file \link[=FcnsByCatPlotUsingggplot2]{Plotting Using ggplot2} lists 
  several \pkg{EnvStats} functions for adding information to plots produced with the 
  \code{\link[ggplot2]{ggplot}} function, including the function \code{\link{geom_stripchart}}, 
  which is an adaptation of the \pkg{EnvStats} function \code{\link{stripChart}}.  
}
\keyword{ package }
