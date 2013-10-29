\name{plotTolIntNormDesign}
\alias{plotTolIntNormDesign}
\title{
  Plots for a Sampling Design Based on a Tolerance Interval for a Normal Distribution
}
\description{
  Create plots involving sample size, half-width, estimated standard deviation, 
  coverage, and confidence level for a  tolerance interval for a normal distribution.
}
\usage{
  plotTolIntNormDesign(x.var = "n", y.var = "half.width", range.x.var = NULL, 
    n = 25, half.width = ifelse(x.var == "sigma.hat", 3 * max.x, 3 * sigma.hat), 
    sigma.hat = 1, coverage = 0.95, conf.level = 0.95, cov.type = "content", 
    round.up = FALSE, n.max = 5000, tol = 1e-07, maxiter = 1000, plot.it = TRUE, 
    add = FALSE, n.points = 100, plot.col = 1, plot.lwd = 3 * par("cex"), 
    plot.lty = 1, digits = .Options$digits, ..., main = NULL, xlab = NULL, 
    ylab = NULL, type = "l")
}
\arguments{
  \item{x.var}{
  character string indicating what variable to use for the x-axis.  Possible values 
  are \code{"n"} (sample size; the default), \code{"half.width"} (half-width), 
  \code{"sigma.hat"} (estimated standard deviation), \code{"coverage"} (the coverage), 
  and \code{"conf.level"} (the confidence level). 
}
  \item{y.var}{
  character string indicating what variable to use for the y-axis.  Possible values 
  are \code{"half.width"} (the half-width; the default), and \code{"n"} (sample size). 
}
  \item{range.x.var}{
  numeric vector of length 2 indicating the range of the x-variable to use for the plot.  
  The default value depends on the value of \code{x.var}.  
  When \code{x.var="n"} the default value is \code{c(2,50)}.  
  When \code{x.var="half.width"} the default value is 
  \code{c(2.5 * sigma.hat, 4 * sigma.hat)}.  
  When \code{x.var="sigma.hat"}, the default value is \code{c(0.1, 2)}. 
  When \code{x.var="coverage"} or \code{x.var="conf.level"}, the default value is 
  \code{c(0.5, 0.99)}.
}
  \item{n}{
  positive integer greater than 1 indicating the sample size upon 
  which the tolerance interval is based.  The default value is \code{n=25}. 
  Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) 
  values are not allowed. 
}
  \item{half.width}{
  positive scalar indicating the half-width of the prediction interval.  
  The default value depends on the value of \code{x.var}.  When 
  \code{x.var="sigma.hat"} the default value is 3 times the second value of 
  \code{range.x.var}.  When \code{x.var} is not equal to \code{"sigma.hat"} the 
  default value is \code{half.width=4*sigma.hat}.  This argument is ignored 
  if either \code{x.var="half.width"} or \code{y.var="half.width"}.
}
  \item{sigma.hat}{
  numeric scalar specifying the value of the estimated standard deviation.  
  The default value is \code{sigma.hat=1}.  This argument is ignored if 
  \code{x.var="sigma.hat"}.
}
  \item{coverage}{
  numeric scalar between 0 and 1 indicating the desired coverage of the 
  tolerance interval.  The default value is \code{coverage=0.95}.
}
  \item{conf.level}{
  numeric scalar between 0 and 1 indicating the confidence level of the 
  tolerance interval.  The default value is \code{conf.level=0.95}.
}
  \item{cov.type}{
  character string specifying the coverage type for the tolerance interval.  The 
  possible values are \code{"content"} (\eqn{\beta}-content; the default), and 
  \code{"expectation"} (\eqn{\beta}-expectation). 
}
  \item{round.up}{
  for the case when \code{y.var="n"}, logical scalar indicating whether to round 
  up the values of the computed sample size(s) to the next smallest integer.  
  The default value is \code{round.up=TRUE}.
}
  \item{n.max}{
  for the case when \code{y.var="n"}, positive integer greater than 1 specifying 
  the maximum possible sample size.  The default value is \code{n.max=5000}.
}
  \item{tol}{
  for the case when \code{y.var="n"}, numeric scalar indicating the tolerance 
  to use in the \code{\link{uniroot}} search algorithm.  The default value is 
  \code{tol=1e-7}.
}
  \item{maxiter}{
  for the case when \code{y.var="n"}, positive integer indicating the maximum 
  number of iterations to use in the \code{\link{uniroot}} search algorithm.  
  The default value is \code{maxiter=1000}.
}
  \item{plot.it}{
  a logical scalar indicating whether to create a plot or add to the existing plot 
  (see explanation of the argument \code{add} below) on the current graphics device.  
  If \code{plot.it=FALSE}, no plot is produced, but a list of (x,y) values is returned 
  (see the section VALUE).  The default value is \code{plot.it=TRUE}.
}
  \item{add}{
  a logical scalar indicating whether to add the design plot to the existing plot (\code{add=TRUE}), 
  or to create a plot from scratch (\code{add=FALSE}).  The default value is \code{add=FALSE}.  
  This argument is ignored if \code{plot.it=FALSE}.
}
  \item{n.points}{
  a numeric scalar specifying how many (x,y) pairs to use to produce the plot.  
  There are \code{n.points} x-values evenly spaced between \code{range.x.var[1]} and \cr
  \code{range.x.var[2]}.  The default value is \code{n.points=100}.
}
  \item{plot.col}{
  a numeric scalar or character string determining the color of the plotted line or points.  The default value 
  is \code{plot.col="black"}.  See the entry for \code{col} in the help file for \code{\link{par}} 
  for more information.
}
  \item{plot.lwd}{
  a numeric scalar determining the width of the plotted line.  The default value is 
  \code{3*par("cex")}.  See the entry for \code{lwd} in the help file for \code{\link{par}} 
  for more information.
}
  \item{plot.lty}{
  a numeric scalar determining the line type of the plotted line.  The default value is 
  \code{plot.lty=1}.  See the entry for \code{lty} in the help file for \code{\link{par}} 
  for more information.
}
  \item{digits}{
  a scalar indicating how many significant digits to print out on the plot.  The default 
  value is the current setting of \code{\link{options}("digits")}.
}
  \item{main, xlab, ylab, type, \dots}{
  additional graphical parameters (see \code{\link{par}}).
}
}
\details{
  See the help files for \code{\link{tolIntNorm}}, \code{\link{tolIntNormK}}, 
  \code{\link{tolIntNormHalfWidth}}, and \code{\link{tolIntNormN}} for information 
  on how to compute a tolerance interval for a normal distribution, how the 
  half-width is computed when other quantities are fixed, and how the sample size 
  is computed when other quantities are fixed.
}
\value{
  \code{plotTolIntNormDesign} invisibly returns a list with components:

  \item{x.var}{x-coordinates of points that have been or would have been plotted.}
  \item{y.var}{y-coordinates of points that have been or would have been plotted.}
}
\references{
  See the help file for \code{\link{tolIntNorm}}.
}
\author{
    Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  See the help file for \code{\link{tolIntNorm}}.

  In the course of designing a sampling program, an environmental scientist may wish 
  to determine the relationship between sample size, confidence level, and half-width 
  if one of the objectives of the sampling program is to produce tolerance intervals.  
  The functions \code{\link{tolIntNormHalfWidth}}, \code{\link{tolIntNormN}}, and 
  \code{plotTolIntNormDesign} can be used to investigate these relationships for the 
  case of normally-distributed observations.
}
\seealso{
  \code{\link{tolIntNorm}}, \code{\link{tolIntNormK}}, 
  \code{\link{tolIntNormN}}, \code{\link{plotTolIntNormDesign}},  
  \code{\link{Normal}}.
}
\examples{
  # Look at the relationship between half-width and sample size for a 
  # 95% beta-content tolerance interval, assuming an estimated standard 
  # deviation of 1 and a confidence level of 95%:

  dev.new()
  plotTolIntNormDesign()

  #==========

  # Plot half-width vs. coverage for various levels of confidence:

  dev.new()
  plotTolIntNormDesign(x.var = "coverage", y.var = "half.width", 
    ylim = c(0, 3.5), main="") 

  plotTolIntNormDesign(x.var = "coverage", y.var = "half.width", 
    conf.level = 0.9, add = TRUE, plot.col = "red") 

  plotTolIntNormDesign(x.var = "coverage", y.var = "half.width", 
    conf.level = 0.8, add = TRUE, plot.col = "blue") 

  legend("topleft", c("95\%", "90\%", "80\%"), lty = 1, lwd = 3 * par("cex"), 
    col = c("black", "red", "blue"), bty = "n")

  title(main = paste("Half-Width vs. Coverage for Tolerance Interval", 
    "with Sigma Hat=1 and Various Confidence Levels", sep = "\n"))

  #==========

  # Example 17-3 of USEPA (2009, p. 17-17) shows how to construct a 
  # beta-content upper tolerance limit with 95% coverage and 95% 
  # confidence  using chrysene data and assuming a lognormal distribution.  
  # The data for this example are stored in EPA.09.Ex.17.3.chrysene.df, 
  # which contains chrysene concentration data (ppb) found in water 
  # samples obtained from two background wells (Wells 1 and 2) and 
  # three compliance wells (Wells 3, 4, and 5).  The tolerance limit 
  # is based on the data from the background wells.

  # Here we will first take the log of the data and then estimate the 
  # standard deviation based on the two background wells.  We will use this 
  # estimate of standard deviation to plot the half-widths of 
  # future tolerance intervals on the log-scale for various sample sizes.

  head(EPA.09.Ex.17.3.chrysene.df)
  #  Month   Well  Well.type Chrysene.ppb
  #1     1 Well.1 Background         19.7
  #2     2 Well.1 Background         39.2
  #3     3 Well.1 Background          7.8
  #4     4 Well.1 Background         12.8
  #5     1 Well.2 Background         10.2
  #6     2 Well.2 Background          7.2

  longToWide(EPA.09.Ex.17.3.chrysene.df, "Chrysene.ppb", "Month", "Well")
  #  Well.1 Well.2 Well.3 Well.4 Well.5
  #1   19.7   10.2   68.0   26.8   47.0
  #2   39.2    7.2   48.9   17.7   30.5
  #3    7.8   16.1   30.1   31.9   15.0
  #4   12.8    5.7   38.1   22.2   23.4

  summary.stats <- summaryStats(log(Chrysene.ppb) ~ Well.type, 
    data = EPA.09.Ex.17.3.chrysene.df)

  summary.stats
  #            N   Mean     SD Median    Min    Max
  #Background  8 2.5086 0.6279 2.4359 1.7405 3.6687
  #Compliance 12 3.4173 0.4361 3.4111 2.7081 4.2195

  sigma.hat <- summary.stats["Background", "SD"]
  sigma.hat
  #[1] 0.6279

  dev.new()
  plotTolIntNormDesign(x.var = "n", y.var = "half.width", 
    range.x.var = c(5, 40), sigma.hat = sigma.hat, cex.main = 1)

  #==========

  # Clean up
  #---------
  rm(summary.stats, sigma.hat)
  graphics.off()
}
\keyword{ distribution }
\keyword{ design }
\keyword{ htest }
