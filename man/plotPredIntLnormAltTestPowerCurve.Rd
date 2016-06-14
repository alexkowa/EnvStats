\name{plotPredIntLnormAltTestPowerCurve}
\alias{plotPredIntLnormAltTestPowerCurve}
\title{
  Power Curves for Sampling Design for Test Based on Prediction Interval for Lognormal Distribution
}
\description{
  Plot power vs. \eqn{\theta_1/\theta_2} (ratio of means) for a 
  sampling design for a test based on a prediction interval for a lognormal distribution.
}
\usage{
  plotPredIntLnormAltTestPowerCurve(n = 8, df = n - 1, n.geomean = 1, k = 1, 
    cv = 1, range.ratio.of.means = c(1, 5), pi.type = "upper", conf.level = 0.95, 
    plot.it = TRUE, add = FALSE, n.points = 20, plot.col = "black", 
    plot.lwd = 3 * par("cex"), plot.lty = 1, digits = .Options$digits, ..., 
    main = NULL, xlab = NULL, ylab = NULL, type = "l")
}
\arguments{
  \item{n}{
  positive integer greater than 2 indicating the sample size upon which 
  the prediction interval is based.  The default is value is \code{n=8}.
}
  \item{df}{
  positive integer indicating the degrees of freedom associated with 
  the sample size.  The default value is \code{df=n-1}.
}
  \item{n.geomean}{
  positive integer specifying the sample size associated with the future geometric 
  mean(s).  The default value is \code{n.geomean=1} (i.e., individual observations).  
  Note that all future geometric means must be based on the same sample size.
}
  \item{k}{
  positive integer specifying the number of future observations that the 
  prediction interval should contain with confidence level \code{conf.level}.  The 
  default value is \code{k=1}.
}
  \item{cv}{
  positive value specifying the coefficient of variation for both the population 
  that was sampled to construct the prediction interval \bold{and} the population that 
  will be sampled to produce the future observations.  The default value is 
  \code{cv=1}.
}
  \item{range.ratio.of.means}{
  numeric vector of length 2 indicating the range of the x-variable to use for the 
  plot.  The default value is \code{range.ratio.of.means=c(1,5)}.
}
  \item{pi.type}{
  character string indicating what kind of prediction interval to compute.  
  The possible values are \code{pi.type="upper"} (the default), and 
  \code{pi.type="lower"}.
}
  \item{conf.level}{
  numeric scalar between 0 and 1 indicating the confidence level of the 
  prediction interval.  The default value is \code{conf.level=0.95}.
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
  See the help file for \code{\link{predIntLnormAltTestPower}} for information on how to 
  compute the power of a hypothesis test for the ratio of two means of lognormal 
  distributions based on a prediction interval for a lognormal distribution.
}
\value{
  \code{plotPredIntLnormAltTestPowerCurve} invisibly returns a list with components:
 
  \item{x.var}{x-coordinates of points that have been or would have been plotted.}
  \item{y.var}{y-coordinates of points that have been or would have been plotted.}
}
\references{
  See the help files for \code{\link{predIntNormTestPower}} and 
  \code{\link{tTestLnormAltPower}}.
}
\author{
    Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  See the help files for \code{\link{predIntNormTestPower}}.
}
\seealso{
  \code{\link{predIntLnormAltTestPower}}, \cr
  \code{\link{predIntLnormAlt}}, \cr
  \code{\link{predIntNorm}}, \cr
  \code{\link{predIntNormK}}, \cr
  \code{\link{plotPredIntNormTestPowerCurve}}, \cr
  \code{\link{predIntLnormAltSimultaneous}}, \cr
  \code{\link{predIntLnormAltSimultaneousTestPower}}, \cr
  \link{Prediction Intervals}, \link{LognormalAlt}.
}
\examples{
  # Plot power vs. ratio of means for k=1 future observation for 
  # various sample sizes using a 5% significance level and assuming cv=1.

  dev.new()
  plotPredIntLnormAltTestPowerCurve(n = 8, k = 1, 
    range.ratio.of.means=c(1, 10), ylim = c(0, 1), main = "") 

  plotPredIntLnormAltTestPowerCurve(n = 16, k = 1, 
    range.ratio.of.means = c(1, 10), add = TRUE, plot.col = "red") 

  plotPredIntLnormAltTestPowerCurve(n = 32, k = 1, 
    range.ratio.of.means=c(1, 10), add = TRUE, plot.col = "blue") 

  legend("topleft", c("n=32", "n=16", "n=8"), lty = 1, lwd = 3 * par("cex"), 
    col = c("blue", "red", "black"), bty = "n")

  title(main = paste("Power vs. Ratio of Means for Upper Prediction Interval", 
    "with k=1, Confidence=95\%, and Various Sample Sizes", sep="\n"))
  mtext("Assuming a Lognormal Distribution with CV = 1", line = 0)

  #==========

  \dontrun{
  # Pages 6-16 to 6-17 of USEPA (2009) present EPA Reference Power Curves (ERPC)
  # for groundwater monitoring:
  #
  # "Since effect sizes discussed in the next section often cannot or have not been 
  # quantified, the Unified Guidance recommends using the ERPC as a suitable basis 
  # of comparison for proposed testing procedures.  Each reference power curve 
  # corresponds to one of three typical yearly statistical evaluation schedules - 
  # quarterly, semi-annual, or annual - and represents the cumulative power 
  # achievable during a single year at one well-constituent pair by a 99% upper 
  # (normal) prediction limit based on n = 10 background measurements and one new 
  # measurement from the compliance well.
  #
  # Here we will create a variation of Figure 6-3 on page 6-17 based on 
  # using a lognormal distribution and plotting power versus ratio of the 
  # means assuming cv=1.

  dev.new()
  plotPredIntLnormAltTestPowerCurve(n = 10, k = 1, cv = 1, conf.level = 0.99, 
    range.ratio.of.means = c(1, 10), ylim = c(0, 1), main="")

  plotPredIntLnormAltTestPowerCurve(n = 10, k = 2, cv = 1, conf.level = 0.99, 
    range.ratio.of.means = c(1, 10), add = TRUE, plot.col = "red", plot.lty = 2)

  plotPredIntLnormAltTestPowerCurve(n = 10, k = 4, cv = 1, conf.level = 0.99, 
    range.ratio.of.means = c(1, 10), add = TRUE, plot.col = "blue", plot.lty = 3)

  legend("topleft", c("Quarterly", "Semi-Annual", "Annual"), lty = 3:1, 
    lwd = 3 * par("cex"), col = c("blue", "red", "black"), bty = "n") 

  title(main = paste("Power vs. Ratio of Means for Upper Prediction Interval with",
    "n=10, Confidence=99\%, and Various Sampling Frequencies", sep="\n"))
  mtext("Assuming a Lognormal Distribution with CV = 1", line = 0)
  }

  #==========

  # Clean up
  #---------
  graphics.off()

}
\keyword{ distribution }
\keyword{ htest }

