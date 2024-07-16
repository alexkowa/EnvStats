\name{plotPredIntLnormAltSimultaneousTestPowerCurve}
\alias{plotPredIntLnormAltSimultaneousTestPowerCurve}
\title{
  Power Curves for Sampling Design for Test Based on Simultaneous Prediction Interval for Lognormal Distribution
}
\description{
  Plot power vs. \eqn{\theta_1/\theta_2} (ratio of means) for a 
  sampling design for a test based on a simultaneous prediction interval for a 
  lognormal distribution.
}
\usage{
  plotPredIntLnormAltSimultaneousTestPowerCurve(n = 8, df = n - 1, n.geomean = 1, 
    k = 1, m = 2, r = 1, rule = "k.of.m", cv = 1, range.ratio.of.means = c(1, 5), 
    pi.type = "upper", conf.level = 0.95, r.shifted = r, 
    K.tol = .Machine$double.eps^(1/2), integrate.args.list = NULL, plot.it = TRUE, 
    add = FALSE, n.points = 20, plot.col = "black", plot.lwd = 3 * par("cex"), 
    plot.lty = 1, digits = .Options$digits, cex.main = par("cex"), ..., 
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
  for the \eqn{k}-of-\eqn{m} rule (\code{rule="k.of.m"}), positive integer 
  specifying the minimum number of observations (or averages) out of \eqn{m} 
  observations (or averages) (all obtained on one future sampling \dQuote{occassion}) 
  the prediction interval should contain with confidence level \code{conf.level}.  
  The default value is \code{k=1}.  This argument is ignored when the argument 
  \code{rule} is not equal to \code{"k.of.m"}.
}
  \item{m}{
  positive integer specifying the maximum number of future observations (or 
  averages) on one future sampling \dQuote{occasion}.  
  The default value is \code{m=2}, except when \code{rule="Modified.CA"}, in which 
  case this argument is ignored and \code{m} is automatically set equal to \code{4}.
}
  \item{r}{
  positive integer specifying the number of future sampling \dQuote{occasions}.  
  The default value is \code{r=1}.
}
  \item{rule}{
  character string specifying which rule to use.  The possible values are 
  \code{"k.of.m"} (\eqn{k}-of-\eqn{m} rule; the default), \code{"CA"} (California rule), 
  and \code{"Modified.CA"} (modified California rule).
}
  \item{cv}{
  positive value specifying the coefficient of variation for 
  both the population that was sampled to construct the prediction interval \bold{and} 
  the population that will be sampled to produce the future observations.  The 
  default value is \code{cv=1}.
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
  \item{r.shifted}{
  positive integer between \code{1} and \code{r} specifying the number of future 
  sampling occasions for which the mean is shifted.  The default value is 
  \code{r.shifted=r}.
}
  \item{K.tol}{
  numeric scalar indicating the tolerance to use in the nonlinear search algorithm to 
  compute \eqn{K}.  The default value is \code{K.tol=.Machine$double.eps^(1/2)}.  
  For many applications, the value of \eqn{K} needs to be known only to the second 
  decimal place, in which case setting \code{K.tol=1e-4} will speed up computation a 
  bit.
}
  \item{integrate.args.list}{
  a list of arguments to supply to the \code{\link{integrate}} function.  The 
  default value is \code{integrate.args.list=NULL} which means that the 
  default values of \code{\link{integrate}} are used.
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
  \item{cex.main, main, xlab, ylab, type, \dots}{
  additional graphical parameters (see \code{\link{par}}).
}
}
\details{
  See the help file for \code{\link{predIntLnormAltSimultaneousTestPower}} for 
  information on how to compute the power of a hypothesis test for the difference 
  between two means of lognormal distributions based on a simultaneous prediction 
  interval for a \link[=LognormalAlt]{lognormal distribution}.
}
\value{
  \code{plotPredIntLnormAltSimultaneousTestPowerCurve} invisibly returns a list with 
  components:
 
  \item{x.var}{x-coordinates of points that have been or would have been plotted.}
  \item{y.var}{y-coordinates of points that have been or would have been plotted.}
}
\references{
  See the help file for \code{\link{predIntNormSimultaneous}}.
}
\author{
    Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  See the help file for \code{\link{predIntNormSimultaneous}}.

  In the course of designing a sampling program, an environmental scientist may wish 
  to determine the relationship between sample size, significance level, power, and 
  scaled difference if one of the objectives of the sampling program is to determine 
  whether two distributions differ from each other.  The functions 
  \code{\link{predIntLnormAltSimultaneousTestPower}} and \cr
  \code{plotPredIntLnormAltSimultaneousTestPowerCurve} can be 
  used to investigate these relationships for the case of normally-distributed 
  observations. 
}
\seealso{
  \code{\link{predIntLnormAltSimultaneousTestPower}}, 
  \code{\link{predIntLnormAltSimultaneous}}, \code{\link{predIntLnormAlt}},  
  \code{\link{predIntLnormAltTestPower}}, \link{Prediction Intervals}, 
  \link{LognormalAlt}.
}
\examples{
  # USEPA (2009) contains an example on page 19-23 that involves monitoring 
  # nw=100 compliance wells at a large facility with minimal natural spatial 
  # variation every 6 months for nc=20 separate chemicals.  
  # There are n=25 background measurements for each chemical to use to create
  # simultaneous prediction intervals.  We would like to determine which kind of
  # resampling plan based on normal distribution simultaneous prediction intervals to
  # use (1-of-m, 1-of-m based on means, or Modified California) in order to have
  # adequate power of detecting an increase in chemical concentration at any of the
  # 100 wells while at the same time maintaining a site-wide false positive rate
  # (SWFPR) of 10% per year over all 4,000 comparisons 
  # (100 wells x 20 chemicals x semi-annual sampling).

  # The function predIntNormSimultaneousTestPower includes the argument "r" 
  # that is the number of future sampling occasions (r=2 in this case because 
  # we are performing semi-annual sampling), so to compute the individual test 
  # Type I error level alpha.test (and thus the individual test confidence level), 
  # we only need to worry about the number of wells (100) and the number of 
  # constituents (20): alpha.test = 1-(1-alpha)^(1/(nw x nc)).  The individual 
  # confidence level is simply 1-alpha.test.  Plugging in 0.1 for alpha, 
  # 100 for nw, and 20 for nc yields an individual test confidence level of 
  # 1-alpha.test = 0.9999473.

  nc <- 20
  nw <- 100
  conf.level <- (1 - 0.1)^(1 / (nc * nw))
  conf.level
  #[1] 0.9999473

  # The help file for predIntNormSimultaneousTestPower shows how to 
  # create the results below for various sampling plans:

  #         Rule k m N.Mean    K Power Total.Samples
  #1      k.of.m 1 2      1 3.16  0.39             2
  #2      k.of.m 1 3      1 2.33  0.65             3
  #3      k.of.m 1 4      1 1.83  0.81             4
  #4 Modified.CA 1 4      1 2.57  0.71             4
  #5      k.of.m 1 1      2 3.62  0.41             2
  #6      k.of.m 1 2      2 2.33  0.85             4
  #7      k.of.m 1 1      3 2.99  0.71             3

  # The above table shows the K-multipliers for each prediction interval, along with
  # the power of detecting a change in concentration of three standard deviations at
  # any of the 100 wells during the course of a year, for each of the sampling
  # strategies considered.  The last three rows of the table correspond to sampling
  # strategies that involve using the mean of two or three observations.

  # Here we will create a variation of this example based on 
  # using a lognormal distribution and plotting power versus ratio of the 
  # means assuming cv=1.

  # Here is the power curve for the 1-of-4 sampling strategy:

  dev.new()
  plotPredIntLnormAltSimultaneousTestPowerCurve(n = 25, k = 1, m = 4, r = 2, 
    rule="k.of.m", range.ratio.of.means = c(1, 10), pi.type = "upper",  
    conf.level = conf.level, ylim = c(0, 1), main = "")

  title(main = paste("Power Curves for 1-of-4 Sampling Strategy Based on 25 Background", 
    "Samples, SWFPR=10\%, and 2 Future Sampling Periods", sep = "\n"))
  mtext("Assuming Lognormal Data with CV=1", line = 0)

  #----------

  # Here are the power curves for the first four sampling strategies.
  # Because this takes several seconds to run, here we have commented out 
  # the R commands.  To run this example, just remove the pound signs (#) 
  # from in front of the R commands.

  #dev.new()
  #plotPredIntLnormAltSimultaneousTestPowerCurve(n = 25, k = 1, m = 4, r = 2, 
  #  rule="k.of.m", range.ratio.of.means = c(1, 10), pi.type = "upper",  
  #  conf.level = conf.level, ylim = c(0, 1), main = "")

  #plotPredIntLnormAltSimultaneousTestPowerCurve(n = 25, k = 1, m = 3, r = 2, 
  #  rule="k.of.m", range.ratio.of.means = c(1, 10), pi.type = "upper",  
  #  conf.level = conf.level, add = TRUE, plot.col = "red", plot.lty = 2)

  #plotPredIntLnormAltSimultaneousTestPowerCurve(n = 25, k = 1, m = 2, r = 2, 
  #  rule="k.of.m", range.ratio.of.means = c(1, 10), pi.type = "upper", 
  #  conf.level = conf.level, add = TRUE, plot.col = "blue", plot.lty = 3)

  #plotPredIntLnormAltSimultaneousTestPowerCurve(n = 25, r = 2, rule="Modified.CA", 
  #  range.ratio.of.means = c(1, 10), pi.type = "upper", conf.level = conf.level, 
  #  add = TRUE, plot.col = "green3", plot.lty = 4)

  #legend("topleft", c("1-of-4", "Modified CA", "1-of-3", "1-of-2"), 
  #  col = c("black", "green3", "red", "blue"), lty = c(1, 4, 2, 3),
  #  lwd = 3 * par("cex"), bty = "n") 

  #title(main = paste("Power Curves for 4 Sampling Strategies Based on 25 Background", 
  #  "Samples, SWFPR=10\%, and 2 Future Sampling Periods", sep = "\n"))
  #mtext("Assuming Lognormal Data with CV=1", line = 0)

  #----------

  # Here are the power curves for the last 3 sampling strategies:
  # Because this takes several seconds to run, here we have commented out 
  # the R commands.  To run this example, just remove the pound signs (#) 
  # from in front of the R commands.

  #dev.new()
  #plotPredIntLnormAltSimultaneousTestPowerCurve(n = 25, k = 1, m = 2, n.geomean = 2, 
  #  r = 2, rule="k.of.m", range.ratio.of.means = c(1, 10), pi.type = "upper", 
  #  conf.level = conf.level, ylim = c(0, 1), main = "")

  #plotPredIntLnormAltSimultaneousTestPowerCurve(n = 25, k = 1, m = 1, n.geomean = 2, 
  #  r = 2, rule="k.of.m", range.ratio.of.means = c(1, 10), pi.type = "upper", 
  #  conf.level = conf.level, add = TRUE, plot.col = "red", plot.lty = 2)

  #plotPredIntLnormAltSimultaneousTestPowerCurve(n = 25, k = 1, m = 1, n.geomean = 3, 
  #  r = 2, rule="k.of.m", range.ratio.of.means = c(1, 10), pi.type = "upper", 
  #  conf.level = conf.level, add = TRUE, plot.col = "blue", plot.lty = 3)

  #legend("topleft", c("1-of-2, Order 2", "1-of-1, Order 3", "1-of-1, Order 2"), 
  #  col = c("black", "blue", "red"), lty = c(1, 3, 2), lwd = 3 * par("cex"), 
  #  bty="n")

  #title(main = paste("Power Curves for 3 Sampling Strategies Based on 25 Background", 
  #  "Samples, SWFPR=10\%, and 2 Future Sampling Periods", sep = "\n"))
  #mtext("Assuming Lognormal Data with CV=1", line = 0)

  #==========

  # Clean up
  #---------
  rm(nc, nw, conf.level)
  graphics.off()
}
\keyword{ distribution }
\keyword{ htest }
