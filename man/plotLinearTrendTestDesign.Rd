\name{plotLinearTrendTestDesign}
\alias{plotLinearTrendTestDesign}
\title{
  Plots for a Sampling Design Based on a t-Test for Linear Trend
}
\description{
  Create plots involving sample size, power, scaled difference, and significance 
  level for a t-test for linear trend.
}
\usage{
  plotLinearTrendTestDesign(x.var = "n", y.var = "power", 
    range.x.var = NULL, n = 12, 
    slope.over.sigma = switch(alternative, greater = 0.1, less = -0.1, 
      two.sided = ifelse(two.sided.direction == "greater", 0.1, -0.1)), 
    alpha = 0.05, power = 0.95, alternative = "two.sided", 
    two.sided.direction = "greater", approx = FALSE, round.up = FALSE, 
    n.max = 5000, tol = 1e-07, maxiter = 1000, plot.it = TRUE, add = FALSE, 
    n.points = ifelse(x.var == "n", diff(range.x.var) + 1, 50), 
    plot.col = "black", plot.lwd = 3 * par("cex"), plot.lty = 1, 
    digits = .Options$digits, ..., main = NULL, xlab = NULL, ylab = NULL, 
    type = "l")
}
\arguments{
  \item{x.var}{
  character string indicating what variable to use for the x-axis.  
  Possible values are \code{"n"} (sample size; the default), 
  \code{"slope.over.sigma"} (scaled minimal detectable slope), \code{"power"} 
  (power of the test), and \code{"alpha"} (significance level of the test).
}
  \item{y.var}{
  character string indicating what variable to use for the y-axis.  
  Possible values are \code{"power"} (power of the test; the default), 
  \code{"slope.over.sigma"} (scaled minimal detectable slope), and 
  \code{"n"} (sample size). 
}
  \item{range.x.var}{
  numeric vector of length 2 indicating the range of the x-variable to use 
  for the plot.  The default value depends on the value of \code{x.var}.  
  When \code{x.var="n"} the default value is \code{c(3,25)}.  When 
  \code{x.var="slope.over.sigma"} and \cr
  \code{alternative="greater"} or 
  \code{alternative="two.sided"} and \cr
  \code{two.sided.direction="greater"}, the default value is \code{c(0.1, 1)}.  
  When \code{x.var="slope.over.sigma"} and \code{alternative="less"} or \code{alternative="two.sided"} and 
  \code{two.sided.direction="less"}, the default value is \code{-c(1, 0.1)}.  
  When \code{x.var="power"} the default value is \cr
  \code{c(alpha + .Machine$double.eps, 0.95)}.  When \code{x.var="alpha"}, the 
  default value is \code{c(0.01, 0.2)}.
}
  \item{n}{
  numeric scalar indicating the sample size.  The default value is 
  \code{n=12}.  Missing (\code{NA}), undefined (\code{NaN}), 
  and infinite (\code{Inf}, \code{-Inf}) values are not allowed.  This 
  argument is ignored if either \code{x.var="n"} or \code{y.var="n"}.
}
  \item{slope.over.sigma}{
  numeric scalar specifying the ratio of the true slope (\eqn{\beta_1}) to the 
  population standard deviation of the error terms (\eqn{\sigma}).  
  This is also called the "scaled slope".  When \code{alternative="greater"} or 
  \code{alternative="two.sided"} and \cr
  \code{two.sided.direction="greater"}, the default 
  value is \code{slope.over.sigma=0.1}.  When \code{alternative="less"} or 
  \code{alternative="two.sided"} and \cr
  \code{two.sided.direction="less"}, the default 
  value is \code{delta.over.sigma=-0.1}.  This argument is ignored when 
  \code{x.var="slope.over.sigma"} or \cr
  \code{y.var="slope.over.sigma"}.
}
  \item{alpha}{
  numeric scalar between 0 and 1 indicating the Type I error level associated 
  with the hypothesis test.  The default value is \code{alpha=0.05}.  
  This argument is ignored when \code{x.var="alpha"}.
}
  \item{power}{
  numeric scalar between 0 and 1 indicating the power associated with the 
  hypothesis test.  The default value is \code{power=0.95}.  This argument is 
  ignored when \code{x.var="power"} or \code{y.var="power"}.
}
  \item{alternative}{
  character string indicating the kind of alternative hypothesis.  The possible values 
  are \code{"two.sided"} (the default), \code{"greater"}, and \code{"less"}.
}
  \item{two.sided.direction}{
  character string indicating the direction (positive or negative) for the 
  scaled minimal detectable slope when \code{alternative="two.sided"}.  When \cr
  \code{two.sided.direction="greater"} (the default), the scaled minimal detectable slope is positive.  
  When \code{two.sided.direction="less"}, the scaled minimal detectable slope is negative.  
  This argument is ignored if \code{alternative="less"} or \code{alternative="greater"}.
}
  \item{approx}{
  logical scalar indicating whether to compute the power based on an approximation to 
  the non-central t-distribution.  The default value is \code{approx=FALSE}.
}
  \item{round.up}{
  logical scalar indicating whether to round up the values of the computed 
  sample size(s) to the next smallest integer.  The default value is 
  \code{FALSE}.
}
  \item{n.max}{
  for the case when \code{y.var="n"}, a positive integer greater than 2 indicating 
  the maximum sample size.  The default value is \code{n.max=5000}.
}
  \item{tol}{
  numeric scalar indicating the toloerance to use in the 
  \code{\link{uniroot}} search algorithm.  
  The default value is \code{tol=1e-7}.
}
  \item{maxiter}{
  positive integer indicating the maximum number of iterations 
  argument to pass to the \code{\link{uniroot}} function.  The default 
  value is \code{maxiter=1000}.
}
  \item{plot.it}{
  a logical scalar indicating whether to create a new plot or add to the existing plot 
  (see \code{add}) on the current graphics device.  If \code{plot.it=FALSE}, no plot 
  is produced, but a list of (x,y) values is returned (see VALUE).  The default value 
  is \code{plot.it=TRUE}.
}
  \item{add}{
  a logical scalar indicating whether to add the design plot to the 
  existing plot (\code{add=TRUE}), or to create a plot from scratch 
  (\code{add=FALSE}).  The default value is \code{add=FALSE}.  
  This argument is ignored if \code{plot.it=FALSE}.
}
  \item{n.points}{
  a numeric scalar specifying how many (x,y) pairs to use to produce the plot.  
  There are \code{n.points} x-values evenly spaced between \code{range.x.var[1]} and \cr
  \code{range.x.var[2]}.  The default value is 
  \code{n.points=50} unless \code{x.var="n"}, in which case \cr
  \code{n.points=diff(range.x.var)+1}.
}
  \item{plot.col}{
  a numeric scalar or character string determining the color of the plotted 
  line or points.  The default value is \code{plot.col="black"}.  See the 
  entry for \code{col} in the help file for \code{\link{par}} 
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
  See the help files for \code{\link{linearTrendTestPower}}, 
  \code{\link{linearTrendTestN}}, and \code{\link{linearTrendTestScaledMds}} for 
  information on how to compute the power, sample size, or scaled minimal detectable 
  slope for a t-test for linear trend.
}
\value{
  \code{plotlinearTrendTestDesign} invisibly returns a list with components 
  \code{x.var} and \code{y.var}, giving coordinates of the points that have 
  been or would have been plotted.
}
\references{
  See the help file for \code{\link{linearTrendTestPower}}.
}
\author{
    Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  See the help files for \code{\link{linearTrendTestPower}}.
}
\seealso{
  \code{\link{linearTrendTestPower}}, \code{\link{linearTrendTestN}}, 
  \code{\link{linearTrendTestScaledMds}}.
}
\examples{
  # Look at the relationship between power and sample size for the t-test for 
  # liner trend, assuming a scaled slope of 0.1 and a 5% significance level:

  dev.new()
  plotLinearTrendTestDesign()

  #==========

  # Plot sample size vs. the scaled minimal detectable slope for various 
  # levels of power, using a 5% significance level:

  dev.new()
  plotLinearTrendTestDesign(x.var = "slope.over.sigma", y.var = "n", 
    ylim = c(0, 30), main = "") 

  plotLinearTrendTestDesign(x.var = "slope.over.sigma", y.var = "n", 
    power = 0.9, add = TRUE, plot.col = "red") 

  plotLinearTrendTestDesign(x.var = "slope.over.sigma", y.var = "n", 
    power = 0.8, add = TRUE, plot.col = "blue") 

  legend("topright", c("95\%", "90\%", "80\%"), lty = 1, bty = "n", 
    lwd = 3 * par("cex"), col = c("black", "red", "blue")) 

  title(main = paste("Sample Size vs. Scaled Slope for t-Test for Linear Trend", 
    "with Alpha=0.05 and Various Powers", sep="\n"))

  #==========

  # Clean up
  #---------
  graphics.off()
}
\keyword{ distribution }
\keyword{ design }
\keyword{ htest }
