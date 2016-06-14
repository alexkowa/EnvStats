\name{plotTTestDesign}
\alias{plotTTestDesign}
\title{
   Plots for a Sampling Design Based on a One- or Two-Sample t-Test 
}
\description{
  Create plots involving sample size, power, scaled difference, and significance 
  level for a one- or two-sample t-test.
}
\usage{
  plotTTestDesign(x.var = "n", y.var = "power", range.x.var = NULL, 
    n.or.n1 = 25, n2 = n.or.n1, 
    delta.over.sigma = switch(alternative, greater = 0.5, less = -0.5, 
      two.sided = ifelse(two.sided.direction == "greater", 0.5, -0.5)), 
    alpha = 0.05, power = 0.95, 
    sample.type = ifelse(!missing(n2), "two.sample", "one.sample"), 
    alternative = "two.sided", two.sided.direction = "greater", approx = FALSE, 
    round.up = FALSE, n.max = 5000, tol = 1e-07, maxiter = 1000, plot.it = TRUE, 
    add = FALSE, n.points = 50, plot.col = "black", plot.lwd = 3 * par("cex"), 
    plot.lty = 1, digits = .Options$digits, ..., main = NULL, xlab = NULL, 
    ylab = NULL, type = "l") 
}
\arguments{
  \item{x.var}{
  character string indicating what variable to use for the x-axis.  
  Possible values are \code{"n"} (sample size; the default), 
  \code{"delta.over.sigma"} (scaled minimal detectable difference), \code{"power"} 
  (power of the test), and \code{"alpha"} (significance level of the test).
}
  \item{y.var}{
  character string indicating what variable to use for the y-axis.  
  Possible values are \code{"power"} (power of the test; the default), 
  \code{"delta.over.sigma"} (scaled minimal detectable difference), and 
  \code{"n"} (sample size). 
}
  \item{range.x.var}{
  numeric vector of length 2 indicating the range of the x-variable to use 
  for the plot.  The default value depends on the value of \code{x.var}.  
  When \code{x.var="n"} the default value is \code{c(2,50)}.  When 
  \code{x.var="delta.over.sigma"} and \cr
  \code{alternative="greater"} or 
  \code{alternative="two.sided"} and \cr
  \code{two.sided.direction="greater"}, 
  the default value is \code{c(0.5, 2)}.  When \code{x.var="delta.over.sigma"} and 
  \code{alternative="less"} or \cr
  \code{alternative="two.sided"} and \code{two.sided.direction="less"}, the default value is 
  \code{-c(2, 0.5)}. When \code{x.var="power"} the default value is \cr
  \code{c(alpha + .Machine$double.eps, 0.95)}.  When \code{x.var="alpha"}, the 
  default value is \code{c(0.01, 0.2)}.
}
  \item{n.or.n1}{
  numeric scalar indicating the sample size.  The default value is 
  \code{n.or.n1=25}.  When \code{sample.type="one.sample"}, \code{n.or.n1} 
  denotes the number of observations in the single sample.  When 
  \code{sample.type="two.sample"}, \code{n.or.n1} denotes the number of 
  observations from group 1.  Missing (\code{NA}), undefined (\code{NaN}), 
  and infinite (\code{Inf}, \code{-Inf}) values are not allowed.  This 
  argument is ignored if either \code{x.var="n"} or \code{y.var="n"}.
}
  \item{n2}{
  numeric scalar indicating the sample size for group 2.  The default value 
  is the value of \code{n.or.n1}.  Missing (\code{NA}), undefined (\code{NaN}), 
  and infinite (\code{Inf}, \code{-Inf}) values are not allowed.  This 
  argument is ignored when \code{sample.type="one.sample"}.
}
  \item{delta.over.sigma}{
  numeric scalar specifying the ratio of the true difference (\eqn{\delta}) to the 
  population standard deviation (\eqn{\sigma}).  This is also called the 
  "scaled difference".  When \code{alternative="greater"} or 
  \code{alternative="two.sided"} and \cr
  \code{two.sided.direction="greater"}, the default 
  value is \code{delta.over.sigma=0.5}.  When \code{alternative="less"} or 
  \code{alternative="two.sided"} and \cr
  \code{two.sided.direction="less"}, the default 
  value is \code{delta.over.sigma=-0.5}.  This argument is ignored when 
  \code{x.var="delta.over.sigma"} or \cr
  \code{y.var="delta.over.sigma"}.
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
  \item{sample.type}{
  character string indicating whether the design is based on a one-sample or 
  two-sample t-test.  When \code{sample.type="one.sample"}, the computations 
  for the plot are based on a one-sample t-test.  When 
  \code{sample.type="two.sample"}, the computations for the plot are based on a 
  two-sample t-test.  The default value is \code{sample.type="one.sample"}.
}
  \item{alternative}{
  character string indicating the kind of alternative hypothesis.  The possible 
  values are \code{"two.sided"} (the default), \code{"less"}, and \code{"greater"}.
}
  \item{two.sided.direction}{
  character string indicating the direction (positive or negative) for the scaled 
  minimal detectable difference when \code{alternative="two.sided"}.  When \cr
  \code{two.sided.direction="greater"} (the default), the scaled minimal detectable 
  difference is positive.  When \code{two.sided.direction="less"}, the scaled minimal 
  detectable difference is negative.  This argument is ignored unless \cr
  \code{alternative="two.sided"} and either \code{x.var="delta"} or 
  \code{y.var="delta"}.
}
  \item{approx}{
  logical scalar indicating whether to compute the power based on an approximation 
  to the non-central t-distribution. The default value is \code{approx=FALSE}.
}
  \item{round.up}{
  logical scalar indicating whether to round up the values of the computed sample 
  size(s) to the next smallest integer.  The default value is \code{round.up=FALSE}.  
  This argument is ignored unless \code{y.var="n"}.
}
  \item{n.max}{
  for the case when \code{y.var="n"}, a positive integer greater than 1 indicating 
  the maximum sample size when \code{sample.type="one.sample"} or the maximum sample 
  size for group 1 when \code{sample.type="two.sample"}.  The default value is 
  \code{n.max=5000}.
}
  \item{tol}{
  numeric scalar relevant to the case when \code{y.var="n"} or 
  \code{y.var="delta.over.sigma"}.  
  This argument is passed to the \code{uniroot} function and indicates the tolerance 
  to use in the search algorithm.  The default value is \code{tol=1e-7}.
}  
  \item{maxiter}{
  numeric scalar relevant to the case when \code{y.var="n"} and \code{approx=FALSE} 
  (i.e., when the power is based on the exact test), or when 
  \code{y.var="delta.over.sigma"}.  This argument is passed to the 
  \code{\link{uniroot}} function and is a positive integer indicating the 
  maximum number of iterations.  The default value is \code{maxiter=1000}.
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
  \code{range.x.var[2]}.  The default value is \code{n.points=50}.
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
  See the help files for \code{\link{tTestPower}}, \code{\link{tTestN}}, and 
  \code{\link{tTestScaledMdd}} for information on how to compute the power, 
  sample size, or scaled minimal detectable difference for a one- or two-sample 
  t-test.
}
\value{
  \code{plotTTestDesign} invisibly returns a list with components 
  \code{x.var} and \code{y.var}, giving coordinates of the points that have 
  been or would have been plotted.
}
\references{
  See the help files for \code{\link{tTestPower}}, \code{\link{tTestN}}, and 
  \code{\link{tTestScaledMdd}}.
}
\author{
    Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  See the help files for \code{\link{tTestPower}}, \code{\link{tTestN}}, and 
  \code{\link{tTestScaledMdd}}.
}
\seealso{
  \code{\link{tTestPower}}, \code{\link{tTestN}}, 
  \code{\link{tTestScaledMdd}}, \code{\link{t.test}}.
}
\examples{
  # Look at the relationship between power and sample size for a two-sample t-test, 
  # assuming a scaled difference of 0.5 and a 5% significance level:

  dev.new()
  plotTTestDesign(sample.type = "two")

  #----------

  # For a two-sample t-test, plot sample size vs. the scaled minimal detectable 
  # difference for various levels of power, using a 5% significance level:

  dev.new()
  plotTTestDesign(x.var = "delta.over.sigma", y.var = "n", sample.type = "two", 
    ylim = c(0, 110), main="") 

  plotTTestDesign(x.var = "delta.over.sigma", y.var = "n", sample.type = "two", 
    power = 0.9, add = TRUE, plot.col = "red") 

  plotTTestDesign(x.var = "delta.over.sigma", y.var = "n", sample.type = "two", 
    power = 0.8, add = TRUE, plot.col = "blue") 

  legend("topright", c("95\%", "90\%", "80\%"), lty = 1, 
    lwd = 3 * par("cex"), col = c("black", "red", "blue"), bty = "n")

  title(main = paste("Sample Size vs. Scaled Difference for", 
    "Two-Sample t-Test, with Alpha=0.05 and Various Powers", 
    sep="\n"))

  #==========

  # Modifying the example on pages 21-4 to 21-5 of USEPA (2009), look at 
  # power versus scaled minimal detectable difference for various sample 
  # sizes in the context of the problem of using a one-sample t-test to 
  # compare the mean for the well with the MCL of 7 ppb.  Use alpha = 0.01, 
  # assume an upper one-sided alternative (i.e., compliance well mean larger 
  # than 7 ppb).

  dev.new()
  plotTTestDesign(x.var = "delta.over.sigma", y.var = "power", 
    range.x.var = c(0.5, 2), n.or.n1 = 8, alpha = 0.01, 
    alternative = "greater", ylim = c(0, 1), main = "")

  plotTTestDesign(x.var = "delta.over.sigma", y.var = "power", 
    range.x.var = c(0.5, 2), n.or.n1 = 6, alpha = 0.01, 
    alternative = "greater", add = TRUE, plot.col = "red")

  plotTTestDesign(x.var = "delta.over.sigma", y.var = "power", 
    range.x.var = c(0.5, 2), n.or.n1 = 4, alpha = 0.01, 
    alternative = "greater", add = TRUE, plot.col = "blue")

  legend("topleft", paste("N =", c(8, 6, 4)), lty = 1, lwd = 3 * par("cex"), 
    col = c("black", "red", "blue"), bty = "n")

  title(main = paste("Power vs. Scaled Difference for One-Sample t-Test", 
    "with Alpha=0.01 and Various Sample Sizes", sep="\n"))

  #==========

  # Clean up
  #---------
  graphics.off()
}
\keyword{ distribution }
\keyword{ design }
\keyword{ htest }
