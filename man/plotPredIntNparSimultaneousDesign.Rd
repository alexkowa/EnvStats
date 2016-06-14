\name{plotPredIntNparSimultaneousDesign}
\alias{plotPredIntNparSimultaneousDesign}
\title{
  Plots for a Sampling Design Based on a Simultaneous Nonparametric Prediction Interval
}
\description{
  Create plots involving sample size (\eqn{n}), number of future observations 
  (\eqn{m}), minimum number of future observations the interval should contain 
  (\eqn{k}), number of future sampling occasions (\eqn{r}), and confidence level 
  \eqn{(1-\alpha)} for a simultaneous nonparametric prediction interval.
}
\usage{
  plotPredIntNparSimultaneousDesign(x.var = "n", y.var = "conf.level", 
    range.x.var = NULL, n = max(25, lpl.rank + n.plus.one.minus.upl.rank + 1), 
    n.median = 1, k = 1, m = ifelse(x.var == "k", ceiling(max.x), 1), r = 2, 
    rule = "k.of.m", conf.level = 0.95, pi.type = "upper", 
    lpl.rank = ifelse(pi.type == "upper", 0, 1), 
    n.plus.one.minus.upl.rank = ifelse(pi.type == "lower", 0, 1), n.max = 5000, 
    maxiter = 1000, integrate.args.list = NULL, plot.it = TRUE, add = FALSE, 
    n.points = 100, plot.col = "black", plot.lwd = 3 * par("cex"), plot.lty = 1, 
    digits = .Options$digits, cex.main = par("cex"), ..., main = NULL, 
    xlab = NULL, ylab = NULL, type = "l")
}
\arguments{
  \item{x.var}{
  character string indicating what variable to use for the x-axis.  
  Possible values are \code{"n"} (sample size; the default), 
  \code{"conf.level"} (the confidence level), \code{"k"} (minimum number of 
  future observations the interval should contain), \code{"m"} (number of 
  future observations), and \code{"r"} (number of future sampling occasions).
}
  \item{y.var}{
  character string indicating what variable to use for the y-axis.  
  Possible values are \code{"conf.level"} (confidence level; the default), and 
  \code{"n"} (sample size).
}
  \item{range.x.var}{
  numeric vector of length 2 indicating the range of the x-variable to use 
  for the plot.  The default value depends on the value of \code{x.var}.  
  When \code{x.var="n"} the default value is \code{c(2,50)}.  
  When \code{x.var="conf.level"}, the default value is \code{c(0.5, 0.99)}.  
  When \code{x.var="k"}, \code{x.var="m"}, or \code{x.var="r"}, the default value 
  is c(1, 20).
}
  \item{n}{
  numeric scalar indicating the sample size.  The default value is \cr 
  \code{max(25, lpl.rank + n.plus.one.minus.upl.rank + 1)}.  
  Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) 
  values are not allowed.  
  This argument is ignored if either \code{x.var="n"} or \code{y.var="n"}.
}
  \item{n.median}{
  positive odd integer specifying the sample size associated with the future medians.  
  The default value is \code{n.median=1} (i.e., individual observations).  Note that 
  all future medians must be based on the same sample size.
}
  \item{k}{
  for the \eqn{k}-of-\eqn{m} rule (\code{rule="k.of.m"}), a positive integer 
  specifying the minimum number of observations (or medians) out of \eqn{m} 
  observations (or medians) (all obtained on one future sampling \dQuote{occassion}) 
  the prediction interval should contain.  
  The default value is \code{k=1}.  This argument is ignored when the argument 
  \code{rule} is not equal to \code{"k.of.m"}.
}
  \item{m}{
  positive integer specifying the maximum number of future observations (or 
  medians) on one future sampling \dQuote{occasion}.  
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
  See the DETAILS section below for more information.
}
  \item{conf.level}{
  numeric scalar between 0 and 1 indicating the confidence level 
  associated with the prediction interval.  The default value is 
  \code{conf.level=0.95}.
}
  \item{pi.type}{
  character string indicating what kind of prediction interval to compute.  
  The possible values are \code{"upper"} (the default) and \code{"lower"}.
}
  \item{lpl.rank}{
  non-negative integer indicating the rank of the order statistic to use for 
  the lower bound of the prediction interval.  If \code{pi.type="lower"}, the 
  default value is \code{lpl.rank=1} (implying the minimum value is used as the 
  lower bound of the prediction interval).  If \code{pi.type="upper"}, this 
  argument is set equal to \code{0}.
}
  \item{n.plus.one.minus.upl.rank}{
  non-negative integer related to the rank of the order statistic to use for 
  the upper bound of the prediction interval.  A value of 
  \code{n.plus.one.minus.upl.rank=1} (the default) means use the 
  first largest value, and in general a value of \cr
  \code{n.plus.one.minus.upl.rank=}\eqn{i} means use the \eqn{i}'th largest value.  
  If \cr
  \code{pi.type="lower"}, this argument is set equal to \code{0}.
}
  \item{n.max}{
  numeric scalar indicating the maximum sample size to consider when \code{y.var="n"}.  
  This argument is used in the search algorithm to determine the required sample size.  
  The default value is \code{n.max=5000}.
}
  \item{maxiter}{
  positive integer indicating the maximum number of iterations to use in the 
  \code{\link{uniroot}} search algorithm when \code{y.var="n"}.  The default value is 
  \code{maxiter=1000}.
}
  \item{integrate.args.list}{
  list of arguments to supply to the \code{\link{integrate}} function.  The default 
  value is \code{NULL}.
}
  \item{plot.it}{
  a logical scalar indicating whether to create a plot or add to the 
  existing plot (see \code{add}) on the current graphics device.  If 
  \code{plot.it=FALSE}, no plot is produced, but a list of (x,y) values 
  is returned (see VALUE).  The default value is \code{plot.it=TRUE}.
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
  \code{range.x.var[2]}.  The default value is \code{n.points=100}.
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
  \item{cex.main, main, xlab, ylab, type, \dots}{
  additional graphical parameters (see \code{\link{par}}).
}
}
\details{
  See the help file for \code{\link{predIntNparSimultaneous}}, 
  \code{\link{predIntNparSimultaneousConfLevel}}, and \cr
  \code{\link{predIntNparSimultaneousN}} for information on how to compute a 
  simultaneous nonparametric prediction interval, how the confidence level 
  is computed when other quantities are fixed, and how the sample size is 
  computed when other quantities are fixed.
}
\value{
  \code{plotPredIntNparSimultaneousDesign} invisibly returns a list with components 
  \code{x.var} and \code{y.var}, giving coordinates of the points that 
  have been or would have been plotted.
}
\references{
  See the help file for \code{\link{predIntNparSimultaneous}}.
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  See the help file for \code{\link{predIntNparSimultaneous}}.
}
\seealso{
  \code{\link{predIntNparSimultaneous}}, 
  \code{\link{predIntNparSimultaneousConfLevel}}, \cr
  \code{\link{predIntNparSimultaneousN}}, 
  \code{\link{predIntNparSimultaneousTestPower}}, 
  \code{\link{predIntNpar}}, \code{\link{tolIntNpar}}.
}
\examples{
  # For the 1-of-3 rule with r=20 future sampling occasions, look at the 
  # relationship between confidence level and sample size for a one-sided 
  # upper simultaneous nonparametric prediction interval.

  dev.new()
  plotPredIntNparSimultaneousDesign(k = 1, m = 3, r = 20, range.x.var = c(2, 20))

  #==========

  # Plot confidence level vs. sample size for various values of number of 
  # future sampling occasions (r):

  dev.new()
  plotPredIntNparSimultaneousDesign(m = 3, r = 10, rule = "CA", 
    ylim = c(0, 1), main = "") 

  plotPredIntNparSimultaneousDesign(m = 3, r = 20, rule = "CA", add = TRUE, 
    plot.col = "red") 

  plotPredIntNparSimultaneousDesign(m = 3, r = 30, rule = "CA", add = TRUE, 
    plot.col = "blue") 

  legend("bottomright", c("r=10", "r=20", "r=30"), lty = 1, lwd = 3 * par("cex"), 
    col = c("black", "red", "blue"), bty = "n") 

  title(main = paste("Confidence Level vs. Sample Size for Simultaneous", 
    "Nonparametric PI with Various Values of r", sep="\n"))

  #==========

  # Modifying Example 19-5 of USEPA (2009, p. 19-33), plot confidence level 
  # versus sample size (number of background observations requried) for 
  # a 1-of-3 plan assuming r = 10 compliance wells (future sampling occasions).
  
  dev.new()
  plotPredIntNparSimultaneousDesign(k = 1, m = 3, r = 10, rule = "k.of.m")

  #==========

  # Clean up
  #---------
  graphics.off()
}
\keyword{ distribution }
\keyword{ design }
\keyword{ htest }


