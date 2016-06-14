\name{plotPredIntNparDesign}
\alias{plotPredIntNparDesign}
\title{
  Plots for a Sampling Design Based on a Nonparametric Prediction Interval
}
\description{
  Create plots involving sample size (\eqn{n}), number of future observations 
  (\eqn{m}), minimum number of future observations the interval should contain 
  (\eqn{k}), and confidence level (\eqn{1-\alpha}) for a nonparametric prediction 
  interval.
}
\usage{
  plotPredIntNparDesign(x.var = "n", y.var = "conf.level", range.x.var = NULL, 
    n = max(25, lpl.rank + n.plus.one.minus.upl.rank + 1), 
    k = 1, m = ifelse(x.var == "k", ceiling(max.x), 1), conf.level = 0.95, 
    pi.type = "two.sided", lpl.rank = ifelse(pi.type == "upper", 0, 1), 
    n.plus.one.minus.upl.rank = ifelse(pi.type == "lower", 0, 1), n.max = 5000, 
    maxiter = 1000, plot.it = TRUE, add = FALSE, n.points = 100, 
    plot.col = "black", plot.lwd = 3 * par("cex"), plot.lty = 1, 
    digits = .Options$digits, cex.main = par("cex"), ..., main = NULL, 
    xlab = NULL, ylab = NULL, type = "l") 
}
\arguments{
  \item{x.var}{
  character string indicating what variable to use for the x-axis.  
  Possible values are \code{"n"} (sample size; the default), 
  \code{"conf.level"} (the confidence level), \code{"k"} (minimum number of 
  future observations the interval should contain), and \code{"m"} (number of 
  future observations).
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
  When \code{x.var="k"} or \code{x.var="m"}, the default value is c(1, 20).
}
  \item{n}{
  numeric scalar indicating the sample size.  The default value is \cr 
  \code{max(25, lpl.rank + n.plus.one.minus.upl.rank + 1)}.  
  Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) 
  values are not allowed.  
  This argument is ignored if either \code{x.var="n"} or \code{y.var="n"}.
}
  \item{k}{
  positive integer specifying the minimum number of future observations out of \code{m} 
  that should be contained in the prediction interval. The default value is \code{k=1}.
}
  \item{m}{
  positive integer specifying the number of future observations.  The default value is 
  \code{ifelse(x.var == "k", ceiling(max.x), 1)}.  That is, if \code{x.var="k"} then 
  the default value is the smallest integer greater than or equal to the maximum 
  value that \eqn{k} will take on in the plot; otherwise the default value is 
  \code{m=1}.
}
  \item{conf.level}{
  numeric scalar between 0 and 1 indicating the confidence level 
  associated with the prediction interval.  The default value is 
  \code{conf.level=0.95}.
}
  \item{pi.type}{
  character string indicating what kind of prediction interval to compute.  
  The possible values are \code{"two-sided"} (the default), \code{"lower"}, and 
  \code{"upper"}.
}
  \item{lpl.rank}{
  non-negative integer indicating the rank of the order statistic to use for 
  the lower bound of the prediction interval.  If \code{pi.type="two-sided"} or 
  \code{pi.type="lower"}, the default value is \code{lpl.rank=1} (implying the 
  minimum value is used as the lower bound of the prediction interval).  
  If \code{pi.type="upper"}, this argument is set equal to \code{0}.
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
  for the case when \code{y.var="n"}, a positive integer greater than 2 indicating 
  the maximum possible sample size.  The default value is \code{n.max=5000}.
}
  \item{maxiter}{
  positive integer indicating the maximum number of iterations to use in the 
  \code{\link{uniroot}} search algorithm for sample size when \code{y.var="n"}.  
  The default value is \cr
  \code{maxiter=1000}.
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
  See the help file for \code{\link{predIntNpar}}, \code{\link{predIntNparConfLevel}}, 
  and \code{\link{predIntNparN}} for information on how to compute a 
  nonparametric prediction interval, how the confidence level 
  is computed when other quantities are fixed, and how the sample size is 
  computed when other quantities are fixed.
}
\value{
  \code{plotPredIntNparDesign} invisibly returns a list with components 
  \code{x.var} and \code{y.var}, giving coordinates of the points that 
  have been or would have been plotted.
}
\references{
  See the help file for \code{\link{predIntNpar}}.
}
\author{
    Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  See the help file for \code{\link{predIntNpar}}.
}
\seealso{
  \code{\link{predIntNpar}}, \code{\link{predIntNparConfLevel}}, 
  \code{\link{predIntNparN}}.
}
\examples{
  # Look at the relationship between confidence level and sample size for a 
  # two-sided nonparametric prediction interval for the next m=1 future observation.

  dev.new()
  plotPredIntNparDesign()

  #==========

  # Plot confidence level vs. sample size for various values of number of 
  # future observations (m):

  dev.new()
  plotPredIntNparDesign(k = 1, m = 1, ylim = c(0, 1), main = "") 

  plotPredIntNparDesign(k = 2, m = 2, add = TRUE, plot.col = "red") 

  plotPredIntNparDesign(k = 3, m = 3, add = TRUE, plot.col = "blue") 

  legend("bottomright", c("m=1", "m=2", "m=3"), lty = 1, lwd = 3 * par("cex"), 
    col = c("black", "red", "blue"), bty = "n") 

  title(main = paste("Confidence Level vs. Sample Size for Nonparametric PI", 
    "with Various Values of m", sep="\n"))

  #==========

  # Example 18-3 of USEPA (2009, p.18-19) shows how to construct 
  # a one-sided upper nonparametric prediction interval for the next 
  # 4 future observations of trichloroethylene (TCE) at a downgradient well.  
  # The data for this example are stored in EPA.09.Ex.18.3.TCE.df.  
  # There are 6 monthly observations of TCE (ppb) at 3 background wells, 
  # and 4 monthly observations of TCE at a compliance well.
  #
  # Modify this example by creating a plot to look at confidence level versus 
  # sample size (i.e., number of observations at the background wells) for 
  # predicting the next m = 4 future observations when constructing a one-sided 
  # upper prediction interval based on the maximum value.

  dev.new()
  plotPredIntNparDesign(k = 4, m = 4, pi.type = "upper")

  #==========

  # Clean up
  #---------
  graphics.off()
}
\keyword{ distribution }
\keyword{ design }
\keyword{ htest }

