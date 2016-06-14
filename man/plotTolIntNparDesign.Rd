\name{plotTolIntNparDesign}
\alias{plotTolIntNparDesign}
\title{
  Plots for a Sampling Design Based on a Nonparametric Tolerance Interval
}
\description{
  Create plots involving sample size (\eqn{n}), coverage (\eqn{\beta}), and confidence level 
  \eqn{(1-\alpha)} for a nonparametric tolerance interval.
}
\usage{
  plotTolIntNparDesign(x.var = "n", y.var = "conf.level", range.x.var = NULL, n = 25, 
    coverage = 0.95, conf.level = 0.95, ti.type = "two.sided", cov.type = "content", 
    ltl.rank = ifelse(ti.type == "upper", 0, 1), 
    n.plus.one.minus.utl.rank = ifelse(ti.type == "lower", 0, 1), plot.it = TRUE, 
    add = FALSE, n.points = 100, plot.col = "black", plot.lwd = 3 * par("cex"), 
    plot.lty = 1, digits = .Options$digits, cex.main = par("cex"), ..., main = NULL, 
    xlab = NULL, ylab = NULL, type = "l")
}
\arguments{
  \item{x.var}{
  character string indicating what variable to use for the x-axis.  Possible values are 
  \code{"n"} (sample size; the default), \code{"coverage"} (the coverage), and \code{"conf.level"} 
  (the confidence level). 
}
  \item{y.var}{
  character string indicating what variable to use for the y-axis.  Possible values are 
  \code{"conf.level"} (the confidence level; the default), \code{"n"} (sample size), and 
  \code{"coverage"} (the coverage). 
}
  \item{range.x.var}{
  numeric vector of length 2 indicating the range of the x-variable to use for the plot.  The 
  default value depends on the value of \code{x.var}.  When \code{x.var="n"} the default value is 
  \code{c(2,50)}.  When \code{x.var="coverage"} or \code{x.var="conf"}, the default value is 
  \code{c(0.5, 0.99)}.
}
  \item{n}{
  numeric scalar indicating the sample size.  The default value is \cr
  \code{max(25, lpl.rank + n.plus.one.minus.upl.rank + 1)}.  
  Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) 
  values are not allowed.  
  This argument is ignored if either \code{x.var="n"} or \code{y.var="n"}.
}
  \item{coverage}{
  numeric scalar between 0 and 1 specifying the coverage of the tolerance interval.  The default 
  value is \code{coverage=0.95}.  This argument is ignored if \cr
  \code{x.var="coverage"} or \code{y.var="coverage"}.
}
  \item{conf.level}{
  a scalar between 0 and 1 indicating the confidence level associated with the tolerance interval.  
  The default value is \code{conf.level=0.95}.  This argument is ignored if \code{x.var="conf.level"} 
  or \code{y.var="conf.level"}, or if \cr
  \code{cov.type="expectation"}.
}
  \item{ti.type}{
  character string indicating what kind of tolerance interval to compute.  
  The possible values are \code{"two-sided"} (the default), \code{"lower"}, and 
  \code{"upper"}.
}
  \item{cov.type}{
  character string specifying the coverage type for the tolerance interval.  
  The possible values are \code{"content"} (\eqn{\beta}-content; the default), and 
  \code{"expectation"} (\eqn{\beta}-expectation).
}
  \item{ltl.rank}{
  vector of positive integers indicating the rank of the order statistic to use for the lower bound 
  of the tolerance interval.  If \code{ti.type="two-sided"} or \cr
  \code{ti.type="lower"}, 
  the default value is \code{ltl.rank=1} (implying the minimum value of \code{x} is used 
  as the lower bound of the tolerance interval).  If \cr
  \code{ti.type="upper"}, this argument 
  is set equal to \code{0}.
}
  \item{n.plus.one.minus.utl.rank}{
  vector of positive integers related to the rank of the order statistic to use for 
  the upper bound of the tolerance interval.  A value of 
  \code{n.plus.one.minus.utl.rank=1} (the default) means use the 
  first largest value, and in general a value of \cr
  \code{n.plus.one.minus.utl.rank=}\eqn{i} means use the \eqn{i}'th largest value.  
  If \cr
  \code{ti.type="lower"}, this argument is set equal to \code{0}.
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
  See the help file for \code{\link{tolIntNpar}}, \code{\link{tolIntNparConfLevel}}, 
  \code{\link{tolIntNparCoverage}}, and \code{\link{tolIntNparN}} for information on how 
  to compute a nonparametric tolerance interval, how the confidence level 
  is computed when other quantities are fixed, how the coverage is computed when other 
  quantites are fixed, and and how the sample size is computed when other quantities are fixed.
}
\value{
  \code{plotTolIntNparDesign} invisibly returns a list with components 
  \code{x.var} and \code{y.var}, giving coordinates of the points that 
  have been or would have been plotted.
}
\references{
  See the help file for \code{\link{tolIntNpar}}.
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  See the help file for \code{\link{tolIntNpar}}.

  In the course of designing a sampling program, an environmental scientist may wish to determine 
  the relationship between sample size, coverage, and confidence level if one of the objectives of 
  the sampling program is to produce tolerance intervals.  The functions 
  \code{\link{tolIntNparN}}, \code{\link{tolIntNparCoverage}}, \code{\link{tolIntNparConfLevel}}, and 
  \code{plotTolIntNparDesign} can be used to investigate these relationships for 
  constructing nonparametric tolerance intervals.
}
\seealso{
  \code{\link{tolIntNpar}}, \code{\link{tolIntNparConfLevel}}, \code{\link{tolIntNparCoverage}}, 
  \code{\link{tolIntNparN}}.
}
\examples{
  # Look at the relationship between confidence level and sample size for a two-sided 
  # nonparametric tolerance interval.

  dev.new()
  plotTolIntNparDesign()

  #==========

  # Plot confidence level vs. sample size for various values of coverage:

  dev.new()
  plotTolIntNparDesign(coverage = 0.7, ylim = c(0,1), main = "") 

  plotTolIntNparDesign(coverage = 0.8, add = TRUE, plot.col = "red") 

  plotTolIntNparDesign(coverage = 0.9, add = TRUE, plot.col = "blue") 

  legend("bottomright", c("coverage = 70\%", "coverage = 80\%", "coverage = 90\%"), lty=1, 
    lwd = 3 * par("cex"), col = c("black", "red", "blue"), bty = "n") 

  title(main = paste("Confidence Level vs. Sample Size for Nonparametric TI", 
    "with Various Levels of Coverage", sep = "\n"))

  #==========

  # Example 17-4 on page 17-21 of USEPA (2009) uses copper concentrations (ppb) from 3 
  # background wells to set an upper limit for 2 compliance wells.  There are 6 observations 
  # per well, and the maximum value from the 3 wells is set to the 95% confidence upper 
  # tolerance limit, and we need to determine the coverage of this tolerance interval.  

  tolIntNparCoverage(n = 24, conf.level = 0.95, ti.type = "upper")
  #[1] 0.8826538

  # Here we will modify the example and look at confidence level versus coverage for 
  # a set sample size of n = 24.

  dev.new()
  plotTolIntNparDesign(x.var = "coverage", y.var = "conf.level", n = 24, ti.type = "upper")

  #==========

  # Clean up
  #---------
  graphics.off()
}
\keyword{ distribution }
\keyword{ design }
\keyword{ htest }
