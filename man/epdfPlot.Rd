\name{epdfPlot}
\alias{epdfPlot}
\alias{empirical PDF}
\title{
  Plot Empirical Probability Density Function
}
\description{
  Produces an empirical probability density function plot.
}
\usage{
  epdfPlot(x, discrete = FALSE, density.arg.list = NULL, plot.it = TRUE, 
    add = FALSE, epdf.col = "black", epdf.lwd = 3 * par("cex"), epdf.lty = 1, 
    curve.fill = FALSE, curve.fill.col = "cyan", ..., 
    type = ifelse(discrete, "h", "l"), main = NULL, xlab = NULL, ylab = NULL, 
    xlim = NULL, ylim = NULL)
}
\arguments{
  \item{x}{
  numeric vector of observations.  Missing (\code{NA}), undefined (\code{NaN}), and 
  infinite (\code{Inf}, \code{-Inf}) values are allowed but will be removed.
}
  \item{discrete}{
  logical scalar indicating whether the assumed parent distribution of \code{x} is 
  discrete (\code{discrete=TRUE}) or continuous (\code{discrete=FALSE}; the default).
}
  \item{density.arg.list}{
  list with arguments to the \code{\link{density}} function.  The default value is \cr
  \code{density.arg.list=NULL}.  This argument is ignored if \code{discrete=TRUE}.
}
  \item{plot.it}{
  logical scalar indicating whether to produce a plot or add to the current plot (see \code{add}) 
  on the current graphics device.  The default value is \code{plot.it=TRUE}.
}
  \item{add}{
  logical scalar indicating whether to add the empirical pdf to the current plot 
  (\code{add=TRUE}) or generate a new plot (\code{add=FALSE}; the default).  
  This argument is ignored if \code{plot.it=FALSE}.
}
  \item{epdf.col}{
  a numeric scalar or character string determining the color of the empirical pdf 
  line or points.  The default value is \code{epdf.col="black"}.  
  See the entry for \code{col} in the help file for 
  \code{\link{par}} for more information.
}
  \item{epdf.lwd}{
  a numeric scalar determining the width of the empirical pdf line.  
  The default value is \code{epdf.lwd=3*par("cex")}.  See the entry for 
  \code{lwd} in the help file for \code{\link{par}} 
  for more information.
}
  \item{epdf.lty}{
  a numeric scalar determining the line type of the empirical pdf line.  
  The default value is \code{ecdf.lty=1}.  See the entry for \code{lty} in the help file for \code{\link{par}} 
  for more information.
}
  \item{curve.fill}{
  a logical scalar indicating whether to fill in the area below the empirical pdf 
  curve with the 
  color specified by \code{curve.fill.col}. The default value is \cr
  \code{curve.fill=FALSE}.
}
  \item{curve.fill.col}{
  a numeric scalar or character string indicating what color to use to fill in the 
  area below the empirical pdf curve.  The default value is 
  \code{curve.fill.col="cyan"}.  This argument is ignored if \code{curve.fill=FALSE}.
}
  \item{type, main, xlab, ylab, xlim, ylim, \dots}{
  additional graphical parameters (see \code{\link{lines}} and \code{\link{par}}).  
  In particular, the argument \code{type} specifies the kind of line type.  
  By default, the function \code{epdfPlot} plots histogram-like vertical lines 
  (\code{type="h"}) when \code{discrete=TRUE}, and 
  plots a straight line between points (\code{type="l"}) when \code{discrete=FALSE}.  
  The user may override these defaults by supplying the graphics parameter \code{type} 
  (\code{type="h"} for histogram-like vertical lines, \code{type="l"} for linear 
  interpolation, \code{type="p"} for points only, etc.).
}
}
\details{
  When a distribution is discrete and can only take on a finite number of values, 
  the empirical pdf plot is the same as the standard relative frequency histogram; 
  that is, each bar of the histogram represents the proportion of the sample 
  equal to that particular number (or category).  When a distribution is continuous, 
  the function \code{epdfPlot} calls the \R function \code{\link{density}} to 
  compute the estimated probability density at a number of evenly spaced points 
  between the minimum and maximum values.
}
\value{
  \code{epdfPlot} invisibly returns a list with the following components:

  \item{x}{numeric vector of ordered quantiles.}
  \item{f.x}{numeric vector of the associated estimated values of the pdf.}
}
\references{
  Chambers, J.M., W.S. Cleveland, B. Kleiner, and P.A. Tukey. (1983). 
  \emph{Graphical Methods for Data Analysis}. Duxbury Press, Boston, MA.

  See the REFERENCES section in the help file for \code{\link{density}}.
}
\author{
    Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  An \bold{\emph{empirical probability density function (epdf) plot}} is a 
  graphical tool that can be used in conjunction with other graphical tools 
  such as \link[=hist]{histograms} and \link[=boxplot]{boxplots} to assess 
  the characteristics of a set of data. 
}
\seealso{
  \link{Empirical}, \code{\link{pdfPlot}}, \code{\link{ecdfPlot}}, 
  \code{\link{cdfPlot}}, \code{\link{cdfCompare}}, \code{\link{qqPlot}}.
}
\examples{
  # Using Reference Area TcCB data in EPA.94b.tccb.df, 
  # create a histogram of the log-transformed observations, 
  # then superimpose the empirical pdf plot.

  dev.new()
  log.TcCB <- with(EPA.94b.tccb.df, log(TcCB[Area == "Reference"]))

  hist(log.TcCB, freq = FALSE, xlim = c(-2, 1),
    col = "cyan", xlab = "log [ TcCB (ppb) ]",
    ylab = "Relative Frequency", 
    main = "Reference Area TcCB with Empirical PDF")

  epdfPlot(log.TcCB, add = TRUE)

  #==========

  # Generate 20 observations from a Poisson distribution with 
  # parameter lambda = 10, and plot the empirical PDF.

  set.seed(875)
  x <- rpois(20, lambda = 10)
  dev.new()
  epdfPlot(x, discrete = TRUE)

  #==========

  # Clean up
  #---------
  rm(log.TcCB, x)
  graphics.off()
}
\keyword{distribution}
\keyword{hplot}

