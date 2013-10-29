\name{cdfPlot}
\alias{cdfPlot}
\alias{Cumulative Distribution}
\alias{Plot Cumulative Distribution}
\alias{Plot CDF}
\title{
  Plot Cumulative Distribution Function
}
\description{
  Produce a cumulative distribution function (cdf) plot for a user-specified distribution.
}
\usage{
  cdfPlot(distribution = "norm", param.list = list(mean = 0, sd = 1), 
    left.tail.cutoff = ifelse(is.finite(supp.min), 0, 0.001), 
    right.tail.cutoff = ifelse(is.finite(supp.max), 0, 0.001), plot.it = TRUE, 
    add = FALSE, n.points = 1000, cdf.col = "black", cdf.lwd = 3 * par("cex"), 
    cdf.lty = 1, curve.fill = FALSE, curve.fill.col = "cyan", 
    digits = .Options$digits, ..., type = ifelse(discrete, "s", "l"), 
    main = NULL, xlab = NULL, ylab = NULL, xlim = NULL, ylim = NULL)
}
\arguments{
  \item{distribution}{
  a character string denoting the distribution abbreviation.  The default value is 
  \code{distribution="norm"}.  See the help file for \code{\link{Distribution.df}} for a 
  list of possible distribution abbreviations.
}
  \item{param.list}{
  a list with values for the parameters of the distribution.  The default value is 
  \code{param.list=list(mean=0, sd=1)}.  See the help file for 
  \code{\link{Distribution.df}} for the names and possible values of the parameters 
  associated with each distribution.
}
  \item{left.tail.cutoff}{
  a numeric scalar indicating what proportion of the left-tail of the probability 
  distribution to omit from the plot.  For densities with a finite support minimum 
  (e.g., \link{Lognormal}) the default value is \code{0}; for all other densities the default 
  value is \code{0.001}.
}
  \item{right.tail.cutoff}{
  a scalar indicating what proportion of the right-tail of the probability 
  distribution to omit from the plot.  For densities with a finite support maximum 
  (e.g., \link{Binomial}) the default value is \code{0}; for all other densities the 
  default value is \code{0.001}.
}
  \item{plot.it}{
  a logical scalar indicating whether to create a plot or add to the existing plot 
  (see \code{add}) on the current graphics device.  If \code{plot.it=FALSE}, no 
  plot is produced, but a list of \eqn{(x, y)} values is returned (see the section 
  VALUE below). The default value is \code{plot.it=TRUE}.
}
  \item{add}{
  a logical scalar indicating whether to add the cumulative distribution function curve 
  to the existing plot (\code{add=TRUE}), or to create a new plot 
  (\code{add=FALSE}; the default).  This argument is ignored if \code{plot.it=FALSE}.
}
  \item{n.points}{
  a numeric scalar specifying at how many evenly-spaced points the cumulative 
  distribution function will be evaluated.  The default value is \code{n.points=1000}.
}
  \item{cdf.col}{
  a numeric scalar or character string determining 
  the color of the cdf line in the plot.  
  The default value is \code{pdf.col="black"}.  See the entry for \code{col} in the 
  help file for \code{\link{par}} for more information.
}
  \item{cdf.lwd}{
  a numeric scalar determining the width of the cdf 
  line in the plot.  
  The default value is \code{pdf.lwd=3*par("cex")}.  
  See the entry for \code{lwd} in the help file for \code{\link{par}} 
  for more information.
}
  \item{cdf.lty}{
  a numeric scalar determining the line type of 
  the cdf line in the plot.  
  The default value is \code{pdf.lty=1}.  See the entry for 
  \code{lty} in the help file for \code{\link{par}} for more information.
}
  \item{curve.fill}{
  a logical value indicating whether to fill in 
  the area below the cumulative distribution function curve with the color specified by 
  \code{curve.fill.col}.  The default value is \code{curve.fill=FALSE}.
}
  \item{curve.fill.col}{
  when \code{curve.fill=TRUE}, a numeric scalar or character string 
  indicating what color to use to fill in the 
  area below the cumulative distribution function curve.  The default value is 
  \code{curve.fill.col="cyan"}.  See the entry for \code{col} in the 
  help file for \code{\link{par}} for more information.
}
  \item{digits}{
  a scalar indicating how many significant digits to print for the distribution 
  parameters.  The default value is \code{digits=.Options$digits}.
}
  \item{type, main, xlab, ylab, xlim, ylim, \dots}{
  additional graphical parameters (see \code{\link{lines}} and \code{\link{par}}).  
  In particular, the argument \code{type} specifies the kind of line type.  
  By default, the function \code{cdfPlot} plots a step function (\code{type="s"}) 
  for discrete distributions, and plots a straight line between points 
  (\code{type="l"}) otherwise.  
  The user may override these defaults by supplying the graphics parameter \code{type} 
  (\code{type="s"} for a step function, \code{type="l"} for linear interpolation, 
  \code{type="p"} for points only, etc.). 
}
}
\details{
  The \bold{\emph{cumulative distribution function (cdf)}} of a random variable \eqn{X}, 
  usually denoted \eqn{F}, is defined as:
  \deqn{F(x) = Pr(X \le x) \;\;\;\;\;\; (1)}
  That is, \eqn{F(x)} is the probability that \eqn{X} is less than or equal to 
  \eqn{x}.  This is the probability that the random variable \eqn{X} takes on a 
  value in the interval \eqn{(-\infty, x]} and is simply the (Lebesgue) integral of 
  the pdf evaluated between \eqn{-\infty} and \eqn{x}. That is,
  \deqn{F(x) = Pr(X \le x) = \int_{-\infty}^x f(t) dt \;\;\;\;\;\; (2)}
  where \eqn{f(t)} denotes the probability density function of \eqn{X} 
  evaluated at \eqn{t}.  For discrete distributions, Equation (2) translates to 
  summing up the probabilities of all values in this interval:
  \deqn{F(x) = Pr(X \le x) = \sum_{t \in (-\infty,x]} f(t) = \sum_{t \in (-\infty,x]} Pr(X = t) \;\;\;\;\;\; (3)}

  A \bold{\emph{cumulative distribution function (cdf) plot}} plots the values of the 
  cdf against quantiles of the specified distribution.  Theoretical cdf plots 
  are sometimes plotted along with \link[=ecdfPlot]{empirical cdf plots} 
  to visually assess whether data have a particular distribution.
}
\value{
  \code{cdfPlot} invisibly returns a list giving coordinates of the points 
  that have been or would have been plotted:
  \item{Quantiles}{The quantiles used for the plot.}
  \item{Cumulative.Probabilities}{The values of the cdf associated with the quantiles.}
}
\references{
  Forbes, C., M. Evans, N. Hastings, and B. Peacock. (2011).  Statistical Distributions. 
  Fourth Edition. John Wiley and Sons, Hoboken, NJ.

  Johnson, N. L., S. Kotz, and A.W. Kemp. (1992).  \emph{Univariate 
  Discrete Distributions, Second Edition}.  John Wiley and Sons, New York.

  Johnson, N. L., S. Kotz, and N. Balakrishnan. (1994). 
  \emph{Continuous Univariate Distributions, Volume 1}. 
  Second Edition. John Wiley and Sons, New York.
 
  Johnson, N. L., S. Kotz, and N. Balakrishnan. (1995). 
  \emph{Continuous Univariate Distributions, Volume 2}. 
  Second Edition. John Wiley and Sons, New York.
}
\author{
    Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\seealso{
  \code{\link{Distribution.df}}, \code{\link{ecdfPlot}}, \code{\link{cdfCompare}}, 
  \code{\link{pdfPlot}}.
}
\examples{
  # Plot the cdf of the standard normal distribution 
  #-------------------------------------------------
  dev.new()
  cdfPlot()

  #==========

  # Plot the cdf of the standard normal distribution
  # and a N(2, 2) distribution on the sample plot. 
  #-------------------------------------------------
  dev.new()
  cdfPlot(param.list = list(mean=2, sd=2), main = "") 

  cdfPlot(add = TRUE, cdf.col = "red") 

  legend("topleft", legend = c("N(2,2)", "N(0,1)"), 
    col = c("black", "red"), lwd = 3 * par("cex")) 

  title("CDF Plots for Two Normal Distributions")
 
  #==========

  # Clean up
  #---------
  graphics.off()
}
\keyword{distribution}
\keyword{hplot}