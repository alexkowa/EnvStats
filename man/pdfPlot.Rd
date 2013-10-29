\name{pdfPlot}
\alias{pdfPlot}
\alias{Probability Density}
\alias{Plot Probability Density}
\alias{Plot PDF}
\title{
  Plot Probability Density Function
}
\description{
  Produce a probability density function (pdf) plot for a user-specified distribution.
}
\usage{
  pdfPlot(distribution = "norm", param.list = list(mean = 0, sd = 1), 
    left.tail.cutoff = ifelse(is.finite(supp.min), 0, 0.001), 
    right.tail.cutoff = ifelse(is.finite(supp.max), 0, 0.001), 
    plot.it = TRUE, add = FALSE, n.points = 1000, pdf.col = "black", 
    pdf.lwd = 3 * par("cex"), pdf.lty = 1, curve.fill = !add, 
    curve.fill.col = "cyan", x.ticks.at.all.x.max = 15, 
    hist.col = ifelse(add, "black", "cyan"), density = 5, 
    digits = .Options$digits, ..., type = "l", main = NULL, xlab = NULL, 
    ylab = NULL, xlim = NULL, ylim = NULL)
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
  a logical scalar indicating whether to add the probability density curve to the 
  existing plot (\code{add=TRUE}), or to create a new plot 
  (\code{add=FALSE}; the default).  This argument is ignored if \code{plot.it=FALSE}.
}
  \item{n.points}{
  a numeric scalar specifying at how many evenly-spaced points the probability 
  density function will be evaluated.  The default value is \code{n.points=1000}.
}
  \item{pdf.col}{
  for continuous distributions, a numeric scalar or character string determining 
  the color of the pdf line in the plot.  
  The default value is \code{pdf.col="black"}.  See the entry for \code{col} in the 
  help file for \code{\link{par}} for more information.
}
  \item{pdf.lwd}{
  for continuous distributions, a numeric scalar determining the width of the pdf 
  line in the plot.  
  The default value is \code{pdf.lwd=3*par("cex")}.  
  See the entry for \code{lwd} in the help file for \code{\link{par}} 
  for more information.
}
  \item{pdf.lty}{
  for continuous distributions, a numeric scalar determining the line type of 
  the pdf line in the plot.  
  The default value is \code{pdf.lty=1}.  See the entry for 
  \code{lty} in the help file for \code{\link{par}} for more information.
}
  \item{curve.fill}{
  for continuous distributions, a logical value indicating whether to fill in 
  the area below the probability density curve with the color specified by 
  \code{curve.fill.col}.  
  The default value is \code{TRUE} unless \code{add=TRUE}.
}
  \item{curve.fill.col}{
  for continuous distributions, when \code{curve.fill=TRUE}, 
  a numeric scalar or character string 
  indicating what color to use to fill in the 
  area below the probability density curve.  The default value is 
  \code{curve.fill.col="cyan"}.  See the entry for \code{col} in the 
  help file for \code{\link{par}} for more information.
}
  \item{x.ticks.at.all.x.max}{
  a numeric scalar indicating the maximum number of ticks marks on the \eqn{x}-axis.  
  The default value is \code{x.ticks.at.all.x.max=15}.
}
  \item{hist.col}{
  for discrete distributions, a numeric scalar or character string indicating 
  what color to use to fill in the histogram if \code{add=FALSE}, or the color 
  of the shading lines if \code{add=TRUE}.  The default is \code{"cyan"} if 
  \code{add=FALSE} and \code{"black"} if \code{add=TRUE}.  
  See the entry for \code{col} in the 
  help file for \code{\link{par}} for more information.
}
  \item{density}{
  for discrete distributions, a scalar indicting the density of line shading for 
  the histogram when \code{add=TRUE}.  This argument is ignored if \code{add=FALSE}.
}
  \item{digits}{
  a scalar indicating how many significant digits to print for the distribution 
  parameters.  The default value is \code{digits=.Options$digits}.
}
  \item{type, main, xlab, ylab, xlim, ylim, \dots}{
  additional graphical parameters.  See \code{\link{plot.default}} and 
  \code{\link{par}}).
}
}
\details{
  The \bold{\emph{probability density function (pdf)}} of a random variable \eqn{X}, 
  usually denoted \eqn{f}, is defined as:
  \deqn{f(x) = \frac{dF(x)}{dx} \;\;\;\;\;\; (1)}
  where \eqn{F} is the cumulative distribution function (cdf) of \eqn{X}.  
  That is, \eqn{f(x)} is the derivative of the cdf 
  \eqn{F} with respect to \eqn{x} (where this derivative exists).

  For discrete distributions, the probability density function is simply:
  \deqn{f(x) = Pr(X = x) \;\;\;\;\;\; (2)}
  In this case, \eqn{f} is sometimes called the \bold{\emph{probability function}} or 
  \bold{\emph{probability mass function}}.

  The probability that the random variable \eqn{X} takes on a value in the interval 
  \eqn{[a, b]} is simply the (Lebesgue) integral of the pdf evaluated between 
  \eqn{a} and \eqn{b}. That is,
  \deqn{Pr(a \le X \le b) = \int_a^b f(x) dx \;\;\;\;\;\; (3)}
  For discrete distributions, Equation (3) translates to summing up the 
  probabilities of all values in this interval:
  \deqn{Pr(a \le X \le b) = \sum_{x \in [a,b]} f(x) = \sum_{x \in [a,b]} Pr(X = x) \;\;\;\;\;\; (4)}

  A \bold{\emph{probability density function (pdf) plot}} plots the values of the 
  pdf against quantiles of the specified distribution.  Theoretical pdf plots 
  are sometimes plotted along with \link[=epdfPlot]{empirical pdf plots} 
  (density plots), histograms or bar graphs to visually assess whether data 
  have a particular distribution.
}
\value{
  \code{pdfPlot} invisibly returns a list giving coordinates of the points 
  that have been or would have been plotted:
  \item{Quantiles}{The quantiles used for the plot.}
  \item{Probability.Densities}{The values of the pdf associated with the quantiles.}
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
  \code{\link{Distribution.df}}, \code{\link{epdfPlot}}, \code{\link{cdfPlot}}.
}
\examples{
  # Plot the pdf of the standard normal distribution 
  #-------------------------------------------------
  dev.new()
  pdfPlot()

  #==========

  # Plot the pdf of the standard normal distribution
  # and a N(2, 2) distribution on the sample plot. 
  #-------------------------------------------------
  dev.new()
  pdfPlot(param.list = list(mean=2, sd=2), 
    curve.fill = FALSE, ylim = c(0, dnorm(0)), main = "") 

  pdfPlot(add = TRUE, pdf.col = "red") 

  legend("topright", legend = c("N(2,2)", "N(0,1)"), 
    col = c("black", "red"), lwd = 3 * par("cex")) 

  title("PDF Plots for Two Normal Distributions")
 
  #==========

  # Clean up
  #---------
  graphics.off()
}
\keyword{distribution}
\keyword{hplot}
