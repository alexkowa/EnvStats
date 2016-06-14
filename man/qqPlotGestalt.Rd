\name{qqPlotGestalt}
\alias{qqPlotGestalt}
\title{
  Develop Gestalt of Q-Q Plots for Specific Distributions
}
\description{
  Produce a series of quantile-quantile (Q-Q) plots (also called probability plots) 
  or Tukey mean-difference Q-Q plots for a user-specified distribution.
}
\usage{
  qqPlotGestalt(distribution = "norm", param.list = list(mean = 0, sd = 1), 
    estimate.params = FALSE, est.arg.list = NULL, sample.size = 10, num.pages = 2, 
    num.plots.per.page = 4, nrow = ceiling(num.plots.per.page/2), plot.type = "Q-Q", 
    plot.pos.con = switch(dist.abb, norm = , lnorm = , lnormAlt = , lnorm3 = 0.375, 
      evd = 0.44, 0.4), equal.axes = (qq.line.type == "0-1" || estimate.params), 
    margin.title = NULL, add.line = FALSE, qq.line.type = "least squares", 
    duplicate.points.method = "standard", points.col = 1, line.col = 1, 
    line.lwd = par("cex"), line.lty = 1, digits = .Options$digits, 
    same.window = TRUE, ask = same.window & num.pages > 1, 
    mfrow = c(nrow, num.plots.per.page/nrow), 
    mar = c(4, 4, 1, 1) + 0.1, oma = c(0, 0, 7, 0), mgp = c(2, 0.5, 0), ..., 
    main = NULL, xlab = NULL, ylab = NULL, xlim = NULL, ylim = NULL)
}
\arguments{
  \item{distribution}{
  a character string denoting the distribution abbreviation.  The default value is 
  \code{distribution="norm"}.  See the help file for \code{\link{Distribution.df}} for a 
  list of possible distribution abbreviations.  This argument is ignored if \code{y} 
  is supplied.
}
  \item{param.list}{
  a list with values for the parameters of the distribution.  The default value is 
  \code{param.list=list(mean=0, sd=1)}.  See the help file for \code{\link{Distribution.df}} 
  for the names and possible values of the parameters associated with each distribution.  
  This argument is ignored if \code{estimate.params=TRUE}.
}
  \item{estimate.params}{
  a logical scalar indicating whether to compute quantiles based on estimating the 
  distribution parameters (\code{estimate.params=TRUE}) or using the known 
  distribution parameters specified in \code{param.list} (\code{estimate.params=FALSE}, 
  the default).  The default value of \code{estimate.params} 
  is \code{FALSE} because the default configuration is to generate random numbers 
  from a standard normal (mean=0, sd=1) distribution and produce a standard 
  normal Q-Q plot.
}
  \item{est.arg.list}{
  a list whose components are optional arguments associated with the function used 
  to estimate the parameters of the assumed distribution (see the help file 
  \link[=FcnsByCatEstDistParams]{Estimating Distribution Parameters}).  
  For example, all functions used to estimate distribution parameters have an optional argument 
  called \code{method} that specifies the method to use to estimate the parameters.  
  (See the help file for \code{\link{Distribution.df}} for a list of available estimation 
  methods for each distribution.)  To override the default estimation method, supply the argument \cr
  \code{est.arg.list} with a component called \code{method}; for example \cr
  \code{est.arg.list=list(method="mle")}.  The default value is 
  \code{est.arg.list=NULL} so that all default values for the estimating function are used.  
  This argument is ignored if \code{estimate.params=FALSE}.
}
  \item{sample.size}{
  numeric scalar indicating the number of observations to generate for each Q-Q plot.  
  The default value is \code{sample.size=10}.
}
  \item{num.pages}{
  numeric scalar indicating the number of pages of plots to generate.  
  The default value is \code{num.pages=2}.
}
  \item{num.plots.per.page}{
  numeric scalar indicating the number of plots per page.  
  The default value is \code{num.pages=4}.
}
  \item{nrow}{
  numeric scalar indicating the number of rows of plots on each page.  
  The default value is the smallest integer greater than or equal to 
  \code{num.plots.per.page/2}.
}
  \item{plot.type}{
  a character string denoting the kind of plot.  Possible values are \code{"Q-Q"} 
  (Quantile-Quantile plot, the default) and \code{"Tukey Mean-Difference Q-Q"} 
  (Tukey mean-difference Q-Q plot).  This argument may be abbreviated (e.g., 
  \code{plot.type="T"} to indicate a Tukey mean-difference Q-Q plot).
}
  \item{plot.pos.con}{
  numeric scalar between 0 and 1 containing the value of the plotting position constant.  
  The default value of \code{plot.pos.con} depends on the value of the argument 
  \code{distribution}.  For the normal, lognormal, three-parameter lognormal, 
  zero-modified normal, and zero-modified lognormal distributions, the default 
  value is \code{plot.pos.con=0.375}.  For the Type I extreme value (Gumbel) 
  distribution (\code{distribution="evd"}), the default value is 
  \code{plot.pos.con=0.44}.  For all other distributions, the default value is 
  \code{plot.pos.con=0.4}.  See the help file for \code{\link{qqPlot}} for the 
  motivation behind these values for plotting positions.
}
  \item{equal.axes}{
  logical scalar indicating whether to use the same range on the \eqn{x}- and 
  \eqn{y}-axes when \code{plot.type="Q-Q"}.  The default value is \code{TRUE} if 
  \code{qq.line.type="0-1"} or \code{estimate.params=TRUE}, otherwise it is 
  \code{FALSE}.  This argument is ignored if 
  \code{plot.type="Tukey Mean-Difference Q-Q"}.
}
  \item{margin.title}{
  character string indicating the title printed in the top margin on each page of 
  plots.  The default value indicates the kind of Q-Q plot, the probability 
  distribution, the sample size, and the estimation method used (if any).
}
  \item{add.line}{
  logical scalar indicating whether to add a line to the plot.  
  If \code{add.line=TRUE} and \code{plot.type="Q-Q"}, a line determined by the 
  value of \code{qq.line.type} is added to the plot.  
  If \code{add.line=TRUE} and \cr
  \code{plot.type="Tukey Mean-Difference Q-Q"}, a horizontal line at 
  \eqn{y=0} is added to the plot.  The default value is \code{add.line=FALSE}.
}
  \item{qq.line.type}{
  character string determining what kind of line to add to the Q-Q plot.  
  Possible values are \code{"least squares"} (the default), \code{"0-1"} and 
  \code{"robust"}.  For the value 
  \code{"least squares"}, a least squares line is fit and added.  For the value \code{"0-1"}, 
  a line with intercept 0 and slope 1 is added.  For the value \code{"robust"}, a line is fit through 
  the first and third quartiles of the \code{x} and \code{y} data.  
  This argument is ignored if 
  \code{add.line=FALSE} or \code{plot.type="Tukey Mean-Difference Q-Q"}.
}
  \item{duplicate.points.method}{
  character string denoting how to plot points with duplicate \eqn{(x,y)} values.  
  Possible values are \code{"standard"} (the default), \code{"jitter"}, and \code{"number"}.  For the value 
  \code{"standard"}, a single plotting symbol is plotted (this is the default behavior of \R).  
  For the value \code{"jitter"}, a separate plotting symbol is plotted for each duplicate point, where 
  the plotting symbols cluster around the true value of \eqn{x} and \eqn{y}.  For the value 
  \code{"number"}, a single number is plotted at \eqn{(x,y)} that represents how many duplicate points 
  are at that \eqn{(x,y)} coordinate.
}
  \item{points.col}{
  numeric scalar or character string determining the color of the points in the plot.  
  The default value is \code{points.col=1}.  See the entry for \code{col} in the 
  help file for \code{\link{par}} for more information.
}
  \item{line.col}{
  numeric scalar or character string determining the color of the line in the plot.  
  The default value is \code{points.col=1}.  See the entry for \code{col} in the 
  help file for \code{\link{par}} for more information.  This argument is ignored 
  if \code{add.line=FALSE}.
}
  \item{line.lwd}{
  numeric scalar determining the width of the line in the plot.  The default value is 
  \code{line.lwd=par("cex")}.  See the entry for \code{lwd} in the help file for \code{\link{par}} 
  for more information.  This argument is ignored if \code{add.line=FALSE}.
}
  \item{line.lty}{
  a numeric scalar determining the line type of the line in the plot.  The default value is 
  \code{line.lty=1}.  See the entry for \code{lty} in the help file for \code{\link{par}} 
  for more information.  This argument is ignored if \code{add.line=FALSE}.
}
  \item{digits}{
  a scalar indicating how many significant digits to print for the distribution 
  parameters.  The default value is \code{digits=.Options$digits}.
}
  \item{same.window}{
  logical scalar indicating whether to produce all plots in the same graphics 
  window (\code{same.window=TRUE}; the default), or to create a new graphics 
  window for each separate plot (\code{same.window=FALSE}). 
}
  \item{ask}{
  logical scalar supplied to the function \code{\link{devAskNewPage}}, indicating 
  whether to prompt the user before creating a new plot within a single graphics 
  window.  The default value is \code{TRUE} if \code{same.window=TRUE} and 
  \code{num.pages > 1}, otherwise it is \code{FALSE}.
}
  \item{mfrow, mar, oma, mgp, main, xlab, ylab, xlim, ylim, \dots}{
  additional graphical parameters (see \code{\link{par}}).
}
}
\details{
  The function \code{qqPlotGestalt} allows the user to display several Q-Q plots or 
  Tukey mean-difference Q-Q plots for a specified probability distribution.  
  The distribution is specified with the arguments \code{distribution} and 
  \code{param.list}.  By default, \link[stats:Normal]{normal (Gaussian)} 
  Q-Q plots are produced.

  If \code{estimate.params=FALSE} (the default), the theoretical quantiles on the 
  \eqn{x}-axis are computed using the known distribution parameters specified in 
  \code{param.list}.  If \code{estimate.params=TRUE}, the distribution parameters 
  are estimated based on the sample, and these estimated parameters are then used 
  to compute the theoretical quantiles.  For distributions that can be specified 
  by a location and scale parameter (e.g., Normal, Logistic, extreme value, etc.), 
  the value of \code{estimate.params} will not affect the general shape of the 
  plot, only the values recorded on the \eqn{x}-axis.  For distributions that cannot 
  be specified by a location and scale parameter (e.g., exponential, gamma, etc.), it 
  is recommended that \code{estimate.params} be set to \code{TRUE} since in pracitice 
  the values of the distribution parameters are not known but must be estimated from 
  the sample.

  The purpose of \code{qqPlotGestalt} is to allow the user to build-up a visual 
  memory of \dQuote{typical} Q-Q plots.  A Q-Q plot is a graphical tool that allows 
  you to assess how well a particular set of observations fit a particular 
  probability distribution.  The value of this tool depends on the user having an 
  internal reference set of Q-Q plots with which to compare the current Q-Q plot.

  See the help file for \code{\link{qqPlot}} for more information.
}
\value{
  The \code{NULL} value is returned.
}
\references{
  See the REFERENCES section for \code{\link{qqPlot}}.
}
\author{
    Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\seealso{
  \code{\link{qqPlot}}.
}
\examples{
  # Look at eight typical normal (Gaussian) Q-Q plots for random samples 
  # of size 10 from a N(0,1) distribution 
  # Are you surprised by the variability in the plots?
  #
  # (Note:  you must use set.seed if you want to reproduce the exact 
  #         same plots more than once.)

  set.seed(298)
  qqPlotGestalt(same.window = FALSE)

  # Add lines to these same Q-Q plots
  #----------------------------------
  set.seed(298)
  qqPlotGestalt(same.window = FALSE, add.line = TRUE)

  # Add lines to different Q-Q plots
  #---------------------------------
  qqPlotGestalt(same.window = FALSE, add.line = TRUE)

  \dontrun{
  # Look at 4 sets of plots all in the same graphics window
  #--------------------------------------------------------
  qqPlotGestalt(add.line = TRUE, num.pages = 4)
  }

  #==========

  # Look at Q-Q plots for a gamma distribution
  #-------------------------------------------

  qqPlotGestalt(dist = "gammaAlt", 
    param.list = list(mean = 10, cv = 1), 
    estimate.params = TRUE, num.pages = 3, 
    same.window = FALSE, add.line = TRUE)


  # Look at Tukey Mean Difference Q-Q plots 
  # for a gamma distribution
  #----------------------------------------

  qqPlotGestalt(dist = "gammaAlt", 
    param.list = list(mean = 10, cv = 1), 
    estimate.params = TRUE, num.pages = 3, 
    plot.type = "Tukey", same.window = FALSE, add.line = TRUE)

  #==========

  # Clean up
  #---------
  graphics.off()
}
\keyword{distribution}
\keyword{hplot}
