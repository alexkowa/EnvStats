\name{cdfCompare}
\alias{cdfCompare}
\alias{Compare CDFs}
\title{
  Plot Two Cumulative Distribution Functions
}
\description{
  For one sample, plots the empirical cumulative distribution function (ecdf) 
  along with a theoretical cumulative distribution function (cdf).  
  For two samples, plots the two ecdf's.  These plots are used to graphically assess 
  goodness of fit.
}
\usage{
  cdfCompare(x, y = NULL, discrete = FALSE, 
    prob.method = ifelse(discrete, "emp.probs", "plot.pos"), plot.pos.con = NULL, 
    distribution = "norm", param.list = NULL, 
    estimate.params = is.null(param.list), est.arg.list = NULL, 
    x.col = "blue", y.or.fitted.col = "black", 
    x.lwd = 3 * par("cex"), y.or.fitted.lwd = 3 * par("cex"), 
    x.lty = 1, y.or.fitted.lty = 2, digits = .Options$digits, ..., 
    type = ifelse(discrete, "s", "l"), main = NULL, xlab = NULL, ylab = NULL, 
    xlim = NULL, ylim = NULL)
}
\arguments{
  \item{x}{
  numeric vector of observations.  Missing (\code{NA}), undefined (\code{NaN}), and 
  infinite (\code{Inf}, \code{-Inf}) values are allowed but will be removed.
}
  \item{y}{
  a numeric vector (not necessarily of the same length as \code{x}). 
  Missing (\code{NA}), undefined (\code{NaN}), and infinite 
  (\code{Inf}, \code{-Inf}) values are allowed but will be removed.  
  The default value is \code{y=NULL}, in which case the empirical cdf of 
  \code{x} will be plotted along with the theoretical cdf specified by the 
  argument \code{distribution}.
}
  \item{discrete}{
  logical scalar indicating whether the assumed parent distribution of \code{x} 
  is discrete (\code{discrete=TRUE}) or continuous (\code{discrete=FALSE}; the default).
}
  \item{prob.method}{
  character string indicating what method to use to compute the plotting positions 
  (empirical probabilities).  Possible values are 
  \code{plot.pos} (plotting positions, the default if \code{discrete=FALSE}) and 
  \code{emp.probs} (empirical probabilities, the default if \code{discrete=TRUE}).  
  See the help file for \code{\link{ecdfPlot}} for more explanation.
}
  \item{plot.pos.con}{
  numeric scalar between 0 and 1 containing the value of the plotting position constant.  
  When \code{y} is supplied, the default value is \code{plot.pos.con=0.375}.  
  When \code{y} is not supplied, for the normal, lognormal, three-parameter lognormal, 
  zero-modified normal, and zero-modified lognormal distributions, the default value 
  is \code{plot.pos.con=0.375}.  
  For the Type I extreme value (Gumbel) distribution (\code{distribution="evd"}), 
  the default value is \code{plot.pos.con=0.44}.  For all other distributions, the 
  default value is \code{plot.pos.con=0.4}.  
  See the help files for \code{\link{ecdfPlot}} and \code{\link{qqPlot}} for more 
  information.  This argument is ignored if \code{prob.method="emp.probs"}.
}
  \item{distribution}{
  when \code{y} is not supplied, 
  a character string denoting the distribution abbreviation.  The default value is 
  \code{distribution="norm"}.  See the help file for \cr
  \code{\link{Distribution.df}} for a 
  list of possible distribution abbreviations.  This argument is ignored if \code{y} 
  is supplied.
}
  \item{param.list}{
  when \code{y} is not supplied, 
  a list with values for the parameters of the distribution.  The default value is 
  \code{param.list=list(mean=0, sd=1)}.  See the help file for \code{\link{Distribution.df}} 
  for the names and possible values of the parameters associated with each distribution.  
  This argument is ignored if \code{y} is supplied or \code{estimate.params=TRUE}.
}
  \item{estimate.params}{
  when \code{y} is not supplied, 
  a logical scalar indicating whether to compute the cdf for \code{x} 
  based on estimating the distribution parameters (\code{estimate.params=TRUE}) or 
  using the known distribution parameters specified in \code{param.list} \cr
  (\code{estimate.params=FALSE}).  The default value is \code{TRUE} unless the argument \cr
  \code{param.list} is supplied.  The argument \code{estimate.params} 
  is ignored if \code{y} is supplied.
}
  \item{est.arg.list}{
  when \code{y} is not supplied and \code{estimate.params=TRUE}, 
  a list whose components are optional arguments associated with the function used to 
  estimate the parameters of the assumed distribution (see the help file 
  \link[=FcnsByCatEstDistParams]{Estimating Distribution Parameters}).  
  For example, all functions used to estimate distribution parameters have an 
  optional argument called \code{method} that specifies the method to use to estimate the parameters.  
  (See the help file for \code{\link{Distribution.df}} for a list of available estimation 
  methods for each distribution.)  To override the default estimation method, supply the argument 
  \code{est.arg.list} with a component called \code{method}; for example 
  \code{est.arg.list=list(method="mle")}.  The default value is 
  \code{est.arg.list=NULL} so that all default values for the estimating function are used.  
  This argument is ignored if \code{estimate.params=FALSE} or \code{y} is supplied.
}
  \item{x.col}{
  a numeric scalar or character string determining the color of the empirical cdf 
  (based on \code{x}) line or points.  The default value is \code{x.col="blue"}.  
  See the entry for \code{col} in the help file for \code{\link{par}} for more 
  information.
}
  \item{y.or.fitted.col}{
  a numeric scalar or character string determining the color of the empirical cdf 
  (based on \code{y}) or the theoretical cdf line or points.  
  The default value is \cr
  \code{y.or.fitted.col="black"}.  See the entry for 
  \code{col} in the help file for \code{\link{par}} for more information.
}
  \item{x.lwd}{
  a numeric scalar determining the width of the empirical cdf (based on \code{x}) line.  
  The default value is \code{x.lwd=3*par("cex")}.  
  See the entry for \code{lwd} in the help file for \code{\link{par}} 
  for more information.
}
  \item{y.or.fitted.lwd}{
  a numeric scalar determining the width of the empirical cdf (based on \code{y}) 
  or theoretical cdf line.  
  The default value is \code{y.or.fitted.lwd=3*par("cex")}.  
  See the entry for \code{lwd} in the help file for \code{\link{par}} 
  for more information.
}
  \item{x.lty}{
  a numeric scalar determining the line type of the empirical cdf 
  (based on \code{x}) line.  The default value is 
  \code{x.lty=1}.  See the entry for \code{lty} in the help file for \code{\link{par}} 
  for more information.
}
  \item{y.or.fitted.lty}{
  a numeric scalar determining the line type of the empirical cdf 
  (based on \code{y}) or theoretical cdf line.  The default value is 
  \code{y.or.fitted.lty=2}.  
  See the entry for \code{lty} in the help file for \code{\link{par}} 
  for more information.
}
  \item{digits}{
  when \code{y} is not supplied, 
  a scalar indicating how many significant digits to print for the distribution 
  parameters.  The default value is \code{digits=.Options$digits}.
}
  \item{type, main, xlab, ylab, xlim, ylim, \dots}{
  additional graphical parameters (see \code{\link{lines}} and \code{\link{par}}).  
  In particular, the argument \code{type} specifies the kind of line type.  
  By default, the function \code{cdfCompare} plots a step function (\code{type="s"}) 
  when \code{discrete=TRUE}, and plots a straight line between points 
  (\code{type="l"}) when \code{discrete=FALSE}.  
  The user may override these defaults by supplying the graphics parameter \code{type} 
  (\code{type="s"} for a step function, \code{type="l"} for linear interpolation, 
  \code{type="p"} for points only, etc.).
}
}
\details{
  When both \code{x} and \code{y} are supplied, the function \code{cdfCompare} 
  creates the empirical cdf plot of \code{x} and \code{y} on 
  the same plot by calling the function \code{\link{ecdfPlot}}.

  When \code{y} is not supplied, the function \code{cdfCompare} creates the 
  emprical cdf plot of \code{x} (by calling \code{\link{ecdfPlot}}) and the 
  theoretical cdf plot (by calling \code{\link{cdfPlot}} and using the 
  argument \code{distribution}) on the same plot.
}
\value{
  When \code{y} is supplied, \code{cdfCompare} invisibly returns a list with 
  components:
  \item{x.ecdf.list}{a list with components \code{Order.Statistics} and 
  \code{Cumulative.Probabilities}, giving coordinates of the points that have 
  been plotted for the \code{x} values.}
  \item{y.ecdf.list}{a list with components \code{Order.Statistics} and 
  \code{Cumulative.Probabilities}, giving coordinates of the points that have 
  been plotted for the \code{y} values.}

  When \code{y} is not supplied, \code{cdfCompare} invisibly returns a list with 
  components:
  \item{x.ecdf.list}{a list with components \code{Order.Statistics} and 
  \code{Cumulative.Probabilities}, giving coordinates of the points that have 
  been plotted for the \code{x} values.}
  \item{fitted.cdf.list}{a list with components \code{Quantiles} and 
  \code{Cumulative.Probabilities}, giving coordinates of the 
  points that have been plotted for the fitted cdf.}
}
\references{
  Chambers, J.M., W.S. Cleveland, B. Kleiner, and P.A. Tukey. (1983). 
  \emph{Graphical Methods for Data Analysis}. Duxbury Press, Boston, MA, pp.11-16.

  Cleveland, W.S. (1993). \emph{Visualizing Data}. Hobart Press, Summit, New Jersey, 360pp.

  D'Agostino, R.B. (1986a). Graphical Analysis. 
  In: D'Agostino, R.B., and M.A. Stephens, eds. \emph{Goodness-of Fit Techniques}. 
  Marcel Dekker, New York, Chapter 2, pp.7-62.
}
\author{
    Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  An empirical cumulative distribution function (ecdf) plot is a graphical tool that 
  can be used in conjunction with other graphical tools such as histograms, 
  strip charts, and boxplots to assess the characteristics of a set of data.  
  It is easy to determine quartiles and the minimum and maximum 
  values from such a plot.  Also, ecdf plots allow you to assess local density:  
  a higher density of observations occurs where the slope is steep.

  Chambers et al. (1983, pp.11-16) plot the observed order statistics on the 
  \eqn{y}-axis vs. the ecdf on the \eqn{x}-axis and call this a quantile plot.

  Empirical cumulative distribution function (ecdf) plots are often plotted with 
  theoretical cdf plots (see \code{\link{cdfPlot}} and \code{\link{cdfCompare}}) to 
  graphically assess whether a sample of observations comes from a particular 
  distribution.  The Kolmogorov-Smirnov goodness-of-fit test 
  (see \code{\link{gofTest}}) is the statistical companion of this kind of 
  comparison; it is based on the maximum vertical distance between the empirical 
  cdf plot and the theoretical cdf plot.  More often, however, 
  quantile-quantile (Q-Q) plots are used instead of ecdf plots to graphically assess 
  departures from an assumed distribution (see \code{\link{qqPlot}}).
}
\seealso{
  \code{\link{cdfPlot}}, \code{\link{ecdfPlot}}, \code{\link{qqPlot}}.
}
\examples{
  # Generate 20 observations from a normal (Gaussian) distribution 
  # with mean=10 and sd=2 and compare the empirical cdf with a 
  # theoretical normal cdf that is based on estimating the parameters. 
  # (Note: the call to set.seed simply allows you to reproduce this example.)

  set.seed(250) 
  x <- rnorm(20, mean = 10, sd = 2) 
  dev.new()
  cdfCompare(x)

  #----------

  # Generate 30 observations from an exponential distribution with parameter 
  # rate=0.1 (see the R help file for Exponential) and compare the empirical 
  # cdf with the empirical cdf of the normal observations generated in the 
  # previous example:

  set.seed(432)
  y <- rexp(30, rate = 0.1) 
  dev.new()
  cdfCompare(x, y)

  #==========

  # Generate 20 observations from a Poisson distribution with parameter lambda=10 
  # (see the R help file for Poisson) and compare the empirical cdf with a 
  # theoretical Poisson cdf based on estimating the distribution parameters. 
  # (Note: the call to set.seed simply allows you to reproduce this example.)

  set.seed(250) 
  x <- rpois(20, lambda = 10) 
  dev.new()
  cdfCompare(x, dist = "pois")

  #==========

  # Clean up
  #---------
  rm(x, y)
  graphics.off()
}
\keyword{distribution}
\keyword{hplot}
