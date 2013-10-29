\name{ecdfPlot}
\alias{ecdfPlot}
\title{
  Empirical Cumulative Distribution Function Plot
}
\description{
  Produce an empirical cumulative distribution function plot.
}
\usage{
  ecdfPlot(x, discrete = FALSE, 
    prob.method = ifelse(discrete, "emp.probs", "plot.pos"), 
    plot.pos.con = 0.375, plot.it = TRUE, add = FALSE, ecdf.col = "black", 
    ecdf.lwd = 3 * par("cex"), ecdf.lty = 1, curve.fill = FALSE, 
    curve.fill.col = "cyan", ..., type = ifelse(discrete, "s", "l"), 
    main = NULL, xlab = NULL, ylab = NULL, xlim = NULL, ylim = NULL)
}
\arguments{
  \item{x}{
  numeric vector of observations.  Missing (\code{NA}), undefined (\code{NaN}), and 
  infinite (\code{Inf}, \code{-Inf}) values are allowed but will be removed.
}
  \item{discrete}{
  logical scalar indicating whether the assumed parent distribution of \code{x} is discrete 
  (\code{discrete=TRUE}) or continuous (\code{discrete=FALSE}; the default).
}
  \item{prob.method}{
  character string indicating what method to use to compute the plotting positions (empirical probabilities).  
  Possible values are \code{plot.pos} (plotting positions, the default if \code{discrete=FALSE}) and 
  \code{emp.probs} (empirical probabilities, the default if \code{discrete=TRUE}).  
  See the DETAILS section for more explanation.
}
  \item{plot.pos.con}{
  numeric scalar between 0 and 1 containing the value of the plotting position constant.  
  The default value is \code{plot.pos.con=0.375}.  See the DETAILS section for more information.  
  This argument is ignored if \code{prob.method="emp.probs"}.
}
  \item{plot.it}{
  logical scalar indicating whether to produce a plot or add to the current plot (see \code{add}) 
  on the current graphics device.  The default value is \code{plot.it=TRUE}.
}
  \item{add}{
  logical scalar indicating whether to add the empirical cdf to the current plot (\code{add=TRUE}) 
  or generate a new plot (\code{add=FALSE}; the default).  This argument is ignored if 
  \code{plot.it=FALSE}.
}
  \item{ecdf.col}{
  a numeric scalar or character string determining the color of the empirical cdf line or points.  
  The default value is \code{ecdf.col=1}.  See the entry for \code{col} in the help file for 
  \code{\link{par}} for more information.
}
  \item{ecdf.lwd}{
  a numeric scalar determining the width of the empirical cdf line.  The default value is 
  \code{ecdf.lwd=3*par("cex")}.  See the entry for \code{lwd} in the help file for \code{\link{par}} 
  for more information.
}
  \item{ecdf.lty}{
  a numeric scalar determining the line type of the empirical cdf line.  The default value is 
  \code{ecdf.lty=1}.  See the entry for \code{lty} in the help file for \code{\link{par}} 
  for more information.
}
  \item{curve.fill}{
  a logical scalar indicating whether to fill in the area below the empirical cdf curve with the 
  color specified by \code{curve.fill.col}. The default value is \cr
  \code{curve.fill=FALSE}.
}
  \item{curve.fill.col}{
  a numeric scalar or character string indicating what color to use to fill in the area below the 
  empirical cdf curve.  The default value is \code{curve.fill.col=5}.  This argument is ignored 
  if \code{curve.fill=FALSE}.
}
  \item{type, main, xlab, ylab, xlim, ylim, \dots}{
  additional graphical parameters (see \code{\link{lines}} and \code{\link{par}}).  In particular, 
  the argument \code{type} specifies the kind of line type.  By default, the function 
  \code{ecdfPlot} plots a step function (\code{type="s"}) when \code{discrete=TRUE}, and 
  plots a straight line between points (\code{type="l"}) when \code{discrete=FALSE}.  
  The user may override these defaults by supplying the graphics parameter \code{type} 
  (\code{type="s"} for a step function, \code{type="l"} for linear interpolation, 
  \code{type="p"} for points only, etc.).
}
}
\details{
  The \bold{\emph{cumulative distribution function (cdf)}} of a random variable 
  \eqn{X} is the function \eqn{F} such that 
  \deqn{F(x) = Pr(X \le x) \;\;\;\;\;\; (1)} 
  for all values of \eqn{x}.  That is, if \eqn{p = F(x)}, then \eqn{p} is the 
  proportion of the population that is less than or equal to \eqn{x}, and 
  \eqn{x} is called the \eqn{p}'th \bold{\emph{quantile}}, or the 100\eqn{p}'th 
  percentile.  A plot of quantiles 
  on the \eqn{x}-axis (i.e., the possible value for the random variable \eqn{X}) vs. 
  the fraction of the population less than or equal to that number on the 
  \eqn{y}-axis is called the \bold{\emph{cumulative distribution function plot}}, and 
  the \eqn{y}-axis is usually labeled as the 
  \dQuote{cumulative probability} or \dQuote{cumulative frequency}.

  When we have a sample of data from some population, we usually do not
  know what percentiles our observations correspond to because we do not
  know the form of the cumulative distribution function \eqn{F}, so we 
  have to use the sample data to estimate the cdf \eqn{F}.  An 
  \bold{\emph{emprical cumulative distribution function (ecdf) plot}}, 
  also called a \bold{\emph{quantile plot}}, is a plot of the observed 
  quantiles (i.e., the ordered observations) on the \eqn{x}-axis vs. 
  the estimated cumulative probabilities on the \eqn{y}-axis
  (Chambers et al., 1983, pp. 11-19; Cleveland, 1993, pp. 17-20; 
  Cleveland, 1994, pp. 136-139; Helsel and Hirsch, 1992, pp. 21-24).
 
  (Note:  Some authors (e.g., Chambers et al., 1983, pp.11-16; Cleveland, 1993, pp.17-20) 
  reverse the axes on a quantile plot, i.e., the observed order statistics from the 
  random sample are on the \eqn{y}-axis and the estimated cumulative probabilities 
  are on the \eqn{x}-axis.)

  The \bold{\emph{empirical cumulative distribution function (ecdf)}}  
  is an estimate of the cdf based on a random sample of \eqn{n} observations 
  from the distribution.  Let \eqn{x_1, x_2, \ldots, x_n} denote the \eqn{n} 
  observations, and let \eqn{x_{(1)}, x_{(2)}, \ldots, x_{(n)}} denote the ordered 
  observations (i.e., the order statistics).  The cdf is usually estimated by either 
  the \bold{\emph{empirical probabilities estimator}} or the 
  \bold{\emph{plotting-position estimator}}.  The empirical probabilities estimator 
  is given by:
  \deqn{\hat{F}[x_{(i)}] = \hat{p}_i = \frac{\#[x_j \le x_{(i)}]}{n} \;\;\;\;\;\; (2)} 
  where \eqn{\#[x_j \le x_{(i)}]} denotes the number of observations less than 
  or equal to \eqn{x_{(i)}}.  The plotting-position estimator is given by:
  \deqn{\hat{F}[x_{(i)}] = \hat{p}_i = \frac{i - a}{n - 2a + 1} \;\;\;\;\;\; (3)}
  where \eqn{0 \le a \le 1} (Cleveland, 1993, p. 18; D'Agostino, 1986a, pp. 8,25).

  For any value \eqn{x} such that \eqn{x_{(1)} < x < x_{(n)}}, the ecdf is usually defined as either a step function:
  \deqn{\hat{F}(x) = \hat{F}[x_{(i)}], \qquad x_{(i)} \le x < x_{(i+1)} \;\;\;\;\;\; (4)}
  (e.g., D'Agostino, 1986a), or linear interpolation between order statistics is used:
  \deqn{\hat{F}(x) = (1-r)\hat{F}[x_{(i)}] + r\hat{F}[x_{(i+1)}], \qquad x_{(i)} \le x < x_{(i+1)} \;\;\;\;\;\; (5)}
  where 
  \deqn{r = \frac{x - x_{(i)}}{x_{(i+1)} - x_{(i)}} \;\;\;\;\;\; (6)}
  (e.g., Chambers et al., 1983).  For the step function version, the ecdf stays flat until it hits a 
  value on the \eqn{x}-axis corresponding to one of the order statistics, then it makes a jump.  
  For the linear interpolation version, the ecdf plot looks like lines connecting the points.  
  By default, the function \code{ecdfPlot} uses the step function version when \code{discrete=TRUE}, and 
  the linear interpolation version when \code{discrete=FALSE}.  The user may override these defaults by 
  supplying the graphics parameter \code{type} (\code{type="s"} for a step function, \code{type="l"} 
  for linear interpolation, \code{type="p"} for points only, etc.).

  The empirical probabilities estimator is intuitively appealing.  This is the estimator used when 
  \code{prob.method="emp.probs"}.  The disadvantage of this estimator is that it implies the largest 
  observed value is the maximum possible value of the distribution (i.e., the 100'th percentile).  This 
  may be satisfactory if the underlying distribution is known to be discrete, but it is usually not 
  satisfactory if the underlying distribution is known to be continuous.

  The plotting-position estimator with various values of \eqn{a} is often used when the goal is 
  to produce a probability plot (see \code{\link{qqPlot}}) rather than an empirical cdf plot.  It is used 
  to compute the estimated expected values or medians of the order statistics for a probability plot.  
  This is the estimator used when \code{prob.method="plot.pos"}.  The argument \code{plot.pos.con} refers 
  to the variable \eqn{a}.  Based on certain principles from statistical theory, certain 
  values of the constant \eqn{a} make sense for specific underlying distributions (see 
  the help file for \code{\link{qqPlot}} for more information).

  Because \eqn{x} is a random sample, the emprical cdf changes from sample to sample and the variability 
  in these estimates can be dramatic for small sample sizes.
}
\value{
  \code{ecdfPlot} invisibly returns a list with the following components:

  \item{Order.Statistics}{numeric vector of the ordered observations.}
  \item{Cumulative.Probabilities}{numeric vector of the associated plotting positions.}
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
  \code{\link{ppoints}}, \code{\link{cdfPlot}}, \code{\link{cdfCompare}}, 
  \code{\link{qqPlot}}, \code{\link{ecdfPlotCensored}}.
}
\examples{
  # Generate 20 observations from a normal distribution with 
  # mean=0 and sd=1 and create an ecdf plot. 
  # (Note: the call to set.seed simply allows you to reproduce this example.)

  set.seed(250) 
  x <- rnorm(20) 
  dev.new()
  ecdfPlot(x)

  #----------

  # Repeat the above example, but fill in the area under the 
  # empirical cdf curve.

  dev.new()
  ecdfPlot(x, curve.fill = TRUE)

  #----------

  # Repeat the above example, but plot only the points.

  dev.new()
  ecdfPlot(x, type = "p")

  #----------

  # Repeat the above example, but force a step function.

  dev.new()
  ecdfPlot(x, type = "s")

  #----------

  # Clean up
  rm(x)

  #-------------------------------------------------------------------------------------

  # The guidance document USEPA (1994b, pp. 6.22--6.25) 
  # contains measures of 1,2,3,4-Tetrachlorobenzene (TcCB) 
  # concentrations (in parts per billion) from soil samples 
  # at a Reference area and a Cleanup area.  These data are strored 
  # in the data frame EPA.94b.tccb.df.  
  #
  # Create an empirical CDF plot for the reference area data.
  
  dev.new()
  with(EPA.94b.tccb.df, 
    ecdfPlot(TcCB[Area == "Reference"], xlab = "TcCB (ppb)"))

  #==========

  # Clean up
  #---------
  graphics.off()
}
\keyword{distribution}
\keyword{hplot}

