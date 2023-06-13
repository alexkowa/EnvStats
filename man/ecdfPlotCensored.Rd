\name{ecdfPlotCensored}
\alias{ecdfPlotCensored}
\title{
  Empirical Cumulative Distribution Function Plot Based on Type I Censored Data
}
\description{
  Produce an empirical cumulative distribution function plot for Type I left-censored
  or right-censored data.
}
\usage{
  ecdfPlotCensored(x, censored, censoring.side = "left", discrete = FALSE,
    prob.method = "michael-schucany", plot.pos.con = 0.375, plot.it = TRUE,
    add = FALSE, ecdf.col = 1, ecdf.lwd = 3 * par("cex"), ecdf.lty = 1,
    include.cen = FALSE, cen.pch = ifelse(censoring.side == "left", 6, 2),
    cen.cex = par("cex"), cen.col = 4, ...,
    type = ifelse(discrete, "s", "l"), main = NULL, xlab = NULL, ylab = NULL,
    xlim = NULL, ylim = NULL)
}
\arguments{
  \item{x}{
  numeric vector of observations.  Missing (\code{NA}), undefined (\code{NaN}), and
  infinite (\code{Inf}, \code{-Inf}) values are allowed but will be removed.
}
  \item{censored}{
  numeric or logical vector indicating which values of \code{x} are censored.  This must be the
  same length as \code{x}.  If the mode of \code{censored} is \code{"logical"}, \code{TRUE} values
  correspond to elements of \code{x} that are censored, and \code{FALSE} values correspond to
  elements of \code{x} that are not censored.  If the mode of \code{censored} is \code{"numeric"},
  it must contain only \code{1}'s and \code{0}'s; \code{1} corresponds to \code{TRUE} and
  \code{0} corresponds to \code{FALSE}.  Missing (\code{NA}) values are allowed but will be removed.
}
  \item{censoring.side}{
  character string indicating on which side the censoring occurs.  The possible values are
  \code{"left"} (the default) and \code{"right"}.
}
  \item{discrete}{
  logical scalar indicating whether the assumed parent distribution of \code{x} is discrete
  (\code{discrete=TRUE}) or continuous (\code{discrete=FALSE}; the default).
}
  \item{prob.method}{
  character string indicating what method to use to compute the plotting positions (empirical probabilities).
  Possible values are \code{"kaplan-meier"} (product-limit method of Kaplan and Meier (1958)),
  \code{"nelson"} (hazard plotting method of Nelson (1972)),
  \code{"michael-schucany"} (generalization of the product-limit method due to Michael and Schucany (1986)), and
  \code{"hirsch-stedinger"} (generalization of the product-limit method due to Hirsch and Stedinger (1987)).
  The default value is \code{prob.method="michael-schucany"}.

  The \code{"nelson"} method is only available for \code{censoring.side="right"}.
  See the DETAILS section for more explanation.
}
  \item{plot.pos.con}{
  numeric scalar between 0 and 1 containing the value of the plotting position constant.
  The default value is \code{plot.pos.con=0.375}.  See the DETAILS section for more information.
  This argument is used only if \code{prob.method} is equal to \code{"michael-schucany"} or
  \code{"hirsch-stedinger"}.
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
  The default value is \code{ecdf.col=1}. See the entry for \code{col} in the help file for
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
  \item{include.cen}{
  logical scalar indicating whether to include censored values in the plot.  The default value is
  \code{include.cen=FALSE}.  If \code{include.cen=TRUE}, censored values are plotted using the
  plotting character indicated by the argument \code{cen.pch} (see below).
}
  \item{cen.pch}{
  numeric scalar or character string indicating the plotting character to use to plot censored values.
  The default value is \code{cen.pch=2} (hollow triangle pointing up) when \code{censoring.side="right"},
  and \code{cen.pch=6} (hollow triangle pointing down) when \code{censoring.side="left"}.
  See the help file for \code{\link{points}} for a list of other possible plotting characters.
  This argument is ignored if \cr
  \code{include.cen=FALSE}.
}
  \item{cen.cex}{
  numeric scalar that determines the size of the plotting character used to plot censored values.
  The default value is the current value of the cex graphics parameter.  See the entry for \code{cex}
  in the help file for \code{\link{par}} for more information.  This argument is ignored if
  \code{include.cen=FALSE}.
}
  \item{cen.col}{
  numeric scalar or character string that determines the color of the plotting character used to
  plot censored values.  The default value is \code{cen.col=4}.  See the entry for \code{col} in
  the help file for \code{\link{par}} for more information.  This argument is ignored if
  \code{include.cen=FALSE}.
}
  \item{type, main, xlab, ylab, xlim, ylim, \dots}{
  additional graphical parameters (see \code{\link{lines}} and \code{\link{par}}).  In particular,
  the argument \code{type} specifies the kind of line type.  By default, the function
  \code{ecdfPlotCensored} plots a step function (\code{type="s"}) when \code{discrete=TRUE}, and
  plots a straight line between points (\code{type="l"}) when \code{discrete=FALSE}.
  The user may override these defaults by supplying the graphics parameter \code{type}
  (\code{type="s"} for a step function, \code{type="l"} for linear interpolation,
  \code{type="p"} for points only, etc.).
}
}
\details{
  The function \code{ecdfPlotCensored} does exactly the same thing as
  \code{\link{ecdfPlot}}, except it calls the function \code{\link{ppointsCensored}}
  to compute the plotting positions (estimated cumulative probabilities) for the
  uncensored observations.

  If \code{plot.it=TRUE}, the estimated cumulative probabilities for the uncensored
  observations are plotted against the uncensored observations.  By default, the
  function \code{ecdfPlotCensored} plots a step function when \code{discrete=TRUE},
  and plots a straight line between points when \code{discrete=FALSE}.  The user may
  override these defaults by supplying the graphics parameter
  type (\code{type="s"} for a step function, \code{type="l"} for linear interpolation,
  \code{type="p"} for points only, etc.).

  If \code{include.cen=TRUE}, censored observations are included on the plot as points.  The arguments
  \code{cen.pch}, \code{cen.cex}, and \code{cen.col} control the appearance of these points.

  In cases where \code{x} is a random sample, the emprical cdf will change from sample to sample and
  the variability in these estimates can be dramatic for small sample sizes.  Caution must be used in
  interpreting the empirical cdf when a large percentage of the observations are censored.
}
\value{
  \code{ecdfPlotCensored} returns a list with the following components:

  \item{Order.Statistics}{numeric vector of the \dQuote{ordered} observations.}
  \item{Cumulative.Probabilities}{numeric vector of the associated plotting positions.}
  \item{Censored}{logical vector indicating which of the ordered observations are censored.}
  \item{Censoring.Side}{character string indicating whether the data are left- or right-censored.
    This is same value as the argument \code{censoring.side}.}
  \item{Prob.Method}{character string indicating what method was used to compute the plotting positions.
    This is the same value as the argument \code{prob.method}.}

  Optional Component (only present when \code{prob.method="michael-schucany"} or \cr
  \code{prob.method="hirsch-stedinger"}):
  \item{Plot.Pos.Con}{numeric scalar containing the value of the plotting position constant that was used.
    This is the same as the argument \code{plot.pos.con}.}
}
\references{
  Chambers, J.M., W.S. Cleveland, B. Kleiner, and P.A. Tukey. (1983).
  \emph{Graphical Methods for Data Analysis}. Duxbury Press, Boston, MA, pp.11-16.

  Cleveland, W.S. (1993). \emph{Visualizing Data}. Hobart Press, Summit, New Jersey, 360pp.

  D'Agostino, R.B. (1986a). Graphical Analysis.
  In: D'Agostino, R.B., and M.A. Stephens, eds. \emph{Goodness-of Fit Techniques}.
  Marcel Dekker, New York, Chapter 2, pp.7-62.

  Gillespie, B.W., Q. Chen, H. Reichert, A. Franzblau, E. Hedgeman, J. Lepkowski,
  P. Adriaens, A. Demond, W. Luksemburg, and D.H. Garabrant. (2010).  Estimating Population
  Distributions When Some Data Are Below a Limit of Detection by Using a Reverse
  Kaplan-Meier Estimator.  \emph{Epidemiology} \bold{21}(4), S64--S70.

  Helsel, D.R. (2012). \emph{Statistics for Censored Environmental Data Using Minitab and R,
  Second Edition}.  John Wiley & Sons, Hoboken, New Jersey.

  Helsel, D.R., and T.A. Cohn. (1988). Estimation of Descriptive Statistics for Multiply Censored
  Water Quality Data. \emph{Water Resources Research} \bold{24}(12), 1997-2004.

  Hirsch, R.M., and J.R. Stedinger. (1987). Plotting Positions for Historical Floods and Their Precision.
  \emph{Water Resources Research} \bold{23}(4), 715-727.

  Kaplan, E.L., and P. Meier. (1958). Nonparametric Estimation From Incomplete Observations.
  \emph{Journal of the American Statistical Association} \bold{53}, 457-481.

  Lee, E.T., and J.W. Wang. (2003).
  \emph{Statistical Methods for Survival Data Analysis, Third Edition}.
  John Wiley & Sons, Hoboken, New Jersey, 513pp.

  Michael, J.R., and W.R. Schucany. (1986). Analysis of Data from Censored Samples.
  In D'Agostino, R.B., and M.A. Stephens, eds. \emph{Goodness-of Fit Techniques}.
  Marcel Dekker, New York, 560pp, Chapter 11, 461-496.

  Nelson, W. (1972). Theory and Applications of Hazard Plotting for Censored Failure Data.
  \emph{Technometrics} \bold{14}, 945-966.

  USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
  EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.
  U.S. Environmental Protection Agency, Washington, D.C. Chapter 15.

  USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
  EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
  U.S. Environmental Protection Agency, Washington, D.C.
}
\author{
    Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  An empirical cumulative distribution function (ecdf) plot is a graphical tool that can be used in
  conjunction with other graphical tools such as histograms, strip charts, and boxplots to assess the
  characteristics of a set of data.

  Censored observations complicate the procedures used to graphically explore data.  Techniques from
  survival analysis and life testing have been developed to generalize the procedures for constructing
  plotting positions, empirical cdf plots, and q-q plots to data sets with censored observations
  (see \code{\link{ppointsCensored}}).

  Empirical cumulative distribution function (ecdf) plots are often plotted with theoretical cdf plots
  (see \code{\link{cdfPlot}} and \code{\link{cdfCompareCensored}}) to graphically assess whether a
  sample of observations comes from a particular distribution.  More often, however, quantile-quantile
  (Q-Q) plots are used instead (see \code{\link{qqPlot}} and \code{\link{qqPlotCensored}}).
}
\seealso{
  \code{\link{ppoints}}, \code{\link{ppointsCensored}}, \code{\link{ecdfPlot}},
  \code{\link{qqPlot}},  \code{\link{qqPlotCensored}},  \code{\link{cdfPlot}},
  \code{\link{cdfCompareCensored}}.
}
\examples{
  # Generate 20 observations from a normal distribution with mean=20 and sd=5,
  # censor all observations less than 18, then generate an empirical cdf plot
  # for the complete data set and the censored data set.  Note that the empirical
  # cdf plot for the censored data set starts at the first ordered uncensored
  # observation, and that for values of x > 18 the two emprical cdf plots are
  # exactly the same.  This is because there is only one censoring level and
  # no uncensored observations fall below the censored observations.
  # (Note: the call to set.seed simply allows you to reproduce this example.)

  set.seed(333)
  x <- rnorm(20, mean=20, sd=5)
  censored <- x < 18

  sum(censored)
  #[1] 7

  new.x <- x
  new.x[censored] <- 18

  dev.new()
  ecdfPlot(x, xlim = range(pretty(x)),
    main = "Empirical CDF Plot for\nComplete Data Set")

  dev.new()
  ecdfPlotCensored(new.x, censored, xlim = range(pretty(x)),
    main="Empirical CDF Plot for\nCensored Data Set")

  # Clean up
  #---------
  rm(x, censored, new.x)

  #------------------------------------------------------------------------------------

  # Example 15-1 of USEPA (2009, page 15-10) gives an example of
  # computing plotting positions based on censored manganese
  # concentrations (ppb) in groundwater collected at 5 monitoring
  # wells.  The data for this example are stored in
  # EPA.09.Ex.15.1.manganese.df.  Here we will create an empirical
  # CDF plot based on the Kaplan-Meier method.

  EPA.09.Ex.15.1.manganese.df
  #   Sample   Well Manganese.Orig.ppb Manganese.ppb Censored
  #1       1 Well.1                 <5           5.0     TRUE
  #2       2 Well.1               12.1          12.1    FALSE
  #3       3 Well.1               16.9          16.9    FALSE
  #4       4 Well.1               21.6          21.6    FALSE
  #5       5 Well.1                 <2           2.0     TRUE
  #...
  #21      1 Well.5               17.9          17.9    FALSE
  #22      2 Well.5               22.7          22.7    FALSE
  #23      3 Well.5                3.3           3.3    FALSE
  #24      4 Well.5                8.4           8.4    FALSE
  #25      5 Well.5                 <2           2.0     TRUE

  dev.new()
  with(EPA.09.Ex.15.1.manganese.df,
    ecdfPlotCensored(Manganese.ppb, Censored,
      prob.method = "kaplan-meier", ecdf.col = "blue",
      main = "Empirical CDF of Manganese Data\nBased on Kaplan-Meier"))

  #==========

  # Clean up
  #---------
  graphics.off()
}
\keyword{distribution}
\keyword{hplot}
