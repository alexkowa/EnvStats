#' Plot Two Cumulative Distribution Functions Based on Censored Data
#' @rawRd \alias{Compare CDFs for Censored Data}
#' @description
#' For one sample, plots the empirical cumulative distribution function (ecdf)
#'   along with a theoretical cumulative distribution function (cdf).
#'   For two samples, plots the two ecdf's.  These plots are used to graphically assess
#'   goodness of fit.
#' @usage
#' cdfCompareCensored(x, censored, censoring.side = "left",
#'     y = NULL, y.censored = NULL, y.censoring.side = censoring.side,
#'     discrete = FALSE, prob.method = "michael-schucany",
#'     plot.pos.con = NULL, distribution = "norm", param.list = NULL,
#'     estimate.params = is.null(param.list), est.arg.list = NULL,
#'     x.col = "blue", y.or.fitted.col = "black", x.lwd = 3 * par("cex"),
#'     y.or.fitted.lwd = 3 * par("cex"), x.lty = 1, y.or.fitted.lty = 2,
#'     include.x.cen = FALSE, x.cen.pch = ifelse(censoring.side == "left", 6, 2),
#'     x.cen.cex = par("cex"), x.cen.col = "red",
#'     include.y.cen = FALSE, y.cen.pch = ifelse(y.censoring.side == "left", 6, 2),
#'     y.cen.cex = par("cex"), y.cen.col = "black", digits = .Options$digits, ...,
#'     type = ifelse(discrete, "s", "l"), main = NULL, xlab = NULL, ylab = NULL,
#'     xlim = NULL, ylim = NULL)
#' @rawRd
#' \arguments{
#'   \item{x}{
#'   numeric vector of observations.  Missing (\code{NA}), undefined (\code{NaN}), and
#'   infinite (\code{Inf}, \code{-Inf}) values are allowed but will be removed.
#' }
#'   \item{censored}{
#'   numeric or logical vector indicating which values of \code{x} are censored.  This must be the
#'   same length as \code{x}.  If the mode of \code{censored} is \code{"logical"}, \code{TRUE} values
#'   correspond to elements of \code{x} that are censored, and \code{FALSE} values correspond to
#'   elements of \code{x} that are not censored.  If the mode of \code{censored} is \code{"numeric"},
#'   it must contain only \code{1}'s and \code{0}'s; \code{1} corresponds to \code{TRUE} and
#'   \code{0} corresponds to \code{FALSE}.  Missing (\code{NA}) values are allowed but will be removed.
#' }
#'   \item{censoring.side}{
#'   character string indicating on which side the censoring occurs.  The possible values are
#'   \code{"left"} (the default) and \code{"right"}.
#' }
#'   \item{y}{
#'   a numeric vector (not necessarily of the same length as \code{x}).
#'   Missing (\code{NA}), undefined (\code{NaN}), and infinite
#'   (\code{Inf}, \code{-Inf}) values are allowed but will be removed.
#'   The default value is \code{y=NULL}, in which case the empirical cdf of
#'   \code{x} will be plotted along with the theoretical cdf specified by the
#'   argument \code{distribution}.
#' }
#'   \item{y.censored}{
#'   numeric or logical vector indicating which values of \code{y} are censored.
#'   This must be the same length as \code{y}.  If the mode of \code{censored} is
#'   \code{"logical"}, \code{TRUE} values correspond to elements of \code{y} that are
#'   censored, and \code{FALSE} values correspond to elements of \code{y} that are not
#'   censored.  If the mode of \code{censored} is \code{"numeric"}, it must contain only
#'   \code{1}'s and \code{0}'s; \code{1} corresponds to \code{TRUE} and
#'   \code{0} corresponds to \code{FALSE}.
#'   Missing (\code{NA}) values are allowed but will be removed.
#'
#'   This argument is ignored when \code{y} is not supplied.  The default value is \cr
#'   \code{y.censored=NULL} since the default value of \code{y} is \code{y=NULL}.
#' }
#'   \item{y.censoring.side}{
#'   character string indicating on which side the censoring occurs for the values of
#'   \code{y}.  The possible values are \code{"left"} (the default) and \code{"right"}.
#'   This argument is ignored when \code{y} is not supplied.  The default value is \cr
#'   \code{y.censoring.side=censoring.side}.
#' }
#'   \item{discrete}{
#'   logical scalar indicating whether the assumed parent distribution of \code{x}
#'   is discrete (\code{discrete=TRUE}) or continuous (\code{discrete=FALSE}; the default).
#' }
#'   \item{prob.method}{
#'   character string indicating what method to use to compute the plotting positions (empirical probabilities).
#'   Possible values are
#'   \code{"kaplan-meier"} (product-limit method of Kaplan and Meier (1958)),
#'   \code{"nelson"} (hazard plotting method of Nelson (1972)),
#'   \code{"michael-schucany"} (generalization of the product-limit method due to Michael and Schucany (1986)), and
#'   \code{"hirsch-stedinger"} (generalization of the product-limit method due to Hirsch and Stedinger (1987)).
#'   The default value is \code{prob.method="michael-schucany"}.
#'
#'   The \code{"nelson"} method is only available for \code{censoring.side="right"}.
#'   See the help file for \code{\link{ecdfPlotCensored}} for more explanation.
#' }
#'   \item{plot.pos.con}{
#'   numeric scalar between 0 and 1 containing the value of the plotting position constant.
#'   When \code{y} is supplied, the default value is \code{plot.pos.con=0.375}.
#'   When \code{y} is not supplied, for the normal, lognormal, three-parameter lognormal,
#'   zero-modified normal, and zero-modified lognormal distributions, the default value
#'   is \code{plot.pos.con=0.375}.
#'   For the Type I extreme value (Gumbel) distribution (\code{distribution="evd"}),
#'   the default value is \code{plot.pos.con=0.44}.  For all other distributions, the
#'   default value is \code{plot.pos.con=0.4}.
#'   See the help files for \code{\link{ecdfPlot}} and \code{\link{qqPlot}} for more
#'   information.  This argument is used only if \code{prob.method} is equal to
#'   \code{"michael-schucany"} or \code{"hirsch-stedinger"}.
#' }
#'   \item{distribution}{
#'   when \code{y} is not supplied,
#'   a character string denoting the distribution abbreviation.  The default value is
#'   \code{distribution="norm"}.  See the help file for \cr
#'   \code{\link{Distribution.df}} for a
#'   list of possible distribution abbreviations.  This argument is ignored if \code{y}
#'   is supplied.
#' }
#'   \item{param.list}{
#'   when \code{y} is not supplied,
#'   a list with values for the parameters of the distribution.  The default value is
#'   \code{param.list=list(mean=0, sd=1)}.  See the help file for \code{\link{Distribution.df}}
#'   for the names and possible values of the parameters associated with each distribution.
#'   This argument is ignored if \code{y} is supplied or \code{estimate.params=TRUE}.
#' }
#'   \item{estimate.params}{
#'   when \code{y} is not supplied,
#'   a logical scalar indicating whether to compute the cdf for \code{x}
#'   based on estimating the distribution parameters (\code{estimate.params=TRUE}) or
#'   using the known distribution parameters specified in \code{param.list} \cr
#'   (\code{estimate.params=FALSE}).  The default value is \code{TRUE} unless the argument \cr
#'   \code{param.list} is supplied.  The argument \code{estimate.params}
#'   is ignored if \code{y} is supplied.
#' }
#'   \item{est.arg.list}{
#'   when \code{y} is not supplied and \code{estimate.params=TRUE},
#'   a list whose components are optional arguments associated with the function used to
#'   estimate the parameters of the assumed distribution (see the Section
#'   \bold{Estimating Distribution Parameters} in the help file \link{Censored Data}).
#'   For example, all functions used to estimate distribution parameters have an
#'   optional argument called \code{method} that specifies the method to use to estimate
#'   the parameters.
#'   (See the help file for \code{\link{Distribution.df}} for a list of available estimation
#'   methods for each distribution.)  To override the default estimation method, supply the argument
#'   \code{est.arg.list} with a component called \code{method}; for example \cr
#'   \code{est.arg.list=list(method="mle")}.  The default value is
#'   \code{est.arg.list=NULL} so that all default values for the estimating function are used.
#'   This argument is ignored if \code{estimate.params=FALSE} or \code{y} is supplied.
#' }
#'   \item{x.col}{
#'   a numeric scalar or character string determining the color of the empirical cdf
#'   (based on \code{x}) line or points.  The default value is \code{x.col="blue"}.
#'   See the entry for \code{col} in the help file for \code{\link{par}} for more
#'   information.
#' }
#'   \item{y.or.fitted.col}{
#'   a numeric scalar or character string determining the color of the empirical cdf
#'   (based on \code{y}) or the theoretical cdf line or points.
#'   The default value is \cr
#'   \code{y.or.fitted.col="black"}.  See the entry for
#'   \code{col} in the help file for \code{\link{par}} for more information.
#' }
#'   \item{x.lwd}{
#'   a numeric scalar determining the width of the empirical cdf (based on \code{x}) line.
#'   The default value is \code{x.lwd=3*par("cex")}.
#'   See the entry for \code{lwd} in the help file for \code{\link{par}}
#'   for more information.
#' }
#'   \item{y.or.fitted.lwd}{
#'   a numeric scalar determining the width of the empirical cdf (based on \code{y})
#'   or theoretical cdf line.
#'   The default value is \code{y.or.fitted.lwd=3*par("cex")}.
#'   See the entry for \code{lwd} in the help file for \code{\link{par}}
#'   for more information.
#' }
#'   \item{x.lty}{
#'   a numeric scalar determining the line type of the empirical cdf
#'   (based on \code{x}) line.  The default value is
#'   \code{x.lty=1}.  See the entry for \code{lty} in the help file for \code{\link{par}}
#'   for more information.
#' }
#'   \item{y.or.fitted.lty}{
#'   a numeric scalar determining the line type of the empirical cdf
#'   (based on \code{y}) or theoretical cdf line.  The default value is
#'   \code{y.or.fitted.lty=2}.
#'   See the entry for \code{lty} in the help file for \code{\link{par}}
#'   for more information.
#' }
#'   \item{include.x.cen}{
#'   logical scalar indicating whether to include censored values in \code{x} in the
#'   plot.  The default value is \code{include.x.cen=FALSE}.  If \code{include.x.cen=TRUE},
#'   censored values in \code{x} are plotted using the plotting character indicated by
#'   the argument \code{x.cen.pch} (see below).  This argument is ignored if there are
#'   no censored values in \code{x}.
#' }
#'   \item{x.cen.pch}{
#'   numeric scalar or character string indicating the plotting character to use to plot
#'   censored values in \code{x}.  The default value is \code{x.cen.pch=2}
#'   (hollow triangle pointing up) when \code{x.censoring.side="right"}, and
#'   \code{x.cen.pch=6} (hollow triangle pointing down) when \code{x.censoring.side="left"}.
#'   See the \R help file for \code{\link{points}} for an explanation of how plotting
#'   symbols are specified.  This argument is ignored if \code{include.x.cen=FALSE}.
#' }
#'   \item{x.cen.cex}{
#'   numeric scalar that determines the size of the plotting character used to plot
#'   censored values in \code{x}. The default value is the current value of the
#'   \code{cex} graphics parameter.  See the entry for \code{cex} in the \R help file for
#'   \code{\link{par}} for more information.  This argument is ignored if
#'   \code{include.x.cen=FALSE}.
#' }
#'   \item{x.cen.col}{
#'   numeric scalar or character string that determines the color of the plotting
#'   character used to plot censored values in \code{x}. The default value is
#'   \code{x.cen.col="red"}.  See the entry for \code{col} in the \R help file for
#'   \code{\link{par}} for more information.  This argument is ignored if
#'   \code{include.x.cen=FALSE}.
#' }
#'   \item{include.y.cen}{
#'   logical scalar indicating whether to include censored values in \code{y} in the plot.
#'   The default value is \code{include.y.cen=FALSE}.  If \code{include.y.cen=TRUE},
#'   censored values in \code{y} are plotted using the plotting character indicated by
#'   the argument \code{y.cen.pch} (see below).  This argument is ignored if \code{y} is
#'   not supplied and/or there are no censored values in \code{y}.
#' }
#'   \item{y.cen.pch}{
#'   numeric scalar or character string indicating the plotting character to use to plot
#'   censored values in \code{y}.  The default value is \code{y.cen.pch=2}
#'   (hollow triangle pointing up) when \code{y.censoring.side="right"}, and
#'   \code{y.cen.pch=6} (hollow triangle pointing down) when \code{y.censoring.side="left"}.
#'   See the \R help file for \code{\link{points}} for an explanation of how plotting
#'   symbols are specified.  This argument is ignored if \code{include.y.cen=FALSE}.
#' }
#'   \item{y.cen.cex}{
#'   numeric scalar that determines the size of the plotting character used to plot
#'   censored values in \code{y}.  The default value is the current value of the
#'   \code{cex} graphics parameter.  See the entry for \code{cex} in the \R help file for
#'   \code{\link{par}} for more information.  This argument is ignored if
#'   \code{include.y.cen=FALSE}.
#' }
#'   \item{y.cen.col}{
#'   numeric scalar or character string that determines the color of the plotting
#'   character used to plot censored values in \code{y}.  The default value is
#'   \code{y.cen.col="black"}.  See the entry for \code{col} in the \R help file for
#'   \code{\link{par}} for more information.  This argument is ignored if
#'   \code{include.y.cen=FALSE}.
#' }
#'   \item{digits}{
#'   when \code{y} is not supplied,
#'   a scalar indicating how many significant digits to print for the distribution
#'   parameters.  The default value is \code{digits=.Options$digits}.
#' }
#'   \item{type, main, xlab, ylab, xlim, ylim, \dots}{
#'   additional graphical parameters (see \code{\link{lines}} and \code{\link{par}}).
#'   In particular, the argument \code{type} specifies the kind of line type.
#'   By default, the function \cr
#'   \code{cdfCompareCensored} plots a step function (\code{type="s"})
#'   when \code{discrete=TRUE}, and plots a straight line between points
#'   (\code{type="l"}) when \code{discrete=FALSE}.
#'   The user may override these defaults by supplying the graphics parameter \code{type}
#'   (\code{type="s"} for a step function, \code{type="l"} for linear interpolation,
#'   \code{type="p"} for points only, etc.).
#' }
#' }
#' @rawRd
#' \details{
#'   When both \code{x} and \code{y} are supplied, the function \code{cdfCompareCensored}
#'   creates the empirical cdf plot of \code{x} and \code{y} on
#'   the same plot by calling the function \code{\link{ecdfPlotCensored}}.
#'
#'   When \code{y} is not supplied, the function \code{cdfCompareCensored} creates the
#'   emprical cdf plot of \code{x} (by calling \code{\link{ecdfPlotCensored}}) and the
#'   theoretical cdf plot (by calling \code{\link{cdfPlot}} and using the
#'   argument \code{distribution}) on the same plot.
#' }
#' @rawRd
#' \value{
#'   When \code{y} is supplied, \code{cdfCompareCensored} invisibly returns a list with
#'   components:
#'   \item{x.ecdf.list}{a list with components \code{Order.Statistics} and
#'   \code{Cumulative.Probabilities}, giving coordinates of the points that have
#'   been plotted for the \code{x} values.}
#'   \item{y.ecdf.list}{a list with components \code{Order.Statistics} and
#'   \code{Cumulative.Probabilities}, giving coordinates of the points that have
#'   been plotted for the \code{y} values.}
#'
#'   When \code{y} is not supplied, \code{cdfCompareCensored} invisibly returns a list with
#'   components:
#'   \item{x.ecdf.list}{a list with components \code{Order.Statistics} and
#'   \code{Cumulative.Probabilities}, giving coordinates of the points that have
#'   been plotted for the \code{x} values.}
#'   \item{fitted.cdf.list}{a list with components \code{Quantiles} and
#'   \code{Cumulative.Probabilities}, giving coordinates of the
#'   points that have been plotted for the fitted cdf.}
#' }
#' @rawRd
#' \references{
#'   Chambers, J.M., W.S. Cleveland, B. Kleiner, and P.A. Tukey. (1983).
#'   \emph{Graphical Methods for Data Analysis}. Duxbury Press, Boston, MA, pp.11-16.
#'
#'   Cleveland, W.S. (1993). \emph{Visualizing Data}. Hobart Press, Summit, New Jersey, 360pp.
#'
#'   D'Agostino, R.B. (1986a). Graphical Analysis.
#'   In: D'Agostino, R.B., and M.A. Stephens, eds. \emph{Goodness-of Fit Techniques}.
#'   Marcel Dekker, New York, Chapter 2, pp.7-62.
#'
#'   Gillespie, B.W., Q. Chen, H. Reichert, A. Franzblau, E. Hedgeman, J. Lepkowski,
#'   P. Adriaens, A. Demond, W. Luksemburg, and D.H. Garabrant. (2010).  Estimating Population
#'   Distributions When Some Data Are Below a Limit of Detection by Using a Reverse
#'   Kaplan-Meier Estimator.  \emph{Epidemiology} \bold{21}(4), S64--S70.
#'
#'   Helsel, D.R. (2012). \emph{Statistics for Censored Environmental Data Using Minitab and R,
#'   Second Edition}.  John Wiley & Sons, Hoboken, New Jersey.
#'
#'   Helsel, D.R., and T.A. Cohn. (1988). Estimation of Descriptive Statistics for Multiply Censored
#'   Water Quality Data. \emph{Water Resources Research} \bold{24}(12), 1997-2004.
#'
#'   Hirsch, R.M., and J.R. Stedinger. (1987). Plotting Positions for Historical Floods and Their Precision.
#'   \emph{Water Resources Research} \bold{23}(4), 715-727.
#'
#'   Kaplan, E.L., and P. Meier. (1958). Nonparametric Estimation From Incomplete Observations.
#'   \emph{Journal of the American Statistical Association} \bold{53}, 457-481.
#'
#'   Lee, E.T., and J.W. Wang. (2003).
#'   \emph{Statistical Methods for Survival Data Analysis, Third Edition}.
#'   John Wiley & Sons, Hoboken, New Jersey, 513pp.
#'
#'   Michael, J.R., and W.R. Schucany. (1986). Analysis of Data from Censored Samples.
#'   In D'Agostino, R.B., and M.A. Stephens, eds. \emph{Goodness-of Fit Techniques}.
#'   Marcel Dekker, New York, 560pp, Chapter 11, 461-496.
#'
#'   Nelson, W. (1972). Theory and Applications of Hazard Plotting for Censored Failure Data.
#'   \emph{Technometrics} \bold{14}, 945-966.
#'
#'   USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
#'   EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.
#'   U.S. Environmental Protection Agency, Washington, D.C. Chapter 15.
#'
#'   USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
#'   EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
#'   U.S. Environmental Protection Agency, Washington, D.C.
#' }
#' @rawRd
#' \author{
#'     Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \note{
#'   An empirical cumulative distribution function (ecdf) plot is a graphical tool that
#'   can be used in conjunction with other graphical tools such as histograms,
#'   strip charts, and boxplots to assess the characteristics of a set of data.
#'   It is easy to determine quartiles and the minimum and maximum
#'   values from such a plot.  Also, ecdf plots allow you to assess local density:
#'   a higher density of observations occurs where the slope is steep.
#'
#'   Chambers et al. (1983, pp.11-16) plot the observed order statistics on the
#'   \eqn{y}-axis vs. the ecdf on the \eqn{x}-axis and call this a quantile plot.
#'
#'   Censored observations complicate the procedures used to graphically explore data.
#'   Techniques from survival analysis and life testing have been developed to generalize
#'   the procedures for constructing plotting positions, empirical cdf plots, and
#'   q-q plots to data sets with censored observations
#'   (see \code{\link{ppointsCensored}}).
#'
#'   Empirical cumulative distribution function (ecdf) plots are often plotted with
#'   theoretical cdf plots to graphically assess whether a sample of observations
#'   comes from a particular distribution.  More often, however, quantile-quantile
#'   (Q-Q) plots are used instead of ecdf plots to graphically assess departures from
#'   an assumed distribution (see \code{\link{qqPlotCensored}}).
#' }
#' @rawRd
#' \seealso{
#'   \code{\link{cdfPlot}}, \code{\link{ecdfPlotCensored}}, \code{\link{qqPlotCensored}}.
#' }
#' @rawRd
#' \examples{
#'   # Generate 20 observations from a normal distribution with mean=20 and sd=5,
#'   # censor all observations less than 18, then compare the empirical cdf with a
#'   # theoretical normal cdf that is based on estimating the parameters.
#'   # (Note: the call to set.seed simply allows you to reproduce this example.)
#'
#'   set.seed(333)
#'   x <- sort(rnorm(20, mean=20, sd=5))
#'   x
#'   # [1]  9.743551 12.370197 14.375499 15.628482 15.883507 17.080124
#'   # [7] 17.197588 18.097714 18.654182 19.585942 20.219308 20.268505
#'   #[13] 20.552964 21.388695 21.763587 21.823639 23.168039 26.165269
#'   #[19] 26.843362 29.673405
#'
#'   censored <- x < 18
#'   x[censored] <- 18
#'
#'   sum(censored)
#'   #[1] 7
#'
#'   dev.new()
#'   cdfCompareCensored(x, censored)
#'
#'   # Clean up
#'   #---------
#'   rm(x, censored)
#'
#'   #==========
#'
#'   # Example 15-1 of USEPA (2009, page 15-10) gives an example of
#'   # computing plotting positions based on censored manganese
#'   # concentrations (ppb) in groundwater collected at 5 monitoring
#'   # wells.  The data for this example are stored in
#'   # EPA.09.Ex.15.1.manganese.df.  Here we will compare the empirical
#'   # cdf based on Kaplan-Meier plotting positions or Michael-Schucany
#'   # plotting positions with various assumed distributions
#'   # (based on estimating the parameters of these distributions):
#'   # 1) normal distribution
#'   # 2) lognormal distribution
#'   # 3) gamma distribution
#'
#'   # First look at the data:
#'   #------------------------
#'
#'   EPA.09.Ex.15.1.manganese.df
#'   #   Sample   Well Manganese.Orig.ppb Manganese.ppb Censored
#'   #1       1 Well.1                 <5           5.0     TRUE
#'   #2       2 Well.1               12.1          12.1    FALSE
#'   #3       3 Well.1               16.9          16.9    FALSE
#'   #4       4 Well.1               21.6          21.6    FALSE
#'   #5       5 Well.1                 <2           2.0     TRUE
#'   #...
#'   #21      1 Well.5               17.9          17.9    FALSE
#'   #22      2 Well.5               22.7          22.7    FALSE
#'   #23      3 Well.5                3.3           3.3    FALSE
#'   #24      4 Well.5                8.4           8.4    FALSE
#'   #25      5 Well.5                 <2           2.0     TRUE
#'
#'   longToWide(EPA.09.Ex.15.1.manganese.df,
#'     "Manganese.Orig.ppb", "Sample", "Well",
#'     paste.row.name = TRUE)
#'
#'   #         Well.1 Well.2 Well.3 Well.4 Well.5
#'   #Sample.1     <5     <5     <5    6.3   17.9
#'   #Sample.2   12.1    7.7    5.3   11.9   22.7
#'   #Sample.3   16.9   53.6   12.6     10    3.3
#'   #Sample.4   21.6    9.5  106.3     <2    8.4
#'   #Sample.5     <2   45.9   34.5   77.2     <2
#'
#'
#'   # Assume a normal distribution
#'   #-----------------------------
#'
#'   # Michael-Schucany plotting positions:
#'   dev.new()
#'   with(EPA.09.Ex.15.1.manganese.df,
#'     cdfCompareCensored(Manganese.ppb, Censored))
#'
#'   # Kaplan-Meier plotting positions:
#'   dev.new()
#'   with(EPA.09.Ex.15.1.manganese.df,
#'     cdfCompareCensored(Manganese.ppb, Censored,
#'       prob.method = "kaplan-meier"))
#'
#'
#'   # Assume a lognormal distribution
#'   #--------------------------------
#'
#'   # Michael-Schucany plotting positions:
#'   dev.new()
#'   with(EPA.09.Ex.15.1.manganese.df,
#'     cdfCompareCensored(Manganese.ppb, Censored, dist = "lnorm"))
#'
#'   # Kaplan-Meier plotting positions:
#'   dev.new()
#'   with(EPA.09.Ex.15.1.manganese.df,
#'     cdfCompareCensored(Manganese.ppb, Censored, dist = "lnorm",
#'       prob.method = "kaplan-meier"))
#'
#'
#'   # Assume a gamma distribution
#'   #----------------------------
#'
#'   # Michael-Schucany plotting positions:
#'   dev.new()
#'   with(EPA.09.Ex.15.1.manganese.df,
#'     cdfCompareCensored(Manganese.ppb, Censored, dist = "gamma"))
#'
#'   # Kaplan-Meier plotting positions:
#'   dev.new()
#'   with(EPA.09.Ex.15.1.manganese.df,
#'     cdfCompareCensored(Manganese.ppb, Censored, dist = "gamma",
#'       prob.method = "kaplan-meier"))
#'
#'   # Clean up
#'   #---------
#'   graphics.off()
#'
#'   #==========
#'
#'   # Compare the distributions of copper and zinc between the Alluvial Fan Zone
#'   # and the Basin-Trough Zone using the data of Millard and Deverel (1988).
#'   # The data are stored in Millard.Deverel.88.df.
#'
#'   Millard.Deverel.88.df
#'   #    Cu.orig Cu Cu.censored Zn.orig  Zn Zn.censored         Zone Location
#'   #1       < 1  1        TRUE     <10  10        TRUE Alluvial.Fan        1
#'   #2       < 1  1        TRUE       9   9       FALSE Alluvial.Fan        2
#'   #3         3  3       FALSE      NA  NA       FALSE Alluvial.Fan        3
#'   #.
#'   #.
#'   #.
#'   #116       5  5       FALSE      50  50       FALSE Basin.Trough       48
#'   #117      14 14       FALSE      90  90       FALSE Basin.Trough       49
#'   #118       4  4       FALSE      20  20       FALSE Basin.Trough       50
#'
#'   Cu.AF <- with(Millard.Deverel.88.df,
#'     Cu[Zone == "Alluvial.Fan"])
#'
#'   Cu.AF.cen <- with(Millard.Deverel.88.df,
#'     Cu.censored[Zone == "Alluvial.Fan"])
#'
#'   Cu.BT <- with(Millard.Deverel.88.df,
#'     Cu[Zone == "Basin.Trough"])
#'
#'   Cu.BT.cen <- with(Millard.Deverel.88.df,
#'     Cu.censored[Zone == "Basin.Trough"])
#'
#'   Zn.AF <- with(Millard.Deverel.88.df,
#'     Zn[Zone == "Alluvial.Fan"])
#'
#'   Zn.AF.cen <- with(Millard.Deverel.88.df,
#'     Zn.censored[Zone == "Alluvial.Fan"])
#'
#'   Zn.BT <- with(Millard.Deverel.88.df,
#'     Zn[Zone == "Basin.Trough"])
#'
#'   Zn.BT.cen <- with(Millard.Deverel.88.df,
#'     Zn.censored[Zone == "Basin.Trough"])
#'
#'
#'   # First compare the copper concentrations
#'   #----------------------------------------
#'   dev.new()
#'   cdfCompareCensored(x = Cu.AF, censored = Cu.AF.cen,
#'     y = Cu.BT, y.censored = Cu.BT.cen)
#'
#'
#'   # Now compare the zinc concentrations
#'   #------------------------------------
#'   dev.new()
#'   cdfCompareCensored(x = Zn.AF, censored = Zn.AF.cen,
#'     y = Zn.BT, y.censored = Zn.BT.cen)
#'
#'
#'   # Compare the Zinc concentrations again, but delete
#'   # the one "outlier".
#'   #--------------------------------------------------
#'
#'   summaryStats(Zn.AF)
#'   #       N    Mean      SD Median Min Max NA's N.Total
#'   #Zn.AF 67 23.5075 74.4192     10   3 620    1      68
#'
#'   summaryStats(Zn.BT)
#'   #       N  Mean      SD Median Min Max
#'   #Zn.BT 50 21.94 18.7044   18.5   3  90
#'
#'   which(Zn.AF == 620)
#'   #[1] 38
#'
#'   summaryStats(Zn.AF[-38])
#'   #            N    Mean     SD Median Min Max NA's N.Total
#'   #Zn.AF[-38] 66 14.4697 8.1604     10   3  50    1      67
#'
#'
#'   dev.new()
#'   cdfCompareCensored(x = Zn.AF[-38], censored = Zn.AF.cen[-38],
#'     y = Zn.BT, y.censored = Zn.BT.cen)
#'
#'   #----------
#'
#'   # Clean up
#'   #---------
#'
#'   rm(Cu.AF, Cu.AF.cen, Cu.BT, Cu.BT.cen,
#'      Zn.AF, Zn.AF.cen, Zn.BT, Zn.BT.cen)
#'   graphics.off()
#' }
#' @rawRd
#' \keyword{distribution}
#' @rawRd
#' \keyword{hplot}

cdfCompareCensored <-
function (x, censored, censoring.side = "left", y = NULL, y.censored = NULL, 
    y.censoring.side = censoring.side, discrete = FALSE, prob.method = "michael-schucany", 
    plot.pos.con = NULL, distribution = "norm", param.list = NULL, 
    estimate.params = is.null(param.list), est.arg.list = NULL, 
    x.col = "blue", y.or.fitted.col = "black", x.lwd = 3 * par("cex"), 
    y.or.fitted.lwd = 3 * par("cex"), x.lty = 1, y.or.fitted.lty = 2, 
    include.x.cen = FALSE, x.cen.pch = ifelse(censoring.side == 
        "left", 6, 2), x.cen.cex = par("cex"), x.cen.col = "red", 
    include.y.cen = FALSE, y.cen.pch = ifelse(y.censoring.side == 
        "left", 6, 2), y.cen.cex = par("cex"), y.cen.col = "black", 
    digits = .Options$digits, ..., type = ifelse(discrete, "s", 
        "l"), main = NULL, xlab = NULL, ylab = NULL, xlim = NULL, 
    ylim = NULL) 
{
    if (!is.vector(x, mode = "numeric")) 
        stop("'x' must be a numeric vector")
    if (!is.vector(censored, mode = "numeric") & !is.vector(censored, 
        mode = "logical")) 
        stop("'censored' must be a logical or numeric vector")
    if (length(censored) != length(x)) 
        stop("'censored' must be the same length as 'x'")
    x.name <- deparse(substitute(x))
    if ((bad.obs <- sum(!(ok <- is.finite(x) & is.finite(as.numeric(censored))))) > 
        0) {
        x <- x[ok]
        censored <- censored[ok]
        warning(paste(bad.obs, "observations with NA/NaN/Inf in 'x' and/or 'censored' removed."))
    }
    if (is.numeric(censored)) {
        if (!all(censored == 0 | censored == 1)) 
            stop(paste("When 'censored' is a numeric vector, all values of", 
                "'censored' must be 0 (not censored) or 1 (censored)."))
        censored <- as.logical(censored)
    }
    n.cen.x <- sum(censored)
    K <- sort(unique(x[censored]))
    if (n.cen.x == 0 && is.null(y)) 
        stop(paste("No censored values indicated by 'censored';", 
            "use \n\t\t\tthe function 'cdfCompare'"))
    if (length(unique(x[!censored])) < 2) 
        stop("'x' must contain at least two non-missing, uncensored value.")
    check.gp.list <- checkGraphicsPars(...)
    gp.arg.list <- check.gp.list$gp.arg.list
    gen.gp.list <- check.gp.list$gen.gp.list
    prob.method <- match.arg(prob.method, c("michael-schucany", 
        "hirsch-stedinger", "kaplan-meier", "nelson"))
    censoring.side <- match.arg(censoring.side, c("left", "right"))
    if (n.cen.x > 0 && censoring.side == "left" && prob.method == 
        "nelson") 
        stop("Nelson method not available for censoring.side='left'")
    if (!is.null(y)) {
        if (!is.vector(y, mode = "numeric")) 
            stop("'y' must be a numeric vector")
        if (!is.vector(y.censored, mode = "numeric") & !is.vector(y.censored, 
            mode = "logical")) 
            stop("'y.censored' must be a logical or numeric vector")
        if (length(y.censored) != length(y)) 
            stop("'y.censored' must be the same length as 'y'")
        y.name <- deparse(substitute(y))
        if ((bad.obs <- sum(!(ok <- is.finite(y) & is.finite(as.numeric(y.censored))))) > 
            0) {
            y <- y[ok]
            y.censored <- y.censored[ok]
            warning(paste(bad.obs, "observations with NA/NaN/Inf in 'y' and/or 'y.censored' removed."))
        }
        if (is.numeric(y.censored)) {
            if (!all(y.censored == 0 | y.censored == 1)) 
                stop(paste("When 'y.censored' is a numeric vector, all values of", 
                  "'y.censored' must be 0 (not censored) or 1 (censored)."))
            y.censored <- as.logical(y.censored)
        }
        n.cen.y <- sum(y.censored)
        if (n.cen.x == 0 && n.cen.y == 0) 
            stop(paste("No censored values indicated by 'censored'", 
                "and 'y.censored';", "use \n\t\t\tthe function 'cdfCompare'"))
        if (length(unique(y[!y.censored])) < 2) 
            stop("'y' must contain at least two non-missing, uncensored value.")
        if (is.null(plot.pos.con)) {
            plot.pos.con <- 0.375
        }
        else {
            if (!is.vector(plot.pos.con, mode = "numeric") || 
                length(plot.pos.con) != 1 || plot.pos.con < 0 || 
                plot.pos.con > 1) 
                stop("'plot.pos.con' must be a numeric scalar between 0 and 1")
        }
        y.censoring.side <- match.arg(y.censoring.side, c("left", 
            "right"))
        if (n.cen.y > 0 && y.censoring.side == "left" && prob.method == 
            "nelson") 
            stop("Nelson method not available for y.censoring.side='left'")
        if (is.null(main)) {
            if (n.cen.x > 0) 
                x.string <- paste(x.name, "(Censored; solid line)\n")
            else x.string <- paste(x.name, "(solid line)\n")
            if (n.cen.y > 0) 
                y.string <- paste(y.name, "(Censored; dashed line)")
            else y.string <- paste(y.name, "(dashed line)\n")
            main <- paste("Empirical CDF for ", x.string, "with Empirical CDF for ", 
                y.string, sep = "", collapse = "")
        }
        if (is.null(xlab)) 
            xlab <- paste("Order Statistics for", x.name, "and", 
                y.name)
        if (is.null(xlim)) 
            xlim <- range(x, y)
        if (n.cen.x == 0) 
            x.ecdf.list <- ecdfPlot(x, discrete = discrete, prob.method = "plot.pos", 
                plot.pos.con = plot.pos.con, ecdf.col = x.col, 
                ecdf.lwd = x.lwd, ecdf.lty = x.lty, ..., type = type, 
                main = main, xlab = xlab, ylab = ylab, xlim = xlim, 
                ylim = ylim)
        else x.ecdf.list <- ecdfPlotCensored(x = x, censored = censored, 
            censoring.side = censoring.side, discrete = discrete, 
            prob.method = prob.method, plot.pos.con = plot.pos.con, 
            ecdf.col = x.col, ecdf.lwd = x.lwd, ecdf.lty = x.lty, 
            include.cen = include.x.cen, cen.pch = x.cen.pch, 
            cen.cex = x.cen.cex, cen.col = x.cen.col, ..., type = type, 
            main = main, xlab = xlab, ylab = ylab, xlim = xlim, 
            ylim = ylim)
        if (n.cen.y == 0) {
            arg.list <- list(x = y, discrete = discrete, prob.method = "plot.pos", 
                plot.pos.con = plot.pos.con, add = TRUE, ecdf.col = y.or.fitted.col, 
                ecdf.lwd = y.or.fitted.lwd, ecdf.lty = y.or.fitted.lty)
            arg.list <- c(arg.list, gen.gp.list, list(type = type))
            y.ecdf.list <- do.call("ecdfPlot", arg.list)
        }
        else {
            arg.list <- list(x = y, censored = y.censored, censoring.side = y.censoring.side, 
                discrete = discrete, prob.method = prob.method, 
                plot.pos.con = plot.pos.con, add = TRUE, ecdf.col = y.or.fitted.col, 
                ecdf.lwd = y.or.fitted.lwd, ecdf.lty = y.or.fitted.lty, 
                include.cen = include.y.cen, cen.pch = y.cen.pch, 
                cen.cex = y.cen.cex, cen.col = y.cen.col)
            arg.list <- c(arg.list, gen.gp.list, list(type = type))
            y.ecdf.list <- do.call("ecdfPlotCensored", arg.list)
        }
        ret.list <- list(x.ecdf.list = x.ecdf.list, y.ecdf.list = y.ecdf.list)
    }
    else {
        if (estimate.params) 
            check.da.list <- check.distribution.args(distribution, 
                check.params = FALSE)
        else {
            if (is.null(param.list)) 
                stop(paste("When 'estimate.params=F' you must supply", 
                  "the argument 'param.list'"))
            check.da.list <- check.distribution.args(distribution, 
                param.list)
        }
        dist.abb <- check.da.list$dist.abb
        dist.name <- check.da.list$dist.name
        dist.type <- check.da.list$dist.type
        n.dist.params <- check.da.list$n.dist.params
        dist.params.names <- check.da.list$dist.params.names
        if (estimate.params) {
            e.fcn <- paste("e", dist.abb, "Censored", sep = "")
            if (length(find(e.fcn)) == 0) 
                stop(paste("No estimation method available for the", 
                  dist.name, "Distribution with Censored Data"))
            est.list <- do.call(e.fcn, c(list(x = x, censored = censored, 
                censoring.side = censoring.side), est.arg.list))
            est.param.vec <- est.list$parameters
            estimation.method <- est.list$method
            if (dist.params.names[n.dist.params] == "ncp") {
                warning(paste("No estimation method available for", 
                  "Non-Central Distributions.\n "))
                est.param.vec <- c(est.param.vec, 0)
            }
            param.list <- as.list(est.param.vec)
            names(param.list) <- dist.params.names
        }
        else param.list <- check.da.list$param.list
        if (is.null(plot.pos.con)) {
            plot.pos.con <- switch(distribution, norm = , lnorm = , 
                lnormAlt = , lnorm3 = , zmnorm = , zmlnorm = , 
                zmlnormAlt = 0.375, evd = 0.44, 0.4)
        }
        else {
            if (!is.vector(plot.pos.con, mode = "numeric") || 
                length(plot.pos.con) != 1 || plot.pos.con < 0 || 
                plot.pos.con > 1) 
                stop("'plot.pos.con' must be a numeric scalar between 0 and 1")
        }
        discrete <- dist.type == "Discrete" || dist.type == "Finite Discrete"
        if (is.null(main)) {
            main <- paste("Empirical CDF for ", x.name, " (Censored; solid line)\nwith Fitted ", 
                dist.name, " CDF (dashed line)", sep = "", collapse = "")
        }
        if (is.null(xlab)) {
            if (any(dist.abb == c("beta", "chisq", "f", "t"))) {
                if (param.list$ncp > 0) 
                  string <- paste("Non-central ", dist.name, 
                    "(", paste(paste(dist.params.names, signif(unlist(param.list), 
                      digits), sep = "="), collapse = ", "), 
                    ")", sep = "")
                else {
                  string <- paste(dist.name, "(", paste(paste(dist.params.names[-n.dist.params], 
                    signif(unlist(param.list[-n.dist.params]), 
                      digits), sep = "="), collapse = ", "), 
                    ")", sep = "")
                }
            }
            else {
                string <- paste(dist.name, "(", paste(paste(dist.params.names, 
                  signif(unlist(param.list), digits), sep = "="), 
                  collapse = ", "), ")", sep = "")
            }
            xlab <- paste("Order Statistics for ", x.name, " and\n", 
                string, " Distribution", sep = "")
        }
        if (is.null(xlim)) {
            qname <- paste("q", dist.abb, sep = "")
            xlim <- do.call(qname, c(list(p = c(0.001, 0.999)), 
                param.list))
        }
        x.ecdf.list <- ecdfPlotCensored(x = x, censored = censored, 
            censoring.side = censoring.side, discrete = discrete, 
            prob.method = prob.method, plot.pos.con = plot.pos.con, 
            ecdf.col = x.col, ecdf.lwd = x.lwd, ecdf.lty = x.lty, 
            include.cen = include.x.cen, cen.pch = x.cen.pch, 
            cen.cex = x.cen.cex, cen.col = x.cen.col, ..., type = type, 
            main = main, xlab = xlab, ylab = ylab, xlim = xlim, 
            ylim = ylim)
        arg.list <- list(distribution = dist.abb, param.list = param.list, 
            add = TRUE, cdf.col = y.or.fitted.col, cdf.lwd = y.or.fitted.lwd, 
            cdf.lty = y.or.fitted.lty)
        arg.list <- c(arg.list, gen.gp.list, list(type = type))
        fitted.cdf.list <- do.call("cdfPlot", arg.list)
        ret.list <- list(x.ecdf.list = x.ecdf.list, fitted.cdf.list = fitted.cdf.list)
    }
    invisible(ret.list)
}
