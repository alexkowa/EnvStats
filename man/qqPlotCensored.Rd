\name{qqPlotCensored}
\alias{qqPlotCensored}
\title{
  Quantile-Quantile (Q-Q) Plot for Type I Censored Data
}
\description{
  Produces a quantile-quantile (Q-Q) plot, also called a probability plot, for Type I censored data.
}
\usage{
  qqPlotCensored(x, censored, censoring.side = "left",
    prob.method = "michael-schucany", plot.pos.con = NULL,
    distribution = "norm", param.list = list(mean = 0, sd = 1),
    estimate.params = plot.type == "Tukey Mean-Difference Q-Q",
    est.arg.list = NULL, plot.type = "Q-Q", plot.it = TRUE,
    equal.axes = qq.line.type == "0-1" || estimate.params,
    add.line = FALSE, qq.line.type = "least squares",
    duplicate.points.method = "standard", points.col = 1, line.col = 1,
    line.lwd = par("cex"), line.lty = 1, digits = .Options$digits,
    include.cen = FALSE, cen.pch = ifelse(censoring.side == "left", 6, 2),
    cen.cex = par("cex"), cen.col = 4, ..., main = NULL, xlab = NULL,
    ylab = NULL, xlim = NULL, ylim = NULL)
}
\arguments{
  \item{x}{
  numeric vector of observations that is assumed to represent a sample from the hypothesized
  distribution specifed by \code{distribution}.   Missing (\code{NA}), undefined (\code{NaN}),
  and infinite (\code{Inf}, \code{-Inf}) values are allowed but will be removed.
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
  \item{prob.method}{
  character string indicating what method to use to compute the plotting positions
  (empirical probabilities).  Possible values are: \cr
  \code{"kaplan-meier"} (product-limit method of Kaplan and Meier (1958)), \cr
  \code{"modified kaplan-meier"} (same as \code{"kaplan-meier"} except the maximum value is plotted too), \cr
  \code{"nelson"} (hazard plotting method of Nelson (1972)), \cr
  \code{"michael-schucany"} (generalization of the product-limit method due to Michael and Schucany (1986)), and \cr
  \code{"hirsch-stedinger"} (generalization of the product-limit method due to Hirsch and Stedinger (1987)).  \cr

  The default value is \code{prob.method="michael-schucany"}.

  The \code{"nelson"} method is only available for \code{censoring.side="right"}, and
  the \code{"modified kaplan-meier"} method is only available for \cr
  \code{censoring.side="left"}.  See the DETAILS section for more explanation.
}
  \item{plot.pos.con}{
  numeric scalar between 0 and 1 containing the value of the plotting position constant.
  The default value is \code{plot.pos.con=0.375}.  See the DETAILS section for more information.
  This argument is used only if \code{prob.method} is equal to \code{"michael-schucany"} or
  \code{"hirsch-stedinger"}.
}
  \item{distribution}{
  a character string denoting the distribution abbreviation.  The default value is
  \code{distribution="norm"}.  See the help file for \code{\link{Distribution.df}} for a
  list of possible distribution abbreviations.
}
  \item{param.list}{
  a list with values for the parameters of the distribution.  The default value is
  \code{param.list=list(mean=0, sd=1)}.  See the help file for \code{\link{Distribution.df}}
  for the names and possible values of the parameters associated with each distribution.
  This argument is ignored if \code{estimate.params=TRUE}.
}
  \item{estimate.params}{
  a logical scalar indicating whether to compute quantiles based on estimating the distribution
  parameters (\code{estimate.params=TRUE}) or using the known distribution parameters specified
  in param.list (\code{estimate.params=FALSE}, the default).  The default value of \code{estimate.params}
  is \code{FALSE} if \code{plot.type="Q-Q"} because the default configuration is a standard normal
  (mean=0, sd=1) Q-Q plot, which will yield roughly a straight line if the observations in
  \code{x} are from any normal distribution.  The default value of \code{estimate.params}
  is \code{TRUE} if \code{plot.type="Tukey Mean-Difference Q-Q"}.

  You can set \code{estimate.params=TRUE} only when the argument \code{distribution} specifies a
  distribution that has an associated function for estimating distribution parameters in the case
  of Type I censored data.  Currently this includes the normal (\code{dist="norm"}),
  lognormal (\code{dist="lnorm"} or \code{dist="lnormAlt"}), and Poisson (\code{dist="pois"})
  distributions (see the section \emph{Estimating Distribution Parameters}
  in the help file \link[=FcnsByCatCensoredData]{EnvStats Functions for Censored Data}).
}
  \item{est.arg.list}{
  a list whose components are optional arguments associated with the function used to estimate
  the parameters of the assumed distribution (see the section \emph{Estimating Distribution Parameters}
  in the help file \link[=FcnsByCatCensoredData]{EnvStats Functions for Censored Data}).
  For example, the function \code{\link{enormCensored}} has an optional argument called
  \code{method} that specifies the method to use to estimate the parameters.  To override the default
  estimation method, supply the argument \code{est.arg.list} with a component called \code{method};
  for example \cr
  \code{est.arg.list=list(method="impute.w.qq.reg")}.  The default value is \cr
  \code{est.arg.list=NULL} so that all default values for the estimating function are used.
  This argument is ignored if \code{estimate.params=FALSE}.
}
  \item{plot.type}{
  a character string denoting the kind of plot.  Possible values are \code{"Q-Q"}
  (Quantile-Quantile plot, the default) and \code{"Tukey Mean-Difference Q-Q"}
  (Tukey mean-difference Q-Q plot).  This argument may be abbreviated (e.g.,
  \code{plot.type="T"} to indicate a Tukey mean-difference Q-Q plot).
}
  \item{plot.it}{
  a logical scalar indicating whether to create a plot on the current graphics device.
  The default value is \code{plot.it=TRUE}.
}
  \item{equal.axes}{
  a logical scalar indicating whether to use the same range on the \eqn{x}- and \eqn{y}-axes
  when \code{plot.type="Q-Q"}.  The default value is \code{TRUE} if \code{qq.line.type="0-1"} or
  \code{estimate.params=TRUE}, otherwise it is \code{FALSE}.  This argument is ignored if
  \code{plot.type="Tukey Mean-Difference Q-Q"}.
}
  \item{add.line}{
  a logical scalar indicating whether to add a line to the plot.  If \code{add.line=TRUE} and
  \code{plot.type="Q-Q"}, a line determined by the value of \code{qq.line.type} is added to the plot.
  If \code{add.line=TRUE} and \cr
  \code{plot.type="Tukey Mean-Difference Q-Q"}, a horizontal line at
  \eqn{y=0} is added to the plot.  The default value is \code{add.line=FALSE}.
}
  \item{qq.line.type}{
  character string determining what kind of line to add to the Q-Q plot.  Possible values are
  \code{"least squares"} (the default), \code{"0-1"} and \code{"robust"}.  For the value
  \code{"least squares"}, a least squares line is fit and added.  For the value \code{"0-1"},
  a line with intercept 0 and slope 1 is added.  For the value \code{"robust"}, a line is fit through
  the first and third quartiles of the \code{x} and \code{y} data.  This argument is ignored if
  \code{add.line=FALSE} or \code{plot.type="Tukey Mean-Difference Q-Q"}.
}
  \item{duplicate.points.method}{
  a character string denoting how to plot points with duplicate \eqn{(x,y)} values.  Possible values
  are \code{"standard"} (the default), \code{"jitter"}, and \code{"number"}.  For the value
  \code{"standard"}, a single plotting symbol is plotted (this is the default behavior of \R).
  For the value \code{"jitter"}, a separate plotting symbol is plotted for each duplicate point, where
  the plotting symbols cluster around the true value of \eqn{x} and \eqn{y}.  For the value
  \code{"number"}, a single number is plotted at \eqn{(x,y)} that represents how many duplicate points
  are at that \eqn{(x,y)} coordinate.
}
  \item{points.col}{
  a numeric scalar or character string determining the color of the points in the plot.
  The default value is \code{points.col=1}.  See the entry for \code{col} in the help file for
  \code{\link{par}} for more information.
}
  \item{line.col}{
  a numeric scalar or character string determining the color of the line in the plot.
  The default value is \code{points.col=1}.  See the entry for \code{col} in the help file for
  \code{\link{par}} for more information.  This argument is ignored if \code{add.line=FALSE}.
}
  \item{line.lwd}{
  a numeric scalar determining the width of the line in the plot.  The default value is
  \code{line.lwd=par("cex")}.  See the entry for \code{lwd} in the help file for \code{\link{par}}
  for more information.  This argument is ignored if \code{add.line=FALSE}.
}
  \item{line.lty}{
  a numeric scalar determining the line type of the line in the plot.  The default value is
  \code{line.lty=1}.  See the entry for \code{lty} in the help file for \code{\link{par}}
  for more information.  This argument is ignored if \code{add.line=FALSE}.
}
  \item{digits}{
  a scalar indicating how many significant digits to print for the distribution parameters.
  The default value is \code{digits=.Options$digits}.
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
  \item{main, xlab, ylab, xlim, ylim, \dots}{
  additional graphical parameters (see \code{\link{par}}).
}
}
\details{
  The function \code{qqPlotCensored} does exactly the same thing as \code{\link{qqPlot}}
  (when the argument \code{y} is not supplied to \code{\link{qqPlot}}), except
  \code{qqPlotCensored} calls the function \code{\link{ppointsCensored}} to compute the
  plotting positions (estimated cumulative probabilities).

  The vector \code{x} is assumed to be a sample from the probability distribution specified
  by the argument \code{distribution} (and \code{param.list} if \code{estimate.params=FALSE}).
  When \code{plot.type="Q-Q"}, the quantiles of \code{x} are plotted on the \eqn{y}-axis against
  the quantiles of the assumed distribution on the \eqn{x}-axis.

  When \code{plot.type="Tukey Mean-Difference Q-Q"}, the difference of the quantiles is plotted on
  the \eqn{y}-axis against the mean of the quantiles on the \eqn{x}-axis.

  When \code{prob.method="kaplan-meier"} and \code{censoring.side="left"} and the assumed
  distribution has a maximum support of infinity (\code{Inf}; e.g., the normal or lognormal
  distribution), the point invovling the largest
  value of \code{x} is not plotted because it corresponds to an estimated cumulative probability
  of 1 which corresponds to an infinite plotting position.

  When \code{prob.method="modified kaplan-meier"} and \code{censoring.side="left"}, the
  estimated cumulative probability associated with the maximum value is modified from 1
  to be \eqn{(N - .375)/(N + .25)} where \eqn{N} denotes the sample size (i.e., the Blom
  plotting position) so that the point associated with the maximum value can be displayed.
}
\value{
  \code{qqPlotCensored} returns a list with the following components:

  \item{x}{numeric vector of \eqn{x}-coordinates for the plot.  When \code{plot.type="Q-Q"}
    these are the quantiles from the theoretical distribution.  When \cr
    \code{plot.type="Tukey Mean-Difference Q-Q"} these are the averages of the observed and
    theoretical quantiles.}
  \item{y}{numeric vector of \eqn{y}-coordinates for the plot.  When \code{plot.type="Q-Q"}
    these are the observed quantiles (order statistics).  When \cr
    \code{plot.type="Tukey Mean-Difference Q-Q"} these are the differences between the
    observed quantiles (order statistics) and the theoretical quantiles.}
  \item{Order.Statistics}{numeric vector of the \dQuote{ordered} observations.
    When \code{plot.type="Q-Q"} this component is exactly the same as the component \code{y}.}
  \item{Cumulative.Probabilities}{numeric vector of the plotting positions associated with the
    order statistics.}
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

  Lee, E.T., and J. Wang. (2003). \emph{Statistical Methods for Survival Data Analysis,
  Third Edition}.  John Wiley and Sons, New York.

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
  A \emph{quantile-quantile (Q-Q) plot}, also called a \emph{probability plot}, is a plot of the observed
  order statistics from a random sample (the empirical quantiles) against their (estimated)
  mean or median values based on an assumed distribution, or against the empirical quantiles
  of another set of data (Wilk and Gnanadesikan, 1968).  Q-Q plots are used to assess whether
  data come from a particular distribution, or whether two datasets have the same parent
  distribution.  If the distributions have the same shape (but not necessarily the same
  location or scale parameters), then the plot will fall roughly on a straight line.  If the
  distributions are exactly the same, then the plot will fall roughly on the straight line \eqn{y=x}.

  A \emph{Tukey mean-difference Q-Q plot}, also called an \emph{m-d plot}, is a modification of a
  Q-Q plot. Rather than plotting observed quantiles vs. theoretical quantiles or observed
  \eqn{y}-quantiles vs. observed \eqn{x}-quantiles, a Tukey mean-difference Q-Q plot plots
  the difference between the quantiles on the \eqn{y}-axis vs. the average of the quantiles on
  the \eqn{x}-axis (Cleveland, 1993, pp.22-23).  If the two sets of quantiles come from the same
  parent distribution, then the points in this plot should fall roughly along the horizontal line
  \eqn{y=0}.  If one set of quantiles come from the same distribution with a shift in median, then
  the points in this plot should fall along a horizontal line above or below the line \eqn{y=0}.
  A Tukey mean-difference Q-Q plot enhances our perception of how the points in the Q-Q plot deviate
  from a straight line, because it is easier to judge deviations from a horizontal line than from a
  line with a non-zero slope.

  In a Q-Q plot, the extreme points have more variability than points toward the center.  A U-shaped
  Q-Q plot indicates that the underlying distribution for the observations on the \eqn{y}-axis is
  skewed to the right relative to the underlying distribution for the observations on the \eqn{x}-axis.
  An upside-down-U-shaped Q-Q plot indicates the \eqn{y}-axis distribution is skewed left relative to
  the \eqn{x}-axis distribution.  An S-shaped Q-Q plot indicates the \eqn{y}-axis distribution has
  shorter tails than the \eqn{x}-axis distribution.  Conversely, a plot that is bent down on the
  left and bent up on the right indicates that the \eqn{y}-axis distribution has longer tails than
  the \eqn{x}-axis distribution.

  Censored observations complicate the procedures used to graphically explore data.  Techniques from
  survival analysis and life testing have been developed to generalize the procedures for
  constructing plotting positions, empirical cdf plots, and Q-Q plots to data sets with censored
  observations (see \code{\link{ppointsCensored}}).
}
\seealso{
  \code{\link{ppointsCensored}}, \link[=FcnsByCatCensoredData]{EnvStats Functions for Censored Data},
  \code{\link{qqPlot}}, \code{\link{ecdfPlotCensored}}, \cr
  \code{\link{qqPlotGestalt}}.
}
\examples{
  # Generate 20 observations from a normal distribution with mean=20 and sd=5,
  # censor all observations less than 18, then generate a Q-Q plot assuming
  # a normal distribution for the complete data set and the censored data set.
  # Note that the Q-Q plot for the censored data set starts at the first ordered
  # uncensored observation, and that for values of x > 18 the two Q-Q plots are
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
  qqPlot(x, ylim = range(pretty(x)),
    main = "Q-Q Plot for\nComplete Data Set")

  dev.new()
  qqPlotCensored(new.x, censored, ylim = range(pretty(x)),
    main="Q-Q Plot for\nCensored Data Set")

  # Clean up
  #---------
  rm(x, censored, new.x)

  #------------------------------------------------------------------------------------

  # Example 15-1 of USEPA (2009, page 15-10) gives an example of
  # computing plotting positions based on censored manganese
  # concentrations (ppb) in groundwater collected at 5 monitoring
  # wells.  The data for this example are stored in
  # EPA.09.Ex.15.1.manganese.df.  Here we will create a Q-Q
  # plot based on the Kaplan-Meier method.  First we'll assume
  # a normal distribution, then a lognormal distribution, then a
  # gamma distribution.

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

  # Assume normal distribution
  #---------------------------

  dev.new()
  with(EPA.09.Ex.15.1.manganese.df,
    qqPlotCensored(Manganese.ppb, Censored,
      prob.method = "kaplan-meier", points.col = "blue", add.line = TRUE,
      main = paste("Normal Q-Q Plot of Manganese Data",
          "Based on Kaplan-Meier Plotting Positions", sep = "\n")))

  # Include max value in the plot
  #------------------------------

  dev.new()
  with(EPA.09.Ex.15.1.manganese.df,
    qqPlotCensored(Manganese.ppb, Censored,
      prob.method = "modified kaplan-meier", points.col = "blue",
      add.line = TRUE,
      main = paste("Normal Q-Q Plot of Manganese Data",
          "Based on Kaplan-Meier Plotting Positions",
          "(Max Included)", sep = "\n")))


  # Assume lognormal distribution
  #------------------------------

  dev.new()
  with(EPA.09.Ex.15.1.manganese.df,
    qqPlotCensored(Manganese.ppb, Censored, dist = "lnorm",
      prob.method = "kaplan-meier", points.col = "blue", add.line = TRUE,
      main = paste("Lognormal Q-Q Plot of Manganese Data",
          "Based on Kaplan-Meier Plotting Positions", sep = "\n")))

  # Include max value in the plot
  #------------------------------

  dev.new()
  with(EPA.09.Ex.15.1.manganese.df,
    qqPlotCensored(Manganese.ppb, Censored, dist = "lnorm",
      prob.method = "modified kaplan-meier", points.col = "blue",
      add.line = TRUE,
      main = paste("Lognormal Q-Q Plot of Manganese Data",
          "Based on Kaplan-Meier Plotting Positions",
          "(Max Included)", sep = "\n")))


  # The lognormal distribution appears to be a better fit.
  # Now create a Q-Q plot assuming a gamma distribution.  Here we'll
  # need to set estimate.params=TRUE.

  dev.new()
  with(EPA.09.Ex.15.1.manganese.df,
    qqPlotCensored(Manganese.ppb, Censored, dist = "gamma",
      estimate.params = TRUE, prob.method = "kaplan-meier",
      points.col = "blue", add.line = TRUE,
      main = paste("Gamma Q-Q Plot of Manganese Data",
          "Based on Kaplan-Meier Plotting Positions", sep = "\n")))

  # Include max value in the plot
  #------------------------------

  dev.new()
  with(EPA.09.Ex.15.1.manganese.df,
    qqPlotCensored(Manganese.ppb, Censored, dist = "gamma",
      estimate.params = TRUE, prob.method = "modified kaplan-meier",
      points.col = "blue", add.line = TRUE,
      main = paste("Gamma Q-Q Plot of Manganese Data",
          "Based on Kaplan-Meier Plotting Positions",
          "(Max Included)", sep = "\n")))

  #==========

  # Clean up
  #---------
  graphics.off()
}
\keyword{distribution}
\keyword{hplot}
