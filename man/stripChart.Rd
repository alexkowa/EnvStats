\name{stripChart}
\alias{stripChart}
\alias{stripChart.default}
\alias{stripChart.formula}
\title{
  1-D Scatter Plots with Confidence Intervals
}
\description{
  \code{stripChart} is a modification of the \R function \code{\link{stripchart}}.  
  It is a generic function used to produce one dimensional scatter 
  plots (or dot plots) of the given data, along with text indicating sample size and 
  estimates of location (mean or median) and scale (standard deviation 
  or interquartile range), as well as confidence intervals for the population 
  location parameter.  
  One dimensional scatterplots are a good alternative to \code{\link[=boxplot]{boxplots}} 
  when sample sizes are small or moderate.  The function invokes particular 
  \code{\link{methods}} which depend on the \code{\link{class}} of the first argument. 
}
\usage{
stripChart(x, ...)

\method{stripChart}{formula}(x, data = NULL, dlab = NULL, 
    subset, na.action = NULL, ...)

\method{stripChart}{default}(x, method = "stack", seed = 47, 
  jitter = 0.1 * cex, offset = 1/2, vertical = TRUE, group.names, 
  drop.unused.levels = TRUE, add = FALSE, at = NULL, 
  xlim = NULL, ylim = NULL, ylab = NULL, xlab = NULL, 
  dlab = "", glab = "", log = "", pch = 1, col = par("fg"), 
  cex = par("cex"), points.cex = cex, axes = TRUE, frame.plot = axes, 
  show.ci = TRUE, location.pch = 16, location.cex = cex, 
  conf.level = 0.95, min.n.for.ci = 2, 
  ci.offset = 3/ifelse(n > 2, (n-1)^(1/3), 1), 
  ci.bar.ends = TRUE, ci.bar.ends.size = 0.5 * cex, 
  ci.bar.gap = FALSE, n.text = "bottom", 
  n.text.line = ifelse(n.text == "bottom", 2, 0), 
  n.text.cex = cex, location.scale.text = "top", 
  location.scale.digits = 1, location.scale.text.line = 
    ifelse(location.scale.text == "top", 0, 3.5), 
  location.scale.text.cex = 
    cex * 0.8 * ifelse(n > 6, max(0.4, 1 - (n-6) * 0.06), 1), 
  p.value = FALSE, p.value.digits = 3, p.value.line = 2, 
  p.value.cex = cex, group.difference.ci = p.value, 
  group.difference.conf.level = 0.95, 
  group.difference.digits = location.scale.digits, 
  ci.and.test = "parametric", ci.arg.list = NULL, 
  test.arg.list = NULL, alternative = "two.sided", ...)
}
\arguments{
  \item{x}{
  the data from which the plots are to be produced.  In the default method the data can be 
  specified as a single numeric vector, or as a list of numeric vectors, each corresponding 
  to a component plot.  In the formula method, a symbolic specification of the form \code{y ~ g} 
  can be given, indicating the observations in the vector \code{y} are to be grouped 
  according to the levels of the factor \code{g} (the form \code{y ~ 1} indicates no grouping).  
  \code{NA}s are allowed in the data.
}
  \item{data}{
  a data.frame (or list) from which the variables in \code{x} should be taken.
}
  \item{subset}{
  an optional vector specifying a subset of observations to be used for plotting.
}
  \item{na.action}{
  a function which indicates what should happen when the data contain \code{NA}s. 
  The default is to ignore missing values in either the response or the group.
}
  \item{\dots}{
  additional parameters passed to the default method, or by it to \code{\link{plot}}, 
  \code{\link{points}}, \code{\link{axis}}, and \code{\link{title}} to control the 
  appearance of the plot. 
}
  \item{method}{
  the method to be used to separate coincident points.  The method \code{"overplot"} 
  causes such points to be overplotted, but it is also possible to specify 
  \code{"jitter"} to jitter the points, or \code{"stack"} to 
  have coincident points stacked (the default).  Note that the default value of 
  \code{method} (\code{method="stack"}) differs from the default value for the \R function 
  \code{\link{stripchart}}, which uses \code{method="overplot"} by default.
}
  \item{seed}{
  when \code{method="jitter"} is used, the argument \code{seed} is passed to 
  the \R function \code{\link{set.seed}}.  Since jittering depends on the 
  \R random number generator, using the same value of \code{seed} each time 
  the same data are plotted with \code{stripChart} ensures that the resulting 
  plot is the same.
}
  \item{jitter}{
  when \code{method="jitter"} is used, \code{jitter} gives the amount of jittering applied.
}
  \item{offset}{
  when stacking is used, points are stacked this many line-heights (symbol widths) apart.
}
  \item{vertical}{
  when \code{vertical=TRUE} (the default), the plots are drawn vertically rather than horizontally.
}
  \item{group.names}{
  group labels which will be printed alongside (or underneath) each plot.
}
  \item{drop.unused.levels}{
  when \code{drop.unused.levels=TRUE}, groups with no observations are dropped.
}
  \item{add}{
  logical, if true \emph{add} the chart to the current plot.
}
  \item{at}{
  numeric vector giving the locations where the charts should be drawn, 
  particularly when \code{add=TRUE}; defaults to \code{1:n} where \code{n} 
  is the number of groups.
}
  \item{xlim, ylim}{
  plot limits: see \code{\link{plot.window}}.
}
  \item{ylab, xlab}{
  labels: see \code{\link{title}}.
}
  \item{dlab, glab}{
  alternate way to specify axis labels.  The \code{dlab} and \code{glab} labels may be used 
  instead of \code{xlab} and \code{ylab} if those are not specified.  \code{dlab} applies 
  to the continuous data axis (the \eqn{y}-axis unless \code{vertical=FALSE}), 
  and \code{glab} to the group axis.
}
  \item{log}{
  on which axes to use a log scale: see \code{\link{plot.default}}.
}
  \item{pch, col, cex}{
  Graphical parameters: see \code{\link{par}}.
}
  \item{points.cex}{
  Sets the \code{cex} value for the points plotted.
}
  \item{axes, frame.plot}{
  Axis control: see \code{\link{plot.default}}.
}
  \item{show.ci}{
  logical scalar indicating whether to plot the confidence interval.  The default is 
  \code{show.ci=TRUE}.
}
  \item{location.pch}{
  integer indicating which plotting character to use to indicate the estimate of location 
  (mean or median) for each group (see the help file for \code{\link{plot.default}}).  
  The default is \code{location.pch=16}, a filled circle.
}
  \item{location.cex}{
  integer giving the amount by which the plotting characters indicating the estimate of 
  location for each group should be scaled relative to the default 
  (see the help file for \code{\link{plot.default}}).  The default is the current value of 
  the graphics parameter \code{cex}.
}
  \item{conf.level}{
  numeric scalar between 0 and 1 indicating the confidence level associated with the 
  confidence interval for the group location (population mean or median).  
  The default value is \code{conf.level=0.95}.
}
  \item{min.n.for.ci}{
  integer indicating the minimum sample size required in order to plot a confidence interval 
  for the group location.  The default value is \code{min.n.for.ci=2}.
}
  \item{ci.offset}{
  numeric scalar in units of \code{cex} indicating the amount of space between the line 
  showing the confidence interval and the center of the plotted points for a group.  The 
  default value depends on the number of groups and is given by 
  \code{3/ifelse(n > 2, (n-1)^(1/3), 1)}.
}
  \item{ci.bar.ends}{
  logical scalar indicating whether to add flat ends to the confidence interval bars.  
  The default value is \code{ci.bar.ends=TRUE}.
}
  \item{ci.bar.ends.size}{
  numeric scalar in units of \code{cxy} indicating the size of confidence interval 
  bar ends.  The default value is half of the current value of \code{cex}.
}
  \item{ci.bar.gap}{
  logical scalar indicating with to add a gap between the estimate of group location and the 
  confidence interval bar.  The default value is \code{ci.bar.gap=FALSE}.
}
  \item{n.text}{
  character string indicating whether and where to indicate the sample size for each group.  
  Possible values are \code{"bottom"} (the default), \code{"top"}, and \code{"none"}.
}
  \item{n.text.line}{
  integer indicating on which plot margin line to show the sample sizes for each group.  The 
  default value is \code{n.text.line=2} when \code{n.text="bottom"} and \code{0} otherwise.
}
  \item{n.text.cex}{
  integer giving the amount by which the text indicating the sample size for each group 
  should be scaled relative to the default (see the help file for \cr
  \code{\link{plot.default}}).  The default is the current value of the graphics 
  parameter \code{cex}.
}
  \item{location.scale.text}{
  character string indicating whether and where to indicate the estimates of location 
  (mean or median) and scale (standard deviation or interquartile range) for each group.  
  Possible values are \code{"top"} (the default), \code{"bottom"}, and \code{"none"}.
}
  \item{location.scale.digits}{
  integer indicating the number of digits to round the estimates of location and scale.  The 
  default value is \code{location.scale.digits=1}.
}
  \item{location.scale.text.line}{
  integer indicating on which plot margin line to show the estimates of location and scale 
  for each group.  The default value is \cr
  \code{location.scale.text.line=0} when \code{n.text="top"} and \code{3.5} otherwise.
}
  \item{location.scale.text.cex}{
  integer giving the amount by which the text indicating the estimates of location and scale 
  for each group should be scaled relative to the default (see the help file for 
  \code{\link{plot.default}}).  The default depends on the number of groups and is 
  given by 
  \code{cex * 0.8 * ifelse(n > 6, max(0.4, 1 - (n-6) * 0.06), 1)}.  
  where \code{cex} denotes the current value of the graphics parameter \code{cex}.
}
  \item{p.value}{
  logical scalar indicating whether to show the p-value associated with testing whether all groups 
  have the same population location.  The default value is \code{p.value=TRUE}.  
  The p-value is displayed at the top of the graph.
}
  \item{p.value.digits}{
  integer indicating the number of digits to round to when displaying the p-value associated with 
  the test of equal group locations.  The default value is \cr
  \code{p.value.digits=3}.
}
  \item{p.value.line}{
  integer indicating on which plot margin line to show the p-value associated with the test of 
  equal group locations.  The default value is \code{p.value.line=2}.
}
  \item{p.value.cex}{
  integer giving the amount by which the text indicating the p-value associated with the test of 
  equal group locations should be scaled relative to the default 
  (see the help file for \code{\link{plot.default}}).  
  The default is the current value of the graphics parameter \code{cex}.
}
  \item{group.difference.ci}{
  for the case when there are just 2 groups, a logical scalar indicating whether to show 
  the confidence interval for the difference between group locations.  The default is 
  the value of the \code{p.value} argument.  The confidence interval is displayed at the 
  top of the graph.
}
  \item{group.difference.conf.level}{
  for the case when there are just 2 groups, a numeric scalar between 0 and 1 
  indicating the confidence level associated with the confidence interval for the 
  difference between group locations.  The default is \code{conf.level=0.95}.
}
  \item{group.difference.digits}{
  for the case when there are just 2 groups, an integer indicating the number of digits to 
  round to when displaying the confidence interval for the difference between group locations.  
  The default value is \code{group.difference.digits=location.scale.digits}.
}
  \item{ci.and.test}{
  character string indicating whether confidence intervals and tests should be based on parametric 
  or nonparametric (\code{ci.and.test="nonparametric"}) methods.  
  When \code{ci.and.test="parametric"} (the default), confidence intervals for the population mean 
  are based on the one-sample t-test (see \code{\link{t.test}}), and the test of group 
  differences is based on the two-sample t-test if there are two groups and the F-test 
  (i.e., one-way analysis of variance, see \code{\link{aov}}) if there are three or more groups.  
  When \code{ci.and.test="nonparametric"}, confidence intervals for the population pseudo-median 
  are based on the Wilcoxon signed rank test (see \code{\link{wilcox.test}} and page 56 of 
  Hollander and Wolfe, 1999), and the test of group differences is based on the 
  Wilcoxon rank sum test if there are two groups (see \code{\link{kruskal.test}}) and the 
  Kruskal-Wallis test (see \code{\link{kruskal.test}}) if there are three or more groups.
}
  \item{ci.arg.list}{
  an optional list of arguments to pass to the function used to compute confidence intervals.  
  The default value is \code{ci.arg.list=NULL}.
}
  \item{test.arg.list}{
  an optional list of arguments to pass to the function used to test for group differences in location.  
  The default value is \code{test.arg.list=NULL}.  In particular, in the case when there are two groups, 
  \code{ci.and.test="parametric"}, and \code{ci.arg.list} is \code{NULL} or does not contain a 
  component specifying the value for \code{var.equal}, this argument is updated to include the 
  component \code{var.equal=TRUE}, which is not the default behavior of \code{\link{t.test}}.
}
  \item{alternative}{
  character string describing the alternative hypothesis for the test of group differences in the 
  case when there are two groups.  Possible values are \code{"two.sided"} (the default), 
  \code{"less"}, and \code{"greater"}.
}
}
\value{
  \code{stripChart} invisibly returns a list with the following components:

  \item{group.centers}{numeric vector of values on the group axis (the \eqn{x}-axis unless 
    \code{vertical=FALSE}) indicating the centers of the groups.}
  \item{group.stats}{a matrix with the number of rows equal to the number of groups and 
    six columns indicating the sample size of the group (N), the estimate of the group 
    location parameter (Mean or Median), the estimate of the group scale (SD or IQR), 
    the lower confidence limit for the group location parameter (LCL), 
    the upper confidence limit for the group location parameter (UCL), and the 
    confidence level associated with the confidence interval (Conf.Level)}
  \item{group.difference.p.value}{numeric scalar indicating the p-value associated with 
    the test of equal group locations.}
  \item{group.difference.conf.int}{numeric vector of two elements indicating the confidence 
    interval for the difference between the group locations.  Only present when there are 
    two groups.}
}
\references{
  Hollander, M., and D.A. Wolfe. (1999). \emph{Nonparametric Statistical Methods}.  
  Second Edition.  John Wiley and Sons, New York.

  Millard, S.P., and N.K. Neerchal. (2001). \emph{Environmental Statistics with S-PLUS}. 
  CRC Press, Boca Raton, FL.

  Zar, J.H. (2010). \emph{Biostatistical Analysis}. Fifth Edition. 
  Prentice-Hall, Upper Saddle River, NJ.
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\seealso{
  \code{\link{stripchart}}, \code{\link{t.test}}, \code{\link{wilcox.test}}, 
  \code{\link{aov}}, \code{\link{kruskal.test}}, \code{\link{t.test}}. 
}
\examples{
  # The guidance document USEPA (1994b, pp. 6.22--6.25) 
  # contains measures of 1,2,3,4-Tetrachlorobenzene (TcCB) 
  # concentrations (in parts per billion) from soil samples 
  # at a Reference area and a Cleanup area.  These data are strored 
  # in the data frame EPA.94b.tccb.df.  
  #
  # First create one-dimensional scatterplots to compare the 
  # TcCB concentrations between the areas and use a nonparametric 
  # test to test for a difference between areas.

  dev.new()
  stripChart(TcCB ~ Area, data = EPA.94b.tccb.df, 
    p.value = TRUE, ci.and.test = "nonparametric", 
    ylab = "TcCB (ppb)")

  #----------

  # Now log-transform the TcCB data and use a parametric test
  # to compare the areas.

  dev.new()
  stripChart(log10(TcCB) ~ Area, data = EPA.94b.tccb.df, 
    p.value = TRUE, ci.and.test = "parametric", 
    ylab = "log10 [ TcCB (ppb) ]")

  #----------

  # Repeat the above procedure, but allow the variances to differ.

  dev.new()
  stripChart(log10(TcCB) ~ Area, data = EPA.94b.tccb.df, 
    p.value = TRUE, ci.and.test = "parametric", 
    ylab = "log10 [ TcCB (ppb) ]", 
    test.arg.list = list(var.equal = FALSE))

  #----------

  # Repeat the above procedure, but jitter the points instead of 
  # stacking them.

  dev.new()
  stripChart(log10(TcCB) ~ Area, data = EPA.94b.tccb.df, 
    p.value = TRUE, ci.and.test = "parametric", 
    ylab = "log10 [ TcCB (ppb) ]", 
    test.arg.list = list(var.equal = FALSE), 
    method = "jitter", ci.offset = 4)

  #==========

  # Clean up
  #---------
  graphics.off()
}
\keyword{hplot}
\keyword{htest}
