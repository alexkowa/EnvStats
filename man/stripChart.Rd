\name{stripChart}
\alias{stripChart}
\alias{stripChart.default}
\alias{stripChart.formula}
\title{
  1-D Scatter Plots with Confidence Intervals
}
\description{
  \code{stripChart} is a modification of the \R function \code{\link[graphics]{stripchart}}.  
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

\method{stripChart}{default}(x, 
    method = ifelse(paired && paired.lines, "overplot", "stack"), 
    seed = 47, jitter = 0.1 * cex, offset = 1/2, vertical = TRUE, 
    group.names, group.names.cex = cex, drop.unused.levels = TRUE, 
    add = FALSE, at = NULL, xlim = NULL, ylim = NULL, ylab = NULL, 
    xlab = NULL, dlab = "", glab = "", log = "", pch = 1, col = par("fg"), 
    cex = par("cex"), points.cex = cex, axes = TRUE, frame.plot = axes, 
    show.ci = TRUE, location.pch = 16, location.cex = cex, 
    conf.level = 0.95, min.n.for.ci = 2, 
    ci.offset = 3/ifelse(n > 2, (n-1)^(1/3), 1), ci.bar.lwd = cex, 
    ci.bar.ends = TRUE, ci.bar.ends.size = 0.5 * cex, ci.bar.gap = FALSE, 
    n.text = "bottom", n.text.line = ifelse(n.text == "bottom", 2, 0), 
    n.text.cex = cex, location.scale.text = "top", 
    location.scale.digits = 1, nsmall = location.scale.digits, 
    location.scale.text.line = ifelse(location.scale.text == "top", 0, 3.5), 
    location.scale.text.cex = 
      cex * 0.8 * ifelse(n > 6, max(0.4, 1 - (n-6) * 0.06), 1), 
    p.value = FALSE, p.value.digits = 3, p.value.line = 2, p.value.cex = cex, 
    group.difference.ci = p.value, group.difference.conf.level = 0.95, 
    group.difference.digits = location.scale.digits, 
    ci.and.test = "parametric", ci.arg.list = NULL, test.arg.list = NULL, 
    alternative = "two.sided", plot.diff = FALSE, diff.col = col[1], 
    diff.method = "stack", diff.pch = pch[1], paired = FALSE, paired.lines = paired, 
    paired.lty = 1:6, paired.lwd = 1, paired.pch = 1:14, paired.col = NULL, 
    diff.name = NULL, diff.name.cex = group.names.cex, sep.line = TRUE, 
    sep.lty = 2, sep.lwd = cex, sep.col = "gray", diff.lim = NULL, 
    diff.at = NULL, diff.axis.label = NULL, 
    plot.diff.mar = c(5, 4, 4, 4) + 0.1, ...)
}
\arguments{
  \item{x}{
  the data from which the plots are to be produced.  In the default method the data can be 
  specified as a list or data frame where each component is numeric, a numeric matrix, 
  or a numeric vector.  In the formula method, a symbolic specification of the form 
  \code{y ~ g} can be given, indicating the observations in the vector \code{y} are to be 
  grouped according to the levels of the factor \code{g} (the form \code{y ~ 1} indicates 
  no grouping).  \code{NA}s are allowed in the data.  \cr
  NOTE:  When the formula method is used and the argument \code{paired=TRUE} (see below), 
  the data in the vector \code{y} must have the same number of observations for each level 
  of the factor \code{g} and for each level sorted in the same way according to the 
  pairing variable.
}
  \item{data}{
  for the formula method, a data.frame (or list) from which the variables in \code{x} 
  should be taken.
}
  \item{subset}{
  for the formula method, an optional vector specifying a subset of observations to be 
  used for plotting.
}
  \item{na.action}{
  for the formula method, a function which indicates what should happen when the data 
  contain \code{NA}s.  The default is to ignore missing values in either the response or 
  the group.
}
  \item{\dots}{
  additional parameters passed to the default method, or by it to \code{\link{plot}}, 
  \code{\link{points}}, \code{\link{axis}}, and \code{\link{title}} to control the 
  appearance of the plot. 
}
  \item{method}{
  the method to be used to separate coincident points.  When \code{method="stack"} 
  coincident points are stacked, when \code{method="jitter"} coincident points are 
  jittered, and when \code{method="overplot"} coincident points are overplotted.  
  When there are 2 groups and \code{paired=TRUE} and \code{paired.lines=TRUE} the 
  default value is \code{method="overplot"}, otherewise the default method is 
  \code{method="stack"} (which differs from the default value for the \R function 
  \code{\link{stripchart}}, which uses \cr
  \code{method="overplot"} by default).
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
  Optional argument (forced to be a character string) explicitly providing the group labels 
  that will be printed alongside (or underneath) each plot. When \code{group.names} is 
  provided, it must have the same number of elements as the number of groups. When 
  \code{group.names} is not provided, if the groups created based on the argument 
  \code{x} have a \code{names} attribute, then that is used for the group names. 
  Otherwise, the group names are set to the integers 1 to the number of groups 
  (e.g., \code{1:3} for three groups).  
}
  \item{group.names.cex}{
  numeric scalar indicating the amount by which the group labels should be scaled 
  relative to the default (see the help file for \code{\link{plot.default}}).  
  The default is the current value of the graphics parameter \code{cex}.
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
  numeric scalar giving the amount by which the plotting characters indicating the 
  estimate of location for each group should be scaled relative to the default 
  (see the help file for \code{\link{plot.default}}).  The default is the current 
  value of the graphics parameter \code{cex}.
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
  numeric scalar or vector of length equal to the number of groups (\code{n}) in units of 
  \code{cex} indicating the amount of space between the line showing the confidence interval 
  and tick mark associated with a particular group.  The default value depends on the number of 
  groups and is given by \cr 
  \code{3/ifelse(n > 2, (n-1)^(1/3), 1)}.
}
  \item{ci.bar.lwd}{
  numeric scalar indicating the line width for the confidence interval bars.
  The default is the current value of the graphics parameter \code{cex}.
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
  numeric scalar giving the amount by which the text indicating the sample size for 
  each group should be scaled relative to the default (see the help file for \cr
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
  \item{nsmall}{
  integer passed to the function \code{\link{format}} indicating the the minimum 
  number of digits to the right of the decimal point for the estimates of location 
  and scale.  The default value is the value of \code{location.scale.digits}, which 
  forces all estimates of location and scale have the same number of digits to the 
  right of the decimal point (including, possibly, trailing zeros).  To omit trailing zeros, 
  set \code{nsmall=0}. 
}
  \item{location.scale.text.line}{
  integer indicating on which plot margin line to show the estimates of location and scale 
  for each group.  The default value is \cr
  \code{location.scale.text.line=0} when \code{n.text="top"} and \code{3.5} otherwise.
}
  \item{location.scale.text.cex}{
  numeric scalar giving the amount by which the text indicating the estimates of 
  location and scale for each group should be scaled relative to the default 
  (see the help file for \code{\link{plot.default}}).  The default depends on the 
  number of groups and is given by 
  \code{cex * 0.8 * ifelse(n > 6, max(0.4, 1 - (n-6) * 0.06), 1)}, 
  where \code{cex} denotes the current value of the graphics parameter \code{cex}.
}
  \item{p.value}{
  logical scalar indicating whether to show the p-value associated with testing whether all groups 
  have the same population location.  The default value is \code{p.value=FALSE}.  
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
  numeric scalar giving the amount by which the text indicating the p-value associated 
  with the test of equal group locations should be scaled relative to the default 
  (see the help file for \code{\link{plot.default}}).  
  The default is the current value of the graphics parameter \code{cex}.
}
  \item{group.difference.ci}{
  for the case when there are just 2 groups, a logical scalar indicating whether to display  
  the confidence interval for the difference between group locations.  The default is 
  the value of the \code{p.value} argument.  The confidence interval is displayed at the 
  top of the graph in the format [Lower CI, Upper CI].
}
  \item{group.difference.conf.level}{
  for the case when there are just 2 groups, a numeric scalar between 0 and 1 
  indicating the confidence level associated with the confidence interval for the 
  difference between group locations.  The default is \code{conf.level=0.95}.
}
  \item{group.difference.digits}{
  for the case when there are just 2 groups, an integer indicating the number of digits to 
  round to when displaying the confidence interval for the difference between group locations.  
  The default value is \cr 
  \code{group.difference.digits=location.scale.digits}.
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
  Wilcoxon rank sum test if there are two groups (see \code{\link{wilcox.test}}) and the 
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
  component \code{var.equal=TRUE}, which is not the default behavior of \code{\link{t.test}}. \cr 
  NOTE: If \code{test.arg.list} contains a component named \code{"paired"}, the value of 
  that component is set to the value of the argument \code{paired} (see below).
}
  \item{alternative}{
  character string describing the alternative hypothesis for the test of group differences in the 
  case when there are two groups.  Possible values are \code{"two.sided"} (the default), 
  \code{"less"}, and \code{"greater"}.
}
  \item{plot.diff}{
  applicable only to the case when there are two groups: \cr  
  logical scalar indicating whether to plot the confidence interval for the difference between 
  the groups.  The default is \code{plot.diff=FALSE}.

  When \code{plot.diff=TRUE} and \code{paired=FALSE}, the confidence interval 
  for the difference between the two locations is displayed and the right-hand axis 
  (when \code{vertical=TRUE}) or top axis (when \code{vertical=FALSE}) is labeled in units of 
  the confidence interval for the difference between the two locations.  If \cr
  \code{ci.and.test="parametric"}, the confidence interval for the difference between the 
  two means is displayed. 
  If \code{ci.and.test="nonparametric"}, the confidence interval for the median of the 
  difference between a sample from the first group and a sample from the second group is displayed 
  (see the help file for \code{\link{wilcox.test}}.

  When \code{plot.diff=TRUE} and \code{paired=TRUE}, the paired differences are 
  displayed and the right-hand axis (when \code{vertical=TRUE}) or 
  top axis (when \cr
  \code{vertical=FALSE}) is labeled in units of the paired differences.
  In addition, if \code{show.ci=TRUE}, the confidence interval based on the paired 
  differences is displayed.  In this case, if \code{ci.and.test="parametric"} 
  the confidence interval for the mean of the paired differences is displayed, and 
  if \cr
  \code{ci.and.test="nonparametric"} the confidence interval for the pseudomedian 
  is displayed (see the help file for \code{\link{wilcox.test}}.
}
  \item{diff.col}{
  applicable only to the case when there are two groups and \code{plot.diff=TRUE}: \cr 
  numeric or character scalar indicating what color to use for the confidence interval for 
  the difference in locations between the two groups.  When \code{paired=TRUE}, this 
  argument also controls the color of the paired differences.  
  The default is \code{diff.col=col[1]}.
}
  \item{diff.method}{
  applicable only to the case when there are two groups, \code{plot.diff=TRUE}, and \code{paired=TRUE}: \cr
  the method to be used to separate coincident points for the paired differences.  
  The default value is \code{diff.method="stack"}.  Other options are \cr
  \code{diff.method="jitter"} 
  and \code{diff.method="overplot"}.  See the explanation for the argument \code{method} above.
}
  \item{diff.pch}{
  applicable only to the case when there are two groups, \code{plot.diff=TRUE}, and \code{paired=TRUE}: \cr
  numeric or character scalar indicating what plotting symbol to use for the paired differences.  
  The default is \code{diff.pch=pch[1]}.
}
  \item{paired}{
  applicable only to the case when there are two groups: \cr
  logical scalar indicating whether the observations in the first group are paired with those 
  in the second group.  The default is \code{paired=FALSE}.  \cr
  NOTE 1:  When the formula method for the argument \code{x} is used (see above) and the 
  argument \code{paired=TRUE}, the data in the vector \code{y} must have the same number 
  of observations for each level of the factor \code{g} and for each level sorted in the 
  same way according to the pairing variable. \cr
  NOTE 2:  If the argument \code{test.arg.list} (see above) contains a component named \code{"paired"}, 
  the value of that component is set to the value of the argument \code{paired}.
}
  \item{paired.lines}{
  applicable only to the case when there are two groups and \code{paired=TRUE}: \cr
  logical scalar indicating whether to join the paired observations with lines.  
  The default value is the value of the argument \code{paired}.
}
  \item{paired.lty}{
  applicable only to the case when there are two groups, \code{paired=TRUE}, and \cr
  \code{paired.lines=TRUE}: \cr 
  numeric vector indicating the line types to use to join the paired observations with lines.  
  The default value is \code{paired.lty=1:6}.
}
  \item{paired.lwd}{
  applicable only to the case when there are two groups, \code{paired=TRUE}, and \cr
  \code{paired.lines=TRUE}: \cr 
  numeric vector indicating the widths of the lines used to join the paired observations with lines.  
  The default value is \code{paired.lwd=1}.
}
  \item{paired.pch}{
  applicable only to the case when there are two groups, \code{paired=TRUE}, and \cr
  \code{paired.lines=TRUE}: \cr
  numeric vector indicating the plotting characters to use at each end of the lines used to 
  join the paired observations with lines.  The default value is \cr
  \code{paired.pch=1:14}.
}
  \item{paired.col}{
  applicable only to the case when there are two groups, \code{paired=TRUE}, and \cr
  \code{paired.lines=TRUE}: \cr 
  character or numeric vector indicating the colors for the lines (and plotting characters) 
  used to join the paired observations with lines.  The default value is \code{paired.col=NULL}, 
  in which case the vector becomes \cr
  \code{c("black", "red", "green3", "blue", "magenta", "darkgreen",} \cr 
  \code{"purple", "orange", "darkolivegreen", "steelblue", "darkgray")}.
}
  \item{diff.name}{
  applicable only to the case when there are two groups and \code{plot.diff=TRUE}: \cr 
  character scalar indicating the label to use for the confidence interval for the 
  difference between groups.  For the case when \code{paired=TRUE}, this label also 
  describes the paired differences.  The default value is \code{diff.name=NULL}, 
  in which case the label is "group 2 - group 1", where group 1 and group 2 denote the names for 
  the each group.  For example, if group 1 is labeled "A" and group 2 is labeled "B", then 
  the default value is \code{diff.name="B-A"}.
}
  \item{diff.name.cex}{
  applicable only to the case when there are two groups and \code{plot.diff=TRUE}: \cr 
  numeric scalar indicating the amount by which the label for group differences 
  should be scaled relative to the default (see the help file for \code{\link{plot.default}}).  
  The default value is \code{diff.name.cex=group.names.cex}.
}
  \item{sep.line}{
  applicable only to the case when there are two groups and \code{plot.diff=TRUE}: \cr 
  logical scalar indicating whether to draw a line between the strip charts for the two 
  groups and the confidence interval for the difference between the two groups (and paired 
  differences when \code{paired=TRUE}).  The default value is \cr
  \code{sep.line=TRUE}.
}
  \item{sep.lty}{
  applicable only to the case when there are two groups, \code{plot.diff=TRUE}, and 
  \code{sep.line=TRUE}: \cr 
  numeric scalar indicating the line type to use for the line drawn between the strip charts 
  for the two groups and the confidence interval for the difference between the two groups.   
  The default value is \code{sep.lty=2}.
}
  \item{sep.lwd}{
  applicable only to the case when there are two groups, \code{plot.diff=TRUE}, and 
  \code{sep.line=TRUE}: \cr 
  numeric scalar indicating the line width to use for the line drawn between the strip charts 
  for the two groups and the confidence interval for the difference between the two groups.   
  The default value is the current value of the graphics parameter \code{cex}.
}
  \item{sep.col}{
  applicable only to the case when there are two groups, \code{plot.diff=TRUE}, and 
  \code{sep.line=TRUE}: \cr 
  numeric or character scalar indicating the color of the line drawn between the strip charts 
  for the two groups and the confidence interval for the difference between the two groups.   
  The default value is \code{sep.col="gray"}.
}
  \item{diff.lim}{
  applicable only to the case when there are two groups and \code{plot.diff=TRUE}: \cr 
  numeric vector of length 2 indicating the limits to use for the axis associated with the 
  confidence interval for the difference between the two groups.  
  When \code{paired=FALSE}, the default value is the range of the y-axis, but centered at 
  the mean of the confidence interval for the difference in locations.  
  When \cr
  \code{paired=TRUE}, the default value is \code{range(pretty(c(X, range(CI))))} where 
  \code{X} denotes the vector containing the paired differences.
}
  \item{diff.at}{
  applicable only to the case when there are two groups and \code{plot.diff=TRUE}: \cr 
  numeric vector indicating the locations of the tick marks for the axis associated with the 
  confidence interval for the difference between groups (see the explanation for the argument 
  \code{at} in the help file for \code{\link{axis}}).  The default value is \code{diff.at=NULL}, 
  in which case default values are used for the locations of the tick marks.
}
  \item{diff.axis.label}{
  applicable only to the case when there are two groups and \code{plot.diff=TRUE}: \cr 
  character string indicating the label to use for the axis associated with the 
  confidence interval for the difference between groups.  When \code{paired=FALSE} the 
  default value is \code{"Difference Between Groups"}, and when \code{paired=TRUE} the 
  default value is \code{"Paired Difference"}.
}
  \item{plot.diff.mar}{
  applicable only to the case when there are two groups, \code{plot.diff=TRUE}, and \code{add=FALSE}: \cr 
  numeric vector of length 4 indicating the number of lines in the plotting margins 
  (see the explanation for the argument \code{mar} in the help file for \code{\link{par}}). 
  The default value is \code{plot.diff.mar = c(5, 4, 4, 4) + 0.1}.
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

In addition, if the argument \code{p.value=TRUE} and/or 1) there are two groups and 2) \code{plot.diff=TRUE}, 
the list also includes these components:

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
  #------------------------
  # Two Independent Samples
  #------------------------

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
  stripChart(TcCB ~ Area, data = EPA.94b.tccb.df, col = c("red", "blue"), 
    p.value = TRUE, ci.and.test = "nonparametric", 
    ylab = "TcCB (ppb)")

  #----------

  # Now log-transform the TcCB data and use a parametric test
  # to compare the areas.

  dev.new()
  stripChart(log10(TcCB) ~ Area, data = EPA.94b.tccb.df, col = c("red", "blue"), 
    p.value = TRUE, ylab = "log10 [ TcCB (ppb) ]")

  #----------

  # Repeat the above procedure, but also plot the confidence interval  
  # for the difference between the means.

  dev.new()
  stripChart(log10(TcCB) ~ Area, data = EPA.94b.tccb.df, col = c("red", "blue"), 
    p.value = TRUE, plot.diff = TRUE, diff.col = "black", 
    ylab = "log10 [ TcCB (ppb) ]")

  #----------

  # Repeat the above procedure, but allow the variances to differ.

  dev.new()
  stripChart(log10(TcCB) ~ Area, data = EPA.94b.tccb.df, col = c("red", "blue"),
    p.value = TRUE, plot.diff = TRUE, diff.col = "black", 
    ylab = "log10 [ TcCB (ppb) ]", test.arg.list = list(var.equal = FALSE))

  #----------

  # Repeat the above procedure, but jitter the points instead of 
  # stacking them.

  dev.new()
  stripChart(log10(TcCB) ~ Area, data = EPA.94b.tccb.df, col = c("red", "blue"),
    p.value = TRUE, plot.diff = TRUE, diff.col = "black", 
    ylab = "log10 [ TcCB (ppb) ]", test.arg.list = list(var.equal = FALSE), 
    method = "jitter", ci.offset = 4)

  #---------- 

  # Clean up
  #---------
  graphics.off()

  #====================

  #--------------------
  # Paired Observations
  #--------------------

  # The data frame ACE.13.TCE.df contians paired observations of 
  # trichloroethylene (TCE; mg/L) at 10 groundwater monitoring wells 
  # before and after remediation.
  #
  # Create one-dimensional scatterplots to compare TCE concentrations 
  # before and after remediation and use a paired t-test to 
  # test for a difference between periods.

  ACE.13.TCE.df
  #   TCE.mg.per.L Well Period
  #1        20.900    1 Before
  #2         9.170    2 Before
  #3         5.960    3 Before
  #...      ......   .. ......
  #18        0.520    8  After
  #19        3.060    9  After
  #20        1.900   10  After

  dev.new()
  stripChart(TCE.mg.per.L ~ Period, data = ACE.13.TCE.df, 
    col = c("brown", "green"), p.value = TRUE, paired = TRUE, 
    ylab = "TCE (mg/L)")

  #----------

  # Repeat the above procedure, but also plot the confidence interval  
  # for the mean of the paired differences.

  dev.new()
  stripChart(TCE.mg.per.L ~ Period, data = ACE.13.TCE.df, 
    col = c("brown", "green"), p.value = TRUE, paired = TRUE, 
    ylab = "TCE (mg/L)", plot.diff = TRUE, diff.col = "blue")


  #==========

  # Repeat the last two examples, but use a one-sided alternative since 
  # remediation should decrease TCE concentration.

  dev.new()
  stripChart(TCE.mg.per.L ~ Period, data = ACE.13.TCE.df, 
    col = c("brown", "green"), p.value = TRUE, paired = TRUE, 
    ylab = "TCE (mg/L)", alternative = "less", 
    group.difference.digits = 2)

  #----------

  # Repeat the above procedure, but also plot the confidence interval  
  # for the mean of the paired differences.
  #
  # NOTE: Although stripChart can *report* one-sided confidence intervals 
  #       for the difference between two groups (see above example), 
  #       when *plotting* the confidence interval for the difference, 
  #       only two-sided CIs are allowed.  
  #       Here, we will set the confidence level of the confidence 
  #       interval for the mean of the paired differences to 90%, 
  #       so that the upper bound of the CI corresponds to the upper 
  #       bound of a 95% one-sided CI.

  dev.new()
  stripChart(TCE.mg.per.L ~ Period, data = ACE.13.TCE.df, 
    col = c("brown", "green"), p.value = TRUE, paired = TRUE, 
    ylab = "TCE (mg/L)", group.difference.digits = 2, 
    plot.diff = TRUE, diff.col = "blue", group.difference.conf.level = 0.9)

 #---------- 

  # Clean up
  #---------
  graphics.off()

  #==========

  # The data frame Helsel.Hirsch.02.Mayfly.df contains paired counts
  # of mayfly nymphs above and below industrial outfalls in 12 streams.  
  #
  # Create one-dimensional scatterplots to compare the 
  # counts between locations and use a nonparametric test 
  # to compare counts above and below the outfalls.

  Helsel.Hirsch.02.Mayfly.df
  #   Mayfly.Count Stream Location
  #1            12      1    Above
  #2            15      2    Above
  #3            11      3    Above
  #...         ...     ..    .....
  #22           60     10    Below
  #23           53     11    Below
  #24          124     12    Below

  dev.new()
  stripChart(Mayfly.Count ~ Location, data = Helsel.Hirsch.02.Mayfly.df, 
    col = c("green", "brown"), p.value = TRUE, paired = TRUE, 
    ci.and.test = "nonparametric", ylab = "Number of Mayfly Nymphs")

  #---------- 
 
  # Repeat the above procedure, but also plot the confidence interval  
  # for the pseudomedian of the paired differences.

  dev.new()
  stripChart(Mayfly.Count ~ Location, data = Helsel.Hirsch.02.Mayfly.df, 
    col = c("green", "brown"), p.value = TRUE, paired = TRUE, 
    ci.and.test = "nonparametric", ylab = "Number of Mayfly Nymphs", 
    plot.diff = TRUE, diff.col = "blue")

 #---------- 

  # Clean up
  #---------
  graphics.off()
}
\keyword{hplot}
\keyword{htest}
