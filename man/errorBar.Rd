\name{errorBar}
\alias{errorBar}
\title{
  Plot Pointwise Error Bars
}
\description{
  Plot pointwise error bars given their upper and lower limits.    

  The \code{errorBar} function is a modified version of the S function 
  \code{error.bar}.  The \pkg{EnvStats} function \code{errorBar} includes the 
  additional arguments \code{draw.lower}, \code{draw.upper}, \code{gap.size}, 
  \code{bar.ends.size}, and \code{col} to determine whether both the lower and 
  upper error bars are drawn and to control the size of the gaps, the size of the bar 
  ends, and the color of the bars.
}
\usage{
  errorBar(x, y = NULL, lower, upper, incr = TRUE, draw.lower = TRUE, 
    draw.upper = TRUE, bar.ends = TRUE, gap = TRUE, add = FALSE, 
    horizontal = FALSE, gap.size = 0.75, bar.ends.size = 1, col = 1, 
    ..., xlab = deparse(substitute(x)), xlim, ylim)
}
\arguments{
  \item{x, y}{
  coordinates of points.  The coordinates can be given by two vector arguments or by a 
  single vector \code{x}.

  When both \code{x} and \code{y} are supplied and  \code{horizontal=FALSE} (see below), 
  \code{x} specifies where the groups will be plotted on the \eqn{x}-axis and \code{y} 
  denotes the centers for each group that will be plotted on the \eqn{y}-axis.

  When both \code{x} and \code{y} are supplied and  \code{horizontal=TRUE} (see below), 
  \code{y} specifies where the groups will be plotted on the \eqn{y}-axis and \code{x} 
  denotes the centers for each group that will be plotted on the \eqn{x}-axis.  

  If a single numeric vector is given, then \code{time(x)} is plotted on the \eqn{x}-axis 
  and \code{x} is plotted on the \eqn{y}-axis.  

  Missing values (\code{NA}s) are allowed; points containing missing values are omitted 
  from the plot.
}
  \item{lower}{
  pointwise lower limits of the error bars.  This may be a single number or a vector 
  the same length as \code{x} and \code{y}.  If \code{incr=TRUE}, then \code{lower} is 
  expected to contain the lower half widths of the error bars.  If \code{incr=FALSE}, 
  then \code{lower} contains the coordinates of the lower limits in terms of \code{x} or 
  \code{y}, depending on the orientation of the bars.
}
  \item{upper}{
  pointwise upper limits of the error bars.  This may be a single number or a vector the 
  same length as \code{x} and \code{y}.  If \code{incr=TRUE}, then \code{upper} is 
  expected to contain the upper half widths of the error bars.  If \code{incr=FALSE}, 
  then upper contains the coordinates of the upper limits in terms of \code{x} or \code{y}, 
  depending on the orientation of the bars.  If \code{upper} is missing, the upper limits 
  are drawn symmetric to the lower limits. 
}
  \item{incr}{
  logical scalar indicating whether the values in \code{lower} and \code{upper} represent 
  increments.  If \code{incr=TRUE} (the default), then \code{lower} and \code{upper} are 
  assumed to represent half the widths of the error bars (increments). 
}
  \item{draw.lower}{
  logical scalar indicating whether to draw the lower error bar.  
  The default is \code{draw.lower=TRUE}.
}
  \item{draw.upper}{
  logical scalar indicating whether to draw the upper error bar.  
  The default is \code{draw.upper=TRUE}.
}
  \item{bar.ends}{
  logical scalar indicating whether flat bars should be drawn at the endpoints.  The 
  default is \code{bar.ends=TRUE}.
}
  \item{gap}{
  logical scalar indicating whether gaps should be left around the points to emphasize 
  their locations.  The default is \code{gap=TRUE}.
}
  \item{add}{
  logical scalar indicating whether error bars should be added to the current plot.  
  If \code{add=TRUE} and a graphics device is open, the error bars are added to the 
  plot in the open device and no axes are drawn.  In this case, you should use the 
  plot parameters \code{xlim} and \code{ylim} to provide enough room for the error bars; 
  otherwise, \dQuote{Lines out of bounds} warnings are generated.  
  If \code{add=FALSE} (the default), a new coordinate system is set up for the 
  error bar plot and axes are drawn. 
}
  \item{horizontal}{
  logical scalar indicating whether the error bars should be oriented horizontally 
  (\code{horizontal=TRUE}) or vertically (\code{horizontal=FALSE}; the default). 
}
  \item{gap.size}{
  numeric scalar controlling the width of the gap.
}
  \item{bar.ends.size}{
  numeric scalar controlling the length of the bar ends.
}
  \item{col}{
  numeric or character vector indicating the color(s) of the bars.
}
  \item{xlab, xlim, ylim, \dots}{
  additional graphical parameters (see \code{\link{par}}).
}
}
\details{
  \code{errorBar} creates a plot of \code{y} versus \code{x} with pointwise error bars.  
}
\value{
  \code{errorBar} invisibly returns a list with the following components:

  \item{group.centers}{numeric vector of values on the group axis (the \eqn{x}-axis unless 
    \code{horizontal=TRUE}) indicating the centers of the groups.}
  \item{group.stats}{a matrix with the number of rows equal to the number of groups and 
    three columns indicating the group location parameter (Center), 
    the lower limit for the error bar (Lower), and 
    the upper limit for the error bar (Upper).}
}
\references{
  Cleveland, W.S. (1994). \emph{The Elements of Graphing Data}.  
  Hobart Press, Summit, New Jersey.
}
\author{
  Authors of S (for code for \code{error.bar} in S).

  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\seealso{
  \code{\link{plot}}, \code{\link{segments}}, \code{\link{pointwise}}, 
  \code{\link{stripChart}}.
}
\examples{
  # The guidance document USEPA (1994b, pp. 6.22--6.25) 
  # contains measures of 1,2,3,4-Tetrachlorobenzene (TcCB) 
  # concentrations (in parts per billion) from soil samples 
  # at a Reference area and a Cleanup area.  These data are strored 
  # in the data frame EPA.94b.tccb.df.  
  #
  # Using the log-transformed data, create 
  #
  # 1. A dynamite plot (bar plot showing mean plus 1 SE)
  #
  # 2. A confidence interval plot.

  TcCB.mat <- summaryStats(TcCB ~ Area, data = EPA.94b.tccb.df, 
      se = TRUE, ci = TRUE)
  Means <- TcCB.mat[, "Mean"]
  SEs   <- TcCB.mat[, "SE"]
  LCLs  <- TcCB.mat[, "95\%.LCL"]
  UCLs  <- TcCB.mat[, "95\%.UCL"]

  # Dynamite Plot
  #--------------
  dev.new()
  group.centers <- barplot(Means, col = c("red", "blue"), 
    ylim = range(0, Means, Means + SEs), ylab = "TcCB (ppb)", 
    main = "Dynamite Plot for TcCB Data")
  errorBar(x = as.vector(group.centers), y = Means, 
    lower = SEs, draw.lower = FALSE, gap = FALSE, 
    col = c("red", "blue"), add = TRUE)


  # Confidence Interval Plot
  #-------------------------
  xlim <- par("usr")[1:2]
  dev.new()
  errorBar(x = as.vector(group.centers), y = Means, 
    lower = LCLs, upper = UCLs, incr = FALSE, gap = FALSE, 
    col = c("red", "blue"), xlim = xlim, xaxt = "n", 
    xlab = "", ylab = "TcCB (ppb)", 
    main = "Confidence Interval Plot for TcCB Data")
  axis(1, at = group.centers, labels = dimnames(TcCB.mat)[[1]])


  # Clean up
  #---------
  rm(TcCB.mat, Means, SEs, LCLs, UCLs, group.centers, xlim)
  graphics.off()
}
\keyword{distribution}
\keyword{hplot}
