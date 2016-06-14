\name{plot.boxcox}
\alias{plot.boxcox}
\title{
  Plot Results of Box-Cox Transformations
}
\description{
  Plot the results of calling the function \code{\link{boxcox}}, which returns an 
  object of class \code{"boxcox"}.  Three different kinds of plots are available.

  The function \code{plot.boxcox} is automatically called by \code{\link{plot}} 
  when given an object of class \code{"boxcox"}.  The names of other functions 
  associated with Box-Cox transformations are listed under \link{Data Transformations}.
}
\usage{
\method{plot}{boxcox}(x, plot.type = "Objective vs. lambda", same.window = TRUE, 
    ask = same.window & plot.type != "Ojective vs. lambda", 
    plot.pos.con = 0.375, estimate.params = FALSE, 
    equal.axes = qq.line.type == "0-1" || estimate.params, add.line = TRUE, 
    qq.line.type = "least squares", duplicate.points.method = "standard", 
    points.col = 1, line.col = 1, line.lwd = par("cex"), line.lty = 1, 
    digits = .Options$digits, cex.main = 1.4 * par("cex"), cex.sub = par("cex"), 
    main = NULL, sub = NULL, xlab = NULL, ylab = NULL, xlim = NULL, 
    ylim = NULL, ...)
}
\arguments{
  \item{x}{
  an object of class \code{"boxcox"}.  See \code{\link{boxcox.object}} for details.
}
  \item{plot.type}{
  character string indicating what kind of plot to create.  Only one particular 
  plot type will be created, unless \code{plot.type="All"}, in which case all plots 
  will be created sequentially.  The possible values of \code{plot.type} are: \cr
  \code{"Objective vs. lambda"} (the default), \cr
  \code{"Q-Q Plots"}, \cr
  \code{"Tukey M-D Q-Q Plots"}, and \cr
  \code{"All"}.
}
  \item{same.window}{
  logical scalar indicating whether to produce all plots in the same graphics 
  window (\code{same.window=TRUE}; the default), or to create a new graphics 
  window for each separate plot (\code{same.window=FALSE}).  The argument is 
  relevant only when \code{plot.type} produces more than one plot (i.e., when 
  \code{plot.type} is not equal to \code{"Objective vs. lambda"}).
}
  \item{ask}{
  logical scalar supplied to the function \code{\link{devAskNewPage}}, indicating 
  whether to prompt the user before creating a new plot within a single graphics 
  window.  This argument is ignored when \code{plot.type="Objective vs. lambda"} 
  (since only one plot is produced) or when \code{same.window=FALSE}, otherwise  
  the default value is \code{ask=TRUE}.
}
  \item{points.col}{
  numeric scalar determining the color of the points in the plot.  The default 
  value is \code{points.col=1}. See the entry for \code{col} in the \R help file for 
  \code{\link{par}} for more information. \cr
}

\bold{The following arguments can be supplied when \code{plot.type="Q-Q Plots"},} \cr
\bold{\code{plot.type="Tukey M-D Q-Q Plots"}, or \code{plot.type="All"} 
(supplied to \code{\link{qqPlot}})}: 

  \item{plot.pos.con}{
  numeric scalar between 0 and 1 containing the value of the plotting position 
  constant used to construct the Q-Q plots and/or Tukey Mean-Difference Q-Q plots.  
  The default value is \code{plot.pos.con=0.375}.  See the help files for 
  \code{\link{qqPlot}} for more information and the motivation for this choice.
}
  \item{estimate.params}{
  logical scalar indicating whether to compute quantiles based on estimating the 
  distribution parameters (\code{estimate.params=TRUE}) or using the 
  distribution parameters for a standard normal distribution (i.e, 
  \code{mean=0}, \code{sd=1}).  The default value is \code{estimate.params=FALSE} 
  because a standard normal Q-Q plot will yield roughly a straight line if the 
  observations are from \emph{any} normal distribution.  If you specify 
  \code{plot.type="Tukey M-D Q-Q Plots"}, then you need to set 
  \code{estiamte.params=TRUE} unless you want to assume the transformed data come 
  from a standard normal distribution.
}
  \item{equal.axes}{
  logical scalar indicating whether to use the same range on the \eqn{x}- and 
  \eqn{y}-axes when \code{plot.type="Q-Q Plots"}.  The default value is 
  \code{TRUE} if \cr
  \code{qq.line.type="0-1"} or \code{estimate.params=TRUE}, 
  otherwise it is \code{FALSE}.
}
  \item{add.line}{
  logical scalar indicating whether to add a line to the plot.  If \code{add.line=TRUE} 
  and \code{plot.type="Q-Q Plots"}, a line determined by the value of 
  \code{qq.line.type} is added to the plot.  If \code{add.line=TRUE} and 
  \code{plot.type="Tukey M-D Q-Q Plots"}, a horizontal line at \eqn{y=0} is added to 
  the plot.  The default value is \code{add.line=TRUE}.
}
  \item{qq.line.type}{
  character string determining what kind of line to add to the plot when \cr
  \code{plot.type="Q-Q Plots"}.  Possible values are: \cr
  \code{"least squares"} (a least squares line; the default), \cr 
  \code{"0-1"} (a line with intercept 0 and slope 1), and \cr
  \code{"robust"} (a line is fit through the first and third quartiles of the 
  \eqn{x} and \eqn{y} data).  \cr
  This argument is ignored if \code{add.line=FALSE}.
}
  \item{duplicate.points.method}{
  a character string denoting how to plot points with duplicate \eqn{(x,y)} values.  
  Possible values are \code{"standard"} (a single plotting symbol is plotted; 
  the default), \code{"jitter"} (a separate plotting symbol is plotted for 
  each duplicate point, where the plotting symbols cluster around the true value 
  of \eqn{x} and \eqn{y}), and \code{"number"} (a single number is plotted at 
  \eqn{(x,y)} that represents how many duplicate points are at that \eqn{(x,y)} 
  coordinate).
}
  \item{line.col}{
  numeric scalar determining the color of the line in the plot.  The default value 
  is \code{line.col=1}.  See the entry for \code{col} in the \R help file for 
  \code{\link{par}} for more information.  This argument is ignored if 
  \code{add.line=FALSE}.
}
  \item{line.lwd}{
  numeric scalar determining the width of the line in the plot.  The default value 
  is \code{line.lwd=par("cex")}.  See the entry for \code{lwd} in the \R help file for 
  \code{\link{par}} for more information.  This argument is ignored if 
  \code{add.line=FALSE}.
}
  \item{line.lty}{
  numeric scalar determining the line type (style) of the line in the plot.  
  The default value is \code{line.lty=1}.  See the entry for \code{lty} 
  in the \R help file for \code{\link{par}} for more information.  
  This argument is ignored if \code{add.line=FALSE}.
}
  \item{digits}{
  scalar indicating how many significant digits to print for the distribution 
  parameters and the value of the objective in the sub-title.  The default 
  value is the current setting of \code{options("digits")}. \cr
}


\bold{Graphics parameters:}
  \item{cex.main, cex.sub, main, sub, xlab, ylab, xlim, ylim, \dots}{
  graphics parameters; see \code{\link{par}} for more information.  The 
  default value of \code{cex.main} is \code{cex.main=1.4 * par("cex")}.  
  The default value of \code{cex.sub} is \cr
  \code{cex.sub=par("cex")}.
}
}
\details{
  The function \code{plot.boxcox} is a method for the generic function 
  \code{\link{plot}} for the class \code{"boxcox"} (see \code{\link{boxcox.object}}).  
  It can be invoked by calling \code{\link{plot}} and giving it an object of 
  class \code{"boxcox"} as the first argument, or by calling \code{plot.boxcox} 
  directly, regardless of the class of the object given as the first argument 
  to \code{plot.boxcox}.

  Plots associated with Box-Cox transformations are produced on the current graphics 
  device.  These can be one or all of the following:
  \itemize{
  \item Objective vs. \eqn{\lambda}.
  \item Observed Quantiles vs. Normal Quantiles (Q-Q Plot) for the transformed 
  observations for each of the values of \eqn{\lambda}.
  \item Tukey Mean-Difference Q-Q Plots for the transformed observations for each 
  of the values of \eqn{\lambda}.
  }
  See the help files for \code{\link{boxcox}} and \code{\link{qqPlot}} for more 
  information.
}
\value{
  \code{plot.boxcox} invisibly returns the first argument, \code{x}.
}
\references{
  Chambers, J. M. and Hastie, T. J. (1992).  \emph{Statistical Models in S}.  
  Wadsworth & Brooks/Cole. 
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\seealso{
  \code{\link{qqPlot}}, \code{\link{boxcox}}, \code{\link{boxcox.object}}, 
  \code{\link{print.boxcox}}, \link{Data Transformations}, \code{\link{plot}}.
}
\examples{
  # Generate 30 observations from a lognormal distribution with 
  # mean=10 and cv=2, call the function boxcox, and then plot 
  # the results.  
  # (Note: the call to set.seed simply allows you to reproduce this example.)

  set.seed(250) 
  x <- rlnormAlt(30, mean = 10, cv = 2) 

  # Plot the results based on the PPCC objective
  #---------------------------------------------
  boxcox.list <- boxcox(x)
  dev.new()
  plot(boxcox.list)

  # Look at Q-Q Plots for the candidate values of lambda
  #-----------------------------------------------------
  plot(boxcox.list, plot.type = "Q-Q Plots", same.window = FALSE) 


  # Look at Tukey Mean-Difference Q-Q Plots 
  # for the candidate values of lambda
  #----------------------------------------
  plot(boxcox.list, plot.type = "Tukey M-D Q-Q Plots", same.window = FALSE) 

  #==========

  # Clean up
  #---------
  rm(x, boxcox.list)
  graphics.off()
}
\keyword{plot}
