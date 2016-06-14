\name{plot.gofTwoSample}
\alias{plot.gofTwoSample}
\title{
  Plot Results of Goodness-of-Fit Test to Compare Two Samples
}
\description{
  Plot the results of calling the function \code{\link{gofTest}} to compare 
  two samples.  \code{\link{gofTest}} returns an object of class \code{"gofTwoSample"} 
  when supplied with both the arguments \code{y} and \code{x}.  
  \code{plot.gofTwoSample} provides five different kinds of plots.

  The function \code{plot.gofTwoSample} is automatically called by \code{\link{plot}} 
  when given an object of class \code{"gofTwoSample"}.  The names of other functions 
  associated with goodness-of-fit test are listed under \link{Goodness-of-Fit Tests}.
}
\usage{
\method{plot}{gofTwoSample}(x, plot.type = "Summary", 
    captions = list(PDFs = NULL, CDFs = NULL, QQ = NULL, MDQQ = NULL, Results = NULL), 
    x.labels = list(PDFs = NULL, CDFs = NULL, QQ = NULL, MDQQ = NULL), 
    y.labels = list(PDFs = NULL, CDFs = NULL, QQ = NULL, MDQQ = NULL), 
    same.window = FALSE, ask = same.window & plot.type == "All", x.points.col = "blue", 
    y.points.col = "black", points.pch = 1, jitter.points = TRUE, discrete = FALSE, 
    plot.pos.con = 0.375, x.ecdf.col = "blue", y.ecdf.col = "black", 
    x.ecdf.lwd = 3 * par("cex"), y.ecdf.lwd = 3 * par("cex"), x.ecdf.lty = 1, 
    y.ecdf.lty = 4, add.line = TRUE, 
    digits = ifelse(plot.type == "Summary", 2, .Options$digits), test.result.font = 1, 
    test.result.cex = ifelse(plot.type == "Summary", 0.9, 1) * par("cex"), 
    test.result.mar = c(0, 0, 3, 0) + 0.1, 
    cex.main = ifelse(plot.type == "Summary", 1.2, 1.5) * par("cex"), 
    cex.axis = ifelse(plot.type == "Summary", 0.9, 1) * par("cex"), 
    cex.lab = ifelse(plot.type == "Summary", 0.9, 1) * par("cex"), 
    main = NULL, xlab = NULL, ylab = NULL, xlim = NULL, ylim = NULL, 
    add.om.title = TRUE, 
    oma = if (plot.type == "Summary" & add.om.title) c(0, 0, 4, 0) else c(0, 0, 0, 0), 
    om.title = NULL, om.font = 2, om.cex.main = 1.5 * par("cex"), om.line = 0, ...)
}
\arguments{
  \item{x}{
  an object of class \code{"gof"}.  See \code{\link{gof.object}} for details.
}
  \item{plot.type}{
  character string indicating what kind of plot to create.  Only one particular 
  plot type will be created, unless \code{plot.type="All"}, in which case all plots 
  will be created sequentially.  The possible values of \code{plot.type} are: \cr
  \code{"Summary"} (the default), \cr
  \code{"PDFs: Observed"}, \cr
  \code{"CDFs: Observed"}, \cr
  \code{"Q-Q Plot"}, \cr
  \code{"Tukey M-D Q-Q Plot"}, \cr
  \code{"Test Results"}, and \cr
  \code{"All"}.  \cr
  See the DETAILS section for more information.
}
  \item{captions}{
  a list with 1 to 5 components with the names \code{"PDFs"}, \code{"CDFs"}, 
  \code{"QQ"}, \code{"MDQQ"}, and/or \code{"Results"}.  Each component either has 
  the value \code{NULL} or else it is a character string containing the title for that 
  particular kind of plot.  When the component has the value \code{NULL} (the default), 
  a default title is used.  This argument is useful when you are creating more than 
  one kind of plot with a single call to \code{plot.gofTwoSample} (i.e., when 
  \code{plot.type="Summary"} or \code{plot.type="All"}) and you want to specify titles 
  different from the default ones.  If you are creating only one kind of plot, 
  then you can just use the \code{main} argument to specify a title different 
  from the default one. 
}
  \item{x.labels}{
  a list of 1 to 4 components with the names \code{"PDFs"}, \code{"CDFs"}, \code{"QQ"}, 
  and/or \code{"MDQQ"}.  Each component either has the value 
  \code{NULL} or else it is a character string containing the label for the 
  \eqn{x}-axis for that particular kind of plot.  When the component has the value 
  \code{NULL} (the default), a default \eqn{x}-axis label is used.  
  This argument is useful when you are creating more than 
  one kind of plot with a single call to \code{plot.gofTwoSample} (i.e., when 
  \code{plot.type="Summary"} or \code{plot.type="All"}) 
  and you want to specify \eqn{x}-axis 
  labels different from the default ones.  If you are creating only one plot, 
  then you can just use the \code{xlab} argument to 
  specify an \eqn{x}-axis label different from the default one.
}
  \item{y.labels}{
  a list of 1 to 4 components with the names \code{"PDFs"}, \code{"CDFs"}, \code{"QQ"}, 
  and/or \code{"MDQQ"}.  Each component either has the value 
  \code{NULL} or else it is a character string containing the label for the 
  \eqn{y}-axis for that particular kind of plot.  When the component has the value 
  \code{NULL} (the default), a default \eqn{y}-axis label is used.  
  This argument is useful when you are creating more than 
  one kind of plot with a single call to \code{plot.gofTwoSample} (i.e., when 
  \code{plot.type="Summary"} or \code{plot.type="All"}) 
  and you want to specify \eqn{y}-axis 
  labels different from the default ones.  If you are creating only one plot, 
  then you can just use the \code{ylab} argument to 
  specify a \eqn{y}-axis label different from the default one.
}
  \item{same.window}{
  logical scalar indicating whether to produce all plots in the same graphics 
  window (\code{same.window=TRUE}), or to create a new graphics 
  window for each separate plot (\code{same.window=FALSE}; the default).  
  The argument is relevant only when \code{plot.type="All"}.
}
  \item{ask}{
  logical scalar supplied to the function \code{\link{devAskNewPage}}, indicating 
  whether to prompt the user before creating a new plot within a single graphics 
  window.  The default value is \code{FALSE} unless \code{same.window=TRUE} and 
  \code{plot.type == "All"}.
}



\bold{Arguments associated with \code{plot.type="PDFs: Observed"}:} \cr

  \item{x.points.col}{
  a character string or numeric scalar determining the color of the plotting symbol  
  used to display the distribution of the observed \code{x} values 
  that were supplied to \code{\link{gofTest}}.  
  The default value is \code{x.points.col="blue"}.  See the entry for 
  \code{col} in the \R help file for \code{\link{par}} for more information.
}
  \item{y.points.col}{
  a character string or numeric scalar determining the color of the plotting symbol  
  used to display the distribution of the observed \code{y} values
  that were supplied to \code{\link{gofTest}}.  
  The default value is \code{y.points.col="black"}.  See the entry for 
  \code{col} in the \R help file for \code{\link{par}} for more information.
}
  \item{points.pch}{
  a character string or numeric scalar determining the plotting symbol  
  used to display the distribution of the observed \code{x} and \code{y} values
  that were supplied to \code{\link{gofTest}}.  
  The default value is \code{points.pch=1}.  See the entry for 
  \code{pch} in the \R help file for \code{\link{par}} for more information.
}
  \item{jitter.points}{
  logical scalar indicating whether to jitter the points in the strip chart.  
  The default value is \code{jitter.points=TRUE}.
}



\bold{Arguments associated with \code{plot.type="CDFs: Observed"}:} \cr

  \item{discrete}{
  logical scalar indicating whether the two distributions are considered to be 
  discrete (\code{discrete=TRUE}) or not(\code{discrete=FALSE}; the default).  When \cr
  \code{discrete=TRUE}, the empirical CDFs are plotted as step functions.
}
  \item{plot.pos.con}{
  numeric scalar between 0 and 1 containing the value of the plotting position 
  constant used to construct the observed (empirical) CDFs.  The default value  
  is \code{plot.pos.con=0.375}.  See the help files for 
  \code{\link{ecdfPlot}} and \code{\link{qqPlot}} for more information and the 
  motivation for this choice of values.

  \bold{NOTE:}  This argument is also used to determine the value of the 
  plotting position constant for the Q-Q plot (\code{plot.type="Q-Q Plot"}), or the 
  Tukey Mean-Difference Q-Q plot (\code{plot.type="Tukey M-D Q-Q Plot"}).
}
  \item{x.ecdf.col}{
  a character string or numeric scalar determining the color of the line  
  used to display the empirical CDF for the \code{x} values
  that were supplied to \code{\link{gofTest}}.  The default value is 
  \code{x.ecdf.col="blue"}.  See the entry for \code{col} in the \R help file for 
  \code{\link{par}} for more information.
}
  \item{y.ecdf.col}{
  a character string or numeric scalar determining the color of the line  
  used to display the empirical CDF for the \code{y} values
  that were supplied to \code{\link{gofTest}}.  The default value is 
  \code{y.ecdf.col="black"}.  See the entry for \code{col} in the \R help file for 
  \code{\link{par}} for more information.
}
  \item{x.ecdf.lwd}{
  numeric scalar determining the width of the line used to display the empirical CDF 
  for the \code{x} values that were supplied to \code{\link{gofTest}}.  
  The default value is \code{x.ecdf.lwd=3*par("cex")}.  
  See the entry for \code{lwd} in the \R help file for \code{\link{par}} for more 
  information.
}
  \item{y.ecdf.lwd}{
  numeric scalar determining the width of the line used to display the empirical CDF 
  for the \code{y} values that were supplied to \code{\link{gofTest}}.  
  The default value is \code{y.ecdf.lwd=3*par("cex")}.  
  See the entry for \code{lwd} in the \R help file for \code{\link{par}} for more 
  information.
}
  \item{x.ecdf.lty}{
  numeric scalar determining the line type used to display the empirical CDF for the 
  \code{x} values that were supplied to \code{\link{gofTest}}.  
  The default value is \code{x.ecdf.lty=1}.  
  See the entry for \code{lty} in the \R help file for \code{\link{par}} for more 
  information.
}
  \item{y.ecdf.lty}{
  numeric scalar determining the line type used to display the empirical CDF for the 
  \code{y} values that were supplied to \code{\link{gofTest}}.  
  The default value is \code{y.ecdf.lty=4}.  
  See the entry for \code{lty} in the \R help file for \code{\link{par}} for more 
  information.
}



\bold{Arguments associated with \code{plot.type="Q-Q Plot"} or} \cr
\bold{\code{plot.type="Tukey M-D Q-Q Plot"}:} \cr

As explained above, \code{plot.pos.con} is used for these plot types.  Also: \cr

  \item{add.line}{
  logical scalar indicating whether to add a line to the plot.  If \code{add.line=TRUE} 
  and \code{plot.type="Q-Q Plot"}, a 0-1 line is added to the plot.  
  If \code{add.line=TRUE} and \code{plot.type="Tukey M-D Q-Q Plot"}, a horizontal 
  line at \eqn{y=0} is added to the plot.  The default value is \code{add.line=TRUE}.
}



\bold{Arguments associated with \code{plot.type="Test Results"}} \cr 

  \item{digits}{
  scalar indicating how many significant digits to print for the test results 
  when \code{plot.type="Summary"} or \code{plot.type="Test Results"}.  If \cr
  \code{plot.type == "Summary"}, the default value is 
  \code{digits=2}, otherwise it is \cr
  \code{.Options$digits} (i.e., the current setting of \code{options("digits")}).
}
  \item{test.result.font}{
  numeric scalar indicating which font to use to print out the test results.  
  The default value is \code{test.result.font=1}.  See the description of the 
  \code{font} argument in the help file for \code{\link{par}} for more information.  
  You may get better results if you use a font number that corresponds to a fixed 
  font (e.g., courier).
}
  \item{test.result.cex}{
  numeric scalar indicating the value of \code{cex} to use to print out the 
  test results.  The default value is \code{0.9*par("cex")} when 
  \code{plot.type="Summary"}, otherwise it is \code{par("cex")}. 
  See the description of the \code{cex} argument in the help file for 
  \code{\link{par}} for more information.  
}
  \item{test.result.mar}{
  numeric vector indicating the value of \code{mar} to use to print out the 
  test results.  The default value is \code{test.result.mar=c(0, 0, 3, 0)+0.1}.  
  See the description of the \code{mar} argument in the help file for 
  \code{\link{par}} for more information.  
}



\bold{Arguments associated with \code{plot.type="Summary"}} \cr

  \item{add.om.title}{
  logical scalar indicating whether to add a title in the outer margin when \cr
  \code{plot.type="Summary"}.  The default value is \code{add.om.title=TRUE}.
}
  \item{om.title}{
  character string containing the outer margin title.  The default value is \cr
  \code{om.title=NULL}, which will result in a default title.
}
  \item{om.font}{
  numeric scalar indicating the font to use for the outer margin.  The default 
  value is \code{om.font=2}.
}
  \item{om.cex.main}{
  numeric scalar indicating the value of \code{cex} for the outer margin title.  
  The default value is \code{1.75 * par("cex")}.
}
  \item{om.line}{
  numeric scalar indicating the line to place the outer margin title on.  The 
  default value is \code{om.line=0.5}.
}



\bold{Graphics parameters:} \cr

  \item{cex.main, cex.axis, cex.lab, main, xlab, ylab, xlim, ylim, oma, \dots}{
  additional graphics parameters.  See the help file for \code{\link{par}}.
}
}
\details{
  The function \code{plot.gofTwoSample} is a method for the generic function 
  \code{\link{plot}} for the class \cr
  \code{"gofTwoSample"} (see \code{\link{gofTwoSample.object}}).  
  It can be invoked by calling \code{\link{plot}} and giving it an object of 
  class \code{"gofTwoSample"} as the first argument, or by calling 
  \code{plot.gofTwoSample} directly, regardless of the class of the object given 
  as the first argument to \code{plot.gofTwoSample}.

  Plots associated with the goodness-of-fit test are produced on the current graphics 
  device.  These can be one or all of the following:
  \itemize{
  \item Observed distributions (\code{plot.type="PDFs: Observed"}).
  \item Observed CDFs (\code{plot.type="CDFs: Observed"}).  
    See the help file for \code{\link{cdfCompare}}.
  \item Q-Q Plot (\code{plot.type="Q-Q Plot"}).  See the help file for 
    \code{\link{qqPlot}}. 
  \item Tukey mean-difference Q-Q plot (\code{plot.type="Tukey M-D Q-Q Plot"}).  
    See the help file for \code{\link{qqPlot}}.
  \item Results of the goodness-of-fit test (\code{plot.type="Test Results"}).  
    See the help file for \code{\link{print.gofTwoSample}}.
  }
  See the help file for \code{\link{gofTest}} for more information.
}
\value{
  \code{plot.gofTwoSample} invisibly returns the first argument, \code{x}.
}
\references{
  Chambers, J. M. and Hastie, T. J. (1992).  \emph{Statistical Models in S}.  
  Wadsworth & Brooks/Cole. 
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\seealso{
  \code{\link{gofTest}}, \code{\link{gofTwoSample.object}}, 
  \code{\link{print.gofTwoSample}}, 
  \link{Goodness-of-Fit Tests}, \code{\link{plot}}.
}
\examples{
  # Create an object of class "gofTwoSample" then plot the results.
  # (Note: the call to set.seed simply allows you to reproduce 
  # this example.)

  set.seed(300) 
  dat1 <- rnorm(20, mean = 3, sd = 2) 
  dat2 <- rnorm(10, mean = 1, sd = 2) 
  gof.obj <- gofTest(x = dat1, y = dat2) 

  # Summary plot (the default)
  #---------------------------
  dev.new()
  plot(gof.obj)


  # Make your own titles for the summary plot
  #------------------------------------------
  dev.new()
  plot(gof.obj, captions = list(PDFs = "Compare PDFs", 
    CDFs = "Compare CDFs", QQ = "Q-Q Plot", Results = "Results"),
    om.title = "Summary Plot")


  # Just the Q-Q Plot
  #------------------
  dev.new()
  plot(gof.obj, plot.type="Q-Q")


  # Make your own title for the Q-Q Plot
  #-------------------------------------
  dev.new()
  plot(gof.obj, plot.type="Q-Q", main = "Q-Q Plot")

  #==========

  # Clean up
  #---------
  rm(dat1, dat2, gof.obj)
  graphics.off()
}
\keyword{plot}
