#' Plot Results of Goodness-of-Fit Test to Compare Two Samples
#' @description
#' Plot the results of calling the function \code{\link{gofTest}} to compare
#'   two samples.  \code{\link{gofTest}} returns an object of class \code{"gofTwoSample"}
#'   when supplied with both the arguments \code{y} and \code{x}.
#'   \code{plot.gofTwoSample} provides five different kinds of plots.
#'
#'   The function \code{plot.gofTwoSample} is automatically called by \code{\link{plot}}
#'   when given an object of class \code{"gofTwoSample"}.  The names of other functions
#'   associated with goodness-of-fit test are listed under \link{Goodness-of-Fit Tests}.
#' @usage
#' \method{plot}{gofTwoSample}(x, plot.type = "Summary",
#'     captions = list(PDFs = NULL, CDFs = NULL, QQ = NULL, MDQQ = NULL, Results = NULL),
#'     x.labels = list(PDFs = NULL, CDFs = NULL, QQ = NULL, MDQQ = NULL),
#'     y.labels = list(PDFs = NULL, CDFs = NULL, QQ = NULL, MDQQ = NULL),
#'     same.window = FALSE, ask = same.window & plot.type == "All", x.points.col = "blue",
#'     y.points.col = "black", points.pch = 1, jitter.points = TRUE, discrete = FALSE,
#'     plot.pos.con = 0.375, x.ecdf.col = "blue", y.ecdf.col = "black",
#'     x.ecdf.lwd = 3 * par("cex"), y.ecdf.lwd = 3 * par("cex"), x.ecdf.lty = 1,
#'     y.ecdf.lty = 4, add.line = TRUE,
#'     digits = ifelse(plot.type == "Summary", 2, .Options$digits), test.result.font = 1,
#'     test.result.cex = ifelse(plot.type == "Summary", 0.9, 1) * par("cex"),
#'     test.result.mar = c(0, 0, 3, 0) + 0.1,
#'     cex.main = ifelse(plot.type == "Summary", 1.2, 1.5) * par("cex"),
#'     cex.axis = ifelse(plot.type == "Summary", 0.9, 1) * par("cex"),
#'     cex.lab = ifelse(plot.type == "Summary", 0.9, 1) * par("cex"),
#'     main = NULL, xlab = NULL, ylab = NULL, xlim = NULL, ylim = NULL,
#'     add.om.title = TRUE,
#'     oma = if (plot.type == "Summary" & add.om.title) c(0, 0, 4, 0) else c(0, 0, 0, 0),
#'     om.title = NULL, om.font = 2, om.cex.main = 1.5 * par("cex"), om.line = 0, ...)
#' @rawRd
#' \arguments{
#'   \item{x}{
#'   an object of class \code{"gof"}.  See \code{\link{gof.object}} for details.
#' }
#'   \item{plot.type}{
#'   character string indicating what kind of plot to create.  Only one particular
#'   plot type will be created, unless \code{plot.type="All"}, in which case all plots
#'   will be created sequentially.  The possible values of \code{plot.type} are: \cr
#'   \code{"Summary"} (the default), \cr
#'   \code{"PDFs: Observed"}, \cr
#'   \code{"CDFs: Observed"}, \cr
#'   \code{"Q-Q Plot"}, \cr
#'   \code{"Tukey M-D Q-Q Plot"}, \cr
#'   \code{"Test Results"}, and \cr
#'   \code{"All"}.  \cr
#'   See the DETAILS section for more information.
#' }
#'   \item{captions}{
#'   a list with 1 to 5 components with the names \code{"PDFs"}, \code{"CDFs"},
#'   \code{"QQ"}, \code{"MDQQ"}, and/or \code{"Results"}.  Each component either has
#'   the value \code{NULL} or else it is a character string containing the title for that
#'   particular kind of plot.  When the component has the value \code{NULL} (the default),
#'   a default title is used.  This argument is useful when you are creating more than
#'   one kind of plot with a single call to \code{plot.gofTwoSample} (i.e., when
#'   \code{plot.type="Summary"} or \code{plot.type="All"}) and you want to specify titles
#'   different from the default ones.  If you are creating only one kind of plot,
#'   then you can just use the \code{main} argument to specify a title different
#'   from the default one.
#' }
#'   \item{x.labels}{
#'   a list of 1 to 4 components with the names \code{"PDFs"}, \code{"CDFs"}, \code{"QQ"},
#'   and/or \code{"MDQQ"}.  Each component either has the value
#'   \code{NULL} or else it is a character string containing the label for the
#'   \eqn{x}-axis for that particular kind of plot.  When the component has the value
#'   \code{NULL} (the default), a default \eqn{x}-axis label is used.
#'   This argument is useful when you are creating more than
#'   one kind of plot with a single call to \code{plot.gofTwoSample} (i.e., when
#'   \code{plot.type="Summary"} or \code{plot.type="All"})
#'   and you want to specify \eqn{x}-axis
#'   labels different from the default ones.  If you are creating only one plot,
#'   then you can just use the \code{xlab} argument to
#'   specify an \eqn{x}-axis label different from the default one.
#' }
#'   \item{y.labels}{
#'   a list of 1 to 4 components with the names \code{"PDFs"}, \code{"CDFs"}, \code{"QQ"},
#'   and/or \code{"MDQQ"}.  Each component either has the value
#'   \code{NULL} or else it is a character string containing the label for the
#'   \eqn{y}-axis for that particular kind of plot.  When the component has the value
#'   \code{NULL} (the default), a default \eqn{y}-axis label is used.
#'   This argument is useful when you are creating more than
#'   one kind of plot with a single call to \code{plot.gofTwoSample} (i.e., when
#'   \code{plot.type="Summary"} or \code{plot.type="All"})
#'   and you want to specify \eqn{y}-axis
#'   labels different from the default ones.  If you are creating only one plot,
#'   then you can just use the \code{ylab} argument to
#'   specify a \eqn{y}-axis label different from the default one.
#' }
#'   \item{same.window}{
#'   logical scalar indicating whether to produce all plots in the same graphics
#'   window (\code{same.window=TRUE}), or to create a new graphics
#'   window for each separate plot (\code{same.window=FALSE}; the default).
#'   The argument is relevant only when \code{plot.type="All"}.
#' }
#'   \item{ask}{
#'   logical scalar supplied to the function \code{\link{devAskNewPage}}, indicating
#'   whether to prompt the user before creating a new plot within a single graphics
#'   window.  The default value is \code{FALSE} unless \code{same.window=TRUE} and
#'   \code{plot.type == "All"}.
#' }
#'
#'
#'
#' \bold{Arguments associated with \code{plot.type="PDFs: Observed"}:} \cr
#'
#'   \item{x.points.col}{
#'   a character string or numeric scalar determining the color of the plotting symbol
#'   used to display the distribution of the observed \code{x} values
#'   that were supplied to \code{\link{gofTest}}.
#'   The default value is \code{x.points.col="blue"}.  See the entry for
#'   \code{col} in the \R help file for \code{\link{par}} for more information.
#' }
#'   \item{y.points.col}{
#'   a character string or numeric scalar determining the color of the plotting symbol
#'   used to display the distribution of the observed \code{y} values
#'   that were supplied to \code{\link{gofTest}}.
#'   The default value is \code{y.points.col="black"}.  See the entry for
#'   \code{col} in the \R help file for \code{\link{par}} for more information.
#' }
#'   \item{points.pch}{
#'   a character string or numeric scalar determining the plotting symbol
#'   used to display the distribution of the observed \code{x} and \code{y} values
#'   that were supplied to \code{\link{gofTest}}.
#'   The default value is \code{points.pch=1}.  See the entry for
#'   \code{pch} in the \R help file for \code{\link{par}} for more information.
#' }
#'   \item{jitter.points}{
#'   logical scalar indicating whether to jitter the points in the strip chart.
#'   The default value is \code{jitter.points=TRUE}.
#' }
#'
#'
#'
#' \bold{Arguments associated with \code{plot.type="CDFs: Observed"}:} \cr
#'
#'   \item{discrete}{
#'   logical scalar indicating whether the two distributions are considered to be
#'   discrete (\code{discrete=TRUE}) or not(\code{discrete=FALSE}; the default).  When \cr
#'   \code{discrete=TRUE}, the empirical CDFs are plotted as step functions.
#' }
#'   \item{plot.pos.con}{
#'   numeric scalar between 0 and 1 containing the value of the plotting position
#'   constant used to construct the observed (empirical) CDFs.  The default value
#'   is \code{plot.pos.con=0.375}.  See the help files for
#'   \code{\link{ecdfPlot}} and \code{\link{qqPlot}} for more information and the
#'   motivation for this choice of values.
#'
#'   \bold{NOTE:}  This argument is also used to determine the value of the
#'   plotting position constant for the Q-Q plot (\code{plot.type="Q-Q Plot"}), or the
#'   Tukey Mean-Difference Q-Q plot (\code{plot.type="Tukey M-D Q-Q Plot"}).
#' }
#'   \item{x.ecdf.col}{
#'   a character string or numeric scalar determining the color of the line
#'   used to display the empirical CDF for the \code{x} values
#'   that were supplied to \code{\link{gofTest}}.  The default value is
#'   \code{x.ecdf.col="blue"}.  See the entry for \code{col} in the \R help file for
#'   \code{\link{par}} for more information.
#' }
#'   \item{y.ecdf.col}{
#'   a character string or numeric scalar determining the color of the line
#'   used to display the empirical CDF for the \code{y} values
#'   that were supplied to \code{\link{gofTest}}.  The default value is
#'   \code{y.ecdf.col="black"}.  See the entry for \code{col} in the \R help file for
#'   \code{\link{par}} for more information.
#' }
#'   \item{x.ecdf.lwd}{
#'   numeric scalar determining the width of the line used to display the empirical CDF
#'   for the \code{x} values that were supplied to \code{\link{gofTest}}.
#'   The default value is \code{x.ecdf.lwd=3*par("cex")}.
#'   See the entry for \code{lwd} in the \R help file for \code{\link{par}} for more
#'   information.
#' }
#'   \item{y.ecdf.lwd}{
#'   numeric scalar determining the width of the line used to display the empirical CDF
#'   for the \code{y} values that were supplied to \code{\link{gofTest}}.
#'   The default value is \code{y.ecdf.lwd=3*par("cex")}.
#'   See the entry for \code{lwd} in the \R help file for \code{\link{par}} for more
#'   information.
#' }
#'   \item{x.ecdf.lty}{
#'   numeric scalar determining the line type used to display the empirical CDF for the
#'   \code{x} values that were supplied to \code{\link{gofTest}}.
#'   The default value is \code{x.ecdf.lty=1}.
#'   See the entry for \code{lty} in the \R help file for \code{\link{par}} for more
#'   information.
#' }
#'   \item{y.ecdf.lty}{
#'   numeric scalar determining the line type used to display the empirical CDF for the
#'   \code{y} values that were supplied to \code{\link{gofTest}}.
#'   The default value is \code{y.ecdf.lty=4}.
#'   See the entry for \code{lty} in the \R help file for \code{\link{par}} for more
#'   information.
#' }
#'
#'
#'
#' \bold{Arguments associated with \code{plot.type="Q-Q Plot"} or} \cr
#' \bold{\code{plot.type="Tukey M-D Q-Q Plot"}:} \cr
#'
#' As explained above, \code{plot.pos.con} is used for these plot types.  Also: \cr
#'
#'   \item{add.line}{
#'   logical scalar indicating whether to add a line to the plot.  If \code{add.line=TRUE}
#'   and \code{plot.type="Q-Q Plot"}, a 0-1 line is added to the plot.
#'   If \code{add.line=TRUE} and \code{plot.type="Tukey M-D Q-Q Plot"}, a horizontal
#'   line at \eqn{y=0} is added to the plot.  The default value is \code{add.line=TRUE}.
#' }
#'
#'
#'
#' \bold{Arguments associated with \code{plot.type="Test Results"}} \cr
#'
#'   \item{digits}{
#'   scalar indicating how many significant digits to print for the test results
#'   when \code{plot.type="Summary"} or \code{plot.type="Test Results"}.  If \cr
#'   \code{plot.type == "Summary"}, the default value is
#'   \code{digits=2}, otherwise it is \cr
#'   \code{.Options$digits} (i.e., the current setting of \code{options("digits")}).
#' }
#'   \item{test.result.font}{
#'   numeric scalar indicating which font to use to print out the test results.
#'   The default value is \code{test.result.font=1}.  See the description of the
#'   \code{font} argument in the help file for \code{\link{par}} for more information.
#'   You may get better results if you use a font number that corresponds to a fixed
#'   font (e.g., courier).
#' }
#'   \item{test.result.cex}{
#'   numeric scalar indicating the value of \code{cex} to use to print out the
#'   test results.  The default value is \code{0.9*par("cex")} when
#'   \code{plot.type="Summary"}, otherwise it is \code{par("cex")}.
#'   See the description of the \code{cex} argument in the help file for
#'   \code{\link{par}} for more information.
#' }
#'   \item{test.result.mar}{
#'   numeric vector indicating the value of \code{mar} to use to print out the
#'   test results.  The default value is \code{test.result.mar=c(0, 0, 3, 0)+0.1}.
#'   See the description of the \code{mar} argument in the help file for
#'   \code{\link{par}} for more information.
#' }
#'
#'
#'
#' \bold{Arguments associated with \code{plot.type="Summary"}} \cr
#'
#'   \item{add.om.title}{
#'   logical scalar indicating whether to add a title in the outer margin when \cr
#'   \code{plot.type="Summary"}.  The default value is \code{add.om.title=TRUE}.
#' }
#'   \item{om.title}{
#'   character string containing the outer margin title.  The default value is \cr
#'   \code{om.title=NULL}, which will result in a default title.
#' }
#'   \item{om.font}{
#'   numeric scalar indicating the font to use for the outer margin.  The default
#'   value is \code{om.font=2}.
#' }
#'   \item{om.cex.main}{
#'   numeric scalar indicating the value of \code{cex} for the outer margin title.
#'   The default value is \code{1.75 * par("cex")}.
#' }
#'   \item{om.line}{
#'   numeric scalar indicating the line to place the outer margin title on.  The
#'   default value is \code{om.line=0.5}.
#' }
#'
#'
#'
#' \bold{Graphics parameters:} \cr
#'
#'   \item{cex.main, cex.axis, cex.lab, main, xlab, ylab, xlim, ylim, oma, \dots}{
#'   additional graphics parameters.  See the help file for \code{\link{par}}.
#' }
#' }
#' @rawRd
#' \details{
#'   The function \code{plot.gofTwoSample} is a method for the generic function
#'   \code{\link{plot}} for the class \cr
#'   \code{"gofTwoSample"} (see \code{\link{gofTwoSample.object}}).
#'   It can be invoked by calling \code{\link{plot}} and giving it an object of
#'   class \code{"gofTwoSample"} as the first argument, or by calling
#'   \code{plot.gofTwoSample} directly, regardless of the class of the object given
#'   as the first argument to \code{plot.gofTwoSample}.
#'
#'   Plots associated with the goodness-of-fit test are produced on the current graphics
#'   device.  These can be one or all of the following:
#'   \itemize{
#'   \item Observed distributions (\code{plot.type="PDFs: Observed"}).
#'   \item Observed CDFs (\code{plot.type="CDFs: Observed"}).
#'     See the help file for \code{\link{cdfCompare}}.
#'   \item Q-Q Plot (\code{plot.type="Q-Q Plot"}).  See the help file for
#'     \code{\link{qqPlot}}.
#'   \item Tukey mean-difference Q-Q plot (\code{plot.type="Tukey M-D Q-Q Plot"}).
#'     See the help file for \code{\link{qqPlot}}.
#'   \item Results of the goodness-of-fit test (\code{plot.type="Test Results"}).
#'     See the help file for \code{\link{print.gofTwoSample}}.
#'   }
#'   See the help file for \code{\link{gofTest}} for more information.
#' }
#' @rawRd
#' \value{
#'   \code{plot.gofTwoSample} invisibly returns the first argument, \code{x}.
#' }
#' @rawRd
#' \references{
#'   Chambers, J. M. and Hastie, T. J. (1992).  \emph{Statistical Models in S}.
#'   Wadsworth & Brooks/Cole.
#' }
#' @rawRd
#' \author{
#'   Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \seealso{
#'   \code{\link{gofTest}}, \code{\link{gofTwoSample.object}},
#'   \code{\link{print.gofTwoSample}},
#'   \link{Goodness-of-Fit Tests}, \code{\link{plot}}.
#' }
#' @rawRd
#' \examples{
#'   # Create an object of class "gofTwoSample" then plot the results.
#'   # (Note: the call to set.seed simply allows you to reproduce
#'   # this example.)
#'
#'   set.seed(300)
#'   dat1 <- rnorm(20, mean = 3, sd = 2)
#'   dat2 <- rnorm(10, mean = 1, sd = 2)
#'   gof.obj <- gofTest(x = dat1, y = dat2)
#'
#'   # Summary plot (the default)
#'   #---------------------------
#'   dev.new()
#'   plot(gof.obj)
#'
#'
#'   # Make your own titles for the summary plot
#'   #------------------------------------------
#'   dev.new()
#'   plot(gof.obj, captions = list(PDFs = "Compare PDFs",
#'     CDFs = "Compare CDFs", QQ = "Q-Q Plot", Results = "Results"),
#'     om.title = "Summary Plot")
#'
#'
#'   # Just the Q-Q Plot
#'   #------------------
#'   dev.new()
#'   plot(gof.obj, plot.type="Q-Q")
#'
#'
#'   # Make your own title for the Q-Q Plot
#'   #-------------------------------------
#'   dev.new()
#'   plot(gof.obj, plot.type="Q-Q", main = "Q-Q Plot")
#'
#'   #==========
#'
#'   # Clean up
#'   #---------
#'   rm(dat1, dat2, gof.obj)
#'   graphics.off()
#' }
#' @rawRd
#' \keyword{plot}
#' @exportS3Method NULL

plot.gofTwoSample <-
function (x, plot.type = "Summary", captions = list(PDFs = NULL, 
    CDFs = NULL, QQ = NULL, MDQQ = NULL, Results = NULL), x.labels = list(PDFs = NULL, 
    CDFs = NULL, QQ = NULL, MDQQ = NULL), y.labels = list(PDFs = NULL, 
    CDFs = NULL, QQ = NULL, MDQQ = NULL), same.window = FALSE, 
    ask = same.window & plot.type == "All", x.points.col = "blue", 
    y.points.col = "black", points.pch = 1, jitter.points = TRUE, 
    discrete = FALSE, plot.pos.con = 0.375, x.ecdf.col = "blue", 
    y.ecdf.col = "black", x.ecdf.lwd = 3 * par("cex"), y.ecdf.lwd = 3 * 
        par("cex"), x.ecdf.lty = 1, y.ecdf.lty = 4, add.line = TRUE, 
    digits = ifelse(plot.type == "Summary", 2, .Options$digits), 
    test.result.font = 1, test.result.cex = ifelse(plot.type == 
        "Summary", 0.9, 1) * par("cex"), test.result.mar = c(0, 
        0, 3, 0) + 0.1, cex.main = ifelse(plot.type == "Summary", 
        1.2, 1.5) * par("cex"), cex.axis = ifelse(plot.type == 
        "Summary", 0.9, 1) * par("cex"), cex.lab = ifelse(plot.type == 
        "Summary", 0.9, 1) * par("cex"), main = NULL, xlab = NULL, 
    ylab = NULL, xlim = NULL, ylim = NULL, add.om.title = TRUE, 
    oma = if (plot.type == "Summary" & add.om.title) c(0, 0, 
        4, 0) else c(0, 0, 0, 0), om.title = NULL, om.font = 2, 
    om.cex.main = 1.5 * par("cex"), om.line = 0, ...) 
{
    gof.obj <- x
    plot.type <- match.arg(plot.type, c("Summary", "All", "PDFs: Observed", 
        "CDFs: Observed", "Q-Q Plot", "Tukey M-D Q-Q Plot", "Test Results"))
    check.gp.list <- checkGraphicsPars(...)
    gp.arg.list <- check.gp.list$gp.arg.list
    gen.gp.list <- check.gp.list$gen.gp.list
    data <- gof.obj$data
    x <- data[[1]]
    y <- data[[2]]
    names.data <- names(data)
    x.name <- names.data[1]
    y.name <- names.data[2]
    data.name <- gof.obj$data.name
    parent.of.data <- gof.obj$parent.of.data
    user.main <- main
    user.xlab <- xlab
    user.ylab <- ylab
    user.xlim <- xlim
    user.ylim <- ylim
    if (!missing(captions)) {
        if (!is.list(captions)) 
            stop("The argument 'captions' must be a list")
        len <- length(captions)
        if (len < 1 | len > 5) 
            stop("The argument 'captions' must be a list with 1 to 5 components")
        if (!all(sapply(captions, length) == 1) || !all(sapply(captions, 
            is.character))) 
            stop("All components of the argument 'captions' must be character strings")
        names.vec <- names(captions)
        if (!all(names.vec %in% c("PDFs", "CDFs", "QQ", "MDQQ", 
            "Results"))) 
            stop(paste("All components of the argument 'captions'", 
                "must have names, and the name must be one of", 
                "\"PDFs\", \"CDFs\", \"QQ\", \"MDQQ\", or \"Results\""))
        if (length(unique(names.vec)) != length(names.vec)) 
            stop(paste("The names of the components for the argument 'captions'", 
                "must be unique"))
        old.captions <- captions
        captions <- list(PDFs = NULL, CDFs = NULL, QQ = NULL, 
            MDQQ = NULL, Results = NULL)
        captions[names.vec] <- old.captions
    }
    if (!missing(x.labels)) {
        if (!is.list(x.labels)) 
            stop("The argument 'x.labels' must be a list")
        len <- length(x.labels)
        if (len < 1 | len > 4) 
            stop("The argument 'x.labels' must be a list with 1 to 4 components")
        if (!all(sapply(x.labels, length) == 1) || !all(sapply(x.labels, 
            is.character))) 
            stop("All components of the argument 'x.labels' must be character strings")
        names.vec <- names(x.labels)
        if (!all(names.vec %in% c("PDFs", "CDFs", "QQ", "MDQQ"))) 
            stop(paste("All components of the argument 'x.labels'", 
                "must have names, and the name must be one of", 
                "\"PDFs\", \"CDFs\", \"QQ\", or \"MDQQ\""))
        if (length(unique(names.vec)) != length(names.vec)) 
            stop(paste("The names of the components for the argument 'x.labels'", 
                "must be unique"))
        old.x.labels <- x.labels
        x.labels <- list(PDFs = NULL, CDFs = NULL, QQ = NULL, 
            MDQQ = NULL)
        x.labels[names.vec] <- old.x.labels
    }
    if (!missing(y.labels)) {
        if (!is.list(y.labels)) 
            stop("The argument 'y.labels' must be a list")
        len <- length(y.labels)
        if (len < 1 | len > 4) 
            stop("The argument 'y.labels' must be a list with 1 to 4 components")
        if (!all(sapply(y.labels, length) == 1) || !all(sapply(y.labels, 
            is.character))) 
            stop("All components of the argument 'y.labels' must be character strings")
        names.vec <- names(y.labels)
        if (!all(names.vec %in% c("PDFs", "CDFs", "QQ", "MDQQ"))) 
            stop(paste("All components of the argument 'y.labels'", 
                "must have names, and the name must be one of", 
                "\"PDFs\", \"CDFs\", \"QQ\", or \"MDQQ\""))
        if (length(unique(names.vec)) != length(names.vec)) 
            stop(paste("The names of the components for the argument 'y.labels'", 
                "must be unique"))
        old.y.labels <- y.labels
        y.labels <- list(PDFs = NULL, CDFs = NULL, QQ = NULL, 
            MDQQ = NULL)
        y.labels[names.vec] <- old.y.labels
    }
    if (is.element(plot.type, c("Summary", "All", "PDFs: Observed"))) {
        if (plot.type == "All" & same.window) {
            devAskNewPage(ask = ask)
        }
        else if (plot.type == "Summary") {
            o.par1 <- par(c("cex", "mex", "mgp"))
            o.par2 <- par(mfrow = c(2, 2), mar = c(4, 4, 3, 1) + 
                0.1, oma = oma)
            par(cex = 0.8 * o.par1$cex, mex = 0.8 * o.par1$mex, 
                mgp = c(2.75, 0.5, 0))
            on.exit(par(c(o.par1, o.par2)))
        }
        if (is.null(user.xlab)) {
            if (!is.null(x.labels[[1]])) 
                xlab <- x.labels[[1]]
            else xlab <- ""
        }
        else xlab <- user.xlab
        if (is.null(user.ylab)) {
            if (!is.null(y.labels[[1]])) 
                ylab <- y.labels[[1]]
            else ylab <- "Observed Data"
        }
        else ylab <- user.ylab
        if (is.null(user.xlim)) 
            xlim <- c(0.5, 2.5)
        if (is.null(user.ylim)) 
            ylim <- range(x, y)
        dum.x.for.x <- rep(1, length(x))
        dum.x.for.y <- rep(2, length(y))
        o.mar <- par("mar")
        new.mar <- o.mar
        new.mar[2] <- max(6, o.mar[2])
        o.par <- par(mar = new.mar)
        plot(dum.x.for.x, x, type = "n", xlim = xlim, ylim = ylim, 
            axes = FALSE, xlab = xlab, ylab = ylab)
        if (jitter.points) {
            points(jitter(dum.x.for.x), x, col = x.points.col, 
                pch = points.pch)
            points(jitter(dum.x.for.y), y, col = y.points.col, 
                pch = points.pch)
        }
        else {
            points(dum.x.for.x, x, col = x.points.col, pch = points.pch)
            points(dum.x.for.y, y, col = y.points.col, pch = points.pch)
        }
        axis(1, at = 1:2, labels = c(x.name, y.name), tick = FALSE)
        axis(2)
        box()
        if (is.null(user.main)) {
            if (!is.null(captions[[1]])) 
                main <- captions[[1]]
            else main <- paste("Observed Data (Strip Plot)", 
                "\nfor", x.name, "and", y.name)
        }
        arg.list <- c(list(cex.main = cex.main), gen.gp.list, 
            list(main = main))
        do.call("title", arg.list)
        par(o.par)
    }
    if (is.element(plot.type, c("Summary", "All", "CDFs: Observed"))) {
        if (plot.type == "All" & !same.window) 
            dev.new()
        if (is.null(user.xlab)) {
            if (!is.null(x.labels[[2]])) 
                xlab <- x.labels[[2]]
            else xlab <- "Order Statistics"
        }
        else xlab <- user.xlab
        if (!is.null(user.ylab)) 
            ylab <- user.ylab
        else if (!is.null(y.labels[[2]])) 
            ylab <- y.labels[[2]]
        cdfCompare(x, y, discrete = discrete, plot.pos.con = plot.pos.con, 
            x.col = x.ecdf.col, y.or.fitted.col = y.ecdf.col, 
            x.lwd = x.ecdf.lwd, y.or.fitted.lwd = y.ecdf.lwd, 
            x.lty = x.ecdf.lty, y.or.fitted.lty = y.ecdf.lty, 
            digits = digits, ..., main = "", xlab = xlab, ylab = user.ylab, 
            xlim = user.xlim, ylim = user.ylim)
        if (is.null(user.main)) {
            if (!is.null(captions[[2]])) 
                main <- captions[[2]]
            else main <- paste("Empirical CDF for ", x.name, 
                " (solid line)\nwith Empirical CDF for ", y.name, 
                " (dashed line)", sep = "", collapse = "")
        }
        arg.list <- c(list(cex.main = cex.main), gen.gp.list, 
            list(main = main))
        do.call("title", arg.list)
    }
    if (is.element(plot.type, c("Summary", "All", "Q-Q Plot"))) {
        if (plot.type == "All" & !same.window) 
            dev.new()
        if (is.null(user.xlab)) {
            if (!is.null(x.labels[[3]])) 
                xlab <- x.labels[[3]]
            else xlab <- paste("Quantiles of", x.name)
        }
        else xlab <- user.xlab
        if (is.null(user.ylab)) {
            if (!is.null(y.labels[[3]])) 
                ylab <- y.labels[[3]]
            else ylab <- paste("Quantiles of", y.name)
        }
        else ylab <- user.ylab
        qqPlot(x, y, plot.pos.con = plot.pos.con, digits = digits, 
            add.line = add.line, qq.line.type = "0-1", ..., xlab = xlab, 
            ylab = ylab, main = "", xlim = user.xlim, ylim = user.ylim)
        if (is.null(user.main)) {
            if (!is.null(captions[[3]])) 
                main <- captions[[3]]
            else {
                main <- paste("Q-Q Plot of\n", y.name, "vs.", 
                  x.name)
            }
        }
        arg.list <- c(list(cex.main = cex.main), gen.gp.list, 
            list(main = main))
        do.call("title", arg.list)
    }
    if (is.element(plot.type, c("All", "Tukey M-D Q-Q Plot"))) {
        if (plot.type == "All" & !same.window) 
            dev.new()
        if (is.null(user.xlab) & !is.null(x.labels[[4]])) 
            xlab <- x.labels[[4]]
        else xlab <- user.xlab
        if (is.null(user.ylab)) {
            if (!is.null(y.labels[[4]])) 
                ylab <- y.labels[[4]]
            else ylab <- paste(y.name, "Quantiles -", x.name, 
                "Quantiles")
        }
        else ylab <- user.ylab
        qqPlot(x, y, plot.type = "Tukey Mean-Difference Q-Q", 
            add.line = add.line, digits = digits, ..., main = "", 
            xlab = xlab, ylab = ylab, xlim = user.xlim, ylim = user.ylim)
        if (is.null(user.main)) {
            if (!is.null(captions[[4]])) 
                main <- captions[[4]]
            else main <- paste("Tukey Mean-Difference Q-Q Plot for\n", 
                x.name, "and", y.name)
        }
        arg.list <- c(list(cex.main = cex.main), gen.gp.list, 
            list(main = main))
        do.call("title", arg.list)
    }
    if (is.element(plot.type, c("Summary", "All", "Test Results"))) {
        if (plot.type == "All" & !same.window) 
            dev.new()
        par(usr = c(0, 1, 0, 1))
        o.mar <- par(mar = test.result.mar)
        plot(0:1, 0:1, type = "n", axes = FALSE, main = "")
        if (is.null(user.main)) {
            if (!is.null(captions[[5]])) 
                main <- captions[[5]]
            else main <- paste("Results of", gof.obj$method)
        }
        arg.list <- c(list(cex.main = cex.main), gen.gp.list, 
            list(main = main))
        do.call("title", arg.list)
        o.par <- par(cex = test.result.cex, font = test.result.font)
        char.ht <- par("cxy")[2] * test.result.cex
        text(0, 1, "Hypothesized\nDistribution:", adj = c(0, 
            1))
        text(1, 1 - char.ht, gof.obj$distribution, adj = c(1, 
            1))
        mf <- 3
        text(0, 1 - mf * char.ht, "Data:", adj = c(0, 1))
        text(1, 1 - mf * char.ht, paste(format(names(data.name), 
            justify = "left"), " = ", format(data.name, justify = "right"), 
            "\n", sep = "", collapse = ""), adj = c(1, 1))
        mf <- mf + 3
        if (!is.null(parent.of.data)) {
            text(0, 1 - mf * char.ht, "Data Source:", adj = 0)
            text(1, 1 - mf * char.ht, parent.of.data, adj = 1)
            mf <- mf + 2
        }
        text(0, 1 - mf * char.ht, "Sample Sizes:", adj = c(0, 
            1))
        text(1, 1 - mf * char.ht, paste(format(names(gof.obj$sample.size), 
            justify = "left"), " = ", format(format(gof.obj$sample.size, 
            digits = digits, nsmall = 0)), "\n", sep = "", collapse = ""), 
            adj = c(1, 1))
        mf <- mf + 2 + length(gof.obj$sample.size)
        text(0, 1 - mf * char.ht, "Test Statistic:", adj = 0)
        text(1, 1 - mf * char.ht, paste(names(gof.obj$statistic), 
            format(gof.obj$statistic, digits = digits, nsmall = 0), 
            sep = " = "), adj = 1)
        mf <- mf + 2
        text(0, 1 - mf * char.ht, "Test Statistic Parameters:", 
            adj = c(0, 1))
        text(1, 1 - mf * char.ht, paste(format(names(gof.obj$parameters), 
            justify = "left"), " = ", format(format(gof.obj$parameters, 
            digits = digits, nsmall = 0)), "\n", sep = "", collapse = ""), 
            adj = c(1, 1))
        mf <- mf + 2 + length(gof.obj$parameters)
        text(0, 1 - mf * char.ht, "P-value:", adj = 0)
        text(1, 1 - mf * char.ht, format(gof.obj$p.value, digits = digits, 
            nsmall = 0), adj = 1)
        par(c(o.mar, o.par))
    }
    if (plot.type == "Summary" & add.om.title) {
        if (is.null(om.title)) {
            data.name <- paste(x.name, "and", y.name)
            if (!is.null(parent.of.data)) 
                data.name <- paste(data.name, "in", parent.of.data)
            om.title <- paste("Results of ", gof.obj$method, 
                " Test for\n", data.name, sep = "")
            mtext(om.title, side = 3, line = om.line, outer = TRUE, 
                cex = om.cex.main, font = om.font)
        }
        else {
            mtext(om.title, side = 3, line = om.line, outer = TRUE, 
                cex = om.cex.main, font = om.font)
        }
    }
    invisible(gof.obj)
}
