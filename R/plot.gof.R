#' Plot Results of Goodness-of-Fit Test
#' @description
#' Plot the results of calling the function \code{\link{gofTest}}, which returns an
#'   object of class \code{"gof"} when testing the goodness-of-fit of a set of data
#'   to a distribution (i.e., when supplied with the \code{y} argument but not
#'   the \code{x} argument).  Five different kinds of plots are available.
#'
#'   The function \code{plot.gof} is automatically called by \code{\link{plot}}
#'   when given an object of class \code{"gof"}.  The names of other functions
#'   associated with goodness-of-fit test are listed under \link{Goodness-of-Fit Tests}.
#' @usage
#' \method{plot}{gof}(x, plot.type = "Summary",
#'   captions = list(PDFs = NULL, CDFs = NULL, QQ = NULL, MDQQ = NULL, Results = NULL),
#'   x.labels = list(PDFs = NULL, CDFs = NULL, QQ = NULL, MDQQ = NULL),
#'   y.labels = list(PDFs = NULL, CDFs = NULL, QQ = NULL, MDQQ = NULL),
#'   same.window = FALSE, ask = same.window & plot.type == "All", hist.col = "cyan",
#'   fitted.pdf.col = "black", fitted.pdf.lwd = 3 * par("cex"), fitted.pdf.lty = 1,
#'   plot.pos.con = switch(dist.abb, norm = , lnorm = , lnormAlt = , lnorm3 = 0.375,
#'     evd = 0.44, 0.4), ecdf.col = "cyan", fitted.cdf.col = "black",
#'   ecdf.lwd = 3 * par("cex"), fitted.cdf.lwd = 3 * par("cex"), ecdf.lty = 1,
#'   fitted.cdf.lty = 2, add.line = TRUE,
#'   digits = ifelse(plot.type == "Summary", 2, .Options$digits),
#'   test.result.font = 1,
#'   test.result.cex = ifelse(plot.type == "Summary", 0.9, 1) * par("cex"),
#'   test.result.mar = c(0, 0, 3, 0) + 0.1,
#'   cex.main = ifelse(plot.type == "Summary", 1.2, 1.5) * par("cex"),
#'   cex.axis = ifelse(plot.type == "Summary", 0.9, 1) * par("cex"),
#'   cex.lab = ifelse(plot.type == "Summary", 0.9, 1) * par("cex"),
#'   main = NULL, xlab = NULL, ylab = NULL, xlim = NULL, ylim = NULL,
#'   add.om.title = TRUE,
#'   oma = if (plot.type == "Summary" & add.om.title) c(0, 0, 2.5, 0) else c(0, 0, 0, 0),
#'   om.title = NULL, om.font = 2, om.cex.main = 1.75 * par("cex"), om.line = 0.5, ...)
#' @rawRd
#' \arguments{
#'   \item{x}{
#'   an object of class \code{"gof"}.  See \code{\link{gof.object}} for details.
#' }
#'   \item{plot.type}{
#'   character string indicating what kind of plot to create.  Only one particular
#'   plot type will be created, unless \code{plot.type="All"}, in which case all plots
#'   will be created sequentially.  The possible values of \code{plot.type} are:
#'   \code{"Summary"} (the default), \code{"PDFs: Observed and Fitted"},
#'   \code{"CDFs: Observed and Fitted"}, \code{"Q-Q Plot"}, \code{"Tukey M-D Q-Q Plot"},
#'   \code{"Test Results"}, and \code{"All"}.  See the DETAILS section for more information.
#' }
#'   \item{captions}{
#'   a list with 1 to 5 components with the names \code{"PDFs"}, \code{"CDFs"},
#'   \code{"QQ"}, \code{"MDQQ"}, and/or \code{"Results"}.  Each component either has
#'   the value \code{NULL} or else it is a character string containing the title for that
#'   particular kind of plot.  When the component has the value \code{NULL} (the default),
#'   a default title is used.  This argument is useful when you are creating more than
#'   one kind of plot with a single call to \code{plot.gof} (i.e., when
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
#'   one kind of plot with a single call to \code{plot.gof} (i.e., when
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
#'   one kind of plot with a single call to \code{plot.gof} (i.e., when
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
#'   \item{digits}{
#'   scalar indicating how many significant digits to print for the distribution
#'   parameters.  If \code{plot.type == "Summary"}, the default value is
#'   \code{digits=2}, otherwise it is \code{.Options$digits} (i.e., the current
#'   setting of \code{options("digits")}).
#'   This argument is ignored when \code{plot.type="PDFs: Observed and Fitted"}.
#' }
#'
#'
#'
#' \bold{Arguments associated with \code{plot.type="PDFs: Observed and Fitted"}:} \cr
#'
#'   \item{hist.col}{
#'   a character string or numeric scalar determining the color of the histogram
#'   used to display the distribution of the observed values.  The default value is
#'   \code{hist.col="cyan"}.  See the entry for \code{col} in the \R help file for
#'   \code{\link{par}} for more information.
#' }
#'   \item{fitted.pdf.col}{
#'   a character string or numeric scalar determining the color of the fitted PDF
#'   (which is displayed as a line for continuous distributions and a histogram for
#'   discrete distributions).  The default value is \code{fitted.pdf.col="black"}.
#'   See the entry for \code{col} in the \R help file for \code{\link{par}} for more information.
#' }
#'   \item{fitted.pdf.lwd}{
#'   numeric scalar determining the width of the line used to display the fitted PDF.
#'   The default value is \code{fitted.pdf.lwd=3*par("cex")}.
#'   See the entry for \code{lwd} in the \R help file for \code{\link{par}} for more information.
#' }
#'   \item{fitted.pdf.lty}{
#'   numeric scalar determining the line type used to display the fitted PDF.
#'   The default value is \code{fitted.pdf.lty=1}.
#'   See the entry for \code{lty} in the \R help file for \code{\link{par}} for more information.
#' }
#'
#'
#'
#' \bold{Arguments associated with \code{plot.type="CDFs: Observed and Fitted"}:} \cr
#'
#'   \item{plot.pos.con}{
#'   numeric scalar between 0 and 1 containing the value of the plotting position
#'   constant used to construct the observed (empirical) CDF.  The default value of
#'   \code{plot.pos.con} depends on the value of \code{gof.obj$distribution}
#'   (i.e., the distribution assumed for the goodness-of-fit test).
#'   For the \link[stats:Normal]{normal}, \link[stats:Lognormal]{lognormal}, and
#'   \link[=Lognormal3]{three-parameter lognormal distributions},
#'   the default value is \cr
#'   \code{plot.pos.con=0.375}.  For the \link[=EVD]{Type I extreme value (Gumbel) distribution},
#'   the default value is \code{plot.pos.con=0.44}.  For all other distributions,
#'   the default value is \code{plot.pos.con=0.4}.  See the help files for
#'   \code{\link{ecdfPlot}} and \code{\link{qqPlot}} for more information and the
#'   motivation for these choices of values.
#'
#'   \bold{NOTE:}  This argument is also used to determine the value of the
#'   plotting position constant for the Q-Q plot (\code{plot.type="Q-Q Plot"}), or the
#'   Tukey Mean-Difference Q-Q plot (\code{plot.type="Tukey M-D Q-Q Plot"}).
#' }
#'   \item{ecdf.col}{
#'   a character string or numeric scalar determining the color of the line
#'   used to display the empirical CDF.  The default value is
#'   \code{ecdf.col="cyan"}.  See the entry for \code{col} in the \R help file for
#'   \code{\link{par}} for more information.
#' }
#'   \item{fitted.cdf.col}{
#'   a character string or numeric scalar determining the color of the line used
#'   to display the fitted CDF.  The default value is \code{fitted.cdf.col="black"}.
#'   See the entry for \code{col} in the \R help file for \code{\link{par}} for more information.
#' }
#'   \item{ecdf.lwd}{
#'   numeric scalar determining the width of the line used to display the empirical CDF.
#'   The default value is \code{ecdf.lwd=3*par("cex")}.
#'   See the entry for \code{lwd} in the \R help file for \code{\link{par}} for more information.
#' }
#'   \item{fitted.cdf.lwd}{
#'   numeric scalar determining the width of the line used to display the fitted CDF.
#'   The default value is \code{fitted.cdf.lwd=3*par("cex")}.
#'   See the entry for \code{lwd} in the \R help file for \code{\link{par}} for more information.
#' }
#'   \item{ecdf.lty}{
#'   numeric scalar determining the line type used to display the empirical CDF.
#'   The default value is \code{ecdf.lty=1}.
#'   See the entry for \code{lty} in the \R help file for \code{\link{par}} for more information.
#' }
#'   \item{fitted.cdf.lty}{
#'   numeric scalar determining the line type used to display the fitted CDF.
#'   The default value is \code{fitted.cdf.lty=2}.
#'   See the entry for \code{lty} in the \R help file for \code{\link{par}} for more information.
#' }
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
#'   The function \code{plot.gof} is a method for the generic function
#'   \code{\link{plot}} for objects that inherit from class \code{"gof"} \cr
#'   (see \code{\link{gof.object}}).
#'   It can be invoked by calling \code{\link{plot}} and giving it an object of
#'   class \code{"gof"} as the first argument, or by calling \code{plot.gof}
#'   directly, regardless of the class of the object given as the first argument
#'   to \code{plot.gof}.
#'
#'   Plots associated with the goodness-of-fit test are produced on the current graphics
#'   device.  These can be one or all of the following:
#'   \itemize{
#'   \item Observed distribution overlaid with fitted distribution \cr
#'     (\code{plot.type="PDFs: Observed and Fitted"}).  See the help files for
#'     \code{\link{hist}} and \code{\link{pdfPlot}}.
#'   \item Observed empirical distribution overlaid with fitted cumulative distribution \cr
#'     (\code{plot.type="CDFs: Observed and Fitted"}).  See the help file for
#'     \code{\link{cdfCompare}}.
#'   \item Observed quantiles vs. fitted quantiles (Q-Q Plot)
#'     (\code{plot.type="Q-Q Plot"}).  See the help file for \code{\link{qqPlot}}.
#'   \item Tukey mean-difference Q-Q plot (\code{plot.type="Tukey M-D Q-Q Plot"}).
#'     See the help file for \code{\link{qqPlot}}.
#'   \item Results of the goodness-of-fit test (\code{plot.type="Test Results"}).
#'     See the help file for \code{\link{print.gof}}.
#'   }
#'   See the help file for \code{\link{gofTest}} for more information.
#' }
#' @rawRd
#' \value{
#'   \code{plot.gof} invisibly returns the first argument, \code{x}.
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
#'   \code{\link{gofTest}}, \code{\link{gof.object}}, \code{\link{print.gof}},
#'   \link{Goodness-of-Fit Tests}, \code{\link{plot}}.
#' }
#' @rawRd
#' \examples{
#'   # Create an object of class "gof" then plot the results.
#'   # (Note: the call to set.seed simply allows you to reproduce
#'   # this example.)
#'
#'   set.seed(250)
#'   dat <- rnorm(20, mean = 3, sd = 2)
#'   gof.obj <- gofTest(dat)
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
#'     om.title = "Summary")
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
#'   rm(dat, gof.obj)
#'   graphics.off()
#' }
#' @rawRd
#' \keyword{plot}
#' @exportS3Method NULL

plot.gof <-
function (x, plot.type = "Summary", captions = list(PDFs = NULL, 
    CDFs = NULL, QQ = NULL, MDQQ = NULL, Results = NULL), x.labels = list(PDFs = NULL, 
    CDFs = NULL, QQ = NULL, MDQQ = NULL), y.labels = list(PDFs = NULL, 
    CDFs = NULL, QQ = NULL, MDQQ = NULL), same.window = FALSE, 
    ask = same.window & plot.type == "All", hist.col = "cyan", 
    fitted.pdf.col = "black", fitted.pdf.lwd = 3 * par("cex"), 
    fitted.pdf.lty = 1, plot.pos.con = switch(dist.abb, norm = , 
        lnorm = , lnormAlt = , lnorm3 = 0.375, evd = 0.44, 0.4), 
    ecdf.col = "cyan", fitted.cdf.col = "black", ecdf.lwd = 3 * 
        par("cex"), fitted.cdf.lwd = 3 * par("cex"), ecdf.lty = 1, 
    fitted.cdf.lty = 2, add.line = TRUE, digits = ifelse(plot.type == 
        "Summary", 2, .Options$digits), test.result.font = 1, 
    test.result.cex = ifelse(plot.type == "Summary", 0.9, 1) * 
        par("cex"), test.result.mar = c(0, 0, 3, 0) + 0.1, cex.main = ifelse(plot.type == 
        "Summary", 1.2, 1.5) * par("cex"), cex.axis = ifelse(plot.type == 
        "Summary", 0.9, 1) * par("cex"), cex.lab = ifelse(plot.type == 
        "Summary", 0.9, 1) * par("cex"), main = NULL, xlab = NULL, 
    ylab = NULL, xlim = NULL, ylim = NULL, add.om.title = TRUE, 
    oma = if (plot.type == "Summary" & add.om.title) c(0, 0, 
        2.5, 0) else c(0, 0, 0, 0), om.title = NULL, om.font = 2, 
    om.cex.main = 1.75 * par("cex"), om.line = 0.5, ...) 
{
    gof.obj <- x
    plot.type <- match.arg(plot.type, c("Summary", "All", "PDFs: Observed and Fitted", 
        "CDFs: Observed and Fitted", "Q-Q Plot", "Tukey M-D Q-Q Plot", 
        "Test Results"))
    gof.obj.orig <- gof.obj
    switch(gof.obj$dist.abb, zmnorm = gof.obj$distribution.parameters <- gof.obj$distribution.parameters[c("mean", 
        "sd", "p.zero")], zmlnorm = gof.obj$distribution.parameters <- gof.obj$distribution.parameters[c("meanlog", 
        "sdlog", "p.zero")], zmlnormAlt = gof.obj$distribution.parameters <- gof.obj$distribution.parameters[c("mean", 
        "cv", "p.zero")])
    check.gp.list <- checkGraphicsPars(...)
    gp.arg.list <- check.gp.list$gp.arg.list
    gen.gp.list <- check.gp.list$gen.gp.list
    dist.params.list <- as.list(gof.obj$distribution.parameters)
    dist.params.names <- names(gof.obj$distribution.parameters)
    names(dist.params.list) <- dist.params.names
    n.dist.params <- length(dist.params.names)
    est.method <- gof.obj$estimation.method
    dist.abb <- gof.obj$dist.abb
    dist.name <- gof.obj$distribution
    dist.type <- .Distribution.type[dist.abb]
    discrete <- !any(dist.type == c("Continuous", "Mixed"))
    data <- gof.obj$data
    data.name <- gof.obj$data.name
    data.name.string <- data.name
    parent.of.data <- gof.obj$parent.of.data
    if (!is.null(parent.of.data)) 
        data.name.string <- paste(data.name, "in", parent.of.data)
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
    if (is.element(plot.type, c("Summary", "All", "PDFs: Observed and Fitted"))) {
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
            else xlab <- data.name
        }
        else xlab <- user.xlab
        if (is.null(user.ylab)) {
            if (!is.null(y.labels[[1]])) 
                ylab <- y.labels[[1]]
            else ylab <- "Relative Frequency"
        }
        else ylab <- user.ylab
        if (!discrete) {
            if (!is.null(gof.obj$cut.points) && all(is.finite(range(gof.obj$cut.points)))) {
                hist.list <- hist(data, breaks = gof.obj$cut.points, 
                  plot = FALSE)
                if (is.null(user.xlim)) 
                  xlim <- range(hist.list$breaks)
                pdf.list <- do.call("pdfPlot", list(distribution = dist.abb, 
                  param.list = dist.params.list, plot.it = FALSE, 
                  ..., xlim = xlim))
                if (is.null(user.ylim)) 
                  ylim <- c(0, max(hist.list$density, pdf.list$Probability.Densities))
                hist(data, probability = TRUE, breaks = gof.obj$cut.points, 
                  col = hist.col, main = "", ..., cex.axis = cex.axis, 
                  cex.lab = cex.lab, xlim = xlim, ylim = ylim, 
                  xlab = xlab, ylab = ylab)
            }
            else {
                data.no.0 <- data[data != 0]
                if (dist.type == "Mixed" && all(data.no.0 > 0)) {
                  upper.breaks <- pretty(data.no.0, n = log(length(data.no.0), 
                    base = 2) + 1)
                  if (min(data.no.0) <= upper.breaks[1]) 
                    upper.breaks[1] <- upper.breaks[1] - 1e+08 * 
                      .Machine$double.eps
                  if (upper.breaks[1] > 0) {
                    upper.0.break <- min(0.5, upper.breaks[1] - 
                      1e+08 * .Machine$double.eps)
                    breaks <- c(-upper.0.break, upper.0.break, 
                      upper.breaks)
                  }
                  else breaks <- upper.breaks
                }
                else {
                  breaks <- pretty(data, n = log(length(data), 
                    base = 2) + 1)
                }
                hist.list <- hist(data, breaks = breaks, plot = FALSE)
                if (is.null(user.xlim)) 
                  xlim <- range(hist.list$breaks)
                pdf.list <- do.call("pdfPlot", list(distribution = dist.abb, 
                  param.list = dist.params.list, plot.it = FALSE, 
                  ..., xlim = xlim))
                if (is.null(user.ylim)) 
                  ylim <- c(0, max(hist.list$density, pdf.list$Probability.Densities))
                hist(data, breaks = breaks, probability = TRUE, 
                  col = hist.col, main = "", ..., cex.axis = cex.axis, 
                  cex.lab = cex.lab, xlim = xlim, ylim = ylim, 
                  xlab = xlab, ylab = ylab)
            }
        }
        else {
            y <- tabulate(data - min(data) + 1)/length(data)
            pdf.list <- do.call("pdfPlot", list(distribution = dist.abb, 
                param.list = dist.params.list, plot.it = FALSE, 
                ..., xlim = xlim))
            if (is.null(user.ylim)) 
                ylim <- c(0, max(y, pdf.list$Probability.Densities))
            x <- min(data):max(data)
            nx <- length(x)
            con <- 0.4 + (0.1 * (nx - 2))/nx
            xleft <- x - con
            xright <- x + con
            ybottom <- rep(0, nx)
            if (is.null(user.xlim)) 
                xlim <- c(min(xleft), max(xright))
            plot(x, y, type = "n", xaxt = "n", bty = "n", ..., 
                cex.axis = cex.axis, cex.lab = cex.lab, xlim = xlim, 
                ylim = ylim, xlab = xlab, ylab = ylab)
            rect(xleft = xleft, ybottom = ybottom, xright = xright, 
                ytop = y, col = hist.col, border = fitted.pdf.col, 
                ...)
            axis(1)
        }
        arg.list <- c(list(distribution = dist.abb, param.list = dist.params.list, 
            add = TRUE, pdf.col = fitted.pdf.col, pdf.lwd = fitted.pdf.lwd, 
            pdf.lty = fitted.pdf.lty), gen.gp.list)
        do.call("pdfPlot", arg.list)
        if (is.null(user.main)) {
            if (!is.null(captions[[1]])) 
                main <- captions[[1]]
            else main <- paste("Histogram for ", data.name, " with\nFitted ", 
                gof.obj$distribution, " Distribution", sep = "")
        }
        arg.list <- c(list(cex.main = cex.main), gen.gp.list, 
            list(main = main))
        do.call("title", arg.list)
    }
    if (is.element(plot.type, c("Summary", "All", "CDFs: Observed and Fitted"))) {
        if (plot.type == "All" & !same.window) 
            dev.new()
        if (is.null(user.xlab)) {
            if (!is.null(x.labels[[2]])) 
                xlab <- x.labels[[2]]
            else {
                if (is.null(est.method)) {
                  string <- dist.name
                }
                else {
                  if (any(dist.abb == c("beta", "chisq", "f", 
                    "t"))) {
                    if (dist.params.list$ncp > 0) 
                      string <- paste("Non-central ", dist.name, 
                        "(", paste(paste(dist.params.names, signif(unlist(dist.params.list), 
                          digits), sep = "="), collapse = ", "), 
                        ")", sep = "")
                    else {
                      string <- paste(dist.name, "(", paste(paste(dist.params.names[-n.dist.params], 
                        signif(unlist(dist.params.list[-n.dist.params]), 
                          digits), sep = "="), collapse = ", "), 
                        ")", sep = "")
                    }
                  }
                  else {
                    string <- paste(dist.name, "(", paste(paste(dist.params.names, 
                      signif(unlist(dist.params.list), digits), 
                      sep = "="), collapse = ", "), ")", sep = "")
                  }
                }
                xlab <- paste("Order Statistics for ", data.name, 
                  " and\n", string, " Distribution", sep = "")
            }
        }
        else xlab <- user.xlab
        if (!is.null(user.ylab)) 
            ylab <- user.ylab
        else if (!is.null(y.labels[[2]])) 
            ylab <- y.labels[[2]]
        cdfCompare(data, plot.pos.con = plot.pos.con, distribution = dist.abb, 
            param.list = dist.params.list, estimate.params = FALSE, 
            x.col = ecdf.col, y.or.fitted.col = fitted.cdf.col, 
            x.lwd = ecdf.lwd, y.or.fitted.lwd = fitted.cdf.lwd, 
            x.lty = ecdf.lty, y.or.fitted.lty = fitted.cdf.lty, 
            digits = digits, ..., cex.axis = cex.axis, cex.lab = cex.lab, 
            main = "", xlab = xlab, ylab = ylab, xlim = user.xlim, 
            ylim = user.ylim)
        if (is.null(user.main)) {
            if (!is.null(captions[[2]])) 
                main <- captions[[2]]
            else main <- paste("Empirical CDF for ", data.name, 
                " (solid line)\nwith Fitted ", gof.obj$distribution, 
                " CDF (dashed line)", sep = "", collapse = "")
        }
        arg.list <- c(list(cex.main = cex.main), gen.gp.list, 
            list(main = main))
        do.call("title", arg.list)
    }
    if (is.element(plot.type, c("Summary", "All", "Q-Q Plot"))) {
        if (plot.type == "All" & !same.window) 
            dev.new()
        if (is.null(user.xlab) & !is.null(x.labels[[3]])) 
            xlab <- x.labels[[3]]
        else xlab <- user.xlab
        if (is.null(user.ylab)) {
            if (!is.null(y.labels[[3]])) 
                ylab <- y.labels[[3]]
            else {
                qlab <- switch(dist.abb, lnorm = paste("Log[", 
                  data.name, "]", sep = ""), lnorm3 = paste("Log[", 
                  data.name, "-", format(dist.params.list[["threshold"]], 
                    digits = digits), "]", sep = ""), data.name)
                ylab <- paste("Quantiles of", qlab)
            }
        }
        else ylab <- user.ylab
        qqPlot(data, distribution = dist.abb, param.list = dist.params.list, 
            plot.pos.con = plot.pos.con, digits = digits, add.line = add.line, 
            qq.line.type = "0-1", ..., cex.axis = cex.axis, cex.lab = cex.lab, 
            xlab = xlab, ylab = ylab, main = "", xlim = user.xlim, 
            ylim = user.ylim)
        if (is.null(user.main)) {
            if (!is.null(captions[[3]])) 
                main <- captions[[3]]
            else {
                main <- paste("Q-Q Plot for ", data.name, " Fitted to\n", 
                  gof.obj$distribution, " Distribution", sep = "")
                if (add.line) 
                  main <- paste(main, ", with 0-1 Line", sep = "")
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
        if (is.null(user.ylab) & !is.null(y.labels[[4]])) 
            ylab <- y.labels[[4]]
        else ylab <- user.ylab
        qqPlot(data, distribution = dist.abb, param.list = dist.params.list, 
            plot.type = "Tukey Mean-Difference Q-Q", add.line = add.line, 
            digits = digits, ..., cex.axis = cex.axis, cex.lab = cex.lab, 
            main = "", xlab = xlab, ylab = ylab, xlim = user.xlim, 
            ylim = user.ylim)
        if (is.null(user.main)) {
            if (!is.null(captions[[4]])) 
                main <- captions[[4]]
            else main <- paste("Tukey Mean-Difference Q-Q Plot\nfor ", 
                data.name, " Fitted to ", gof.obj$distribution, 
                " Distribution", sep = "")
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
            else main <- gof.obj$method
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
        if (gof.obj$n.param.est > 0) {
            text(0, 1 - mf * char.ht, "Estimated Parameters:", 
                adj = c(0, 1))
            text(1, 1 - mf * char.ht, paste(format(names(gof.obj$distribution.parameters), 
                justify = "left"), " = ", format(format(gof.obj$distribution.parameters, 
                digits = digits, nsmall = 0)), "\n", sep = "", 
                collapse = ""), adj = c(1, 1))
            mf <- mf + 2 + length(gof.obj$distribution.parameters) - 
                1
        }
        text(0, 1 - mf * char.ht, "Data:", adj = 0)
        text(1, 1 - mf * char.ht, gof.obj$data.name, adj = 1)
        mf <- mf + 2
        if (!is.null(gof.obj$parent.of.data)) {
            text(0, 1 - mf * char.ht, "Data Source:", adj = 0)
            text(1, 1 - mf * char.ht, gof.obj$parent.of.data, 
                adj = 1)
            mf <- mf + 2
        }
        text(0, 1 - mf * char.ht, "Sample Size:", adj = 0)
        text(1, 1 - mf * char.ht, gof.obj$sample.size, adj = 1)
        mf <- mf + 2
        text(0, 1 - mf * char.ht, "Test Statistic:", adj = 0)
        text(1, 1 - mf * char.ht, paste(names(gof.obj$statistic), 
            format(gof.obj$statistic, digits = digits, nsmall = 0), 
            sep = " = "), adj = 1)
        mf <- mf + 2
        n.params <- length(gof.obj$parameters)
        if (n.params > 0) {
            text(0, 1 - mf * char.ht, "Test Statistic Parameter:", 
                adj = 0)
            text(1, 1 - mf * char.ht, paste(format(names(gof.obj$parameters), 
                justify = "left"), format(gof.obj$parameters, 
                digits = digits, nsmall = 0), sep = " = "), adj = 1)
            mf <- mf + 2
        }
        text(0, 1 - mf * char.ht, "P-value:", adj = 0)
        text(1, 1 - mf * char.ht, format(gof.obj$p.value, digits = digits, 
            nsmall = 0), adj = 1)
        par(c(o.mar, o.par))
    }
    if (plot.type == "Summary" & add.om.title) {
        if (is.null(om.title)) 
            om.title <- paste("Goodness-of-Fit Results for", 
                data.name.string)
        mtext(om.title, side = 3, line = om.line, outer = TRUE, 
            cex = om.cex.main, font = om.font)
    }
    invisible(gof.obj.orig)
}
