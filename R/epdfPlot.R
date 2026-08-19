#' Plot Empirical Probability Density Function
#' @rawRd \alias{empirical PDF}
#' @description
#' Produces an empirical probability density function plot.
#' @usage
#' epdfPlot(x, discrete = FALSE, density.arg.list = NULL, plot.it = TRUE,
#'     add = FALSE, epdf.col = "black", epdf.lwd = 3 * par("cex"), epdf.lty = 1,
#'     curve.fill = FALSE, curve.fill.col = "cyan", ...,
#'     type = ifelse(discrete, "h", "l"), main = NULL, xlab = NULL, ylab = NULL,
#'     xlim = NULL, ylim = NULL)
#' @rawRd
#' \arguments{
#'   \item{x}{
#'   numeric vector of observations.  Missing (\code{NA}), undefined (\code{NaN}), and
#'   infinite (\code{Inf}, \code{-Inf}) values are allowed but will be removed.
#' }
#'   \item{discrete}{
#'   logical scalar indicating whether the assumed parent distribution of \code{x} is
#'   discrete (\code{discrete=TRUE}) or continuous (\code{discrete=FALSE}; the default).
#' }
#'   \item{density.arg.list}{
#'   list with arguments to the \code{\link{density}} function.  The default value is \cr
#'   \code{density.arg.list=NULL}.  This argument is ignored if \code{discrete=TRUE}.
#' }
#'   \item{plot.it}{
#'   logical scalar indicating whether to produce a plot or add to the current plot (see \code{add})
#'   on the current graphics device.  The default value is \code{plot.it=TRUE}.
#' }
#'   \item{add}{
#'   logical scalar indicating whether to add the empirical pdf to the current plot
#'   (\code{add=TRUE}) or generate a new plot (\code{add=FALSE}; the default).
#'   This argument is ignored if \code{plot.it=FALSE}.
#' }
#'   \item{epdf.col}{
#'   a numeric scalar or character string determining the color of the empirical pdf
#'   line or points.  The default value is \code{epdf.col="black"}.
#'   See the entry for \code{col} in the help file for
#'   \code{\link{par}} for more information.
#' }
#'   \item{epdf.lwd}{
#'   a numeric scalar determining the width of the empirical pdf line.
#'   The default value is \code{epdf.lwd=3*par("cex")}.  See the entry for
#'   \code{lwd} in the help file for \code{\link{par}}
#'   for more information.
#' }
#'   \item{epdf.lty}{
#'   a numeric scalar determining the line type of the empirical pdf line.
#'   The default value is \code{ecdf.lty=1}.  See the entry for \code{lty} in the help file for \code{\link{par}}
#'   for more information.
#' }
#'   \item{curve.fill}{
#'   a logical scalar indicating whether to fill in the area below the empirical pdf
#'   curve with the
#'   color specified by \code{curve.fill.col}. The default value is \cr
#'   \code{curve.fill=FALSE}.
#' }
#'   \item{curve.fill.col}{
#'   a numeric scalar or character string indicating what color to use to fill in the
#'   area below the empirical pdf curve.  The default value is
#'   \code{curve.fill.col="cyan"}.  This argument is ignored if \code{curve.fill=FALSE}.
#' }
#'   \item{type, main, xlab, ylab, xlim, ylim, \dots}{
#'   additional graphical parameters (see \code{\link{lines}} and \code{\link{par}}).
#'   In particular, the argument \code{type} specifies the kind of line type.
#'   By default, the function \code{epdfPlot} plots histogram-like vertical lines
#'   (\code{type="h"}) when \code{discrete=TRUE}, and
#'   plots a straight line between points (\code{type="l"}) when \code{discrete=FALSE}.
#'   The user may override these defaults by supplying the graphics parameter \code{type}
#'   (\code{type="h"} for histogram-like vertical lines, \code{type="l"} for linear
#'   interpolation, \code{type="p"} for points only, etc.).
#' }
#' }
#' @rawRd
#' \details{
#'   When a distribution is discrete and can only take on a finite number of values,
#'   the empirical pdf plot is the same as the standard relative frequency histogram;
#'   that is, each bar of the histogram represents the proportion of the sample
#'   equal to that particular number (or category).  When a distribution is continuous,
#'   the function \code{epdfPlot} calls the \R function \code{\link{density}} to
#'   compute the estimated probability density at a number of evenly spaced points
#'   between the minimum and maximum values.
#' }
#' @rawRd
#' \value{
#'   \code{epdfPlot} invisibly returns a list with the following components:
#'
#'   \item{x}{numeric vector of ordered quantiles.}
#'   \item{f.x}{numeric vector of the associated estimated values of the pdf.}
#' }
#' @rawRd
#' \references{
#'   Chambers, J.M., W.S. Cleveland, B. Kleiner, and P.A. Tukey. (1983).
#'   \emph{Graphical Methods for Data Analysis}. Duxbury Press, Boston, MA.
#'
#'   See the REFERENCES section in the help file for \code{\link{density}}.
#' }
#' @rawRd
#' \author{
#'     Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \note{
#'   An \bold{\emph{empirical probability density function (epdf) plot}} is a
#'   graphical tool that can be used in conjunction with other graphical tools
#'   such as \link[=hist]{histograms} and \link[=boxplot]{boxplots} to assess
#'   the characteristics of a set of data.
#' }
#' @rawRd
#' \seealso{
#'   \link{Empirical}, \code{\link{pdfPlot}}, \code{\link{ecdfPlot}},
#'   \code{\link{cdfPlot}}, \code{\link{cdfCompare}}, \code{\link{qqPlot}}.
#' }
#' @rawRd
#' \examples{
#'   # Using Reference Area TcCB data in EPA.94b.tccb.df,
#'   # create a histogram of the log-transformed observations,
#'   # then superimpose the empirical pdf plot.
#'
#'   dev.new()
#'   log.TcCB <- with(EPA.94b.tccb.df, log(TcCB[Area == "Reference"]))
#'
#'   hist(log.TcCB, freq = FALSE, xlim = c(-2, 1),
#'     col = "cyan", xlab = "log [ TcCB (ppb) ]",
#'     ylab = "Relative Frequency",
#'     main = "Reference Area TcCB with Empirical PDF")
#'
#'   epdfPlot(log.TcCB, add = TRUE)
#'
#'   #==========
#'
#'   # Generate 20 observations from a Poisson distribution with
#'   # parameter lambda = 10, and plot the empirical PDF.
#'
#'   set.seed(875)
#'   x <- rpois(20, lambda = 10)
#'   dev.new()
#'   epdfPlot(x, discrete = TRUE)
#'
#'   #==========
#'
#'   # Clean up
#'   #---------
#'   rm(log.TcCB, x)
#'   graphics.off()
#' }
#' @rawRd
#' \keyword{distribution}
#' @rawRd
#' \keyword{hplot}

epdfPlot <-
function (x, discrete = FALSE, density.arg.list = NULL, plot.it = TRUE, 
    add = FALSE, epdf.col = "black", epdf.lwd = 3 * par("cex"), 
    epdf.lty = 1, curve.fill = FALSE, curve.fill.col = "cyan", 
    ..., type = ifelse(discrete, "h", "l"), main = NULL, xlab = NULL, 
    ylab = NULL, xlim = NULL, ylim = NULL) 
{
    if (!is.vector(x, mode = "numeric") || is.factor(x)) 
        stop("'x' must be a numeric vector")
    data.name <- deparse(substitute(x))
    if ((bad.obs <- sum(!(x.ok <- is.finite(x)))) > 0) {
        is.not.finite.warning(x)
        x <- x[x.ok]
        warning(paste(bad.obs, "observations with NA/NaN/Inf in 'x' removed."))
    }
    n.x <- length(x)
    x <- sort(x)
    if (discrete) {
        f.x <- as.numeric(table(x)/n.x)
        x <- unique(x)
    }
    else {
        density.list <- do.call("density", args = c(list(x = x), 
            density.arg.list))
        x <- density.list$x
        f.x <- density.list$y
    }
    names(x) <- NULL
    names(f.x) <- NULL
    if (plot.it) {
        if (!add) {
            if (is.null(main)) 
                main <- paste("Empirical PDF of", data.name)
            if (is.null(xlab)) 
                xlab <- data.name
            if (is.null(ylab)) 
                ylab <- "Relative Frequency"
            if (is.null(xlim)) 
                xlim <- range(x)
            if (is.null(ylim)) 
                ylim <- c(0, max(f.x))
            plot(x, f.x, type = "n", ..., xlim = xlim, ylim = ylim, 
                xlab = xlab, ylab = ylab, main = main)
            arg.list <- list(x = x, y = f.x)
            arg.list <- c(arg.list, checkGraphicsPars(...)$gen.gp.list, 
                list(type = type, col = epdf.col, lwd = epdf.lwd, 
                  lty = epdf.lty))
            do.call("lines", arg.list)
        }
        else lines(x, f.x, ..., type = type, col = epdf.col, 
            lwd = epdf.lwd, lty = epdf.lty)
        if ((!discrete) && curve.fill) {
            n <- length(f.x)
            polygon(c(x, rev(x)), c(f.x, rep(0, n)), border = FALSE, 
                col = curve.fill.col)
        }
    }
    invisible(list(x = x, f.x = f.x))
}
