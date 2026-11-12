#' Plots for Sampling Design Based on Nonparametric Confidence Interval for a Quantile
#' @description
#' Create plots involving sample size, quantile, and confidence level for a
#'   nonparametric confidence interval for a quantile.
#' @usage
#' plotCiNparDesign(x.var = "n", y.var = "conf.level", range.x.var = NULL,
#'     n = 25, p = 0.5, conf.level = 0.95, ci.type = "two.sided",
#'     lcl.rank = ifelse(ci.type == "upper", 0, 1),
#'     n.plus.one.minus.ucl.rank = ifelse(ci.type == "lower", 0, 1),
#'     plot.it = TRUE, add = FALSE, n.points = 100, plot.col = "black",
#'     plot.lwd = 3 * par("cex"), plot.lty = 1, digits = .Options$digits,
#'     cex.main = par("cex"), ..., main = NULL, xlab = NULL, ylab = NULL,
#'     type = "l")
#' @rawRd
#' \arguments{
#'   \item{x.var}{
#'   character string indicating what variable to use for the x-axis.
#'   Possible values are \code{"n"} (sample size; the default),
#'   \code{"p"} (quantile), and \code{"conf.level"} (the confidence level).
#' }
#'   \item{y.var}{
#'   character string indicating what variable to use for the y-axis.
#'   Possible values are \code{conf.level} (confidence level; the default), and
#'   \code{"n"} (sample size).
#' }
#'   \item{range.x.var}{
#'   numeric vector of length 2 indicating the range of the x-variable to use
#'   for the plot.  The default value depends on the value of \code{x.var}.
#'   When \code{x.var="n"} the default value is \code{c(2,50)}.  When
#'   \code{x.var="p"} the default value is \code{c(0.5, 0.99)}.
#'   When \code{x.var="conf.level"}, the default value is \code{c(0.5, 0.99)}.
#' }
#'   \item{n}{
#'   numeric scalar indicating the sample size.  The default value is
#'   \code{n=25}.
#'   Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) values are not allowed.
#'   This argument is ignored if either \code{x.var="n"} or \code{y.var="n"}.
#' }
#'   \item{p}{
#'   numeric scalar specifying the quantile.  The value of this argument must be
#'   between 0 and 1.  The default value is \code{p=0.5}.  The argument is
#'   ignored if \code{x.var="p"}.
#' }
#'   \item{conf.level}{
#'   a scalar between 0 and 1 indicating the confidence level associated with the confidence interval.
#'   The default value is \code{conf.level=0.95}.  This argument is ignored if
#'   \code{x.var="conf.level"} or \code{y.var="conf.level"}.
#' }
#'   \item{ci.type}{
#'   character string indicating what kind of confidence interval to compute.  The
#'   possible values are \code{"two-sided"} (the default), \code{"lower"}, and
#'   \code{"upper"}.
#' }
#'   \item{lcl.rank, n.plus.one.minus.ucl.rank}{
#'   numeric vectors of non-negative integers indicating the ranks of the
#'   order statistics that are used for the lower and upper bounds of the
#'   confidence interval for the specified quantile(s).  When \code{lcl.rank=1}
#'   that means use the smallest value as the lower bound, when \code{lcl.rank=2}
#'   that means use the second to smallest value as the lower bound, etc.
#'   When \code{n.plus.one.minus.ucl.rank=1} that means use the largest value
#'   as the upper bound, when \code{n.plus.one.minus.ucl.rank=2} that means use
#'   the second to largest value as the upper bound, etc.
#'   A value of \code{0} for \code{lcl.rank} indicates no lower bound
#'   (i.e., -Inf) and a value of
#'   \code{0} for \code{n.plus.one.minus.ucl.rank} indicates no upper bound
#'   (i.e., \code{Inf}).  When \cr
#'   \code{ci.type="upper"} then \code{lcl.rank} is
#'   set to \code{0} by default, otherwise it is set to \code{1} by default.
#'   When \code{ci.type="lower"} then \code{n.plus.one.minus.ucl.rank} is set
#'   to \code{0} by default, otherwise it is set to \code{1} by default.
#' }
#'   \item{plot.it}{
#'   a logical scalar indicating whether to create a plot or add to the
#'   existing plot (see \code{add}) on the current graphics device.  If
#'   \code{plot.it=FALSE}, no plot is produced, but a list of (x,y) values
#'   is returned (see VALUE).  The default value is \code{plot.it=TRUE}.
#' }
#'   \item{add}{
#'   a logical scalar indicating whether to add the design plot to the
#'   existing plot (\code{add=TRUE}), or to create a plot from scratch
#'   (\code{add=FALSE}).  The default value is \code{add=FALSE}.
#'   This argument is ignored if \code{plot.it=FALSE}.
#' }
#'   \item{n.points}{
#'   a numeric scalar specifying how many (x,y) pairs to use to produce the plot.
#'   There are \code{n.points} x-values evenly spaced between \code{range.x.var[1]} and \cr
#'   \code{range.x.var[2]}.  The default value is \code{n.points=100}.
#' }
#'   \item{plot.col}{
#'   a numeric scalar or character string determining the color of the plotted
#'   line or points.  The default value is \code{plot.col="black"}.  See the
#'   entry for \code{col} in the help file for \code{\link{par}}
#'   for more information.
#' }
#'   \item{plot.lwd}{
#'   a numeric scalar determining the width of the plotted line.  The default value is
#'   \code{3*par("cex")}.  See the entry for \code{lwd} in the help file for \code{\link{par}}
#'   for more information.
#' }
#'   \item{plot.lty}{
#'   a numeric scalar determining the line type of the plotted line.  The default value is
#'   \code{plot.lty=1}.  See the entry for \code{lty} in the help file for \code{\link{par}}
#'   for more information.
#' }
#'   \item{digits}{
#'   a scalar indicating how many significant digits to print out on the plot.  The default
#'   value is the current setting of \code{\link{options}("digits")}.
#' }
#'   \item{cex.main, main, xlab, ylab, type, \dots}{
#'   additional graphical parameters (see \code{\link{par}}).
#' }
#' }
#' @rawRd
#' \details{
#'   See the help files for \code{\link{eqnpar}}, \code{\link{ciNparConfLevel}},
#'   and \code{\link{ciNparN}} for information on how to compute a
#'   nonparametric confidence interval for a quantile, how the confidence level
#'   is computed when other quantities are fixed, and how the sample size is
#'   computed when other quantities are fixed.
#' }
#' @rawRd
#' \value{
#'   \code{plotCiNparDesign} invisibly returns a list with components
#'   \code{x.var} and \code{y.var}, giving coordinates of the points that
#'   have been or would have been plotted.
#' }
#' @rawRd
#' \references{
#'   See the help file for \code{\link{eqnpar}}.
#' }
#' @rawRd
#' \author{
#'     Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \note{
#'   See the help file for \code{\link{eqnpar}}.
#' }
#' @rawRd
#' \seealso{
#'   \code{\link{eqnpar}}, \code{\link{ciNparConfLevel}},
#'   \code{\link{ciNparN}}.
#' }
#' @rawRd
#' \examples{
#'   # Look at the relationship between confidence level and sample size for
#'   # a two-sided nonparametric confidence interval for the 90'th percentile.
#'
#'   dev.new()
#'   plotCiNparDesign(p = 0.9)
#'
#'   #----------
#'
#'   # Plot sample size vs. quantile for various levels of confidence:
#'
#'   dev.new()
#'   plotCiNparDesign(x.var = "p", y.var = "n", range.x.var = c(0.8, 0.95),
#'     ylim = c(0, 60), main = "")
#'
#'   plotCiNparDesign(x.var = "p", y.var = "n", conf.level = 0.9, add = TRUE,
#'     plot.col = 2, plot.lty = 2)
#'
#'   plotCiNparDesign(x.var = "p", y.var = "n", conf.level = 0.8, add = TRUE,
#'     plot.col = 3, plot.lty = 3)
#'
#'   legend("topleft", c("95\%", "90\%", "80\%"), lty = 1:3, col = 1:3,
#'     lwd = 3 * par('cex'), bty = 'n')
#'
#'   title(main = paste("Sample Size vs. Quantile for ",
#'     "Nonparametric CI for \nQuantile, with ",
#'     "Various Confidence Levels", sep=""))
#'
#'   #==========
#'
#'   # Clean up
#'   #---------
#'   graphics.off()
#' }
#' @rawRd
#' \keyword{ distribution }
#' @rawRd
#' \keyword{ design }
#' @rawRd
#' \keyword{ htest }

plotCiNparDesign <-
function (x.var = "n", y.var = "conf.level", range.x.var = NULL, 
    n = 25, p = 0.5, conf.level = 0.95, ci.type = "two.sided", 
    lcl.rank = ifelse(ci.type == "upper", 0, 1), n.plus.one.minus.ucl.rank = ifelse(ci.type == 
        "lower", 0, 1), plot.it = TRUE, add = FALSE, n.points = 100, 
    plot.col = "black", plot.lwd = 3 * par("cex"), plot.lty = 1, 
    digits = .Options$digits, cex.main = par("cex"), ..., main = NULL, 
    xlab = NULL, ylab = NULL, type = "l") 
{
    x.var <- match.arg(x.var, c("n", "p", "conf.level"))
    y.var <- match.arg(y.var, c("conf.level", "n"))
    ci.type <- match.arg(ci.type, c("two.sided", "lower", "upper"))
    if (x.var == y.var) 
        stop("'x.var' and 'y.var' cannot denote the same quantity")
    if (missing(range.x.var)) {
        range.x.var <- switch(x.var, n = c(2, 50), p = c(0.5, 
            0.99), conf.level = c(0.5, 0.99))
    }
    else {
        if (is.null(range.x.var) || !all(is.finite(range.x.var)) || 
            !is.vector(range.x.var, mode = "numeric") || length(range.x.var) != 
            2 || range.x.var[1] >= range.x.var[2]) 
            stop(paste("'range.x.var' must be a numeric vector of length 2", 
                "with no missing (NA), infinite(-Inf, Inf), or undefined(NaN) values", 
                "and the first element must be less than the second element"))
    }
    min.x <- range.x.var[1]
    max.x <- range.x.var[2]
    if (!is.vector(n.points, mode = "numeric") || length(n.points) != 
        1 || n.points != trunc(n.points) || n.points < 2) 
        stop("'n.points' must be an integer larger than 1")
    switch(x.var, n = {
        if (min.x < 2 || min.x != trunc(min.x)) stop(paste("When x.var=\"n\" the first element of", 
            "range.x.var must be an integer greater than 1"))
        if (max.x != trunc(max.x)) stop(paste("When x.var=\"n\" the second element of", 
            "range.x.var must be an integer greater than", "the first element of range.x.var"))
    }, p = {
        if (min.x < .Machine$double.eps || min.x > 1 - .Machine$double.eps) stop(paste("When x.var=\"p\" the first element of range.x.var", 
            "must be a positive number between 0 and 1"))
        if (max.x > 1 - .Machine$double.eps) stop(paste("When x.var=\"p\" the second element of", 
            "range.x.var must be an positive number between 0 and 1", 
            "and greater than the first element of range.x.var"))
    }, conf.level = {
        if (min.x < .Machine$double.eps || min.x > 1 - .Machine$double.eps) stop(paste("When x.var=\"conf.level\" the first element of range.x.var", 
            "must be a positive number between 0 and 1"))
        if (max.x > 1 - .Machine$double.eps) stop(paste("When x.var=\"conf.level\" the second element of", 
            "range.x.var must be an positive number between 0 and 1", 
            "and greater than the first element of range.x.var"))
    })
    if (length(lcl.rank) != 1 || !is.vector(lcl.rank, mode = "numeric") || 
        !is.finite(lcl.rank) || lcl.rank != trunc(lcl.rank) || 
        lcl.rank < 0) 
        stop("'lcl.rank' must be a non-negative integer")
    if (length(n.plus.one.minus.ucl.rank) != 1 || !is.vector(n.plus.one.minus.ucl.rank, 
        mode = "numeric") || !is.finite(n.plus.one.minus.ucl.rank) || 
        n.plus.one.minus.ucl.rank != trunc(n.plus.one.minus.ucl.rank) || 
        n.plus.one.minus.ucl.rank < 0) 
        stop("'n.plus.one.minus.ucl.rank' must be a non-negative integer")
    if (x.var != "n" && y.var != "n") {
        if (is.null(n) || !is.finite(n) || !is.vector(n, mode = "numeric") || 
            length(n) != 1 || n < 2 || n != trunc(n)) 
            stop("'n' must be an integer greater than 1")
    }
    if (x.var != "p") 
        if (is.null(p) || !is.finite(p) || !is.vector(p, mode = "numeric") || 
            length(p) != 1 || p <= .Machine$double.eps || p >= 
            1 - .Machine$double.eps) {
            stop("'p' must be a scalar between 0 and 1")
        }
    if (x.var != "conf.level" && y.var != "conf.level") 
        if (is.null(conf.level) || !is.finite(conf.level) || 
            !is.vector(conf.level, mode = "numeric") || length(conf.level) != 
            1 || conf.level <= .Machine$double.eps || conf.level >= 
            1 - .Machine$double.eps) {
            stop("'conf.level' must be a scalar between 0 and 1")
        }
    ci.string <- switch(ci.type, two.sided = "(Two-Sided CI)", 
        lower = "(One-Sided Lower CI)", upper = "(One-Sided Upper CI)")
    n.string <- paste("n =", n)
    conf.string <- paste("Confidence Level = ", format(100 * 
        conf.level, digits = digits), "%", sep = "")
    p.string <- paste(format(100 * p, digits = digits), "'th Percentile", 
        sep = "")
    rank.string <- switch(ci.type, two.sided = paste("Rank(LCL) =", 
        lcl.rank, "and [n + 1 - Rank(UCL)] =", n.plus.one.minus.ucl.rank), 
        lower = paste("Rank(LCL) =", lcl.rank), upper = paste("[n + 1 - Rank(UCL)] =", 
            n.plus.one.minus.ucl.rank))
    if (plot.it) 
        gen.gp.list <- checkGraphicsPars(...)$gen.gp.list
    combo <- paste(c(x.var, y.var), collapse = " & ")
    switch(combo, `n & conf.level` = {
        x <- seq(min.x, max.x, by = ceiling((max.x - min.x + 
            1)/n.points))
        if (is.null(xlab)) xlab <- "Sample Size (n)"
        y <- ciNparConfLevel(n = x, p = p, lcl.rank = lcl.rank, 
            n.plus.one.minus.ucl.rank = n.plus.one.minus.ucl.rank, 
            ci.type = ci.type)
        if (is.null(ylab)) ylab <- "Confidence Level"
        line1 <- "Confidence Level vs. Sample Size for Nonparametric"
        line2 <- paste("Confidence Interval for", p.string, ci.string)
        line3 <- rank.string
    }, `conf.level & n` = {
        x <- seq(min.x, max.x, length = n.points)
        if (is.null(xlab)) xlab <- "Confidence Level"
        y <- ciNparN(p = p, lcl.rank = lcl.rank, n.plus.one.minus.ucl.rank = n.plus.one.minus.ucl.rank, 
            ci.type = ci.type, conf.level = x)
        if (is.null(ylab)) ylab <- "Sample Size (n)"
        line1 <- "Sample Size vs. Confidence Level for Nonparametric"
        line2 <- paste("Confidence Interval for", p.string, ci.string)
        line3 <- rank.string
    }, `p & conf.level` = {
        x <- seq(min.x, max.x, length = n.points)
        if (is.null(xlab)) xlab <- "Quantile (p)"
        y <- ciNparConfLevel(n = n, p = x, lcl.rank = lcl.rank, 
            n.plus.one.minus.ucl.rank = n.plus.one.minus.ucl.rank, 
            ci.type = ci.type)
        if (is.null(ylab)) ylab <- "Confidence Level"
        line1 <- "Confidence Level vs. Quantile for Nonparametric"
        line2 <- paste("Confidence Interval with", n.string, 
            ci.string)
        line3 <- rank.string
    }, `p & n` = {
        x <- seq(min.x, max.x, length = n.points)
        if (is.null(xlab)) xlab <- "Quantile (p)"
        y <- ciNparN(p = x, lcl.rank = lcl.rank, n.plus.one.minus.ucl.rank = n.plus.one.minus.ucl.rank, 
            ci.type = ci.type, conf.level = conf.level)
        if (is.null(ylab)) ylab <- "Sample Size (n)"
        line1 <- "Sample Size vs. Quantile for Nonparametric"
        line2 <- paste("Confidence Interval with", conf.string, 
            ci.string)
        line3 <- rank.string
    })
    if (plot.it) {
        if (!add) {
            plot(x, y, type = "n", main = "", sub = "", ..., 
                xlab = xlab, ylab = ylab)
            if (is.null(main)) {
                mtext(text = line1, side = 3, line = 3, cex = 1.25 * 
                  cex.main, font = 2)
                mtext(text = line2, side = 3, line = 1.5, cex = 1.25 * 
                  cex.main, font = 2)
                mtext(text = line3, side = 3, line = 0.25, cex = cex.main)
            }
            else {
                arg.list <- c(list(main = main), gen.gp.list, 
                  list(cex = cex.main))
                do.call("title", arg.list)
            }
            arg.list <- c(list(x = x, y = y), gen.gp.list, list(type = type, 
                col = plot.col, lwd = plot.lwd, lty = plot.lty))
            do.call("lines", arg.list)
        }
        else {
            arg.list <- c(list(x = x, y = y), gen.gp.list, list(type = type, 
                col = plot.col, lwd = plot.lwd, lty = plot.lty))
            do.call("lines", arg.list)
        }
    }
    ret.list <- list(x, y)
    names(ret.list) <- c(x.var, y.var)
    invisible(ret.list)
}
