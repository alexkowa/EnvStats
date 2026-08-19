#' Plots for a Sampling Design Based on a Nonparametric Tolerance Interval
#' @description
#' Create plots involving sample size (\eqn{n}), coverage (\eqn{\beta}), and confidence level
#'   \eqn{(1-\alpha)} for a nonparametric tolerance interval.
#' @usage
#' plotTolIntNparDesign(x.var = "n", y.var = "conf.level", range.x.var = NULL, n = 25,
#'     coverage = 0.95, conf.level = 0.95, ti.type = "two.sided", cov.type = "content",
#'     ltl.rank = ifelse(ti.type == "upper", 0, 1),
#'     n.plus.one.minus.utl.rank = ifelse(ti.type == "lower", 0, 1), plot.it = TRUE,
#'     add = FALSE, n.points = 100, plot.col = "black", plot.lwd = 3 * par("cex"),
#'     plot.lty = 1, digits = .Options$digits, cex.main = par("cex"), ..., main = NULL,
#'     xlab = NULL, ylab = NULL, type = "l")
#' @rawRd
#' \arguments{
#'   \item{x.var}{
#'   character string indicating what variable to use for the x-axis.  Possible values are
#'   \code{"n"} (sample size; the default), \code{"coverage"} (the coverage), and \code{"conf.level"}
#'   (the confidence level).
#' }
#'   \item{y.var}{
#'   character string indicating what variable to use for the y-axis.  Possible values are
#'   \code{"conf.level"} (the confidence level; the default), \code{"n"} (sample size), and
#'   \code{"coverage"} (the coverage).
#' }
#'   \item{range.x.var}{
#'   numeric vector of length 2 indicating the range of the x-variable to use for the plot.  The
#'   default value depends on the value of \code{x.var}.  When \code{x.var="n"} the default value is
#'   \code{c(2,50)}.  When \code{x.var="coverage"} or \code{x.var="conf"}, the default value is
#'   \code{c(0.5, 0.99)}.
#' }
#'   \item{n}{
#'   numeric scalar indicating the sample size.  The default value is \cr
#'   \code{max(25, lpl.rank + n.plus.one.minus.upl.rank + 1)}.
#'   Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf})
#'   values are not allowed.
#'   This argument is ignored if either \code{x.var="n"} or \code{y.var="n"}.
#' }
#'   \item{coverage}{
#'   numeric scalar between 0 and 1 specifying the coverage of the tolerance interval.  The default
#'   value is \code{coverage=0.95}.  This argument is ignored if \cr
#'   \code{x.var="coverage"} or \code{y.var="coverage"}.
#' }
#'   \item{conf.level}{
#'   a scalar between 0 and 1 indicating the confidence level associated with the tolerance interval.
#'   The default value is \code{conf.level=0.95}.  This argument is ignored if \code{x.var="conf.level"}
#'   or \code{y.var="conf.level"}, or if \cr
#'   \code{cov.type="expectation"}.
#' }
#'   \item{ti.type}{
#'   character string indicating what kind of tolerance interval to compute.
#'   The possible values are \code{"two-sided"} (the default), \code{"lower"}, and
#'   \code{"upper"}.
#' }
#'   \item{cov.type}{
#'   character string specifying the coverage type for the tolerance interval.
#'   The possible values are \code{"content"} (\eqn{\beta}-content; the default), and
#'   \code{"expectation"} (\eqn{\beta}-expectation).
#' }
#'   \item{ltl.rank}{
#'   vector of positive integers indicating the rank of the order statistic to use for the lower bound
#'   of the tolerance interval.  If \code{ti.type="two-sided"} or \cr
#'   \code{ti.type="lower"},
#'   the default value is \code{ltl.rank=1} (implying the minimum value of \code{x} is used
#'   as the lower bound of the tolerance interval).  If \cr
#'   \code{ti.type="upper"}, this argument
#'   is set equal to \code{0}.
#' }
#'   \item{n.plus.one.minus.utl.rank}{
#'   vector of positive integers related to the rank of the order statistic to use for
#'   the upper bound of the tolerance interval.  A value of
#'   \code{n.plus.one.minus.utl.rank=1} (the default) means use the
#'   first largest value, and in general a value of \cr
#'   \code{n.plus.one.minus.utl.rank=}\eqn{i} means use the \eqn{i}'th largest value.
#'   If \cr
#'   \code{ti.type="lower"}, this argument is set equal to \code{0}.
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
#'   See the help file for \code{\link{tolIntNpar}}, \code{\link{tolIntNparConfLevel}},
#'   \code{\link{tolIntNparCoverage}}, and \code{\link{tolIntNparN}} for information on how
#'   to compute a nonparametric tolerance interval, how the confidence level
#'   is computed when other quantities are fixed, how the coverage is computed when other
#'   quantites are fixed, and and how the sample size is computed when other quantities are fixed.
#' }
#' @rawRd
#' \value{
#'   \code{plotTolIntNparDesign} invisibly returns a list with components
#'   \code{x.var} and \code{y.var}, giving coordinates of the points that
#'   have been or would have been plotted.
#' }
#' @rawRd
#' \references{
#'   See the help file for \code{\link{tolIntNpar}}.
#' }
#' @rawRd
#' \author{
#'   Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \note{
#'   See the help file for \code{\link{tolIntNpar}}.
#'
#'   In the course of designing a sampling program, an environmental scientist may wish to determine
#'   the relationship between sample size, coverage, and confidence level if one of the objectives of
#'   the sampling program is to produce tolerance intervals.  The functions
#'   \code{\link{tolIntNparN}}, \code{\link{tolIntNparCoverage}}, \code{\link{tolIntNparConfLevel}}, and
#'   \code{plotTolIntNparDesign} can be used to investigate these relationships for
#'   constructing nonparametric tolerance intervals.
#' }
#' @rawRd
#' \seealso{
#'   \code{\link{tolIntNpar}}, \code{\link{tolIntNparConfLevel}}, \code{\link{tolIntNparCoverage}},
#'   \code{\link{tolIntNparN}}.
#' }
#' @rawRd
#' \examples{
#'   # Look at the relationship between confidence level and sample size for a two-sided
#'   # nonparametric tolerance interval.
#'
#'   dev.new()
#'   plotTolIntNparDesign()
#'
#'   #==========
#'
#'   # Plot confidence level vs. sample size for various values of coverage:
#'
#'   dev.new()
#'   plotTolIntNparDesign(coverage = 0.7, ylim = c(0,1), main = "")
#'
#'   plotTolIntNparDesign(coverage = 0.8, add = TRUE, plot.col = "red")
#'
#'   plotTolIntNparDesign(coverage = 0.9, add = TRUE, plot.col = "blue")
#'
#'   legend("bottomright", c("coverage = 70\%", "coverage = 80\%", "coverage = 90\%"), lty=1,
#'     lwd = 3 * par("cex"), col = c("black", "red", "blue"), bty = "n")
#'
#'   title(main = paste("Confidence Level vs. Sample Size for Nonparametric TI",
#'     "with Various Levels of Coverage", sep = "\n"))
#'
#'   #==========
#'
#'   # Example 17-4 on page 17-21 of USEPA (2009) uses copper concentrations (ppb) from 3
#'   # background wells to set an upper limit for 2 compliance wells.  There are 6 observations
#'   # per well, and the maximum value from the 3 wells is set to the 95% confidence upper
#'   # tolerance limit, and we need to determine the coverage of this tolerance interval.
#'
#'   tolIntNparCoverage(n = 24, conf.level = 0.95, ti.type = "upper")
#'   #[1] 0.8826538
#'
#'   # Here we will modify the example and look at confidence level versus coverage for
#'   # a set sample size of n = 24.
#'
#'   dev.new()
#'   plotTolIntNparDesign(x.var = "coverage", y.var = "conf.level", n = 24, ti.type = "upper")
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

plotTolIntNparDesign <-
function (x.var = "n", y.var = "conf.level", range.x.var = NULL, 
    n = 25, coverage = 0.95, conf.level = 0.95, ti.type = "two.sided", 
    cov.type = "content", ltl.rank = ifelse(ti.type == "upper", 
        0, 1), n.plus.one.minus.utl.rank = ifelse(ti.type == 
        "lower", 0, 1), plot.it = TRUE, add = FALSE, n.points = 100, 
    plot.col = "black", plot.lwd = 3 * par("cex"), plot.lty = 1, 
    digits = .Options$digits, cex.main = par("cex"), ..., main = NULL, 
    xlab = NULL, ylab = NULL, type = "l") 
{
    x.var <- match.arg(x.var, c("n", "coverage", "conf.level"))
    y.var <- match.arg(y.var, c("conf.level", "n", "coverage"))
    ti.type <- match.arg(ti.type, c("two.sided", "lower", "upper"))
    cov.type <- match.arg(cov.type, c("content", "expectation"))
    if (x.var == y.var) 
        stop("'x.var' and 'y.var' cannot denote the same quantity")
    if (missing(range.x.var)) {
        range.x.var <- switch(x.var, n = c(2, 50), coverage = c(0.5, 
            0.99), conf.level = c(0.5, 0.99))
    }
    else {
        if (is.null(range.x.var) || !all(is.finite(range.x.var)) || 
            !is.vector(range.x.var, mode = "numeric") || length(range.x.var) != 
            2) 
            stop(paste("'range.x.var' must be a numeric vector of length 2", 
                "with no missing (NA), infinite(-Inf, Inf), or undefined(NaN) values"))
    }
    min.x <- range.x.var[1]
    max.x <- range.x.var[2]
    if (min.x >= max.x) 
        stop("The second element of 'range.x.var' must be larger than the first")
    if (!is.vector(n.points, mode = "numeric") || length(n.points) != 
        1 || n.points != trunc(n.points) || n.points < 2) 
        stop("'n.points' must be an integer larger than 1")
    switch(x.var, n = {
        if (min.x < 2 || min.x != trunc(min.x)) stop(paste("When x.var=\"n\" the first element of", 
            "range.x.var must be an integer greater than 1"))
        if (max.x != trunc(max.x)) stop(paste("When x.var=\"n\" the second element of", 
            "range.x.var must be an integer greater than", "the first element of range.x.var"))
    }, coverage = {
        if (min.x < .Machine$double.eps || min.x > 1 - .Machine$double.eps) stop(paste("When x.var=\"coverage\" the first element of range.x.var", 
            "must be a positive number between 0 and 1"))
        if (max.x > 1 - .Machine$double.eps) stop(paste("When x.var=\"coverage\" the second element of", 
            "range.x.var must be an positive number between 0 and 1", 
            "and greater than the first element of range.x.var"))
    }, conf.level = {
        if (min.x < .Machine$double.eps || min.x > 1 - .Machine$double.eps) stop(paste("When x.var=\"conf.level\" the first element of range.x.var", 
            "must be a positive number between 0 and 1"))
        if (max.x > 1 - .Machine$double.eps) stop(paste("When x.var=\"conf.level\" the second element of", 
            "range.x.var must be an positive number between 0 and 1", 
            "and greater than the first element of range.x.var"))
        if (cov.type != "content") {
            cov.type <- "content"
            warning(paste("When x.var=\"conf.level\" cov.type", 
                "is automatically set to \"content\""))
        }
    })
    if (length(ltl.rank) != 1 || !is.vector(ltl.rank, mode = "numeric") || 
        !is.finite(ltl.rank) || ltl.rank != trunc(ltl.rank) || 
        ltl.rank < 0) 
        stop("'ltl.rank' must be a non-negative integer")
    if (length(n.plus.one.minus.utl.rank) != 1 || !is.vector(n.plus.one.minus.utl.rank, 
        mode = "numeric") || !is.finite(n.plus.one.minus.utl.rank) || 
        n.plus.one.minus.utl.rank != trunc(n.plus.one.minus.utl.rank) || 
        n.plus.one.minus.utl.rank < 0) 
        stop("'n.plus.one.minus.utl.rank' must be a non-negative integer")
    if (y.var == "conf.level" && cov.type != "content") {
        cov.type <- "content"
        warning(paste("When y.var=\"conf.level\" cov.type", "is automatically set to \"content\""))
    }
    if (x.var != "n" && y.var != "n") {
        if (is.null(n) || !is.finite(n) || !is.vector(n, mode = "numeric") || 
            length(n) != 1 || n < 2 || n != trunc(n)) 
            stop("'n' must be an integer greater than 1")
    }
    if (x.var != "coverage" && y.var != "coverage") 
        if (is.null(coverage) || !is.finite(coverage) || !is.vector(coverage, 
            mode = "numeric") || length(coverage) != 1 || coverage <= 
            .Machine$double.eps || coverage >= 1 - .Machine$double.eps) {
            stop("'coverage' must be a scalar between 0 and 1")
        }
    if (x.var != "conf.level" && y.var != "conf.level") 
        if (is.null(conf.level) || !is.finite(conf.level) || 
            !is.vector(conf.level, mode = "numeric") || length(conf.level) != 
            1 || conf.level <= .Machine$double.eps || conf.level >= 
            1 - .Machine$double.eps) {
            stop("'conf.level' must be a scalar between 0 and 1")
        }
    ti.string <- switch(ti.type, two.sided = "(Two-Sided TI)", 
        lower = "(One-Sided Lower TI)", upper = "(One-Sided Upper TI)")
    cov.type.string <- ifelse(cov.type == "content", "Nonparametric B-Content Tolerance Interval", 
        "Nonparametric B-Expectation Tolerance Interval")
    n.string <- paste("n =", n)
    conf.string <- paste("Confidence Level = ", format(100 * 
        conf.level, digits = digits), "%", sep = "")
    cov.string <- paste("Coverage = ", format(100 * coverage, 
        digits = digits), "%", sep = "")
    rank.string <- switch(ti.type, two.sided = paste("Rank(LTL) =", 
        ltl.rank, "and [n + 1 - Rank(UTL)] =", n.plus.one.minus.utl.rank), 
        lower = paste("Rank(LTL) =", ltl.rank), upper = paste("[n + 1 - Rank(UTL)] =", 
            n.plus.one.minus.utl.rank))
    if (plot.it) 
        gen.gp.list <- checkGraphicsPars(...)$gen.gp.list
    combo <- paste(c(x.var, y.var), collapse = " & ")
    switch(combo, `n & conf.level` = {
        x <- seq(min.x, max.x, by = ceiling((max.x - min.x + 
            1)/n.points))
        if (is.null(xlab)) xlab <- "Sample Size (n)"
        y <- tolIntNparConfLevel(n = x, coverage = coverage, 
            ltl.rank = ltl.rank, n.plus.one.minus.utl.rank = n.plus.one.minus.utl.rank, 
            ti.type = ti.type)
        if (is.null(ylab)) ylab <- "Confidence Level"
        line1 <- "Confidence Level vs. Sample Size for"
        line2 <- paste(cov.type.string, "with", cov.string, ti.string)
        line3 <- rank.string
    }, `n & coverage` = {
        x <- seq(min.x, max.x, by = ceiling((max.x - min.x + 
            1)/n.points))
        if (is.null(xlab)) xlab <- "Sample Size (n)"
        y <- tolIntNparCoverage(n = x, conf.level = conf.level, 
            cov.type = cov.type, ltl.rank = ltl.rank, n.plus.one.minus.utl.rank = n.plus.one.minus.utl.rank, 
            ti.type = ti.type)
        if (is.null(ylab)) ylab <- "Coverage"
        line1 <- "Coverage vs. Sample Size for"
        line2 <- ifelse(cov.type == "content", paste(cov.type.string, 
            "with", conf.string, ti.string), paste(cov.type.string, 
            ti.string))
        line3 <- rank.string
    }, `conf.level & n` = {
        x <- seq(min.x, max.x, length = n.points)
        if (is.null(xlab)) xlab <- "Confidence Level"
        y <- tolIntNparN(ltl.rank = ltl.rank, n.plus.one.minus.utl.rank = n.plus.one.minus.utl.rank, 
            coverage = coverage, cov.type = cov.type, ti.type = ti.type, 
            conf.level = x)
        if (is.null(ylab)) ylab <- "Sample Size (n)"
        line1 <- "Sample Size vs. Confidence Level for"
        line2 <- paste(cov.type.string, "with", cov.string, ti.string)
        line3 <- rank.string
    }, `conf.level & coverage` = {
        x <- seq(min.x, max.x, length = n.points)
        if (is.null(xlab)) xlab <- "Confidence Level"
        y <- tolIntNparCoverage(n = n, conf.level = x, cov.type = cov.type, 
            ltl.rank = ltl.rank, n.plus.one.minus.utl.rank = n.plus.one.minus.utl.rank, 
            ti.type = ti.type)
        if (is.null(ylab)) ylab <- "Coverage"
        line1 <- "Coverage vs. Confidence Level for"
        line2 <- paste(cov.type.string, "with", n.string, ti.string)
        line3 <- rank.string
    }, `coverage & conf.level` = {
        x <- seq(min.x, max.x, length = n.points)
        if (is.null(xlab)) xlab <- "Coverage"
        y <- tolIntNparConfLevel(n = n, coverage = x, ltl.rank = ltl.rank, 
            n.plus.one.minus.utl.rank = n.plus.one.minus.utl.rank, 
            ti.type = ti.type)
        if (is.null(ylab)) ylab <- "Confidence Level"
        line1 <- "Confidence Level vs. Coverage for"
        line2 <- paste(cov.type.string, "with", n.string, ti.string)
        line3 <- rank.string
    }, `coverage & n` = {
        x <- seq(min.x, max.x, length = n.points)
        if (is.null(xlab)) xlab <- "Coverage"
        y <- tolIntNparN(ltl.rank = ltl.rank, n.plus.one.minus.utl.rank = n.plus.one.minus.utl.rank, 
            coverage = x, cov.type = cov.type, ti.type = ti.type, 
            conf.level = conf.level)
        if (is.null(ylab)) ylab <- "Sample Size (n)"
        line1 <- "Sample Size vs. Coverage for"
        line2 <- ifelse(cov.type == "content", paste(cov.type.string, 
            "with", conf.string, ti.string), paste(cov.type.string, 
            ti.string))
        line3 <- rank.string
    })
    if (plot.it) {
        if (!add) {
            plot(x, y, type = "n", main = "", sub = "", ..., 
                xlab = xlab, ylab = ylab)
            if (is.null(main)) {
                mtext(text = line1, side = 3, line = 3, cex = 1.2 * 
                  cex.main)
                mtext(text = line2, side = 3, line = 1.75, cex = 1.2 * 
                  cex.main)
                mtext(text = line3, side = 3, line = 0.5, cex = 0.9 * 
                  cex.main)
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
