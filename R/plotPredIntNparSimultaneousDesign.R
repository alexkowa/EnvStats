#' Plots for a Sampling Design Based on a Simultaneous Nonparametric Prediction Interval
#' @description
#' Create plots involving sample size (\eqn{n}), number of future observations
#'   (\eqn{m}), minimum number of future observations the interval should contain
#'   (\eqn{k}), number of future sampling occasions (\eqn{r}), and confidence level
#'   \eqn{(1-\alpha)} for a simultaneous nonparametric prediction interval.
#' @usage
#' plotPredIntNparSimultaneousDesign(x.var = "n", y.var = "conf.level",
#'     range.x.var = NULL, n = max(25, lpl.rank + n.plus.one.minus.upl.rank + 1),
#'     n.median = 1, k = 1, m = ifelse(x.var == "k", ceiling(max.x), 1), r = 2,
#'     rule = "k.of.m", conf.level = 0.95, pi.type = "upper",
#'     lpl.rank = ifelse(pi.type == "upper", 0, 1),
#'     n.plus.one.minus.upl.rank = ifelse(pi.type == "lower", 0, 1), n.max = 5000,
#'     maxiter = 1000, integrate.args.list = NULL, plot.it = TRUE, add = FALSE,
#'     n.points = 100, plot.col = "black", plot.lwd = 3 * par("cex"), plot.lty = 1,
#'     digits = .Options$digits, cex.main = par("cex"), ..., main = NULL,
#'     xlab = NULL, ylab = NULL, type = "l")
#' @rawRd
#' \arguments{
#'   \item{x.var}{
#'   character string indicating what variable to use for the x-axis.
#'   Possible values are \code{"n"} (sample size; the default),
#'   \code{"conf.level"} (the confidence level), \code{"k"} (minimum number of
#'   future observations the interval should contain), \code{"m"} (number of
#'   future observations), and \code{"r"} (number of future sampling occasions).
#' }
#'   \item{y.var}{
#'   character string indicating what variable to use for the y-axis.
#'   Possible values are \code{"conf.level"} (confidence level; the default), and
#'   \code{"n"} (sample size).
#' }
#'   \item{range.x.var}{
#'   numeric vector of length 2 indicating the range of the x-variable to use
#'   for the plot.  The default value depends on the value of \code{x.var}.
#'   When \code{x.var="n"} the default value is \code{c(2,50)}.
#'   When \code{x.var="conf.level"}, the default value is \code{c(0.5, 0.99)}.
#'   When \code{x.var="k"}, \code{x.var="m"}, or \code{x.var="r"}, the default value
#'   is c(1, 20).
#' }
#'   \item{n}{
#'   numeric scalar indicating the sample size.  The default value is \cr
#'   \code{max(25, lpl.rank + n.plus.one.minus.upl.rank + 1)}.
#'   Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf})
#'   values are not allowed.
#'   This argument is ignored if either \code{x.var="n"} or \code{y.var="n"}.
#' }
#'   \item{n.median}{
#'   positive odd integer specifying the sample size associated with the future medians.
#'   The default value is \code{n.median=1} (i.e., individual observations).  Note that
#'   all future medians must be based on the same sample size.
#' }
#'   \item{k}{
#'   for the \eqn{k}-of-\eqn{m} rule (\code{rule="k.of.m"}), a positive integer
#'   specifying the minimum number of observations (or medians) out of \eqn{m}
#'   observations (or medians) (all obtained on one future sampling \dQuote{occassion})
#'   the prediction interval should contain.
#'   The default value is \code{k=1}.  This argument is ignored when the argument
#'   \code{rule} is not equal to \code{"k.of.m"}.
#' }
#'   \item{m}{
#'   positive integer specifying the maximum number of future observations (or
#'   medians) on one future sampling \dQuote{occasion}.
#'   The default value is \code{m=2}, except when \code{rule="Modified.CA"}, in which
#'   case this argument is ignored and \code{m} is automatically set equal to \code{4}.
#' }
#'   \item{r}{
#'   positive integer specifying the number of future sampling \dQuote{occasions}.
#'   The default value is \code{r=1}.
#' }
#'   \item{rule}{
#'   character string specifying which rule to use.  The possible values are
#'   \code{"k.of.m"} (\eqn{k}-of-\eqn{m} rule; the default), \code{"CA"} (California rule),
#'   and \code{"Modified.CA"} (modified California rule).
#'   See the DETAILS section below for more information.
#' }
#'   \item{conf.level}{
#'   numeric scalar between 0 and 1 indicating the confidence level
#'   associated with the prediction interval.  The default value is
#'   \code{conf.level=0.95}.
#' }
#'   \item{pi.type}{
#'   character string indicating what kind of prediction interval to compute.
#'   The possible values are \code{"upper"} (the default) and \code{"lower"}.
#' }
#'   \item{lpl.rank}{
#'   non-negative integer indicating the rank of the order statistic to use for
#'   the lower bound of the prediction interval.  If \code{pi.type="lower"}, the
#'   default value is \code{lpl.rank=1} (implying the minimum value is used as the
#'   lower bound of the prediction interval).  If \code{pi.type="upper"}, this
#'   argument is set equal to \code{0}.
#' }
#'   \item{n.plus.one.minus.upl.rank}{
#'   non-negative integer related to the rank of the order statistic to use for
#'   the upper bound of the prediction interval.  A value of
#'   \code{n.plus.one.minus.upl.rank=1} (the default) means use the
#'   first largest value, and in general a value of \cr
#'   \code{n.plus.one.minus.upl.rank=}\eqn{i} means use the \eqn{i}'th largest value.
#'   If \cr
#'   \code{pi.type="lower"}, this argument is set equal to \code{0}.
#' }
#'   \item{n.max}{
#'   numeric scalar indicating the maximum sample size to consider when \code{y.var="n"}.
#'   This argument is used in the search algorithm to determine the required sample size.
#'   The default value is \code{n.max=5000}.
#' }
#'   \item{maxiter}{
#'   positive integer indicating the maximum number of iterations to use in the
#'   \code{\link{uniroot}} search algorithm when \code{y.var="n"}.  The default value is
#'   \code{maxiter=1000}.
#' }
#'   \item{integrate.args.list}{
#'   list of arguments to supply to the \code{\link{integrate}} function.  The default
#'   value is \code{NULL}.
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
#'   See the help file for \code{\link{predIntNparSimultaneous}},
#'   \code{\link{predIntNparSimultaneousConfLevel}}, and \cr
#'   \code{\link{predIntNparSimultaneousN}} for information on how to compute a
#'   simultaneous nonparametric prediction interval, how the confidence level
#'   is computed when other quantities are fixed, and how the sample size is
#'   computed when other quantities are fixed.
#' }
#' @rawRd
#' \value{
#'   \code{plotPredIntNparSimultaneousDesign} invisibly returns a list with components
#'   \code{x.var} and \code{y.var}, giving coordinates of the points that
#'   have been or would have been plotted.
#' }
#' @rawRd
#' \references{
#'   See the help file for \code{\link{predIntNparSimultaneous}}.
#' }
#' @rawRd
#' \author{
#'   Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \note{
#'   See the help file for \code{\link{predIntNparSimultaneous}}.
#' }
#' @rawRd
#' \seealso{
#'   \code{\link{predIntNparSimultaneous}},
#'   \code{\link{predIntNparSimultaneousConfLevel}}, \cr
#'   \code{\link{predIntNparSimultaneousN}},
#'   \code{\link{predIntNparSimultaneousTestPower}},
#'   \code{\link{predIntNpar}}, \code{\link{tolIntNpar}}.
#' }
#' @rawRd
#' \examples{
#'   # For the 1-of-3 rule with r=20 future sampling occasions, look at the
#'   # relationship between confidence level and sample size for a one-sided
#'   # upper simultaneous nonparametric prediction interval.
#'
#'   dev.new()
#'   plotPredIntNparSimultaneousDesign(k = 1, m = 3, r = 20, range.x.var = c(2, 20))
#'
#'   #==========
#'
#'   # Plot confidence level vs. sample size for various values of number of
#'   # future sampling occasions (r):
#'
#'   dev.new()
#'   plotPredIntNparSimultaneousDesign(m = 3, r = 10, rule = "CA",
#'     ylim = c(0, 1), main = "")
#'
#'   plotPredIntNparSimultaneousDesign(m = 3, r = 20, rule = "CA", add = TRUE,
#'     plot.col = "red")
#'
#'   plotPredIntNparSimultaneousDesign(m = 3, r = 30, rule = "CA", add = TRUE,
#'     plot.col = "blue")
#'
#'   legend("bottomright", c("r=10", "r=20", "r=30"), lty = 1, lwd = 3 * par("cex"),
#'     col = c("black", "red", "blue"), bty = "n")
#'
#'   title(main = paste("Confidence Level vs. Sample Size for Simultaneous",
#'     "Nonparametric PI with Various Values of r", sep="\n"))
#'
#'   #==========
#'
#'   # Modifying Example 19-5 of USEPA (2009, p. 19-33), plot confidence level
#'   # versus sample size (number of background observations requried) for
#'   # a 1-of-3 plan assuming r = 10 compliance wells (future sampling occasions).
#'
#'   dev.new()
#'   plotPredIntNparSimultaneousDesign(k = 1, m = 3, r = 10, rule = "k.of.m")
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

plotPredIntNparSimultaneousDesign <-
function (x.var = "n", y.var = "conf.level", range.x.var = NULL, 
    n = max(25, lpl.rank + n.plus.one.minus.upl.rank + 1), n.median = 1, 
    k = 1, m = ifelse(x.var == "k", ceiling(max.x), 1), r = 2, 
    rule = "k.of.m", conf.level = 0.95, pi.type = "upper", lpl.rank = ifelse(pi.type == 
        "upper", 0, 1), n.plus.one.minus.upl.rank = ifelse(pi.type == 
        "lower", 0, 1), n.max = 5000, maxiter = 1000, integrate.args.list = NULL, 
    plot.it = TRUE, add = FALSE, n.points = 100, plot.col = "black", 
    plot.lwd = 3 * par("cex"), plot.lty = 1, digits = .Options$digits, 
    cex.main = par("cex"), ..., main = NULL, xlab = NULL, ylab = NULL, 
    type = "l") 
{
    x.var <- match.arg(x.var, c("n", "conf.level", "k", "m", 
        "r"))
    y.var <- match.arg(y.var, c("conf.level", "n"))
    rule <- match.arg(rule, c("k.of.m", "CA", "Modified.CA"))
    if (x.var == y.var) 
        stop("'x.var' and 'y.var' cannot denote the same quantity")
    if (x.var == "k" && y.var == "n") 
        stop(paste("The combination of x.var=\"k\" and", "y.var=\"n\" is currently not allowed"))
    if (rule != "k.of.m" && x.var == "k") 
        stop(paste("When rule=\"", rule, "\" you cannot set x.var=\"k\"", 
            sep = ""))
    if (rule == "Modified.CA" && x.var == "m") 
        stop("When rule=\"Modified.CA\" you cannot set x.var=\"m\"")
    if (missing(range.x.var)) {
        range.x.var <- switch(x.var, n = c(ifelse(pi.type == 
            "lower", lpl.rank + 1, n.plus.one.minus.upl.rank + 
            1), 50), conf.level = c(0.5, 0.99), k = c(1, 20), 
            m = c(1, 20), r = c(1, 20))
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
    }, conf.level = {
        if (min.x < .Machine$double.eps || min.x > 1 - .Machine$double.eps) stop(paste("When x.var=\"conf.level\" the first element of range.x.var", 
            "must be a positive number between 0 and 1"))
        if (max.x > 1 - .Machine$double.eps) stop(paste("When x.var=\"conf.level\" the second element of", 
            "range.x.var must be an positive number between 0 and 1", 
            "and greater than the first element of range.x.var"))
    }, k = {
        if (min.x < 1 || min.x != trunc(min.x)) stop(paste("When x.var=\"k\" the first element of", 
            "range.x.var must be an integer greater than 0"))
        if (max.x != trunc(max.x)) stop(paste("When x.var=\"k\" the second element of", 
            "range.x.var must be an integer greater than", "the first element of range.x.var"))
    }, m = {
        if (min.x < 1 || min.x != trunc(min.x)) stop(paste("When x.var=\"m\" the first element of", 
            "range.x.var must be an integer greater than 0"))
        if (max.x != trunc(max.x)) stop(paste("When x.var=\"m\" the second element of", 
            "range.x.var must be an integer greater than", "the first element of range.x.var"))
    }, r = {
        if (min.x < 1 || min.x != trunc(min.x)) stop(paste("When x.var=\"r\" the first element of", 
            "range.x.var must be an integer greater than 0"))
        if (max.x != trunc(max.x)) stop(paste("When x.var=\"r\" the second element of", 
            "range.x.var must be an integer greater than", "the first element of range.x.var"))
    })
    if (!is.vector(n.max, mode = "numeric") || length(n.max) != 
        1 || !is.finite(n.max) || n.max != trunc(n.max) || n.max < 
        2) 
        stop("'n.max' must be a positive integer greater than 1")
    if (!is.vector(maxiter, mode = "numeric") || length(maxiter) != 
        1 || !is.finite(maxiter) || maxiter != trunc(maxiter) || 
        maxiter < 2) 
        stop("'maxiter' must be a positive integer greater than 1")
    pi.type <- match.arg(pi.type, c("upper", "lower"))
    if (pi.type == "upper") 
        lpl.rank <- 0
    else n.plus.one.minus.upl.rank <- 0
    if (length(lpl.rank) != 1 || !is.vector(lpl.rank, mode = "numeric") || 
        !is.finite(lpl.rank) || lpl.rank != trunc(lpl.rank) || 
        lpl.rank < 0 || lpl.rank >= n.max) 
        stop("'lpl.rank' must be a non-negative integer less than 'n.max'")
    if (pi.type == "lower" & lpl.rank < 1) 
        stop("When pi.type='lower', 'lpl.rank' must be a positive integer")
    if (length(n.plus.one.minus.upl.rank) != 1 || !is.vector(n.plus.one.minus.upl.rank, 
        mode = "numeric") || !is.finite(n.plus.one.minus.upl.rank) || 
        n.plus.one.minus.upl.rank != trunc(n.plus.one.minus.upl.rank) || 
        n.plus.one.minus.upl.rank < 0 || n.plus.one.minus.upl.rank >= 
        n.max) 
        stop("'n.plus.one.minus.upl.rank' must be a non-negative integer less than 'n.max'")
    if (pi.type == "upper" & n.plus.one.minus.upl.rank < 1) 
        stop(paste("When pi.type='two.sided' or pi.type='upper',", 
            "'n.plus.one.minus.upl.rank' must be a positive integer"))
    if (x.var != "n" && y.var != "n") {
        if (is.null(n) || !is.finite(n) || !is.vector(n, mode = "numeric") || 
            length(n) != 1 || n < 2 || n != trunc(n)) 
            stop("'n' must be an integer greater than 1")
        if (pi.type == "lower" && lpl.rank >= n) 
            stop("When pi.type='lower', 'lpl.rank' must be less than 'n'")
        if (pi.type == "upper" && n.plus.one.minus.upl.rank >= 
            n) 
            stop("When pi.type='lower', 'n.plus.one.minus.upl.rank' must be less than 'n'")
    }
    if (is.null(n.median) || !is.finite(n.median) || !is.vector(n.median, 
        mode = "numeric") || length(n.median) != 1 || n.median < 
        1 || n.median != trunc(n.median) || !is.odd(n.median)) 
        stop("'n.median' must be a positive odd integer")
    if (x.var != "conf.level" && y.var != "conf.level") {
        if (is.null(conf.level) || !is.finite(conf.level) || 
            !is.vector(conf.level, mode = "numeric") || length(conf.level) != 
            1 || conf.level <= .Machine$double.eps || conf.level >= 
            1 - .Machine$double.eps) {
            stop("'conf.level' must be a scalar between 0 and 1")
        }
    }
    if (x.var != "m") {
        if (is.null(m) || !is.finite(m) || !is.vector(m, mode = "numeric") || 
            length(m) != 1 || m < 1 || m != trunc(m)) 
            stop("'m' must be a positive integer")
    }
    if (x.var != "r") {
        if (is.null(r) || !is.finite(r) || !is.vector(r, mode = "numeric") || 
            length(r) != 1 || r < 1 || r != trunc(r)) 
            stop("'r' must be a positive integer")
    }
    if (x.var != "k" && rule == "k.of.m") {
        if (is.null(k) || !is.finite(k) || !is.vector(k, mode = "numeric") || 
            length(k) != 1 || k < 1 || k != trunc(k)) 
            stop("'k' must be a positive integer")
    }
    if (x.var != "k" && x.var != "m" && rule == "k.of.m" && k > 
        m) 
        stop("'k' must be less than or equal to 'm'")
    if (x.var == "k" && max.x > m) 
        stop(paste("When x.var=\"k\" 'max.x' must be", "less than or equal to 'm'"))
    if (x.var == "m" && rule == "k.of.m" && min.x < k) 
        stop(paste("When x.var=\"m\" 'min.x' must be", "greater than or equal to 'k'"))
    pi.string <- switch(pi.type, two.sided = "(Two-Sided PI)", 
        lower = "(One-Sided Lower PI)", upper = "(One-Sided Upper PI)")
    n.string <- paste("n =", n)
    n.median.string <- ifelse(n.median == 1, "", paste("n.median = ", 
        n, ", ", sep = ""))
    conf.string <- paste("Confidence Level = ", format(100 * 
        conf.level, digits = digits), "%", sep = "")
    k.string <- paste("k =", k)
    m.string <- paste("m =", m)
    r.string <- paste("r =", r)
    rule.string <- switch(rule, k.of.m = "Based on k-of-m Rule", 
        CA = "Based on California Rule", Modified.CA = "Based on Modified California Rule")
    if (x.var != "n" & y.var != "n") {
        upl.rank <- n + 1 - n.plus.one.minus.upl.rank
        rank.string <- switch(pi.type, lower = paste("Rank(LPL) =", 
            lpl.rank), upper = paste("Rank(UPL) =", upl.rank))
    }
    else {
        rank.string <- switch(pi.type, lower = paste("Rank(LPL) =", 
            lpl.rank), upper = paste("Rank(UPL) =", ifelse(n.plus.one.minus.upl.rank == 
            1, "n", paste("n -", n.plus.one.minus.upl.rank - 
            1))))
    }
    if (plot.it) 
        gen.gp.list <- checkGraphicsPars(...)$gen.gp.list
    combo <- paste(c(x.var, y.var), collapse = " & ")
    switch(combo, `n & conf.level` = {
        x <- seq(min.x, max.x, by = ceiling((max.x - min.x + 
            1)/n.points))
        if (is.null(xlab)) xlab <- "Sample Size (n)"
        y <- predIntNparSimultaneousConfLevel(n = x, n.median = n.median, 
            k = k, m = m, r = r, rule = rule, lpl.rank = lpl.rank, 
            n.plus.one.minus.upl.rank = n.plus.one.minus.upl.rank, 
            pi.type = pi.type, integrate.args.list = integrate.args.list)
        if (is.null(ylab)) ylab <- "Confidence Level"
        line1 <- paste("Confidence Level vs. Sample Size for", 
            "Nonparametric Simultaneous Prediction Interval")
        line2 <- switch(rule, k.of.m = paste(rule.string, " with ", 
            n.median.string, k.string, ", ", m.string, ", ", 
            r.string, " ", pi.string, sep = ""), CA = paste(rule.string, 
            " with ", n.median.string, m.string, ", ", r.string, 
            " ", pi.string, sep = ""), Modified.CA = paste(rule.string, 
            " with ", n.median.string, r.string, " ", pi.string, 
            sep = ""))
        line3 <- rank.string
    }, `conf.level & n` = {
        x <- seq(min.x, max.x, length = n.points)
        if (is.null(xlab)) xlab <- "Confidence Level"
        y <- predIntNparSimultaneousN(n.median = n.median, k = k, 
            m = m, r = r, rule = rule, lpl.rank = lpl.rank, n.plus.one.minus.upl.rank = n.plus.one.minus.upl.rank, 
            pi.type = pi.type, conf.level = x, n.max = n.max, 
            integrate.args.list = integrate.args.list, maxiter = maxiter)
        if (is.null(ylab)) ylab <- "Sample Size (n)"
        line1 <- paste("Sample Size vs. Confidence Level for", 
            "Nonparametric Simultaneous Prediction Interval")
        line2 <- switch(rule, k.of.m = paste(rule.string, " with ", 
            n.median.string, k.string, ", ", m.string, ", ", 
            r.string, " ", pi.string, sep = ""), CA = paste(rule.string, 
            " with ", n.median.string, m.string, ", ", r.string, 
            " ", pi.string, sep = ""), Modified.CA = paste(rule.string, 
            " with ", n.median.string, r.string, " ", pi.string, 
            sep = ""))
        line3 <- rank.string
    }, `k & conf.level` = {
        x <- seq(min.x, max.x, by = ceiling((max.x - min.x + 
            1)/n.points))
        if (is.null(xlab)) xlab <- "Min # Future Obs PI Should Contain (k)"
        y <- predIntNparSimultaneousConfLevel(n = n, n.median = n.median, 
            k = x, m = m, r = r, rule = rule, lpl.rank = lpl.rank, 
            n.plus.one.minus.upl.rank = n.plus.one.minus.upl.rank, 
            pi.type = pi.type, integrate.args.list = integrate.args.list)
        if (is.null(ylab)) ylab <- "Confidence Level"
        line1 <- paste("Confidence Level vs. Min # Future Obs for", 
            "Nonparametric Simultaneous Prediction Interval")
        line2 <- paste(rule.string, " with ", n.string, ", ", 
            n.median.string, , m.string, ", ", r.string, " ", 
            pi.string, sep = "")
        line3 <- rank.string
    }, `m & conf.level` = {
        x <- seq(min.x, max.x, by = ceiling((max.x - min.x + 
            1)/n.points))
        if (is.null(xlab)) xlab <- "# Future Obs (m)"
        y <- predIntNparSimultaneousConfLevel(n = n, n.median = n.median, 
            k = k, m = x, r = r, rule = rule, lpl.rank = lpl.rank, 
            n.plus.one.minus.upl.rank = n.plus.one.minus.upl.rank, 
            pi.type = pi.type, integrate.args.list = integrate.args.list)
        if (is.null(ylab)) ylab <- "Confidence Level"
        line1 <- paste("Confidence Level vs. # Future Obs for", 
            "Nonparametric Simultaneous Prediction Interval")
        line2 <- paste(rule.string, " with ", n.string, ", ", 
            n.median.string, , k.string, ", ", r.string, " ", 
            pi.string, sep = "")
        line3 <- rank.string
    }, `m & n` = {
        x <- seq(min.x, max.x, by = ceiling((max.x - min.x + 
            1)/n.points))
        if (is.null(xlab)) xlab <- "# Future Obs (m)"
        y <- predIntNparSimultaneousN(n.median = n.median, k = k, 
            m = x, r = r, rule = rule, lpl.rank = lpl.rank, n.plus.one.minus.upl.rank = n.plus.one.minus.upl.rank, 
            pi.type = pi.type, conf.level = conf.level, n.max = n.max, 
            integrate.args.list = integrate.args.list, maxiter = maxiter)
        if (is.null(ylab)) ylab <- "Sample Size (n)"
        line1 <- paste("Sample Size vs. # Future Obs for", "Nonparametric Simultaneous Prediction Interval")
        line2 <- paste(rule.string, " with ", n.median.string, 
            k.string, ", ", r.string, ", ", conf.string, " ", 
            pi.string, sep = "")
        line3 <- rank.string
    }, `r & conf.level` = {
        x <- seq(min.x, max.x, by = ceiling((max.x - min.x + 
            1)/n.points))
        if (is.null(xlab)) xlab <- "# Future Sampling Occasions (r)"
        y <- predIntNparSimultaneousConfLevel(n = n, n.median = n.median, 
            k = k, m = m, r = x, rule = rule, lpl.rank = lpl.rank, 
            n.plus.one.minus.upl.rank = n.plus.one.minus.upl.rank, 
            pi.type = pi.type, integrate.args.list = integrate.args.list)
        if (is.null(ylab)) ylab <- "Confidence Level"
        line1 <- paste("Confidence Level vs. # Future Sampling Occasions for", 
            "Nonparametric Simultaneous Prediction Interval")
        line2 <- paste(rule.string, " with ", n.string, ", ", 
            n.median.string, , k.string, ", ", m.string, " ", 
            pi.string, sep = "")
        line3 <- rank.string
    }, `r & n` = {
        x <- seq(min.x, max.x, by = ceiling((max.x - min.x + 
            1)/n.points))
        if (is.null(xlab)) xlab <- "# Future Sampling Occasions (r)"
        y <- predIntNparSimultaneousN(n.median = n.median, k = k, 
            m = m, r = x, rule = rule, lpl.rank = lpl.rank, n.plus.one.minus.upl.rank = n.plus.one.minus.upl.rank, 
            pi.type = pi.type, conf.level = conf.level, n.max = n.max, 
            integrate.args.list = integrate.args.list, maxiter = maxiter)
        if (is.null(ylab)) ylab <- "Sample Size (n)"
        line1 <- paste("Sample Size vs. # Future Sampling Occasions for", 
            "Nonparametric Simultaneous Prediction Interval")
        line2 <- paste(rule.string, " with ", n.median.string, 
            k.string, ", ", m.string, ", ", conf.string, " ", 
            pi.string, sep = "")
        line3 <- rank.string
    })
    if (plot.it) {
        if (!add) {
            plot(x, y, type = "n", main = "", sub = "", ..., 
                xlab = xlab, ylab = ylab)
            if (is.null(main)) {
                mtext(text = line1, side = 3, line = 3, cex = cex.main)
                mtext(text = line2, side = 3, line = 2, cex = cex.main)
                mtext(text = line3, side = 3, line = 1, cex = cex.main)
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
