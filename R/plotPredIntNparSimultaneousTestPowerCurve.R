#' Power Curves for Sampling Design for Test Based on Nonparametric Simultaneous Prediction Interval
#' @description
#' Plot power vs. \eqn{\Delta/\sigma} (scaled minimal detectable difference) for a
#'   sampling design for a test based on a nonparametric simultaneous prediction
#'   interval.  The power is based on assuming the true distribution of the
#'   observations is \link[stats:Normal]{normal}.
#' @usage
#' plotPredIntNparSimultaneousTestPowerCurve(n = 8, n.median = 1, k = 1, m = 2,
#'     r = 1, rule = "k.of.m", lpl.rank = ifelse(pi.type == "upper", 0, 1),
#'     n.plus.one.minus.upl.rank = ifelse(pi.type == "lower", 0, 1), pi.type = "upper",
#'     r.shifted = r, integrate.args.list = NULL, method = "approx", NMC = 100,
#'     range.delta.over.sigma = c(0, 5), plot.it = TRUE, add = FALSE, n.points = 20,
#'     plot.col = "black", plot.lwd = 3 * par("cex"), plot.lty = 1,
#'     digits = .Options$digits, cex.main = par("cex"), ..., main = NULL,
#'     xlab = NULL, ylab = NULL, type = "l")
#' @rawRd
#' \arguments{
#'   \item{n}{
#'   positive integer specifying the sample sizes.
#' }
#'   \item{n.median}{
#'   positive odd integer specifying the sample size associated with the
#'   future medians.  The default value is \code{n.median=1} (i.e., individual
#'   observations).  Note that all future medians must be based on the same
#'   sample size.
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
#'   positive integer specifying the number of future sampling
#'   \dQuote{occasions}.  The default value is \code{r=1}.
#' }
#'   \item{rule}{
#'   character string specifying which rule to use.  The possible values are
#'   \code{"k.of.m"} (\eqn{k}-of-\eqn{m} rule; the default), \code{"CA"} (California rule),
#'   and \code{"Modified.CA"} (modified California rule).
#' }
#'   \item{lpl.rank}{
#'   non-negative integer indicating the rank of the order statistic to use for
#'   the lower bound of the prediction interval.  When \code{pi.type="lower"}, the
#'   default value is \code{lpl.rank=1} (implying the minimum value of \code{x} is used
#'   as the lower bound of the prediction interval).  When \code{pi.type="upper"},
#'   the argument \code{lpl.rank} is set equal to \code{0}.
#' }
#'   \item{n.plus.one.minus.upl.rank}{
#'   non-negative integer related to the rank of the order statistic to use for
#'   the upper
#'   bound of the prediction interval.  A value of \code{n.plus.one.minus.upl.rank=1}
#'   means use the first largest value, and in general a value of \cr
#'   \code{n.plus.one.minus.upl.rank=}\eqn{i} means use the \eqn{i}'th largest value.
#'   When \cr
#'   \code{pi.type="upper"}, the default value is \code{n.plus.one.minus.upl.rank=1}.
#'   When \code{pi.type="lower"}, the argument \code{n.plus.one.minus.upl.rank} is set
#'   equal to \code{0}.
#' }
#'   \item{pi.type}{
#'   character string indicating what kind of prediction interval to compute.
#'   The possible values are \code{"two.sided"} (the default), \code{"lower"}, and
#'   \code{"upper"}.
#' }
#'   \item{r.shifted}{
#'   integer between \code{1} and \code{r} specifying the number of future sampling
#'   occasions for which the scaled mean is shifted by \eqn{\Delta/\sigma}.
#'   The default value is \code{r.shifted=r}.
#' }
#'   \item{integrate.args.list}{
#'   list of arguments to supply to the \code{\link{integrate}} function.  The default
#'   value is \code{NULL}.
#' }
#'   \item{method}{
#'   character string indicating what method to use to compute the power.  The possible
#'   values are \code{"approx"} (the default) and \code{"simulate"} (use
#'   Monte Carlo simulation).
#' }
#'   \item{NMC}{
#'   positive integer indicating the number of Monte Carlo trials to run when \cr
#'   \code{method="simulate"}.  The default value is \code{NMC=100}.
#' }
#'   \item{range.delta.over.sigma}{
#'   numeric vector of length 2 indicating the range of the x-variable to use for the
#'   plot.  The default value is \code{range.delta.over.sigma=c(0,5)}.
#' }
#'   \item{plot.it}{
#'   a logical scalar indicating whether to create a plot or add to the existing plot
#'   (see explanation of the argument \code{add} below) on the current graphics device.
#'   If \code{plot.it=FALSE}, no plot is produced, but a list of (x,y) values is returned
#'   (see the section VALUE).  The default value is \code{plot.it=TRUE}.
#' }
#'   \item{add}{
#'   a logical scalar indicating whether to add the design plot to the existing plot (\code{add=TRUE}),
#'   or to create a plot from scratch (\code{add=FALSE}).  The default value is \code{add=FALSE}.
#'   This argument is ignored if \code{plot.it=FALSE}.
#' }
#'   \item{n.points}{
#'   a numeric scalar specifying how many (x,y) pairs to use to produce the plot.
#'   There are \code{n.points} x-values evenly spaced between \code{range.x.var[1]} and \cr
#'   \code{range.x.var[2]}.  The default value is \code{n.points=100}.
#' }
#'   \item{plot.col}{
#'   a numeric scalar or character string determining the color of the plotted line or points.  The default value
#'   is \code{plot.col="black"}.  See the entry for \code{col} in the help file for \code{\link{par}}
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
#'   See the help file for \code{\link{predIntNparSimultaneousTestPower}} for
#'   information on how to compute the power of a hypothesis test for the difference
#'   between two means of normal distributions based on a nonparametric simultaneous
#'   prediction interval.
#' }
#' @rawRd
#' \value{
#'   \code{plotPredIntNparSimultaneousTestPowerCurve} invisibly returns a list with
#'   components:
#'
#'   \item{x.var}{x-coordinates of points that have been or would have been plotted.}
#'   \item{y.var}{y-coordinates of points that have been or would have been plotted.}
#' }
#' @rawRd
#' \references{
#'   See the help file for \code{\link{predIntNparSimultaneous}}.
#'
#'   Gansecki, M. (2009).  \emph{Using the Optimal Rank Values Calculator}.
#'   US Environmental Protection Agency, Region 8, March 10, 2009.
#' }
#' @rawRd
#' \author{
#'     Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \note{
#'   See the help file for \code{\link{predIntNparSimultaneous}}.
#'
#'   In the course of designing a sampling program, an environmental scientist may wish
#'   to determine the relationship between sample size, significance level, power, and
#'   scaled difference if one of the objectives of the sampling program is to determine
#'   whether two distributions differ from each other.  The functions
#'   \code{\link{predIntNparSimultaneousTestPower}} and \cr
#'   \code{plotPredIntNparSimultaneousTestPowerCurve} can be
#'   used to investigate these relationships for the case of normally-distributed
#'   observations.
#' }
#' @rawRd
#' \seealso{
#'   \code{\link{predIntNparSimultaneousTestPower}},
#'   \code{\link{predIntNparSimultaneous}},
#'   \code{\link{predIntNparSimultaneousN}},
#'   \code{\link{predIntNparSimultaneousConfLevel}},
#'   \code{\link{plotPredIntNparSimultaneousDesign}},
#'   \code{\link{predIntNpar}}, \code{\link{tolIntNpar}}.
#' }
#' @rawRd
#' \examples{
#'   # Example 19-5 of USEPA (2009, p. 19-33) shows how to compute nonparametric upper
#'   # simultaneous prediction limits for various rules based on trace mercury data (ppb)
#'   # collected in the past year from a site with four background wells and 10 compliance
#'   # wells (data for two of the compliance wells  are shown in the guidance document).
#'   # The facility must monitor the 10 compliance wells for five constituents
#'   # (including mercury) annually.
#'
#'   # We will pool data from 4 background wells that were sampled on
#'   # a number of different occasions, giving us a sample size of
#'   # n = 20 to use to construct the prediction limit.
#'
#'   # There are 10 compliance wells and we will monitor 5 different
#'   # constituents at each well annually.  For this example, USEPA (2009)
#'   # recommends setting r to the product of the number of compliance wells and
#'   # the number of evaluations per year (i.e., r = 10 * 1 = 10).
#'
#'   # Here we will reproduce Figure 19-2 on page 19-35.  This figure plots the
#'   # power of the nonparametric simultaneous prediction interval for 6 different
#'   # plans:
#'   #          Rule Median.n k m Order.Statistic Achieved.alpha BG.Limit
#'   #1)      k.of.m        1 1 3             Max         0.0055     0.28
#'   #2)      k.of.m        1 1 4             Max         0.0009     0.28
#'   #3) Modified.CA        1 1 4             Max         0.0140     0.28
#'   #4)      k.of.m        3 1 2             Max         0.0060     0.28
#'   #5)      k.of.m        1 1 4             2nd         0.0046     0.25
#'   #6)      k.of.m        1 1 4             3rd         0.0135     0.24
#'
#'   # Here is the power curve for the 1-of-4 sampling strategy.
#'
#'   dev.new()
#'   plotPredIntNparSimultaneousTestPowerCurve(n = 20, k = 1, m = 4, r = 10,
#'     rule = "k.of.m", n.plus.one.minus.upl.rank = 3, pi.type = "upper",
#'     r.shifted = 1, method = "approx", range.delta.over.sigma = c(0, 5), main = "")
#'
#'   title(main = paste(
#'     "Power Curve for Nonparametric 1-of-4 Sampling Strategy Based on",
#'     "25 Background Samples, SWFPR=10\%, and 2 Future Sampling Periods",
#'     sep = "\n"), cex.main = 1.1)
#'
#'   #----------
#'
#'   # Here are the power curves for all 6 sampling strategies.
#'   # Because these take several seconds to create, here we have commented out
#'   # the R commands.  To run this example, just remove the pound signs (#) from
#'   # in front of the R commands.
#'
#'   #dev.new()
#'   #plotPredIntNparSimultaneousTestPowerCurve(n = 20, k = 1, m = 4, r = 10,
#'   #  rule = "k.of.m", n.plus.one.minus.upl.rank = 3, pi.type = "upper",
#'   #  r.shifted = 1, method = "approx", range.delta.over.sigma = c(0, 5), main = "")
#'
#'   #plotPredIntNparSimultaneousTestPowerCurve(n = 20, n.median = 3, k = 1, m = 2,
#'   #  r = 10, rule = "k.of.m", n.plus.one.minus.upl.rank = 1, pi.type = "upper",
#'   #  r.shifted = 1, method = "approx", range.delta.over.sigma = c(0, 5),
#'   #  add = TRUE, plot.col = 2, plot.lty = 2)
#'
#'   #plotPredIntNparSimultaneousTestPowerCurve(n = 20, r = 10, rule = "Modified.CA",
#'   #  n.plus.one.minus.upl.rank = 1, pi.type = "upper", r.shifted = 1,
#'   #  method = "approx", range.delta.over.sigma = c(0, 5), add = TRUE,
#'   #  plot.col = 3, plot.lty = 3)
#'
#'   #plotPredIntNparSimultaneousTestPowerCurve(n = 20, k = 1, m = 4, r = 10,
#'   #  rule = "k.of.m", n.plus.one.minus.upl.rank = 2, pi.type = "upper",
#'   #  r.shifted = 1, method = "approx", range.delta.over.sigma = c(0, 5),
#'   #  add = TRUE, plot.col = 4, plot.lty = 4)
#'
#'   #plotPredIntNparSimultaneousTestPowerCurve(n = 20, k = 1, m = 3, r = 10,
#'   #  rule = "k.of.m", n.plus.one.minus.upl.rank = 1, pi.type = "upper",
#'   #  r.shifted = 1, method = "approx", range.delta.over.sigma = c(0, 5),
#'   #  add = TRUE, plot.col = 5, plot.lty = 5)
#'
#'   #plotPredIntNparSimultaneousTestPowerCurve(n = 20, k = 1, m = 4, r = 10,
#'   #  rule = "k.of.m", n.plus.one.minus.upl.rank = 1, pi.type = "upper",
#'   #  r.shifted = 1, method = "approx", range.delta.over.sigma = c(0, 5),
#'   #  add = TRUE, plot.col = 6, plot.lty = 6)
#'
#'   #legend("topleft", legend = c("1-of-4, 3rd", "1-of-2, Max, Median", "Mod CA",
#'   #  "1-of-4, 2nd", "1-of-3, Max", "1-of-4, Max"), lwd = 3 * par("cex"),
#'   #  col = 1:6, lty = 1:6, bty = "n")
#'
#'   #title(main = "Figure 19-2. Comparison of Full Power Curves")
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

plotPredIntNparSimultaneousTestPowerCurve <-
function (n = 8, n.median = 1, k = 1, m = 2, r = 1, rule = "k.of.m", 
    lpl.rank = ifelse(pi.type == "upper", 0, 1), n.plus.one.minus.upl.rank = ifelse(pi.type == 
        "lower", 0, 1), pi.type = "upper", r.shifted = r, integrate.args.list = NULL, 
    method = "approx", NMC = 100, range.delta.over.sigma = c(0, 
        5), plot.it = TRUE, add = FALSE, n.points = 20, plot.col = "black", 
    plot.lwd = 3 * par("cex"), plot.lty = 1, digits = .Options$digits, 
    cex.main = par("cex"), ..., main = NULL, xlab = NULL, ylab = NULL, 
    type = "l") 
{
    rule <- match.arg(rule, c("k.of.m", "CA", "Modified.CA"))
    pi.type <- match.arg(pi.type, c("upper", "lower"))
    method <- match.arg(method, c("approx", "Monte.Carlo"))
    if (is.null(range.delta.over.sigma) || !all(is.finite(range.delta.over.sigma)) || 
        !is.vector(range.delta.over.sigma, mode = "numeric") || 
        length(range.delta.over.sigma) != 2) 
        stop(paste("'range.delta.over.sigma' must be a numeric vector of length 2", 
            "with no missing (NA), infinite(-Inf, Inf), or undefined(NaN) values"))
    min.x <- range.delta.over.sigma[1]
    max.x <- range.delta.over.sigma[2]
    if (min.x >= max.x) 
        stop("The second element of 'range.delta.over.sigma' must be larger than the first")
    if (!is.vector(n.points, mode = "numeric") || length(n.points) != 
        1 || n.points != trunc(n.points) || n.points < 2) 
        stop("'n.points' must be an integer larger than 1")
    if (is.null(n) || !is.finite(n) || !is.vector(n, mode = "numeric") || 
        length(n) != 1 || n < 2 || n != trunc(n)) 
        stop("'n' must be an integer greater than 1")
    if (is.null(n.median) || !is.finite(n.median) || !is.vector(n.median, 
        mode = "numeric") || length(n.median) != 1 || n.median < 
        1 || n.median != trunc(n.median) || !is.odd(n.median)) 
        stop("'n.median' must be a positive odd integer")
    if (rule == "k.of.m") {
        if (length(m) != 1 || !is.vector(m, mode = "numeric") || 
            !is.finite(m) || m < 1 || m != trunc(m)) 
            stop("'m' must be a positive integer")
        if (length(k) != 1 || !is.vector(k, mode = "numeric") || 
            !is.finite(k) || k < 1 || k > m || k != trunc(k)) 
            stop("'k' must be a positive integer between 1 and 'm'")
    }
    else if (rule == "CA") {
        if (length(m) != 1 || !is.vector(m, mode = "numeric") || 
            !is.finite(m) || m < 1 || m != trunc(m)) 
            stop("'m' must be a positive integer")
        if (m == 1) 
            rule <- "k.of.m"
    }
    else {
        m <- 4
    }
    if (length(r) != 1 || !is.vector(r, mode = "numeric") || 
        !is.finite(r) || r != trunc(r) || r < 1) 
        stop("'r' must be a positive integer")
    if (is.null(r.shifted) || !is.finite(r.shifted) || !is.vector(r.shifted, 
        mode = "numeric") || length(r.shifted) != 1 || r.shifted != 
        trunc(r.shifted) || r.shifted < 1 || r.shifted > r) 
        stop("'r.shifted' must be a positive integer less than or equal to 'r'")
    if (pi.type == "upper") 
        lpl.rank <- 0
    else n.plus.one.minus.upl.rank <- 0
    if (!is.vector(lpl.rank, mode = "numeric") || length(lpl.rank) != 
        1 || !is.finite(lpl.rank) || lpl.rank != trunc(lpl.rank) || 
        lpl.rank < 0 || lpl.rank >= n) 
        stop("'lpl.rank' must be a non-negative integers less than 'n'")
    if (pi.type == "lower" & lpl.rank < 1) 
        stop("When pi.type='lower', 'lpl.rank' must be a positive integer")
    if (!is.vector(n.plus.one.minus.upl.rank, mode = "numeric") || 
        length(n.plus.one.minus.upl.rank) != 1 || !is.finite(n.plus.one.minus.upl.rank) || 
        n.plus.one.minus.upl.rank != trunc(n.plus.one.minus.upl.rank) || 
        n.plus.one.minus.upl.rank < 0 || n.plus.one.minus.upl.rank >= 
        n) 
        stop("'n.plus.one.minus.upl.rank' must be a non-negative integer less than 'n'")
    if (pi.type == "upper" & n.plus.one.minus.upl.rank < 1) 
        stop("When pi.type='upper', 'n.plus.one.minus.upl.rank' must be a positive integer")
    pl.rank <- ifelse(pi.type == "upper", n + 1 - n.plus.one.minus.upl.rank, 
        lpl.rank)
    delta.string <- "(Future Mean - Background Mean) / SD"
    pi.string <- switch(pi.type, lower = "Lower One-Sided PI", 
        upper = "Upper One-Sided PI")
    n.string <- paste("n =", n)
    n.median.string <- ifelse(n.median == 1, "", paste("n.median = ", 
        n.median, ", ", sep = ""))
    pl.string <- paste("PL Rank =", pl.rank)
    if (rule == "k.of.m") 
        k.string <- paste("k =", k)
    m.string <- paste("m =", m)
    r.string <- paste("r = ", r)
    if (r > 1) 
        r.string <- paste(r.string, ", r shifted = ", r.shifted, 
            sep = "")
    rule.string <- switch(rule, k.of.m = "k-of-m", CA = "CA", 
        Modified.CA = "Modified CA")
    method.string <- switch(method, approx = "Approximating K-multiplier", 
        Monte.Carlo = paste(NMC, "Monte Carlo Simulations"))
    if (plot.it) 
        gen.gp.list <- checkGraphicsPars(...)$gen.gp.list
    x <- seq(min.x, max.x, length = n.points)
    y <- predIntNparSimultaneousTestPower(n = n, n.median = n.median, 
        k = k, m = m, r = r, rule = rule, lpl.rank = lpl.rank, 
        n.plus.one.minus.upl.rank = n.plus.one.minus.upl.rank, 
        delta.over.sigma = x, pi.type = pi.type, r.shifted = r.shifted, 
        method = method, NMC = NMC, integrate.args.list = integrate.args.list)
    if (is.null(xlab)) 
        xlab <- delta.string
    if (is.null(ylab)) 
        ylab <- "Power"
    if (plot.it) {
        if (!add) {
            plot(x, y, type = "n", main = "", sub = "", ..., 
                xlab = xlab, ylab = ylab)
            if (is.null(main)) {
                string <- switch(rule, k.of.m = paste(k.string, 
                  m.string, r.string, sep = ", "), CA = paste(m.string, 
                  r.string, sep = ", "), Modified.CA = r.string)
                line1 <- paste("Power vs. Delta/Sigma for", "Simultaneous Nonparametric Prediction Interval")
                line2 <- paste("Using ", rule.string, " Rule with ", 
                  n.string, ", ", n.median.string, pl.string, 
                  ", ", string, sep = "")
                line3 <- paste(pi.string, "Based on", method.string, 
                  "and Assuming a Normal Distribution")
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
    names(ret.list) <- c("delta.over.sigma", "power")
    invisible(ret.list)
}
