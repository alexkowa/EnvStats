#' Plots for a Sampling Design Based on a Prediction Interval for the Next \eqn{k} Observations from a Normal Distribution
#' @description
#' Create plots involving sample size, number of future observations, half-width,
#'   estimated standard deviation, and confidence level for a prediction interval for
#'   the next \eqn{k} observations from a normal distribution.
#' @usage
#' plotPredIntNormDesign(x.var = "n", y.var = "half.width", range.x.var = NULL,
#'     n = 25, k = 1, n.mean = 1, half.width = 4 * sigma.hat, sigma.hat = 1,
#'     method = "Bonferroni", conf.level = 0.95, round.up = FALSE, n.max = 5000,
#'     tol = 1e-07, maxiter = 1000, plot.it = TRUE, add = FALSE, n.points = 100,
#'     plot.col = "black", plot.lwd = 3 * par("cex"), plot.lty = 1,
#'     digits = .Options$digits, cex.main = par("cex"), ..., main = NULL,
#'     xlab = NULL, ylab = NULL, type = "l")
#' @rawRd
#' \arguments{
#'   \item{x.var}{
#'   character string indicating what variable to use for the x-axis.
#'   Possible values are \code{"n"} (sample size; the default),
#'   \code{"half.width"} (the half-width of the confidence interval),
#'   \code{"k"} (number of future observations or averages),
#'   \code{"sigma.hat"} (the estimated standard deviation), and
#'   \code{"conf.level"} (the confidence level).
#' }
#'   \item{y.var}{
#'   character string indicating what variable to use for the y-axis.
#'   Possible values are \code{"half.width"} (the half-width of the confidence interval;
#'   the default), and \code{"n"} (sample size).
#' }
#'   \item{range.x.var}{
#'   numeric vector of length 2 indicating the range of the x-variable to use for the plot.
#'   The default value depends on the value of \code{x.var}.
#'   When \code{x.var="n"} the default value is \code{c(2,50)}.
#'   When \code{x.var="half.width"} the default value is \code{c(2.5 * sigma.hat, 4 * sigma.hat)}.
#'   When \code{x.var="k"} the default value is \code{c(1, 20)}.
#'   When \code{x.var="sigma.hat"}, the default value is \code{c(0.1, 2)}.
#'   When \code{x.var="conf.level"}, the default value is \code{c(0.5, 0.99)}.
#' }
#'   \item{n}{
#'   positive integer greater than 1 indicating the sample size upon
#'   which the prediction interval is based.  The default value is \code{n=25}.
#'   Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf})
#'   values are not allowed.
#' }
#'   \item{k}{
#'   positive integer specifying the number of future observations
#'   or averages the prediction interval should contain with confidence level
#'   \code{conf.level}.  The default value is \code{k=1}.   This argument is
#'   ignored if \code{x.var="k"}.
#' }
#'   \item{n.mean}{
#'   positive integer specifying the sample size associated with the \eqn{k} future
#'   \emph{averages}.  The default value is \code{n.mean=1} (i.e., individual observations).
#'   Note that all future averages must be based on the same sample size.
#' }
#'   \item{half.width}{
#'   positive scalar indicating the half-widths of the prediction interval.
#'   The default value is \code{half.width=4*sigma.hat}.  This argument is ignored
#'   if either \code{x.var="half.width"} or \code{y.var="half.width"}.
#' }
#'   \item{sigma.hat}{
#'   numeric scalar specifying the value of the estimated standard deviation.
#'   The default value is \code{sigma.hat=1}.  This argument is ignored if
#'   \code{x.var="sigma.hat"}.
#' }
#'   \item{method}{
#'   character string specifying the method to use if the number of future observations
#'   (\code{k}) is greater than 1.  The possible values are \code{method="Bonferroni"}
#'   (approximate method based on Bonferonni inequality; the default), and \cr
#'   \code{method="exact"} (exact method due to Dunnett, 1955).
#'   This argument is ignored if \code{k=1}.
#' }
#'   \item{conf.level}{
#'   numeric scalar between 0 and 1 indicating the confidence level of the
#'   prediction interval.  The default value is \code{conf.level=0.95}.
#' }
#'   \item{round.up}{
#'   for the case when \code{y.var="n"}, logical scalar indicating whether to round
#'   up the values of the computed sample
#'   sizes to the next smallest integer.  The default value is \code{round.up=TRUE}.
#' }
#'   \item{n.max}{
#'   for the case when \code{y.var="n"}, the maximum possible sample size.  The default
#'   value is \code{n.max=5000}.
#' }
#'   \item{tol}{
#'   numeric scalar indicating the tolerance to use in the \code{\link{uniroot}}
#'   search algorithm.  The default value is \code{tol=1e-7}.
#' }
#'   \item{maxiter}{
#'   positive integer indicating the maximum number of iterations to use in the
#'   \code{\link{uniroot}} search algorithm.  The default value is
#'   \code{maxiter=1000}.
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
#'   See the help files for \code{\link{predIntNorm}}, \code{\link{predIntNormK}},
#'   \code{\link{predIntNormHalfWidth}}, and \code{\link{predIntNormN}} for
#'   information on how to compute a prediction interval for the next \eqn{k}
#'   observations or averages from a normal distribution, how the half-width is
#'   computed when other quantities are fixed, and how the
#'   sample size is computed when other quantities are fixed.
#' }
#' @rawRd
#' \value{
#'   \code{plotPredIntNormDesign} invisibly returns a list with components:
#'
#'   \item{x.var}{x-coordinates of points that have been or would have been plotted.}
#'   \item{y.var}{y-coordinates of points that have been or would have been plotted.}
#' }
#' @rawRd
#' \references{
#'   See the help file for \code{\link{predIntNorm}}.
#' }
#' @rawRd
#' \author{
#'     Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \note{
#'   See the help file for \code{\link{predIntNorm}}.
#'
#'   In the course of designing a sampling program, an environmental scientist may wish
#'   to determine the relationship between sample size, confidence level, and half-width
#'   if one of the objectives of the sampling program is to produce prediction intervals.
#'   The functions \code{\link{predIntNormHalfWidth}}, \code{\link{predIntNormN}}, and
#'   \code{plotPredIntNormDesign} can be used to investigate these relationships for the
#'   case of normally-distributed observations.
#' }
#' @rawRd
#' \seealso{
#'   \code{\link{predIntNorm}}, \code{\link{predIntNormK}},
#'   \code{\link{predIntNormHalfWidth}}, \code{\link{predIntNormN}},
#'   \code{\link{Normal}}.
#' }
#' @rawRd
#' \examples{
#'   # Look at the relationship between half-width and sample size for a
#'   # prediction interval for k=1 future observation, assuming an estimated
#'   # standard deviation of 1 and a confidence level of 95%:
#'
#'   dev.new()
#'   plotPredIntNormDesign()
#'
#'   #==========
#'
#'   # Plot sample size vs. the estimated standard deviation for various levels
#'   # of confidence, using a half-width of 4:
#'
#'   dev.new()
#'   plotPredIntNormDesign(x.var = "sigma.hat", y.var = "n", range.x.var = c(1, 2),
#'     ylim = c(0, 90), main = "")
#'
#'   plotPredIntNormDesign(x.var = "sigma.hat", y.var = "n", range.x.var = c(1, 2),
#'     conf.level = 0.9, add = TRUE, plot.col = "red")
#'
#'   plotPredIntNormDesign(x.var = "sigma.hat", y.var = "n", range.x.var = c(1, 2),
#'     conf.level = 0.8, add = TRUE, plot.col = "blue")
#'
#'   legend("topleft", c("95\%", "90\%", "80\%"), lty = 1, lwd = 3 * par("cex"),
#'     col = c("black", "red", "blue"), bty = "n")
#'
#'   title(main = paste("Sample Size vs. Sigma Hat for Prediction Interval for",
#'     "k=1 Future Obs, Half-Width=4, and Various Confidence Levels",
#'     sep = "\n"))
#'
#'   #==========
#'
#'   # The data frame EPA.92c.arsenic3.df contains arsenic concentrations (ppb)
#'   # collected quarterly for 3 years at a background well and quarterly for
#'   # 2 years at a compliance well.  Using the data from the background well,
#'   # plot the relationship between half-width and sample size for a two-sided
#'   # 90% prediction interval for k=4 future observations.
#'
#'   EPA.92c.arsenic3.df
#'   #   Arsenic Year  Well.type
#'   #1     12.6    1 Background
#'   #2     30.8    1 Background
#'   #3     52.0    1 Background
#'   #...
#'   #18     3.8    5 Compliance
#'   #19     2.6    5 Compliance
#'   #20    51.9    5 Compliance
#'
#'   mu.hat <- with(EPA.92c.arsenic3.df,
#'     mean(Arsenic[Well.type=="Background"]))
#'
#'   mu.hat
#'   #[1] 27.51667
#'
#'   sigma.hat <- with(EPA.92c.arsenic3.df,
#'     sd(Arsenic[Well.type=="Background"]))
#'
#'   sigma.hat
#'   #[1] 17.10119
#'
#'   dev.new()
#'   plotPredIntNormDesign(x.var = "n", y.var = "half.width", range.x.var = c(4, 50),
#'     k = 4, sigma.hat = sigma.hat, conf.level = 0.9)
#'
#'   #==========
#'
#'   # Clean up
#'   #---------
#'   rm(mu.hat, sigma.hat)
#'   graphics.off()
#' }
#' @rawRd
#' \keyword{ distribution }
#' @rawRd
#' \keyword{ design }
#' @rawRd
#' \keyword{ htest }

plotPredIntNormDesign <-
function (x.var = "n", y.var = "half.width", range.x.var = NULL, 
    n = 25, k = 1, n.mean = 1, half.width = 4 * sigma.hat, sigma.hat = 1, 
    method = "Bonferroni", conf.level = 0.95, round.up = FALSE, 
    n.max = 5000, tol = 1e-07, maxiter = 1000, plot.it = TRUE, 
    add = FALSE, n.points = 100, plot.col = "black", plot.lwd = 3 * 
        par("cex"), plot.lty = 1, digits = .Options$digits, cex.main = par("cex"), 
    ..., main = NULL, xlab = NULL, ylab = NULL, type = "l") 
{
    method <- match.arg(method, c("Bonferroni", "exact"))
    x.var <- match.arg(x.var, c("n", "half.width", "k", "sigma.hat", 
        "conf.level"))
    y.var <- match.arg(y.var, c("half.width", "n"))
    if (x.var == y.var) 
        stop("'x.var' and 'y.var' cannot denote the same quantity")
    if (missing(range.x.var)) {
        range.x.var = switch(x.var, n = c(2, 50), half.width = c(2.5 * 
            sigma.hat, 4 * sigma.hat), k = c(1, 20), sigma.hat = c(0.1, 
            2), conf.level = c(0.5, 0.99))
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
    if (x.var != "n" && y.var != "n") {
        if (is.null(n) || !is.finite(n) || !is.vector(n, mode = "numeric") || 
            length(n) != 1 || n < 2 || n != trunc(n)) 
            stop("'n' must be an integer greater than 1")
    }
    if (x.var != "k") {
        if (is.null(k) || !is.finite(k) || !is.vector(k, mode = "numeric") || 
            length(k) != 1 || k < 1 || k != trunc(k)) 
            stop("'k' must be a positive integer")
    }
    switch(x.var, n = {
        if (min.x < 2 || min.x != trunc(min.x)) stop(paste("When x.var=\"n\" the first element of", 
            "range.x.var must be an integer greater than 1"))
        if (max.x != trunc(max.x)) stop(paste("When x.var=\"n\" the second element of", 
            "range.x.var must be an integer greater than", "the first element of range.x.var"))
        if (min.x < k) stop(paste("When x.var=\"n\", 'min.x' must be", 
            "greater than or equal to 'k'"))
    }, half.width = {
        if (min.x < .Machine$double.eps) stop(paste("When x.var=\"half.width\" the first element of", 
            "range.x.var must be a positive number"))
        if (max.x < .Machine$double.eps) stop(paste("When x.var=\"half.width\" the second element of", 
            "range.x.var must be a positive number greater than", 
            "the first element of range.x.var"))
    }, k = {
        if (min.x < 1 || min.x != trunc(min.x)) stop(paste("When x.var=\"k\" the first element of", 
            "range.x.var must be a positive integer"))
        if (max.x != trunc(max.x)) stop(paste("When x.var=\"k\" the second element of", 
            "range.x.var must be an integer greater than", "the first element of range.x.var"))
        if (y.var != "n" && max.x > n) stop(paste("When x.var=\"k\", max.x must be", 
            "less than or equal to n"))
    }, sigma.hat = {
        if (min.x < .Machine$double.eps) stop(paste("When x.var=\"sigma.hat\" the first element of", 
            "range.x.var must be a positive number"))
        if (max.x < .Machine$double.eps) stop(paste("When x.var=\"sigma.hat\" the second element of", 
            "range.x.var must be a positive number greater than", 
            "the first element of range.x.var"))
    }, conf.level = {
        if (min.x < .Machine$double.eps || min.x > 1 - .Machine$double.eps) stop(paste("When x.var=\"conf.level\" the first element of range.x.var", 
            "must be a positive number between 0 and 1"))
        if (max.x > 1 - .Machine$double.eps) stop(paste("When x.var=\"conf.level\" the second element of", 
            "range.x.var must be an positive number between 0 and 1", 
            "and greater than the first element of range.x.var"))
    })
    if (is.null(n.mean) || !is.finite(n.mean) || !is.vector(n.mean, 
        mode = "numeric") || length(n.mean) != 1 || n.mean < 
        1 || n.mean != trunc(n.mean)) 
        stop("'n.mean' must be a positive integer")
    if (x.var != "conf.level") 
        if (is.null(conf.level) || !is.finite(conf.level) || 
            !is.vector(conf.level, mode = "numeric") || length(conf.level) != 
            1 || conf.level <= .Machine$double.eps || conf.level >= 
            1 - .Machine$double.eps) {
            stop("'conf.level' must be a scalar between 0 and 1")
        }
    if (x.var != "sigma.hat" && (is.null(sigma.hat) || !is.finite(sigma.hat) || 
        !is.vector(sigma.hat, mode = "numeric") || length(sigma.hat) != 
        1 || sigma.hat <= .Machine$double.eps)) 
        stop("'sigma.hat' must be a positive scalar")
    if (x.var != "half.width" && y.var != "half.width" && (is.null(half.width) || 
        !is.finite(half.width) || !is.vector(half.width, mode = "numeric") || 
        length(half.width) != 1 || half.width <= .Machine$double.eps)) 
        stop("'half.width' must be a positive scalar")
    if (!is.vector(n.max, mode = "numeric") || length(n.max) != 
        1 || !is.finite(n.max) || n.max != trunc(n.max) || n.max < 
        2) 
        stop("'n.max' must be a positive integer greater than 1")
    if (!is.vector(maxiter, mode = "numeric") || length(maxiter) != 
        1 || !is.finite(maxiter) || maxiter != trunc(maxiter) || 
        maxiter < 2) 
        stop("'maxiter' must be a positive integer greater than 1")
    n.string <- paste("n =", n)
    n.mean.string <- paste("n (Mean) =", n.mean)
    hw.string <- paste("Half-Width =", format(half.width, digits = digits))
    k.string <- paste("k =", k)
    sh.string <- paste("Sigma Hat =", format(sigma.hat, digits = digits))
    conf.string <- paste("Confidence Level =", format(conf.level, 
        digits = digits))
    method.string <- paste("Based on", ifelse(method == "Bonferroni", 
        "Bonferroni", "Exact"), "Method")
    if (n.mean != 1) 
        line3 <- paste(method.string, "and", n.mean.string)
    else line3 <- method.string
    if (plot.it) 
        gen.gp.list <- checkGraphicsPars(...)$gen.gp.list
    combo <- paste(c(x.var, y.var), collapse = " & ")
    switch(combo, `n & half.width` = {
        x <- seq(min.x, max.x, length = n.points)
        if (is.null(xlab)) xlab <- "Sample Size (n)"
        y <- predIntNormHalfWidth(n = x, k = k, n.mean = n.mean, 
            sigma.hat = sigma.hat, method = method, conf.level = conf.level)
        if (is.null(ylab)) ylab <- "Half-Width"
        line1 <- "Half-Width vs. Sample Size for Prediction Interval"
        line2 <- paste(" with ", k.string, ", ", sh.string, ", and ", 
            conf.string, sep = "")
    }, `half.width & n` = {
        x <- seq(min.x, max.x, length = n.points)
        if (is.null(xlab)) xlab <- "Half-Width"
        y <- predIntNormN(half.width = x, k = k, n.mean = n.mean, 
            sigma.hat = sigma.hat, method = method, conf.level = conf.level, 
            round.up = round.up, n.max = n.max, tol = tol, maxiter = maxiter)
        if (is.null(ylab)) ylab <- "Sample Size (n)"
        line1 <- "Sample Size vs. Half-Width for Prediction Interval"
        line2 <- paste(" with ", k.string, ", ", sh.string, ", and ", 
            conf.string, sep = "")
    }, `k & half.width` = {
        x <- seq(min.x, max.x, by = ceiling((max.x - min.x + 
            1)/n.points))
        if (is.null(xlab)) xlab <- "# Future Observations (k)"
        y <- predIntNormHalfWidth(n = n, k = x, n.mean = n.mean, 
            sigma.hat = sigma.hat, method = method, conf.level = conf.level)
        if (is.null(ylab)) ylab <- "Half-Width"
        line1 <- "Half-Width vs. # Future Observations for Prediction Interval"
        line2 <- paste(" with ", n.string, ", ", sh.string, ", and ", 
            conf.string, sep = "")
    }, `k & n` = {
        x <- seq(min.x, max.x, by = ceiling((max.x - min.x + 
            1)/n.points))
        if (is.null(xlab)) xlab <- "# Future Observations (k)"
        y <- predIntNormN(half.width = half.width, k = x, n.mean = n.mean, 
            sigma.hat = sigma.hat, method = method, conf.level = conf.level, 
            round.up = round.up, n.max = n.max, tol = tol, maxiter = maxiter)
        if (is.null(ylab)) ylab <- "Sample Size (n)"
        line1 <- "Sample Size vs. # Future Observations for Prediction Interval"
        line2 <- paste(" with ", hw.string, ", ", sh.string, 
            ", and ", conf.string, sep = "")
    }, `sigma.hat & half.width` = {
        x <- seq(min.x, max.x, length = n.points)
        if (is.null(xlab)) xlab <- "Sigma Hat"
        y <- predIntNormHalfWidth(n = n, k = k, n.mean = n.mean, 
            sigma.hat = x, method = method, conf.level = conf.level)
        if (is.null(ylab)) ylab <- "Half-Width"
        line1 <- "Half-Width vs. Sigma Hat for Prediction Interval"
        line2 <- paste(" with ", n.string, ", ", k.string, ", and ", 
            conf.string, sep = "")
    }, `sigma.hat & n` = {
        x <- seq(min.x, max.x, length = n.points)
        if (is.null(xlab)) xlab <- "Sigma Hat"
        y <- predIntNormN(half.width = half.width, k = k, n.mean = n.mean, 
            sigma.hat = x, method = method, conf.level = conf.level, 
            round.up = round.up, n.max = n.max, tol = tol, maxiter = maxiter)
        if (is.null(ylab)) ylab <- "Sample Size (n)"
        line1 <- "Sample Size vs. Sigma Hat for Prediction Interval"
        line2 <- paste(" with ", k.string, ", ", hw.string, ", and ", 
            conf.string, sep = "")
    }, `conf.level & half.width` = {
        x <- seq(min.x, max.x, length = n.points)
        if (is.null(xlab)) xlab <- "Confidence Level"
        y <- predIntNormHalfWidth(n = n, k = k, n.mean = n.mean, 
            sigma.hat = sigma.hat, method = method, conf.level = x)
        if (is.null(ylab)) ylab <- "Half-Width"
        line1 <- "Half-Width vs. Confidence Level for Prediction Interval"
        line2 <- paste(" with ", n.string, ", ", k.string, ", and ", 
            sh.string, sep = "")
    }, `conf.level & n` = {
        x <- seq(min.x, max.x, length = n.points)
        if (is.null(xlab)) xlab <- "Confidence Level"
        y <- predIntNormN(half.width = half.width, k = k, n.mean = n.mean, 
            sigma.hat = sigma.hat, method = method, conf.level = x, 
            round.up = round.up, n.max = n.max, tol = tol, maxiter = maxiter)
        if (is.null(ylab)) ylab <- "Sample Size (n)"
        line1 <- "Sample Size vs. Confidence Level for Prediction Interval"
        line2 <- paste(" with ", k.string, ", ", hw.string, ", and ", 
            sh.string, sep = "")
    })
    if (plot.it) {
        if (!add) {
            plot(x, y, type = "n", main = "", sub = "", ..., 
                xlab = xlab, ylab = ylab)
            if (is.null(main)) {
                mtext(text = line1, side = 3, line = 3, cex = 1.25 * 
                  cex.main)
                mtext(text = line2, side = 3, line = 1.75, cex = 1.25 * 
                  cex.main)
                mtext(text = line3, side = 3, line = 0.5, cex = cex.main)
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
