#' Plot Results of Permutation Test
#' @description
#' Plot the results of calling functions that return an object of class
#'   \code{"permutationTest"}.  Currently, the \pkg{EnvStats} functions that perform
#'   permutation tests and produce objects of class \cr
#'   \code{"permutationTest"} are:  \code{\link{oneSamplePermutationTest}},
#'   \code{\link{twoSamplePermutationTestLocation}}, and \cr
#'   \code{\link{twoSamplePermutationTestProportion}}.
#'
#'   The function \code{plot.permutationTest} is automatically called by
#'   \code{\link{plot}} when given an object of class \code{"permutationTest"}.
#' @usage
#' \method{plot}{permutationTest}(x, hist.col = "cyan", stat.col = "black",
#'   stat.lwd = 3 * par("cex"), stat.lty = 1, cex.main = par("cex"),
#'   digits = .Options$digits, main = NULL, xlab = NULL, ylab = NULL,
#'   xlim = NULL, ylim = NULL, ...)
#' @rawRd
#' \arguments{
#'   \item{x}{
#'   an object of class \code{"permutationTest"}.
#'   See \code{\link{permutationTest.object}} for details.
#' }
#'   \item{hist.col}{
#'   a character string or numeric scalar determining the color of the histogram
#'   used to display the permutation distribution.  The default
#'   value is \code{hist.col="cyan"}.  See the entry for \code{col} in the
#'   \R help file for \code{\link{par}} for more information.
#' }
#'   \item{stat.col}{
#'   a character string or numeric scalar determining the color of the line indicating
#'   the value of the observed test statistic.  The default value is
#'   \code{stat.col="black"}.  See the entry for \code{col} in the \R help file for
#'   \code{\link{par}} for more information.
#' }
#'   \item{stat.lwd}{
#'   numeric scalar determining the width of the line indicating the value of the
#'   observed test statistic.  The default value is \code{stat.lwd=3*par("cex")}.
#'   See the entry for \code{lwd} in the \R help file for \code{\link{par}} for
#'   more information.
#' }
#'   \item{stat.lty}{
#'   numeric scalar determining the line type used to display the value of the
#'   observed test statistic.  The default value is \code{stat.lty=1}.
#'   See the entry for \code{lty} in the \R help file for \code{\link{par}} for
#'   more information.
#' }
#'   \item{digits}{
#'   scalar indicating how many significant digits to print for the distribution
#'   parameters.  The default value is \code{.Options$digits} (i.e., the current
#'   setting of \code{options("digits")}).
#' }
#'   \item{cex.main, main, xlab, ylab, xlim, ylim, \dots}{
#'   graphics parameters.  See the help file for \code{\link{par}}.
#' }
#' }
#' @rawRd
#' \details{
#'   Produces a plot displaying the permutation distribution (\code{exact=TRUE}) or a
#'   sample of the permutation distribution (\code{exact=FALSE}), and a line indicating
#'   the observed value of the test statistic.  The title in the plot includes
#'   information on the data used, null hypothesis, and p-value.
#'
#'   The function \code{plot.permutationTest} is a method for the generic function
#'   \code{\link{plot}} for the class \code{"permutationTest"}
#'   (see \code{\link{permutationTest.object}}).  It can be invoked by calling
#'   \code{\link{plot}} and giving it an object of
#'   class \code{"permutationTest"} as the first argument, or by calling \cr
#'   \code{plot.permutationTest} directly, regardless of the class of the object given
#'   as the first argument to \code{plot.permutationTest}.
#' }
#' @rawRd
#' \value{
#'   \code{plot.permutationTest} invisibly returns the first argument, \code{x}.
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
#'   \code{\link{permutationTest.object}}, \code{\link{print.permutationTest}},
#'   \code{\link{oneSamplePermutationTest}},  \cr
#'   \code{\link{twoSamplePermutationTestLocation}},
#'   \code{\link{twoSamplePermutationTestProportion}},
#'   \link{Hypothesis Tests}, \code{\link{plot}}.
#' }
#' @rawRd
#' \examples{
#'   # Create an object of class "permutationTest", then print it and plot it.
#'   # (Note:  the call to set.seed() allows you to reproduce this example.)
#'   #------------------------------------------------------------------------
#'
#'   set.seed(23)
#'
#'   dat <- rlogis(10, location = 7, scale = 2)
#'
#'   permutationTest.obj <- oneSamplePermutationTest(dat, mu = 5,
#'     alternative = "greater", exact = TRUE)
#'
#'   mode(permutationTest.obj)
#'   #[1] "list"
#'
#'   class(permutationTest.obj)
#'   #[1] "permutationTest"
#'
#'   names(permutationTest.obj)
#'   # [1] "statistic"         "parameters"        "p.value"
#'   # [4] "estimate"          "null.value"        "alternative"
#'   # [7] "method"            "estimation.method" "sample.size"
#'   #[10] "data.name"         "bad.obs"           "stat.dist"
#'   #[13] "exact"
#'
#'   #==========
#'
#'   # Print the results of the test
#'   #------------------------------
#'   permutationTest.obj
#'
#'   #Results of Hypothesis Test
#'   #--------------------------
#'   #
#'   #Null Hypothesis:                 Mean (Median) = 5
#'   #
#'   #Alternative Hypothesis:          True Mean (Median) is greater than 5
#'   #
#'   #Test Name:                       One-Sample Permutation Test
#'   #                                 (Exact)
#'   #
#'   #Estimated Parameter(s):          Mean = 9.977294
#'   #
#'   #Data:                            dat
#'   #
#'   #Sample Size:                     10
#'   #
#'   #Test Statistic:                  Sum(x - 5) = 49.77294
#'   #
#'   #P-value:                         0.001953125
#'
#'   #==========
#'
#'   # Plot the results of the test
#'   #-----------------------------
#'   dev.new()
#'   plot(permutationTest.obj)
#'
#'   #==========
#'
#'   # Extract the test statistic
#'   #---------------------------
#'
#'   permutationTest.obj$statistic
#'   #Sum(x - 5)
#'   #  49.77294
#'
#'   #==========
#'
#'   # Clean up
#'   #---------
#'   rm(permutationTest.obj)
#'   graphics.off()
#' }
#' @rawRd
#' \keyword{plot}
#' @exportS3Method NULL

plot.permutationTest <-
function (x, hist.col = "cyan", stat.col = "black", stat.lwd = 3 * 
    par("cex"), stat.lty = 1, cex.main = par("cex"), digits = .Options$digits, 
    main = NULL, xlab = NULL, ylab = NULL, xlim = NULL, ylim = NULL, 
    ...) 
{
    exact <- x$exact
    subset.expression <- x$subset.expression
    is.null.subset.expression <- is.null(subset.expression)
    if (is.null(main)) {
        if (exact) {
            num.top.lines <- ifelse(is.null.subset.expression, 
                4, 5)
        }
        else {
            num.top.lines <- ifelse(is.null.subset.expression, 
                5, 6)
        }
        o.mar <- par(mar = c(5, 4, num.top.lines, 4) + 0.1)
        on.exit(par(o.mar))
    }
    if (is.null(xlab)) 
        xlab <- names(x$statistic)
    if (is.null(ylab)) 
        ylab <- "Relative Frequency"
    stat <- x$statistic
    stat.dist <- x$stat.dist
    probs.stat.dist <- x$probs.stat.dist
    if (is.null(probs.stat.dist)) {
        hist.list <- do.call("hist", c(list(x = stat.dist, plot = FALSE), 
            list(...)))
        if (is.null(xlim)) {
            xlim <- range(stat.dist, stat, hist.list$breaks)
        }
        do.call("hist", c(list(x = stat.dist, prob = TRUE, col = hist.col), 
            list(...), list(xlab = xlab, ylab = ylab, xlim = xlim, 
                main = "")))
    }
    else {
        nx <- length(stat.dist)
        min.diff <- min(diff(stat.dist))
        con.upper.bound <- (min.diff/2)
        con <- con.upper.bound * (0.9 + ((nx - 2)/nx) * 0.1)
        xleft <- stat.dist - con
        xright <- stat.dist + con
        ybottom <- rep(0, nx)
        if (is.null(xlim)) {
            xlim <- range(stat.dist, stat, xleft, xright)
        }
        if (is.null(ylim)) {
            ylim <- c(0, max(probs.stat.dist))
        }
        plot(xleft, probs.stat.dist, type = "n", ..., xlim = xlim, 
            ylim = ylim, xlab = xlab, ylab = ylab)
        rect(xleft = xleft, ybottom = ybottom, xright = xright, 
            ytop = probs.stat.dist, col = hist.col, border = stat.col, 
            ...)
    }
    abline(v = x$statistic, col = stat.col, lwd = stat.lwd, lty = stat.lty)
    if (is.null(main)) {
        nv <- x$null.value
        nnv <- names(nv)
        method.vec <- string.break.line(x$method)[[1]]
        data.name <- paste(x$data.name, collapse = " and ")
        parent.of.data <- x$parent.of.data
        if (!is.null(parent.of.data)) 
            data.name <- paste(data.name, "in", parent.of.data)
        main1 <- paste("Results of", method.vec[1], "for")
        main2 <- data.name
        if (!is.null.subset.expression) {
            main3 <- paste("(Subset with ", subset.expression, 
                ")", sep = "")
        }
        alternative <- x$alternative
        string <- switch(alternative, two.sided = "Two-Sided", 
            less = "One-Sided Lower", greater = "One-Sided Upper")
        string <- paste(string, "P-Value =", signif(x$p.value, 
            digits = digits))
        main.H0 <- paste("H0:", paste(format(nnv, justify = "left"), 
            format(nv, digits = digits), sep = " = "))
        main.H0 <- paste(main.H0, "; ", string)
        if (exact) {
            if (is.null.subset.expression) {
                mtext(main1, side = 3, line = 3, cex = cex.main)
                mtext(main2, side = 3, line = 2, cex = cex.main)
                mtext(main.H0, side = 3, line = 1, cex = cex.main)
            }
            else {
                mtext(main1, side = 3, line = 4, cex = cex.main)
                mtext(main2, side = 3, line = 3, cex = cex.main)
                mtext(main3, side = 3, line = 2, cex = cex.main)
                mtext(main.H0, side = 3, line = 1, cex = cex.main)
            }
        }
        else {
            method.vec <- method.vec[-1]
            nchar.vec <- nchar(method.vec)
            main4 <- paste(substring(method.vec, 34, nchar.vec), 
                collapse = " ")
            if (is.null.subset.expression) {
                mtext(main1, side = 3, line = 4, cex = cex.main)
                mtext(main2, side = 3, line = 3, cex = cex.main)
                mtext(main4, side = 3, line = 2, cex = cex.main)
                mtext(main.H0, side = 3, line = 1, cex = cex.main)
            }
            else {
                mtext(main1, side = 3, line = 5, cex = cex.main)
                mtext(main2, side = 3, line = 4, cex = cex.main)
                mtext(main3, side = 3, line = 3, cex = cex.main)
                mtext(main4, side = 3, line = 2, cex = cex.main)
                mtext(main.H0, side = 3, line = 1, cex = cex.main)
            }
        }
    }
    else title(main = main)
    invisible(x)
}
