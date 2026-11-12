#' Plots for Sampling Design Based on One- or Two-Sample Proportion Test
#' @description
#' Create plots involving sample size, power, difference, and
#'   significance level for a one- or two-sample proportion test.
#' @usage
#' plotPropTestDesign(x.var = "n", y.var = "power",
#'     range.x.var = NULL, n.or.n1 = 25, n2 = n.or.n1, ratio = 1,
#'      p.or.p1 = switch(alternative, greater = 0.6, less = 0.4,
#'        two.sided = ifelse(two.sided.direction == "greater", 0.6, 0.4)),
#'     p0.or.p2 = 0.5, alpha = 0.05, power = 0.95,
#'     sample.type = ifelse(!missing(n2) || !missing(ratio), "two.sample", "one.sample"),
#'     alternative = "two.sided", two.sided.direction = "greater",
#'     approx = TRUE, correct = sample.type == "two.sample", round.up = FALSE,
#'     warn = TRUE, n.min = 2, n.max = 10000, tol.alpha = 0.1 * alpha,
#'     tol = 1e-07, maxiter = 1000, plot.it = TRUE, add = FALSE, n.points = 50,
#'     plot.col = "black", plot.lwd = 3 * par("cex"), plot.lty = 1,
#'     digits = .Options$digits, cex.main = par("cex"), ..., main = NULL,
#'     xlab = NULL, ylab = NULL, type = "l")
#' @rawRd
#' \arguments{
#'   \item{x.var}{
#'   character string indicating what variable to use for the x-axis.
#'   Possible values are \code{"n"} (sample size; the default),
#'   \code{"delta"} (minimal detectable difference), \code{"power"}
#'   (power of the test), and \code{"alpha"} (significance level of the test).
#' }
#'   \item{y.var}{
#'   character string indicating what variable to use for the y-axis.
#'   Possible values are \code{"power"} (power of the test; the default),
#'   \code{"delta"} (minimal detectable difference), and \code{"n"} (sample size).
#' }
#'   \item{range.x.var}{
#'   numeric vector of length 2 indicating the range of the x-variable to use
#'   for the plot.  The default value depends on the value of \code{x.var}.
#'   When \code{x.var="n"} the default value is \code{c(20,400)}.  When
#'   \code{x.var="delta"} and \code{alternative="greater"} or
#'   \code{alternative="two.sided"} and \code{two.sided.direction="greater"},
#'   the default value is \code{c(0.05, 0.2)}.  When \code{x.var="delta"} and
#'   \code{alternative="less"} or \code{alternative="two.sided"} and
#'   \code{two.sided.direction="less"}, the default value is \code{-c(0.2, 0.05)}.
#'   When \code{x.var="power"} the default value is \cr
#'   \code{c(alpha + .Machine$double.eps, 0.95)}.  When \code{x.var="alpha"}, the
#'   default value is \code{c(0.01, 0.2)}.
#' }
#'   \item{n.or.n1}{
#'   numeric scalar indicating the sample size.  The default value is
#'   \code{n.or.n1=25}.  When \code{sample.type="one.sample"}, \code{n.or.n1}
#'   denotes the number of observations in the single sample.  When
#'   \code{sample.type="two.sample"}, \code{n.or.n1} denotes the number of
#'   observations from group 1.  Missing (\code{NA}), undefined (\code{NaN}),
#'   and infinite (\code{Inf}, \code{-Inf}) values are not allowed.  This
#'   argument is ignored if either \code{x.var="n"} or \code{y.var="n"}.
#' }
#'   \item{n2}{
#'   numeric scalar indicating the sample size for group 2.  The default value
#'   is the value of \code{n.or.n1}.  Missing (\code{NA}), undefined (\code{NaN}),
#'   and infinite (\code{Inf}, \code{-Inf}) values are not allowed.  This
#'   argument is ignored when \code{sample.type="one.sample"}.
#' }
#'   \item{ratio}{
#'   numeric vector indicating the ratio of sample size in group 2 to sample
#'   size in group 1 \eqn{n_2/n_1}. The default value is \code{ratio=1}.
#'   All values of \code{ratio} must be greater than or equal to 1.  This
#'   argument is only used when \code{x.var="n"} or \code{y.var="n"} and
#'   \code{sample.type="two.sample"}.
#' }
#'   \item{p.or.p1}{
#'   numeric vector of proportions.  When \code{sample.type="one.sample"},
#'   \code{p.or.p1} denotes the \emph{true} value of \eqn{p}, the probability of
#'   \dQuote{success}.  When \cr
#'   \code{sample.type="two.sample"}, \code{p.or.p1}
#'   denotes the value of \eqn{p_1}, the probability of \dQuote{success} in
#'   group 1.  When \code{alternative="greater"} or \cr
#'   \code{alternative="two.sided"} and \code{two.sided.direction="greater"},
#'   the default value is \code{p.or.p1=0.6}.  When \code{alternative="less"} or \cr
#'   \code{alternative="two.sided"} and \code{two.sided.direction="less"},
#'   the default value is \code{p.or.p1=0.4}.
#'   Missing (\code{NA}), undefined (\code{NaN}),
#'   and infinite (\code{Inf}, \code{-Inf}) values are not allowed.
#'   This argument is ignored when \code{x.var="delta"} or \code{y.var="delta"}.
#' }
#'   \item{p0.or.p2}{
#'   numeric vector of proportions.  When \code{sample.type="one.sample"},
#'   \code{p0.or.p2} denotes the \emph{hypothesized} value of \eqn{p}, the
#'   probability of \dQuote{success}.  When \cr
#'   \code{sample.type="two.sample"},
#'   \code{p0.or.p2} denotes the value of \eqn{p_2}, the probability of
#'   \dQuote{success} in group 2.  The default value is \code{p0.or.p2=0.5}.
#'   Missing (\code{NA}), undefined (\code{NaN}),
#'   and infinite (\code{Inf}, \code{-Inf}) values are not allowed.
#' }
#'   \item{alpha}{
#'   numeric scalar between 0 and 1 indicating the Type I error level associated
#'   with the hypothesis test.  The default value is \code{alpha=0.05}.
#'   This argument is ignored when \code{x.var="alpha"}.
#' }
#'   \item{power}{
#'   numeric scalar between 0 and 1 indicating the power associated with the
#'   hypothesis test.  The default value is \code{power=0.95}.  This argument is
#'   ignored when \code{x.var="power"} or \code{y.var="power"}.
#' }
#'   \item{sample.type}{
#'   character string indicating whether the design is based on a one-sample or
#'   two-sample proportion test.  When \code{sample.type="one.sample"}, the computations
#'   for the plot are based on a one-sample proportion test.  When \cr
#'   \code{sample.type="two.sample"}, the computations for the plot are based on a
#'   two-sample proportion test.  The default value is \code{sample.type="one.sample"}.
#' }
#'   \item{alternative}{
#'   character string indicating the kind of alternative hypothesis.  The possible
#'   values are \code{"two.sided"} (the default), \code{"less"}, and \code{"greater"}.
#' }
#'   \item{two.sided.direction}{
#'   character string indicating the direction (positive or negative) for the minimal
#'   detectable difference when \code{alternative="two.sided"}.  When \cr
#'   \code{two.sided.direction="greater"} (the default), the minimal detectable
#'   difference is positive.  When \code{two.sided.direction="less"}, the minimal
#'   detectable difference is negative.  This argument is ignored unless \cr
#'   \code{alternative="two.sided"} and either \code{x.var="delta"} or \code{y.var="delta"}.
#' }
#'   \item{approx}{
#'   logical scalar indicating whether to compute the power, sample size, or minimal
#'   detectable difference based on the normal approximation to the binomial distribution.
#'   The default value is \code{approx=TRUE}.  Currently, the exact method
#'   (\code{approx=FALSE})
#'   is only available for the one-sample case (i.e., \cr
#'   \code{sample.type="one.sample"}).
#' }
#'   \item{correct}{
#'   logical scalar indicating whether to use the continuity correction when \cr
#'   \code{approx=TRUE}.  The default value is \code{correct=TRUE} when \cr
#'   \code{sample.type="two.sample"} and \code{correct=FALSE} when \cr
#'   \code{sample.type="one.sample"}.  This argument is ignored when
#'   \code{approx=FALSE}.
#' }
#'   \item{round.up}{
#'   logical scalar indicating whether to round up the values of the computed sample
#'   size(s) to the next smallest integer.  The default value is \code{round.up=FALSE}.
#'   This argument is ignored unless \code{y.var="n"}.
#' }
#'   \item{warn}{
#'   logical scalar indicating whether to issue a warning.  The default value is \cr
#'   \code{warn=TRUE}.  When \code{approx=TRUE} (test based on the normal approximation)
#'   and \code{warn=TRUE}, a warning is issued for cases when the normal approximation
#'   to the binomial distribution probably is not accurate.  When \code{approx=FALSE}
#'   (exact test) and \code{warn=TRUE}, a warning is issued when the user-supplied
#'   sample size is too small to yield a significance level less than or equal to the
#'   user-supplied value of \code{alpha}.
#' }
#'   \item{n.min}{
#'   integer relevant to the case when \code{y.var="n"} and \code{approx=FALSE}
#'   (i.e., when the power is based on the exact test).  This argument indicates the
#'   minimum allowed value for \eqn{n} to use in the search algorithm.  The default
#'   value is \code{n.min=2}.
#' }
#'   \item{n.max}{
#'   integer relevant to the case when \code{y.var="n"} and \code{approx=FALSE}
#'   (i.e., when the power is based on the exact test).  This argument indicates the
#'   maximum allowed value for \eqn{n} to use in the search algorithm.  The default
#'   value is \code{n.max=10000}.
#' }
#'   \item{tol.alpha}{
#'   numeric vector relevant to the case when \code{y.var="n"} and \code{approx=FALSE}
#'   (i.e., when the power is based on the exact test).  This argument indicates the
#'   tolerance on \code{alpha} to use in the search algorithm (i.e., how close the
#'   actual Type I error level is to the value prescribed by the argument \code{alpha}).
#'   The default value is \code{tol.alpha=0.1*alpha}.
#' }
#'   \item{tol}{
#'   numeric scalar relevant to the case when \code{y.var="n"} and \code{approx=FALSE}
#'   (i.e., when the power is based on the exact test), or when \code{y.var="delta"}.
#'   This argument is passed to the \code{uniroot} function and indicates the tolerance
#'   to use in the search algorithm.  The default value is \code{tol=1e-7}.
#' }
#'   \item{maxiter}{
#'   integer relevant to the case when \code{y.var="n"} and \code{approx=FALSE}
#'   (i.e., when the power is based on the exact test), or when \code{y.var="delta"}.
#'   This argument is passed to the
#'   \code{uniroot} function and indicates the maximum number of iterations to use
#'   in the search algorithm.  The default value is \code{maxiter=1000}.
#' }
#'   \item{plot.it}{
#'   a logical scalar indicating whether to create a new plot or add to the existing plot
#'   (see \code{add}) on the current graphics device.  If \code{plot.it=FALSE}, no plot
#'   is produced, but a list of (x,y) values is returned (see VALUE).  The default value
#'   is \code{plot.it=TRUE}.
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
#'   See the help files for \code{\link{propTestPower}}, \code{\link{propTestN}}, and
#'   \code{\link{propTestMdd}} for information on how to compute the power, sample size,
#'   or minimal detectable difference for a one- or two-sample proportion test.
#' }
#' @rawRd
#' \value{
#'   \code{plotPropTestDesign} invisibly returns a list with components
#'   \code{x.var} and \code{y.var}, giving coordinates of the points that have
#'   been or would have been plotted.
#' }
#' @rawRd
#' \references{
#'   See the help files for \code{\link{propTestPower}}, \code{\link{propTestN}}, and
#'   \code{\link{propTestMdd}}.
#' }
#' @rawRd
#' \author{
#'     Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \note{
#'   See the help files for \code{\link{propTestPower}}, \code{\link{propTestN}}, and
#'   \code{\link{propTestMdd}}.
#' }
#' @rawRd
#' \seealso{
#'   \code{\link{propTestPower}}, \code{\link{propTestN}},
#'   \code{\link{propTestMdd}}, \link{Binomial},
#'   \code{\link{binom.test}}, \code{\link{prop.test}}.
#' }
#' @rawRd
#' \examples{
#'   # Look at the relationship between power and sample size for a
#'   # one-sample proportion test, assuming the true proportion is 0.6, the
#'   # hypothesized proportion is 0.5, and a 5% significance level.
#'   # Compute the power based on the normal approximation to the binomial
#'   # distribution.
#'
#'   dev.new()
#'   plotPropTestDesign()
#'
#'   #----------
#'
#'   # For a two-sample proportion test, plot sample size vs. the minimal detectable
#'   # difference for various levels of power, using a 5% significance level and a
#'   # two-sided alternative:
#'
#'   dev.new()
#'   plotPropTestDesign(x.var = "delta", y.var = "n", sample.type = "two",
#'     ylim = c(0, 2800), main="")
#'
#'   plotPropTestDesign(x.var = "delta", y.var = "n", sample.type = "two",
#'     power = 0.9, add = TRUE, plot.col = "red")
#'
#'   plotPropTestDesign(x.var = "delta", y.var = "n", sample.type = "two",
#'     power = 0.8, add = TRUE, plot.col = "blue")
#'
#'   legend("topright", c("95\%", "90\%", "80\%"), lty = 1,
#'     lwd = 3 * par("cex"), col = c("black", "red", "blue"), bty = "n")
#'
#'   title(main = paste("Sample Size vs. Minimal Detectable Difference for Two-Sample",
#'     "Proportion Test with p2=0.5, Alpha=0.05 and Various Powers", sep = "\n"))
#'
#'   #==========
#'
#'   # Example 22-3 on page 22-20 of USEPA (2009) involves determining whether more than
#'   # 10% of chlorine gas containers are stored at pressures above a compliance limit.
#'   # We want to test the one-sided null hypothesis that 10% or fewer of the containers
#'   # are stored at pressures greater than the compliance limit versus the alternative
#'   # that more than 10% are stored at pressures greater than the compliance limit.
#'   # We want to have at least 90% power of detecting a true proportion of 30% or
#'   # greater, using a 5% Type I error level.
#'
#'   # Here we will modify this example and create a plot of power versus
#'   # sample size for various assumed minimal detactable differences,
#'   # using a 5% Type I error level.
#'
#'
#'   dev.new()
#'   plotPropTestDesign(x.var = "n", y.var = "power",
#'     sample.type = "one", alternative = "greater",
#'     p0.or.p2 = 0.1, p.or.p1 = 0.25,
#'     range.x.var = c(20, 50), ylim = c(0.6, 1), main = "")
#'
#'   plotPropTestDesign(x.var = "n", y.var = "power",
#'     sample.type = "one", alternative = "greater",
#'     p0.or.p2 = 0.1, p.or.p1 = 0.3,
#'     range.x.var = c(20, 50), add = TRUE, plot.col = "red")
#'
#'   plotPropTestDesign(x.var = "n", y.var = "power",
#'     sample.type = "one", alternative = "greater",
#'     p0.or.p2 = 0.1, p.or.p1 = 0.35,
#'     range.x.var = c(20, 50), add = TRUE, plot.col = "blue")
#'
#'   legend("bottomright", c("p=0.35", "p=0.3", "p=0.25"), lty = 1,
#'     lwd = 3 * par("cex"), col = c("blue", "red", "black"), bty = "n")
#'
#'   title(main = paste("Power vs. Sample Size for One-Sided One-Sample Proportion",
#'     "Test with p0=0.1, Alpha=0.05 and Various Detectable Differences",
#'     sep = "\n"))
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

plotPropTestDesign <-
function (x.var = "n", y.var = "power", range.x.var = NULL, n.or.n1 = 25, 
    n2 = n.or.n1, ratio = 1, p.or.p1 = switch(alternative, greater = 0.6, 
        less = 0.4, two.sided = ifelse(two.sided.direction == 
            "greater", 0.6, 0.4)), p0.or.p2 = 0.5, alpha = 0.05, 
    power = 0.95, sample.type = ifelse(!missing(n2) || !missing(ratio), 
        "two.sample", "one.sample"), alternative = "two.sided", 
    two.sided.direction = "greater", approx = TRUE, correct = sample.type == 
        "two.sample", round.up = FALSE, warn = TRUE, n.min = 2, 
    n.max = 10000, tol.alpha = 0.1 * alpha, tol = 1e-07, maxiter = 1000, 
    plot.it = TRUE, add = FALSE, n.points = 50, plot.col = "black", 
    plot.lwd = 3 * par("cex"), plot.lty = 1, digits = .Options$digits, 
    cex.main = par("cex"), ..., main = NULL, xlab = NULL, ylab = NULL, 
    type = "l") 
{
    alternative <- match.arg(alternative, c("two.sided", "less", 
        "greater"))
    two.sided.direction <- match.arg(two.sided.direction, c("greater", 
        "less"))
    sample.type <- match.arg(sample.type, c("one.sample", "two.sample"))
    if (sample.type == "two.sample" && !approx) 
        stop(paste("Exact method not available for", "the two-sample case.  Set approx=T."))
    x.var <- match.arg(x.var, c("n", "delta", "power", "alpha"))
    y.var <- match.arg(y.var, c("power", "delta", "n"))
    if (x.var == y.var) 
        stop("'x.var' and 'y.var' cannot denote the same quantity")
    if (missing(range.x.var)) {
        range.x.var <- switch(x.var, n = c(20, 400), delta = switch(alternative, 
            greater = c(0.05, 0.2), less = -c(0.2, 0.05), two.sided = if (two.sided.direction == 
                "greater") c(0.05, 0.2) else -c(0.2, 0.05)), 
            power = c(alpha + .Machine$double.eps, 0.95), alpha = c(0.01, 
                0.2))
    }
    else {
        if (is.null(range.x.var) || !all(is.finite(range.x.var)) || 
            !is.vector(range.x.var, mode = "numeric") || length(range.x.var) != 
            2 || range.x.var[1] >= range.x.var[2]) 
            stop(paste("'range.x.var' must be a numeric vector of length 2", 
                "with no missing (NA), infinite(-Inf, Inf), or undefined(NaN) values", 
                "and the first element must be less than the second"))
    }
    min.x <- range.x.var[1]
    max.x <- range.x.var[2]
    if (!is.vector(n.points, mode = "numeric") || length(n.points) != 
        1 || n.points != trunc(n.points) || n.points < 2) 
        stop("'n.points' must be an integer larger than 1")
    if (x.var != "alpha") {
        if (is.null(alpha) || !is.finite(alpha) || !is.vector(alpha, 
            mode = "numeric") || length(alpha) != 1 || alpha <= 
            0 || alpha >= 1) {
            stop("'alpha' must be a scalar between 0 and 1")
        }
    }
    switch(x.var, n = {
        if (!all(range.x.var == trunc(range.x.var)) || min.x < 
            2) stop(paste("When x.var='n', 'range.x.var' must be", 
            "a vector containing two integers, with the first", 
            "element greater than 1, and the second", "element larger than the first"))
    }, delta = {
        if (alternative == "greater" && (min.x <= 0 || min.x >= 
            1)) stop(paste("When alternative='greater',", "and x.var='delta', all values of", 
            "'delta' must be positive and less than 1,", "so the first element of 'range.x.var' must be", 
            "positive and less than 1."))
        if (alternative == "less" && (max.x >= 0 || max.x <= 
            -1)) stop(paste("When alternative='less',", "and x.var='delta', all values of", 
            "'delta' must be negative and greater than -1,", 
            "so the second element of 'range.x.var' must be", 
            "negative and greater than 1."))
    }, power = {
        if (min.x < alpha || max.x >= 1) stop(paste("When x.var='power', 'range.x.var' must be", 
            "a vector of two elements, with the first element", 
            "greater than or equal to 'alpha', the second element", 
            "less than 1, and the second", "element larger than the first."))
    }, alpha = {
        if (min.x <= 0 || max.x >= 1) stop(paste("When x.var='alpha', 'range.x.var' must be", 
            "a vector of two elements, with the first element", 
            "greater than 0, the second element less than 1, and the second", 
            "element larger than the first."))
    })
    n2.constrained <- !missing(n2) && !is.null(n2)
    ratio.supplied <- !missing(ratio) && !is.null(ratio)
    if (x.var != "n" && y.var != "n") {
        if (is.null(n.or.n1) || !is.finite(n.or.n1) || !is.vector(n.or.n1, 
            mode = "numeric") || length(n.or.n1) != 1 || n.or.n1 < 
            2 || n.or.n1 != trunc(n.or.n1)) 
            stop("'n.or.n1 must be an integer greater than 1")
        if (sample.type == "two.sample" && (is.null(n2) || !is.finite(n2) || 
            !is.vector(n2, mode = "numeric") || length(n2) != 
            1 || n2 < 2 || n2 != trunc(n2))) 
            stop("n2 must be an integer greater than 1")
    }
    else {
        if (sample.type == "two.sample") {
            if (x.var == "n") {
                if (n2.constrained && (is.null(n2) || !is.finite(n2) || 
                  !is.vector(n2, mode = "numeric") || length(n2) != 
                  1 || n2 < 2 || n2 != trunc(n2))) 
                  stop("n2 must be an integer greater than 1")
            }
            else {
                if (ratio.supplied) {
                  if (!is.vector(ratio, mode = "numeric") || 
                    length(ratio) != 1 || ratio < 1) 
                    stop("'ratio' must be a numeric scalar greater than or equal to 1")
                  ratio.gt.1 <- ratio > 1
                }
                else ratio.gt.1 <- FALSE
            }
        }
    }
    if (!is.vector(p0.or.p2, mode = "numeric") || length(p0.or.p2) != 
        1 || p0.or.p2 <= 0 || p0.or.p2 >= 1) 
        stop("'p0.or.p2' must be a numeric scalar greater than 0 and less than 1")
    if (x.var != "delta" && y.var != "delta") {
        if (!is.vector(p.or.p1, mode = "numeric") || length(p.or.p1) != 
            1 || p.or.p1 <= 0 || p.or.p1 >= 1) 
            stop("'p.or.p1' must be a numeric scalar greater than 0 and less than 1")
        if (p.or.p1 == p0.or.p2) 
            stop("'p.or.p1' cannot equal 'p0.or.p2'")
    }
    if (x.var != "power" && y.var != "power") {
        if (is.null(power) || !is.vector(power, mode = "numeric") || 
            length(power) != 1 || power <= 0 || power >= 1) 
            stop("'power' must be a numeric scalar greater than 0 and less than 1")
        if (x.var != "alpha" && power <= alpha) 
            stop("power must be greater than alpha")
    }
    type.string <- paste(ifelse(sample.type == "one.sample", 
        "One-Sample", "Two-Sample"), "Proportion Test with")
    delta.string <- ifelse(sample.type == "one.sample", "(p - p0)", 
        "(p1 - p2)")
    p0.or.p2.string <- paste(ifelse(sample.type == "one.sample", 
        "p0", "p2"), "=", format(p0.or.p2, digits = digits))
    p.or.p1.string <- paste(ifelse(sample.type == "one.sample", 
        "p", "p1"), "=", format(p.or.p1, digits = digits))
    alt.string <- switch(alternative, two.sided = "(Two-Sided Alternative)", 
        less = "(Lower One-Sided Alternative)", greater = "(Upper One-Sided Alternative)")
    if (sample.type == "two.sample") 
        n.string <- paste("n1 = ", n.or.n1, ", n2 = ", n2, sep = "")
    else n.string <- paste("n =", n.or.n1)
    alpha.string <- paste(ifelse(approx, "Alpha =", "Alpha <="), 
        format(alpha, digits = digits))
    power.string <- paste("Power =", format(power, digits = digits))
    approx.string <- paste("Based on", ifelse(approx, "Normal Approximation", 
        "Exact Test"))
    if (plot.it) 
        gen.gp.list <- checkGraphicsPars(...)$gen.gp.list
    combo <- paste(sort(c(x.var, y.var)), collapse = " & ")
    switch(combo, `alpha & delta` = {
        x <- seq(range.x.var[1], range.x.var[2], length = n.points)
        y <- propTestMdd(n.or.n1 = n.or.n1, n2 = n2, p0.or.p2 = p0.or.p2, 
            alpha = x, power = power, sample.type = sample.type, 
            alternative = alternative, two.sided.direction = two.sided.direction, 
            approx = approx, correct = correct, tol = tol, warn = warn, 
            return.exact.list = FALSE)
        if (is.null(ylab)) ylab <- delta.string
        if (is.null(xlab)) xlab <- "Alpha"
        line1 <- paste("Delta vs. Alpha for", type.string)
        line2 <- paste(n.string, ", ", p0.or.p2.string, ", and ", 
            power.string, " ", alt.string, sep = "")
    }, `alpha & n` = {
        if (alternative == "greater" && p.or.p1 <= p0.or.p2) stop(paste("When alternative='greater',", 
            "'p.or.p1' must be greater than 'p0.or.p2'"))
        if (alternative == "less" && p.or.p1 >= p0.or.p2) stop(paste("When alternative='less',", 
            "'p.or.p1' must be less than 'p0.or.p2'"))
        x <- seq(range.x.var[1], range.x.var[2], length = n.points)
        if (sample.type == "two.sample") {
            if (ratio.gt.1) {
                if (is.null(ylab)) ylab <- paste("Sample Size (n1), With n2 =", 
                  ratio, "* n1")
                y <- propTestN(p.or.p1 = p.or.p1, p0.or.p2 = p0.or.p2, 
                  alpha = x, power = power, sample.type = "two.sample", 
                  alternative = alternative, ratio = ratio, approx = approx, 
                  correct = correct, round.up = round.up, warn = warn, 
                  return.exact.list = FALSE)$n1
            } else {
                if (is.null(ylab)) ylab <- "Sample Size (n1 and n2)"
                y <- propTestN(p.or.p1 = p.or.p1, p0.or.p2 = p0.or.p2, 
                  alpha = x, power = power, sample.type = "two.sample", 
                  alternative = alternative, ratio = 1, approx = approx, 
                  correct = correct, round.up = round.up, warn = warn, 
                  return.exact.list = FALSE)
            }
        } else {
            if (is.null(ylab)) ylab <- "Sample Size (n)"
            y <- propTestN(p.or.p1 = p.or.p1, p0.or.p2 = p0.or.p2, 
                alpha = x, power = power, sample.type = "one.sample", 
                alternative = alternative, approx = approx, correct = correct, 
                round.up = round.up, warn = warn, return.exact.list = FALSE, 
                tol = tol, n.min = n.min, n.max = n.max, tol.alpha = tol.alpha, 
                maxiter = maxiter)
        }
        if (is.null(xlab)) xlab <- "Alpha"
        line1 <- paste("Sample Size vs. Alpha for", type.string)
        line2 <- paste(p.or.p1.string, ", ", p0.or.p2.string, 
            ", and ", power.string, " ", alt.string, sep = "")
    }, `alpha & power` = {
        x <- seq(range.x.var[1], range.x.var[2], length = n.points)
        y <- propTestPower(n.or.n1 = n.or.n1, p.or.p1 = p.or.p1, 
            n2 = n2, p0.or.p2 = p0.or.p2, alpha = x, sample.type = sample.type, 
            alternative = alternative, approx = approx, correct = correct, 
            warn = warn, return.exact.list = FALSE)
        if (is.null(xlab)) xlab <- "Alpha"
        if (is.null(ylab)) ylab <- "Power"
        line1 <- paste("Power vs. Alpha for", type.string)
        line2 <- paste(n.string, ", ", p.or.p1.string, ", and ", 
            p0.or.p2.string, " ", alt.string, sep = "")
    }, `delta & n` = {
        if (x.var == "delta") {
            if (any(range.x.var + p0.or.p2 <= 0) || any(range.x.var + 
                p0.or.p2 >= 1)) stop(paste("When x.var='delta' and y.var='n',", 
                "both elements of", "'range.x.var' + 'p0.or.p2' must fall in the", 
                "interval (0,1)"))
            x <- seq(range.x.var[1], range.x.var[2], length = n.points)
            if (sample.type == "two.sample") {
                if (ratio.gt.1) {
                  if (is.null(ylab)) ylab <- paste("Sample Size (n1), With n2 =", 
                    ratio, "* n1")
                  y <- propTestN(p.or.p1 = x + p0.or.p2, p0.or.p2 = p0.or.p2, 
                    alpha = alpha, power = power, sample.type = "two.sample", 
                    alternative = alternative, ratio = ratio, 
                    approx = approx, correct = correct, round.up = round.up, 
                    warn = warn, return.exact.list = FALSE)$n1
                } else {
                  if (is.null(ylab)) ylab <- "Sample Size (n1 and n2)"
                  y <- propTestN(p.or.p1 = x + p0.or.p2, p0.or.p2 = p0.or.p2, 
                    alpha = alpha, power = power, sample.type = "two.sample", 
                    alternative = alternative, approx = approx, 
                    correct = correct, round.up = round.up, warn = warn, 
                    return.exact.list = FALSE)
                }
            } else {
                if (is.null(ylab)) ylab <- "Sample Size (n)"
                y <- propTestN(p.or.p1 = x + p0.or.p2, p0.or.p2 = p0.or.p2, 
                  alpha = alpha, power = power, sample.type = "one.sample", 
                  alternative = alternative, approx = approx, 
                  correct = correct, round.up = round.up, warn = warn, 
                  return.exact.list = FALSE, tol = tol, n.min = n.min, 
                  n.max = n.max, tol.alpha = tol.alpha, maxiter = maxiter)
            }
            if (is.null(xlab)) xlab <- delta.string
            line1 <- paste("Sample Size vs. Delta for", type.string)
            line2 <- paste(p0.or.p2.string, ", ", power.string, 
                ", and ", alpha.string, " ", alt.string, sep = "")
        } else {
            if (approx) x <- seq(range.x.var[1], range.x.var[2], 
                length = n.points) else x <- seq(min.x, max.x, 
                by = ceiling((max.x - min.x + 1)/n.points))
            if (sample.type == "two.sample") {
                if (n2.constrained) {
                  if (is.null(xlab)) xlab <- paste("Sample Size (n1), With n2 =", 
                    n2)
                } else {
                  n2 <- x
                  if (is.null(xlab)) xlab <- "Sample Size (n1 and n2)"
                }
            } else if (is.null(xlab)) xlab <- "Sample Size (n)"
            y <- propTestMdd(n.or.n1 = x, n2 = n2, p0.or.p2 = p0.or.p2, 
                alpha = alpha, power = power, sample.type = sample.type, 
                alternative = alternative, two.sided.direction = two.sided.direction, 
                approx = approx, correct = correct, tol = tol, 
                warn = warn, return.exact.list = FALSE)
            if (is.null(ylab)) ylab <- delta.string
            line1 <- paste("Delta vs. Sample Size for", type.string)
            line2 <- paste(p0.or.p2.string, ", ", power.string, 
                ", and ", alpha.string, " ", alt.string, sep = "")
        }
    }, `delta & power` = {
        if (x.var == "delta") {
            if (any(range.x.var + p0.or.p2 <= 0) || any(range.x.var + 
                p0.or.p2 >= 1)) stop(paste("When x.var='delta' and y.var='power',", 
                "both elements of", "'range.x.var' + 'p0.or.p2' must fall in the", 
                "interval (0,1)"))
            x <- seq(range.x.var[1], range.x.var[2], length = n.points)
            y <- propTestPower(n.or.n1 = n.or.n1, p.or.p1 = x + 
                p0.or.p2, n2 = n2, p0.or.p2 = p0.or.p2, alpha = alpha, 
                sample.type = sample.type, alternative = alternative, 
                approx = approx, correct = correct, warn = warn, 
                return.exact.list = FALSE)
            if (is.null(xlab)) xlab <- delta.string
            if (is.null(ylab)) ylab <- "Power"
            line1 <- paste("Power vs. Delta for", type.string)
            line2 <- paste(n.string, ", ", p0.or.p2.string, ", and ", 
                alpha.string, " ", alt.string, sep = "")
        } else {
            x <- seq(range.x.var[1], range.x.var[2], length = n.points)
            y <- propTestMdd(n.or.n1 = n.or.n1, n2 = n2, p0.or.p2 = p0.or.p2, 
                alpha = alpha, power = x, sample.type = sample.type, 
                alternative = alternative, two.sided.direction = two.sided.direction, 
                approx = approx, correct = correct, tol = tol, 
                warn = warn, return.exact.list = FALSE)
            if (is.null(ylab)) ylab <- delta.string
            if (is.null(xlab)) xlab <- "Power"
            line1 <- paste("Delta vs. Power for", type.string)
            line2 <- paste(n.string, ", ", p0.or.p2.string, ", and ", 
                alpha.string, " ", alt.string, sep = "")
        }
    }, `n & power` = {
        if (x.var == "n") {
            if (approx) x <- seq(range.x.var[1], range.x.var[2], 
                length = n.points) else x <- seq(min.x, max.x, 
                by = ceiling((max.x - min.x + 1)/n.points))
            if (sample.type == "two.sample") {
                if (n2.constrained) {
                  if (is.null(xlab)) xlab <- paste("Sample Size (n1), With n2 =", 
                    n2)
                } else {
                  n2 <- x
                  if (is.null(xlab)) xlab <- "Sample Size (n1 and n2)"
                }
            } else if (is.null(xlab)) xlab <- "Sample Size (n)"
            y <- propTestPower(n.or.n1 = x, p.or.p1 = p.or.p1, 
                n2 = n2, p0.or.p2 = p0.or.p2, alpha = alpha, 
                sample.type = sample.type, alternative = alternative, 
                approx = approx, correct = correct, warn = warn, 
                return.exact.list = FALSE)
            if (is.null(ylab)) ylab <- "Power"
            line1 <- paste("Power vs. Sample Size for", type.string)
            line2 <- paste(p.or.p1.string, ", ", p0.or.p2.string, 
                ", and ", alpha.string, " ", alt.string, sep = "")
        } else {
            if (alternative == "greater" && p.or.p1 <= p0.or.p2) stop(paste("When alternative='greater',", 
                "'p.or.p1' must be greater than 'p0.or.p2'"))
            if (alternative == "less" && p.or.p1 >= p0.or.p2) stop(paste("When alternative='less',", 
                "'p.or.p1' must be less than 'p0.or.p2'"))
            x <- seq(range.x.var[1], range.x.var[2], length = n.points)
            if (sample.type == "two.sample") {
                if (ratio.gt.1) {
                  if (is.null(ylab)) ylab <- paste("Sample Size (n1), With n2 =", 
                    ratio, "* n1")
                  y <- propTestN(p.or.p1 = p.or.p1, p0.or.p2 = p0.or.p2, 
                    alpha = alpha, power = x, sample.type = "two.sample", 
                    alternative = alternative, ratio = ratio, 
                    approx = approx, correct = correct, round.up = round.up, 
                    warn = warn, return.exact.list = FALSE)$n1
                } else {
                  if (is.null(ylab)) ylab <- "Sample Size (n1 and n2)"
                  y <- propTestN(p.or.p1 = p.or.p1, p0.or.p2 = p0.or.p2, 
                    alpha = alpha, power = x, sample.type = "two.sample", 
                    alternative = alternative, approx = approx, 
                    correct = correct, round.up = round.up, warn = warn, 
                    return.exact.list = FALSE)
                }
            } else {
                if (is.null(ylab)) ylab <- "Sample Size (n)"
                y <- propTestN(p.or.p1 = p.or.p1, p0.or.p2 = p0.or.p2, 
                  alpha = alpha, power = x, sample.type = "one.sample", 
                  alternative = alternative, approx = approx, 
                  correct = correct, round.up = round.up, warn = warn, 
                  return.exact.list = FALSE, tol = tol, n.min = n.min, 
                  n.max = n.max, tol.alpha = tol.alpha, maxiter = maxiter)
            }
            if (is.null(xlab)) xlab <- "Power"
            line1 <- paste("Sample Size vs. Power for", type.string)
            line2 <- paste(p.or.p1.string, ", ", p0.or.p2.string, 
                ", and ", alpha.string, " ", alt.string, sep = "")
        }
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
                mtext(text = approx.string, side = 3, line = 0.5, 
                  cex = cex.main)
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
