#' Scaled Minimal Detectable Slope for a t-Test for Linear Trend
#' @description
#' Compute the scaled minimal detectable slope associated with a t-test for liner
#'   trend, given the sample size or predictor variable values, power, and
#'   significance level.
#' @usage
#' linearTrendTestScaledMds(n, x = lapply(n, seq), alpha = 0.05, power = 0.95,
#'     alternative = "two.sided", two.sided.direction = "greater", approx = FALSE,
#'     tol = 1e-07, maxiter = 1000)
#' @rawRd
#' \arguments{
#'   \item{n}{
#'   numeric vector of sample sizes.  All values of \code{n} must be positive integers
#'   larger than 2.  This argument is ignored when \code{x} is supplied.
#'   Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf})
#'   values are not allowed.
#' }
#'   \item{x}{
#'   numeric vector of predictor variable values, or a list in which each component is
#'   a numeric vector of predictor variable values.  Usually, the predictor variable is
#'   time (e.g., days, months, quarters, etc.).  The default value is
#'   \code{x=lapply(n,seq)}, which yields a list in which the i'th component is the
#'   seqence of integers from 1 to the i'th value of the vector \code{n}.  If \code{x}
#'   is a numeric vector, it must contain at least three elements, two of which must be
#'   unique.  If \code{x} is a list of numeric vectors, each component of \code{x}
#'   must contain at least three elements, two of which must be unique.
#'   Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf})
#'   values are not allowed.
#' }
#'   \item{alpha}{
#'   numeric vector of numbers between 0 and 1 indicating the Type I error level
#'   associated with the hypothesis test.  The default value is \code{alpha=0.05}.
#' }
#'   \item{power}{
#'   numeric vector of numbers between 0 and 1 indicating the power
#'   associated with the hypothesis test.  The default value is \code{power=0.95}.
#' }
#'   \item{alternative}{
#'   character string indicating the kind of alternative hypothesis.  The possible values
#'   are \code{"two.sided"} (the default), \code{"greater"}, and \code{"less"}.
#' }
#'   \item{two.sided.direction}{
#'   character string indicating the direction (positive or negative) for the
#'   scaled minimal detectable slope when \code{alternative="two.sided"}.  When \cr
#'   \code{two.sided.direction="greater"} (the default), the scaled minimal
#'   detectable slope is positive.  When \code{two.sided.direction="less"},
#'   the scaled minimal detectable slope is negative.  This argument
#'   is ignored if \code{alternative="less"} or \code{alternative="greater"}.
#' }
#'   \item{approx}{
#'   logical scalar indicating whether to compute the power based on an approximation to
#'   the non-central t-distribution.  The default value is \code{approx=FALSE}.
#' }
#'   \item{tol}{
#'   numeric scalar indicating the toloerance to use in the
#'   \code{\link{uniroot}} search algorithm.
#'   The default value is \code{tol=1e-7}.
#' }
#'   \item{maxiter}{
#'   positive integer indicating the maximum number of iterations
#'   argument to pass to the \code{\link{uniroot}} function.  The default
#'   value is \code{maxiter=1000}.
#' }
#' }
#' @rawRd
#' \details{
#'   If the argument \code{x} is a vector, it is converted into a list with one
#'   component.  If the arguments \code{n}, \code{x}, \code{alpha}, and
#'   \code{power} are not all the same length, they are replicated to be the same
#'   length as the length of the longest argument.
#'
#'   Formulas for the power of the t-test of linear trend for specified values of
#'   the sample size, scaled slope, and Type I error level are given in
#'   the help file for \code{\link{linearTrendTestPower}}.  The function
#'   \code{linearTrendTestScaledMds} uses the \code{\link{uniroot}} search algorithm to
#'   determine the minimal detectable scaled slope for specified values of the power,
#'   sample size, and Type I error level.
#' }
#' @rawRd
#' \value{
#'   numeric vector of computed scaled minimal detectable slopes.  When
#'   \code{alternative="less"}, or \code{alternative="two.sided"} and
#'   \code{two.sided.direction="less"}, the computed slopes are negative.  Otherwise,
#'   the slopes are positive.
#' }
#' @rawRd
#' \references{
#'   See the help file for \code{\link{linearTrendTestPower}}.
#' }
#' @rawRd
#' \author{
#'   Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \note{
#'   See the help file for \code{\link{linearTrendTestPower}}.
#' }
#' @rawRd
#' \seealso{
#'   \code{\link{linearTrendTestPower}}, \code{\link{linearTrendTestN}},
#'   \code{\link{plotLinearTrendTestDesign}}, \code{\link{lm}},
#'   \code{\link{summary.lm}}, \code{\link{kendallTrendTest}},
#'   \link{Power and Sample Size}, \link{Normal}, \code{\link{t.test}}.
#' }
#' @rawRd
#' \examples{
#'   # Look at how the scaled minimal detectable slope for the t-test for linear
#'   # trend increases with increasing required power:
#'
#'   seq(0.5, 0.9, by = 0.1)
#'   #[1] 0.5 0.6 0.7 0.8 0.9
#'
#'   scaled.mds <- linearTrendTestScaledMds(n = 10, power = seq(0.5, 0.9, by = 0.1))
#'
#'   round(scaled.mds, 2)
#'   #[1] 0.25 0.28 0.31 0.35 0.41
#'
#'   #----------
#'
#'   # Repeat the last example, but compute the scaled minimal detectable slopes
#'   # based on the approximate power instead of the exact:
#'
#'   scaled.mds <- linearTrendTestScaledMds(n = 10, power = seq(0.5, 0.9, by = 0.1),
#'     approx = TRUE)
#'
#'   round(scaled.mds, 2)
#'   #[1] 0.25 0.28 0.31 0.35 0.41
#'
#'   #==========
#'
#'   # Look at how the scaled minimal detectable slope for the t-test for linear trend
#'   # decreases with increasing sample size:
#'
#'   seq(10, 50, by = 10)
#'   #[1] 10 20 30 40 50
#'
#'   scaled.mds <- linearTrendTestScaledMds(seq(10, 50, by = 10), alternative = "greater")
#'
#'   round(scaled.mds, 2)
#'   #[1] 0.40 0.13 0.07 0.05 0.03
#'
#'   #==========
#'
#'   # Look at how the scaled minimal detectable slope for the t-test for linear trend
#'   # decreases with increasing values of Type I error:
#'
#'   scaled.mds <- linearTrendTestScaledMds(10, alpha = c(0.001, 0.01, 0.05, 0.1),
#'     alternative="greater")
#'
#'   round(scaled.mds, 2)
#'   #[1] 0.76 0.53 0.40 0.34
#'
#'   #----------
#'
#'   # Repeat the last example, but compute the scaled minimal detectable slopes
#'   # based on the approximate power instead of the exact:
#'
#'   scaled.mds <- linearTrendTestScaledMds(10, alpha = c(0.001, 0.01, 0.05, 0.1),
#'     alternative="greater", approx = TRUE)
#'
#'   round(scaled.mds, 2)
#'   #[1] 0.70 0.52 0.41 0.36
#'
#'   #==========
#'
#'   # Clean up
#'   #---------
#'   rm(scaled.mds)
#' }
#' @rawRd
#' \keyword{ distribution }
#' @rawRd
#' \keyword{ design }
#' @rawRd
#' \keyword{ htest }

linearTrendTestScaledMds <-
function (n, x = lapply(n, seq), alpha = 0.05, power = 0.95, 
    alternative = "two.sided", two.sided.direction = "greater", 
    approx = FALSE, tol = 1e-07, maxiter = 1000) 
{
    if (missing(n) && missing(x)) 
        stop("You must supply either 'n' or 'x'")
    if (!missing(x)) {
        if (is.vector(x) && !is.list(x)) 
            x <- list(x)
        if (!is.list(x) || !all(sapply(x, function(i) {
            is.vector(i, mode = "numeric")
        }))) 
            stop(paste("'x' must be either a numeric vector or", 
                "a list in which each component of 'x'", "is a numeric vector"))
        if (any(sapply(x, function(i) {
            !all(is.finite(i))
        }))) 
            stop(paste("Missing (NA), Infinite (Inf, -Inf), and", 
                "Undefined (Nan) values are not allowed in", 
                "any components of 'x'"))
        n <- sapply(x, length)
        n.unique <- sapply(x, function(y) length(unique(y)))
        if (any(n < 3) || any(n.unique < 2)) 
            stop(paste("All components of 'x' must contain at least 3 elements", 
                "and at least 2 distinct values."))
    }
    else {
        if (!is.vector(n, mode = "numeric")) 
            stop("'n' must be a numeric vector")
        if (any(is.na(n))) 
            stop(paste("Missing (NA) and Undefined (Nan) values", 
                "are not allowed in 'n'"))
        if (any(n < 3)) 
            stop("All values of 'n' must be greater than or equal to 3.")
    }
    if (!is.vector(alpha, mode = "numeric") || !is.vector(power, 
        mode = "numeric")) 
        stop("'alpha', and 'power' must be numeric vectors.")
    if (!all(is.finite(alpha)) || !all(is.finite(power))) 
        stop(paste("Missing (NA), Infinite (Inf, -Inf), and", 
            "Undefined (Nan) values are not allowed in", "'alpha', or 'power'"))
    if (any(alpha <= 0) || any(alpha >= 1)) 
        stop("All values of 'alpha' must be greater than 0 and less than 1")
    if (any(power < alpha) || any(power >= 1)) 
        stop(paste("All values of 'power' must be greater than or equal to", 
            "the corresponding elements of 'alpha', and less than 1"))
    alternative <- match.arg(alternative, c("two.sided", "less", 
        "greater"))
    two.sided.direction <- match.arg(two.sided.direction, c("greater", 
        "less"))
    alt.fac <- ifelse(alternative == "two.sided", 2, 1)
    df <- n - 2
    oossx <- 1/sqrt(sapply(x, function(x) (length(x) - 1) * var(x)))
    slope.over.sigma.vec <- oossx * (qt(1 - alpha/alt.fac, df) + 
        qt(power, df))
    index <- power == alpha
    slope.over.sigma.vec[index] <- 0
    if (!approx) {
        alt <- ifelse(alternative == "less", "greater", alternative)
        arg.mat <- cbind.no.warn(n = as.vector(n), list.index = 1:length(x), 
            power = as.vector(power), alpha = as.vector(alpha))
        n <- arg.mat[, "n"]
        list.index <- arg.mat[, "list.index"]
        power <- arg.mat[, "power"]
        alpha <- arg.mat[, "alpha"]
        x <- x[list.index]
        N <- nrow(arg.mat)
        fcn.for.root <- function(slope.over.sigma, x, power, 
            alpha, alternative, approx) {
            power - linearTrendTestPower(x = x, slope.over.sigma = slope.over.sigma, 
                alpha = alpha, alternative = alternative, approx = approx)
        }
        for (i in (1:N)[!index]) {
            x.i <- x[i]
            power.i <- power[i]
            alpha.i <- alpha[i]
            slope.over.sigma.i <- slope.over.sigma.vec[i]
            upper <- 2 * slope.over.sigma.i
            power.upper <- linearTrendTestPower(x = x.i, slope.over.sigma = upper, 
                alpha = alpha.i, alternative = alt, approx = FALSE)
            upper.too.small <- power.upper <= power.i
            iter <- 1
            while (upper.too.small && iter <= maxiter) {
                upper <- 2 * upper
                power.upper <- linearTrendTestPower(x = x.i, 
                  slope.over.sigma = upper, alpha = alpha.i, 
                  alternative = alt, approx = FALSE)
                upper.too.small <- power.upper <= power.i
                iter <- iter + 1
            }
            if (iter > maxiter) 
                stop("Error in search algorithm.  Try increasing the value of the argument 'maxiter'")
            slope.over.sigma.vec[i] <- uniroot(fcn.for.root, 
                lower = 0, upper = upper, f.lower = power.i - 
                  alpha.i, f.upper = power.i - power.upper, x = x.i, 
                power = power.i, alpha = alpha.i, alternative = alt, 
                approx = FALSE, tol = tol, maxiter = maxiter)$root
        }
    }
    if (alternative == "less" || (alternative == "two.sided" && 
        two.sided.direction == "less")) 
        slope.over.sigma.vec <- -slope.over.sigma.vec
    slope.over.sigma.vec
}
