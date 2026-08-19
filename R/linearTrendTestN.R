#' Sample Size for a t-Test for Linear Trend
#' @description
#' Compute the sample size necessary to achieve a specified power for a t-test for
#'   linear trend, given the scaled slope and significance level.
#' @usage
#' linearTrendTestN(slope.over.sigma, alpha = 0.05, power = 0.95,
#'     alternative = "two.sided", approx = FALSE, round.up = TRUE,
#'     n.max = 5000, tol = 1e-07, maxiter = 1000)
#' @rawRd
#' \arguments{
#'   \item{slope.over.sigma}{
#'   numeric vector specifying the ratio of the true slope to the standard deviation of
#'   the error terms (\eqn{\sigma}).  This is also called the "scaled slope".  The
#'   default value is \code{slope.over.sigma=0}.
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
#'   \item{approx}{
#'   logical scalar indicating whether to compute the power based on an approximation to
#'   the non-central t-distribution.  The default value is \code{approx=FALSE}.
#' }
#'   \item{round.up}{
#'   logical scalar indicating whether to round up the values of the computed
#'   sample size(s) to the next smallest integer.  The default value is
#'   \code{TRUE}.
#' }
#'   \item{n.max}{
#'   positive integer greater than 2 indicating the maximum sample size.
#'   The default value is \code{n.max=5000}.
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
#'   If the arguments \code{slope.over.sigma}, \code{alpha}, and \code{power} are not
#'   all the same length, they are replicated to be the same length as the length of
#'   the longest argument.
#'
#'   Formulas for the power of the t-test of linear trend for specified values of
#'   the sample size, scaled slope, and Type I error level are given in
#'   the help file for \code{\link{linearTrendTestPower}}.  The function
#'   \code{linearTrendTestN} uses the \code{\link{uniroot}} search algorithm to
#'   determine the required sample size(s) for specified values of the power,
#'   scaled slope, and Type I error level.
#' }
#' @rawRd
#' \value{
#'   a numeric vector of sample sizes.
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
#'   \code{\link{linearTrendTestPower}}, \code{\link{linearTrendTestScaledMds}},
#'   \code{\link{plotLinearTrendTestDesign}}, \code{\link{lm}}, \cr
#'   \code{\link{summary.lm}}, \code{\link{kendallTrendTest}},
#'   \link{Power and Sample Size}, \link{Normal}, \code{\link{t.test}}.
#' }
#' @rawRd
#' \examples{
#'   # Look at how the required sample size for the t-test for zero slope
#'   # increases with increasing required power:
#'
#'   seq(0.5, 0.9, by = 0.1)
#'   #[1] 0.5 0.6 0.7 0.8 0.9
#'
#'   linearTrendTestN(slope.over.sigma = 0.1, power = seq(0.5, 0.9, by = 0.1))
#'   #[1] 18 19 21 22 25
#'
#'   #----------
#'
#'   # Repeat the last example, but compute the sample size based on the approximate
#'   # power instead of the exact:
#'
#'   linearTrendTestN(slope.over.sigma = 0.1, power = seq(0.5, 0.9, by = 0.1),
#'     approx = TRUE)
#'   #[1] 18 19 21 22 25
#'
#'   #==========
#'
#'   # Look at how the required sample size for the t-test for zero slope decreases
#'   # with increasing scaled slope:
#'
#'   seq(0.05, 0.2, by = 0.05)
#'   #[1] 0.05 0.10 0.15 0.20
#'
#'   linearTrendTestN(slope.over.sigma = seq(0.05, 0.2, by = 0.05))
#'   #[1] 41 26 20 17
#'
#'   #==========
#'
#'   # Look at how the required sample size for the t-test for zero slope decreases
#'   # with increasing values of Type I error:
#'
#'   linearTrendTestN(slope.over.sigma = 0.1, alpha = c(0.001, 0.01, 0.05, 0.1))
#'   #[1] 33 29 26 25
#' }
#' @rawRd
#' \keyword{ distribution }
#' @rawRd
#' \keyword{ design }
#' @rawRd
#' \keyword{ htest }

linearTrendTestN <-
function (slope.over.sigma, alpha = 0.05, power = 0.95, alternative = "two.sided", 
    approx = FALSE, round.up = TRUE, n.max = 5000, tol = 1e-07, 
    maxiter = 1000) 
{
    alternative <- match.arg(alternative, c("two.sided", "less", 
        "greater"))
    if (!is.vector(slope.over.sigma, mode = "numeric") || !is.vector(alpha, 
        mode = "numeric") || !is.vector(power, mode = "numeric")) 
        stop("'slope.over.sigma', 'alpha', and 'power' must be numeric vectors.")
    if (!all(is.finite(slope.over.sigma)) || !all(is.finite(alpha)) || 
        !all(is.finite(power))) 
        stop(paste("Missing (NA), Infinite (Inf, -Inf), and", 
            "Undefined (Nan) values are not allowed in", "'slope.over.sigma', 'alpha', or 'power'"))
    if (any(abs(slope.over.sigma) < .Machine$double.eps)) 
        stop("All values of 'slope.over.sigma' must be non-zero")
    if (any(alpha <= 0) || any(alpha >= 1)) 
        stop("All values of 'alpha' must be greater than 0 and less than 1")
    if (any(power <= alpha) || any(power >= 1)) 
        stop(paste("All values of 'power' must be greater than or equal to", 
            "the corresponding elements of 'alpha' and less than 1"))
    if (alternative == "greater" && any(slope.over.sigma <= 0)) 
        stop("When alternative='greater', all values of 'slope.over.sigma' must be positive.")
    if (alternative == "less" && any(slope.over.sigma >= 0)) 
        stop("When alternative='less', all values of 'slope.over.sigma' must be negative.")
    if (!is.vector(n.max, mode = "numeric") || length(n.max) != 
        1 || !is.finite(n.max) || n.max != trunc(n.max) || n.max < 
        3) 
        stop("'n.max' must be a positive integer greater than 2")
    if (!is.vector(maxiter, mode = "numeric") || length(maxiter) != 
        1 || !is.finite(maxiter) || maxiter != trunc(maxiter) || 
        maxiter < 2) 
        stop("'maxiter' must be a positive integer greater than 1")
    arg.mat <- cbind.no.warn(power = as.vector(power), slope.over.sigma = as.vector(slope.over.sigma), 
        alpha = as.vector(alpha))
    N <- nrow(arg.mat)
    n.vec <- numeric(N)
    for (i in c("power", "slope.over.sigma", "alpha")) assign(i, 
        arg.mat[, i])
    alt.fac <- ifelse(alternative == "two.sided", 2, 1)
    fcn.for.root <- function(n, power, slope.over.sigma, alpha, 
        alternative, approx) {
        power - linearTrendTestPower(n = n, slope.over.sigma = slope.over.sigma, 
            alpha = alpha, alternative = alternative, approx = approx)
    }
    power.3 <- linearTrendTestPower(n = 3, slope.over.sigma = slope.over.sigma, 
        alpha = alpha, alternative = alternative, approx = approx)
    power.n.max <- linearTrendTestPower(n = n.max, slope.over.sigma = slope.over.sigma, 
        alpha = alpha, alternative = alternative, approx = approx)
    for (i in 1:N) {
        power.i <- power[i]
        power.3.i <- power.3[i]
        if (power.3.i >= power.i) 
            n.vec[i] <- 3
        else {
            power.n.max.i <- power.n.max[i]
            if (power.n.max.i < power.i) {
                n.vec[i] <- NA
                warning("Error in search algorithm.  Try increasing the value of the argument 'n.max'")
            }
            else {
                slope.over.sigma.i <- slope.over.sigma[i]
                alpha.i <- alpha[i]
                n.vec[i] <- uniroot(fcn.for.root, lower = 3, 
                  upper = n.max, f.lower = power.i - power.3.i, 
                  f.upper = power.i - power.n.max.i, power = power.i, 
                  slope.over.sigma = slope.over.sigma.i, alpha = alpha.i, 
                  alternative = alternative, approx = approx, 
                  tol = tol, maxiter = maxiter)$root
            }
        }
    }
    if (round.up) 
        n.vec <- ceiling(n.vec)
    n.vec
}
