#' Sample Size for a One- or Two-Sample t-Test
#' @description
#' Compute the sample size necessary to achieve a specified power for a one-
#'   or two-sample t-test, given the scaled difference and significance level.
#' @usage
#' tTestN(delta.over.sigma, alpha = 0.05, power = 0.95,
#'     sample.type = ifelse(!is.null(n2), "two.sample", "one.sample"),
#'     alternative = "two.sided", approx = FALSE, n2 = NULL, round.up = TRUE,
#'     n.max = 5000, tol = 1e-07, maxiter = 1000)
#' @rawRd
#' \arguments{
#'   \item{delta.over.sigma}{
#'   numeric vector specifying the ratio of the true difference \eqn{\delta}
#'   (\eqn{\delta = \mu - \mu_0} for the one-sample case and
#'   \eqn{\delta = \mu_1 - \mu_2} for the two-sample case)
#'   to the population standard deviation (\eqn{\sigma}).  This is also called the
#'   \dQuote{scaled difference}.
#' }
#'   \item{alpha}{
#'   numeric vector of numbers between 0 and 1 indicating the Type I error level
#'   associated with the hypothesis test.  The default value is \code{alpha=0.05}.
#' }
#'   \item{power}{
#'   numeric vector of numbers between 0 and 1 indicating the power
#'   associated with the hypothesis test.  The default value is \code{power=0.95}.
#' }
#'   \item{sample.type}{
#'   character string indicating whether to compute power based on a one-sample or
#'   two-sample hypothesis test.  When \code{sample.type="one.sample"}, the computed
#'   power is based on a hypothesis test for a single mean.  When \cr
#'   \code{sample.type="two.sample"}, the computed power is based on a hypothesis test
#'   for the difference between two means.  The default value is \cr
#'   \code{sample.type="one.sample"} unless the argument \code{n2} is supplied.
#' }
#'   \item{alternative}{
#'   character string indicating the kind of alternative hypothesis.  The possible values
#'   are:
#'
#'   \itemize{
#'     \item \code{"two.sided"} (the default).  \eqn{H_a: \mu \ne \mu_0} for the one-sample case and
#'     \eqn{H_a: \mu_1 \ne \mu_2} for the two-sample case.
#'
#'     \item \code{"greater"}.  \eqn{H_a: \mu > \mu_0} for the one-sample case and
#'     \eqn{H_a: \mu_1 > \mu_2} for the two-sample case.
#'
#'     \item \code{"less"}.  \eqn{H_a: \mu < \mu_0} for the one-sample case and
#'     \eqn{H_a: \mu_1 < \mu_2} for the two-sample case.
#'   }
#'
#' }
#'   \item{approx}{
#'   logical scalar indicating whether to compute the power based on an approximation to
#'   the non-central t-distribution.  The default value is \code{FALSE}.
#' }
#'   \item{n2}{
#'   numeric vector of sample sizes for group 2.  The default value is
#'   \code{NULL} in which case it is assumed that the sample sizes for groups
#'   1 and 2 are equal.
#'   This argument is ignored when \code{sample.type="one.sample"}.
#'   Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf})
#'   values are \bold{\emph{not}} allowed.
#' }
#'   \item{round.up}{
#'   logical scalar indicating whether to round up the values of the computed
#'   sample size(s) to the next smallest integer.  The default value is
#'   \code{TRUE}.
#' }
#'   \item{n.max}{
#'   positive integer greater than 1 indicating the maximum sample size when \cr
#'   \code{sample.type="one.sample"} or the maximum sample size for group 1
#'   when \code{sample.type="two.sample"}.  The default value is \code{n.max=5000}.
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
#'   Formulas for the power of the t-test for specified values of
#'   the sample size, scaled difference, and Type I error level are given in
#'   the help file for \code{\link{tTestPower}}.  The function \code{tTestN}
#'   uses the \code{\link{uniroot}} search algorithm to determine the
#'   required sample size(s) for specified values of the power,
#'   scaled difference, and Type I error level.
#' }
#' @rawRd
#' \value{
#'   When \code{sample.type="one.sample"}, \code{tTestN} returns a numeric vector of sample sizes.
#'
#'   When \code{sample.type="two.sample"} and \code{n2} is not supplied,
#'   equal sample sizes for each group is assumed and \code{tTestN} returns a numeric vector of
#'   sample sizes indicating the required sample size \emph{for each group}.
#'
#'   When \code{sample.type="two.sample"} and \code{n2} is supplied,
#'   \code{tTestN} returns a list with two components called \code{n1} and
#'   \code{n2}, specifying the sample sizes for each group.
#' }
#' @rawRd
#' \references{
#'   See \code{\link{tTestPower}}.
#' }
#' @rawRd
#' \author{
#'   Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \note{
#'   See \code{\link{tTestPower}}.
#' }
#' @rawRd
#' \seealso{
#'   \code{\link{tTestPower}}, \code{\link{tTestScaledMdd}}, \code{\link{tTestAlpha}},
#'   \code{\link{plotTTestDesign}}, \link[stats]{Normal},
#'   \code{\link{t.test}}, \link{Hypothesis Tests}.
#' }
#' @rawRd
#' \examples{
#'   # Look at how the required sample size for the one-sample t-test
#'   # increases with increasing required power:
#'
#'   seq(0.5, 0.9, by = 0.1)
#'   #[1] 0.5 0.6 0.7 0.8 0.9
#'
#'   tTestN(delta.over.sigma = 0.5, power = seq(0.5, 0.9, by = 0.1))
#'   #[1] 18 22 27 34 44
#'
#'   #----------
#'
#'   # Repeat the last example, but compute the sample size based on the
#'   # approximation to the power instead of the exact method:
#'
#'   tTestN(delta.over.sigma = 0.5, power = seq(0.5, 0.9, by = 0.1),
#'     approx = TRUE)
#'   #[1] 18 22 27 34 45
#'
#'   #==========
#'
#'   # Look at how the required sample size for the two-sample t-test
#'   # decreases with increasing scaled difference:
#'
#'   seq(0.5, 2,by = 0.5)
#'   #[1] 0.5 1.0 1.5 2.0
#'
#'   tTestN(delta.over.sigma = seq(0.5, 2, by = 0.5), sample.type = "two")
#'   #[1] 105  27  13   8
#'
#'   #----------
#'
#'   # Look at how the required sample size for the two-sample t-test decreases
#'   # with increasing values of Type I error:
#'
#'   tTestN(delta.over.sigma = 0.5, alpha = c(0.001, 0.01, 0.05, 0.1),
#'     sample.type="two")
#'   #[1] 198 145 105  88
#'
#'   #----------
#'
#'   # For the two-sample t-test, compare the total sample size required to
#'   # detect a scaled difference of 1 for equal sample sizes versus the case
#'   # when the sample size for the second group is constrained to be 20.
#'   # Assume a 5% significance level and 95% power.  Note that for the case
#'   # of equal sample sizes, a total of 54 samples (27+27) are required,
#'   # whereas when n2 is constrained to be 20, a total of 62 samples
#'   # (42 + 20) are required.
#'
#'   tTestN(1, sample.type="two")
#'   #[1] 27
#'
#'   tTestN(1, n2 = 20)
#'   #$n1
#'   #[1] 42
#'   #
#'   #$n2
#'   #[1] 20
#'
#'   #==========
#'
#'   # Modifying the example on pages 21-4 to 21-5 of USEPA (2009), determine the
#'   # required sample size to detect a mean aldicarb level greater than the MCL
#'   # of 7 ppb at the third compliance well with a power of 95%, assuming the
#'   # true mean is 10 or 14.  Use the estimated standard deviation from the
#'   # first four months of data to estimate the true population standard
#'   # deviation, use a Type I error level of alpha=0.01, and assume an
#'   # upper one-sided alternative (third compliance well mean larger than 7).
#'   # (The data are stored in EPA.09.Ex.21.1.aldicarb.df.)
#'   # Note that the required sample size changes from 11 to 5 as the true mean
#'   # increases from 10 to 14.
#'
#'
#'   EPA.09.Ex.21.1.aldicarb.df
#'   #   Month   Well Aldicarb.ppb
#'   #1      1 Well.1         19.9
#'   #2      2 Well.1         29.6
#'   #3      3 Well.1         18.7
#'   #4      4 Well.1         24.2
#'   #5      1 Well.2         23.7
#'   #6      2 Well.2         21.9
#'   #7      3 Well.2         26.9
#'   #8      4 Well.2         26.1
#'   #9      1 Well.3          5.6
#'   #10     2 Well.3          3.3
#'   #11     3 Well.3          2.3
#'   #12     4 Well.3          6.9
#'
#'   sigma <- with(EPA.09.Ex.21.1.aldicarb.df,
#'     sd(Aldicarb.ppb[Well == "Well.3"]))
#'
#'   sigma
#'   #[1] 2.101388
#'
#'   tTestN(delta.over.sigma = (c(10, 14) - 7)/sigma,
#'     alpha = 0.01, sample.type="one", alternative="greater")
#'   #[1] 11  5
#'
#'
#'   # Clean up
#'   #---------
#'   rm(sigma)
#' }
#' @rawRd
#' \keyword{ distribution }
#' @rawRd
#' \keyword{ design }
#' @rawRd
#' \keyword{ htest }

tTestN <-
function (delta.over.sigma, alpha = 0.05, power = 0.95, sample.type = ifelse(!is.null(n2), 
    "two.sample", "one.sample"), alternative = "two.sided", approx = FALSE, 
    n2 = NULL, round.up = TRUE, n.max = 5000, tol = 1e-07, maxiter = 1000) 
{
    sample.type <- match.arg(sample.type, c("one.sample", "two.sample"))
    alternative <- match.arg(alternative, c("two.sided", "less", 
        "greater"))
    if (!is.vector(delta.over.sigma, mode = "numeric") || !is.vector(alpha, 
        mode = "numeric") || !is.vector(power, mode = "numeric")) 
        stop("'delta.over.sigma', 'alpha', and 'power' must be numeric vectors.")
    if (!all(is.finite(delta.over.sigma)) || !all(is.finite(alpha)) || 
        !all(is.finite(power))) 
        stop(paste("Missing (NA), Infinite (Inf, -Inf), and", 
            "Undefined (Nan) values are not allowed in", "'delta.over.sigma', 'alpha', or 'power'"))
    if (any(abs(delta.over.sigma) <= 0)) 
        stop("All values of 'delta.over.sigma' must be non-zero")
    if (any(alpha <= 0) || any(alpha >= 1)) 
        stop("All values of 'alpha' must be greater than 0 and less than 1")
    if (any(power <= alpha) || any(power >= 1)) 
        stop(paste("All values of 'power' must be greater than or equal to", 
            "the corresponding elements of 'alpha' and less than 1"))
    if (alternative == "greater" && any(delta.over.sigma <= 0)) 
        stop("When alternative='greater', all values of 'delta.over.sigma' must be positive.")
    if (alternative == "less" && any(delta.over.sigma >= 0)) 
        stop("When alternative='less', all values of 'delta.over.sigma' must be negative.")
    if (!is.vector(n.max, mode = "numeric") || length(n.max) != 
        1 || !is.finite(n.max) || n.max != trunc(n.max) || n.max < 
        2) 
        stop("'n.max' must be a positive integer greater than 1")
    if (!is.vector(maxiter, mode = "numeric") || length(maxiter) != 
        1 || !is.finite(maxiter) || maxiter != trunc(maxiter) || 
        maxiter < 2) 
        stop("'maxiter' must be a positive integer greater than 1")
    if (n2.constrained <- sample.type == "two.sample" && !is.null(n2)) {
        if (!is.vector(n2, mode = "numeric")) 
            stop("'n2' must be a numeric vector")
        if (!all(is.finite(n2))) 
            stop(paste("Missing (NA), Infinite (Inf, -Inf), and", 
                "Undefined (Nan) values are not allowed in 'n2'"))
        if (any(n2 != trunc(n2)) || any(n2 < 2)) 
            stop("All values of 'n2' must be positive integers larger than 1")
        arg.mat <- cbind.no.warn(power = as.vector(power), delta.over.sigma = as.vector(delta.over.sigma), 
            alpha = as.vector(alpha), n2 = as.vector(n2))
        N <- nrow(arg.mat)
        n.vec <- numeric(N)
        for (i in c("power", "delta.over.sigma", "alpha", "n2")) assign(i, 
            arg.mat[, i])
    }
    else {
        arg.mat <- cbind.no.warn(power = as.vector(power), delta.over.sigma = as.vector(delta.over.sigma), 
            alpha = as.vector(alpha))
        N <- nrow(arg.mat)
        n.vec <- numeric(N)
        for (i in c("power", "delta.over.sigma", "alpha")) assign(i, 
            arg.mat[, i])
    }
    type.fac <- ifelse(sample.type == "two.sample", 2, 1)
    alt.fac <- ifelse(alternative == "two.sided", 2, 1)
    sod2 <- 1/delta.over.sigma^2
    fcn.for.root <- function(n, power, delta.over.sigma, alpha, 
        sample.type, alternative, approx) {
        power - tTestPower(n.or.n1 = n, delta.over.sigma = delta.over.sigma, 
            alpha = alpha, sample.type = sample.type, alternative = alternative, 
            approx = approx)
    }
    power.2 <- tTestPower(n.or.n1 = 2, delta.over.sigma = delta.over.sigma, 
        alpha = alpha, sample.type = sample.type, alternative = alternative, 
        approx = approx)
    power.n.max <- tTestPower(n.or.n1 = n.max, delta.over.sigma = delta.over.sigma, 
        alpha = alpha, sample.type = sample.type, alternative = alternative, 
        approx = approx)
    for (i in 1:N) {
        power.i <- power[i]
        power.2.i <- power.2[i]
        if (power.2.i >= power.i) 
            n.vec[i] <- 2
        else {
            power.n.max.i <- power.n.max[i]
            if (power.n.max.i < power.i) {
                n.vec[i] <- NA
                warning(paste("Error in search algorithm for element ", 
                  i, ".  Try increasing the argument 'n.max'", 
                  sep = ""))
            }
            else {
                delta.over.sigma.i <- delta.over.sigma[i]
                alpha.i <- alpha[i]
                n.vec[i] <- uniroot(fcn.for.root, lower = 2, 
                  upper = n.max, f.lower = power.i - power.2.i, 
                  f.upper = power.i - power.n.max.i, power = power.i, 
                  delta.over.sigma = delta.over.sigma.i, alpha = alpha.i, 
                  sample.type = sample.type, alternative = alternative, 
                  approx = approx, tol = tol, maxiter = maxiter)$root
            }
        }
    }
    if (n2.constrained) {
        n1 <- (n.vec * n2)/(2 * n2 - n.vec)
        if (any(index <- !is.finite(n1) | n1 < 0)) {
            n1[index] <- NA
            warning(paste("One or more constrained values of 'n2' is(are)", 
                "too small given the associated values of", "'power', 'delta.over.sigma', and 'alpha'"))
        }
        names(n1) <- names(n2) <- NULL
        if (round.up) 
            n1 <- ceiling(n1)
        ret.val <- list(n1 = n1, n2 = n2)
    }
    else {
        if (round.up) 
            n.vec <- ceiling(n.vec)
        ret.val <- n.vec
    }
    ret.val
}
