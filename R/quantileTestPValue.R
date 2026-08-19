#' Compute p-Value for the Quantile Test
#' @description
#' Compute the p-value associated with a specified combination of
#'   \eqn{m}, \eqn{n}, \eqn{r}, and \eqn{k} for the
#'   \link[=quantileTest]{quantile test} (useful for determining \eqn{r} and
#'   \eqn{k} for a given significance level \eqn{\alpha}).
#' @usage
#' quantileTestPValue(m, n, r, k, exact.p = TRUE)
#' @rawRd
#' \arguments{
#'   \item{m}{
#'   numeric vector of integers indicating the number of observations from the
#'   \dQuote{treatment} group.
#'   Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf})
#'   values are allowed but will be removed.
#' }
#'   \item{n}{
#'   numeric vector of integers indicating the number of observations from the
#'   \dQuote{reference} group.
#'   Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf})
#'   values are allowed but will be removed.
#' }
#'   \item{r}{
#'   numeric vector of integers indicating the ranks of the observations to use as the
#'   lower cut off for the quantile test.  All values of \code{r} must be greater than
#'   or equal to 2 and less than or equal to the corresponding elements of
#'   \code{m+n} (the total number of observations from both groups).
#'   Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf})
#'   values are allowed but will be removed.
#' }
#'   \item{k}{
#'   numeric vector of integers indicating the number of observations from the
#'   \dQuote{treatment} group contained in the \eqn{r} largest observations.  This is
#'   the critical value used to decide whether to reject the null hypothesis.
#'   All values of \code{k} must be greater than or equal to 0 and less than or equal
#'   to the corresponding elements of \code{r}.
#'   Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf})
#'   values are allowed but will be removed.
#' }
#'   \item{exact.p}{
#'   logical scalar indicating whether to compute the p-value based on the exact
#'   distribution of the test statistic (\code{exact.p=TRUE}; the default) or based on
#'   the normal approximation (\code{exact.p=FALSE}).
#' }
#' }
#' @rawRd
#' \details{
#'   If the arguments \code{m}, \code{n}, \code{r}, and \code{k} are not all the same
#'   length, they are replicated to be the same length as the length of the longest
#'   argument.
#'
#'   For details on how the p-value is computed, see the help file for
#'   \code{\link{quantileTest}}.
#'
#'   The function \code{quantileTestPValue} is useful for determining what values to
#'   use for \code{r} and \code{k}, given the values of \code{m}, \code{n}, and a
#'   specified significance level \eqn{\alpha}.  The function
#'   \code{quantileTestPValue} can be used to reproduce Tables A.6-A.9 in
#'   USEPA (1994, pp.A.22-A.25).
#' }
#' @rawRd
#' \value{
#'   numeric vector of p-values.
#' }
#' @rawRd
#' \references{
#'   See the help file for \code{\link{quantileTest}}.
#' }
#' @rawRd
#' \author{
#'   Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \note{
#'   See the help file for \code{\link{quantileTest}}.
#' }
#' @rawRd
#' \seealso{
#'   \code{\link{quantileTest}}, \code{\link{wilcox.test}},
#'   \code{\link{htest.object}}, \link{Hypothesis Tests}.
#' }
#' @rawRd
#' \examples{
#'   # Reproduce the first column of Table A.9 in USEPA (1994, p.A.25):
#'   #-----------------------------------------------------------------
#'
#'   p.vals <- quantileTestPValue(m = 5, n = seq(15, 45, by = 5),
#'     r = c(9, 3, 4, 4, 5, 5, 6), k = c(4, 2, 2, 2, 2, 2, 2))
#'
#'   round(p.vals, 3)
#'   #[1] 0.098 0.091 0.119 0.089 0.109 0.087 0.103
#'
#'   #==========
#'
#'   # Clean up
#'   #---------
#'
#'   rm(p.vals)
#' }
#' @rawRd
#' \keyword{htest}
#' @rawRd
#' \keyword{models}

quantileTestPValue <-
function (m, n, r, k, exact.p = TRUE) 
{
    if (!is.vector(m, mode = "numeric") || !is.vector(n, mode = "numeric") || 
        !is.vector(r, mode = "numeric") || !is.vector(k, mode = "numeric")) 
        stop("'m', 'n', 'r',  and 'k' must be numeric vectors")
    if (any(m != trunc(m)) || any(n != trunc(n)) || any(r != 
        trunc(r)) || any(k != trunc(k)) || any(m < 1) || any(n < 
        1) || any(r < 1) || any(k < 1)) 
        stop("All values of 'm', 'n', 'r', and 'k' must be positive integers")
    arg.mat <- cbind.no.warn(m = as.vector(m), n = as.vector(n), 
        r = as.vector(r), k = as.vector(k))
    for (i in c("m", "n", "r", "k")) assign(i, arg.mat[, i])
    N <- m + n
    if (any(r < 2) || any(r > N)) 
        stop("All values of 'r' must be greater than 1 and less than or equal to 'm+n'")
    if (any(k < 0) || any(k > r)) 
        stop(paste("All values of 'k' must be greater than or equal to 0", 
            "and less than or equal to 'r'"))
    if (exact.p) 
        p.val <- 1 - phyper(q = k - 1, m = m, n = n, k = r)
    else p.val <- 1 - pnorm((k - (m * r)/N - 0.5)/sqrt((m * n * 
        r * (N - r))/(N^2 * (N - 1))))
    names(p.val) <- NULL
    p.val
}
