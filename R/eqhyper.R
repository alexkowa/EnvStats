#' Estimate Quantiles of a Hypergeometric Distribution
#' @description
#' Estimate quantiles of a \link[stats:Hypergeometric]{hypergeometric distribution}.
#' @usage
#' eqhyper(x, m = NULL, total = NULL, k = NULL, p = 0.5, method = "mle", digits = 0)
#' @rawRd
#' \arguments{
#'   \item{x}{
#'   non-negative integer indicating the number of white balls out of a sample of
#'   size \code{k} drawn without replacement from the urn, or an object resulting
#'   from a call to an estimating function that assumes a hypergeometric distribution
#'   (e.g., \code{\link{ehyper}}).  Missing (\code{NA}),
#'   undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) values are not
#'   allowed.
#' }
#'   \item{m}{
#'   non-negative integer indicating the number of white balls in the urn.
#'   You must supply \code{m} or \code{total}, but not both.
#'   Missing values (\code{NA}s) are not allowed.
#' }
#'   \item{total}{
#'   positive integer indicating the total number of balls in the urn (i.e.,
#'   \code{m+n}).  You must supply \code{m} or \code{total}, but not both.
#'   Missing values (\code{NA}s) are not allowed.
#' }
#'   \item{k}{
#'   positive integer indicating the number of balls drawn without replacement from the
#'   urn.  Missing values (\code{NA}s) are not allowed.
#' }
#'   \item{p}{
#'   numeric vector of probabilities for which quantiles will be estimated.
#'   All values of \code{p} must be between 0 and 1.  The default value is \code{p=0.5}.
#' }
#'   \item{method}{
#'   character string specifying the method of estimating the parameters of the
#'   hypergeometric distribution.  Possible values are
#'   \code{"mle"} (maximum likelihood; the default) and \code{"mvue"}
#'   (minimum variance unbiased).  The mvue method is only available when you
#'   are estimating \eqn{m} (i.e., when you supply the argument \code{total}).
#'   See the DETAILS section of the help file for \code{\link{ehyper}} for more
#'   information on these estimation methods.
#' }
#'   \item{digits}{
#'   an integer indicating the number of decimal places to round to when printing out
#'   the value of \code{100*p}. The default value is \code{digits=0}.
#' }
#' }
#' @rawRd
#' \details{
#'   The function \code{eqhyper} returns estimated quantiles as well as
#'   estimates of the hypergeometric distribution parameters.
#'
#'   Quantiles are estimated by 1) estimating the distribution parameters by
#'   calling \code{\link{ehyper}}, and then 2) calling the function
#'   \code{\link[stats:Hypergeometric]{qhyper}} and using the estimated values for
#'   the distribution parameters.
#' }
#' @rawRd
#' \value{
#'   If \code{x} is a numeric vector, \code{eqhyper} returns a
#'   list of class \code{"estimate"} containing the estimated quantile(s) and other
#'   information. See \code{\link{estimate.object}} for details.
#'
#'   If \code{x} is the result of calling an estimation function, \code{eqhyper}
#'   returns a list whose class is the same as \code{x}.  The list
#'   contains the same components as \code{x}, as well as components called
#'   \code{quantiles} and \code{quantile.method}.
#' }
#' @rawRd
#' \references{
#'   Forbes, C., M. Evans, N. Hastings, and B. Peacock. (2011).  Statistical Distributions.
#'   Fourth Edition. John Wiley and Sons, Hoboken, NJ.
#'
#'   Johnson, N. L., S. Kotz, and A. Kemp. (1992).
#'   \emph{Univariate Discrete Distributions}.  Second Edition. John Wiley and Sons,
#'   New York, Chapter 6.
#' }
#' @rawRd
#' \author{
#'     Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \note{
#'   The \link[stats:Hypergeometric]{hypergeometric distribution} can be described by
#'   an urn model with \eqn{M} white balls and \eqn{N} black balls.  If \eqn{K} balls
#'   are drawn \emph{with} replacement, then the number of white balls in the sample
#'   of size \eqn{K} follows a \link[stats:Binomial]{binomial distribution} with
#'   parameters \code{size=}\eqn{K} and \code{prob=}\eqn{M/(M+N)}.  If \eqn{K} balls are
#'   drawn \emph{without} replacement, then the number of white balls in the sample of
#'   size \eqn{K} follows a \link[stats:Hypergeometric]{hypergeometric distribution}
#'   with parameters \code{m=}\eqn{M}, \code{n=}\eqn{N}, and \code{k=}\eqn{K}.
#'
#'   The name \dQuote{hypergeometric} comes from the fact that the probabilities
#'   associated with this distribution can be written as successive terms in the
#'   expansion of a function of a Gaussian hypergeometric series.
#'
#'   The hypergeometric distribution is applied in a variety of fields, including
#'   quality control and estimation of animal population size.  It is also the
#'   distribution used to compute probabilities for
#'   \link[stats:fisher.test]{Fishers's exact test} for a 2x2 contingency table.
#' }
#' @rawRd
#' \seealso{
#'   \code{\link{ehyper}}, \link[stats]{Hypergeometric}, \code{\link{estimate.object}}.
#' }
#' @rawRd
#' \examples{
#'   # Generate an observation from a hypergeometric distribution with
#'   # parameters m=10, n=30, and k=5, then estimate the parameter m, and
#'   # the 80'th percentile.
#'   # Note: the call to set.seed simply allows you to reproduce this example.
#'   # Also, the only parameter actually estimated is m; once m is estimated,
#'   # n is computed by subtracting the estimated value of m (8 in this example)
#'   # from the given of value of m+n (40 in this example).  The parameters
#'   # n and k are shown in the output in order to provide information on
#'   # all of the parameters associated with the hypergeometric distribution.
#'
#'   set.seed(250)
#'   dat <- rhyper(nn = 1, m = 10, n = 30, k = 5)
#'   dat
#'   #[1] 1
#'
#'   eqhyper(dat, total = 40, k = 5, p = 0.8)
#'
#'   #Results of Distribution Parameter Estimation
#'   #--------------------------------------------
#'   #
#'   #Assumed Distribution:            Hypergeometric
#'   #
#'   #Estimated Parameter(s):          m =  8
#'   #                                 n = 32
#'   #                                 k =  5
#'   #
#'   #Estimation Method:               mle for 'm'
#'   #
#'   #Estimated Quantile(s):           80'th %ile = 2
#'   #
#'   #Quantile Estimation Method:      Quantile(s) Based on
#'   #                                 mle for 'm' Estimators
#'   #
#'   #Data:                            dat
#'   #
#'   #Sample Size:                     1
#'
#'   #----------
#'
#'   # Clean up
#'   #---------
#'   rm(dat)
#' }
#' @rawRd
#' \keyword{ distribution }
#' @rawRd
#' \keyword{ htest }

eqhyper <-
function (x, m = NULL, total = NULL, k = NULL, p = 0.5, method = "mle", 
    digits = 0) 
{
    if (!is.vector(p, mode = "numeric")) 
        stop("'p' must be a numeric vector.")
    if (any(!is.finite(p))) 
        stop("NA/NaN/Inf values not allowed in 'p'.")
    if (any(p < 0) || any(p > 1)) 
        stop("All values of 'p' must be between 0 and 1.")
    method <- match.arg(method, c("mle", "mvue"))
    if (x.is.est.obj <- data.class(x) == "estimate" || data.class(x) == 
        "estimateCensored") {
        if (x$distribution != "Hypergeometric") 
            stop(paste("'eqhyper' estimates quantiles", "for a hypergeometric distribution.  You have supplied an object", 
                "that assumes a different distribution."))
        class.x <- oldClass(x)
        if (!is.null(x$interval)) {
            x <- x[-match("interval", names(x))]
            oldClass(x) <- class.x
        }
        m <- x$parameters["m"]
        n <- x$parameters["n"]
        k <- x$parameters["k"]
        ret.list <- x
    }
    else {
        if (!is.vector(x, mode = "numeric") || length(x) != 1 || 
            !is.finite(x) || x != trunc(x)) 
            stop(paste("'x' must be either a list that inherits from", 
                "the class 'estimate', or else a non-missing integer"))
        bad.obs <- 0
        if (!is.vector(k, mode = "numeric") || length(k) != 1 || 
            !is.finite(k) || k != trunc(k)) 
            stop("Missing values not allowed for 'k' and it must be an integer")
        if ((is.null(m) & is.null(total)) || (!is.null(m) & !is.null(total))) 
            stop("You must supply 'm' or 'total' but not both")
        if (!is.null(m)) {
            if (!is.vector(m, mode = "numeric") || length(m) != 
                1 || !is.finite(m) || m != trunc(m)) 
                stop("Missing values not allowed for 'm' and it must be an integer")
        }
        else {
            if (!is.vector(total, mode = "numeric") || length(total) != 
                1 || !is.finite(total) || total != trunc(total)) 
                stop("Missing values not allowed for 'total' and it must be an integer")
        }
        data.name <- deparse(substitute(x))
        ret.list <- ehyper(x, m = m, total = total, k = k, method = method)
        ret.list$data.name <- data.name
        ret.list$bad.obs <- bad.obs
        m <- ret.list$parameters["m"]
        n <- ret.list$parameters["n"]
        k <- ret.list$parameters["k"]
    }
    q <- qhyper(p, m = m, n = n, k = k)
    if (length(p) == 1 && p == 0.5) 
        names(q) <- "Median"
    else {
        pct <- round(100 * p, digits)
        names(q) <- paste(pct, number.suffix(pct), " %ile", sep = "")
    }
    ret.list <- c(ret.list, list(quantiles = q))
    ret.list$quantile.method <- paste("Quantile(s) Based on\n", 
        space(33), ret.list$method, " Estimators", sep = "")
    if (x.is.est.obj) 
        oldClass(ret.list) <- class.x
    else oldClass(ret.list) <- "estimate"
    ret.list
}
