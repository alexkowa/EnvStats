#' Estimate Quantiles of a Beta Distribution
#' @description
#' Estimate quantiles of a \link[stats:Beta]{beta distribution}.
#' @usage
#' eqbeta(x, p = 0.5, method = "mle", digits = 0)
#' @rawRd
#' \arguments{
#'   \item{x}{
#'   a numeric vector of observations, or an object resulting from a call to an
#'   estimating function that assumes a beta distribution
#'   (e.g., \code{\link{ebeta}}). If \code{x} is a numeric vector,
#'   missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf})
#'   values are allowed but will be removed.
#' }
#'   \item{p}{
#'   numeric vector of probabilities for which quantiles will be estimated.
#'   All values of \code{p} must be between 0 and 1.  The default value is \code{p=0.5}.
#' }
#'   \item{method}{
#'   character string specifying the method to use to estimate the shape and scale
#'   parameters of the distribution.  The possible values are
#'   \code{"mle"} (maximum likelihood; the default),
#'   \code{"mme"} (method of moments), and
#'   \code{"mmue"} (method of moments based on the unbiased estimator of variance).
#'   See the DETAILS section of the help file for \code{\link{ebeta}} for more information.
#' }
#'   \item{digits}{
#'   an integer indicating the number of decimal places to round to when printing out
#'   the value of \code{100*p}. The default value is \code{digits=0}.
#' }
#' }
#' @rawRd
#' \details{
#'   The function \code{eqbeta} returns estimated quantiles as well as
#'   estimates of the shape1 and shape2 parameters.
#'
#'   Quantiles are estimated by 1) estimating the shape1 and shape2 parameters by
#'   calling \code{\link{ebeta}}, and then 2) calling the function
#'   \code{\link[stats:Beta]{qbeta}} and using the estimated values for
#'   shape1 and shape2.
#' }
#' @rawRd
#' \value{
#'   If \code{x} is a numeric vector, \code{eqbeta} returns a
#'   list of class \code{"estimate"} containing the estimated quantile(s) and other
#'   information. See \code{\link{estimate.object}} for details.
#'
#'   If \code{x} is the result of calling an estimation function, \code{eqbeta}
#'   returns a list whose class is the same as \code{x}.  The list
#'   contains the same components as \code{x}, as well as components called
#'   \code{quantiles} and \code{quantile.method}.
#' }
#' @rawRd
#' \references{
#'   Forbes, C., M. Evans, N. Hastings, and B. Peacock. (2011).  Statistical Distributions.
#'   Fourth Edition. John Wiley and Sons, Hoboken, NJ.
#'
#'   Johnson, N. L., S. Kotz, and N. Balakrishnan. (1995).
#'   \emph{Continuous Univariate Distributions, Volume 2}.
#'   Second Edition. John Wiley and Sons, New York.
#' }
#' @rawRd
#' \author{
#'     Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \note{
#'   The beta distribution takes real values between 0 and 1.  Special cases of the
#'   beta are the \link[stats:Uniform]{Uniform}[0,1] when \code{shape1=1} and
#'   \code{shape2=1}, and the arcsin distribution when \code{shape1=0.5} and \cr
#'   \code{shape2=0.5}.  The arcsin distribution appears in the theory of random walks.
#'   The beta distribution is used in Bayesian analyses as a conjugate to the binomial
#'   distribution.
#' }
#' @rawRd
#' \seealso{
#'   \code{\link{ebeta}}, \code{\link[stats:Beta]{Beta}}, \code{\link{estimate.object}}.
#' }
#' @rawRd
#' \examples{
#'   # Generate 20 observations from a beta distribution with parameters
#'   # shape1=2 and shape2=4, then estimate the parameters via
#'   # maximum likelihood and estimate the 90'th percentile.
#'   # (Note: the call to set.seed simply allows you to reproduce this example.)
#'
#'   set.seed(250)
#'   dat <- rbeta(20, shape1 = 2, shape2 = 4)
#'   eqbeta(dat, p = 0.9)
#'
#'   #Results of Distribution Parameter Estimation
#'   #--------------------------------------------
#'   #
#'   #Assumed Distribution:            Beta
#'   #
#'   #Estimated Parameter(s):          shape1 =  5.392221
#'   #                                 shape2 = 11.823233
#'   #
#'   #Estimation Method:               mle
#'   #
#'   #Estimated Quantile(s):           90'th %ile = 0.4592796
#'   #
#'   #Quantile Estimation Method:      Quantile(s) Based on
#'   #                                 mle Estimators
#'   #
#'   #Data:                            dat
#'   #
#'   #Sample Size:                     20
#'
#'   #----------
#'   # Clean up
#'
#'   rm(dat)
#' }
#' @rawRd
#' \keyword{ distribution }
#' @rawRd
#' \keyword{ htest }

eqbeta <-
function (x, p = 0.5, method = "mle", digits = 0) 
{
    if (!is.vector(p, mode = "numeric") || is.factor(p)) 
        stop("'p' must be a numeric vector.")
    if (any(!is.finite(p))) 
        stop("NA/NaN/Inf values not allowed in 'p'.")
    if (any(p < 0) || any(p > 1)) 
        stop("All values of 'p' must be between 0 and 1.")
    method <- match.arg(method, c("mle", "mme", "mmue"))
    if (x.is.est.obj <- data.class(x) == "estimate" || data.class(x) == 
        "estimateCensored") {
        if (x$distribution != "Beta") 
            stop(paste("'eqbeta' estimates quantiles", "for a beta distribution.  You have supplied an object", 
                "that assumes a different distribution."))
        class.x <- oldClass(x)
        if (!is.null(x$interval)) {
            x <- x[-match("interval", names(x))]
            oldClass(x) <- class.x
        }
        shape1 <- x$parameters["shape1"]
        shape2 <- x$parameters["shape2"]
        n <- x$sample.size
        ret.list <- x
    }
    else {
        if (!is.vector(x, mode = "numeric") || is.factor(x)) 
            stop(paste("'x' must be either a list that inherits from", 
                "the class 'estimate', or else a numeric vector"))
        data.name <- deparse(substitute(x))
        if ((bad.obs <- sum(!(x.ok <- is.finite(x)))) > 0) {
            is.not.finite.warning(x)
            x <- x[x.ok]
            warning(paste(bad.obs, "observations with NA/NaN/Inf in 'x' removed."))
        }
        n <- length(x)
        if (n < 2 || min(x) < 0 || max(x) > 1 || length(unique(x)) < 
            2) 
            stop(paste("'x' must contain at least 2 non-missing distinct values,", 
                "and all non-missing values of 'x' must be between 0 and 1. ", 
                "This is not true for 'x' =", data.name))
        ret.list <- ebeta(x, method = method)
        ret.list$data.name <- data.name
        ret.list$bad.obs <- bad.obs
        shape1 <- ret.list$parameters["shape1"]
        shape2 <- ret.list$parameters["shape2"]
    }
    q <- qbeta(p, shape1 = shape1, shape2 = shape2)
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
