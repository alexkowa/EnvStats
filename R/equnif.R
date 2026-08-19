#' Estimate Quantiles of a Uniform Distribution
#' @description
#' Estimate quantiles of a \link[stats:Uniform]{uniform distribution}.
#' @usage
#' equnif(x, p = 0.5, method = "mle", digits = 0)
#' @rawRd
#' \arguments{
#'   \item{x}{
#'   a numeric vector of observations, or an object resulting from a call to an
#'   estimating function that assumes a uniform distribution
#'   (e.g., \code{\link{eunif}}). If \code{x} is a numeric vector,
#'   missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf})
#'   values are allowed but will be removed.
#' }
#'   \item{p}{
#'   numeric vector of probabilities for which quantiles will be estimated.
#'   All values of \code{p} must be between 0 and 1.  The default value is \code{p=0.5}.
#' }
#'   \item{method}{
#'   character string specifying the method of estimating the distribution parameters.
#'   The possible values are
#'   \code{"mle"} (maximum likelihood; the default), \code{"mme"} (method of moments),
#'   and \code{"mmue"} (method of moments based on the unbiased estimator of variance).
#'   See the DETAILS section of the help file for \code{\link{eunif}} for more
#'   information on these estimation methods.
#' }
#'   \item{digits}{
#'   an integer indicating the number of decimal places to round to when printing out
#'   the value of \code{100*p}. The default value is \code{digits=0}.
#' }
#' }
#' @rawRd
#' \details{
#'   The function \code{equnif} returns estimated quantiles as well as
#'   estimates of the location and scale parameters.
#'
#'   Quantiles are estimated by 1) estimating the location and scale parameters by
#'   calling \code{\link{eunif}}, and then 2) calling the function
#'   \code{\link[stats:Uniform]{qunif}} and using the estimated values for
#'   location and scale.
#' }
#' @rawRd
#' \value{
#'   If \code{x} is a numeric vector, \code{equnif} returns a
#'   list of class \code{"estimate"} containing the estimated quantile(s) and other
#'   information. See \code{\link{estimate.object}} for details.
#'
#'   If \code{x} is the result of calling an estimation function, \code{equnif}
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
#'   The \link[stats:Uniform]{uniform distribution} (also called the rectangular
#'   distribution) with parameters \code{min} and \code{max} takes on values on the
#'   real line between \code{min} and \code{max} with equal probability.  It has been
#'   used to represent the distribution of round-off errors in tabulated values.  Another
#'   important application is that the distribution of the cumulative distribution
#'   function (cdf) of any kind of continuous random variable follows a uniform
#'   distribution with parameters \code{min=0} and \code{max=1}.
#' }
#' @rawRd
#' \seealso{
#'   \code{\link{eunif}}, \link[stats]{Uniform}, \code{\link{estimate.object}}.
#' }
#' @rawRd
#' \examples{
#'   # Generate 20 observations from a uniform distribution with parameters
#'   # min=-2 and max=3, then estimate the parameters via maximum likelihood
#'   # and estimate the 90th percentile.
#'   # (Note: the call to set.seed simply allows you to reproduce this example.)
#'
#'   set.seed(250)
#'   dat <- runif(20, min = -2, max = 3)
#'   equnif(dat, p = 0.9)
#'
#'   #Results of Distribution Parameter Estimation
#'   #--------------------------------------------
#'   #
#'   #Assumed Distribution:            Uniform
#'   #
#'   #Estimated Parameter(s):          min = -1.574529
#'   #                                 max =  2.837006
#'   #
#'   #Estimation Method:               mle
#'   #
#'   #Estimated Quantile(s):           90'th %ile = 2.395852
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

equnif <-
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
        if (x$distribution != "Uniform") 
            stop(paste("'equnif' estimates quantiles", "for a uniform distribution.  You have supplied an object", 
                "that assumes a different distribution."))
        class.x <- oldClass(x)
        if (!is.null(x$interval)) {
            x <- x[-match("interval", names(x))]
            oldClass(x) <- class.x
        }
        min <- x$parameters["min"]
        max <- x$parameters["max"]
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
        if (n < 2 || length(unique(x)) < 2) 
            stop(paste("'x' must contain at least 2 non-missing distinct values. ", 
                "This is not true for 'x' =", data.name))
        ret.list <- eunif(x, method = method)
        ret.list$data.name <- data.name
        ret.list$bad.obs <- bad.obs
        min <- ret.list$parameters["min"]
        max <- ret.list$parameters["max"]
    }
    q <- qunif(p, min = min, max = max)
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
