#' Estimate Quantiles of an Exponential Distribution
#' @description
#' Estimate quantiles of an \link[stats:Exponential]{exponential distribution}.
#' @usage
#' eqexp(x, p = 0.5, method = "mle/mme", digits = 0)
#' @rawRd
#' \arguments{
#'   \item{x}{
#'   a numeric vector of observations, or an object resulting from a call to an
#'   estimating function that assumes an exponential distribution
#'   (e.g., \code{\link{eexp}}).  If \code{x} is a numeric vector,
#'   missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf})
#'   values are allowed but will be removed.
#' }
#'   \item{p}{
#'   numeric vector of probabilities for which quantiles will be estimated.
#'   All values of \code{p} must be between 0 and 1.  The default value is \code{p=0.5}.
#' }
#'   \item{method}{
#'   character string specifying the method to use to estimate the rate parameter.
#'   Currently the only possible value is \code{"mle/mme"}
#'   (maximum likelihood/method of moments; the default).  See the DETAILS section of
#'   the help file for \code{\link{eexp}} for more information.
#' }
#'   \item{digits}{
#'   an integer indicating the number of decimal places to round to when printing out
#'   the value of \code{100*p}. The default value is \code{digits=0}.
#' }
#' }
#' @rawRd
#' \details{
#'   The function \code{eqexp} returns estimated quantiles as well as
#'   the estimate of the rate parameter.
#'
#'   Quantiles are estimated by 1) estimating the rate parameter by
#'   calling \code{\link{eexp}}, and then 2) calling the function
#'   \code{\link[stats:Exponential]{qexp}} and using the estimated value for
#'   rate.
#' }
#' @rawRd
#' \value{
#'   If \code{x} is a numeric vector, \code{eqexp} returns a
#'   list of class \code{"estimate"} containing the estimated quantile(s) and other
#'   information. See \code{\link{estimate.object}} for details.
#'
#'   If \code{x} is the result of calling an estimation function, \code{eqexp}
#'   returns a list whose class is the same as \code{x}.  The list
#'   contains the same components as \code{x}, as well as components called
#'   \code{quantiles} and \code{quantile.method}.
#' }
#' @rawRd
#' \references{
#'   Forbes, C., M. Evans, N. Hastings, and B. Peacock. (2011).  Statistical Distributions.
#'   Fourth Edition. John Wiley and Sons, Hoboken, NJ.
#'
#'   Johnson, N. L., S. Kotz, and N. Balakrishnan. (1994).
#'   \emph{Continuous Univariate Distributions, Volume 1}.
#'   Second Edition. John Wiley and Sons, New York.
#' }
#' @rawRd
#' \author{
#'     Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \note{
#'   The \link[stats:Exponential]{exponential distribution} is a special case of the
#'   \link[stats:GammaDist]{gamma distribution}, and
#'   takes on positive real values.  A major use of the exponential distribution is
#'   in life testing where it is used to model the lifetime of a product, part,
#'   person, etc.
#'
#'   The exponential distribution is the only continuous distribution with a
#'   \dQuote{lack of memory} property.  That is, if the lifetime of a part follows
#'   the exponential distribution, then the distribution of the time until failure
#'   is the same as the distribution of the time until failure given that the part
#'   has survived to time \eqn{t}.
#'
#'   The exponential distribution is related to the double exponential (also called
#'   Laplace) distribution, and to the \link[=EVD]{extreme value distribution}.
#' }
#' @rawRd
#' \seealso{
#'   \code{\link{eexp}}, \link[stats:Exponential]{Exponential},
#'   \code{\link{estimate.object}}.
#' }
#' @rawRd
#' \examples{
#'   # Generate 20 observations from an exponential distribution with parameter
#'   # rate=2, then estimate the parameter and estimate the 90th percentile.
#'   # (Note: the call to set.seed simply allows you to reproduce this example.)
#'
#'   set.seed(250)
#'   dat <- rexp(20, rate = 2)
#'   eqexp(dat, p = 0.9)
#'
#'   #Results of Distribution Parameter Estimation
#'   #--------------------------------------------
#'   #
#'   #Assumed Distribution:            Exponential
#'   #
#'   #Estimated Parameter(s):          rate = 2.260587
#'   #
#'   #Estimation Method:               mle/mme
#'   #
#'   #Estimated Quantile(s):           90'th %ile = 1.018578
#'   #
#'   #Quantile Estimation Method:      Quantile(s) Based on
#'   #                                 mle/mme Estimators
#'   #
#'   #Data:                            dat
#'   #
#'   #Sample Size:                     20
#'   #
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

eqexp <-
function (x, p = 0.5, method = "mle/mme", digits = 0) 
{
    if (!is.vector(p, mode = "numeric") || is.factor(p)) 
        stop("'p' must be a numeric vector.")
    if (any(!is.finite(p))) 
        stop("NA/NaN/Inf values not allowed in 'p'.")
    if (any(p < 0) || any(p > 1)) 
        stop("All values of 'p' must be between 0 and 1.")
    method <- match.arg(method)
    if (x.is.est.obj <- data.class(x) == "estimate" || data.class(x) == 
        "estimateCensored") {
        if (x$distribution != "Exponential") 
            stop(paste("'eqexp' estimates quantiles", "for an exponential distribution.  You have supplied an object", 
                "that assumes a different distribution."))
        class.x <- oldClass(x)
        if (!is.null(x$interval)) {
            x <- x[-match("interval", names(x))]
            oldClass(x) <- class.x
        }
        rate <- x$parameters
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
        if (n < 1 || any(x < 0) || all(x == 0)) 
            stop(paste("'x' must contain at least one non-missing value,", 
                "all non-missing values of 'x' must be non-negative,", 
                "and at least one value of 'x' must be positive. ", 
                "This is not true for 'x' =", data.name))
        ret.list <- eexp(x, method = method)
        ret.list$data.name <- data.name
        ret.list$bad.obs <- bad.obs
        rate <- ret.list$parameters
    }
    q <- qexp(p, rate = rate)
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
