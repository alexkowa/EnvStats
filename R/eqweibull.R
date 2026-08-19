#' Estimate Quantiles of a Weibull Distribution
#' @description
#' Estimate quantiles of a \link[stats:Weibull]{Weibull distribution}.
#' @usage
#' eqweibull(x, p = 0.5, method = "mle", digits = 0)
#' @rawRd
#' \arguments{
#'   \item{x}{
#'   a numeric vector of observations, or an object resulting from a call to an
#'   estimating function that assumes a Weibull distribution
#'   (e.g., \code{\link{eweibull}}).  If \code{x} is a numeric vector,
#'   missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf})
#'   values are allowed but will be removed.
#' }
#'   \item{p}{
#'   numeric vector of probabilities for which quantiles will be estimated.
#'   All values of \code{p} must be between 0 and 1.  The default value is \code{p=0.5}.
#' }
#'   \item{method}{
#'   character string specifying the method of estimating the distribution parameters.
#'   Possible values are
#'   \code{"mle"} (maximum likelihood; the default), \code{"mme"} (methods of moments),
#'   and \code{"mmue"} (method of moments based on the unbiased estimator of variance).
#'   See the DETAILS section of the help file for \code{\link{eweibull}} for more
#'   information.
#' }
#'   \item{digits}{
#'   an integer indicating the number of decimal places to round to when printing out
#'   the value of \code{100*p}. The default value is \code{digits=0}.
#' }
#' }
#' @rawRd
#' \details{
#'   The function \code{eqweibull} returns estimated quantiles as well as
#'   estimates of the shape and scale parameters.
#'
#'   Quantiles are estimated by 1) estimating the shape and scale parameters by
#'   calling \code{\link{eweibull}}, and then 2) calling the function
#'   \code{\link[stats:Weibull]{qweibull}} and using the estimated values for
#'   shape and scale.
#' }
#' @rawRd
#' \value{
#'   If \code{x} is a numeric vector, \code{eqweibull} returns a
#'   list of class \code{"estimate"} containing the estimated quantile(s) and other
#'   information. See \code{\link{estimate.object}} for details.
#'
#'   If \code{x} is the result of calling an estimation function, \code{eqweibull}
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
#'   The \link[stats:Weibull]{Weibull distribution} is named after the Swedish physicist
#'   Waloddi Weibull, who used this distribution to model breaking strengths of
#'   materials.  The Weibull distribution has been extensively applied in the fields
#'   of reliability and quality control.
#'
#'   The \link[stats:Exponential]{exponential distribution} is a special case of the
#'   Weibull distribution: a Weibull random variable with parameters \code{shape=}\eqn{1}
#'   and \code{scale=}\eqn{\beta} is equivalent to an exponential random variable with
#'   parameter \code{rate=}\eqn{1/\beta}.
#'
#'   The Weibull distribution is related to the
#'   \link[=EVD]{Type I extreme value (Gumbel) distribution} as follows:
#'   if \eqn{X} is a random variable from a Weibull distribution with parameters
#'   \code{shape=}\eqn{\alpha} and \code{scale=}\eqn{\beta}, then
#'   \deqn{Y = -log(X) \;\;\;\; (10)}
#'   is a random variable from an extreme value distribution with parameters
#'   \code{location=}\eqn{-log(\beta)} and \code{scale=}\eqn{1/\alpha}.
#' }
#' @rawRd
#' \seealso{
#'   \code{\link{eweibull}}, \link[stats]{Weibull}, \link[stats]{Exponential},
#'   \link{EVD}, \code{\link{estimate.object}}.
#' }
#' @rawRd
#' \examples{
#'   # Generate 20 observations from a Weibull distribution with parameters
#'   # shape=2 and scale=3, then estimate the parameters via maximum likelihood,
#'   # and estimate the 90'th percentile.
#'   # (Note: the call to set.seed simply allows you to reproduce this example.)
#'
#'   set.seed(250)
#'   dat <- rweibull(20, shape = 2, scale = 3)
#'   eqweibull(dat, p = 0.9)
#'
#'   #Results of Distribution Parameter Estimation
#'   #--------------------------------------------
#'   #
#'   #Assumed Distribution:            Weibull
#'   #
#'   #Estimated Parameter(s):          shape = 2.673098
#'   #                                 scale = 3.047762
#'   #
#'   #Estimation Method:               mle
#'   #
#'   #Estimated Quantile(s):           90'th %ile = 4.163755
#'   #
#'   #Quantile Estimation Method:      Quantile(s) Based on
#'   #                                 mle Estimators
#'   #
#'   #Data:                            dat
#'   #
#'   #Sample Size:                     20
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

eqweibull <-
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
        if (x$distribution != "Weibull") 
            stop(paste("'eqweibull' estimates quantiles", "for a Weibull distribution.  You have supplied an object", 
                "that assumes a different distribution."))
        class.x <- oldClass(x)
        if (!is.null(x$interval)) {
            x <- x[-match("interval", names(x))]
            oldClass(x) <- class.x
        }
        shape <- x$parameters["shape"]
        scale <- x$parameters["scale"]
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
        if (n < 2 || any(x < 0) || length(unique(x)) < 2) 
            stop(paste("'x' must contain at least 2 non-missing distinct values,", 
                "and all non-missing values of 'x' must be non-negative. ", 
                "This is not true for 'x' =", data.name))
        ret.list <- eweibull(x, method = method)
        ret.list$data.name <- data.name
        ret.list$bad.obs <- bad.obs
        shape <- ret.list$parameters["shape"]
        scale <- ret.list$parameters["scale"]
    }
    q <- qweibull(p, shape = shape, scale = scale)
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
