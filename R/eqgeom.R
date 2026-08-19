#' Estimate Quantiles of a Geometric Distribution
#' @description
#' Estimate quantiles of a \link[stats:Geometric]{geometric distribution}.
#' @usage
#' eqgeom(x, p = 0.5, method = "mle/mme", digits = 0)
#' @rawRd
#' \arguments{
#'   \item{x}{
#'   a numeric vector of observations, or an object resulting from a call to an
#'   estimating function that assumes a geometric distribution
#'   (e.g., \code{\link{egeom}}). If \code{x} is a numeric vector,
#'   missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf})
#'   values are allowed but will be removed.
#' }
#'   \item{p}{
#'   numeric vector of probabilities for which quantiles will be estimated.
#'   All values of \code{p} must be between 0 and 1.  The default value is \code{p=0.5}.
#' }
#'   \item{method}{
#'   character string specifying the method to use to estimate the probability parameter.
#'   Possible values are \code{"mle/mme"} (maximum likelihood and method of moments;
#'   the default) and \code{"mvue"} (minimum variance unbiased).  You cannot use
#'   \code{method="mvue"} if \code{length(x)=1}.  See the DETAILS section of the help file
#'   for \code{\link{egeom}} for more information on these estimation methods.
#' }
#'   \item{digits}{
#'   an integer indicating the number of decimal places to round to when printing out
#'   the value of \code{100*p}. The default value is \code{digits=0}.
#' }
#' }
#' @rawRd
#' \details{
#'   The function \code{eqgeom} returns estimated quantiles as well as
#'   the estimate of the rate parameter.
#'
#'   Quantiles are estimated by 1) estimating the probability parameter by
#'   calling \code{\link{egeom}}, and then 2) calling the function
#'   \code{\link[stats:Geometric]{qgeom}} and using the estimated value for
#'   the probability parameter.
#' }
#' @rawRd
#' \value{
#'   If \code{x} is a numeric vector, \code{eqgeom} returns a
#'   list of class \code{"estimate"} containing the estimated quantile(s) and other
#'   information. See \code{\link{estimate.object}} for details.
#'
#'   If \code{x} is the result of calling an estimation function, \code{eqgeom}
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
#'   New York, Chapter 5.
#' }
#' @rawRd
#' \author{
#'     Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \note{
#'   The \link[stats:Geometric]{geometric distribution} with parameter
#'   \code{prob=}\eqn{p} is a special case of the
#'   \link[stats:NegBinomial]{negative binomial distribution} with parameters
#'   \code{size=1} and \code{prob=p}.
#'
#'   The negative binomial distribution has its roots in a gambling game where
#'   participants would bet on the number of tosses of a coin necessary to achieve
#'   a fixed number of heads.  The negative binomial distribution has been applied
#'   in a wide variety of fields, including accident statistics, birth-and-death
#'   processes, and modeling spatial distributions of biological organisms.
#' }
#' @rawRd
#' \seealso{
#'   \code{\link{egeom}}, \link[stats]{Geometric}, \code{\link{enbinom}},
#'   \link[stats]{NegBinomial}, \code{\link{estimate.object}}.
#' }
#' @rawRd
#' \examples{
#'   # Generate an observation from a geometric distribution with parameter
#'   # prob=0.2, then estimate the parameter prob and the 90'th percentile.
#'   # (Note: the call to set.seed simply allows you to reproduce this example.)
#'
#'   set.seed(250)
#'   dat <- rgeom(1, prob = 0.2)
#'   dat
#'   #[1] 4
#'
#'   eqgeom(dat, p = 0.9)
#'
#'   #Results of Distribution Parameter Estimation
#'   #--------------------------------------------
#'   #
#'   #Assumed Distribution:            Geometric
#'   #
#'   #Estimated Parameter(s):          prob = 0.2
#'   #
#'   #Estimation Method:               mle/mme
#'   #
#'   #Estimated Quantile(s):           90'th %ile = 10
#'   #
#'   #Quantile Estimation Method:      Quantile(s) Based on
#'   #                                 mle/mme Estimators
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

eqgeom <-
function (x, p = 0.5, method = "mle/mme", digits = 0) 
{
    if (!is.vector(p, mode = "numeric") || is.factor(p)) 
        stop("'p' must be a numeric vector.")
    if (any(!is.finite(p))) 
        stop("NA/NaN/Inf values not allowed in 'p'.")
    if (any(p < 0) || any(p > 1)) 
        stop("All values of 'p' must be between 0 and 1.")
    method <- match.arg(method, c("mle/mme", "mvue"))
    if (x.is.est.obj <- data.class(x) == "estimate" || data.class(x) == 
        "estimateCensored") {
        if (x$distribution != "Geometric") 
            stop(paste("'eqgeom' estimates quantiles", "for a geometric distribution.  You have supplied an object", 
                "that assumes a different distribution."))
        class.x <- oldClass(x)
        if (!is.null(x$interval)) {
            x <- x[-match("interval", names(x))]
            oldClass(x) <- class.x
        }
        prob <- x$parameters
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
        if (n < 1 || !all(x == trunc(x)) || any(x < 0)) 
            stop(paste("'x' must contain at least one non-missing value,", 
                "and all values of 'x' must be non-negative integers. ", 
                "This is not true for 'x' =", data.name))
        ret.list <- egeom(x, method = method)
        ret.list$data.name <- data.name
        ret.list$bad.obs <- bad.obs
        prob <- ret.list$parameters
    }
    q <- qgeom(p, prob = prob)
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
