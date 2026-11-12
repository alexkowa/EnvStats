#' Estimate Quantiles of a Negative Binomial Distribution
#' @description
#' Estimate quantiles of a \link[stats:NegBinomial]{negative binomial distribution}.
#' @usage
#' eqnbinom(x, size = NULL, p = 0.5, method = "mle/mme", digits = 0)
#' @rawRd
#' \arguments{
#'   \item{x}{
#'   vector of non-negative integers indicating the number of trials that took place
#'   \emph{before} \code{size} \dQuote{successes} occurred (the total number of
#'   trials that took place is \code{x+1}), or an object resulting
#'   from a call to an estimating function that assumes a negative binomial distribution
#'   (e.g., \code{\link{enbinom}}).  If \code{x} is a vector of non-negative integers, then
#'   missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf})
#'   values are allowed but will be removed.  If \code{length(x)=n} and \code{n} is
#'   greater than 1, it is assumed that \code{x} represents observations from \code{n}
#'   separate negative binomial experiments that all had the same probability of
#'   success (\code{prob}), but possibly different values of \code{size}.
#' }
#'   \item{size}{
#'   vector of positive integers indicating the number of \dQuote{successes} that
#'   must be observed before the trials are stopped.  Missing (\code{NA}),
#'   undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) values are allowed
#'   but will be removed.  The length of \code{size} must be 1 or else the same
#'   length as \code{x}.
#' }
#'   \item{p}{
#'   numeric vector of probabilities for which quantiles will be estimated.
#'   All values of \code{p} must be between 0 and 1.  The default value is \code{p=0.5}.
#' }
#'   \item{method}{
#'   character string specifying the method of estimating the probability parameter.
#'   Possible values are
#'   \code{"mle/mme"} (maximum likelihood and method of moments; the default) and
#'   \code{"mvue"} (minimum variance unbiased).  You cannot use \code{method="mvue"} if
#'   the sum of the elements in \code{size} is 1.  See the DETAILS section of the help file
#'   for \code{\link{enbinom}} for more information on these estimation methods.
#' }
#'   \item{digits}{
#'   an integer indicating the number of decimal places to round to when printing out
#'   the value of \code{100*p}. The default value is \code{digits=0}.
#' }
#' }
#' @rawRd
#' \details{
#'   The function \code{eqnbinom} returns estimated quantiles as well as
#'   estimates of the \code{prob} parameter.
#'
#'   Quantiles are estimated by 1) estimating the prob parameter by
#'   calling \code{\link{enbinom}}, and then 2) calling the function
#'   \code{\link[stats:NegBinomial]{qnbinom}} and using the estimated value for
#'   \code{prob}.
#' }
#' @rawRd
#' \value{
#'   If \code{x} is a numeric vector, \code{eqnbinom} returns a
#'   list of class \code{"estimate"} containing the estimated quantile(s) and other
#'   information. See \code{\link{estimate.object}} for details.
#'
#'   If \code{x} is the result of calling an estimation function, \code{eqnbinom}
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
#'   The \link[stats:NegBinomial]{negative binomial distribution} has its roots in
#'   a gambling game where participants would bet on the number of tosses of a
#'   coin necessary to achieve a fixed number of heads.  The negative binomial
#'   distribution has been applied in a wide variety of fields, including accident
#'   statistics, birth-and-death processes, and modeling spatial distributions of
#'   biological organisms.
#'
#'   The \link[stats:Geometric]{geometric distribution} with parameter \code{prob=}\eqn{p}
#'   is a special case of the negative binomial distribution with parameters
#'   \code{size=1} and \code{prob=}\eqn{p}.
#' }
#' @rawRd
#' \seealso{
#'   \code{\link{enbinom}}, \link[stats]{NegBinomial}, \code{\link{egeom}},
#'   \link[stats]{Geometric}, \code{\link{estimate.object}}.
#' }
#' @rawRd
#' \examples{
#'   # Generate an observation from a negative binomial distribution with
#'   # parameters size=2 and prob=0.2, then estimate the parameter prob
#'   # and the 90th percentile.
#'   # Note: the call to set.seed simply allows you to reproduce this example.
#'   # Also, the only parameter that is estimated is prob; the parameter
#'   # size is supplied in the call to enbinom.  The parameter size is printed in
#'   # order to show all of the parameters associated with the distribution.
#'
#'   set.seed(250)
#'   dat <- rnbinom(1, size = 2, prob = 0.2)
#'   dat
#'   #[1] 5
#'
#'   eqnbinom(dat, size = 2, p = 0.9)
#'
#'   #Results of Distribution Parameter Estimation
#'   #--------------------------------------------
#'   #
#'   #Assumed Distribution:            Negative Binomial
#'   #
#'   #Estimated Parameter(s):          size = 2.0000000
#'   #                                 prob = 0.2857143
#'   #
#'   #Estimation Method:               mle/mme for 'prob'
#'   #
#'   #Estimated Quantile(s):           90'th %ile = 11
#'   #
#'   #Quantile Estimation Method:      Quantile(s) Based on
#'   #                                 mle/mme for 'prob' Estimators
#'   #
#'   #Data:                            dat, 2
#'   #
#'   #Sample Size:                     1
#'
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

eqnbinom <-
function (x, size = NULL, p = 0.5, method = "mle/mme", digits = 0) 
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
        if (x$distribution != "Negative Binomial") 
            stop(paste("'eqnbinom' estimates quantiles", "for a negative binomial distribution.  You have supplied an object", 
                "that assumes a different distribution."))
        class.x <- oldClass(x)
        if (!is.null(x$interval)) {
            x <- x[-match("interval", names(x))]
            oldClass(x) <- class.x
        }
        size <- x$parameters["size"]
        prob <- x$parameters["prob"]
        n <- x$sample.size
        ret.list <- x
    }
    else {
        if (!is.vector(x, mode = "numeric") || !is.vector(size, 
            mode = "numeric")) 
            stop(paste("'x' must be either a list that inherits from", 
                "the class 'estimate', or else a numeric vector,", 
                "and 'size' must be a numeric vector"))
        data.name <- deparse(substitute(x))
        if ((bad.obs <- sum(!(all.ok <- is.finite(x) & is.finite(size)))) > 
            0) {
            is.not.finite.warning(x)
            is.not.finite.warning(size)
            x <- x[all.ok]
            size <- size[all.ok]
            warning(paste(bad.obs, "observations with NA/NaN/Inf in 'x' and/or 'size' removed."))
        }
        n <- length(x)
        if (n < 1) 
            stop("'x' and 'size' must contain at least one non-missing pair of values.")
        if (!all(x == trunc(x)) || any(x < 0) || !all(size == 
            trunc(size)) || any(size < 1)) 
            stop(paste("All values of 'x' must be non-negative integers,", 
                "and all values of 'size' must be positive integers."))
        ret.list <- enbinom(x, size = size, method = method)
        ret.list$data.name <- data.name
        ret.list$bad.obs <- bad.obs
        size <- ret.list$parameters["size"]
        prob <- ret.list$parameters["prob"]
    }
    q <- qnbinom(p, size = size, prob = prob)
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
