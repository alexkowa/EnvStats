#' Print Objects of Class "estimate"
#' @description
#' Formats and prints the results of \pkg{EnvStats} functions that estimate
#'   the parameters or quantiles of a probability distribution and optionally
#'   construct confidence, prediction, or tolerance intervals based on a sample
#'   of data assumed to come from that distribution.
#'   This method is automatically called by \code{\link{print}} when given an
#'   object of class \code{"estimate"}.
#'
#'   See the help files
#'   \link{Estimating Distribution Parameters} and
#'   \link{Estimating Distribution Quantiles}
#'   for lists of functions that estimate distribution parameters
#'   and quantiles.  See the help files \link{Prediction Intervals}
#'   and \link{Tolerance Intervals} for lists of functions
#'   that create prediction and tolerance intervals.
#' @usage
#' \method{print}{estimate}(x, conf.cov.sig.digits = .Options$digits,
#'   limits.sig.digits = .Options$digits, ...)
#' @rawRd
#' \arguments{
#'   \item{x}{
#'   an object of class \code{"estimate"}.  See \code{\link{estimate.object}} for details.
#' }
#'   \item{conf.cov.sig.digits}{
#'   numeric scalar indicating the number of significant digits to print for the
#'   confidence level or coverage of a confidence, prediction, or tolerance interval.
#' }
#'   \item{limits.sig.digits}{
#'   numeric scalar indicating the number of significant digits to print for the upper
#'   and lower limits of a confidence, prediction, or tolerance interval.
#' }
#'   \item{\dots}{
#'   arguments that can be supplied to the \code{\link[base]{format}} function.
#' }
#' }
#' @rawRd
#' \details{
#'   This is the \code{"estimate"} method for the generic function
#'   \code{\link[base]{print}}.
#'   Prints estimated parameters and, if present in the object, information regarding
#'   confidence, prediction, or tolerance intervals.
#' }
#' @rawRd
#' \value{
#'   Invisibly returns the input \code{x}.
#' }
#' @rawRd
#' \references{
#'   Chambers, J. M. and Hastie, T. J. (1992).  \emph{Statistical Models in S}.
#'   Wadsworth & Brooks/Cole.
#' }
#' @rawRd
#' \author{
#'   Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \seealso{
#'   \code{\link{estimate.object}},
#'   \link{Estimating Distribution Parameters},
#'   \link{Estimating Distribution Quantiles},
#'   \link{Prediction Intervals}, \link{Tolerance Intervals},
#'   \code{\link[base]{print}}.
#' }
#' @rawRd
#' \keyword{print}

print.estimate <-
function (x, conf.cov.sig.digits = .Options$digits, limits.sig.digits = .Options$digits, 
    ...) 
{
    coll.string <- paste("\n", space(33), sep = "")
    cat("\nResults of Distribution Parameter Estimation\n")
    cat("--------------------------------------------\n\n")
    cat("Assumed Distribution:", space(12), x$distribution, "\n\n", 
        sep = "")
    if (!is.null(x$par)) {
        cat("Estimated Parameter(s):", space(10), paste(paste(format(names(x$par), 
            justify = "left"), format(x$par, ..., nsmall = 0), 
            sep = " = "), collapse = paste("\n", space(33), sep = "")), 
            "\n\n", sep = "")
        if (!is.null(x$method)) 
            cat("Estimation Method:", space(15), x$method, "\n\n", 
                sep = "")
    }
    if (!is.null(x$quantiles)) {
        cat("Estimated Quantile(s):", space(11), paste(paste(format(names(x$quantiles), 
            justify = "left"), format(x$quantiles, ..., nsmall = 0), 
            sep = " = "), collapse = paste("\n", space(33), sep = "")), 
            "\n\n", sep = "")
        if (!is.null(x$quantile.method)) 
            cat("Quantile Estimation Method:", space(6), x$quantile.method, 
                "\n\n", sep = "")
    }
    if (is.null(names(x$data.name))) 
        cat("Data:", space(28), x$data.name, "\n\n", sep = "")
    else cat("Data:", space(28), paste(paste(format(names(x$data.name), 
        justify = "left"), format(x$data.name, ...), sep = " = "), 
        collapse = coll.string), "\n\n", sep = "")
    if (!is.null(x$subset.expression)) 
        cat("Subset With:", space(21), x$subset.expression, "\n\n", 
            sep = "")
    if (!is.null(x$parent.of.data)) 
        cat("Data Source:", space(21), x$parent.of.data, "\n\n", 
            sep = "")
    if (!is.null(x$sample.size)) {
        if (length(x$sample.size) > 1) {
            cat("Sample Sizes:", space(20), paste(paste(format(names(x$sample.size), 
                justify = "left"), format(x$sample.size, nsmall = 0, 
                ...), sep = " = "), collapse = coll.string), 
                "\n\n", sep = "")
        }
        else {
            cat("Sample Size:", space(21), x$sample.size, "\n\n", 
                sep = "")
        }
    }
    if (!is.null(x$bad.obs) && any(x$bad.obs > 0)) {
        if (length(x$bad.obs) > 1) 
            cat("Number NA/NaN/Inf's:", space(13), paste(paste(format(names(x$bad.obs), 
                justify = "left"), format(x$bad.obs, nsmall = 0, 
                ...), sep = " = "), collapse = coll.string), 
                "\n\n", sep = "")
        else cat("Number NA/NaN/Inf's:", space(13), x$bad.obs, 
            "\n\n", sep = "")
    }
    if (!is.null(x$interval)) {
        print.intervalEstimate(x$interval, conf.cov.sig.digits = conf.cov.sig.digits, 
            limits.sig.digits = limits.sig.digits, ...)
    }
    invisible(x)
}
