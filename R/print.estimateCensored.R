#' Print Objects of Class "estimateCensored"
#' @description
#' Formats and prints the results of \pkg{EnvStats} functions that estimate
#'   the parameters or quantiles of a probability distribution and optionally
#'   construct confidence, prediction, or tolerance intervals based on a sample
#'   of Tyep I censored data assumed to come from that distribution.
#'   This method is automatically called by \code{\link{print}} when given an
#'   object of class \code{"estimateCensored"}.
#'
#'   See the subsections \emph{Estimating Distribution Parameters} and
#'   \emph{Estimating Distribution Quantiles} in the help file
#'   \link{Censored Data} for lists of functions that estimate
#'   distribution parameters and quantiles based on Type I censored data.
#'
#'   See the subsection \emph{Prediction and Tolerance Intervals}
#'   in the help file \link{Censored Data} for lists of functions
#'   that create prediction and tolerance intervals.
#' @usage
#' \method{print}{estimateCensored}(x, show.cen.levels = TRUE,
#'   pct.censored.digits = .Options$digits,
#'   conf.cov.sig.digits = .Options$digits, limits.sig.digits = .Options$digits,
#'   ...)
#' @rawRd
#' \arguments{
#'   \item{x}{
#'   an object of class \code{"estimateCensored"}.  See
#'   \code{\link{estimateCensored.object}} for details.
#' }
#'   \item{show.cen.levels}{
#'   logical scalar indicating whether to print the censoring levels.  The default is
#'   \code{show.cen.levels=TRUE}.
#' }
#'   \item{pct.censored.digits}{
#'   numeric scalar indicating the number of significant digits to print for the
#'   percent of censored observations.
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
#'   This is the \code{"estimateCensored"} method for the generic function
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
#'   \code{\link{estimateCensored.object}},
#'   \link{Censored Data}, \code{\link[base]{print}}.
#' }
#' @rawRd
#' \keyword{print}
#' @exportS3Method NULL

print.estimateCensored <-
function (x, show.cen.levels = TRUE, pct.censored.digits = .Options$digits, 
    conf.cov.sig.digits = .Options$digits, limits.sig.digits = .Options$digits, 
    ...) 
{
    coll.string <- paste("\n", space(33), sep = "")
    cat("\nResults of Distribution Parameter Estimation\n")
    cat("Based on Type I Censored Data\n")
    cat("--------------------------------------------\n\n")
    cat("Assumed Distribution:", space(12), x$distribution, "\n\n", 
        sep = "")
    cat("Censoring Side:", space(18), x$censoring.side, "\n\n", 
        sep = "")
    if (show.cen.levels) {
        cat("Censoring Level(s):", space(12), format(x$censoring.levels, 
            nsmall = 0, justify = "left", ...), "\n\n")
    }
    if (!is.null(x$parameters)) {
        cat("Estimated Parameter(s):", space(10), paste(paste(format(names(x$parameters), 
            justify = "left"), format(x$parameters, ..., nsmall = 0), 
            sep = " = "), collapse = coll.string), "\n\n", sep = "")
        if (!is.null(x$method)) 
            cat("Estimation Method:", space(15), x$method, "\n\n", 
                sep = "")
        if (!is.null(x$prob.method)) 
            cat("Plotting Position Method:", space(8), x$prob.method, 
                "\n\n", sep = "")
        if (!is.null(x$plot.pos.con)) 
            cat("Plotting Position Constant:", space(6), x$plot.pos.con, 
                "\n\n", sep = "")
    }
    if (!is.null(x$quantiles)) {
        cat("Estimated Quantile(s):", space(11), paste(paste(format(names(x$quantiles), 
            justify = "left"), format(x$quantiles, ..., nsmall = 0), 
            sep = " = "), collapse = coll.string), "\n\n", sep = "")
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
    cat("Censoring Variable:", space(14), x$censoring.name, "\n\n", 
        sep = "")
    if (!is.null(x$bad.obs) && x$bad.obs > 0) 
        cat("Number NA/NaN/Inf's Removed:", space(5), x$bad.obs, 
            "\n\n", sep = "")
    cat("Sample Size:", space(21), x$sample.size, "\n\n", sep = "")
    cat("Percent Censored:", space(16), round(x$percent.censored, 
        pct.censored.digits), "%", "\n\n", sep = "")
    if (!is.null(x$interval)) {
        print.intervalEstimateCensored(x$interval, conf.cov.sig.digits = conf.cov.sig.digits, 
            limits.sig.digits = limits.sig.digits, ...)
    }
    invisible(x)
}
