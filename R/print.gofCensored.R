#' Print Output of Goodness-of-Fit Tests Based on Censored Data
#' @description
#' Formats and prints the results of performing a goodness-of-fit test.  This method is
#'   automatically called by \code{\link[base]{print}} when given an object of class
#'   \code{"gofCensored"}.  Currently, the only function that produces an object of
#'   this class is \code{\link{gofTestCensored}}.
#' @usage
#' \method{print}{gofCensored}(x, show.cen.levels = TRUE,
#'   pct.censored.digits = .Options$digits, ...)
#' @rawRd
#' \arguments{
#'   \item{x}{
#'   an object of class \code{"gofCensored"}.  See \code{\link{gofCensored.object}} for
#'   details.
#' }
#'   \item{show.cen.levels}{
#'   logical scalar indicating whether to print the censoring levels.  The default is
#'   \code{show.cen.levels=TRUE}.
#' }
#'   \item{pct.censored.digits}{
#'   numeric scalar indicating the number of significant digits to print for the
#'   percent of censored observations.
#' }
#'   \item{\dots}{
#'   arguments that can be supplied to the \code{\link[base]{format}} function.
#' }
#' }
#' @rawRd
#' \details{
#'   This is the \code{"gofCensored"} method for the generic function
#'   \code{\link[base]{print}}.
#'   Prints name of the test, hypothesized distribution, estimated population parameter(s),
#'   estimation method, data name, sample size, censoring information, value of the test
#'   statistic, parameters associated with the null distribution of the test statistic,
#'   p-value associated with the test statistic, and the alternative hypothesis.
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
#'   \link[=FcnsByCatCensoredData]{Censored Data}, \code{\link{gofCensored.object}},
#'   \code{\link[base]{print}}.
#' }
#' @rawRd
#' \keyword{print}
#' @exportS3Method NULL

print.gofCensored <-
function (x, show.cen.levels = TRUE, pct.censored.digits = .Options$digits, 
    ...) 
{
    coll.string <- paste("\n", space(33), sep = "")
    cat("\nResults of Goodness-of-Fit Test\n")
    cat("Based on Type I Censored Data\n")
    cat("-------------------------------\n\n")
    cat("Test Method:", space(21), x$method, "\n\n", sep = "")
    cat("Hypothesized Distribution:", space(7), x$distribution, 
        "\n\n", sep = "")
    cat("Censoring Side:", space(18), x$censoring.side, "\n\n", 
        sep = "")
    if (show.cen.levels) {
        cat("Censoring Level(s):", space(12), format(x$censoring.levels, 
            nsmall = 0, justify = "left", ...), "\n\n")
    }
    if (!is.null(x$n.param.est) && x$n.param.est > 0) {
        cat("Estimated Parameter(s):", space(10), paste(paste(format(names(x$distribution.parameters), 
            justify = "left"), format(x$distribution.parameters, 
            nsmall = 0, ...), sep = " = "), collapse = coll.string), 
            "\n\n", sep = "")
        cat("Estimation Method:", space(15), x$estimation.method, 
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
    if (length(x$sample.size) > 1) {
        cat("Sample Sizes:", space(20), paste(paste(format(names(x$sample.size), 
            justify = "left"), format(x$sample.size, nsmall = 0, 
            ...), sep = " = "), collapse = coll.string), "\n\n", 
            sep = "")
    }
    else {
        cat("Sample Size:", space(21), x$sample.size, "\n\n", 
            sep = "")
    }
    cat("Percent Censored:", space(16), round(x$percent.censored, 
        pct.censored.digits), "%", "\n\n", sep = "")
    cat("Test Statistic:", space(18), paste(paste(names(x$statistic), 
        format(x$statistic, nsmall = 0, ...), sep = " = "), collapse = coll.string), 
        "\n\n", sep = "")
    string <- ifelse(length(x$parameters) > 1, paste("Test Statistic Parameters:", 
        space(7), sep = ""), paste("Test Statistic Parameter:", 
        space(6), sep = ""))
    cat(string, paste(paste(format(names(x$parameters), justify = "left"), 
        format(x$parameters, nsmall = 0, ...), sep = " = "), 
        collapse = coll.string), "\n\n", sep = "")
    cat("P-value:", space(25), format(x$p.value, nsmall = 0, 
        ...), "\n\n", sep = "")
    cat("Alternative Hypothesis:", space(10), x$alternative, 
        "\n", sep = "")
    invisible(x)
}
