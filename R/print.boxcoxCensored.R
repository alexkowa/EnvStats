#' Print Output of Objective for Box-Cox Power Transformations Based on Type I Censored Data
#' @description
#' Formats and prints the results of calling the function \code{\link{boxcoxCensored}}.
#'   This method is automatically called by \code{\link{print}} when given an
#'   object of class \code{"boxcoxCensored"}.
#' @usage
#' \method{print}{boxcoxCensored}(x, ...)
#' @rawRd
#' \arguments{
#'   \item{x}{
#'   an object of class \code{"boxcoxCensored"}.  See \code{\link{boxcoxCensored.object}}
#'   for details.
#' }
#'   \item{\dots}{
#'   arguments that can be supplied to the \code{\link[base]{format}} function.
#' }
#' }
#' @rawRd
#' \details{
#'   This is the \code{"boxcoxCensored"} method for the generic function
#'   \code{\link[base]{print}}.
#'   Prints the objective name, the name of the data object used, the sample size,
#'   the percentage of censored observations, the values of the powers, and the
#'   values of the objective.  In the case of
#'   optimization, also prints the range of powers over which the optimization
#'   took place.
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
#'   \code{\link{boxcoxCensored}}, \code{\link{boxcoxCensored.object}},
#'   \code{\link{plot.boxcoxCensored}},
#'   \link{Data Transformations}, \code{\link[base]{print}}.
#' }
#' @rawRd
#' \keyword{print}

print.boxcoxCensored <-
function (x, ...) 
{
    coll.string <- paste("\n", space(33), sep = "")
    cat("\nResults of Box-Cox Transformation\n")
    cat("Based on Type I Censored Data\n")
    cat("---------------------------------\n\n")
    cat("Objective Name:", space(18), x$objective.name, "\n\n", 
        sep = "")
    if (is.null(names(x$data.name))) 
        cat("Data:", space(28), x$data.name, "\n\n", sep = "")
    else cat("Data:", space(28), paste(paste(format(names(x$data.name), 
        justify = "left"), format(x$data.name, ...), sep = " = "), 
        collapse = coll.string), "\n\n", sep = "")
    if (!is.null(x$subset.expression)) 
        cat("Subset With:", space(21), x$subset.expression, "\n\n", 
            sep = "")
    cat("Censoring Variable:", space(14), x$censoring.name, "\n\n", 
        sep = "")
    cat("Censoring Side:", space(18), x$censoring.side, "\n\n", 
        sep = "")
    cat("Censoring Level(s):", space(12), format(x$censoring.levels, 
        nsmall = 0, justify = "left", ...), "\n\n")
    if (!is.null(x$parent.of.data)) 
        cat("Data Source:", space(21), x$parent.of.data, "\n\n", 
            sep = "")
    if (!is.null(x$bad.obs) && any(x$bad.obs > 0)) 
        cat("Number NA/NaN/Inf's Removed:", space(5), x$bad.obs, 
            "\n\n", sep = "")
    cat("Sample Size:", space(21), x$sample.size, "\n\n", sep = "")
    cat("Percent Censored:", space(16), round(x$percent.censored, 
        1), "%", "\n\n", sep = "")
    if (x$optimize) {
        cat("Bounds for Optimization:", space(9), paste(paste(format(names(x$optimize.bounds), 
            justify = "left"), format(x$optimize.bounds, nsmall = 0, 
            ...), sep = " = "), collapse = paste("\n", space(33), 
            sep = "")), "\n\n", sep = "")
        cat("Optimal Value:", space(19), paste("lambda =", format(x$lambda, 
            ...)), "\n\n", sep = "")
        cat("Value of Objective:", space(14), paste(x$objective.name, 
            "=", format(x$objective, ...)), "\n\n", sep = "")
    }
    else {
        dum.mat <- cbind(x$lambda, x$objective)
        dimnames(dum.mat) <- list(rep("", nrow(dum.mat)), c("lambda", 
            x$objective.name))
        print(dum.mat, ...)
    }
    invisible(x)
}
