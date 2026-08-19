#' Print Output of Objective for Box-Cox Power Transformations for an "lm" Object
#' @description
#' Formats and prints the results of calling the function \code{\link{boxcox}}
#'   when the argument \code{x} supplied to \code{\link{boxcox}} is an object of
#'   class \code{"lm"}.  This method is automatically called by \code{\link{print}}
#'   when given an object of class \code{"boxcoxLm"}.  The names of other functions
#'   involved in Box-Cox transformations are listed under \link{Data Transformations}.
#' @usage
#' \method{print}{boxcoxLm}(x, ...)
#' @rawRd
#' \arguments{
#'   \item{x}{
#'   an object of class \code{"boxcoxLm"}.  See \code{\link{boxcoxLm.object}} for details.
#' }
#'   \item{\dots}{
#'   arguments that can be supplied to the \code{\link[base]{format}} function.
#' }
#' }
#' @rawRd
#' \details{
#'   This is the \code{"boxcoxLm"} method for the generic function \code{\link[base]{print}}.
#'   Prints the objective name,  the details of the \code{"lm"} object used,
#'   the sample size,
#'   the values of the powers, and the values of the objective.  In the case of
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
#'   \code{\link{boxcox}}, \code{\link{boxcoxLm.object}}, \code{\link{plot.boxcoxLm}},
#'   \link{Data Transformations}, \code{\link[base]{print}}.
#' }
#' @rawRd
#' \keyword{print}
#' @exportS3Method NULL

print.boxcoxLm <-
function (x, ...) 
{
    cat("\nResults of Box-Cox Transformation\n")
    cat("---------------------------------\n\n")
    cat("Objective Name:", space(18), x$objective.name, "\n\n", 
        sep = "")
    data.name <- x$data.name
    cat("Linear Model:", space(20), data.name, "\n\n", sep = "")
    cat("Sample Size:", space(21), x$sample.size, "\n\n", sep = "")
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
