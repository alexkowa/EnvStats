#' Print Output of Group Goodness-of-Fit Tests
#' @description
#' Formats and prints the results of performing a group goodness-of-fit test.
#'   This method is automatically called by \code{\link[base]{print}} when given an
#'   object of class \code{\link[=gofGroup.object]{"gofGroup"}}.  Currently,
#'   the only \pkg{EnvStats} function that performs a group goodness-of-fit test
#'   that produces an object of class \code{\link[=gofGroup.object]{"gofGroup"}}
#'   is \code{\link{gofGroupTest}}.
#' @usage
#' \method{print}{gofGroup}(x, ...)
#' @rawRd
#' \arguments{
#'   \item{x}{
#'   an object of class \code{"gofGroup"}.
#'   See \code{\link{gofGroup.object}} for details.
#' }
#'   \item{\dots}{
#'   arguments that can be supplied to the \code{\link[base]{format}} function.
#' }
#' }
#' @rawRd
#' \details{
#'   This is the \code{"gofGroup"} method for the generic function
#'   \code{\link[base]{print}}.
#'   See the help file for \code{\link{gofGroup.object}} for information
#'   on the information contained in this kind of object.
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
#'   \link[=FcnsByCatGOFTests]{Goodness-of-Fit Tests},
#'   \code{\link{gofGroup.object}},
#'   \code{\link[base]{print}}.
#' }
#' @rawRd
#' \keyword{print}

print.gofGroup <-
function (x, ...) 
{
    coll.string <- paste("\n", space(33), sep = "")
    cat("\nResults of Group Goodness-of-Fit Test\n")
    cat("-------------------------------------\n\n")
    cat("Test Method:", space(21), x$method, "\n\n", sep = "")
    cat("Hypothesized Distribution:", space(7), x$distribution, 
        "\n\n", sep = "")
    if (is.null(names(x$data.name))) {
        cat("Data:", space(28), x$data.name, "\n\n", sep = "")
    }
    else cat("Data:", space(28), paste(paste(format(names(x$data.name), 
        justify = "left"), format(x$data.name, ...), sep = " = "), 
        collapse = coll.string), "\n\n", sep = "")
    if (!is.null(x$grouping.variable)) 
        cat("Grouping Variable:", space(15), x$grouping.variable, 
            "\n\n", sep = "")
    if (!is.null(x$subset.expression)) 
        cat("Subset With:", space(21), x$subset.expression, "\n\n", 
            sep = "")
    if (!is.null(x$parent.of.data)) 
        cat("Data Source:", space(21), x$parent.of.data, "\n\n", 
            sep = "")
    if (!is.null(x$n.groups)) 
        cat("Number of Groups:", space(16), x$n.groups, "\n\n", 
            sep = "")
    if (!is.null(x$bad.obs) && any(x$bad.obs > 0)) {
        if (length(x$bad.obs) == 1) 
            cat("Number NA/NaN/Inf's Removed:", space(5), x$bad.obs, 
                "\n\n", sep = "")
        else {
            cat("Number NA/NaN/Inf's Removed:", space(5), paste(paste(format(names(x$bad.obs), 
                justify = "left"), format(x$bad.obs, nsmall = 0, 
                ...), sep = " = "), collapse = coll.string), 
                "\n\n", sep = "")
        }
    }
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
    string <- ifelse(length(x$statistic) == 1, paste("Test Statistic:", 
        space(18), sep = ""), paste("Test Statistics:", space(17), 
        sep = ""))
    cat(string, paste(paste(format(names(x$statistic), justify = "left"), 
        format(x$statistic, nsmall = 0, ...), sep = " = "), collapse = coll.string), 
        "\n\n", sep = "")
    if (!is.null(x$parameters)) {
        string <- ifelse(length(x$parameters) > 1, paste("Test Statistic Parameters:", 
            space(7), sep = ""), paste("Test Statistic Parameter:", 
            space(8), sep = ""))
        cat(string, paste(paste(format(names(x$parameters), justify = "left"), 
            format(x$parameters, nsmall = 0, ...), sep = " = "), 
            collapse = coll.string), "\n\n", sep = "")
    }
    index <- 1:x$n.groups
    cat("P-values for\nIndividual Tests:", space(16), paste(paste(format(names(x$p.value[index]), 
        justify = "left"), format(x$p.value[index], ...), sep = " = "), 
        collapse = coll.string), "\n\n", sep = "")
    index <- 1 + x$n.groups
    cat("P-value for\nGroup Test:", space(22), format(x$p.value[index], 
        ...), "\n\n", sep = "")
    cat("Alternative Hypothesis:", space(10), x$alternative, 
        "\n", sep = "")
    invisible(x)
}
