#' Print Output of Goodness-of-Fit Tests
#' @description
#' Formats and prints the results of calling the function \code{\link{distChooseCensored}}, which
#'   uses a series of goodness-of-fit tests to choose among candidate distributions based on
#'   censored data.
#'   This method is automatically called by \code{\link[base]{print}} when given an
#'   object of class \code{"distChooseCensored"}.
#' @usage
#' \method{print}{distChooseCensored}(x, show.cen.levels = TRUE,
#'   pct.censored.digits = .Options$digits, ...)
#' @rawRd
#' \arguments{
#'   \item{x}{
#'   an object of class \code{"distChoose"}.  See \code{\link{distChoose.object}} for details.
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
#'   This is the \code{"distChooseCensored"} method for the generic function
#'   \code{\link[base]{print}}.
#'   Prints the candidate distributions,
#'   method used to choose among the candidate distributions,
#'   chosen distribution, Type I error associated with each goodness-of-fit test,
#'   estimated population parameter(s) associated with the chosen distribution,
#'   estimation method, goodness-of-fit test results for each candidate distribution,
#'   and the data name and censoring variable.
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
#'   \code{\link{distChooseCensored}}, \code{\link{distChooseCensored.object}},
#'   \link{Censored Data}, \code{\link[base]{print}}.
#' }
#' @rawRd
#' \keyword{print}

print.distChooseCensored <-
function (x, show.cen.levels = TRUE, pct.censored.digits = .Options$digits, 
    ...) 
{
    coll.string <- paste("\n", space(33), sep = "")
    choices <- x$choices
    n.choices <- length(choices)
    cat("\nResults of Choosing Distribution\n")
    cat("--------------------------------\n\n")
    cat("Candidate Distributions:", space(9), paste(choices, 
        collapse = coll.string), "\n\n", sep = "")
    cat("Choice Method:", space(19), x$method, "\n\n", sep = "")
    cat("Type I Error per Test:", space(11), x$alpha, "\n\n", 
        sep = "")
    cat("Decision:", space(24), x$decision, "\n\n", sep = "")
    if (x$decision != "Nonparametric") {
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
    cat("Sample Size:", space(21), x$sample.size, "\n\n", sep = "")
    cat("Censoring Side:", space(18), x$censoring.side, "\n\n", 
        sep = "")
    cat("Censoring Variable:", space(14), x$censoring.name, "\n\n", 
        sep = "")
    if (show.cen.levels) {
        cat("Censoring Level(s):", space(12), format(x$censoring.levels, 
            nsmall = 0, justify = "left", ...), "\n\n")
    }
    cat("Percent Censored:", space(16), round(x$percent.censored, 
        pct.censored.digits), "%", "\n\n", sep = "")
    if (!is.null(x$proucl.sample.size)) 
        cat("ProUCL Sample Size:", space(14), x$proucl.sample.size, 
            "\n\n", sep = "")
    cat("Test Results:\n\n")
    if (x$method != "ProUCL") {
        for (i in 1:n.choices) {
            choice.i <- choices[i]
            cat("  ", choice.i, space(33 - nchar(choice.i) - 
                2), "\n", sep = "")
            cat("    Test Statistic:", space(14), paste(paste(format(names(x$test.results[[i]]$statistic), 
                justify = "left"), format(x$test.results[[i]]$statistic, 
                nsmall = 0, ...), sep = " = "), collapse = coll.string), 
                "\n", sep = "")
            cat("    P-value:", space(21), format(x$test.results[[i]]$p.value, 
                ...), "\n\n", sep = "")
        }
    }
    else {
        for (i in 1:n.choices) {
            choice.i <- choices[i]
            ncc.i <- nchar(choice.i)
            cat("  ", choice.i, space(33 - ncc.i - 2), "\n", 
                sep = "")
            tests.i <- names(x$test.results[[i]])[1:2]
            cat("    ", tests.i[1], space(33 - ncc.i - 4), "\n", 
                sep = "")
            cat("      Test Statistic:", space(12), paste(paste(format(names(x$test.results[[i]][[1]]$statistic), 
                justify = "left"), format(x$test.results[[i]][[1]]$statistic, 
                nsmall = 0, ...), sep = " = "), collapse = coll.string), 
                "\n", sep = "")
            cat("      P-value:", space(19), format(x$test.results[[i]][[1]]$p.value, 
                ...), "\n", sep = "")
            cat("    ", tests.i[2], space(33 - ncc.i - 4), "\n", 
                sep = "")
            cat("      Test Statistic:", space(12), paste(paste(format(names(x$test.results[[i]][[2]]$statistic), 
                justify = "left"), format(x$test.results[[i]][[2]]$statistic, 
                nsmall = 0, ...), sep = " = "), collapse = coll.string), 
                "\n", sep = "")
            cat("      P-value:", space(19), format(x$test.results[[i]][[2]]$p.value, 
                ...), "\n\n", sep = "")
        }
    }
    invisible(x)
}
