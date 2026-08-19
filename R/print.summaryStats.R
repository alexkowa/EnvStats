#' Print Summary Statistics
#' @description
#' Formats and prints the results of calling \code{\link{summaryStats}} or
#'   \code{\link{summaryFull}}.  This method is automatically called by
#'   \code{\link[base]{print}} when given an object of class \code{"summaryStats"}.
#' @usage
#' \method{print}{summaryStats}(x, ...)
#' @rawRd
#' \arguments{
#'   \item{x}{
#'   an object of class \code{"summaryStats"}.  See \code{\link{summaryStats.object}} for
#'   details.
#' }
#'   \item{\dots}{
#'   arguments that can be supplied to the \code{\link[base]{format}} function.
#' }
#' }
#' @rawRd
#' \details{
#'   This is the \code{"summaryStats"} method for the generic function
#'   \code{\link[base]{print}}.  Prints summary statistics.
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
#'   \code{\link{summaryStats}}, \code{\link{summaryFull}},
#'   \code{\link{summaryStats.object}}, \code{\link[base]{print}}.
#' }
#' @rawRd
#' \keyword{print}

print.summaryStats <-
function (x, ...) 
{
    new.x <- unclass(x)
    stats.in.rows <- attr(x, "stats.in.rows")
    drop0trailing <- attr(x, "drop0trailing")
    rn <- rownames(x)
    if (stats.in.rows) {
        p.names <- c("p.value", "ChiSq_p", "Fisher_p", "Exact_p")
        index <- unlist(sapply(p.names, grep, rn))
        if (any(index)) {
            p <- new.x[index, "Combined"]
            p.char <- format(new.x)[index, "Combined"]
            if (length(grep("e", p.char))) {
                new.x[index, "Combined"] <- 0
                new.x <- format(new.x, drop0trailing = drop0trailing)
                new.x[index, "Combined"] <- p
            }
            else {
                new.x <- format(new.x, drop0trailing = drop0trailing)
            }
        }
        else {
            new.x <- format(new.x, drop0trailing = drop0trailing)
        }
        new.x[grep("NA", new.x)] <- ""
    }
    else {
        new.x <- data.frame(new.x, check.names = FALSE)
        new.x[is.na(new.x)] <- ""
    }
    print(new.x, quote = FALSE, ...)
    invisible(x)
}
