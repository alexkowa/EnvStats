#' @exportS3Method NULL
#' @noRd
summaryStats.logical <-
function (object, ...) 
{
    summaryStats.factor(factor(object), ...)
}
