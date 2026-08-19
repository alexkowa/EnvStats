#' @exportS3Method NULL
#' @noRd
summaryStats.character <-
function (object, ...) 
{
    summaryStats.factor(factor(object), ...)
}
