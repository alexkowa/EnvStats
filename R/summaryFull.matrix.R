#' @exportS3Method NULL
#' @noRd
summaryFull.matrix <-
function (object, ...) 
{
    summaryFull.data.frame(as.data.frame.matrix(object), ...)
}
