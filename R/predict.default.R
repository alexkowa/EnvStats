#' @exportS3Method NULL
#' @noRd
predict.default <-
function (object, ...) 
stats::predict(object, ...)
