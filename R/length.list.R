#' @exportS3Method NULL
#' @noRd
length.list <-
function (...) 
{
    sapply(list(...), length)
}
