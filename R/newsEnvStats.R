#' Show the EnvStats NEWS File
#' @description
#' Show the NEWS file of the \pkg{EnvStats} package.
#' @usage
#' newsEnvStats()
#' @rawRd
#' \details{
#'   The function \code{newsEnvStats} displays the contents of the \pkg{EnvStats} NEWS file in a
#'   separate text window.  You can also access the NEWS file with the command
#'   \code{news(package="EnvStats")}, which returns the contents of the file to the \R command
#'   window.
#' }
#' @rawRd
#' \value{
#'   None.
#' }
#' @rawRd
#' \author{
#'   Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \seealso{
#'   \code{\link{news}}.
#' }
#' @rawRd
#' \keyword{ package }

newsEnvStats <-
function () 
{
    utils::news(package = "EnvStats")
}
