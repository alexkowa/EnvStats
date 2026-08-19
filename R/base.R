#' Base \eqn{b} Representation of a Number
#' @description
#' For any number represented in base 10, compute the representation in any user-specified base.
#' @usage
#' base(n, base = 10, num.digits = max(0, floor(log(n, base))) + 1)
#' @rawRd
#' \arguments{
#'   \item{n}{
#'   a non-negative integer (base 10).
#' }
#'   \item{base}{
#'   a positive integer greater than 1 indicating what base to represent \code{n} in.
#' }
#'   \item{num.digits}{
#'   a positive integer indicating how many digits to use to represent \code{n} in base \code{base}.
#'   By default, \code{num.digits} is equal to just the number of required digits
#'   (i.e., \code{max(0, floor(log(n, base))) + 1)}.  Setting \code{num.digits} to a larger number
#'   than this will result in 0's padding the left.
#' }
#' }
#' @rawRd
#' \details{
#'   If \eqn{b} is a positive integer greater than 1, and \eqn{n} is a positive integer,
#'   then \eqn{n} can be expressed uniquely in the form
#'
#'   \deqn{n = a_kb^k + a_{k-1}b^{k-1} + \ldots + a_1b + a0}
#'
#'   where \eqn{k} is a non-negative integer, the coefficients \eqn{a_0, a_1, \ldots, a_k}
#'   are non-negative integers less than \eqn{b}, and \eqn{a_k > 0}
#'   (Rosen, 1988, p.105).  The function \code{base} computes the coefficients
#'   \eqn{a_0, a_1, \ldots, a_k}.
#' }
#' @rawRd
#' \value{
#'   A numeric vector of length \code{num.digits} showing the representation of \code{n} in base \code{base}.
#' }
#' @rawRd
#' \references{
#'   Rosen, K.H. (1988). \emph{Discrete Mathematics and Its Applications}.  Random House, New York,
#'   pp.105-107.
#' }
#' @rawRd
#' \author{
#'     Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \note{
#'   The function \code{base} is included in \pkg{EnvStats} because it
#'   is called by the function \cr
#'   \code{\link{oneSamplePermutationTest}}.
#' }
#' @rawRd
#' \seealso{
#'   \code{\link{oneSamplePermutationTest}}.
#' }
#' @rawRd
#' \examples{
#'   # Compute the value of 7 in base 2.
#'
#'   base(7, 2)
#'   #[1] 1 1 1
#'
#'   base(7, 2, num.digits=5)
#'   #[1] 0 0 1 1 1
#' }
#' @rawRd
#' \keyword{ math }

base <-
function (n, base = 10, num.digits = max(0, floor(log(n, base))) + 
    1) 
{
    if (!is.vector(n, mode = "numeric") || is.factor(n) || length(n) != 
        1 || n != trunc(n) || n < 0) 
        stop("'n' must be a non-negative integer")
    if (!is.vector(base, mode = "numeric") || is.factor(base) || 
        length(base) != 1 || base != trunc(base) || base <= 1) 
        stop("'base' must be positive integer greater than 1")
    if (n == 0) {
        if (!is.vector(num.digits, mode = "numeric") || is.factor(num.digits) || 
            length(num.digits) != 1 || num.digits != trunc(num.digits) || 
            num.digits < 1) 
            stop("'num.digits' must be a positive integer")
        vec <- numeric(num.digits)
    }
    else {
        min.num.digits <- floor(log(n, base)) + 1
        if (!is.vector(num.digits, mode = "numeric") || is.factor(num.digits) || 
            length(num.digits) != 1 || num.digits != trunc(num.digits) || 
            num.digits < min.num.digits) 
            stop(paste("'num.digits' must be a positive integer", 
                "greater than or equal to", min.num.digits))
        vec <- numeric(num.digits)
        vec[(num.digits - min.num.digits) + (1:min.num.digits)] <- (n%/%(base^((min.num.digits - 
            1):0)))%%base
    }
    vec
}
