#' Coverage for Nonparametric Tolerance Interval for Continuous Distribution
#' @description
#' Compute the coverage associated with a nonparametric tolerance interval for a continuous
#'   distribution given the sample size, confidence level, coverage type
#'   (\eqn{\beta}-content versus \eqn{\beta}-expectation), and ranks of the order statistics
#'   used for the interval.
#' @usage
#' tolIntNparCoverage(n, conf.level = 0.95, cov.type = "content",
#'     ltl.rank = ifelse(ti.type == "upper", 0, 1),
#'     n.plus.one.minus.utl.rank = ifelse(ti.type == "lower", 0, 1),
#'     ti.type = "two.sided")
#' @rawRd
#' \arguments{
#'   \item{n}{
#'   vector of positive integers specifying the sample sizes.
#'   Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf})
#'   values are not allowed.
#' }
#'   \item{conf.level}{
#'   numeric vector of values between 0 and 1 indicating the confidence level of the
#'   tolerance interval.
#' }
#'   \item{cov.type}{
#'   character string specifying the coverage type for the tolerance interval.
#'   The possible values are \code{"content"} (\eqn{\beta}-content; the default), and
#'   \code{"expectation"} (\eqn{\beta}-expectation).
#' }
#'   \item{ltl.rank}{
#'   vector of positive integers indicating the rank of the order statistic to use for the lower bound
#'   of the tolerance interval.  If \code{ti.type="two-sided"} or \cr
#'   \code{ti.type="lower"},
#'   the default value is \code{ltl.rank=1} (implying the minimum value of \code{x} is used
#'   as the lower bound of the tolerance interval).  If \cr
#'   \code{ti.type="upper"}, this argument
#'   is set equal to \code{0}.
#' }
#'   \item{n.plus.one.minus.utl.rank}{
#'   vector of positive integers related to the rank of the order statistic to use for
#'   the upper bound of the tolerance interval.  A value of
#'   \code{n.plus.one.minus.utl.rank=1} (the default) means use the
#'   first largest value, and in general a value of \cr
#'   \code{n.plus.one.minus.utl.rank=}\eqn{i} means use the \eqn{i}'th largest value.
#'   If \cr
#'   \code{ti.type="lower"}, this argument is set equal to \code{0}.
#' }
#'   \item{ti.type}{
#'   character string indicating what kind of tolerance interval to compute.
#'   The possible values are \code{"two-sided"} (the default), \code{"lower"}, and
#'   \code{"upper"}.
#' }
#' }
#' @rawRd
#' \details{
#'   If the arguments \code{n}, \code{conf.level}, \code{ltl.rank}, and
#'   \code{n.plus.one.minus.utl.rank} are not all the same length, they are replicated to be the
#'   same length as the length of the longest argument.
#'
#'   The help file for \code{\link{tolIntNpar}} explains how nonparametric \eqn{\beta}-content
#'   tolerance intervals are constructed and how the coverage
#'   associated with the tolerance interval is computed based on specified values
#'   for the sample size, the confidence level, and the ranks of the order statistics used for
#'   the bounds of the tolerance interval.
#' }
#' @rawRd
#' \value{
#'   vector of values between 0 and 1 indicating the coverage associated with
#'   the specified nonparametric tolerance interval.
#' }
#' @rawRd
#' \references{
#'   See the help file for \code{\link{tolIntNpar}}.
#' }
#' @rawRd
#' \author{
#'   Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \note{
#'   See the help file for \code{\link{tolIntNpar}}.
#'
#'   In the course of designing a sampling program, an environmental scientist may wish to determine
#'   the relationship between sample size, coverage, and confidence level if one of the objectives of
#'   the sampling program is to produce tolerance intervals.  The functions
#'   \code{\link{tolIntNparN}}, \code{\link{tolIntNparConfLevel}}, \code{tolIntNparCoverage}, and
#'   \code{\link{plotTolIntNparDesign}} can be used to investigate these relationships for
#'   constructing nonparametric tolerance intervals.
#' }
#' @rawRd
#' \seealso{
#'   \code{\link{tolIntNpar}}, \code{\link{tolIntNparN}}, \code{\link{tolIntNparConfLevel}},
#'   \code{\link{plotTolIntNparDesign}}.
#' }
#' @rawRd
#' \examples{
#'   # Look at how the coverage of a nonparametric tolerance interval increases with
#'   # increasing sample size:
#'
#'   seq(10, 60, by=10)
#'   #[1] 10 20 30 40 50 60
#'
#'   round(tolIntNparCoverage(n = seq(10, 60, by = 10)), 2)
#'   #[1] 0.61 0.78 0.85 0.89 0.91 0.92
#'
#'   #---------
#'
#'   # Look at how the coverage of a nonparametric tolerance interval decreases with
#'   # increasing confidence level:
#'
#'   seq(0.5, 0.9, by=0.1)
#'   #[1] 0.5 0.6 0.7 0.8 0.9
#'
#'   round(tolIntNparCoverage(n = 10, conf.level = seq(0.5, 0.9, by = 0.1)), 2)
#'   #[1] 0.84 0.81 0.77 0.73 0.66
#'
#'   #----------
#'
#'   # Look at how the coverage of a nonparametric tolerance interval decreases with
#'   # the rank of the lower tolerance limit:
#'
#'   round(tolIntNparCoverage(n = 60, ltl.rank = 1:5), 2)
#'   #[1] 0.92 0.90 0.88 0.85 0.83
#'
#'   #==========
#'
#'   # Example 17-4 on page 17-21 of USEPA (2009) uses copper concentrations (ppb) from 3
#'   # background wells to set an upper limit for 2 compliance wells.  The maximum value from
#'   # the 3 wells is set to the 95% confidence upper tolerance limit, and we need to
#'   # determine the coverage of this tolerance interval.
#'
#'   tolIntNparCoverage(n = 24, conf.level = 0.95, ti.type = "upper")
#'   #[1] 0.8826538
#' }
#' @rawRd
#' \keyword{ distribution }
#' @rawRd
#' \keyword{ design }
#' @rawRd
#' \keyword{ htest }

tolIntNparCoverage <-
function (n, conf.level = 0.95, cov.type = "content", ltl.rank = ifelse(ti.type == 
    "upper", 0, 1), n.plus.one.minus.utl.rank = ifelse(ti.type == 
    "lower", 0, 1), ti.type = "two.sided") 
{
    if (!is.vector(n, mode = "numeric") || !all(is.finite(n)) || 
        any(n != trunc(n)) || any(n < 2)) 
        stop("'n' must be a vector of positive integers greater than 1.")
    cov.type <- match.arg(cov.type, c("content", "expectation"))
    if (cov.type == "content") {
        if (!is.vector(conf.level, mode = "numeric") || !all(is.finite(conf.level)) || 
            any(conf.level <= 0) || any(conf.level >= 1)) 
            stop("All values of 'conf.level' must be greater than 0 and less than 1.")
    }
    ti.type <- match.arg(ti.type, c("two.sided", "lower", "upper"))
    if (ti.type == "upper") 
        ltl.rank <- 0
    else if (ti.type == "lower") 
        n.plus.one.minus.utl.rank <- 0
    if (!is.vector(ltl.rank, mode = "numeric") || !all(is.finite(ltl.rank)) || 
        any(ltl.rank != trunc(ltl.rank)) || any(ltl.rank < 0 | 
        ltl.rank >= n)) 
        stop(paste("'ltl.rank' must be a vector of non-negative", 
            "integers less than the corresponding value of 'n'"))
    if (ti.type %in% c("two.sided", "lower") & any(ltl.rank < 
        1)) 
        stop("When ti.type='two.sided' or ti.type='lower', all values of 'ltl.rank' must be positive integers")
    if (!is.vector(n.plus.one.minus.utl.rank, mode = "numeric") || 
        !all(is.finite(n.plus.one.minus.utl.rank)) || any(n.plus.one.minus.utl.rank != 
        trunc(n.plus.one.minus.utl.rank)) || any(n.plus.one.minus.utl.rank < 
        0 | n.plus.one.minus.utl.rank >= n)) 
        stop(paste("'n.plus.one.minus.utl.rank' must be a vector of non-negative", 
            "integers less than the corresponding value of 'n'"))
    if (ti.type %in% c("two.sided", "upper") & any(n.plus.one.minus.utl.rank < 
        1)) 
        stop("When ti.type='two.sided' or ti.type='upper' all values of 'n.plus.one.minus.utl.rank' must be positive integers")
    arg.mat <- cbind.no.warn(n = as.vector(n), conf.level = as.vector(conf.level), 
        ltl.rank = as.vector(ltl.rank), n.plus.one.minus.utl.rank = as.vector(n.plus.one.minus.utl.rank))
    for (i in c("n", "conf.level", "ltl.rank", "n.plus.one.minus.utl.rank")) assign(i, 
        arg.mat[, i])
    if (ti.type == "two.sided") {
        utl.rank <- n + 1 - n.plus.one.minus.utl.rank
        if (any(ltl.rank >= utl.rank)) 
            stop(paste("Illegal values for 'ltl.rank' and 'n.plus.one.minus.utl.rank'.", 
                "Make one or both of them smaller"))
    }
    N <- length(n)
    coverage.vec <- numeric(N)
    for (i in 1:N) {
        coverage.vec[i] <- tolIntNparCoverageScalar(n = n[i], 
            conf.level = conf.level[i], cov.type = cov.type, 
            ltl.rank = ltl.rank[i], n.plus.one.minus.utl.rank = n.plus.one.minus.utl.rank[i], 
            ti.type = ti.type)
    }
    names(coverage.vec) <- NULL
    coverage.vec
}
