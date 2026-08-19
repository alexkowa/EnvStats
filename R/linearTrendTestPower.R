#' Power of a t-Test for Linear Trend
#' @description
#' Compute the power of a parametric test for linear trend, given the sample size or
#'   predictor variable values, scaled slope, and significance level.
#' @usage
#' linearTrendTestPower(n, x = lapply(n, seq), slope.over.sigma = 0, alpha = 0.05,
#'     alternative = "two.sided", approx = FALSE)
#' @rawRd
#' \arguments{
#'   \item{n}{
#'   numeric vector of sample sizes.  All values of \code{n} must be positive integers
#'   larger than 2.  This argument is ignored when \code{x} is supplied.
#'   Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf})
#'   values are not allowed.
#' }
#'   \item{x}{
#'   numeric vector of predictor variable values, or a list in which each component is
#'   a numeric vector of predictor variable values.  Usually, the predictor variable is
#'   time (e.g., days, months, quarters, etc.).  The default value is
#'   \code{x=lapply(n,seq)}, which yields a list in which the i'th component is the
#'   seqence of integers from 1 to the i'th value of the vector \code{n}.  If \code{x}
#'   is a numeric vector, it must contain at least three elements, two of which must be
#'   unique.  If \code{x} is a list of numeric vectors, each component of \code{x}
#'   must contain at least three elements, two of which must be unique.
#'   Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf})
#'   values are not allowed.
#' }
#'   \item{slope.over.sigma}{
#'   numeric vector specifying the ratio of the true slope to the standard deviation of
#'   the error terms (\eqn{\sigma}).  This is also called the "scaled slope".  The
#'   default value is \code{slope.over.sigma=0}.
#' }
#'   \item{alpha}{
#'   numeric vector of numbers between 0 and 1 indicating the Type I error level
#'   associated with the hypothesis test.  The default value is \code{alpha=0.05}.
#' }
#'   \item{alternative}{
#'   character string indicating the kind of alternative hypothesis.  The possible values
#'   are \code{"two.sided"} (the default), \code{"greater"}, and \code{"less"}.
#' }
#'   \item{approx}{
#'   logical scalar indicating whether to compute the power based on an approximation to
#'   the non-central t-distribution.  The default value is \code{FALSE}.
#' }
#' }
#' @details
#' This help page has been shortened to keep function help focused on usage,
#' arguments, return values, and examples. Extended method details, formulas,
#' and background material are available in \code{vignette("extended-function-details", package = "EnvStats")}, section \code{linearTrendTestPower}.
#' @rawRd
#' \value{
#'   a numeric vector powers.
#' }
#' @rawRd
#' \references{
#'   Berthouex, P.M., and L.C. Brown. (2002).
#'   \emph{Statistics for Environmental Engineers}.  Second Edition.
#'   Lewis Publishers, Boca Raton, FL.
#'
#'   Draper, N., and H. Smith. (1998).  \emph{Applied Regression Analysis}.
#'   Third Edition.  John Wiley and Sons, New York, Chapter 1.
#'
#'   Helsel, D.R., and R.M. Hirsch. (1992).
#'   \emph{Statistical Methods in Water Resources Research}.
#'   Elsevier, New York, NY, Chapter 9.
#'
#'   Johnson, N. L., S. Kotz, and N. Balakrishnan. (1995).  \emph{Continuous Univariate
#'   Distributions, Volume 2}.  Second Edition.  John Wiley and Sons, New York,
#'   Chapters 28, 31
#'
#'   Millard, S.P., and N.K. Neerchal. (2001). \emph{Environmental Statistics with S-PLUS}.
#'   CRC Press, Boca Raton, FL.
#'
#'   Zar, J.H. (2010). \emph{Biostatistical Analysis}. Fifth Edition.
#'   Prentice-Hall, Upper Saddle River, NJ.
#' }
#' @rawRd
#' \author{
#'   Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \note{
#'   Often in environmental data analysis, we are interested in determining whether
#'   there is a trend in some indicator variable over time.  In this case, the predictor
#'   variable \eqn{X} is time (e.g., day, month, quarter, year, etc.), and the \eqn{n}
#'   values of the response variable represent measurements taken over time.  The slope
#'   then represents the change in the average of the response variable per one unit of
#'   time.
#'
#'   You can use the parametric model (1) to model your data, then use the \R function
#'   \code{\link{lm}} to fit the regression coefficients and the \code{\link{summary.lm}}
#'   function to perform a test for the significance of the slope coefficient.  The
#'   function \code{linearTrendTestPower} computes the power of this t-test, given a
#'   fixed value of the sample size, scaled slope, and significance level.
#'
#'   You can also use \link[=kendallTrendTest]{Kendall's nonparametric test for trend}
#'   if you don't want to assume the error terms are normally distributed.  When the
#'   errors are truly normally distributed, the asymptotic relative efficiency of
#'   Kendall's test for trend versus the parametric t-test for a zero slope is 0.98,
#'   and Kendall's test can be more powerful than the parametric t-test when the errors
#'   are not normally distributed.  Thus the function \code{linearTrendTestPower} can
#'   also be used to estimate the power of Kendall's test for trend.
#'
#'   In the course of designing a sampling program, an environmental scientist may wish
#'   to determine the relationship between sample size, significance level, power, and
#'   scaled slope if one of the objectives of the sampling program is to determine
#'   whether a trend is occurring.  The functions \code{linearTrendTestPower},
#'   \code{\link{linearTrendTestN}}, \code{\link{linearTrendTestScaledMds}}, and \cr
#'   \code{\link{plotLinearTrendTestDesign}} can be used to investigate these
#'   relationships.
#' }
#' @rawRd
#' \seealso{
#'   \code{\link{linearTrendTestN}}, \code{\link{linearTrendTestScaledMds}},
#'   \code{\link{plotLinearTrendTestDesign}}, \code{\link{lm}}, \cr
#'   \code{\link{summary.lm}}, \code{\link{kendallTrendTest}},
#'   \link{Power and Sample Size}, \link{Normal}, \code{\link{t.test}}.
#' }
#' @rawRd
#' \examples{
#'   # Look at how the power of the t-test for zero slope increases with increasing
#'   # sample size:
#'
#'   seq(5, 30, by = 5)
#'   #[1] 5 10 15 20 25 30
#'
#'   power <- linearTrendTestPower(n = seq(5, 30, by = 5), slope.over.sigma = 0.1)
#'
#'   round(power, 2)
#'   #[1] 0.06 0.13 0.34 0.68 0.93 1.00
#'
#'   #----------
#'
#'   # Repeat the last example, but compute the approximate power instead of the
#'   # exact:
#'
#'   power <- linearTrendTestPower(n = seq(5, 30, by = 5), slope.over.sigma = 0.1,
#'     approx = TRUE)
#'
#'   round(power, 2)
#'   #[1] 0.05 0.11 0.32 0.68 0.93 0.99
#'
#'   #----------
#'
#'   # Look at how the power of the t-test for zero slope increases with increasing
#'   # scaled slope:
#'
#'   seq(0.05, 0.2, by = 0.05)
#'   #[1] 0.05 0.10 0.15 0.20
#'
#'   power <- linearTrendTestPower(15, slope.over.sigma = seq(0.05, 0.2, by = 0.05))
#'
#'   round(power, 2)
#'   #[1] 0.12 0.34 0.64 0.87
#'
#'   #----------
#'
#'   # Look at how the power of the t-test for zero slope increases with increasing
#'   # values of Type I error:
#'
#'   power <- linearTrendTestPower(20, slope.over.sigma = 0.1,
#'     alpha = c(0.001, 0.01, 0.05, 0.1))
#'
#'   round(power, 2)
#'   #[1] 0.14 0.41 0.68 0.80
#'
#'   #----------
#'
#'   # Show that for a simple regression model, you get a greater power of detecting
#'   # a non-zero slope if you take all the observations at two endpoints, rather than
#'   # spreading the observations evenly between two endpoints.
#'   # (Note: This design usually cannot work with environmental monitoring data taken
#'   # over time since usually observations taken close together in time are not
#'   # independent.)
#'
#'   linearTrendTestPower(x = 1:10, slope.over.sigma = 0.1)
#'   #[1] 0.1265976
#'
#'
#'   linearTrendTestPower(x = c(rep(1, 5), rep(10, 5)), slope.over.sigma = 0.1)
#'   #[1] 0.2413823
#'
#'   #==========
#'
#'   # Clean up
#'   #---------
#'   rm(power)
#' }
#' @rawRd
#' \keyword{ distribution }
#' @rawRd
#' \keyword{ design }
#' @rawRd
#' \keyword{ htest }

linearTrendTestPower <-
function (n, x = lapply(n, seq), slope.over.sigma = 0, alpha = 0.05, 
    alternative = "two.sided", approx = FALSE) 
{
    if (missing(n) && missing(x)) 
        stop("You must supply either 'n' or 'x'")
    if (!missing(x)) {
        if (is.vector(x) && !is.list(x)) 
            x <- list(x)
        if (!is.list(x) || !all(sapply(x, function(i) {
            is.vector(i, mode = "numeric")
        }))) 
            stop(paste("'x' must be either a numeric vector or", 
                "a list in which each component of 'x'", "is a numeric vector"))
        if (any(sapply(x, function(i) {
            !all(is.finite(i))
        }))) 
            stop(paste("Missing (NA), Infinite (Inf, -Inf), and", 
                "Undefined (Nan) values are not allowed in", 
                "any components of 'x'"))
        n <- sapply(x, length)
        n.unique <- sapply(x, function(y) length(unique(y)))
        if (any(n < 3) || any(n.unique < 2)) 
            stop(paste("All components of 'x' must contain at least 3 elements", 
                "and at least 2 distinct values."))
    }
    else {
        if (!is.vector(n, mode = "numeric")) 
            stop("'n' must be a numeric vector")
        if (any(is.na(n))) 
            stop(paste("Missing (NA) and Undefined (Nan) values", 
                "are not allowed in 'n'"))
        if (any(n < 3)) 
            stop("All values of 'n' must be greater than or equal to 3.")
    }
    if (!is.vector(slope.over.sigma, mode = "numeric") || !is.vector(alpha, 
        mode = "numeric")) 
        stop("'slope.over.sigma', and 'alpha' must be numeric vectors.")
    if (any(is.na(slope.over.sigma))) 
        stop(paste("Missing (NA) and Undefined (Nan) values", 
            "are not allowed in 'slope.over.sigma'"))
    if (!all(is.finite(alpha))) 
        stop(paste("Missing (NA), Infinite (Inf, -Inf), and", 
            "Undefined (Nan) values are not allowed in", "'alpha'"))
    if (any(alpha <= 0) || any(alpha >= 1)) 
        stop("All values of 'alpha' must be between 0 and 1.")
    alternative <- match.arg(alternative, c("two.sided", "less", 
        "greater"))
    df <- n - 2
    ncp <- sqrt(sapply(x, function(x) (length(x) - 1) * var(x))) * 
        slope.over.sigma
    if (approx) 
        power <- switch(alternative, less = pt(qt(alpha, df) - 
            ncp, df), greater = 1 - pt(qt(1 - alpha, df) - ncp, 
            df), two.sided = pt(qt(alpha/2, df) - ncp, df) + 
            1 - pt(qt(1 - alpha/2, df) - ncp, df))
    else power <- switch(alternative, less = pT(qt(alpha, df), 
        df = df, ncp = ncp), greater = 1 - pT(qt(1 - alpha, df), 
        df = df, ncp = ncp), two.sided = 1 - pf(qf(1 - alpha, 
        1, df), df1 = 1, df2 = df, ncp = ncp^2))
    power
}
