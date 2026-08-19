#' Power of a One- or Two-Sample t-Test
#' @rawRd \alias{t-test power}
#' @rawRd \alias{T-test power}
#' @rawRd \alias{t-test Power}
#' @rawRd \alias{Power t-test}
#' @rawRd \alias{power t-test}
#' @description
#' Compute the power of a one- or two-sample t-test, given the sample size, scaled
#'   difference, and significance level.
#' @usage
#' tTestPower(n.or.n1, n2 = n.or.n1, delta.over.sigma = 0, alpha = 0.05,
#'     sample.type = ifelse(!missing(n2), "two.sample", "one.sample"),
#'     alternative = "two.sided", approx = FALSE)
#' @rawRd
#' \arguments{
#'   \item{n.or.n1}{
#'   numeric vector of sample sizes.  When \code{sample.type="one.sample"},
#'   \code{n.or.n1} denotes \eqn{n}, the number of observations in the single sample.  When \cr
#'   \code{sample.type="two.sample"}, \code{n.or.n1} denotes \eqn{n_1}, the number
#'   of observations from group 1.
#'   Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf})
#'   values are \bold{\emph{not}} allowed.
#' }
#'   \item{n2}{
#'   numeric vector of sample sizes for group 2.  The default value is the value of
#'   \code{n.or.n1}. This argument is ignored when \code{sample.type="one.sample"}.
#'   Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf})
#'   values are \bold{\emph{not}} allowed.
#' }
#'   \item{delta.over.sigma}{
#'   numeric vector specifying the ratio of the true difference \eqn{\delta}
#'   (\eqn{\delta = \mu - \mu_0} for the one-sample case and
#'   \eqn{\delta = \mu_1 - \mu_2} for the two-sample case)
#'   to the population standard deviation (\eqn{\sigma}).  This is also called the
#'   \dQuote{scaled difference}.
#'
#'   The default value is \code{delta.over.sigma=0}.
#' }
#'   \item{alpha}{
#'   numeric vector of numbers between 0 and 1 indicating the Type I error level
#'   associated with the hypothesis test.  The default value is \code{alpha=0.05}.
#' }
#'   \item{sample.type}{
#'   character string indicating whether to compute power based on a one-sample or
#'   two-sample hypothesis test.  When \code{sample.type="one.sample"}, the computed
#'   power is based on a hypothesis test for a single mean.  When \cr
#'   \code{sample.type="two.sample"}, the computed power is based on a hypothesis test
#'   for the difference between two means.  The default value is
#'   \code{sample.type="one.sample"} unless the argument \code{n2} is supplied.
#' }
#'   \item{alternative}{
#'   character string indicating the kind of alternative hypothesis.  The possible values
#'   are:
#'
#'   \itemize{
#'     \item \code{"two.sided"} (the default).  \eqn{H_a: \mu \ne \mu_0} for the one-sample case and
#'     \eqn{H_a: \mu_1 \ne \mu_2} for the two-sample case.
#'
#'     \item \code{"greater"}.  \eqn{H_a: \mu > \mu_0} for the one-sample case and
#'     \eqn{H_a: \mu_1 > \mu_2} for the two-sample case.
#'
#'     \item \code{"less"}.  \eqn{H_a: \mu < \mu_0} for the one-sample case and
#'     \eqn{H_a: \mu_1 < \mu_2} for the two-sample case.
#'   }
#' }
#'   \item{approx}{
#'   logical scalar indicating whether to compute the power based on an approximation to
#'   the non-central t-distribution.  The default value is \code{FALSE}.
#' }
#' }
#' @details
#' This help page has been shortened to keep function help focused on usage,
#' arguments, return values, and examples. Extended method details, formulas,
#' and background material are available in \code{vignette("extended-function-details", package = "EnvStats")}, section \code{tTestPower}.
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
#'   Helsel, D.R., and R.M. Hirsch. (1992).
#'   \emph{Statistical Methods in Water Resources Research}.
#'   Elsevier, New York, NY, Chapter 7.
#'
#'   Johnson, N. L., S. Kotz, and N. Balakrishnan. (1995).  \emph{Continuous Univariate
#'   Distributions, Volume 2}.  Second Edition.  John Wiley and Sons, New York,
#'   Chapters 28, 31
#'
#'   Millard, S.P., and N.K. Neerchal. (2001). \emph{Environmental Statistics with S-PLUS}.
#'   CRC Press, Boca Raton, FL.
#'
#'   USEPA. (1989b). \emph{Statistical Analysis of Ground-Water Monitoring Data at RCRA Facilities, Interim Final Guidance}.
#'   EPA/530-SW-89-026. Office of Solid Waste, U.S. Environmental Protection Agency, Washington, D.C.
#'
#'   USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
#'   EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.
#'   U.S. Environmental Protection Agency, Washington, D.C.
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
#'   The \link[stats:Normal]{normal distribution} and
#'   \link[stats:Lognormal]{lognormal distribution} are probably the two most
#'   frequently used distributions to model environmental data.  Often, you need to
#'   determine whether a population mean is significantly different from a specified
#'   standard (e.g., an MCL or ACL, USEPA, 1989b, Section 6), or whether two different
#'   means are significantly different from each other (e.g., USEPA 2009, Chapter 16).
#'   In this case, assuming normally distributed data, you can perform the
#'   Student's t-test.
#'
#'   In the course of designing a sampling program, an environmental scientist may wish
#'   to determine the relationship between sample size, significance level, power, and
#'   scaled difference if one of the objectives of the sampling program is to determine
#'   whether a mean differs from a specified level or two means differ from each other.
#'   The functions \code{tTestPower}, \code{\link{tTestN}},
#'   \code{\link{tTestScaledMdd}}, and \code{\link{plotTTestDesign}} can be used to
#'   investigate these relationships for the case of normally-distributed observations.
#' }
#' @rawRd
#' \seealso{
#'   \code{\link{tTestN}}, \code{\link{tTestScaledMdd}}, \code{\link{tTestAlpha}},
#'   \code{\link{plotTTestDesign}}, \link[stats]{Normal},
#'   \code{\link{t.test}}, \link{Hypothesis Tests}.
#' }
#' @rawRd
#' \examples{
#'   # Look at how the power of the one-sample t-test increases with
#'   # increasing sample size:
#'
#'   seq(5, 30, by = 5)
#'   #[1] 5 10 15 20 25 30
#'
#'   power <- tTestPower(n.or.n1 = seq(5, 30, by = 5), delta.over.sigma = 0.5)
#'
#'   round(power, 2)
#'   #[1] 0.14 0.29 0.44 0.56 0.67 0.75
#'
#'   #----------
#'
#'   # Repeat the last example, but use the approximation.
#'   # Note how the approximation underestimates the power
#'   # for the smaller sample sizes.
#'   #----------------------------------------------------
#'
#'   power <- tTestPower(n.or.n1 = seq(5, 30, by = 5), delta.over.sigma = 0.5,
#'     approx = TRUE)
#'
#'   round(power, 2)
#'   #[1] 0.10 0.26 0.42 0.56 0.67 0.75
#'
#'   #----------
#'
#'   # Look at how the power of the two-sample t-test increases with increasing
#'   # scaled difference:
#'
#'   seq(0.5, 2, by = 0.5)
#'   #[1] 0.5 1.0 1.5 2.0
#'
#'   power <- tTestPower(10, sample.type = "two.sample",
#'     delta.over.sigma = seq(0.5, 2, by = 0.5))
#'
#'   round(power, 2)
#'   #[1] 0.19 0.56 0.89 0.99
#'
#'   #----------
#'
#'   # Look at how the power of the two-sample t-test increases with increasing values
#'   # of Type I error:
#'
#'   power <- tTestPower(20, sample.type = "two.sample", delta.over.sigma = 0.5,
#'     alpha = c(0.001, 0.01, 0.05, 0.1))
#'
#'   round(power, 2)
#'   #[1] 0.03 0.14 0.34 0.46
#'
#'   #==========
#'
#'   # Modifying the example on pages 21-4 to 21-5 of USEPA (2009), determine how
#'   # adding another four months of observations to increase the sample size from
#'   # 4 to 8 for any one particular compliance well will affect the power of a
#'   # one-sample t-test that compares the mean for the well with the MCL of
#'   # 7 ppb.  Use alpha = 0.01, assume an upper one-sided alternative
#'   # (i.e., compliance well mean larger than 7 ppb), and assume a scaled
#'   # difference of 2.  (The data are stored in EPA.09.Ex.21.1.aldicarb.df.)
#'   # Note that the power changes from 49% to 98% by increasing the sample size
#'   # from 4 to 8.
#'
#'   tTestPower(n.or.n1 = c(4, 8), delta.over.sigma = 2, alpha = 0.01,
#'     sample.type = "one.sample", alternative = "greater")
#'   #[1] 0.4865800 0.9835401
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

tTestPower <-
function (n.or.n1, n2 = n.or.n1, delta.over.sigma = 0, alpha = 0.05, 
    sample.type = ifelse(!missing(n2), "two.sample", "one.sample"), 
    alternative = "two.sided", approx = FALSE) 
{
    sample.type <- match.arg(sample.type, c("one.sample", "two.sample"))
    alternative <- match.arg(alternative, c("two.sided", "less", 
        "greater"))
    if (!is.vector(n.or.n1, mode = "numeric") || !is.vector(delta.over.sigma, 
        mode = "numeric") || !is.vector(alpha, mode = "numeric")) 
        stop("'n.or.n1', 'delta.over.sigma', and 'alpha' must be numeric vectors.")
    if (any(is.na(n.or.n1)) || any(is.na(delta.over.sigma))) 
        stop(paste("Missing (NA) and Undefined (Nan) values", 
            "are not allowed in 'n.or.n1' or 'delta.over.sigma'"))
    if (!all(is.finite(alpha))) 
        stop(paste("Missing (NA), Infinite (Inf, -Inf), and", 
            "Undefined (Nan) values are not allowed in", "'alpha'"))
    if (sample.type == "one.sample" && any(n.or.n1 < 2)) 
        stop("All values of 'n.or.n1' must be greater than or equal to 2.")
    if (any(alpha <= 0) || any(alpha >= 1)) 
        stop("All values of 'alpha' must be greater than 0 and less than 1.")
    if (sample.type == "two.sample") {
        if (!missing(n2)) {
            if (!is.vector(n2, mode = "numeric")) 
                stop("'n2' must be a numeric vector")
            if (any(is.na(n2))) 
                stop(paste("Missing (NA) and Undefined (Nan) values", 
                  "are not allowed in 'n2'"))
        }
        if (any(n.or.n1 < 1) || any(n2 < 1) || any((n.or.n1 + 
            n2) < 3)) 
            stop(paste("When sample.type='two.sample',", "all values of 'n.or.n1' and 'n2' must be", 
                "greater than or equal to 1 and they must sum", 
                "to at least 3"))
    }
    if (sample.type == "one.sample") {
        df <- n.or.n1 - 1
        ncp <- sqrt(n.or.n1) * delta.over.sigma
    }
    else {
        df <- n.or.n1 + n2 - 2
        ncp <- sqrt(1/(1/n.or.n1 + 1/n2)) * delta.over.sigma
    }
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
