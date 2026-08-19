#' Probability That at Least One Set of Future Observations Violates the Given Rule Based on a Nonparametric Simultaneous Prediction Interval
#' @description
#' Compute the probability that at least one set of future observations violates the
#'   given rule based on a nonparametric simultaneous prediction interval for the next
#'   \eqn{r} future sampling occasions.  The three possible rules are:
#'   \eqn{k}-of-\eqn{m}, California, or Modified California.  The probability is based
#'   on assuming the true distribution of the observations is \link[stats:Normal]{normal}.
#' @usage
#' predIntNparSimultaneousTestPower(n, n.median = 1, k = 1, m = 2, r = 1,
#'     rule = "k.of.m", lpl.rank = ifelse(pi.type == "upper", 0, 1),
#'     n.plus.one.minus.upl.rank = ifelse(pi.type == "lower", 0, 1),
#'     delta.over.sigma = 0, pi.type = "upper", r.shifted = r,
#'     method = "approx", NMC = 100, ci = FALSE, ci.conf.level = 0.95,
#'     integrate.args.list = NULL, evNormOrdStats.method = "royston")
#' @rawRd
#' \arguments{
#'   \item{n}{
#'   vector of positive integers specifying the sample sizes.
#'   Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf})
#'   values are not allowed.
#' }
#'   \item{n.median}{
#'   vector of positive odd integers specifying the sample size associated with the
#'   future medians.  The default value is \code{n.median=1} (i.e., individual
#'   observations).  Note that all future medians must be based on the same
#'   sample size.
#' }
#'   \item{k}{
#'   for the \eqn{k}-of-\eqn{m} rule (\code{rule="k.of.m"}), a vector of positive integers
#'   specifying the minimum number of observations (or medians) out of \eqn{m}
#'   observations (or medians) (all obtained on one future sampling \dQuote{occassion})
#'   the prediction interval should contain.
#'   The default value is \code{k=1}.  This argument is ignored when the argument
#'   \code{rule} is not equal to \code{"k.of.m"}.
#' }
#'   \item{m}{
#'   vector of positive integers specifying the maximum number of future observations (or
#'   medians) on one future sampling \dQuote{occasion}.
#'   The default value is \code{m=2}, except when \code{rule="Modified.CA"}, in which
#'   case this argument is ignored and \code{m} is automatically set equal to \code{4}.
#' }
#'   \item{r}{
#'   vector of positive integers specifying the number of future sampling
#'   \dQuote{occasions}.  The default value is \code{r=1}.
#' }
#'   \item{rule}{
#'   character string specifying which rule to use.  The possible values are
#'   \code{"k.of.m"} (\eqn{k}-of-\eqn{m} rule; the default), \code{"CA"} (California rule),
#'   and \code{"Modified.CA"} (modified California rule).
#' }
#'   \item{lpl.rank}{
#'   vector of non-negative integers indicating the rank of the order statistic to use for
#'   the lower bound of the prediction interval.  When \code{pi.type="lower"}, the
#'   default value is \code{lpl.rank=1} (implying the minimum value of \code{x} is used
#'   as the lower bound of the prediction interval).  When \code{pi.type="upper"},
#'   the argument \code{lpl.rank} is set equal to \code{0}.
#' }
#'   \item{n.plus.one.minus.upl.rank}{
#'   vector of non-negative integers related to the rank of the order statistic to use for
#'   the upper bound of the prediction interval.  A value of \code{n.plus.one.minus.upl.rank=1}
#'   (the default) means use the first largest value, and in general a value of \cr
#'   \code{n.plus.one.minus.upl.rank=}\eqn{i} means use the \eqn{i}'th largest value.
#'   When \code{pi.type="lower"}, the argument \code{n.plus.one.minus.upl.rank} is set
#'   equal to \code{0}.
#' }
#'   \item{delta.over.sigma}{
#'   numeric vector indicating the ratio \eqn{\Delta/\sigma}.  The quantity
#'   \eqn{\Delta} (delta) denotes the difference between the mean of the population
#'   that was sampled to construct the prediction interval, and the mean of the
#'   population that will be sampled to produce the future observations.  The quantity
#'   \eqn{\sigma} (sigma) denotes the population standard deviation for both populations.
#'   The default value is \cr
#'   \code{delta.over.sigma=0}.
#' }
#'   \item{pi.type}{
#'   character string indicating what kind of prediction interval to compute.
#'   The possible values are \code{"two.sided"} (the default), \code{"lower"}, and
#'   \code{"upper"}.
#' }
#'   \item{r.shifted}{
#'   vector of positive integers specifying the number of future sampling occasions for
#'   which the scaled mean is shifted by \eqn{\Delta/\sigma}.  All values must be
#'   integeters between \code{1} and the corresponding element of \code{r}.
#'   The default value is \code{r.shifted=r}.
#' }
#'   \item{method}{
#'   character string indicating what method to use to compute the power.  The possible
#'   values are \code{"approx"} (approximation based on \cr
#'   \code{\link{predIntNormSimultaneousTestPower}}; the default) and
#'   \code{"simulate"} (Monte Carlo simulation).
#' }
#'   \item{NMC}{
#'   positive integer indicating the number of Monte Carlo trials to run when \cr
#'   \code{method="simulate"}.  The default value is \code{NMC=100}.
#' }
#'   \item{ci}{
#'   logical scalar indicating whether to compute a confidence interval for the power
#'   when \code{method="simulate"}.  The default value is \code{ci=FALSE}.
#' }
#'   \item{ci.conf.level}{
#'   numeric scalar between 0 and 1 indicating the confidence level associated with the
#'   confidence interval for the power.  The argument is ignored if \code{ci=FALSE}
#'   or \code{method="approx"}.
#' }
#'   \item{integrate.args.list}{
#'   list of arguments to supply to the \code{\link{integrate}} function.  The default
#'   value is \code{NULL}.
#' }
#'   \item{evNormOrdStats.method}{
#'   character string indicating which method to use in the call to
#'   \code{\link{evNormOrdStatsScalar}} when \code{method="approx"}.  The default
#'   value is \code{evNormOrdStats.method="royston"}.  See the DETAILS section for
#'   more information.
#' }
#' }
#' @details
#' This help page has been shortened to keep function help focused on usage,
#' arguments, return values, and examples. Extended method details, formulas,
#' and background material are available in \code{vignette("extended-function-details", package = "EnvStats")}, section \code{predIntNparSimultaneousTestPower}.
#' @rawRd
#' \value{
#'   vector of values between 0 and 1 equal to the probability that
#'   the rule will be violated.
#' }
#' @rawRd
#' \references{
#'   See the help file for \code{\link{predIntNparSimultaneous}}.
#'
#'   Gansecki, M. (2009).  \emph{Using the Optimal Rank Values Calculator}.
#'   US Environmental Protection Agency, Region 8, March 10, 2009.
#' }
#' @rawRd
#' \author{
#'     Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \note{
#'   See the help file for \code{\link{predIntNparSimultaneous}}.
#'
#'   In the course of designing a sampling program, an environmental scientist may wish
#'   to determine the relationship between sample size, significance level, power, and
#'   scaled difference if one of the objectives of the sampling program is to determine
#'   whether two distributions differ from each other.  The functions
#'   \code{predIntNparSimultaneousTestPower} and \cr
#'   \code{\link{plotPredIntNparSimultaneousTestPowerCurve}} can be
#'   used to investigate these relationships for the case of normally-distributed
#'   observations.
#' }
#' @rawRd
#' \seealso{
#'   \code{\link{plotPredIntNparSimultaneousTestPowerCurve}},
#'   \code{\link{predIntNparSimultaneous}}, \cr
#'   \code{\link{predIntNparSimultaneousN}},
#'   \code{\link{predIntNparSimultaneousConfLevel}}, \cr
#'   \code{\link{plotPredIntNparSimultaneousDesign}},
#'   \code{\link{predIntNpar}}, \code{\link{tolIntNpar}}.
#' }
#' @rawRd
#' \examples{
#'   # Example 19-5 of USEPA (2009, p. 19-33) shows how to compute nonparametric upper
#'   # simultaneous prediction limits for various rules based on trace mercury data (ppb)
#'   # collected in the past year from a site with four background wells and 10 compliance
#'   # wells (data for two of the compliance wells  are shown in the guidance document).
#'   # The facility must monitor the 10 compliance wells for five constituents
#'   # (including mercury) annually.
#'
#'   # Here we will compute the confidence levels and powers associated with
#'   # two different sampling plans:
#'   # 1) the 1-of-2 retesting plan for a median of order 3 using the
#'   #    background maximum and
#'   # 2) the 1-of-4 plan on individual observations using the 3rd highest
#'   #    background value.
#'   # Power will be computed assuming a normal distribution and setting
#'   # delta.over.sigma equal to 2, 3, and 4.
#'   # The data for this example are stored in EPA.09.Ex.19.5.mercury.df.
#'
#'   # We will pool data from 4 background wells that were sampled on
#'   # a number of different occasions, giving us a sample size of
#'   # n = 20 to use to construct the prediction limit.
#'
#'   # There are 10 compliance wells and we will monitor 5 different
#'   # constituents at each well annually.  For this example, USEPA (2009)
#'   # recommends setting r to the product of the number of compliance wells and
#'   # the number of evaluations per year.
#'
#'   # To determine the minimum confidence level we require for
#'   # the simultaneous prediction interval, USEPA (2009) recommends
#'   # setting the maximum allowed individual Type I Error level per constituent to:
#'
#'   # 1 - (1 - SWFPR)^(1 / Number of Constituents)
#'
#'   # which translates to setting the confidence limit to
#'
#'   # (1 - SWFPR)^(1 / Number of Constituents)
#'
#'   # where SWFPR = site-wide false positive rate.  For this example, we
#'   # will set SWFPR = 0.1.  Thus, the required individual Type I Error level
#'   # and confidence level per constituent are given as follows:
#'
#'   # n  = 20 based on 4 Background Wells
#'   # nw = 10 Compliance Wells
#'   # nc =  5 Constituents
#'   # ne =  1 Evaluation per year
#'
#'   n  <- 20
#'   nw <- 10
#'   nc <-  5
#'   ne <-  1
#'
#'   # Set number of future sampling occasions r to
#'   # Number Compliance Wells x Number Evaluations per Year
#'   r  <-  nw * ne
#'
#'   conf.level <- (1 - 0.1)^(1 / nc)
#'   conf.level
#'   #[1] 0.9791484
#'
#'   # So the required confidence level is 0.98, or 98%.
#'   # Now determine the confidence level associated with each plan.
#'   # Note that both plans achieve the required confidence level.
#'
#'   # 1) the 1-of-2 retesting plan for a median of order 3 using the
#'   #    background maximum
#'
#'   predIntNparSimultaneousConfLevel(n = 20, n.median = 3, k = 1, m = 2, r = r)
#'   #[1] 0.9940354
#'
#'
#'   # 2) the 1-of-4 plan based on individual observations using the 3rd highest
#'   #    background value.
#'
#'   predIntNparSimultaneousConfLevel(n = 20, k = 1, m = 4, r = r,
#'     n.plus.one.minus.upl.rank = 3)
#'   #[1] 0.9864909
#'
#'   #------------------------------------------------------------------------------
#'   # Compute approximate power of each plan to detect contamination at just 1 well
#'   # assuming true underying distribution of Hg is Normal at all wells and
#'   # using delta.over.sigma equal to 2, 3, and 4.
#'   #------------------------------------------------------------------------------
#'
#'   # Computer aproximate power for
#'   # 1) the 1-of-2 retesting plan for a median of order 3 using the
#'   #    background maximum
#'
#'   predIntNparSimultaneousTestPower(n = 20, n.median = 3, k = 1, m = 2, r = r,
#'     delta.over.sigma = 2:4, r.shifted = 1)
#'   #[1] 0.3953712 0.9129671 0.9983054
#'
#'
#'   # Compute approximate power for
#'   # 2) the 1-of-4 plan based on individual observations using the 3rd highest
#'   #    background value.
#'
#'   predIntNparSimultaneousTestPower(n = 20, k = 1, m = 4, r = r,
#'     n.plus.one.minus.upl.rank = 3, delta.over.sigma = 2:4, r.shifted = 1)
#'   #[1] 0.4367972 0.8694664 0.9888779
#'
#'
#'   #----------
#'
#'   \dontrun{
#'   # Compare estimated power using approximation method with estimated power
#'   # using Monte Carlo simulation for the 1-of-4 plan based on individual
#'   # observations using the 3rd highest background value.
#'
#'   predIntNparSimultaneousTestPower(n = 20, k = 1, m = 4, r = r,
#'     n.plus.one.minus.upl.rank = 3, delta.over.sigma = 2:4, r.shifted = 1,
#'     method = "simulate", ci = TRUE, NMC = 1000)
#'   #[1] 0.437 0.863 0.989
#'   #attr(,"conf.int")
#'   #         [,1]      [,2]      [,3]
#'   #LCL 0.4111999 0.8451148 0.9835747
#'   #UCL 0.4628001 0.8808852 0.9944253
#'   }
#'
#'   #==========
#'
#'   # Cleanup
#'   #--------
#'   rm(n, nw, nc, ne, r, conf.level)
#' }
#' @rawRd
#' \keyword{ distribution }
#' @rawRd
#' \keyword{ htest }

predIntNparSimultaneousTestPower <-
function (n, n.median = 1, k = 1, m = 2, r = 1, rule = "k.of.m", 
    lpl.rank = ifelse(pi.type == "upper", 0, 1), n.plus.one.minus.upl.rank = ifelse(pi.type == 
        "lower", 0, 1), delta.over.sigma = 0, pi.type = "upper", 
    r.shifted = r, method = "approx", NMC = 100, ci = FALSE, 
    ci.conf.level = 0.95, integrate.args.list = NULL, evNormOrdStats.method = "royston") 
{
    rule <- match.arg(rule, c("k.of.m", "CA", "Modified.CA"), 
        several.ok = TRUE)
    pi.type <- match.arg(pi.type, c("upper", "lower"))
    method <- match.arg(method, c("approx", "simulate"))
    if (!is.vector(n, mode = "numeric") || !all(is.finite(n)) || 
        any(n < 2)) 
        stop(paste("'n' must be a numeric vector", "with all elements greater than or equal to 2", 
            "and no Missing (NA), Infinite (-Inf, Inf),", "or Undefined (Nan) values."))
    if (!is.vector(n.median, mode = "numeric") || !all(is.finite(n.median)) || 
        any(n.median < 1) || !all(n.median == trunc(n.median)) || 
        !all(is.odd(n.median))) 
        stop("'n.median' must be a numeric vector of positive odd integers")
    if (!is.vector(k, mode = "numeric") || !all(is.finite(k)) || 
        any(k < 1)) 
        stop(paste("'k' must be a numeric vector", "with all elements greater than or equal to 1", 
            "and no Missing (NA), Infinite (-Inf, Inf),", "or Undefined (Nan) values."))
    if (!is.vector(m, mode = "numeric") || !all(is.finite(m)) || 
        any(m < 1)) 
        stop(paste("'m' must be a numeric vector", "with all elements greater than or equal to 1", 
            "and no Missing (NA), Infinite (Inf, -Inf),", "or Undefined (Nan) values."))
    if (!is.vector(r, mode = "numeric") || !all(is.finite(r)) || 
        any(r < 1)) 
        stop(paste("'r' must be a numeric vector", "with all elements greater than or equal to 1", 
            "and no Missing (NA), Infinite (-Inf, Inf),", "or Undefined (Nan) values."))
    if (pi.type == "upper") 
        lpl.rank <- 0
    else n.plus.one.minus.upl.rank <- 0
    if (!is.vector(lpl.rank, mode = "numeric") || !all(is.finite(lpl.rank)) || 
        any(lpl.rank != trunc(lpl.rank)) || any(lpl.rank < 0 | 
        lpl.rank >= n)) 
        stop(paste("'lpl.rank' must be a vector of non-negative", 
            "integers less than the corresponding value of 'n'"))
    if (pi.type == "lower" & any(lpl.rank < 1)) 
        stop("When pi.type='lower', all values of 'lpl.rank' must be positive integers")
    if (!is.vector(n.plus.one.minus.upl.rank, mode = "numeric") || 
        !all(is.finite(n.plus.one.minus.upl.rank)) || any(n.plus.one.minus.upl.rank != 
        trunc(n.plus.one.minus.upl.rank)) || any(n.plus.one.minus.upl.rank < 
        0 | n.plus.one.minus.upl.rank >= n)) 
        stop(paste("'n.plus.one.minus.upl.rank' must be a vector of non-negative", 
            "integers less than the corresponding value of 'n'"))
    if (pi.type == "upper" & any(n.plus.one.minus.upl.rank < 
        1)) 
        stop("When pi.type='upper' all values of 'n.plus.one.minus.upl.rank' must be positive integers")
    if (!is.vector(delta.over.sigma, mode = "numeric") || any(is.na(delta.over.sigma))) 
        stop(paste("'delta.over.sigma' must be a numeric vector", 
            "with no Missing (NA) or Undefined (Nan) values."))
    if (!is.vector(r.shifted, mode = "numeric") || !all(is.finite(r.shifted)) || 
        !all(r.shifted == trunc(r.shifted)) || any(r.shifted < 
        1) || any(r.shifted > r)) 
        stop(paste("'r.shifted' must be a numeric vector of positive integers", 
            "with all values less than or equal to", "the corresponding values of 'r'"))
    arg.mat <- cbind.no.warn(n = as.vector(n), n.median = as.vector(n.median), 
        k = as.vector(k), m = as.vector(m), r = as.vector(r), 
        lpl.rank = as.vector(lpl.rank), n.plus.one.minus.upl.rank = as.vector(n.plus.one.minus.upl.rank), 
        delta.over.sigma = as.vector(delta.over.sigma), r.shifted = as.vector(r.shifted))
    nrow.arg.mat <- nrow(arg.mat)
    length.rule <- length(rule)
    if (length.rule > nrow.arg.mat) 
        arg.mat <- arg.mat[rep(1:nrow.arg.mat, length.out = length.rule), 
            ]
    else rule <- rep(rule, length.out = nrow.arg.mat)
    for (i in c("n", "n.median", "k", "m", "r", "lpl.rank", "n.plus.one.minus.upl.rank", 
        "delta.over.sigma", "r.shifted")) assign(i, arg.mat[, 
        i])
    index <- rule == "k.of.m"
    if (any(index)) {
        if (any(k[index] > m[index])) 
            stop(paste("For cases where rule='k.of.m',", "all elements of 'k' must be less than or equal to", 
                "the corresponding elements of 'm'"))
    }
    index <- rule == "Modified.CA"
    m[index] <- 4
    N <- length(n)
    conf.level <- numeric(N)
    power <- numeric(N)
    for (i in 1:N) {
        conf.level[i] <- predIntNparSimultaneousConfLevel(n = n[i], 
            n.median = n.median[i], k = k[i], m = m[i], r = r[i], 
            rule = rule[i], lpl.rank = lpl.rank[i], n.plus.one.minus.upl.rank = n.plus.one.minus.upl.rank[i], 
            pi.type = pi.type, integrate.args.list = integrate.args.list)
    }
    if (pi.type == "upper") 
        pl.rank <- n + 1 - n.plus.one.minus.upl.rank
    else pl.rank <- lpl.rank
    if (method == "approx") {
        K <- numeric(N)
        for (i in 1:N) K[i] <- evNormOrdStatsScalar(r = pl.rank[i], 
            n = n[i], method = evNormOrdStats.method)
        if (pi.type == "lower") 
            K <- -K
        for (i in 1:N) {
            power[i] <- predIntNormSimultaneousTestPowerScalar(n = n[i], 
                n.mean = n.median[i], K = K[i], k = k[i], m = m[i], 
                r = r.shifted[i], rule = rule[i], delta.over.sigma = delta.over.sigma[i], 
                pi.type = pi.type, conf.level = conf.level[i], 
                integrate.args.list = integrate.args.list)
        }
    }
    else {
        for (i in 1:N) {
            n.i <- n[i]
            n.median.i <- n.median[i]
            k.i <- k[i]
            m.i <- m[i]
            r.i <- r[i]
            pl.rank.i <- pl.rank[i]
            r.shifted.i <- r.shifted[i]
            delta.over.sigma.i <- delta.over.sigma[i]
            mean.i <- c(rep(delta.over.sigma.i, r.shifted.i), 
                rep(0, r.i - r.shifted.i))
            out.vec <- logical(NMC)
            if (pi.type == "upper") {
                test.fcn <- switch(rule[i], k.of.m = function(z, 
                  PL, k) sum(z <= PL) < k, CA = function(z, PL, 
                  k) (z[1] > PL) & (sum(z > PL) >= 2), Modified.CA = function(z, 
                  PL, k) (z[1] > PL) & (sum(z > PL) >= 3))
            }
            else {
                test.fcn <- switch(rule[i], k.of.m = function(z, 
                  PL, k) sum(z >= PL) < k, CA = function(z, PL, 
                  k) (z[1] < PL) & (sum(z < PL) >= 2), Modified.CA = function(z, 
                  PL, k) (z[1] < PL) & (sum(z < PL) >= 3))
            }
            for (j in 1:NMC) {
                x <- rnorm(n.i)
                PL <- sort(x)[pl.rank.i]
                new.x <- array(rnorm(n.median.i * m.i * r.i, 
                  mean = mean.i), dim = c(r.i, n.median.i, m.i))
                new.x <- apply(new.x, c(1, 3), median)
                out.vec[j] <- any(apply(new.x, 1, test.fcn, PL = PL, 
                  k = k.i))
            }
            power[i] <- mean(out.vec)
        }
        if (ci) {
            SE.power <- sqrt(power * (1 - power)/NMC)
            LCL <- power - qnorm(ci.conf.level) * SE.power
            UCL <- power + qnorm(ci.conf.level) * SE.power
            attr(power, "conf.int") <- rbind(LCL = LCL, UCL = UCL)
        }
    }
    power
}
