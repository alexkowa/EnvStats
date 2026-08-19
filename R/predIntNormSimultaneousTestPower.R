#' Probability That at Least One Set of Future Observations Violates the Given Rule Based on a Simultaneous Prediction Interval for a Normal Distribution
#' @description
#' Compute the probability that at least one set of future observations violates the
#'   given rule based on a simultaneous prediction interval for the next \eqn{r} future
#'   sampling occasions for a normal distribution.  The three possible rules are:
#'   \eqn{k}-of-\eqn{m}, California, or Modified California.
#' @usage
#' predIntNormSimultaneousTestPower(n, df = n - 1, n.mean = 1, k = 1, m = 2, r = 1,
#'     rule = "k.of.m", delta.over.sigma = 0, pi.type = "upper", conf.level = 0.95,
#'     r.shifted = r, K.tol = .Machine$double.eps^0.5, integrate.args.list = NULL)
#' @rawRd
#' \arguments{
#'   \item{n}{
#'   vector of positive integers greater than 2 indicating the sample size upon which
#'   the prediction interval is based.
#' }
#'   \item{df}{
#'   vector of positive integers indicating the degrees of freedom associated with
#'   the sample size.  The default value is \code{df=n-1}.
#' }
#'   \item{n.mean}{
#'   positive integer specifying the sample size associated with the future averages.
#'   The default value is \code{n.mean=1} (i.e., individual observations).  Note that all
#'   future averages must be based on the same sample size.
#' }
#'   \item{k}{
#'   for the \eqn{k}-of-\eqn{m} rule (\code{rule="k.of.m"}), vector of positive integers
#'   specifying the minimum number of observations (or averages) out of \eqn{m}
#'   observations (or averages) (all obtained on one future sampling \dQuote{occassion})
#'   the prediction interval should contain with confidence level \code{conf.level}.
#'   The default value is \code{k=1}.  This argument is ignored when the argument
#'   \code{rule} is not equal to \code{"k.of.m"}.
#' }
#'   \item{m}{
#'   vector of positive integers specifying the maximum number of future observations (or
#'   averages) on one future sampling \dQuote{occasion}.
#'   The default value is \code{m=2}, except when \code{rule="Modified.CA"}, in which
#'   case this argument is ignored and \code{m} is automatically set equal to \code{4}.
#' }
#'   \item{r}{
#'   vector of positive integers specifying the number of future sampling \dQuote{occasions}.
#'   The default value is \code{r=1}.
#' }
#'   \item{rule}{
#'   character string specifying which rule to use.  The possible values are
#'   \code{"k.of.m"} (\eqn{k}-of-\eqn{m} rule; the default), \code{"CA"} (California rule),
#'   and \code{"Modified.CA"} (modified California rule).
#'   See the DETAILS section below for more information.
#' }
#'   \item{delta.over.sigma}{
#'   numeric vector indicating the ratio \eqn{\Delta/\sigma}.  The quantity
#'   \eqn{\Delta} (delta) denotes the difference between the mean of the population
#'   that was sampled to construct the prediction interval, and the mean of the
#'   population that will be sampled to produce the future observations.  The quantity
#'   \eqn{\sigma} (sigma) denotes the population standard deviation for both populations.
#'   See the DETAILS section below for more information.  The default value is
#'   \code{delta.over.sigma=0}.
#' }
#'   \item{pi.type}{
#'   character string indicating what kind of prediction interval to compute.
#'   The possible values are \code{pi.type="upper"} (the default), and
#'   \code{pi.type="lower"}.
#' }
#'   \item{conf.level}{
#'   vector of values between 0 and 1 indicating the confidence level of the prediction interval.
#'   The default value is \code{conf.level=0.95}.
#' }
#'   \item{r.shifted}{
#'   vector of positive integers specifying the number of future sampling occasions for
#'   which the scaled mean is shifted by \eqn{\Delta/\sigma}.  All values must be
#'   integeters between \code{1} and the corresponding element of \code{r}.
#'   The default value is \code{r.shifted=r}.
#' }
#'   \item{K.tol}{
#'   numeric scalar indicating the tolerance to use in the nonlinear search algorithm to
#'   compute \eqn{K}.  The default value is \code{K.tol=.Machine$double.eps^(1/2)}.
#'   For many applications, the value of \eqn{K} needs to be known only to the second
#'   decimal place, in which case setting \code{K.tol=1e-4} will speed up computation a
#'   bit.
#' }
#'   \item{integrate.args.list}{
#'   a list of arguments to supply to the \code{\link{integrate}} function.  The
#'   default value is \code{integrate.args.list=NULL} which means that the
#'   default values of \code{\link{integrate}} are used.
#' }
#' }
#' @details
#' This help page has been shortened to keep function help focused on usage,
#' arguments, return values, and examples. Extended method details, formulas,
#' and background material are available in \code{vignette("extended-function-details", package = "EnvStats")}, section \code{predIntNormSimultaneousTestPower}.
#' @rawRd
#' \value{
#'   vector of values between 0 and 1 equal to the probability that
#'   the rule will be violated.
#' }
#' @rawRd
#' \references{
#'   See the help file for \code{\link{predIntNormSimultaneous}}.
#' }
#' @rawRd
#' \author{
#'     Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \note{
#'   See the help file for \code{\link{predIntNormSimultaneous}}.
#'
#'   In the course of designing a sampling program, an environmental scientist may wish
#'   to determine the relationship between sample size, significance level, power, and
#'   scaled difference if one of the objectives of the sampling program is to determine
#'   whether two distributions differ from each other.  The functions
#'   \code{predIntNormSimultaneousTestPower} and \cr
#'   \code{\link{plotPredIntNormSimultaneousTestPowerCurve}} can be
#'   used to investigate these relationships for the case of normally-distributed
#'   observations.
#' }
#' @rawRd
#' \seealso{
#'   \code{\link{predIntNormSimultaneous}}, \code{\link{predIntNormSimultaneousK}}, \cr
#'   \code{\link{plotPredIntNormSimultaneousTestPowerCurve}},
#'   \code{\link{predIntNorm}}, \code{\link{predIntNormK}}, \cr
#'   \code{\link{predIntNormTestPower}}, \link{Prediction Intervals},
#'   \link{Normal}.
#' }
#' @rawRd
#' \examples{
#'   # For the k-of-m rule with n=4, k=1, m=3, and r=1, show how the power increases
#'   # as delta.over.sigma increases. Assume a 95% upper prediction interval.
#'
#'   predIntNormSimultaneousTestPower(n = 4, m = 3, delta.over.sigma = 0:2)
#'   #[1] 0.0500000 0.2954156 0.7008558
#'
#'   #----------
#'
#'   # Look at how the power increases with sample size for an upper one-sided
#'   # prediction interval using the k-of-m rule with k=1, m=3, r=20,
#'   # delta.over.sigma=2, and a confidence level of 95%.
#'
#'   predIntNormSimultaneousTestPower(n = c(4, 8), m = 3, r = 20, delta.over.sigma = 2)
#'   #[1] 0.6075972 0.9240924
#'
#'   #----------
#'
#'   # Compare the power for the 1-of-3 rule with the power for the California and
#'   # Modified California rules, based on a 95% upper prediction interval and
#'   # delta.over.sigma=2.  Assume a sample size of n=8.  Note that in this case the
#'   # power for the Modified California rule is greater than the power for the
#'   # 1-of-3 rule and California rule.
#'
#'   predIntNormSimultaneousTestPower(n = 8, k = 1, m = 3, delta.over.sigma = 2)
#'   #[1] 0.788171
#'
#'   predIntNormSimultaneousTestPower(n = 8, m = 3, rule = "CA", delta.over.sigma = 2)
#'   #[1] 0.7160434
#'
#'   predIntNormSimultaneousTestPower(n = 8, rule = "Modified.CA", delta.over.sigma = 2)
#'   #[1] 0.8143687
#'
#'   #----------
#'
#'   # Show how the power for an upper 95% simultaneous prediction limit increases
#'   # as the number of future sampling occasions r increases.  Here, we'll use the
#'   # 1-of-3 rule with n=8 and delta.over.sigma=1.
#'
#'   predIntNormSimultaneousTestPower(n = 8, k = 1, m = 3, r=c(1, 2, 5, 10),
#'     delta.over.sigma = 1)
#'   #[1] 0.3492512 0.4032111 0.4503603 0.4633773
#'
#'   #==========
#'
#'   # USEPA (2009) contains an example on page 19-23 that involves monitoring
#'   # nw=100 compliance wells at a large facility with minimal natural spatial
#'   # variation every 6 months for nc=20 separate chemicals.
#'   # There are n=25 background measurements for each chemical to use to create
#'   # simultaneous prediction intervals.  We would like to determine which kind of
#'   # resampling plan based on normal distribution simultaneous prediction intervals to
#'   # use (1-of-m, 1-of-m based on means, or Modified California) in order to have
#'   # adequate power of detecting an increase in chemical concentration at any of the
#'   # 100 wells while at the same time maintaining a site-wide false positive rate
#'   # (SWFPR) of 10% per year over all 4,000 comparisons
#'   # (100 wells x 20 chemicals x semi-annual sampling).
#'
#'   # The function predIntNormSimultaneousTestPower includes the argument "r"
#'   # that is the number of future sampling occasions (r=2 in this case because
#'   # we are performing semi-annual sampling), so to compute the individual test
#'   # Type I error level alpha.test (and thus the individual test confidence level),
#'   # we only need to worry about the number of wells (100) and the number of
#'   # constituents (20): alpha.test = 1-(1-alpha)^(1/(nw x nc)).  The individual
#'   # confidence level is simply 1-alpha.test.  Plugging in 0.1 for alpha,
#'   # 100 for nw, and 20 for nc yields an individual test confidence level of
#'   # 1-alpha.test = 0.9999473.
#'
#'   nc <- 20
#'   nw <- 100
#'   conf.level <- (1 - 0.1)^(1 / (nc * nw))
#'   conf.level
#'   #[1] 0.9999473
#'
#'   # Now we can compute the power of any particular sampling strategy using
#'   # predIntNormSimultaneousTestPower.  For example, here is the power of
#'   # detecting an increase of three standard deviations in concentration using
#'   # the prediction interval based on the "1-of-2" resampling rule:
#'
#'   predIntNormSimultaneousTestPower(n = 25, k = 1, m = 2, r = 2, rule = "k.of.m",
#'     delta.over.sigma = 3,  pi.type = "upper", conf.level = conf.level)
#'   #[1] 0.3900202
#'
#'   # The following commands will reproduce the table shown in Step 2 on page
#'   # 19-23 of USEPA (2009).  Because these commands can take more than a few
#'   # seconds to execute, we have commented them out here.  To run this example,
#'   # just remove the pound signs (#) that are in front of R commands.
#'
#'   #rule.vec <- c(rep("k.of.m", 3), "Modified.CA",  rep("k.of.m", 3))
#'
#'   #m.vec <- c(2, 3, 4, 4, 1, 2, 1)
#'
#'   #n.mean.vec <- c(rep(1, 4), 2, 2, 3)
#'
#'   #n.scenarios <- length(rule.vec)
#'
#'   #K.vec <- numeric(n.scenarios)
#'
#'   #Power.vec <- numeric(n.scenarios)
#'
#'   #K.vec <- predIntNormSimultaneousK(n = 25, k = 1, m = m.vec,  n.mean = n.mean.vec,
#'   #  r = 2, rule = rule.vec,  pi.type = "upper", conf.level = conf.level)
#'
#'   #Power.vec <- predIntNormSimultaneousTestPower(n = 25, k = 1, m = m.vec,
#'   #  n.mean = n.mean.vec, r = 2, rule = rule.vec,  delta.over.sigma = 3,
#'   #  pi.type = "upper",  conf.level = conf.level)
#'
#'   #Power.df <- data.frame(Rule = rule.vec, k = rep(1, n.scenarios),  m = m.vec,
#'   #  N.Mean = n.mean.vec, K = round(K.vec, 2),  Power = round(Power.vec, 2),
#'   #  Total.Samples = m.vec * n.mean.vec)
#'
#'   #Power.df
#'
#'   #         Rule k m N.Mean    K Power Total.Samples
#'   #1      k.of.m 1 2      1 3.16  0.39             2
#'   #2      k.of.m 1 3      1 2.33  0.65             3
#'   #3      k.of.m 1 4      1 1.83  0.81             4
#'   #4 Modified.CA 1 4      1 2.57  0.71             4
#'   #5      k.of.m 1 1      2 3.62  0.41             2
#'   #6      k.of.m 1 2      2 2.33  0.85             4
#'   #7      k.of.m 1 1      3 2.99  0.71             3
#'
#'   # The above table shows the K-multipliers for each prediction interval, along with
#'   # the power of detecting a change in concentration of three standard deviations at
#'   # any of the 100 wells during the course of a year, for each of the sampling
#'   # strategies considered.  The last three rows of the table correspond to sampling
#'   # strategies that involve using the mean of two or three observations.
#'
#'   #==========
#'
#'   # Clean up
#'   #---------
#'   rm(nc, nw, conf.level, rule.vec, m.vec, n.mean.vec, n.scenarios, K.vec,
#'     Power.vec, Power.df)
#' }
#' @rawRd
#' \keyword{ distribution }
#' @rawRd
#' \keyword{ design }
#' @rawRd
#' \keyword{ htest }

predIntNormSimultaneousTestPower <-
function (n, df = n - 1, n.mean = 1, k = 1, m = 2, r = 1, rule = "k.of.m", 
    delta.over.sigma = 0, pi.type = "upper", conf.level = 0.95, 
    r.shifted = r, K.tol = .Machine$double.eps^0.5, integrate.args.list = NULL) 
{
    rule <- match.arg(rule, c("k.of.m", "CA", "Modified.CA"), 
        several.ok = TRUE)
    pi.type <- match.arg(pi.type, c("upper", "lower"))
    if (!is.vector(n, mode = "numeric") || !all(is.finite(n)) || 
        any(n < 2)) 
        stop(paste("'n' must be a numeric vector", "with all elements greater than or equal to 2", 
            "and no Missing (NA), Infinite (-Inf, Inf),", "or Undefined (Nan) values."))
    if (!is.vector(df, mode = "numeric") || !all(is.finite(df)) || 
        any(df < 1)) 
        stop(paste("'df' must be a numeric vector", "with all elements greater than or equal to 1", 
            "and no Missing (NA), Infinite (Inf, -Inf),", "or Undefined (Nan) values."))
    if (!is.vector(n.mean, mode = "numeric") || !all(is.finite(n.mean)) || 
        any(n.mean < 1)) 
        stop(paste("'n.mean' must be a numeric vector", "with all elements greater than or equal to 1", 
            "and no Missing (NA), Infinite (Inf, -Inf),", "or Undefined (Nan) values."))
    if (!is.vector(m, mode = "numeric") || !all(is.finite(m)) || 
        any(m < 1)) 
        stop(paste("'m' must be a numeric vector", "with all elements greater than or equal to 1", 
            "and no Missing (NA), Infinite (Inf, -Inf),", "or Undefined (Nan) values."))
    if (!is.vector(k, mode = "numeric") || !all(is.finite(k)) || 
        any(k < 1)) 
        stop(paste("'k' must be a numeric vector", "with all elements greater than or eqal to 1", 
            "and no Missing (NA), Infinite (-Inf, Inf),", "or Undefined (Nan) values."))
    if (!is.vector(r, mode = "numeric") || !all(is.finite(r)) || 
        any(r < 1)) 
        stop(paste("'r' must be a numeric vector", "with all elements greater than or equal to 1", 
            "and no Missing (NA), Infinite (-Inf, Inf),", "or Undefined (Nan) values."))
    if (!is.vector(delta.over.sigma, mode = "numeric") || any(is.na(delta.over.sigma))) 
        stop(paste("'delta.over.sigma' must be a numeric vector", 
            "with no Missing (NA) or Undefined (Nan) values."))
    if (!is.vector(conf.level, mode = "numeric") || !all(is.finite(conf.level)) || 
        any(conf.level <= .Machine$double.eps) || any(conf.level >= 
        1 - .Machine$double.eps)) 
        stop(paste("'conf.level' must be a numeric vector", "with all elements between 0 and 1", 
            "and no Missing (NA), Infinite(-Inf, Inf),", "or Undefined (Nan) values."))
    if (!is.vector(r.shifted, mode = "numeric") || !all(is.finite(r.shifted)) || 
        !all(r.shifted == trunc(r.shifted)) || any(r.shifted < 
        1) || any(r.shifted > r)) 
        stop(paste("'r.shifted' must be a numeric vector of positive integers", 
            "with all values must be less than or equal to", 
            "the corresponding values of 'r'"))
    arg.mat <- cbind.no.warn(n = as.vector(n), df = as.vector(df), 
        n.mean = as.vector(n.mean), k = as.vector(k), m = as.vector(m), 
        r = as.vector(r), delta.over.sigma = as.vector(delta.over.sigma), 
        conf.level = as.vector(conf.level), r.shifted = as.vector(r.shifted))
    nrow.arg.mat <- nrow(arg.mat)
    length.rule <- length(rule)
    if (length.rule > nrow.arg.mat) 
        arg.mat <- arg.mat[rep(1:nrow.arg.mat, length.out = length.rule), 
            ]
    else rule <- rep(rule, length.out = nrow.arg.mat)
    for (i in c("n", "df", "n.mean", "k", "m", "r", "delta.over.sigma", 
        "conf.level", "r.shifted")) assign(i, arg.mat[, i])
    index <- rule == "k.of.m"
    if (any(index)) {
        if (any(k[index] > m[index])) 
            stop(paste("For cases where rule='k.of.m',", "all elements of 'k' must be less than or equal to", 
                "the corresponding elements of 'm'"))
    }
    index <- rule == "Modified.CA"
    m[index] <- 4
    N <- length(n)
    power <- numeric(N)
    index.0 <- delta.over.sigma == 0
    if (any(index.0)) 
        power[index.0] <- 1 - conf.level[index.0]
    if (!all(index.0)) {
        for (i in c("n", "df", "n.mean", "k", "m", "r", "delta.over.sigma", 
            "conf.level", "r.shifted")) assign(i, arg.mat[!index.0, 
            i])
        rule <- rule[!index.0]
        N <- length(n)
        power.sub <- numeric(N)
        index.easy <- rule == "k.of.m" & k == m & r == 1
        if (any(index.easy)) 
            power.sub[index.easy] <- predIntNormTestPower(n = n[index.easy], 
                df = df[index.easy], n.mean = n.mean[index.easy], 
                k = k[index.easy], delta.over.sigma = delta.over.sigma[index.easy], 
                pi.type = pi.type, conf.level = conf.level[index.easy])
        if (!all(index.easy)) {
            for (i in c("n", "df", "n.mean", "k", "m", "r", "delta.over.sigma", 
                "conf.level", "r.shifted")) assign(i, arg.mat[!index.0, 
                , drop = FALSE][!index.easy, i])
            rule <- rule[!index.easy]
            N <- length(n)
            if (all(n == n[1]) & all(df == df[1]) & all(k == 
                k[1]) & all(m == m[1]) & all(n.mean == n.mean[1]) & 
                all(r == r[1]) & all(rule == rule[1]) & all(conf.level == 
                conf.level[1])) {
                K <- predIntNormSimultaneousK(n = n[1], df = df[1], 
                  n.mean = n.mean[1], k = k[1], m = m[1], r = r[1], 
                  rule = rule[1], delta.over.sigma = 0, pi.type = pi.type, 
                  conf.level = conf.level[1], K.tol = K.tol)
                K <- rep(K, N)
            }
            else {
                K <- predIntNormSimultaneousK(n = n, df = df, 
                  n.mean = n.mean, k = k, m = m, r = r, rule = rule, 
                  delta.over.sigma = 0, pi.type = pi.type, conf.level = conf.level, 
                  K.tol = K.tol)
            }
            for (i in 1:N) power.sub[!index.easy][i] <- predIntNormSimultaneousTestPowerScalar(n = n[i], 
                df = df[i], n.mean = n.mean[i], K = K[i], k = k[i], 
                m = m[i], r = r.shifted[i], rule = rule[i], delta.over.sigma = delta.over.sigma[i], 
                pi.type = pi.type, conf.level = conf.level[i], 
                integrate.args.list = integrate.args.list)
        }
        power[!index.0] <- power.sub
    }
    power
}
