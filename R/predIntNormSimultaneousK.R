#' Compute the Value of \eqn{K} for a Simultaneous Prediction Interval for a Normal Distribution
#' @description
#' Compute the value of \eqn{K} (the multiplier of estimated standard deviation) used
#'   to construct a simultaneous prediction interval based on data from a
#'   \link[stats:Normal]{normal distribution}.
#'   The function \cr
#'   \code{predIntNormSimultaneousK} is called by \code{\link{predIntNormSimultaneous}}.
#' @usage
#' predIntNormSimultaneousK(n, df = n - 1, n.mean = 1, k = 1, m = 2, r = 1,
#'     rule = "k.of.m", delta.over.sigma = 0, pi.type = "upper", conf.level = 0.95,
#'     K.tol = .Machine$double.eps^0.5, integrate.args.list = NULL)
#' @rawRd
#' \arguments{
#'   \item{n}{
#'   a positive integer greater than 2 indicating the sample size upon which the
#'   prediction interval is based.
#' }
#'   \item{df}{
#'   the degrees of freedom associated with the prediction interval.  The default is
#'   \code{df=n-1}.
#' }
#'   \item{n.mean}{
#'   positive integer specifying the sample size associated with the future averages.
#'   The default value is \code{n.mean=1} (i.e., individual observations).  Note that all
#'   future averages must be based on the same sample size.
#' }
#'   \item{k}{
#'   for the \eqn{k}-of-\eqn{m} rule (\code{rule="k.of.m"}), a positive integer
#'   specifying the minimum number of observations (or averages) out of \eqn{m}
#'   observations (or averages) (all obtained on one future sampling \dQuote{occassion})
#'   the prediction interval should contain with confidence level \code{conf.level}.
#'   The default value is \code{k=1}.  This argument is ignored when the argument
#'   \code{rule} is not equal to \code{"k.of.m"}.
#' }
#'   \item{m}{
#'   positive integer specifying the maximum number of future observations (or
#'   averages) on one future sampling \dQuote{occasion}.
#'   The default value is \code{m=2}, except when \code{rule="Modified.CA"}, in which
#'   case this argument is ignored and \code{m} is automatically set equal to \code{4}.
#' }
#'   \item{r}{
#'   positive integer specifying the number of future sampling \dQuote{occasions}.
#'   The default value is \code{r=1}.
#' }
#'   \item{rule}{
#'   character string specifying which rule to use.  The possible values are
#'   \code{"k.of.m"} (\eqn{k}-of-\eqn{m} rule; the default), \code{"CA"} (California rule),
#'   and \code{"Modified.CA"} (modified California rule).
#'   See the DETAILS section below for more information.
#' }
#'   \item{delta.over.sigma}{
#'   numeric scalar indicating the ratio \eqn{\Delta/\sigma}.  The quantity
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
#'   \code{pi.type="lower"}.  \bold{NOTE:} In Versions 2.4.0 - 2.8.1 of \emph{EnvStats},
#'   the value \code{pi.type="two-sided"} was allowed, but these two-sided simultaneous
#'   prediction intervals were based on faulty assumptions and were \bold{NOT} valid.
#'
#' }
#'   \item{conf.level}{
#'   a scalar between 0 and 1 indicating the confidence level of the prediction interval.
#'   The default value is \code{conf.level=0.95}.
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
#' and background material are available in \code{vignette("extended-function-details", package = "EnvStats")}, section \code{predIntNormSimultaneousK}.
#' @rawRd
#' \value{
#'   A numeric scalar equal to \eqn{K}, the multiplier of estimated standard
#'   deviation that is used to construct the simultaneous prediction interval.
#' }
#' @rawRd
#' \references{
#'   \bold{Barclay's California Code of Regulations}. (1991). Title 22,
#'   Section 66264.97 [concerning hazardous waste facilities] and Title 23,
#'   Section 2550.7(e)(8) [concerning solid waste facilities].
#'   Barclay's Law Publishers, San Francisco, CA.
#'
#'   Davis, C.B. (1998a).  \emph{Ground-Water Statistics & Regulations:  Principles,
#'   Progress and Problems}.  Second Edition.  Environmetrics & Statistics Limited,
#'   Henderson, NV.
#'
#'   Davis, C.B. (1998b). Personal Communication, September 3, 1998.
#'
#'   Davis, C.B., and R.J. McNichols. (1987).  One-sided Intervals for at Least \eqn{p}
#'   of \eqn{m} Observations from a Normal Population on Each of \eqn{r} Future Occasions.
#'   \emph{Technometrics} \bold{29}, 359--370.
#'
#'   Fertig, K.W., and N.R. Mann. (1977).  One-Sided Prediction Intervals for at Least
#'   \eqn{p} Out of \eqn{m} Future Observations From a Normal Population.
#'   \emph{Technometrics} \bold{19}, 167--177.
#'
#'   Gibbons, R.D., D.K. Bhaumik, and S. Aryal. (2009).
#'   \emph{Statistical Methods for Groundwater Monitoring}, Second Edition.
#'   John Wiley & Sons, Hoboken.
#'
#'   Hahn, G.J. (1969). Factors for Calculating Two-Sided Prediction Intervals for
#'   Samples from a Normal Distribution.
#'   \emph{Journal of the American Statistical Association} \bold{64}(327), 878-898.
#'
#'   Hahn, G.J. (1970a). Additional Factors for Calculating Prediction Intervals for
#'   Samples from a Normal Distribution.
#'   \emph{Journal of the American Statistical Association} \bold{65}(332), 1668-1676.
#'
#'   Hahn, G.J. (1970b). Statistical Intervals for a Normal Population, Part I: Tables,
#'   Examples and Applications. \emph{Journal of Quality Technology} \bold{2}(3), 115-125.
#'
#'   Hahn, G.J. (1970c). Statistical Intervals for a Normal Population, Part II:
#'   Formulas, Assumptions, Some Derivations. \emph{Journal of Quality Technology}
#'   \bold{2}(4), 195-206.
#'
#'   Hahn, G.J., and W.Q. Meeker. (1991). \emph{Statistical Intervals: A Guide for
#'   Practitioners}.  John Wiley and Sons, New York.
#'
#'   Hahn, G., and W. Nelson. (1973). A Survey of Prediction Intervals and Their
#'   Applications.  \emph{Journal of Quality Technology} \bold{5}, 178-188.
#'
#'   Hall, I.J., and R.R. Prairie. (1973).  One-Sided Prediction Intervals to Contain at
#'   Least \eqn{m} Out of \eqn{k} Future Observations.
#'   \emph{Technometrics} \bold{15}, 897--914.
#'
#'   Millard, S.P. (1987).  Environmental Monitoring, Statistics, and the Law:  Room for
#'   Improvement (with Comment).  \emph{The American Statistician} \bold{41}(4), 249--259.
#'
#'   Millard, S.P., and Neerchal, N.K. (2001). \emph{Environmental Statistics with S-PLUS}.
#'   CRC Press, Boca Raton, Florida.
#'
#'   USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
#'   EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.
#'   U.S. Environmental Protection Agency, Washington, D.C.
#'
#'   USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
#'   EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
#'   U.S. Environmental Protection Agency, Washington, D.C.
#' }
#' @rawRd
#' \author{
#'     Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \note{
#'   \emph{Motivation} \cr
#'   Prediction and tolerance intervals have long been applied to quality control and
#'   life testing problems (Hahn, 1970b,c; Hahn and Nelson, 1973).  In the context of
#'   environmental statistics, prediction intervals are useful for analyzing data from
#'   groundwater detection monitoring programs at hazardous and solid waste facilities.
#'
#'   One of the main statistical problems that plague groundwater monitoring programs at
#'   hazardous and solid waste facilities is the requirement of testing several wells and
#'   several constituents at each well on each sampling occasion.  This is an obvious
#'   multiple comparisons problem, and the naive approach of using a standard t-test at
#'   a conventional \eqn{\alpha}-level (e.g., 0.05 or 0.01) for each test leads to a
#'   very high probability of at least one significant result on each sampling occasion,
#'   when in fact no contamination has occurred.  This problem was pointed out years ago
#'   by Millard (1987) and others.
#'
#'   Davis and McNichols (1987) proposed simultaneous prediction intervals as a way of
#'   controlling the facility-wide false positive rate (FWFPR) while maintaining adequate
#'   power to detect contamination in the groundwater.  Because of the ubiquitous presence
#'   of spatial variability, it is usually best to use simultaneous prediction intervals
#'   at each well (Davis, 1998a).  That is, by constructing prediction intervals based on
#'   background (pre-landfill) data on each well, and comparing future observations at a
#'   well to the prediction interval for that particular well.  In each of these cases,
#'   the individual \eqn{\alpha}-level at each well is equal to the FWFRP divided by the
#'   product of the number of wells and constituents.
#'
#'   Often, observations at downgradient wells are not available prior to the
#'   construction and operation of the landfill.  In this case, upgradient well data can
#'   be combined to create a background prediction interval, and observations at each
#'   downgradient well can be compared to this prediction interval.  If spatial
#'   variability is present and a major source of variation, however, this method is not
#'   really valid (Davis, 1994; Davis, 1998a).
#'
#'   Chapter 19 of USEPA (2009) contains an extensive discussion of using the
#'   \eqn{1}-of-\eqn{m} rule and the Modified California rule.
#'
#'   Chapters 1 and 3 of Gibbons et al. (2009) discuss simultaneous prediction intervals
#'   for the normal and lognormal distributions, respectively.
#'   \cr
#'
#'   \emph{The k-of-m Rule} \cr
#'   For the \eqn{k}-of-\eqn{m} rule, Davis and McNichols (1987) give tables with
#'   \dQuote{optimal} choices of \eqn{k} (in terms of best power for a given overall
#'   confidence level) for selected values of \eqn{m}, \eqn{r}, and \eqn{n}.  They found
#'   that the optimal ratios of \eqn{k} to \eqn{m} (i.e., \eqn{k/m}) are generally small,
#'   in the range of 15-50\%.
#'   \cr
#'
#'   \emph{The California Rule} \cr
#'   The California rule was mandated in that state for groundwater monitoring at waste
#'   disposal facilities when resampling verification is part of the statistical program
#'   (Barclay's Code of California Regulations, 1991).  The California code mandates a
#'   \dQuote{California} rule with \eqn{m \ge 3}.  The motivation for this rule may have
#'   been a desire to have a majority of the observations in bounds (Davis, 1998a).  For
#'   example, for a \eqn{k}-of-\eqn{m} rule with \eqn{k=1} and \eqn{m=3}, a monitoring
#'   location will pass if the first observation is out of bounds, the second resample
#'   is out of bounds, but the last resample is in bounds, so that 2 out of 3 observations
#'   are out of bounds.  For the California rule with \eqn{m=3}, either the first
#'   observation must be in bounds, or the next 2 observations must be in bounds in order
#'   for the monitoring location to pass.
#'
#'   Davis (1998a) states that if the FWFPR is kept constant, then the California rule
#'   offers little increased power compared to the \eqn{k}-of-\eqn{m} rule, and can
#'   actually decrease the power of detecting contamination.
#'   \cr
#'
#'   \emph{The Modified California Rule} \cr
#'   The Modified California Rule was proposed as a compromise between a 1-of-\eqn{m}
#'   rule and the California rule.  For a given FWFPR, the Modified California rule
#'   achieves better power than the California rule, and still requires at least as many
#'   observations in bounds as out of bounds, unlike a 1-of-\eqn{m} rule.
#'   \cr
#'
#'   \emph{Different Notations Between Different References} \cr
#'   For the \eqn{k}-of-\eqn{m} rule described in this help file, both
#'   Davis and McNichols (1987) and USEPA (2009, Chapter 19) use the variable \eqn{p} instead of \eqn{k} to represent the minimum number
#'   of future observations the interval should contain on each of the \eqn{r} sampling
#'   occasions.
#'
#'   Gibbons et al. (2009, Chapter 1) presents extensive lists of the value of
#'   \eqn{K} for both \eqn{k}-of-\eqn{m} rules and California rules.  Gibbons et al.'s
#'   notation reverses the meaning of \eqn{k} and \eqn{r} compared to the notation used
#'   in this help file.  That is, in Gibbons et al.'s notation, \eqn{k} represents the
#'   number of future sampling occasions or monitoring wells, and \eqn{r} represents the
#'   minimum number of observations the interval should contain on each sampling occasion.
#'
#'   USEPA (2009, Chapter 19) uses \eqn{p} in place of \eqn{k}.
#' }
#' @rawRd
#' \seealso{
#'   \code{\link{predIntNormSimultaneous}},
#'   \code{\link{predIntNormSimultaneousTestPower}},
#'   \code{\link{predIntNorm}}, \code{\link{predIntNormK}},
#'   \code{\link{predIntLnormSimultaneous}}, \code{\link{tolIntNorm}},
#'   \link{Normal}, \code{\link{estimate.object}}, \code{\link{enorm}}
#' }
#' @rawRd
#' \examples{
#'   # Compute the value of K for an upper 95% simultaneous prediction
#'   # interval to contain at least 1 out of the next 3 observations
#'   # given a background sample size of n=8.
#'
#'   predIntNormSimultaneousK(n = 8, k = 1, m = 3)
#'   #[1] 0.5123091
#'
#'   #----------
#'
#'   # Compare the value of K for a 95% 1-of-3 upper prediction interval to
#'   # the value for the California and Modified California rules.
#'   # Note that the value of K for the Modified California rule is between
#'   # the value of K for the 1-of-3 rule and the California rule.
#'
#'   predIntNormSimultaneousK(n = 8, k = 1, m = 3)
#'   #[1] 0.5123091
#'
#'   predIntNormSimultaneousK(n = 8, m = 3, rule = "CA")
#'   #[1] 1.252077
#'
#'   predIntNormSimultaneousK(n = 8, rule = "Modified.CA")
#'   #[1] 0.8380233
#'
#'   #----------
#'
#'   # Show how the value of K for an upper 95% simultaneous prediction
#'   # limit increases as the number of future sampling occasions r increases.
#'   # Here, we'll use the 1-of-3 rule.
#'
#'   predIntNormSimultaneousK(n = 8, k = 1, m = 3)
#'   #[1] 0.5123091
#'
#'
#'   predIntNormSimultaneousK(n = 8, k = 1, m = 3, r = 10)
#'   #[1] 1.363002
#'
#'   #==========
#'
#'   # Example 19-1 of USEPA (2009, p. 19-17) shows how to compute an
#'   # upper simultaneous prediction limit for the 1-of-3 rule for
#'   # r = 2 future sampling occasions.  The data for this example are
#'   # stored in EPA.09.Ex.19.1.sulfate.df.
#'
#'   # We will pool data from 4 background wells that were sampled on
#'   # a number of different occasions, giving us a sample size of
#'   # n = 25 to use to construct the prediction limit.
#'
#'   # There are 50 compliance wells and we will monitor 10 different
#'   # constituents at each well at each of the r=2 future sampling
#'   # occasions.  To determine the confidence level we require for
#'   # the simultaneous prediction interval, USEPA (2009) recommends
#'   # setting the individual Type I Error level at each well to
#'
#'   # 1 - (1 - SWFPR)^(1 / (Number of Constituents * Number of Wells))
#'
#'   # which translates to setting the confidence limit to
#'
#'   # (1 - SWFPR)^(1 / (Number of Constituents * Number of Wells))
#'
#'   # where SWFPR = site-wide false positive rate.  For this example, we
#'   # will set SWFPR = 0.1.  Thus, the confidence level is given by:
#'
#'   nc <- 10
#'   nw <- 50
#'   SWFPR <- 0.1
#'   conf.level <- (1 - SWFPR)^(1 / (nc * nw))
#'
#'   conf.level
#'   #[1] 0.9997893
#'
#'   #----------
#'
#'   # Compute the value of K for the upper simultaneous prediction
#'   # limit for the 1-of-3 plan.
#'
#'   predIntNormSimultaneousK(n = 25, k = 1, m = 3, r = 2,
#'     rule = "k.of.m", pi.type = "upper", conf.level = conf.level)
#'   #[1] 2.014365
#'
#'
#'  #==========
#'
#'   \dontrun{
#'   # Try to compute K for a two-sided simultaneous prediction interval:
#'
#'   predIntNormSimultaneousK(n = 25, k = 1, m = 3, r = 2,
#'    rule = "k.of.m", pi.type = "two-sided", conf.level = conf.level)
#'   #Error in predIntNormSimultaneousK(n = 25, k = 1, m = 3, r = 2, rule = "k.of.m",  :
#'   #  Two-sided simultaneous prediction intervals are not currently available.
#'   # NOTE: Two-sided simultaneous prediction intervals computed using
#'   # Versions 2.4.0 - 2.8.1 of EnvStats are *NOT* valid.
#'   }
#'
#'   #==========
#'
#'   # Cleanup
#'   #--------
#'
#'   rm(nc, nw, SWFPR, conf.level)
#' }
#' @rawRd
#' \keyword{ distribution }
#' @rawRd
#' \keyword{ htest }

predIntNormSimultaneousK <-
function (n, df = n - 1, n.mean = 1, k = 1, m = 2, r = 1, rule = "k.of.m",
    delta.over.sigma = 0, pi.type = "upper", conf.level = 0.95,
    K.tol = .Machine$double.eps^0.5, integrate.args.list = NULL)
{
    rule <- match.arg(rule, c("k.of.m", "CA", "Modified.CA"),
        several.ok = TRUE)
    pi.type <- match.arg(pi.type, c("upper", "lower", "two-sided"))
    if (pi.type == "two-sided") {
        stop(paste(
            "Two-sided simultaneous prediction intervals are not currently available.\n",
            "NOTE: Two-sided simultaneous prediction intervals computed using\n",
            "Versions 2.4.0 - 2.8.1 of EnvStats are *NOT* valid."
        ))
    }
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
    if (!is.vector(k, mode = "numeric") || !all(is.finite(k)) ||
        any(k < 1))
        stop(paste("'k' must be a numeric vector", "with all elements greater than or eqal to 1",
            "and no Missing (NA), Infinite (-Inf, Inf),", "or Undefined (Nan) values."))
    if (!is.vector(m, mode = "numeric") || !all(is.finite(m)) ||
        any(m < 1))
        stop(paste("'m' must be a numeric vector", "with all elements greater than or equal to 1",
            "and no Missing (NA), Infinite (Inf, -Inf),", "or Undefined (Nan) values."))
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
    arg.mat <- cbind.no.warn(n = as.vector(n), df = as.vector(df),
        n.mean = as.vector(n.mean), k = as.vector(k), m = as.vector(m),
        r = as.vector(r), delta.over.sigma = as.vector(delta.over.sigma),
        conf.level = as.vector(conf.level))
    nrow.arg.mat <- nrow(arg.mat)
    length.rule <- length(rule)
    if (length.rule > nrow.arg.mat)
        arg.mat <- arg.mat[rep(1:nrow.arg.mat, length.out = length.rule),
            ]
    else rule <- rep(rule, length.out = nrow.arg.mat)
    for (i in c("n", "df", "n.mean", "k", "m", "r", "delta.over.sigma",
        "conf.level")) assign(i, arg.mat[, i])
    index <- rule == "k.of.m"
    if (any(index)) {
        if (any(k[index] > m[index]))
            stop(paste("For cases where rule='k.of.m',", "all elements of 'k' must be less than or equal to",
                "the corresponding elements of 'm'"))
    }
    index <- rule == "Modified.CA"
    m[index] <- 4
    N <- length(n)
    K <- numeric(N)
    for (i in 1:N) {
        K[i] <- switch(rule[i], k.of.m = {
            pred.int.norm.k.of.m.on.r.K(n = n[i], df = df[i],
                n.mean = n.mean[i], k = k[i], m = m[i], r = r[i],
                delta.over.sigma = delta.over.sigma[i],
                pi.type = pi.type,
                conf.level = conf.level[i], K.tol = K.tol, integrate.args.list = integrate.args.list)
        }, CA = {
            pred.int.norm.CA.on.r.K(n = n[i], df = df[i], n.mean = n.mean[i],
                m = m[i], r = r[i], delta.over.sigma = delta.over.sigma[i],
                conf.level = conf.level[i],
                K.tol = K.tol)
        }, Modified.CA = {
            pred.int.norm.Modified.CA.on.r.K(n = n[i], df = df[i],
                n.mean = n.mean[i], r = r[i], delta.over.sigma = delta.over.sigma[i],
                conf.level = conf.level[i],
                K.tol = K.tol)
        })
    }
    K
}
