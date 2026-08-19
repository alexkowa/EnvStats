#' Sample Size for Simultaneous Nonparametric Prediction Interval for Continuous Distribution
#' @description
#' Compute the sample size necessary for a nonparametric simultaneous prediction
#'   interval to achieve a specified confidence level based on one of three possible
#'   rules: k-of-m, California, or Modified California.  Observations are assumed to
#'   come from from a continuous distribution.
#' @usage
#' predIntNparSimultaneousN(n.median = 1, k = 1, m = 2, r = 1, rule = "k.of.m",
#'     lpl.rank = ifelse(pi.type == "upper", 0, 1),
#'     n.plus.one.minus.upl.rank = ifelse(pi.type == "lower", 0, 1), pi.type = "upper",
#'     conf.level = 0.95, n.max = 5000, integrate.args.list = NULL, maxiter = 1000)
#' @rawRd
#' \arguments{
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
#'   vector of positive integers indicating the rank of the order statistic to use for
#'   the lower bound of the prediction interval.  When \code{pi.type="lower"}, the
#'   default value is \code{lpl.rank=1} (implying the minimum value of \code{x} is used
#'   as the lower bound of the prediction interval).  When \code{pi.type="upper"},
#'   the argument \code{lpl.rank} is set equal to \code{0}.
#' }
#'   \item{n.plus.one.minus.upl.rank}{
#'   vector of positive integers related to the rank of the order statistic to use for
#'   the upper bound of the prediction interval.  A value of \code{n.plus.one.minus.upl.rank=1}
#'   (the default) means use the first largest value, and in general a value of \cr
#'   \code{n.plus.one.minus.upl.rank=}\eqn{i} means use the \eqn{i}'th largest value. If \cr
#'   \code{pi.type="lower"}, this argument is set equal to \code{0}.
#' }
#'   \item{pi.type}{
#'   character string indicating what kind of prediction interval to compute.
#'   The possible values are \code{"two.sided"} (the default), \code{"lower"}, and
#'   \code{"upper"}.
#' }
#'   \item{conf.level}{
#'   numeric vector of values between 0 and 1 indicating the confidence level
#'   associated with the prediction interval.  The default value is \code{conf=0.95}.
#' }
#'   \item{n.max}{
#'   numeric scalar indicating the maximum sample size to consider.  This argument
#'   is used in the search algorithm to determine the required sample size.  The
#'   default value is \code{n.max=5000}.
#' }
#'   \item{integrate.args.list}{
#'   list of arguments to supply to the \code{\link{integrate}} function.  The default
#'   value is \code{NULL}.
#' }
#'   \item{maxiter}{
#'   positive integer indicating the maximum number of iterations to use in the
#'   \code{\link{uniroot}} search algorithm.  The default value is
#'   \code{maxiter=1000}.
#' }
#' }
#' @rawRd
#' \details{
#'   If the arguments \code{k}, \code{m}, \code{r}, \code{lpl.rank}, and
#'   \code{n.plus.one.minus.upl.rank} are not all the same length, they are replicated
#'   to be the same length as the length of the longest argument.
#'
#'   The function \code{predIntNparSimultaneousN} computes the required sample size
#'   \eqn{n} by solving Equation (8), (9), or (10) in the help file for
#'   \code{\link{predIntNparSimultaneous}} for \eqn{n}, depending on the value of the
#'   argument \code{rule}.
#'
#'   Note that when \code{rule="k.of.m"} and \code{r=1}, this is equivalent to a
#'   standard nonparametric prediction interval and you can use the function
#'   \code{\link{predIntNparN}} instead.
#' }
#' @rawRd
#' \value{
#'   vector of positive integers indicating the required sample size(s) for the
#'   specified nonparametric simultaneous prediction interval(s).
#' }
#' @rawRd
#' \references{
#'   See the help file for \code{\link{predIntNparSimultaneous}}.
#' }
#' @rawRd
#' \author{
#'   Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \note{
#'   See the help file for \code{\link{predIntNparSimultaneous}}.
#' }
#' @rawRd
#' \seealso{
#'   \code{\link{predIntNparSimultaneous}},
#'   \code{\link{predIntNparSimultaneousConfLevel}}, \cr
#'   \code{\link{plotPredIntNparSimultaneousDesign}},
#'   \code{\link{predIntNparSimultaneousTestPower}},
#'   \code{\link{predIntNpar}}, \code{\link{tolIntNpar}}.
#' }
#' @rawRd
#' \examples{
#'   # For the 1-of-2 rule, look at how the required sample size for a one-sided
#'   # upper simultaneous nonparametric prediction interval for r=20 future
#'   # sampling occasions increases with increasing confidence level:
#'
#'   seq(0.5, 0.9, by = 0.1)
#'   #[1] 0.5 0.6 0.7 0.8 0.9
#'
#'   predIntNparSimultaneousN(r = 20, conf.level = seq(0.5, 0.9, by = 0.1))
#'   #[1]  4  5  7 10 17
#'
#'   #----------
#'
#'   # For the 1-of-m rule, look at how the required sample size for a one-sided
#'   # upper simultaneous nonparametric prediction interval decreases with increasing
#'   # number of future observations (m), given r=20 future sampling occasions:
#'
#'   predIntNparSimultaneousN(k = 1, m = 1:5, r = 20)
#'   #[1] 380  26  11   7   5
#'
#'   #----------
#'
#'   # For the 1-of-3 rule, look at how the required sample size for a one-sided
#'   # upper simultaneous nonparametric prediction interval increases with number
#'   # of future sampling occasions (r):
#'
#'   predIntNparSimultaneousN(k = 1, m = 3, r = c(5, 10, 15, 20))
#'   #[1]  7  8 10 11
#'
#'   #----------
#'
#'   # For the 1-of-3 rule, look at how the required sample size for a one-sided
#'   # upper simultaneous nonparametric prediction interval increases as the rank
#'   # of the upper prediction limit decreases, given r=20 future sampling occasions:
#'
#'   predIntNparSimultaneousN(k = 1, m = 3, r = 20, n.plus.one.minus.upl.rank = 1:5)
#'   #[1] 11 19 26 34 41
#'
#'   #----------
#'
#'   # Compare the required sample size for r=20 future sampling occasions based
#'   # on the 1-of-3 rule, the CA rule with m=3, and the Modified CA rule.
#'
#'   predIntNparSimultaneousN(k = 1, m = 3, r = 20, rule = "k.of.m")
#'   #[1] 11
#'
#'   predIntNparSimultaneousN(m = 3, r = 20, rule = "CA")
#'   #[1] 36
#'
#'   predIntNparSimultaneousN(r = 20, rule = "Modified.CA")
#'   #[1] 15
#'
#'   #==========
#'
#'   # Example 19-5 of USEPA (2009, p. 19-33) shows how to compute nonparametric upper
#'   # simultaneous prediction limits for various rules based on trace mercury data (ppb)
#'   # collected in the past year from a site with four background wells and 10 compliance
#'   # wells (data for two of the compliance wells  are shown in the guidance document).
#'   # The facility must monitor the 10 compliance wells for five constituents
#'   # (including mercury) annually.
#'
#'   # Here we will modify the example to compute the required number of background
#'   # observations for two different sampling plans:
#'   # 1) the 1-of-2 retesting plan for a median of order 3 using the background maximum and
#'   # 2) the 1-of-4 plan on individual observations using the 3rd highest background value.
#'   # The data for this example are stored in EPA.09.Ex.19.5.mercury.df.
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
#'   # nw = 10 Compliance Wells
#'   # nc =  5 Constituents
#'   # ne =  1 Evaluation per year
#'
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
#'
#'   # Now determine the required number of background observations for  each plan.
#'
#'   # 1) the 1-of-2 retesting plan for a median of order 3 using the
#'   #    background maximum
#'
#'   predIntNparSimultaneousN(n.median = 3, k = 1, m = 2, r = r,
#'     conf.level = conf.level)
#'   #[1] 14
#'
#'
#'   # 2) the 1-of-4 plan on individual observations using the 3rd highest
#'   #    background value.
#'
#'   predIntNparSimultaneousN(k = 1, m = 4, r = r,
#'     n.plus.one.minus.upl.rank = 3, conf.level = conf.level)
#'   #[1] 18
#'
#'   #==========
#'
#'   # Cleanup
#'   #--------
#'   rm(nw, nc, ne, r, conf.level)
#' }
#' @rawRd
#' \keyword{ distribution }
#' @rawRd
#' \keyword{ design }
#' @rawRd
#' \keyword{ htest }

predIntNparSimultaneousN <-
function (n.median = 1, k = 1, m = 2, r = 1, rule = "k.of.m", 
    lpl.rank = ifelse(pi.type == "upper", 0, 1), n.plus.one.minus.upl.rank = ifelse(pi.type == 
        "lower", 0, 1), pi.type = "upper", conf.level = 0.95, 
    n.max = 5000, integrate.args.list = NULL, maxiter = 1000) 
{
    rule <- match.arg(rule, c("k.of.m", "CA", "Modified.CA"), 
        several.ok = TRUE)
    if (!is.vector(n.median, mode = "numeric") || length(n.median) != 
        1 || !is.finite(n.median) || n.median != trunc(n.median) || 
        n.median < 1 || !is.odd(n.median)) 
        stop("'n.median' must be a positive odd integer")
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
    if (!is.vector(r, mode = "numeric") || !all(is.finite(r)) || 
        any(r < 1)) 
        stop(paste("'r' must be a numeric vector", "with all elements greater than or equal to 1", 
            "and no Missing (NA), Infinite (-Inf, Inf),", "or Undefined (Nan) values."))
    if (!is.vector(conf.level, mode = "numeric") || !all(is.finite(conf.level)) || 
        any(conf.level <= .Machine$double.eps) || any(conf.level >= 
        1 - .Machine$double.eps)) 
        stop(paste("'conf.level' must be a numeric vector", "with all elements between 0 and 1", 
            "and no Missing (NA), Infinite(-Inf, Inf),", "or Undefined (Nan) values."))
    if (!is.vector(n.max, mode = "numeric") || length(n.max) != 
        1 || !is.finite(n.max) || n.max != trunc(n.max) || n.max < 
        2) 
        stop("'n.max' must be a positive integer greater than 1")
    if (!is.vector(maxiter, mode = "numeric") || length(maxiter) != 
        1 || !is.finite(maxiter) || maxiter != trunc(maxiter) || 
        maxiter < 2) 
        stop("'maxiter' must be a positive integer greater than 1")
    pi.type <- match.arg(pi.type, c("upper", "lower"))
    if (pi.type == "upper") 
        lpl.rank <- 0
    else n.plus.one.minus.upl.rank <- 0
    if (!is.vector(lpl.rank, mode = "numeric") || !all(is.finite(lpl.rank)) || 
        any(lpl.rank != trunc(lpl.rank)) || any(lpl.rank < 0 | 
        lpl.rank >= n.max)) 
        stop(paste("'lpl.rank' must be a vector of non-negative integers", 
            "with all elements less than 'n.max'"))
    if (pi.type == "lower" & any(lpl.rank < 1)) 
        stop("When pi.type='lower', all values of 'lpl.rank' must be positive integers")
    if (!is.vector(n.plus.one.minus.upl.rank, mode = "numeric") || 
        !all(is.finite(n.plus.one.minus.upl.rank)) || any(n.plus.one.minus.upl.rank != 
        trunc(n.plus.one.minus.upl.rank)) || any(n.plus.one.minus.upl.rank < 
        0 | n.plus.one.minus.upl.rank >= n.max)) 
        stop(paste("'n.plus.one.minus.upl.rank' must be a vector of non-negative", 
            "integers less than 'n.max'"))
    if (pi.type == "upper" & any(n.plus.one.minus.upl.rank < 
        1)) 
        stop("When pi.type='upper' all values of 'n.plus.one.minus.upl.rank' must be positive integers")
    arg.mat <- cbind.no.warn(n.median = as.vector(n.median), 
        k = as.vector(k), m = as.vector(m), r = as.vector(r), 
        lpl.rank = as.vector(lpl.rank), n.plus.one.minus.upl.rank = as.vector(n.plus.one.minus.upl.rank), 
        conf.level = as.vector(conf.level))
    nrow.arg.mat <- nrow(arg.mat)
    length.rule <- length(rule)
    if (length.rule > nrow.arg.mat) 
        arg.mat <- arg.mat[rep(1:nrow.arg.mat, length.out = length.rule), 
            ]
    else rule <- rep(rule, length.out = nrow.arg.mat)
    for (i in c("n.median", "k", "m", "r", "lpl.rank", "n.plus.one.minus.upl.rank", 
        "conf.level")) assign(i, arg.mat[, i])
    index <- rule == "k.of.m"
    if (any(index)) {
        if (any(k[index] > m[index])) 
            stop(paste("For cases where rule='k.of.m',", "all elements of 'k' must be less than or equal to", 
                "the corresponding elements of 'm'"))
        if (any(k[index] > 1)) 
            stop(paste("Determining sample size for the k-of-m rule", 
                "with k > 1 is not yet implemented,", "so currently all values of 'k' must be equal to 1"))
    }
    index <- rule == "Modified.CA"
    m[index] <- 4
    N <- length(k)
    n <- numeric(N)
    for (i in 1:N) {
        n[i] <- predIntNparSimultaneousNScalar(n.median = n.median[i], 
            k = k[i], m = m[i], r = r[i], rule = rule[i], lpl.rank = lpl.rank[i], 
            n.plus.one.minus.upl.rank = n.plus.one.minus.upl.rank[i], 
            pi.type = pi.type, conf.level = conf.level[i], n.max = n.max, 
            integrate.args.list = integrate.args.list, maxiter = maxiter)
    }
    n
}
