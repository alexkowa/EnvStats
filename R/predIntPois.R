#' Prediction Interval for a Poisson Distribution
#' @description
#' Estimate the mean of a \code{\link[stats:Poisson]{Poisson distribution}}, and
#'   construct a prediction interval for the next \eqn{k} observations or
#'   next set of \eqn{k} sums.
#' @usage
#' predIntPois(x, k = 1, n.sum = 1, method = "conditional",
#'     pi.type = "two-sided", conf.level = 0.95, round.limits = TRUE)
#' @rawRd
#' \arguments{
#'   \item{x}{
#'   numeric vector of observations, or an object resulting from a call to an
#'   estimating function that assumes a Poisson distribution
#'   (i.e., \code{\link{epois}} or \code{\link{epoisCensored}}).
#'   If \code{x} is a numeric vector,
#'   missing (\code{NA}), undefined (\code{NaN}), and
#'   infinite (\code{Inf}, \code{-Inf}) values are allowed but will be removed.
#' }
#'   \item{k}{
#'   positive integer specifying the number of future observations or sums the
#'   prediction interval should contain with confidence level \code{conf.level}.
#'   The default value is \code{k=1}.
#' }
#'   \item{n.sum}{
#'   positive integer specifying the sample size associated with the \eqn{k} future
#'   sums.  The default value is \code{n.sum=1} (i.e., individual observations).
#'   Note that all future sums must be based on the same sample size.
#' }
#'   \item{method}{
#'   character string specifying the method to use.  The possible values are: \cr
#'   \code{"conditional"} (based on a conditional distribution; the default), \cr
#'   \code{"conditional.approx.normal"} (method based on approximating a conditional
#'   distribution with the standard normal distribution), \cr
#'   \code{"conditional.approx.t"} (method based on approximating a conditional
#'   distribution with Student's t-distribution), and \cr
#'   \code{"normal.approx"} (approximate method based on the fact that the
#'   mean and varaince of a Poisson distribution are the same). \cr
#'
#'   See the DETAILS section for more information on these methods.  The \cr
#'   \code{"conditional"} method
#'   is only implemented for \code{k=1}; when \code{k} is bigger than 1, the value of
#'   \code{method} cannot be \code{"conditional"}.
#' }
#'   \item{pi.type}{
#'   character string indicating what kind of prediction interval to compute.
#'   The possible values are \code{pi.type="two-sided"} (the default),
#'   \code{pi.type="lower"}, and \code{pi.type="upper"}.
#' }
#'   \item{conf.level}{
#'   a scalar between 0 and 1 indicating the confidence level of the prediction interval.
#'   The default value is \code{conf.level=0.95}.
#' }
#'   \item{round.limits}{
#'   logical scalar indicating whether to round the computed prediction limits to the
#'   nearest integer.  The default value is \code{round.limits=TRUE}.
#' }
#' }
#' @details
#' This help page has been shortened to keep function help focused on usage,
#' arguments, return values, and examples. Extended method details, formulas,
#' and background material are available in \code{vignette("extended-function-details", package = "EnvStats")}, section \code{predIntPois}.
#' @rawRd
#' \value{
#'   If \code{x} is a numeric vector, \code{predIntPois} returns a list of class
#'   \code{"estimate"} containing the estimated parameter, the prediction interval,
#'   and other information.  See the help file for \cr
#'   \code{\link{estimate.object}} for details.
#'
#'   If \code{x} is the result of calling an estimation function,
#'   \code{predIntPois} returns a list whose class is the same as \code{x}.
#'   The list contains the same components as \code{x}, as well as a component called
#'   \code{interval} containing the prediction interval information.
#'   If \code{x} already has a component called \code{interval}, this component is
#'   replaced with the prediction interval information.
#' }
#' @rawRd
#' \references{
#'   Cox, D.R., and D.V. Hinkley. (1974).  \emph{Theoretical Statistics}.
#'   Chapman and Hall, New York, pp.242--245.
#'
#'   Gibbons, R.D. (1987b).  Statistical Models for the Analysis of Volatile Organic
#'   Compounds in Waste Disposal Sites.  \emph{Ground Water} \bold{25}, 572--580.
#'
#'   Gibbons, R.D., D.K. Bhaumik, and S. Aryal. (2009).
#'   \emph{Statistical Methods for Groundwater Monitoring}, Second Edition.
#'   John Wiley & Sons, Hoboken, pp. 72--76.
#'
#'   Hahn, G.J., and W.Q. Meeker. (1991). \emph{Statistical Intervals: A Guide for Practitioners}.
#'   John Wiley and Sons, New York.
#'
#'   Hahn, G., and W. Nelson. (1973).  A Survey of Prediction Intervals and Their
#'   Applications.  \emph{Journal of Quality Technology} \bold{5}, 178--188.
#'
#'   Johnson, N. L., S. Kotz, and A. Kemp. (1992).  \emph{Univariate Discrete
#'   Distributions}.  Second Edition.  John Wiley and Sons, New York, Chapter 4.
#'
#'   Millard, S.P., and N.K. Neerchal. (2001). \emph{Environmental Statistics with S-PLUS}.
#'   CRC Press, Boca Raton.
#'
#'   Miller, R.G. (1981a).  \emph{Simultaneous Statistical Inference}.
#'   McGraw-Hill, New York, pp.8, 76--81.
#'
#'   Nelson, W.R. (1970).  Confidence Intervals for the Ratio of Two Poisson Means and
#'   Poisson Predictor Intervals.  \emph{IEEE Transactions of Reliability} \bold{R-19},
#'   42--49.
#'
#'   Nelson, W.R. (1982).  \emph{Applied Life Data Analysis}.  John Wiley and Sons,
#'   New York, pp.200--204.
#'
#'   Zar, J.H. (2010). \emph{Biostatistical Analysis}.
#'   Fifth Edition. Prentice-Hall, Upper Saddle River, NJ, pp. 585--586.
#' }
#' @rawRd
#' \author{
#'     Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \note{
#'   Prediction and tolerance intervals have long been applied to quality control and
#'   life testing problems.  Nelson (1970) notes that his development of confidence and
#'   prediction limits for the Poisson distribution is based on well-known results
#'   dating back to the 1950's.  Hahn and Nelson (1973) review predicion intervals for
#'   several distributions, including Poisson prediction intervals.  The mongraph by
#'   Hahn and Meeker (1991) includes a discussion of Poisson prediction intervals.
#'
#'   Gibbons (1987b) uses the Poisson distribution to model the number of detected
#'   compounds per scan of the 32 volatile organic priority pollutants (VOC), and also
#'   to model the distribution of chemical concentration (in ppb), and presents formulas
#'   for prediction and tolerance intervals.  The formulas for prediction intervals are
#'   based on Cox and Hinkley (1974, p.245).  Gibbons (1987b) only deals with
#'   the case where \code{n.sum=1}.
#'
#'   Gibbons et al. (2009, pp. 72--76) discuss methods for Poisson prediction limits.
#' }
#' @rawRd
#' \seealso{
#'   \code{\link{Poisson}}, \code{\link{epois}},
#'   \code{\link{estimate.object}}, \link{Prediction Intervals},
#'   \code{\link{tolIntPois}}, \link{Estimating Distribution Parameters}.
#' }
#' @rawRd
#' \examples{
#'   # Generate 20 observations from a Poisson distribution with parameter
#'   # lambda=2.  The interval [0, 4] contains 94.7% of this distribution and
#'   # the interval [0,5] contains 98.3% of this distribution.  Thus, because
#'   # of the discrete nature of the Poisson distribution, no interval contains
#'   # exactly 95% of this distribution.  Use predIntPois to estimate the mean
#'   # parameter of the true distribution, and construct a one-sided upper
#'   # 95% prediction interval for the next single observation from this distribution.
#'   # (Note: the call to set.seed simply allows you to reproduce this example.)
#'
#'   set.seed(250)
#'   dat <- rpois(20, lambda = 2)
#'
#'   predIntPois(dat, pi.type = "upper")
#'
#'   #Results of Distribution Parameter Estimation
#'   #--------------------------------------------
#'   #
#'   #Assumed Distribution:            Poisson
#'   #
#'   #Estimated Parameter(s):          lambda = 1.8
#'   #
#'   #Estimation Method:               mle/mme/mvue
#'   #
#'   #Data:                            dat
#'   #
#'   #Sample Size:                     20
#'   #
#'   #Prediction Interval Method:      conditional
#'   #
#'   #Prediction Interval Type:        upper
#'   #
#'   #Confidence Level:                95%
#'   #
#'   #Number of Future Observations:   1
#'   #
#'   #Prediction Interval:             LPL = 0
#'   #                                 UPL = 5
#'
#'   #----------
#'
#'   # Compare results above with the other approximation methods:
#'
#'   predIntPois(dat, method = "conditional.approx.normal",
#'     pi.type = "upper")$interval$limits
#'   #LPL UPL
#'   #  0   4
#'
#'
#'   predIntPois(dat, method = "conditional.approx.t",
#'     pi.type = "upper")$interval$limits
#'   #LPL UPL
#'   #  0   4
#'
#'
#'   predIntPois(dat, method = "normal.approx",
#'     pi.type = "upper")$interval$limits
#'   #LPL UPL
#'   #  0   4
#'   #Warning message:
#'   #In predIntPois(dat, method = "normal.approx", pi.type = "upper") :
#'   #  Estimated value of 'lambda' and/or number of future observations
#'   #  is/are probably too small for the normal approximation to work well.
#'
#'   #==========
#'
#'   # Using the same data as in the previous example, compute a one-sided
#'   # upper 95% prediction limit for k=10 future observations.
#'
#'   # Using conditional approximation method based on the normal distribution.
#'
#'   predIntPois(dat, k = 10, method = "conditional.approx.normal",
#'     pi.type = "upper")
#'
#'   #Results of Distribution Parameter Estimation
#'   #--------------------------------------------
#'   #
#'   #Assumed Distribution:            Poisson
#'   #
#'   #Estimated Parameter(s):          lambda = 1.8
#'   #
#'   #Estimation Method:               mle/mme/mvue
#'   #
#'   #Data:                            dat
#'   #
#'   #Sample Size:                     20
#'   #
#'   #Prediction Interval Method:      conditional.approx.normal
#'   #
#'   #Prediction Interval Type:        upper
#'   #
#'   #Confidence Level:                95%
#'   #
#'   #Number of Future Observations:   10
#'   #
#'   #Prediction Interval:             LPL = 0
#'   #                                 UPL = 6
#'
#'
#'   # Using method based on approximating conditional distribution with
#'   # Student's t-distribution
#'
#'   predIntPois(dat, k = 10, method = "conditional.approx.t",
#'     pi.type = "upper")$interval$limits
#'   #LPL UPL
#'   #  0   6
#'
#'   #==========
#'
#'   # Repeat the above example, but set k=5 and n.sum=3.  Thus, we want a
#'   # 95% upper prediction limit for the next 5 sets of sums of 3 observations.
#'
#'   predIntPois(dat, k = 5, n.sum = 3, method = "conditional.approx.t",
#'     pi.type = "upper")$interval$limits
#'   #LPL UPL
#'   #  0  12
#'
#'   #==========
#'
#'   # Reproduce Example 3.6 in Gibbons et al. (2009, p. 75)
#'   # A 32-constituent VOC scan was performed for n=16 upgradient
#'   # samples and there were 5 detections out of these 16.  We
#'   # want to construct a one-sided upper 95% prediction limit
#'   # for 20 monitoring wells (so k=20 future observations) based
#'   # on these data.
#'
#'   # First we need to create a data set that will yield a mean
#'   # of 5/16 based on a sample size of 16.  Any number of data
#'   # sets will do.  Here are two possible ones:
#'
#'   dat <- c(rep(1, 5), rep(0, 11))
#'   dat <- c(2, rep(1, 3), rep(0, 12))
#'
#'   # Now call predIntPois.  Don't round the limits so we can
#'   # compare to the example in Gibbons et al. (2009).
#'
#'   predIntPois(dat, k = 20, method = "conditional.approx.t",
#'     pi.type = "upper", round.limits = FALSE)
#'
#'   #Results of Distribution Parameter Estimation
#'   #--------------------------------------------
#'   #
#'   #Assumed Distribution:            Poisson
#'   #
#'   #Estimated Parameter(s):          lambda = 0.3125
#'   #
#'   #Estimation Method:               mle/mme/mvue
#'   #
#'   #Data:                            dat
#'   #
#'   #Sample Size:                     16
#'   #
#'   #Prediction Interval Method:      conditional.approx.t
#'   #
#'   #Prediction Interval Type:        upper
#'   #
#'   #Confidence Level:                95%
#'   #
#'   #Number of Future Observations:   20
#'   #
#'   #Prediction Interval:             LPL = 0.000000
#'   #                                 UPL = 2.573258
#'
#'   #==========
#'
#'   # Cleanup
#'   #--------
#'   rm(dat)
#' }
#' @rawRd
#' \keyword{ distribution }
#' @rawRd
#' \keyword{ htest }

predIntPois <-
function (x, k = 1, n.sum = 1, method = "conditional", pi.type = "two-sided", 
    conf.level = 0.95, round.limits = TRUE) 
{
    if (length(k) != 1 || !is.numeric(k) || k != trunc(k) || 
        k < 1 || length(n.sum) != 1 || !is.numeric(n.sum) || 
        n.sum != trunc(n.sum) || n.sum < 1) 
        stop("'k' and 'n.sum' must be a positive integers")
    method <- match.arg(method, c("conditional", "conditional.approx.normal", 
        "conditional.approx.t", "normal.approx"))
    if (method == "conditional" && k > 1) 
        stop("The 'conditonal' method is only implemented for 'k'=1")
    pi.type <- match.arg(pi.type, c("two-sided", "lower", "upper"))
    if (!is.numeric(conf.level) || length(conf.level) > 1 || 
        conf.level <= 0 || conf.level >= 1) 
        stop("'conf.level' must be a scalar greater than 0 and less than 1.")
    if (x.is.est.obj <- data.class(x) == "estimate" || data.class(x) == 
        "estimateCensored") {
        if (x$distribution != "Poisson") 
            stop(paste("'predIntPois' creates prediction intervals", 
                "for a Poisson distribution.  You have supplied an object", 
                "that assumes a different distribution."))
        class.x <- oldClass(x)
        if (!is.null(x$interval)) {
            x <- x[-match("interval", names(x))]
            oldClass(x) <- class.x
        }
        lambda.hat <- x$parameters
        n <- x$sample.size
        ret.list <- x
    }
    else {
        if (!is.vector(x, mode = "numeric")) 
            stop(paste("'x' must be either a list that inherits from", 
                "the class 'estimate', or else a numeric vector"))
        data.name <- deparse(substitute(x))
        if ((bad.obs <- sum(!(x.ok <- is.finite(x)))) > 0) {
            is.not.finite.warning(x)
            x <- x[x.ok]
            warning(paste(bad.obs, "observations with NA/NaN/Inf in 'x' removed."))
        }
        n <- length(x)
        if (n == 0) 
            stop("'x' does not contain any finite, non-missing values")
        if (any(x < 0) || any(x != trunc(x))) 
            stop("All non-missing values of 'x' must be non-negative integers")
        if (all(x == 0)) 
            stop("All finite, non-missing values of 'x' are 0")
        ret.list <- epois(x)
        ret.list$data.name <- data.name
        ret.list$bad.obs <- bad.obs
        lambda.hat <- ret.list$parameters
    }
    sum.x <- n * lambda.hat
    alpha <- 1 - conf.level
    x.hat <- n.sum * lambda.hat
    switch(method, conditional = {
        limits <- ci.normal.approx(theta.hat = x.hat, sd.theta.hat = sqrt((x.hat * 
            (n + n.sum))/n), n = n, df = n - 1, ci.type = pi.type, 
            alpha = alpha, lb = 0)$limits
        lpl <- limits[1]
        upl <- limits[2]
        switch(pi.type, `two-sided` = {
            if (ebinom(0, sum.x, ci = TRUE, ci.type = "two-sided", 
                conf.level = 1 - alpha/2)$interval$limits["UCL"] >= 
                (n.sum/(n.sum + n))) {
                lpl <- 0
                warning(paste("Lower prediction limit not accurate", 
                  "due to discrete nature of Poisson distribution"))
            } else {
                fcn.to.min.two.lpl <- function(lpl, n.weird, 
                  m.weird, y.weird, alpha) (m.weird/(lpl + 1) - 
                  (n.weird/y.weird) * qf(1 - alpha/2, 2 * (lpl + 
                    1), 2 * y.weird))^2
                lpl <- nlminb(start = lpl, objective = fcn.to.min.two.lpl, 
                  lower = 0, n.weird = n, m.weird = n.sum, y.weird = sum.x, 
                  alpha = alpha)$par
                fcn.to.min.two.upl <- function(upl, n.weird, 
                  m.weird, y.weird, alpha) (upl/m.weird - ((y.weird + 
                  1)/n.weird) * qf(1 - alpha/2, 2 * (y.weird + 
                  1), 2 * upl))^2
                upl <- nlminb(start = max(upl, 1), objective = fcn.to.min.two.upl, 
                  lower = 1, n.weird = n, m.weird = n.sum, y.weird = sum.x, 
                  alpha = alpha)$par
            }
        }, lower = {
            if (ebinom(0, sum.x, ci = TRUE, ci.type = "upper", 
                conf.level = conf.level)$interval$limits["UCL"] >= 
                (n.sum/(n.sum + n))) {
                lpl <- 0
                warning(paste("Lower prediction limit not accurate", 
                  "due to discrete nature of Poisson distribution"))
            } else {
                fcn.to.min.one.lpl <- function(lpl, n.weird, 
                  m.weird, y.weird, conf.level) (m.weird/(lpl + 
                  1) - n.weird/y.weird * qf(conf.level, 2 * (lpl + 
                  1), 2 * y.weird))^2
                lpl <- nlminb(start = lpl, objective = fcn.to.min.one.lpl, 
                  lower = 0, n.weird = n, m.weird = n.sum, y.weird = sum.x, 
                  conf.level = conf.level)$par
            }
        }, upper = {
            fcn.to.min.one.upl <- function(upl, n.weird, m.weird, 
                y.weird, conf.level) (upl/m.weird - (y.weird + 
                1)/n.weird * qf(conf.level, 2 * (y.weird + 1), 
                2 * upl))^2
            upl <- nlminb(start = upl, objective = fcn.to.min.one.upl, 
                lower = 0, n.weird = n, m.weird = n.sum, y.weird = sum.x, 
                conf.level = conf.level)$par
        })
        limits <- c(lpl, upl)
    }, conditional.approx.normal = {
        cf <- n.sum/n
        z.crit <- ifelse(pi.type == "two-sided", qnorm(1 - (alpha/k)/2), 
            qnorm(1 - alpha/k))
        hw <- (cf * z.crit^2)/2 + cf * z.crit * sqrt(sum.x * 
            (1 + 1/cf) + z.crit^2/4)
        switch(pi.type, `two-sided` = {
            lpl <- x.hat - hw
            upl <- x.hat + hw
            lpl <- max(0, lpl)
        }, lower = {
            lpl <- x.hat - hw
            lpl <- max(0, lpl)
            upl <- Inf
        }, upper = {
            lpl <- 0
            upl <- x.hat + hw
        })
        limits <- c(lpl, upl)
    }, conditional.approx.t = {
        df <- n - 1
        cf <- n.sum/n
        t.crit <- ifelse(pi.type == "two-sided", qt(1 - (alpha/k)/2, 
            df), qt(1 - alpha/k, df))
        hw <- (cf * t.crit^2)/2 + cf * t.crit * sqrt(sum.x * 
            (1 + 1/cf) + t.crit^2/4)
        switch(pi.type, `two-sided` = {
            lpl <- x.hat - hw
            upl <- x.hat + hw
            lpl <- max(0, lpl)
        }, lower = {
            lpl <- x.hat - hw
            lpl <- max(0, lpl)
            upl <- Inf
        }, upper = {
            lpl <- 0
            upl <- x.hat + hw
        })
        limits <- c(lpl, upl)
    }, normal.approx = {
        if (sum.x <= 10 || x.hat <= 10) warning(paste("Estimated value of 'lambda' and/or", 
            "number of future observations is/are probably too small", 
            "for the normal approximation to work well.\n"))
        limits <- ci.normal.approx(theta.hat = x.hat, sd.theta.hat = sqrt((x.hat * 
            (n + n.sum))/n), n = n, df = n - 1, ci.type = pi.type, 
            alpha = alpha/k, lb = 0)$limits
    })
    if (round.limits) 
        limits <- round(limits, 0)
    names(limits) <- c("LPL", "UPL")
    pi.obj <- list(name = "Prediction", limits = limits, type = pi.type, 
        method = method, conf.level = conf.level, sample.size = n, 
        k = k, n.sum = n.sum)
    oldClass(pi.obj) <- "intervalEstimate"
    ret.list <- c(ret.list, list(interval = pi.obj))
    if (x.is.est.obj) 
        oldClass(ret.list) <- class.x
    else oldClass(ret.list) <- "estimate"
    ret.list
}
