#' Estimate Parameters of a Three-Parameter Lognormal Distribution (Log-Scale)
#' @description
#' Estimate the mean, standard deviation, and threshold parameters for a
#'   \link[=Lognormal3]{three-parameter lognormal distribution}, and optionally
#'   construct a confidence interval for the threshold or the median of the distribution.
#' @usage
#' elnorm3(x, method = "lmle", ci = FALSE, ci.parameter = "threshold",
#'     ci.method = "avar", ci.type = "two-sided", conf.level = 0.95,
#'     threshold.lb.sd = 100, evNormOrdStats.method = "royston")
#' @rawRd
#' \arguments{
#'   \item{x}{
#'   numeric vector of observations.
#' }
#'   \item{method}{
#'   character string specifying the method of estimation.  Possible values are:
#'   \itemize{
#'     \item \code{"lmle"} (local maximum likelihood; the default)
#'     \item \code{"mme"} (method of moments)
#'     \item \code{"mmue"} (method of moments using an unbaised estimate of variance)
#'     \item \code{"mmme"} (modified method of moments due to Cohen and Whitten (1980))
#'     \item \code{"zero.skew"} (zero-skewness estimator due to Griffiths (1980))
#'     \item \code{"royston.skew"} (estimator based on Royston's (1992b) index of skewness).
#'   }
#'   See the DETAILS section for more information.
#' }
#'   \item{ci}{
#'   logical scalar indicating whether to compute a confidence interval for either
#'   the threshold or median of the distribution.  The default value is \code{FALSE}.
#' }
#'   \item{ci.parameter}{
#'   character string indicating the parameter for which the confidence interval is
#'   desired.  The possible values are \code{"threshold"} (the default) and
#'   \code{"median"}.  This argument is ignored if \code{ci=FALSE}.
#' }
#'   \item{ci.method}{
#'   character string indicating the method to use to construct the confidence interval.
#'   The possible values are \code{"avar"} (asymptotic variance; the default), \cr
#'   \code{"likelihood.profile"}, and \code{"skewness"} (method suggested by Royston
#'   (1992b) for \code{method="zero.skew"}).  This argument is ignored if \code{ci=FALSE}.
#' }
#'   \item{ci.type}{
#'   character string indicating what kind of confidence interval to compute.  The
#'   possible values are \code{"two-sided"} (the default), \code{"lower"}, and
#'   \code{"upper"}.  This argument is ignored if \code{ci=FALSE}.
#' }
#'   \item{conf.level}{
#'   a scalar between 0 and 1 indicating the confidence level of the confidence interval.
#'   The default value is \code{conf.level=0.95}. This argument is ignored if
#'   \code{ci=FALSE}.
#' }
#'   \item{threshold.lb.sd}{
#'   a positive numeric scalar specifying the range over which to look for the
#'   local maximum likelihood (\code{method="lmle"}) or zero-skewness \cr
#'   (\code{method="zero.skewness"}) estimator of threshold.  The range is set to \cr
#'   \code{[ mean(x) - threshold.lb.sd * sd(x), min(x) ]}.  If you receive a warning
#'   message that \code{elnorm3} is unable to find an acceptable estimate of threshold
#'   in this range, it may be because of convergence problems specific to the data in
#'   \code{x}.  When this occurs, try changing the value of \code{threshold.lb.sd}.  This
#'   same range is used in constructing confidence intervals for the threshold parameter.
#'   The default value is \code{threshold.lb.sd=100}.  This argument is relevant only if
#'   \code{method="lmle"}, \code{method="zero.skew"},
#'   \code{ci.method="likelihood.profile"}, and/or \code{ci.method="skewness"}.
#' }
#'   \item{evNormOrdStats.method}{
#'   character string indicating which method to use in the call to
#'   \code{link{evNormOrdStatsScalar}} when \code{method="mmme"}.  See the DETAILS
#'   section for more information.
#' }
#' }
#' @details
#' This help page has been shortened to keep function help focused on usage,
#' arguments, return values, and examples. Extended method details, formulas,
#' and background material are available in \code{vignette("extended-function-details", package = "EnvStats")}, section \code{elnorm3}.
#' @rawRd
#' \value{
#'   a list of class \code{"estimate"} containing the estimated parameters and other information.
#'   See \cr
#'   \code{\link{estimate.object}} for details.
#' }
#' @rawRd
#' \references{
#'   Aitchison, J., and J.A.C. Brown (1957).  \emph{The Lognormal Distribution
#'   (with special references to its uses in economics)}.  Cambridge University Press,
#'   London, Chapter 5.
#'
#'   Calitz, F. (1973).  Maximum Likelihood Estimation of the Parameters of the
#'   Three-Parameter Lognormal Distribution--a Reconsideration.  \emph{Australian
#'   Journal of Statistics} \bold{15}(3), 185--190.
#'
#'   Cohen, A.C. (1951).  Estimating Parameters of Logarithmic-Normal Distributions by
#'   Maximum Likelihood.  \emph{Journal of the American Statistical Association}
#'   \bold{46}, 206--212.
#'
#'   Cohen, A.C. (1988).  Three-Parameter Estimation.  In Crow, E.L., and K. Shimizu, eds.
#'   \emph{Lognormal Distributions: Theory and Applications}.  Marcel Dekker, New York,
#'   Chapter 4.
#'
#'   Cohen, A.C., and B.J. Whitten. (1980).  Estimation in the Three-Parameter Lognormal
#'   Distribution.  \emph{Journal of the American Statistical Association} \bold{75},
#'   399--404.
#'
#'   Cohen, A.C., B.J. Whitten, and Y. Ding. (1985).  Modified Moment Estimation for the
#'   Three-Parameter Lognormal Distribution.  \emph{Journal of Quality Technology}
#'   \bold{17}, 92--99.
#'
#'   Crow, E.L., and K. Shimizu. (1988).  \emph{Lognormal Distributions: Theory and
#'   Applications}.  Marcel Dekker, New York, Chapter 2.
#'
#'   Griffiths, D.A. (1980).  Interval Estimation for the Three-Parameter Lognormal
#'   Distribution via the Likelihood Function.  \emph{Applied Statistics} \bold{29},
#'   58--68.
#'
#'   Harter, H.L., and A.H. Moore. (1966).  Local-Maximum-Likelihood Estimation of the
#'   Parameters of Three-Parameter Lognormal Populations from Complete and Censored
#'   Samples.  \emph{Journal of the American Statistical Association} \bold{61}, 842--851.
#'
#'   Heyde, C.C. (1963).  On a Property of the Lognormal Distribution.  \emph{Journal of
#'   the Royal Statistical Society, Series B} \bold{25}, 392--393.
#'
#'   Hill, .B.M. (1963).  The Three-Parameter Lognormal Distribution and Bayesian
#'   Analysis of a Point-Source Epidemic.  \emph{Journal of the American Statistical
#'   Association} \bold{58}, 72--84.
#'
#'   Hoshi, K., J.R. Stedinger, and J. Burges. (1984).  Estimation of Log-Normal
#'   Quantiles: Monte Carlo Results and First-Order Approximations.  \emph{Journal of
#'   Hydrology} \bold{71}, 1--30.
#'
#'   Johnson, N. L., S. Kotz, and N. Balakrishnan. (1994).
#'   \emph{Continuous Univariate Distributions, Volume 1}.
#'   Second Edition. John Wiley and Sons, New York.
#'
#'   Royston, J.P. (1992b).  Estimation, Reference Ranges and Goodness of Fit for the
#'   Three-Parameter Log-Normal Distribution.  \emph{Statistics in Medicine} \bold{11},
#'   897--912.
#'
#'   Stedinger, J.R. (1980).  Fitting Lognormal Distributions to Hydrologic Data.
#'   \emph{Water Resources Research} \bold{16}(3), 481--490.
#' }
#' @rawRd
#' \author{
#'     Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \note{
#'   The problem of estimating the parameters of a three-parameter lognormal distribution
#'   has been extensively discussed by Aitchison and Brown (1957, Chapter 6),
#'   Calitz (1973), Cohen (1951), Cohen (1988), Cohen and Whitten (1980),
#'   Cohen et al. (1985), Griffiths (1980), Harter and Moore (1966), Hill (1963), and
#'   Royston (1992b).  Stedinger (1980) and Hoshi et al. (1984) discuss fitting the
#'   three-parameter lognormal distribution to hydrologic data.
#'
#'   The global maximum likelihood estimates are inadmissible.  In the past, several
#'   researchers have found that the local maximum likelihood estimates (lmle's)
#'   occasionally fail because of convergence problems, but they were not using the
#'   likelihood profile and reparameterization of Griffiths (1980).  Cohen (1988)
#'   recommends the modified methods of moments estimators over lmle's because they are
#'   easy to compute, they are unbiased with respect to \eqn{\mu} and \eqn{\sigma^2} (the
#'   mean and standard deviation on the log-scale), their variances are minimal or near
#'   minimal, and they do not suffer from regularity problems.
#'
#'   Because the distribution of the lmle of the threshold parameter \eqn{\gamma} is far
#'   from normal for moderate sample sizes (Griffiths, 1980), it is questionable whether
#'   confidence intervals for \eqn{\gamma} or the median based on asymptotic variances
#'   and covariances will perform well.  Cohen and Whitten (1980) and Cohen et al. (1985),
#'   however, found that the asymptotic variances and covariances are reasonably close to
#'   corresponding simulated variances and covariances for the modified method of moments
#'   estimators (\code{method="mmme"}).  In a simulation study (5000 monte carlo trials),
#'   Royston (1992b) found that the coverage of confidence intervals for \eqn{\gamma}
#'   based on the likelihood profile (\code{ci.method="likelihood.profile"}) was very
#'   close the nominal level (94.1\% for a nominal level of 95\%), although not
#'   symmetric.  Royston (1992b) also found that the coverage of confidence intervals
#'   for \eqn{\gamma} based on the skewness method (\code{ci.method="skewness"}) was also
#'   very close (95.4\%) and symmetric.
#' }
#' @rawRd
#' \seealso{
#'   \link{Lognormal3}, \link[stats]{Lognormal}, \link{LognormalAlt},
#'   \link[stats]{Normal}.
#' }
#' @rawRd
#' \examples{
#'   # Generate 20 observations from a 3-parameter lognormal distribution
#'   # with parameters meanlog=1.5, sdlog=1, and threshold=10, then use
#'   # Cohen and Whitten's (1980) modified moments estimators to estimate
#'   # the parameters, and construct a confidence interval for the
#'   # threshold based on the estimated asymptotic variance.
#'   # (Note: the call to set.seed simply allows you to reproduce this example.)
#'
#'   set.seed(250)
#'   dat <- rlnorm3(20, meanlog = 1.5, sdlog = 1, threshold = 10)
#'   elnorm3(dat, method = "mmme", ci = TRUE)
#'
#'   #Results of Distribution Parameter Estimation
#'   #--------------------------------------------
#'   #
#'   #Assumed Distribution:            3-Parameter Lognormal
#'   #
#'   #Estimated Parameter(s):          meanlog   = 1.5206664
#'   #                                 sdlog     = 0.5330974
#'   #                                 threshold = 9.6620403
#'   #
#'   #Estimation Method:               mmme
#'   #
#'   #Data:                            dat
#'   #
#'   #Sample Size:                     20
#'   #
#'   #Confidence Interval for:         threshold
#'   #
#'   #Confidence Interval Method:      Normal Approximation
#'   #                                 Based on Asymptotic Variance
#'   #
#'   #Confidence Interval Type:        two-sided
#'   #
#'   #Confidence Level:                95%
#'   #
#'   #Confidence Interval:             LCL =  6.985258
#'   #                                 UCL = 12.338823
#'
#'   #----------
#'
#'   # Repeat the above example using the other methods of estimation
#'   # and compare.
#'
#'   round(elnorm3(dat, "lmle")$parameters, 1)
#'   #meanlog     sdlog threshold
#'   #    1.3       0.7      10.5
#'
#'   round(elnorm3(dat, "mme")$parameters, 1)
#'   #meanlog     sdlog threshold
#'   #    2.1       0.3       6.0
#'
#'   round(elnorm3(dat, "mmue")$parameters, 1)
#'   #meanlog     sdlog threshold
#'   #    2.2       0.3       5.8
#'
#'   round(elnorm3(dat, "mmme")$parameters, 1)
#'   #meanlog     sdlog threshold
#'   #    1.5       0.5       9.7
#'
#'   round(elnorm3(dat, "zero.skew")$parameters, 1)
#'   #meanlog     sdlog threshold
#'   #    1.3       0.6      10.3
#'
#'   round(elnorm3(dat, "royston")$parameters, 1)
#'   #meanlog     sdlog threshold
#'   #    1.4       0.6      10.1
#'
#'   #----------
#'
#'   # Compare methods for computing a two-sided 95\% confidence interval
#'   # for the threshold:
#'   # modified method of moments estimator using asymptotic variance,
#'   # lmle using asymptotic variance,
#'   # lmle using likelihood profile, and
#'   # zero-skewness estimator using the skewness method.
#'
#'   elnorm3(dat, method = "mmme", ci = TRUE,
#'     ci.method = "avar")$interval$limits
#'   #      LCL       UCL
#'   # 6.985258 12.338823
#'
#'   elnorm3(dat, method = "lmle", ci = TRUE,
#'     ci.method = "avar")$interval$limits
#'   #       LCL       UCL
#'   #  9.017223 11.980107
#'
#'   elnorm3(dat, method = "lmle", ci = TRUE,
#'     ci.method="likelihood.profile")$interval$limits
#'   #      LCL       UCL
#'   # 3.699989 11.266029
#'
#'
#'   elnorm3(dat, method = "zero.skew", ci = TRUE,
#'     ci.method = "skewness")$interval$limits
#'   #      LCL       UCL
#'   #-25.18851  11.18652
#'
#'   #----------
#'
#'   # Now construct a confidence interval for the median of the distribution
#'   # based on using the modified method of moments estimator for threshold
#'   # and the asymptotic variances and covariances.  Note that the true median
#'   # is given by threshold + exp(meanlog) = 10 + exp(1.5) = 14.48169.
#'
#'   elnorm3(dat, method = "mmme", ci = TRUE, ci.parameter = "median")
#'
#'   #Results of Distribution Parameter Estimation
#'   #--------------------------------------------
#'   #
#'   #Assumed Distribution:            3-Parameter Lognormal
#'   #
#'   #Estimated Parameter(s):          meanlog   = 1.5206664
#'   #                                 sdlog     = 0.5330974
#'   #                                 threshold = 9.6620403
#'   #
#'   #Estimation Method:               mmme
#'   #
#'   #Data:                            dat
#'   #
#'   #Sample Size:                     20
#'   #
#'   #Confidence Interval for:         median
#'   #
#'   #Confidence Interval Method:      Normal Approximation
#'   #                                 Based on Asymptotic Variance
#'   #
#'   #Confidence Interval Type:        two-sided
#'   #
#'   #Confidence Level:                95%
#'   #
#'   #Confidence Interval:             LCL = 11.20541
#'   #                                 UCL = 17.26922
#'
#'   #----------
#'
#'   # Compare methods for computing a two-sided 95\% confidence interval
#'   # for the median:
#'   # modified method of moments estimator using asymptotic variance,
#'   # lmle using asymptotic variance,
#'   # lmle using likelihood profile, and
#'   # zero-skewness estimator using the skewness method.
#'
#'   elnorm3(dat, method = "mmme", ci = TRUE, ci.parameter = "median",
#'     ci.method = "avar")$interval$limits
#'   #     LCL      UCL
#'   #11.20541 17.26922
#'
#'   elnorm3(dat, method = "lmle", ci = TRUE, ci.parameter = "median",
#'     ci.method = "avar")$interval$limits
#'   #     LCL      UCL
#'   #12.28326 15.87233
#'
#'   elnorm3(dat, method = "lmle", ci = TRUE, ci.parameter = "median",
#'     ci.method = "likelihood.profile")$interval$limits
#'   #      LCL       UCL
#'   # 6.314583 16.165525
#'
#'   elnorm3(dat, method = "zero.skew", ci = TRUE, ci.parameter = "median",
#'     ci.method = "skewness")$interval$limits
#'   #      LCL       UCL
#'   #-22.38322  16.33569
#'
#'   #----------
#'
#'   # Clean up
#'   #---------
#'
#'   rm(dat)
#'
#' }
#' @rawRd
#' \keyword{ distribution }
#' @rawRd
#' \keyword{ htest }

elnorm3 <-
function (x, method = "lmle", ci = FALSE, ci.parameter = "threshold", 
    ci.method = "avar", ci.type = "two-sided", conf.level = 0.95, 
    threshold.lb.sd = 100, evNormOrdStats.method = "royston") 
{
    if (!is.vector(x, mode = "numeric")) 
        stop("'x' must be a numeric vector.")
    data.name <- deparse(substitute(x))
    method <- match.arg(method, c("lmle", "mme", "mmue", "mmme", 
        "royston.skew", "zero.skew"))
    if ((bad.obs <- sum(!(x.ok <- is.finite(x)))) > 0) {
        is.not.finite.warning(x)
        x <- x[x.ok]
        warning(paste(bad.obs, "observations with NA/NaN/Inf in 'x' removed."))
    }
    if (ci) {
        ci.parameter <- match.arg(ci.parameter, c("threshold", 
            "median"))
        ci.method <- match.arg(ci.method, c("avar", "likelihood.profile", 
            "skewness"))
        ci.type <- match.arg(ci.type, c("two-sided", "lower", 
            "upper"))
        if (conf.level <= 0 || conf.level >= 1) 
            stop("The value of 'conf.level' must be between 0 and 1.")
        if (ci.method == "likelihood.profile" && method != "lmle") 
            stop(paste("You must set method='lmle' to use", "ci.method='likelihood.profile'"))
        if (ci.method == "skewness" && method != "zero.skew") 
            stop(paste("You must set method='zero.skew' to use", 
                "ci.method='skewness'"))
    }
    if (length(threshold.lb.sd) != 1 || !is.vector(threshold.lb.sd, 
        mode = "numeric") || threshold.lb.sd <= 0) 
        stop("'threshold.lb.sd' must be a positive scalar")
    n <- length(x)
    if (n < 3 || length(unique(x)) < 2) 
        stop("'x' must contain at least 3 non-missing distinct values")
    mean.x <- mean(x)
    var.x <- var(x)
    sd.x <- sqrt(var.x)
    x1 <- min(x)
    cf <- (n - 1)/n
    if (method == "royston.skew") {
        xn <- max(x)
        med.x <- median(x)
        q <- (xn - med.x)/(med.x - x1)
        if (q <= 1) 
            stop("Royston's skewness index indicates left-skewed data")
        threshold <- (x1 * xn - med.x^2)/(x1 + xn - 2 * med.x)
        y <- log(x - threshold)
        meanlog <- mean(y)
        sdlog <- sd(y)
    }
    else {
        m2 <- cf * var.x
        sqrt.b1 <- skewness(x, method = "moment")
        if (sqrt.b1 <= 0) 
            stop(paste("The sample skew is not positive. ", "Admissible moment estimates do not exist"))
        b1 <- sqrt.b1^2
        t1 <- 1 + b1/2
        t2 <- sqrt(t1^2 - 1)
        omega <- (t1 + t2)^(1/3) + (t1 - t2)^(1/3) - 1
        varlog <- log(omega)
        sdlog <- sqrt(varlog)
        meanlog <- 0.5 * log(m2/(omega * (omega - 1)))
        threshold <- mean.x - exp(meanlog + varlog/2)
        if (method != "mme") {
            meanlog <- 0.5 * log(var.x/(omega * (omega - 1)))
            threshold <- mean.x - exp(meanlog + varlog/2)
        }
        if (method != "mme" && method != "mmue") {
            fcn.to.min <- function(omega, s2, x.bar, x1, EZ1.n) {
                (s2/((x.bar - x1)^2) - (omega * (omega - 1))/((sqrt(omega) - 
                  exp(sqrt(log(omega)) * EZ1.n))^2))^2
            }
            nlminb.list <- nlminb(start = omega, objective = fcn.to.min, 
                lower = 1 + sqrt(.Machine$double.eps), s2 = var.x, 
                x.bar = mean.x, x1 = x1, EZ1.n = evNormOrdStatsScalar(r = 1, 
                  n = n, method = evNormOrdStats.method))
            if (nlminb.list$convergence != 0) {
                warning("Unable to solve for 'omega'")
                threshold <- meanlog <- sdlog <- NA
                ci <- FALSE
            }
            else {
                omega <- nlminb.list$par
                varlog <- log(omega)
                sdlog <- sqrt(varlog)
                meanlog <- 0.5 * log(var.x/(omega * (omega - 
                  1)))
                threshold <- mean.x - exp(meanlog + varlog/2)
            }
        }
    }
    switch(method, mme = {
    }, mmue = {
    }, mmme = {
    }, lmle = {
        threshold.lb <- mean.x - threshold.lb.sd * sd.x
        threshold.ub <- x1 - sqrt(.Machine$double.eps)
        eta.lb <- -log(x1 - threshold.lb)
        eta.ub <- -log(x1 - threshold.ub)
        eta <- -log(x1 - threshold)
        neg.2.ll <- function(eta, x.weird, x1, n.weird) {
            threshold <- x1 - exp(-eta)
            y <- log(x.weird - threshold)
            ny <- length(y)
            meanlog <- mean(y)
            sdlog <- sqrt((ny - 1)/ny) * sd(y)
            n.weird * (1 + log(2 * pi) + 2 * meanlog + 2 * log(sdlog))
        }
        nlminb.list <- nlminb(start = eta, objective = neg.2.ll, 
            lower = eta.lb, upper = eta.ub, x.weird = x, x1 = x1, 
            n.weird = n)
        threshold <- x1 - exp(-nlminb.list$par)
        if (threshold == threshold.lb || threshold == threshold.ub) {
            warning(paste("Unable to solve for 'threshold' in the interval", 
                "[ mean(x) -", threshold.lb.sd, "* sd(x), min(x) ). ", 
                "Try changing the value of 'threshold.lb.sd'"))
            threshold <- meanlog <- sdlog <- NA
            ci <- FALSE
        } else {
            fcn.for.root.lmle <- function(threshold, x.weird, 
                n.weird) {
                y <- x.weird - threshold
                ly <- log(y)
                sly <- sum(ly)
                sum(1/y) * (sly - sum(ly^2) + (sly^2)/n.weird) - 
                  n.weird * sum(ly/y)
            }
            con <- 0.25 * abs(threshold)
            threshold <- uniroot(fcn.for.root.lmle, lower = max(threshold - 
                con, threshold.lb), upper = min(threshold + con, 
                mean(c(threshold, threshold.ub))), tol = .Machine$double.eps, 
                x.weird = x, n.weird = n)$root
            y <- log(x - threshold)
            ny <- length(y)
            meanlog <- mean(y)
            sdlog <- sqrt((ny - 1)/ny) * sd(y)
        }
    }, zero.skew = {
        threshold.lb <- mean.x - threshold.lb.sd * sd.x
        threshold.ub <- x1 - sqrt(.Machine$double.eps)
        fcn.for.root.zs <- function(threshold, x.weird) {
            y <- log(x.weird - threshold)
            sum((y - mean(y))^3)
        }
        uniroot.list <- uniroot(fcn.for.root.zs, lower = threshold.lb, 
            upper = threshold.ub, tol = .Machine$double.eps, 
            x.weird = x)
        threshold <- uniroot.list$root
        if (uniroot.list$f.root > sqrt(.Machine$double.eps) || 
            threshold == threshold.lb || threshold == threshold.ub) {
            warning(paste("Unable to solve for 'threshold' in the interval", 
                "[ mean(x) -", threshold.lb.sd, "* sd(x), min(x) ). ", 
                "Try changing the value of threshold.lb.sd"))
            threshold <- meanlog <- sdlog <- NA
            ci <- FALSE
        } else {
            y <- log(x - threshold)
            meanlog <- mean(y)
            sdlog <- sd(y)
        }
    }, royston.skew = {
    })
    ret.list <- list(distribution = "3-Parameter Lognormal", 
        sample.size = n, parameters = c(meanlog = meanlog, sdlog = sdlog, 
            threshold = threshold), n.param.est = 3, method = method, 
        data.name = data.name, bad.obs = bad.obs)
    if (ci) {
        alpha <- 1 - conf.level
        beta <- exp(meanlog)
        beta2 <- beta^2
        switch(ci.method, avar = {
            varlog <- sdlog^2
            omega <- exp(varlog)
            H <- (omega * (1 + varlog) - (1 + 2 * varlog))^-1
            var.threshold <- (varlog/n) * (beta2/omega) * H
            if (ci.parameter == "threshold") {
                ci.obj <- ci.normal.approx(threshold, sqrt(var.threshold), 
                  n = n, df = n - 2, ci.type = ci.type, alpha = alpha)
                ci.obj$parameter <- "threshold"
            } else {
                var.beta <- (varlog/n) * beta2 * (1 + H)
                cov.threshold.beta <- -((sdlog^3)/n) * (beta2/sqrt(omega)) * 
                  H
                median <- threshold + beta
                var.median <- var.threshold + var.beta + 2 * 
                  cov.threshold.beta
                ci.obj <- ci.normal.approx(median, sqrt(var.median), 
                  n = n, df = n - 2, ci.type = ci.type, alpha = alpha)
                ci.obj$parameter <- "median"
            }
            ci.obj$method <- paste("Normal Approximation\n", 
                space(33), "Based on Asymptotic Variance", sep = "")
        }, likelihood.profile = {
            ci.obj <- ci.lnorm3.likelihood.profile(threshold = threshold, 
                meanlog = meanlog, sdlog = sdlog, x = x, x1 = x1, 
                n = n, eta.lb = eta.lb, eta.ub = eta.ub, ci.type = ci.type, 
                alpha = alpha, ci.parameter = ci.parameter)
        }, skewness = {
            ci.obj <- ci.lnorm3.zero.skew(threshold = threshold, 
                meanlog = meanlog, sdlog = sdlog, x = x, x1 = x1, 
                n = n, threshold.lb = threshold.lb, threshold.ub = threshold.ub, 
                ci.type = ci.type, alpha = alpha, ci.parameter = ci.parameter)
        })
        ret.list <- c(ret.list, list(interval = ci.obj))
    }
    oldClass(ret.list) <- "estimate"
    ret.list
}
