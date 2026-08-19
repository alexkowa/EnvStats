#' Estimate Mean, Standard Deviation, and Standard Error Nonparametrically
#' @description
#' Estimate the mean, standard deviation, and standard error of the mean
#'   nonparametrically given a sample of data, and optionally construct
#'   a confidence interval for the mean.
#' @usage
#' enpar(x, ci = FALSE, ci.method = "bootstrap", ci.type = "two-sided",
#'       conf.level = 0.95, pivot.statistic = "z", n.bootstraps = 1000, seed = NULL)
#' @rawRd
#' \arguments{
#'   \item{x}{
#'   numeric vector of observations.
#'   Missing (\code{NA}), undefined (\code{NaN}), and
#'   infinite (\code{Inf}, \code{-Inf}) values are allowed but will be removed.
#' }
#'   \item{ci}{
#'   logical scalar indicating whether to compute a confidence interval for the
#'   mean.  The default value is \code{ci=FALSE}.
#' }
#'   \item{ci.method}{
#'   character string indicating what method to use to construct the confidence interval
#'   for the mean.  The possible values are
#'   \code{"bootstrap"} (based on bootstrapping; the default), and
#'   \code{"normal.approx"} (normal approximation).
#'   See the \bold{DETAILS} section for more information.
#'   This argument is ignored if \code{ci=FALSE}.
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
#'   \item{pivot.statistic}{
#'   character string indicating which statistic to use for the confidence interval
#'   for the mean when \code{ci.method="normal.approx"}.  Possible values are
#'   \code{"z"} (confidence interval based on the z-statistic; the default), and
#'   \code{"t"} (confidence interval based on the t-statistic).
#'   This argument is ignored if \code{ci=FALSE} or \cr
#'   \code{ci.method="bootstrap"}.
#' }
#'   \item{n.bootstraps}{
#'   numeric scalar indicating how many bootstraps to use to construct the
#'   confidence interval for the mean.  This argument is ignored if
#'   \code{ci=FALSE} or \cr
#'   \code{ci.method="normal.approx"}.
#' }
#'   \item{seed}{
#'   integer supplied to the function \code{\link[base]{set.seed}} and used when \cr
#'   \code{ci=TRUE} and \code{ci.method="bootstrap"}.  The default value is
#'   \code{seed=NULL}, in which case the current value of \code{.Random.seed}
#'   is used.  This argument is ignored if \code{ci=FALSE} or
#'   \code{ci.method="normal.approx"}.  This argument is necessary to create
#'   reproducible results for the bootstrapped confidence intervals
#'   (see the \bold{EXAMPLES} section).
#' }
#' }
#' @details
#' This help page has been shortened to keep function help focused on usage,
#' arguments, return values, and examples. Extended method details, formulas,
#' and background material are available in \code{vignette("extended-function-details", package = "EnvStats")}, section \code{enpar}.
#' @rawRd
#' \value{
#'   a list of class \code{"estimate"} containing the estimated parameters
#'   and other information.  See \code{\link{estimate.object}} for details.
#' }
#' @rawRd
#' \references{
#'   Efron, B. (1979).  Bootstrap Methods: Another Look at the Jackknife.
#'   \emph{The Annals of Statistics} \bold{7}, 1--26.
#'
#'   Efron, B., and R.J. Tibshirani. (1993).  \emph{An Introduction to the Bootstrap}.
#'   Chapman and Hall, New York, 436pp.
#' }
#' @rawRd
#' \author{
#'     Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \note{
#'   The function \code{enpar} is related to the companion function
#'   \code{\link{enparCensored}} for censored data.  To estimate the median and
#'   compute a confidence interval, use \code{\link{eqnpar}}.
#'
#'   The result of the call to \code{enpar} with \code{ci.method="normal.approx"}
#'   and \code{pivot.statistic="t"} produces the same result as the call to
#'   \code{\link{enorm}} with \code{ci.param="mean"}.
#' }
#' @rawRd
#' \seealso{
#'   \code{\link{enparCensored}}, \code{\link{eqnpar}}, \code{\link{enorm}},
#'   \code{\link{mean}}, \code{\link{sd}}, \code{\link{estimate.object}}.
#' }
#' @rawRd
#' \examples{
#'   # The data frame ACE.13.TCE.df contains observations on
#'   # Trichloroethylene (TCE) concentrations (mg/L) at
#'   # 10 groundwater monitoring wells before and after remediation.
#'   #
#'   # Compute the mean concentration for each period along with
#'   # a 95% bootstrap BCa confidence interval for the mean.
#'   #
#'   # NOTE: Use of the argument "seed" is necessary to reproduce this example.
#'   #
#'   # Before remediation: 21.6 [14.2, 30.1]
#'   # After remediation:   3.6 [ 1.6,  5.7]
#'
#'   with(ACE.13.TCE.df,
#'     enpar(TCE.mg.per.L[Period=="Before"], ci = TRUE, seed = 476))
#'
#'   #Results of Distribution Parameter Estimation
#'   #--------------------------------------------
#'   #
#'   #Assumed Distribution:            None
#'   #
#'   #Estimated Parameter(s):          mean    = 21.62400
#'   #                                 sd      = 13.51134
#'   #                                 se.mean =  4.27266
#'   #
#'   #Estimation Method:               Sample Mean
#'   #
#'   #Data:                            TCE.mg.per.L[Period == "Before"]
#'   #
#'   #Sample Size:                     10
#'   #
#'   #Confidence Interval for:         mean
#'   #
#'   #Confidence Interval Method:      Bootstrap
#'   #
#'   #Number of Bootstraps:            1000
#'   #
#'   #Confidence Interval Type:        two-sided
#'   #
#'   #Confidence Level:                95%
#'   #
#'   #Confidence Interval:             Pct.LCL = 13.95560
#'   #                                 Pct.UCL = 29.79510
#'   #                                 BCa.LCL = 14.16080
#'   #                                 BCa.UCL = 30.06848
#'   #                                 t.LCL   = 12.41945
#'   #                                 t.UCL   = 32.47306
#'
#'   #----------
#'
#'   with(ACE.13.TCE.df,
#'     enpar(TCE.mg.per.L[Period=="After"], ci = TRUE, seed = 543))
#'
#'   #Results of Distribution Parameter Estimation
#'   #--------------------------------------------
#'   #
#'   #Assumed Distribution:            None
#'   #
#'   #Estimated Parameter(s):          mean    = 3.632900
#'   #                                 sd      = 3.554419
#'   #                                 se.mean = 1.124006
#'   #
#'   #Estimation Method:               Sample Mean
#'   #
#'   #Data:                            TCE.mg.per.L[Period == "After"]
#'   #
#'   #Sample Size:                     10
#'   #
#'   #Confidence Interval for:         mean
#'   #
#'   #Confidence Interval Method:      Bootstrap
#'   #
#'   #Number of Bootstraps:            1000
#'   #
#'   #Confidence Interval Type:        two-sided
#'   #
#'   #Confidence Level:                95%
#'   #
#'   #Confidence Interval:             Pct.LCL = 1.833843
#'   #                                 Pct.UCL = 5.830230
#'   #                                 BCa.LCL = 1.631655
#'   #                                 BCa.UCL = 5.677514
#'   #                                 t.LCL   = 1.683791
#'   #                                 t.UCL   = 8.101829
#' }
#' @rawRd
#' \keyword{ distribution }
#' @rawRd
#' \keyword{ htest }

enpar <-
function (x, ci = FALSE, ci.method = "bootstrap", ci.type = "two-sided", 
    conf.level = 0.95, pivot.statistic = "z", n.bootstraps = 1000, seed = NULL) 
{
    if (!is.vector(x, mode = "numeric")) 
        stop("'x' must be a numeric vector")
    data.name <- deparse(substitute(x))
    if ((bad.obs <- sum(!(ok <- is.finite(x)))) > 0) {
        x <- x[ok]
        warning(paste(bad.obs, "observations with NA/NaN/Inf in 'x' removed."))
    }
    if (length(unique(x)) < 2) 
        stop("'x' must contain at least 2 non-missing, distinct values.")
    N <- length(x)
    est.fcn <- function(x) {
        sd.x <- sd(x)
        c(mean = mean(x), sd = sd.x, se.mean = sd.x / sqrt(length(x)))
    }
    parameters <- est.fcn(x)
    param.ci.list <- list(parameters = parameters)
    if(ci) {
        ci.method <- match.arg(ci.method, c("normal.approx", "bootstrap"))
        ci.type <- match.arg(ci.type, c("two-sided", "lower", "upper"))
        if(ci.method == "normal.approx") {
            pivot.statistic <- match.arg(pivot.statistic, c("z", "t"))
            ci.obj <- ci.normal.approx(theta.hat = parameters["mean"], 
                sd.theta.hat = parameters["se.mean"], n = N, 
                df = N - 1, ci.type = ci.type, alpha = 1 - conf.level, 
                test.statistic = pivot.statistic)
            ci.obj$parameter <- "mean"
        }
        else {
            ci.obj <- enpar.bootstrap.ci(x = x, est.fcn = est.fcn, ci.type = ci.type, 
                conf.level = conf.level, n.bootstraps = n.bootstraps, 
                obs.mean = param.ci.list$parameters["mean"], 
                obs.se.mean = param.ci.list$parameters["se.mean"], seed = seed)
        }
        param.ci.list <- c(param.ci.list, list(ci.obj = ci.obj))
    }
    method <- "Sample Mean"
    ret.list <- list(distribution = "None", sample.size = N, 
        parameters = param.ci.list$parameters, n.param.est = 2, 
        method = method, data.name = data.name, 
        bad.obs = bad.obs)
    if (ci) {
        ret.list <- c(ret.list, list(interval = param.ci.list$ci.obj))
    }
    oldClass(ret.list) <- "estimate"
    ret.list
}


