#' Estimate Parameters of a Lognormal Distribution (Log-Scale)
#' @description
#' Estimate the mean and standard deviation parameters of the logarithm of a
#'   \link[stats:Lognormal]{lognormal distribution}, and optionally construct a
#'   confidence interval for the mean.
#' @usage
#' elnorm(x, method = "mvue", ci = FALSE, ci.type = "two-sided",
#'     ci.method = "exact", conf.level = 0.95)
#' @rawRd
#' \arguments{
#'   \item{x}{
#'   numeric vector of observations.
#' }
#'   \item{method}{
#'   character string specifying the method of estimation.  Possible values are
#'   \code{"mvue"} (minimum variance unbiased; the default), and \code{"mle/mme"}
#'   (maximum likelihood/method of moments).
#'   See the DETAILS section for more information on these estimation methods.
#' }
#'   \item{ci}{
#'   logical scalar indicating whether to compute a confidence interval for the
#'   mean.  The default value is \code{FALSE}.
#' }
#'   \item{ci.type}{
#'   character string indicating what kind of confidence interval to compute.  The
#'   possible values are \code{"two-sided"} (the default), \code{"lower"}, and
#'   \code{"upper"}.  This argument is ignored if \code{ci=FALSE}.
#' }
#'   \item{ci.method}{
#'   character string indicating what method to use to construct the confidence interval
#'   for the mean or variance.  The only possible value is \code{"exact"} (the default).
#'   See the DETAILS section for more information.  This argument is ignored if
#'   \code{ci=FALSE}.
#' }
#'   \item{conf.level}{
#'   a scalar between 0 and 1 indicating the confidence level of the confidence interval.
#'   The default value is \code{conf.level=0.95}. This argument is ignored if
#'   \code{ci=FALSE}.
#' }
#' }
#' @rawRd
#' \details{
#'   If \code{x} contains any missing (\code{NA}), undefined (\code{NaN}) or
#'   infinite (\code{Inf}, \code{-Inf}) values, they will be removed prior to
#'   performing the estimation.
#'
#'   Let \eqn{X} denote a random variable with a
#'   \link[stats:Lognormal]{lognormal distribution} with
#'   parameters \code{meanlog=}\eqn{\mu} and \code{sdlog=}\eqn{\sigma}.  Then
#'   \eqn{Y = log(X)} has a \link[stats:Normal]{normal (Gaussian) distribution} with
#'   parameters \code{mean=}\eqn{\mu} and \code{sd=}\eqn{\sigma}.  Thus, the function
#'   \code{elnorm} simply calls the function \code{\link{enorm}} using the
#'   log-transformed values of \code{x}.
#' }
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
#'   Crow, E.L., and K. Shimizu. (1988).  \emph{Lognormal Distributions: Theory and
#'   Applications}.  Marcel Dekker, New York, Chapter 2.
#'
#'   Forbes, C., M. Evans, N. Hastings, and B. Peacock. (2011).  Statistical Distributions.
#'   Fourth Edition. John Wiley and Sons, Hoboken, NJ.
#'
#'   Gilbert, R.O. (1987). \emph{Statistical Methods for Environmental Pollution Monitoring}.
#'   Van Nostrand Reinhold, New York, NY.
#'
#'   Johnson, N. L., S. Kotz, and N. Balakrishnan. (1994).
#'   \emph{Continuous Univariate Distributions, Volume 1}.
#'   Second Edition. John Wiley and Sons, New York.
#'
#'   Limpert, E., W.A. Stahel, and M. Abbt. (2001).  Log-Normal Distributions Across the
#'   Sciences:  Keys and Clues.  \emph{BioScience} \bold{51}, 341--352.
#'
#'   Millard, S.P., and N.K. Neerchal. (2001). \emph{Environmental Statistics with S-PLUS}.
#'   CRC Press, Boca Raton, FL.
#'
#'   Ott, W.R. (1995). \emph{Environmental Statistics and Data Analysis}.
#'   Lewis Publishers, Boca Raton, FL.
#'
#'   USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
#'   EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.
#'   U.S. Environmental Protection Agency, Washington, D.C.
#' }
#' @rawRd
#' \author{
#'     Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \note{
#'   The normal and lognormal distribution are probably the two most frequently used
#'   distributions to model environmental data.  In order to make any kind of
#'   probability statement about a normally-distributed population (of chemical
#'   concentrations for example), you have to first estimate the mean and standard
#'   deviation (the population parameters) of the distribution.  Once you estimate
#'   these parameters, it is often useful to characterize the uncertainty in the
#'   estimate of the mean or variance.  This is done with confidence intervals.
#' }
#' @rawRd
#' \seealso{
#'   \link[stats]{Lognormal}, \link{LognormalAlt}, \link[stats]{Normal}.
#' }
#' @rawRd
#' \examples{
#'   # Using the Reference area TcCB data in the data frame EPA.94b.tccb.df,
#'   # estimate the mean and standard deviation of the log-transformed distribution,
#'   # and construct a 95% confidence interval for the mean.
#'
#'   with(EPA.94b.tccb.df, elnorm(TcCB[Area == "Reference"], ci = TRUE))
#'
#'   #Results of Distribution Parameter Estimation
#'   #--------------------------------------------
#'   #
#'   #Assumed Distribution:            Lognormal
#'   #
#'   #Estimated Parameter(s):          meanlog = -0.6195712
#'   #                                 sdlog   =  0.4679530
#'   #
#'   #Estimation Method:               mvue
#'   #
#'   #Data:                            TcCB[Area == "Reference"]
#'   #
#'   #Sample Size:                     47
#'   #
#'   #Confidence Interval for:         mean
#'   #
#'   #Confidence Interval Method:      Exact
#'   #
#'   #Confidence Interval Type:        two-sided
#'   #
#'   #Confidence Level:                95%
#'   #
#'   #Confidence Interval:             LCL = -0.7569673
#'   #                                 UCL = -0.4821751
#' }
#' @rawRd
#' \keyword{ distribution }
#' @rawRd
#' \keyword{ htest }

elnorm <-
function (x, method = "mvue", ci = FALSE, ci.type = "two-sided", 
    ci.method = "exact", conf.level = 0.95) 
{
    if (!is.vector(x, mode = "numeric")) 
        stop("'x' must be a numeric vector.")
    data.name <- deparse(substitute(x))
    method <- ifelse(ci, "mvue", match.arg(method, c("mvue", 
        "mle/mme")))
    if ((bad.obs <- sum(!(x.ok <- is.finite(x)))) > 0) {
        is.not.finite.warning(x)
        x <- x[x.ok]
        warning(paste(bad.obs, "observations with NA/NaN/Inf in 'x' removed."))
    }
    n <- length(x)
    if (n < 2 || length(unique(x)) < 2 || any(x <= 0)) 
        stop(paste("'x' must contain at least 2 non-missing distinct values,", 
            "and all non-missing values must be positive."))
    log.x <- log(x)
    muhat <- mean(log.x)
    sdhat <- ifelse(method == "mvue", sd(log.x), sqrt((n - 1)/n) * 
        sd(log.x))
    ret.list <- list(distribution = "Lognormal", sample.size = n, 
        parameters = c(meanlog = muhat, sdlog = sdhat), n.param.est = 2, 
        method = method, data.name = data.name, bad.obs = bad.obs)
    if (ci) {
        ci.type <- match.arg(ci.type, c("two-sided", "lower", 
            "upper"))
        ci.method <- match.arg(ci.method, "exact")
        if (conf.level <= 0 || conf.level >= 1) 
            stop("The value of 'conf.level' must be between 0 and 1.")
        ci.obj <- switch(ci.method, exact = ci.norm(muhat, sdhat, 
            n, ci.type, alpha = 1 - conf.level))
        ci.obj$parameter <- "meanlog"
        ret.list <- c(ret.list, list(interval = ci.obj))
    }
    oldClass(ret.list) <- "estimate"
    ret.list
}
