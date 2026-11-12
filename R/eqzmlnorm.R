#' Estimate Quantiles of a Zero-Modified Lognormal (Delta) Distribution
#' @aliases eqzmlnormAlt
#' @description
#' Estimate quantiles of a
#'   \link[=ZeroModifiedLognormal]{zero-modified lognormal distribution} or a
#'   \link[=ZeroModifiedLognormalAlt]{zero-modified lognormal distribution (alternative parameterization)}.
#' @usage
#' eqzmlnorm(x, p = 0.5, method = "mvue", digits = 0)
#'
#'   eqzmlnormAlt(x, p = 0.5, method = "mvue", digits = 0)
#' @rawRd
#' \arguments{
#'   \item{x}{
#'   a numeric vector of positive observations, or an object resulting from a call to
#'   an estimating function that assumes a zero-modified lognormal distribution.
#'
#'   For \code{eqzmlnorm}, if \code{x} is an object, it must be the result of calling
#'   \code{\link{ezmlnorm}}, not \code{\link{ezmlnormAlt}}.
#'
#'   For \code{eqzmlnormAlt}, if \code{x} is an object, it must be the
#'   result of calling \code{\link{ezmlnormAlt}}, not \code{\link{ezmlnorm}}.
#'
#'   If \code{x} is a numeric vector,
#'   missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf})
#'   values are allowed but will be removed.
#' }
#'   \item{p}{
#'   numeric vector of probabilities for which quantiles will be estimated.
#'   All values of \code{p} must be between 0 and 1.  When \code{ci=TRUE}, \code{p}
#'   must be a scalar. The default value is \code{p=0.5}.
#' }
#'   \item{method}{
#'   character string specifying the method of estimation.  The only possible value is
#'   \code{"mvue"} (minimum variance unbiased; the default).  See the DETAILS section of
#'   the help file for \code{\link{ezmlnorm}} for more information.
#' }
#'   \item{digits}{
#'   an integer indicating the number of decimal places to round to when printing out
#'   the value of \code{100*p}. The default value is \code{digits=0}.
#' }
#' }
#' @rawRd
#' \details{
#'   The functions \code{eqzmlnorm} and \code{eqzmlnormAlt} return estimated quantiles
#'   as well as estimates of the distribution parameters.
#'
#'   Quantiles are estimated by:
#'   \enumerate{
#'     \item estimating the distribution parameters by calling \code{\link{ezmlnorm}} or
#'     \code{\link{ezmlnormAlt}}, and then
#'
#'     \item calling the function \code{\link[=ZeroModifiedLognormal]{qzmlnorm}} or
#'     \code{\link[=ZeroModifiedLognormalAlt]{qzmlnormAlt}} and using the estimated
#'     distribution parameters.
#'   }
#' }
#' @rawRd
#' \value{
#'   If \code{x} is a numeric vector, \code{eqzmlnorm} and \code{eqzmlnormAlt} return a
#'   list of class \code{"estimate"} containing the estimated quantile(s) and other
#'   information. See \code{\link{estimate.object}} for details.
#'
#'   If \code{x} is the result of calling an estimation function, \code{eqzmlnorm} and
#'   \code{eqzmlnormAlt} return a list whose class is the same as \code{x}.  The list
#'   contains the same components as \code{x}, as well as components called
#'   \code{quantiles} and \code{quantile.method}.
#' }
#' @rawRd
#' \references{
#'   Aitchison, J. (1955).  On the Distribution of a Positive Random Variable Having
#'   a Discrete Probability Mass at the Origin.  \emph{Journal of the American
#'   Statistical Association} \bold{50}, 901--908.
#'
#'   Aitchison, J., and J.A.C. Brown (1957).  \emph{The Lognormal Distribution
#'   (with special reference to its uses in economics)}.  Cambridge University Press,
#'   London. pp.94-99.
#'
#'   Crow, E.L., and K. Shimizu. (1988).  \emph{Lognormal Distributions:
#'   Theory and Applications}.  Marcel Dekker, New York, pp.47--51.
#'
#'   Gibbons, RD., D.K. Bhaumik, and S. Aryal. (2009).  \emph{Statistical Methods
#'   for Groundwater Monitoring}.  Second Edition.  John Wiley and Sons, Hoboken, NJ.
#'
#'   Gilliom, R.J., and D.R. Helsel. (1986).  Estimation of Distributional Parameters
#'   for Censored Trace Level Water Quality Data: 1. Estimation Techniques.
#'   \emph{Water Resources Research} \bold{22}, 135--146.
#'
#'   Helsel, D.R. (2012).  \emph{Statistics for Censored Environmental Data Using
#'   Minitab and R}.  Second Edition.  John Wiley and Sons, Hoboken, NJ, Chapter 1.
#'
#'   Johnson, N. L., S. Kotz, and A.W. Kemp. (1992).  \emph{Univariate Discrete Distributions}.
#'   Second Edition. John Wiley and Sons, New York, p.312.
#'
#'   Owen, W., and T. DeRouen. (1980).  Estimation of the Mean for Lognormal Data
#'   Containing Zeros and Left-Censored Values, with Applications to the Measurement
#'   of Worker Exposure to Air Contaminants.  \emph{Biometrics} \bold{36}, 707--719.
#'
#'   USEPA (1992c).  \emph{Statistical Analysis of Ground-Water Monitoring Data at
#'   RCRA Facilities: Addendum to Interim Final Guidance}.  Office of Solid Waste,
#'   Permits and State Programs Division, US Environmental Protection Agency,
#'   Washington, D.C.
#'
#'   USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
#'   EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.
#'   U.S. Environmental Protection Agency, Washington, D.C.
#' }
#' @rawRd
#' \author{
#'   Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \note{
#'   The zero-modified lognormal (delta) distribution is sometimes used to
#'   model chemical concentrations for which some observations are reported as
#'   \dQuote{Below Detection Limit} (the nondetects are assumed equal to 0).
#'   See, for example, Gilliom and Helsel (1986), Owen and DeRouen (1980), and
#'   Gibbons et al. (2009, Chapter 12).  USEPA (2009, Chapter 15) recommends this
#'   strategy only in specific situations, and Helsel (2012, Chapter 1) strongly
#'   discourages this approach to dealing with non-detects.
#'
#'   A variation of the zero-modified lognormal (delta) distribution is the
#'   \link[=ZeroModifiedNormal]{zero-modified normal distribution}, in which a
#'   normal distribution is mixed with a positive probability mass at 0.
#'
#'   One way to try to assess whether a zero-modified lognormal (delta),
#'   zero-modified normal, censored normal, or censored lognormal is the best
#'   model for the data is to construct both censored and detects-only probability
#'   plots (see \code{\link{qqPlotCensored}}).
#' }
#' @rawRd
#' \seealso{
#'   \code{\link{ezmlnorm}}, \link[=ZeroModifiedLognormal]{Zero-Modified Lognormal},
#'   \code{\link{ezmlnormAlt}},
#'   \link[=ZeroModifiedLognormalAlt]{Zero-Modified Lognormal (Alternative Parameterization)},
#'   \link[=ZeroModifiedNormal]{Zero-Modified Normal}, \link[stats:Lognormal]{Lognormal}.
#' }
#' @rawRd
#' \examples{
#'   # Generate 100 observations from a zero-modified lognormal (delta)
#'   # distribution with mean=2, cv=1, and p.zero=0.5, then estimate the
#'   # parameters and also the 80'th and 90'th percentiles.
#'   # (Note: the call to set.seed simply allows you to reproduce this example.)
#'
#'   set.seed(250)
#'   dat <- rzmlnormAlt(100, mean = 2, cv = 1, p.zero = 0.5)
#'   eqzmlnormAlt(dat, p = c(0.8, 0.9))
#'
#'   #Results of Distribution Parameter Estimation
#'   #--------------------------------------------
#'   #
#'   #Assumed Distribution:            Zero-Modified Lognormal (Delta)
#'   #
#'   #Estimated Parameter(s):          mean         = 1.9604561
#'   #                                 cv           = 0.9169411
#'   #                                 p.zero       = 0.4500000
#'   #                                 mean.zmlnorm = 1.0782508
#'   #                                 cv.zmlnorm   = 1.5307175
#'   #
#'   #Estimation Method:               mvue
#'   #
#'   #Estimated Quantile(s):           80'th %ile = 1.897451
#'   #                                 90'th %ile = 2.937976
#'   #
#'   #Quantile Estimation Method:      Quantile(s) Based on
#'   #                                 mvue Estimators
#'   #
#'   #Data:                            dat
#'   #
#'   #Sample Size:                     100
#'
#'   #----------
#'
#'   # Compare the estimated quatiles with the true quantiles
#'
#'   qzmlnormAlt(mean = 2, cv = 1, p.zero = 0.5, p = c(0.8, 0.9))
#'   #[1] 1.746299 2.849858
#'
#'   #----------
#'
#'   # Clean up
#'   rm(dat)
#' }
#' @rawRd
#' \keyword{ distribution }
#' @rawRd
#' \keyword{ htest }

eqzmlnorm <-
function (x, p = 0.5, method = "mvue", digits = 0) 
{
    if (!is.vector(p, mode = "numeric") || is.factor(p)) 
        stop("'p' must be a numeric vector.")
    if (any(!is.finite(p))) 
        stop("NA/NaN/Inf values not allowed in 'p'.")
    if (any(p < 0) || any(p > 1)) 
        stop("All values of 'p' must be between 0 and 1.")
    method <- match.arg(method)
    if (x.is.est.obj <- data.class(x) == "estimate" || data.class(x) == 
        "estimateCensored") {
        if (x$distribution != "Zero-Modified Lognormal (Delta)" || 
            names(x$parameters) != c("meanlog", "sdlog", "p.zero", 
                "mean.zmlnorm", "sd.zmlnorm")) 
            stop(paste("'eqzmlnorm' estimates quantiles", "for a zero-modified lognormal distribution.  You have supplied an object", 
                "that assumes a different distribution."))
        class.x <- oldClass(x)
        if (!is.null(x$interval)) {
            x <- x[-match("interval", names(x))]
            oldClass(x) <- class.x
        }
        meanlog <- x$parameters["meanlog"]
        sdlog <- x$parameters["sdlog"]
        p.zero <- x$parameters["p.zero"]
        n <- x$sample.size
        ret.list <- x
    }
    else {
        if (!is.vector(x, mode = "numeric") || is.factor(x)) 
            stop(paste("'x' must be either a list that inherits from", 
                "the class 'estimate', or else a numeric vector"))
        data.name <- deparse(substitute(x))
        if ((bad.obs <- sum(!(x.ok <- is.finite(x)))) > 0) {
            is.not.finite.warning(x)
            x <- x[x.ok]
            warning(paste(bad.obs, "observations with NA/NaN/Inf in 'x' removed."))
        }
        n <- length(x)
        if (n < 1 || any(x < 0)) 
            stop(paste("'x' must contain at least 1 non-missing value,", 
                "and all non-missing values of 'x' must be non-negative. ", 
                "This is not true for 'x' =", data.name))
        ret.list <- ezmlnorm(x, method = method)
        ret.list$data.name <- data.name
        ret.list$bad.obs <- bad.obs
        meanlog <- ret.list$parameters["meanlog"]
        sdlog <- ret.list$parameters["sdlog"]
        p.zero <- ret.list$parameters["p.zero"]
    }
    q <- qzmlnorm(p, meanlog = meanlog, sdlog = sdlog, p.zero = p.zero)
    if (length(p) == 1 && p == 0.5) 
        names(q) <- "Median"
    else {
        pct <- round(100 * p, digits)
        names(q) <- paste(pct, number.suffix(pct), " %ile", sep = "")
    }
    ret.list <- c(ret.list, list(quantiles = q))
    ret.list$quantile.method <- paste("Quantile(s) Based on\n", 
        space(33), ret.list$method, " Estimators", sep = "")
    if (x.is.est.obj) 
        oldClass(ret.list) <- class.x
    else oldClass(ret.list) <- "estimate"
    ret.list
}
