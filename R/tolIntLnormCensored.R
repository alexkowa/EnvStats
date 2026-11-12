#' Tolerance Interval for a Lognormal Distribution Based on Censored Data
#' @description
#' Construct a \eqn{\beta}-content or \eqn{\beta}-expectation tolerance
#'   interval for a lognormal distribution based on Type I or Type II
#'   censored data.
#' @usage
#' tolIntLnormCensored(x, censored, censoring.side = "left", coverage = 0.95,
#'     cov.type = "content", ti.type = "two-sided", conf.level = 0.95,
#'     method = "mle", ti.method = "exact.for.complete", seed = NULL,
#'     nmc = 1000)
#' @rawRd
#' \arguments{
#'   \item{x}{
#'   numeric vector of positive observations.  Missing (\code{NA}), undefined (\code{NaN}), and
#'   infinite (\code{Inf}, \code{-Inf}) values are allowed but will be removed.
#' }
#'   \item{censored}{
#'   numeric or logical vector indicating which values of \code{x} are censored.  This must be the
#'   same length as \code{x}.  If the mode of \code{censored} is \code{"logical"}, \code{TRUE} values
#'   correspond to elements of \code{x} that are censored, and \code{FALSE} values correspond to
#'   elements of \code{x} that are not censored.  If the mode of \code{censored} is \code{"numeric"},
#'   it must contain only \code{1}'s and \code{0}'s; \code{1} corresponds to \code{TRUE} and
#'   \code{0} corresponds to \code{FALSE}.  Missing (\code{NA}) values are allowed but will be removed.
#' }
#'   \item{censoring.side}{
#'   character string indicating on which side the censoring occurs.  The possible values are
#'   \code{"left"} (the default) and \code{"right"}.
#' }
#'   \item{coverage}{
#'   a scalar between 0 and 1 indicating the desired coverage of the tolerance interval.
#'   The default value is \code{coverage=0.95}.
#' }
#'   \item{cov.type}{
#'   character string specifying the coverage type for the tolerance interval.
#'   The possible values are \code{"content"} (\eqn{\beta}-content; the default), and
#'   \code{"expectation"} (\eqn{\beta}-expectation).  See the DETAILS section for more
#'   information.
#' }
#'   \item{ti.type}{
#'   character string indicating what kind of tolerance interval to compute.
#'   The possible values are \code{"two-sided"} (the default), \code{"lower"}, and
#'   \code{"upper"}.
#' }
#'   \item{conf.level}{
#'   a scalar between 0 and 1 indicating the confidence level associated with the tolerance
#'   interval.  The default value is \code{conf.level=0.95}.
#' }
#'   \item{method}{
#'   character string indicating the method to use for parameter estimation on the log-scale.  \cr
#'   \cr
#'   For singly censored data, possible values are \code{"mle"} (the default), \code{"bcmle"},
#'   \code{"qq.reg"}, \code{"qq.reg.w.cen.level"}, \code{"impute.w.qq.reg"}, \cr
#'   \code{"impute.w.qq.reg.w.cen.level"}, \code{"impute.w.mle"}, \cr
#'   \code{"iterative.impute.w.qq.reg"},
#'   \code{"m.est"}, and \code{"half.cen.level"}.  See the help file for \code{\link{enormCensored}}
#'   for details. \cr
#'   \cr
#'   For multiply censored data, possible values are \code{"mle"} (the default), \code{"qq.reg"},
#'   \code{"impute.w.qq.reg"}, and \code{"half.cen.level"}.  See the help file for \cr
#'   \code{\link{enormCensored}} for details.
#' }
#'   \item{ti.method}{
#'   character string specifying the method for constructing the tolerance
#'   interval.  Possible values are: \cr
#'   \code{"exact.for.complete"} (the default), \cr
#'   \code{"gpq"} (Generalized Pivotal Quantity), and \cr
#'   \code{"wald.wolfowitz.for.complete"} (only available for a two-sided tolerance interval,
#'   i.e., when \code{ti.type="two-sided"}).  \cr
#'   See the DETAILS section for more information.
#' }
#'   \item{seed}{
#'   for the case when \code{ti.method="gpq"}, a positive integer to pass to the
#'   function \code{\link{gpqTolIntNormSinglyCensored}} or
#'   \code{\link{gpqTolIntNormMultiplyCensored}}.  This argument is
#'   ignored if \code{seed=NULL} (the default).  Using the \code{seed}
#'   argument lets you reproduce the exact same result if all other
#'   arguments stay the same.
#' }
#'   \item{nmc}{
#'   for the case when \code{ti.method="gpq"}, a positive integer \eqn{\ge 10}
#'   indicating the number of Monte Carlo trials to run in order to compute the
#'   GPQ(s).
#' }
#' }
#' @rawRd
#' \details{
#'   A tolerance interval for a lognormal distribution is constructed by taking the
#'   natural logarithm of the observations and constructing a tolerance interval
#'   based on the normal (Gaussian) distribution by calling \code{\link{tolIntNormCensored}}.
#'   These tolerance limits are then exponentiated to produce a tolerance interval on
#'   the original scale of the data.
#' }
#' @rawRd
#' \value{
#'   A list of class \code{"estimateCensored"} containing the estimated
#'   parameters, the tolerance interval, and other information.
#'   See \code{\link{estimateCensored.object}} for details.
#' }
#' @rawRd
#' \references{
#'   Berthouex, P.M., and L.C. Brown. (2002). \emph{Statistics for Environmental Engineers}.
#'   Lewis Publishers, Boca Raton.
#'
#'   Draper, N., and H. Smith. (1998). \emph{Applied Regression Analysis}. Third Edition.
#'   John Wiley and Sons, New York.
#'
#'   Ellison, B.E. (1964). On Two-Sided Tolerance Intervals for a Normal Distribution.
#'   \emph{Annals of Mathematical Statistics} \bold{35}, 762-772.
#'
#'   Gibbons, R.D., D.K. Bhaumik, and S. Aryal. (2009).
#'   \emph{Statistical Methods for Groundwater Monitoring}, Second Edition.
#'   John Wiley & Sons, Hoboken.
#'
#'   Guttman, I. (1970). \emph{Statistical Tolerance Regions: Classical and Bayesian}.
#'   Hafner Publishing Co., Darien, CT.
#'
#'   Hahn, G.J. (1970b). Statistical Intervals for a Normal Population, Part I: Tables, Examples
#'   and Applications. \emph{Journal of Quality Technology} \bold{2}(3), 115-125.
#'
#'   Hahn, G.J. (1970c). Statistical Intervals for a Normal Population, Part II: Formulas, Assumptions,
#'   Some Derivations. \emph{Journal of Quality Technology} \bold{2}(4), 195-206.
#'
#'   Hahn, G.J., and W.Q. Meeker. (1991). \emph{Statistical Intervals: A Guide for Practitioners}.
#'   John Wiley and Sons, New York.
#'
#'   Krishnamoorthy K., and T. Mathew. (2009).
#'   \emph{Statistical Tolerance Regions: Theory, Applications, and Computation}.
#'   John Wiley and Sons, Hoboken.
#'
#'   Millard, S.P., and N.K. Neerchal. (2001). \emph{Environmental Statistics with S-PLUS}.
#'   CRC Press, Boca Raton.
#'
#'   Odeh, R.E., and D.B. Owen. (1980). \emph{Tables for Normal Tolerance Limits, Sampling Plans,
#'   and Screening}. Marcel Dekker, New York.
#'
#'   Owen, D.B. (1962). \emph{Handbook of Statistical Tables}. Addison-Wesley, Reading, MA.
#'
#'   Singh, A., R. Maichle, and N. Armbya. (2010a).
#'   \emph{ProUCL Version 4.1.00 User Guide (Draft)}. EPA/600/R-07/041, May 2010.
#'   Office of Research and Development, U.S. Environmental Protection Agency, Washington, D.C.
#'
#'   Singh, A., N. Armbya, and A. Singh. (2010b).
#'   \emph{ProUCL Version 4.1.00 Technical Guide (Draft)}. EPA/600/R-07/041, May 2010.
#'   Office of Research and Development, U.S. Environmental Protection Agency, Washington, D.C.
#'
#'   USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
#'   EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.
#'   U.S. Environmental Protection Agency, Washington, D.C.
#'
#'   USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
#'   EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
#'   U.S. Environmental Protection Agency, Washington, D.C.
#'
#'   Wald, A., and J. Wolfowitz. (1946). Tolerance Limits for a Normal Distribution.
#'   \emph{Annals of Mathematical Statistics} \bold{17}, 208-215.
#' }
#' @rawRd
#' \author{
#'     Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \note{
#'   Tolerance intervals have long been applied to quality control and
#'   life testing problems (Hahn, 1970b,c; Hahn and Meeker, 1991; Krishnamoorthy and Mathew, 2009).
#'   References that discuss tolerance intervals in the context of environmental monitoring include:
#'   Berthouex and Brown (2002, Chapter 21), Gibbons et al. (2009),
#'   Millard and Neerchal (2001, Chapter 6), Singh et al. (2010b), and USEPA (2009).
#' }
#' @rawRd
#' \seealso{
#'   \code{\link{tolIntNormCensored}},
#'   \code{\link{gpqTolIntNormSinglyCensored}}, \code{\link{eqnormCensored}},
#'   \code{\link{enormCensored}}, \cr
#'   \code{\link{estimateCensored.object}}.
#' }
#' @rawRd
#' \examples{
#'   # Generate 20 observations from a lognormal distribution with parameters
#'   # mean=10 and cv=1, censor the observations less than 5,
#'   # then create a one-sided upper tolerance interval with 90%
#'   # coverage and 95% confidence based on these Type I left, singly
#'   # censored data.
#'   # (Note: the call to set.seed allows you to reproduce this example.)
#'
#'   set.seed(250)
#'   dat <- rlnormAlt(20, mean = 10, cv = 1)
#'   sort(dat)
#'   # [1]  2.608298  3.185459  4.196216  4.383764  4.569752  5.136130
#'   # [7]  5.209538  5.916284  6.199076  6.214755  6.255779  6.778361
#'   #[13]  7.074972  7.100494  8.930845 10.388766 11.402769 14.247062
#'   #[19] 14.559506 15.437340
#'
#'   censored <- dat < 5
#'   dat[censored] <- 5
#'
#'   tolIntLnormCensored(dat, censored, coverage = 0.9, ti.type="upper")
#'
#'   #Results of Distribution Parameter Estimation
#'   #Based on Type I Censored Data
#'   #--------------------------------------------
#'   #
#'   #Assumed Distribution:            Lognormal
#'   #
#'   #Censoring Side:                  left
#'   #
#'   #Censoring Level(s):              5
#'   #
#'   #Estimated Parameter(s):          meanlog = 1.8993686
#'   #                                 sdlog   = 0.4804343
#'   #
#'   #Estimation Method:               MLE
#'   #
#'   #Data:                            dat
#'   #
#'   #Censoring Variable:              censored
#'   #
#'   #Sample Size:                     20
#'   #
#'   #Percent Censored:                25%
#'   #
#'   #Assumed Sample Size:             20
#'   #
#'   #Tolerance Interval Coverage:     90%
#'   #
#'   #Coverage Type:                   content
#'   #
#'   #Tolerance Interval Method:       Exact for
#'   #                                 Complete Data
#'   #
#'   #Tolerance Interval Type:         upper
#'   #
#'   #Confidence Level:                95%
#'   #
#'   #Tolerance Interval:              LTL =  0.00000
#'   #                                 UTL = 16.85556
#'
#' \dontrun{
#'   # Note:  The true 90'th percentile is 20.55231
#'   #---------------------------------------------
#'   qlnormAlt(0.9, mean = 10, cv = 1)
#'   #[1] 20.55231
#'
#'
#'   # Compare the result using the method "gpq"
#'   tolIntLnormCensored(dat, censored, coverage = 0.9, ti.type="upper",
#'     ti.method = "gpq", seed = 432)$interval$limits
#'
#'   #     LTL      UTL
#'   # 0.00000 17.85474
#'
#'   # Clean Up
#'   #---------
#'   rm(dat, censored)
#'
#'   #--------------------------------------------------------------
#'
#'   # Example 15-1 of USEPA (2009, p. 15-10) shows how to estimate
#'   # the mean and standard deviation using log-transformed multiply
#'   # left-censored manganese concentration data.  Here we'll construct a
#'   # 95% upper tolerance limit with 90% coverage using these data.
#'
#'   EPA.09.Ex.15.1.manganese.df
#'   #    Sample   Well Manganese.Orig.ppb Manganese.ppb Censored
#'   # 1       1 Well.1                 <5           5.0     TRUE
#'   # 2       2 Well.1               12.1          12.1    FALSE
#'   # 3       3 Well.1               16.9          16.9    FALSE
#'   # ...
#'   # 23      3 Well.5                3.3           3.3    FALSE
#'   # 24      4 Well.5                8.4           8.4    FALSE
#'   # 25      5 Well.5                 <2           2.0     TRUE
#'
#'   with(EPA.09.Ex.15.1.manganese.df,
#'     tolIntLnormCensored(Manganese.ppb, Censored, coverage = 0.9,
#'       ti.type = "upper"))
#'
#'   #Results of Distribution Parameter Estimation
#'   #Based on Type I Censored Data
#'   #--------------------------------------------
#'   #
#'   #Assumed Distribution:            Lognormal
#'   #
#'   #Censoring Side:                  left
#'   #
#'   #Censoring Level(s):              2 5
#'   #
#'   #Estimated Parameter(s):          meanlog = 2.215905
#'   #                                 sdlog   = 1.356291
#'   #
#'   #Estimation Method:               MLE
#'   #
#'   #Data:                            Manganese.ppb
#'   #
#'   #Censoring Variable:              censored
#'   #
#'   #Sample Size:                     25
#'   #
#'   #Percent Censored:                24%
#'   #
#'   #Assumed Sample Size:             25
#'   #
#'   #Tolerance Interval Coverage:     90%
#'   #
#'   #Coverage Type:                   content
#'   #
#'   #Tolerance Interval Method:       Exact for
#'   #                                 Complete Data
#'   #
#'   #Tolerance Interval Type:         upper
#'   #
#'   #Confidence Level:                95%
#'   #
#'   #Tolerance Interval:              LTL =   0.0000
#'   #                                 UTL = 110.9305
#'   }
#' }
#' @rawRd
#' \keyword{ distribution }
#' @rawRd
#' \keyword{ htest }

tolIntLnormCensored <-
function (x, censored, censoring.side = "left", coverage = 0.95, 
    cov.type = "content", ti.type = "two-sided", conf.level = 0.95, 
    method = "mle", ti.method = "exact.for.complete", seed = NULL, 
    nmc = 1000) 
{
    if (!is.vector(x, mode = "numeric")) 
        stop("'x' must be a numeric vector")
    if (!is.vector(censored, mode = "numeric") && !is.vector(censored, 
        mode = "logical")) 
        stop("'censored' must be a logical or numeric vector")
    if (length(censored) != length(x)) 
        stop("'censored' must be the same length as 'x'")
    data.name <- deparse(substitute(x))
    censoring.name <- deparse(substitute(censored))
    if ((bad.obs <- sum(!(ok <- is.finite(x) & is.finite(as.numeric(censored))))) > 
        0) {
        x <- x[ok]
        censored <- censored[ok]
        warning(paste(bad.obs, "observations with NA/NaN/Inf in 'x' and 'censored' removed."))
    }
    if (is.numeric(censored)) {
        if (!all(censored == 0 | censored == 1)) 
            stop(paste("When 'censored' is a numeric vector, all values of", 
                "'censored' must be 0 (not censored) or 1 (censored)."))
        censored <- as.logical(censored)
    }
    n.cen <- sum(censored)
    if (n.cen == 0) 
        stop("No censored values indicated by 'censored'.")
    if (any(x <= 0)) 
        stop("All values of 'x' (including censored ones) must be positive")
    x.no.cen <- x[!censored]
    if (length(unique(x.no.cen)) < 2) 
        stop("'x' must contain at least 2 non-missing, uncensored, distinct values")
    censoring.side <- match.arg(censoring.side, c("left", "right"))
    if (!is.numeric(coverage) || length(coverage) != 1 || is.na(coverage) || 
        coverage <= 0 || coverage >= 1) 
        stop("'coverage' must be a scalar greater than 0 and less than 1.")
    cov.type <- match.arg(cov.type, c("content", "expectation"))
    ti.type <- match.arg(ti.type, c("two-sided", "lower", "upper"))
    if (!is.numeric(conf.level) || length(conf.level) > 1 || 
        conf.level <= 0 || conf.level >= 1) 
        stop("'conf.level' must be a scalar greater than 0 and less than 1.")
    multiple <- TRUE
    T.vec <- sort(unique(x[censored]))
    if (length(T.vec) == 1) {
        if (censoring.side == "left") {
            if (T.vec <= min(x.no.cen)) 
                multiple <- FALSE
        }
        else {
            if (T.vec >= max(x.no.cen)) 
                multiple <- FALSE
        }
    }
    if (multiple) {
        method <- match.arg(method, c("mle", "qq.reg", "impute.w.qq.reg", 
            "half.cen.level"))
    }
    else {
        method <- match.arg(method, c("mle", "bcmle", "qq.reg", 
            "qq.reg.w.cen.level", "impute.w.qq.reg", "impute.w.qq.reg.w.cen.level", 
            "impute.w.mle", "iterative.impute.w.qq.reg", "m.est", 
            "half.cen.level"))
    }
    ti.method <- match.arg(ti.method, c("exact.for.complete", 
        "wald.wolfowitz.for.complete", "gpq"))
    if (ti.method == "gpq" && cov.type != "content") 
        stop("When ti.method='gpq' you must set cov.type='content'")
    ret.list <- tolIntNormCensored(x = log(x), censored = censored, 
        censoring.side = censoring.side, coverage = coverage, 
        cov.type = cov.type, ti.type = ti.type, conf.level = conf.level, 
        method = method, ti.method = ti.method, seed = seed, 
        nmc = nmc)
    names(ret.list$parameters) <- c("meanlog", "sdlog")
    ret.list$censoring.levels <- T.vec
    ret.list$data.name <- data.name
    ret.list$bad.obs <- bad.obs
    ret.list$distribution <- "Lognormal"
    ret.list$interval$limits <- exp(ret.list$interval$limits)
    ret.list
}
