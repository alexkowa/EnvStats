#' Estimate Parameters of Gamma Distribution
#' @aliases egammaAlt
#' @description
#' Estimate the shape and scale parameters (or the mean and coefficient of
#'   variation) of a \link[=GammaDist]{Gamma} distribution.
#' @usage
#' egamma(x, method = "mle", ci = FALSE,
#'     ci.type = "two-sided", ci.method = "normal.approx",
#'     normal.approx.transform = "kulkarni.powar", conf.level = 0.95)
#'
#'   egammaAlt(x, method = "mle", ci = FALSE,
#'     ci.type = "two-sided", ci.method = "normal.approx",
#'     normal.approx.transform = "kulkarni.powar", conf.level = 0.95)
#' @rawRd
#' \arguments{
#'   \item{x}{
#'   numeric vector of non-negative observations.
#'   Missing (\code{NA}), undefined (\code{NaN}), and
#'   infinite (\code{Inf}, \code{-Inf}) values are allowed but will be removed.
#' }
#'   \item{method}{
#'   character string specifying the method of estimation.  The possible values are: \cr
#'   \code{"mle"} (maximum likelihood; the default), \cr
#'   \code{"bcmle"} (bias-corrected mle), \cr
#'   \code{"mme"} (method of moments), and \cr
#'   \code{"mmue"} (method of moments based on the unbiased estimator of variance). \cr
#'   See the DETAILS section for more information.
#' }
#'   \item{ci}{
#'   logical scalar indicating whether to compute a confidence interval for the mean.
#'   The default value is \code{ci=FALSE}.
#' }
#'   \item{ci.type}{
#'   character string indicating what kind of confidence interval to compute.
#'   The possible values are
#'   \code{"two-sided"} (the default), \code{"lower"}, and \code{"upper"}.
#'   This argument is ignored if \code{ci=FALSE}.
#' }
#'   \item{ci.method}{
#'   character string indicating which method to use to construct the confidence interval.
#'   Possible values are \code{"normal.approx"} (the default),
#'   \code{"profile.likelihood"}, \code{"chisq.approx"}, and \code{"chisq.adj"}.
#'   This argument is ignored if \code{ci=FALSE}.
#' }
#'   \item{normal.approx.transform}{
#'   character string indicating which power transformation to use when \cr
#'   \code{ci.method="normal.approx"}.  Possible values are \cr
#'   \code{"kulkarni.powar"} (the default), \code{"cube.root"}, and
#'   \code{"fourth.root"}.  See the DETAILS section for more informaiton.
#'   This argument is ignored if \code{ci=FALSE} or \code{ci.method="chisq.approx"}.
#' }
#'   \item{conf.level}{
#'   a scalar between 0 and 1 indicating the confidence level of the confidence interval.  The default
#'   value is \code{conf.level=0.95}.  This argument is ignored if \code{ci=FALSE}.
#' }
#' }
#' @details
#' This help page has been shortened to keep function help focused on usage,
#' arguments, return values, and examples. Extended method details, formulas,
#' and background material are available in \code{vignette("extended-function-details", package = "EnvStats")}, section \code{egamma}.
#' @rawRd
#' \value{
#'   a list of class \code{"estimate"} containing the estimated parameters and other information.
#'   See \cr
#'   \code{\link{estimate.object}} for details.
#' }
#' @rawRd
#' \references{
#'   Anderson, C.W., and W.D. Ray. (1975). Improved Maximum Likelihood Estimators
#'   for the Gamma Distribution. \emph{Communications in Statistics}, \bold{4}, 437--448.
#'
#'   Forbes, C., M. Evans, N. Hastings, and B. Peacock. (2011).
#'   \emph{Statistical Distributions, Fourth Edition}.
#'   John Wiley and Sons, Hoboken, NJ.
#'
#'   Grice, J.V., and L.J. Bain. (1980). Inferences Concerning the Mean of the Gamma Distribution.
#'   \emph{Journal of the American Statistician}, \bold{75}, 929-933.
#'
#'   Hawkins, D. M., and R.A.J. Wixley. (1986). A Note on the Transformation of
#'   Chi-Squared Variables to Normality. \emph{The American Statistician},
#'   \bold{40}, 296--298.
#'
#'   Johnson, N.L., S. Kotz, and N. Balakrishnan. (1994).
#'   \emph{Continuous Univariate Distributions, Volume 1}. Second Edition.
#'   John Wiley and Sons, New York, Chapter 17.
#'
#'   Kulkarni, H.V., and S.K. Powar. (2010). A New Method for Interval Estimation of the Mean
#'   of the Gamma Distribution. \emph{Lifetime Data Analysis}, \bold{16}, 431--447.
#'
#'   Singh, A., A.K. Singh, and R.J. Iaci. (2002).
#'   \emph{Estimation of the Exposure Point Concentration Term Using a Gamma Distribution}.
#'   EPA/600/R-02/084. October 2002. Technology Support Center for Monitoring and
#'   Site Characterization, Office of Research and Development, Office of Solid Waste and
#'   Emergency Response, U.S. Environmental Protection Agency, Washington, D.C.
#'
#'   USEPA. (2015).  \emph{ProUCL Version 5.1.002 Technical Guide}.  EPA/600/R-07/041, October 2015.
#'   Office of Research and Development. U.S. Environmental Protection Agency, Washington, D.C.
#'
#'   Wilson, E.B., and M.M. Hilferty. (1931). The Distribution of Chi-Squares.
#'   \emph{Proceedings of the National Academy of Sciences}, \bold{17}, 684--688.
#' }
#' @rawRd
#' \author{
#'     Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \note{
#'   The gamma distribution takes values on the positive real line.
#'   Special cases of the gamma are the \link[=Exponential]{exponential} distribution and
#'   the \link[=Chisquare]{chi-square} distributions. Applications of the gamma include
#'   life testing, statistical ecology, queuing theory, inventory control, and precipitation
#'   processes. A gamma distribution starts to resemble a normal distribution as the
#'   shape parameter a tends to infinity.
#'
#'   Some EPA guidance documents (e.g., Singh et al., 2002; Singh et al., 2010a,b) strongly recommend
#'   against using a lognormal model for environmental data and recommend trying a gamma distribuiton
#'   instead.
#' }
#' @rawRd
#' \section{Warning}{
#'   When \code{ci=TRUE} and \code{ci.method="normal.approx"}, it is possible for the
#'   lower confidence limit based on the transformed data to be less than 0.
#'   In this case, the lower confidence limit on the original scale is set to 0 and a warning is
#'   issued stating that the normal approximation is not accurate in this case.
#' }
#' @rawRd
#' \seealso{
#'   \code{\link{GammaDist}}, \code{\link{estimate.object}}, \code{\link{eqgamma}},
#'   \code{\link{predIntGamma}}, \code{\link{tolIntGamma}}.
#' }
#' @rawRd
#' \examples{
#'   # Generate 20 observations from a gamma distribution with parameters
#'   # shape=3 and scale=2, then estimate the parameters.
#'   # (Note: the call to set.seed simply allows you to reproduce this
#'   # example.)
#'
#'   set.seed(250)
#'   dat <- rgamma(20, shape = 3, scale = 2)
#'   egamma(dat, ci = TRUE)
#'
#'   #Results of Distribution Parameter Estimation
#'   #--------------------------------------------
#'   #
#'   #Assumed Distribution:            Gamma
#'   #
#'   #Estimated Parameter(s):          shape = 2.203862
#'   #                                 scale = 2.174928
#'   #
#'   #Estimation Method:               mle
#'   #
#'   #Data:                            dat
#'   #
#'   #Sample Size:                     20
#'   #
#'   #Confidence Interval for:         mean
#'   #
#'   #Confidence Interval Method:      Optimum Power Normal Approximation
#'   #                                 of Kulkarni & Powar (2010)
#'   #                                 using mle of 'shape'
#'   #
#'   #Normal Transform Power:          0.246
#'   #
#'   #Confidence Interval Type:        two-sided
#'   #
#'   #Confidence Level:                95%
#'   #
#'   #Confidence Interval:             LCL = 3.361652
#'   #                                 UCL = 6.746794
#'
#'   # Clean up
#'   rm(dat)
#'
#'   #====================================================================
#'
#'   # Using the reference area TcCB data in EPA.94b.tccb.df, assume a
#'   # gamma distribution, estimate the parameters based on the
#'   # bias-corrected mle of shape, and compute a one-sided upper 90%
#'   # confidence interval for the mean.
#'
#'   #----------
#'   # First test to see whether the data appear to follow a gamma
#'   # distribution.
#'
#'   with(EPA.94b.tccb.df,
#'     gofTest(TcCB[Area == "Reference"], dist = "gamma",
#'       est.arg.list = list(method = "bcmle"))
#'   )
#'
#'   #Results of Goodness-of-Fit Test
#'   #-------------------------------
#'   #
#'   #Test Method:                     Shapiro-Wilk GOF Based on
#'   #                                 Chen & Balakrisnan (1995)
#'   #
#'   #Hypothesized Distribution:       Gamma
#'   #
#'   #Estimated Parameter(s):          shape = 4.5695247
#'   #                                 scale = 0.1309788
#'   #
#'   #Estimation Method:               bcmle
#'   #
#'   #Data:                            TcCB[Area == "Reference"]
#'   #
#'   #Sample Size:                     47
#'   #
#'   #Test Statistic:                  W = 0.9703827
#'   #
#'   #Test Statistic Parameter:        n = 47
#'   #
#'   #P-value:                         0.2739512
#'   #
#'   #Alternative Hypothesis:          True cdf does not equal the
#'   #                                 Gamma Distribution.
#'
#'   #----------
#'   # Now estimate the paramters and compute the upper confidence limit.
#'
#'   with(EPA.94b.tccb.df,
#'     egamma(TcCB[Area == "Reference"], method = "bcmle", ci = TRUE,
#'       ci.type = "upper", conf.level = 0.9)
#'   )
#'
#'   #Results of Distribution Parameter Estimation
#'   #--------------------------------------------
#'   #
#'   #Assumed Distribution:            Gamma
#'   #
#'   #Estimated Parameter(s):          shape = 4.5695247
#'   #                                 scale = 0.1309788
#'   #
#'   #Estimation Method:               bcmle
#'   #
#'   #Data:                            TcCB[Area == "Reference"]
#'   #
#'   #Sample Size:                     47
#'   #
#'   #Confidence Interval for:         mean
#'   #
#'   #Confidence Interval Method:      Optimum Power Normal Approximation
#'   #                                 of Kulkarni & Powar (2010)
#'   #                                 using bcmle of 'shape'
#'   #
#'   #Normal Transform Power:          0.246
#'   #
#'   #Confidence Interval Type:        upper
#'   #
#'   #Confidence Level:                90%
#'   #
#'   #Confidence Interval:             LCL = 0.0000000
#'   #                                 UCL = 0.6561838
#'
#'   #------------------------------------------------------------------
#'
#'   # Repeat the above example but use the alternative parameterization.
#'
#'   with(EPA.94b.tccb.df,
#'     egammaAlt(TcCB[Area == "Reference"], method = "bcmle", ci = TRUE,
#'       ci.type = "upper", conf.level = 0.9)
#'   )
#'
#'   #Results of Distribution Parameter Estimation
#'   #--------------------------------------------
#'   #
#'   #Assumed Distribution:            Gamma
#'   #
#'   #Estimated Parameter(s):          mean = 0.5985106
#'   #                                 cv   = 0.4678046
#'   #
#'   #Estimation Method:               bcmle of 'shape'
#'   #
#'   #Data:                            TcCB[Area == "Reference"]
#'   #
#'   #Sample Size:                     47
#'   #
#'   #Confidence Interval for:         mean
#'   #
#'   #Confidence Interval Method:      Optimum Power Normal Approximation
#'   #                                 of Kulkarni & Powar (2010)
#'   #                                 using bcmle of 'shape'
#'   #
#'   #Normal Transform Power:          0.246
#'   #
#'   #Confidence Interval Type:        upper
#'   #
#'   #Confidence Level:                90%
#'   #
#'   #Confidence Interval:             LCL = 0.0000000
#'   #                                 UCL = 0.6561838
#'
#'   #------------------------------------------------------------------
#'
#'   # Compare the upper confidence limit based on
#'   # 1) the default method:
#'   #    normal approximation method based on Kulkarni and Powar (2010)
#'   # 2) Profile Likelihood
#'   # 3) Chi-Square Approximation
#'   # 4) Chi-Square Adjusted
#'
#'   # Default Method
#'   #---------------
#'   with(EPA.94b.tccb.df,
#'     egamma(TcCB[Area == "Reference"], method = "bcmle", ci = TRUE,
#'       ci.type = "upper", conf.level = 0.9)$interval$limits["UCL"]
#'   )
#'
#'   #      UCL
#'   #0.6561838
#'
#'   # Profile Likelihood
#'   #-------------------
#'   with(EPA.94b.tccb.df,
#'     egamma(TcCB[Area == "Reference"], method = "mle", ci = TRUE,
#'       ci.type = "upper", conf.level = 0.9,
#'       ci.method = "profile.likelihood")$interval$limits["UCL"]
#'   )
#'
#'   #      UCL
#'   #0.6527009
#'
#'
#'   # Chi-Square Approximation
#'   #-------------------------
#'   with(EPA.94b.tccb.df,
#'     egamma(TcCB[Area == "Reference"], method = "mle", ci = TRUE,
#'       ci.type = "upper", conf.level = 0.9,
#'       ci.method = "chisq.approx")$interval$limits["UCL"]
#'   )
#'
#'   #      UCL
#'   #0.6532188
#'
#'
#'   # Chi-Square Adjusted
#'   #--------------------
#'   with(EPA.94b.tccb.df,
#'     egamma(TcCB[Area == "Reference"], method = "mle", ci = TRUE,
#'       ci.type = "upper", conf.level = 0.9,
#'       ci.method = "chisq.adj")$interval$limits["UCL"]
#'   )
#'
#'   #    UCL
#'   #0.65467
#'
#' }
#' @rawRd
#' \keyword{ distribution }
#' @rawRd
#' \keyword{ htest }

egamma <-
function (x, method = "mle", ci = FALSE, ci.type = "two-sided", 
    ci.method = "normal.approx", normal.approx.transform = "kulkarni.powar", 
    conf.level = 0.95) 
{
    if (!is.vector(x, mode = "numeric") || is.factor(x)) 
        stop("'x' must be a numeric vector")
    data.name <- deparse(substitute(x))
    if ((bad.obs <- sum(!(x.ok <- is.finite(x)))) > 0) {
        is.not.finite.warning(x)
        x <- x[x.ok]
        warning(paste(bad.obs, "observations with NA/NaN/Inf in 'x' removed."))
    }
    n <- length(x)
    if (n < 2 || any(x < 0) || length(unique(x)) < 2) 
        stop(paste("'x' must contain at least 2 non-missing distinct values,", 
            "and all non-missing values of x must be non-negative."))
    method <- match.arg(method, c("mle", "bcmle", "mme", "mmue"))
    m <- mean(x)
    s <- sqrt((n - 1)/n) * sd(x)
    shape <- (m/s)^2
    if (method != "mme") {
        shape <- switch(method, mmue = (m/(sqrt(n/(n - 1)) * 
            s))^2, bcmle = , mle = {
            msf <- function(shape, lmx, mlx) {
                (log(shape) - digamma(shape) - lmx + mlx)^2
            }
            nlminb(start = shape, objective = msf, lower = .Machine$double.eps, 
                lmx = log(m), mlx = mean(log(x)))$par
        })
    }
    if (method == "bcmle") {
        shape <- ((n - 3)/n) * shape + (2/(3 * n))
    }
    scale <- m/shape
    method.string <- switch(method, mle = "MLE", bcmle = "Bias-Corrected MLE", 
        mme = "Method of Moments", mmue = paste("Method of Moments Based on\n", 
            space(33), "Unbiased Variance Estimate", sep = ""))
    ret.list <- list(distribution = "Gamma", sample.size = n, 
        parameters = c(shape = shape, scale = scale), method = method.string, 
        data.name = data.name, bad.obs = bad.obs)
    if (ci) {
        ci.type <- match.arg(ci.type, c("two-sided", "lower", 
            "upper"))
        ci.method <- match.arg(ci.method, c("normal.approx", 
            "chisq.approx", "chisq.adj", "profile.likelihood"))
        if (ci.method == "profile.likelihood") {
            if (method != "mle") 
                stop("When ci.method=\"profile.likelihood\" you must set method=\"mle\"")
        }
        normal.approx.transform <- match.arg(normal.approx.transform, 
            c("kulkarni.powar", "cube.root", "fourth.root"))
        if (conf.level <= 0 || conf.level >= 1) 
            stop("The value of 'conf.level' must be between 0 and 1.")
        if (ci.method %in% c("normal.approx", "profile.likelihood")) {
            ci.obj <- ci.gamma.normal.approx(x = x, shape = shape, 
                shape.est.method = method, ci.type = ci.type, 
                conf.level = conf.level, normal.approx.transform = normal.approx.transform)
            if (ci.method == "profile.likelihood") {
                limits <- ci.obj$limits
                names(limits) <- NULL
                ci.obj <- ci.gamma.profile.likelihood(x = x, 
                  shape.mle = shape, scale.mle = scale, ci.type = ci.type, 
                  conf.level = conf.level, LCL.start = limits[1], 
                  UCL.start = limits[2])
            }
        }
        else if (ci.method == "chisq.approx") {
            ci.obj <- ci.gamma.chisq.approx(x = x, shape = shape, 
                shape.est.method = method, ci.type = ci.type, 
                conf.level = conf.level)
        }
        else {
            if (n < 5 || conf.level > (1 - 0.005) || conf.level < 
                (1 - 0.25)) 
                stop(paste("When ci.method='chisq.adj' x", "must contain at least 5 non-missing values, and", 
                  "conf.level must be between 0.75 and 0.995."))
            if (n == 5 && conf.level >= 0.99) 
                stop(paste("When ci.method='chisq.adj' and the sample size is 5,", 
                  "conf.level must be less than 0.99."))
            ci.obj <- ci.gamma.chisq.adj(x = x, shape = shape, 
                shape.est.method = method, ci.type = ci.type, 
                conf.level = conf.level)
        }
        ret.list <- c(ret.list, list(interval = ci.obj))
    }
    oldClass(ret.list) <- "estimate"
    ret.list
}
