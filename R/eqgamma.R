#' Estimate Quantiles of a Gamma Distribution
#' @aliases eqgammaAlt
#' @description
#' Estimate quantiles of a \link[stats:GammaDist]{gamma distribution}, and
#'   optionally construct a confidence interval for a quantile.
#' @usage
#' eqgamma(x, p = 0.5, method = "mle", ci = FALSE,
#'     ci.type = "two-sided", conf.level = 0.95,
#'     normal.approx.transform = "kulkarni.powar", digits = 0)
#'
#'   eqgammaAlt(x, p = 0.5, method = "mle", ci = FALSE,
#'     ci.type = "two-sided", conf.level = 0.95,
#'     normal.approx.transform = "kulkarni.powar", digits = 0)
#' @rawRd
#' \arguments{
#'   \item{x}{
#'   a numeric vector of observations, or an object resulting from a call to an
#'   estimating function that assumes a gamma distribution
#'   (e.g., \code{\link{egamma}} or \code{\link{egammaAlt}}).
#'   If \code{ci=TRUE} then \code{x} must be a
#'   numeric vector of observations.  If \code{x} is a numeric vector,
#'   missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf})
#'   values are allowed but will be removed.
#' }
#'   \item{p}{
#'   numeric vector of probabilities for which quantiles will be estimated.
#'   All values of \code{p} must be between 0 and 1.  When \code{ci=TRUE}, \code{p}
#'   must be a scalar. The default value is \code{p=0.5}.
#' }
#'   \item{method}{
#'   character string specifying the method to use to estimate the shape and scale
#'   parameters of the distribution.  The possible values are
#'   \code{"mle"} (maximum likelihood; the default), \code{"bcmle"} (bias-corrected mle),
#'   \code{"mme"} (method of moments), and
#'   \code{"mmue"} (method of moments based on the unbiased estimator of variance).
#'   See the DETAILS section of the help file for \code{\link{egamma}} for more information.
#' }
#'   \item{ci}{
#'   logical scalar indicating whether to compute a confidence interval for the quantile.
#'   The default value is \code{ci=FALSE}.
#' }
#'   \item{ci.type}{
#'   character string indicating what kind of confidence interval for the quantile to compute.
#'   The possible values are \code{"two-sided"} (the default), \code{"lower"}, and
#'   \code{"upper"}.  This argument is ignored if \code{ci=FALSE}.
#' }
#'   \item{conf.level}{
#'   a scalar between 0 and 1 indicating the confidence level of the confidence interval.
#'   The default value is \code{conf.level=0.95}.  This argument is ignored if \code{ci=FALSE}.
#' }
#'   \item{normal.approx.transform}{
#'   character string indicating which power transformation to use.
#'   Possible values are \code{"kulkarni.powar"} (the default), \code{"cube.root"}, and \cr
#'   \code{"fourth.root"}.  See the DETAILS section for more informaiton.
#'   This argument is ignored if \code{ci=FALSE}.
#' }
#'   \item{digits}{
#'   an integer indicating the number of decimal places to round to when printing out
#'   the value of \code{100*p}. The default value is \code{digits=0}.
#' }
#' }
#' @rawRd
#' \details{
#'   The function \code{eqgamma} returns estimated quantiles as well as
#'   estimates of the shape and scale parameters.
#'   The function \code{eqgammaAlt} returns estimated quantiles as well as
#'   estimates of the mean and coefficient of variation.
#'
#'   Quantiles are estimated by 1) estimating the shape and scale parameters by
#'   calling \code{\link{egamma}}, and then 2) calling the function
#'   \code{\link[stats:GammaDist]{qgamma}} and using the estimated values for shape
#'   and scale.
#'
#'   The confidence interval for a quantile is computed by:
#'   \enumerate{
#'     \item using a power transformation on the original data to induce approximate
#'       normality,
#'     \item using \code{\link[stats:Normal]{eqnorm}} to compute the confidence interval,
#'     and then
#'     \item back-transforming the interval to create a confidence interval on the original
#'     scale.
#'   }
#'   This is similar to what is done to create tolerance intervals for a gamma distribuiton
#'   (Krishnamoorthy et al., 2008), and there is a one-to-one relationship between confidence
#'   intervals for a quantile and tolerance intervals (see the DETAILS section of the
#'   help file for \code{\link{eqnorm}}).  The value \code{normal.approx.transform="cube.root"}
#'   uses the cube root transformation suggested by Wilson and Hilferty (1931) and used by
#'   Krishnamoorthy et al. (2008) and Singh et al. (2010b), and the value
#'   \code{normal.approx.transform="fourth.root"} uses the fourth root transformation suggested
#'   by Hawkins and Wixley (1986) and used by Singh et al. (2010b).
#'   The default value \code{normal.approx.transform="kulkarni.powar"}
#'   uses the \dQuote{Optimum Power Normal Approximation Method} of Kulkarni and Powar (2010).
#'   The \dQuote{optimum} power \eqn{r} is determined by:
#'   \tabular{ll}{
#'   \eqn{r = -0.0705 - 0.178 \, shape + 0.475 \, \sqrt{shape}} \tab if \eqn{shape \le 1.5} \cr
#'   \eqn{r = 0.246} \tab if \eqn{shape > 1.5} \cr
#'   }
#'   where \eqn{shape} denotes the estimate of the shape parameter.  Although
#'   Kulkarni and Powar (2010) use the maximum likelihood estimate of shape to
#'   determine the power \eqn{r}, for the functions \code{eqgamma} and
#'   \code{eqgammaAlt} the power \eqn{r} is based on whatever estimate of
#'   shape is used \cr
#'   (e.g., \code{method="mle"}, \code{method="bcmle"}, etc.).
#' }
#' @rawRd
#' \value{
#'   If \code{x} is a numeric vector, \code{eqgamma} and \code{eqgammaAlt} return a
#'   list of class \code{"estimate"} containing the estimated quantile(s) and other
#'   information. See \code{\link{estimate.object}} for details.
#'
#'   If \code{x} is the result of calling an estimation function, \code{eqgamma} and
#'   \code{eqgammaAlt} return a list whose class is the same as \code{x}.  The list
#'   contains the same components as \code{x}, as well as components called
#'   \code{quantiles} and \code{quantile.method}.  In addition, if \code{ci=TRUE},
#'   the returned list contains a component called \code{interval} containing the
#'   confidence interval information.  If \code{x} already has a component called
#'   \code{interval}, this component is replaced with the confidence interval information.
#' }
#' @rawRd
#' \references{
#'   Berthouex, P.M., and L.C. Brown. (2002). \emph{Statistics for Environmental Engineers}.
#'   Lewis Publishers, Boca Raton.
#'
#'   Conover, W.J. (1980). \emph{Practical Nonparametric Statistics}. Second Edition.
#'   John Wiley and Sons, New York.
#'
#'   Forbes, C., M. Evans, N. Hastings, and B. Peacock. (2011).  Statistical Distributions.
#'   Fourth Edition. John Wiley and Sons, Hoboken, NJ.
#'
#'   Gibbons, R.D., D.K. Bhaumik, and S. Aryal. (2009).
#'   \emph{Statistical Methods for Groundwater Monitoring}, Second Edition.
#'   John Wiley & Sons, Hoboken.
#'
#'   Hawkins, D. M., and R.A.J. Wixley. (1986). A Note on the Transformation of
#'   Chi-Squared Variables to Normality. \emph{The American Statistician}, \bold{40},
#'   296--298.
#'
#'   Johnson, N.L., S. Kotz, and N. Balakrishnan. (1994).
#'   \emph{Continuous Univariate Distributions, Volume 1}. Second Edition.
#'   John Wiley and Sons, New York, Chapter 17.
#'
#'   Krishnamoorthy K., T. Mathew, and S. Mukherjee. (2008). Normal-Based Methods for a
#'   Gamma Distribution: Prediction and Tolerance Intervals and Stress-Strength Reliability.
#'   \emph{Technometrics}, \bold{50}(1), 69--78.
#'
#'   Krishnamoorthy K., and T. Mathew. (2009).
#'   \emph{Statistical Tolerance Regions: Theory, Applications, and Computation}.
#'   John Wiley and Sons, Hoboken.
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
#'   Singh, A., R. Maichle, and N. Armbya. (2010a).
#'   \emph{ProUCL Version 4.1.00 User Guide (Draft)}. EPA/600/R-07/041, May 2010.
#'   Office of Research and Development, U.S. Environmental Protection Agency, Washington, D.C.
#'
#'   Singh, A., N. Armbya, and A. Singh. (2010b).
#'   \emph{ProUCL Version 4.1.00 Technical Guide (Draft)}. EPA/600/R-07/041, May 2010.
#'   Office of Research and Development, U.S. Environmental Protection Agency, Washington, D.C.
#'
#'   Wilson, E.B., and M.M. Hilferty. (1931). The Distribution of Chi-Squares.
#'   \emph{Proceedings of the National Academy of Sciences}, \bold{17}, 684--688.
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
#'   The gamma distribution takes values on the positive real line.
#'   Special cases of the gamma are the \link[=Exponential]{exponential} distribution and
#'   the \link[stats:Chisquare]{chi-square distributions}. Applications of the gamma include
#'   life testing, statistical ecology, queuing theory, inventory control, and precipitation
#'   processes. A gamma distribution starts to resemble a normal distribution as the
#'   shape parameter a tends to infinity.
#'
#'   Some EPA guidance documents (e.g., Singh et al., 2002; Singh et al., 2010a,b) strongly
#'   recommend against using a lognormal model for environmental data and recommend trying a
#'   gamma distribuiton instead.
#'
#'   Percentiles are sometimes used in environmental standards and regulations.  For example,
#'   Berthouex and Brown (2002, p.71) note that England has water quality limits based on
#'   the 90th and 95th percentiles of monitoring data not exceeding specified levels.  They also
#'   note that the U.S. EPA has specifications for air quality monitoring, aquatic standards on
#'   toxic chemicals, and maximum daily limits for industrial effluents that are all based on
#'   percentiles.  Given the importance of these quantities, it is essential to characterize
#'   the amount of uncertainty associated with the estimates of these quantities.
#'   This is done with confidence intervals.
#' }
#' @rawRd
#' \seealso{
#'   \code{\link{egamma}}, \code{\link[stats:GammaDist]{GammaDist}},
#'   \code{\link{estimate.object}}, \code{\link{eqnorm}}, \code{\link{tolIntGamma}}.
#' }
#' @rawRd
#' \examples{
#'   # Generate 20 observations from a gamma distribution with parameters
#'   # shape=3 and scale=2, then estimate the 90th percentile and create
#'   # a one-sided upper 95% confidence interval for that percentile.
#'   # (Note: the call to set.seed simply allows you to reproduce this
#'   # example.)
#'
#'   set.seed(250)
#'   dat <- rgamma(20, shape = 3, scale = 2)
#'   eqgamma(dat, p = 0.9, ci = TRUE, ci.type = "upper")
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
#'   #Estimated Quantile(s):           90'th %ile = 9.113446
#'   #
#'   #Quantile Estimation Method:      Quantile(s) Based on
#'   #                                 mle Estimators
#'   #
#'   #Data:                            dat
#'   #
#'   #Sample Size:                     20
#'   #
#'   #Confidence Interval for:         90'th %ile
#'   #
#'   #Confidence Interval Method:      Exact using
#'   #                                 Kulkarni & Powar (2010)
#'   #                                 transformation to Normality
#'   #                                 based on mle of 'shape'
#'   #
#'   #Confidence Interval Type:        upper
#'   #
#'   #Confidence Level:                95%
#'   #
#'   #Confidence Interval:             LCL =  0.00000
#'   #                                 UCL = 13.79733
#'
#'   #----------
#'   # Compare these results with the true 90'th percentile:
#'
#'   qgamma(p = 0.9, shape = 3, scale = 2)
#'   #[1] 10.64464
#'
#'   #----------
#'
#'   # Using the same data as in the previous example, use egammaAlt
#'   # to estimate the mean and cv based on the bias-corrected
#'   # estimate of shape, and use the cube-root transformation to
#'   # normality.
#'
#'   eqgammaAlt(dat, p = 0.9, method = "bcmle", ci = TRUE,
#'     ci.type = "upper", normal.approx.transform = "cube.root")
#'
#'   #Results of Distribution Parameter Estimation
#'   #--------------------------------------------
#'   #
#'   #Assumed Distribution:            Gamma
#'   #
#'   #Estimated Parameter(s):          mean = 4.7932408
#'   #                                 cv   = 0.7242165
#'   #
#'   #Estimation Method:               bcmle of 'shape'
#'   #
#'   #Estimated Quantile(s):           90'th %ile = 9.428
#'   #
#'   #Quantile Estimation Method:      Quantile(s) Based on
#'   #                                 bcmle of 'shape'
#'   #
#'   #Data:                            dat
#'   #
#'   #Sample Size:                     20
#'   #
#'   #Confidence Interval for:         90'th %ile
#'   #
#'   #Confidence Interval Method:      Exact using
#'   #                                 Wilson & Hilferty (1931) cube-root
#'   #                                 transformation to Normality
#'   #
#'   #Confidence Interval Type:        upper
#'   #
#'   #Confidence Level:                95%
#'   #
#'   #Confidence Interval:             LCL =  0.00000
#'   #                                 UCL = 12.89643
#'
#'   #----------
#'
#'   # Clean up
#'   rm(dat)
#'
#'   #--------------------------------------------------------------------
#'
#'   # Example 17-3 of USEPA (2009, p. 17-17) shows how to construct a
#'   # beta-content upper tolerance limit with 95% coverage and
#'   # 95% confidence  using chrysene data and assuming a lognormal
#'   # distribution.  Here we will use the same chrysene data but assume a
#'   # gamma distribution.
#'
#'   # A beta-content upper tolerance limit with 95% coverage and
#'   # 95% confidence is equivalent to the 95% upper confidence limit for
#'   # the 95th percentile.
#'
#'   attach(EPA.09.Ex.17.3.chrysene.df)
#'   Chrysene <- Chrysene.ppb[Well.type == "Background"]
#'   eqgamma(Chrysene, p = 0.95, ci = TRUE, ci.type = "upper")
#'
#'   #Results of Distribution Parameter Estimation
#'   #--------------------------------------------
#'   #
#'   #Assumed Distribution:            Gamma
#'   #
#'   #Estimated Parameter(s):          shape = 2.806929
#'   #                                 scale = 5.286026
#'   #
#'   #Estimation Method:               mle
#'   #
#'   #Estimated Quantile(s):           95'th %ile = 31.74348
#'   #
#'   #Quantile Estimation Method:      Quantile(s) Based on
#'   #                                 mle Estimators
#'   #
#'   #Data:                            Chrysene
#'   #
#'   #Sample Size:                     8
#'   #
#'   #Confidence Interval for:         95'th %ile
#'   #
#'   #Confidence Interval Method:      Exact using
#'   #                                 Kulkarni & Powar (2010)
#'   #                                 transformation to Normality
#'   #                                 based on mle of 'shape'
#'   #
#'   #Confidence Interval Type:        upper
#'   #
#'   #Confidence Level:                95%
#'   #
#'   #Confidence Interval:             LCL =  0.00000
#'   #                                 UCL = 69.32425
#'
#'   #----------
#'   # Clean up
#'
#'   rm(Chrysene)
#'   detach("EPA.09.Ex.17.3.chrysene.df")
#' }
#' @rawRd
#' \keyword{ distribution }
#' @rawRd
#' \keyword{ htest }

eqgamma <-
function (x, p = 0.5, method = "mle", ci = FALSE, ci.type = "two-sided", 
    conf.level = 0.95, normal.approx.transform = "kulkarni.powar", 
    digits = 0) 
{
    if (!is.vector(p, mode = "numeric")) 
        stop("'p' must be a numeric vector.")
    if (any(!is.finite(p))) 
        stop("NA/NaN/Inf values not allowed in 'p'.")
    if (any(p < 0) || any(p > 1)) 
        stop("All values of 'p' must be between 0 and 1")
    method <- match.arg(method, c("mle", "bcmle", "mme", "mmue"))
    x.is.est.obj <- data.class(x) == "estimate" || data.class(x) == 
        "estimateCensored"
    if (x.is.est.obj) {
        if (ci) 
            stop(paste("When ci=TRUE the argument 'x' must be the raw data,", 
                "not the result of parameter estimation."))
        if (x$distribution != "Gamma") 
            stop(paste("'eqgamma' estimates quantiles for a gamma distribution. ", 
                "You have supplied an object that assumes a different distribution."))
        class.x <- oldClass(x)
        if (!is.null(x$interval)) {
            x <- x[-match("interval", names(x))]
            oldClass(x) <- class.x
        }
        parameters <- x$parameters
        if (names(parameters)[1] == "mean") 
            stop(paste("You supplied the result of parameter estimation", 
                "for the alternative parameterization of the Gamma distribution. ", 
                "Use the function eqgammaAlt."))
        shape <- parameters["shape"]
        scale <- parameters["scale"]
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
        if (n < 2 || any(x < 0) || length(unique(x)) < 2) 
            stop(paste("'x' must contain at least 2 non-missing distinct values,", 
                "and all non-missing values of x must be non-negative. ", 
                "This is not true for 'x' =", data.name))
        ret.list <- egamma(x, method = method)
        ret.list$data.name <- data.name
        ret.list$bad.obs <- bad.obs
        shape <- ret.list$parameters["shape"]
        scale <- ret.list$parameters["scale"]
    }
    q <- qgamma(p, shape = shape, scale = scale)
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
    if (ci) {
        if (length(p) > 1 || p <= 0 || p >= 1) 
            stop(paste("When 'ci' = TRUE, 'p' must be a scalar", 
                "larger than 0 and less than 1."))
        ci.type <- match.arg(ci.type, c("two-sided", "lower", 
            "upper"))
        normal.approx.transform <- match.arg(normal.approx.transform, 
            c("kulkarni.powar", "cube.root", "fourth.root"))
        switch(normal.approx.transform, kulkarni.powar = {
            p.trans <- ifelse(shape > 1.5, 0.246, -0.0705 - 0.178 * 
                shape + 0.475 * sqrt(shape))
            string <- paste("Kulkarni & Powar (2010)\n", space(33), 
                "transformation to Normality\n", space(33), "based on ", 
                method, " of 'shape'", sep = "")
        }, cube.root = {
            p.trans <- 1/3
            string <- paste("Wilson & Hilferty (1931) cube-root\n", 
                space(33), "transformation to Normality", sep = "")
        }, fourth.root = {
            p.trans <- 1/4
            string <- paste("Hawkins & Wixley (1986) fourth-root\n", 
                space(33), "transformation to Normality", sep = "")
        })
        Y <- x^p.trans
        ci.ret.list <- eqnorm(Y, p = p, ci = TRUE, ci.type = ci.type, 
            conf.level = conf.level, digits = digits)$interval
        limits <- ci.ret.list$limits
        if (ci.type == "upper") 
            limits["LCL"] <- 0
        if (ci.type %in% c("two-sided", "lower") & limits["LCL"] < 
            0) {
            limits["LCL"] <- 0
            warning(paste("Normal approximation to Gamma distribution", 
                "to compute lower confidence limit of quantile", 
                "not accurate for this case"))
        }
        ci.ret.list$limits <- limits^(1/p.trans)
        ci.ret.list$method <- paste(ci.ret.list$method, " using\n", 
            space(33), string, sep = "")
        ret.list$interval <- ci.ret.list
    }
    ret.list
}
