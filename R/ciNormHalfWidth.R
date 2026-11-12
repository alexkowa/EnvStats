#' Half-Width of Confidence Interval for Normal Distribution Mean or Difference Between Two Means
#' @description
#' Compute the half-width of a confidence interval for the mean of a normal
#'   distribution or the difference between two means, given the sample size(s),
#'   estimated standard deviation, and confidence level.
#' @usage
#' ciNormHalfWidth(n.or.n1, n2 = n.or.n1,
#'     sigma.hat = 1, conf.level = 0.95,
#'     sample.type = ifelse(missing(n2), "one.sample", "two.sample"))
#' @rawRd
#' \arguments{
#'   \item{n.or.n1}{
#'   numeric vector of sample sizes.  When \code{sample.type="one.sample"},
#'   this argument denotes \eqn{n}, the number of observations in the single sample.
#'   When \code{sample.type="two.sample"}, this argument denotes \eqn{n_1},
#'   the number of observations from group 1.
#'   Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) values are not allowed.
#' }
#'   \item{n2}{
#'   numeric vector of sample sizes for group 2.  The default value is the value of \code{n.or.n1}.
#'   This argument is ignored when \code{sample.type="one.sample"}.
#'   Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) values are not allowed.
#' }
#'   \item{sigma.hat}{
#'   numeric vector specifying the value(s) of the estimated standard deviation(s).
#' }
#'   \item{conf.level}{
#'   numeric vector of numbers between 0 and 1 indicating the confidence level
#'   associated with the confidence interval(s).  The default value is \code{conf.level=0.95}.
#' }
#'   \item{sample.type}{
#'   character string indicating whether this is a one-sample \cr
#'   (\code{sample.type="one.sample"}) or two-sample \cr
#'   (\code{sample.type="two.sample"}) confidence interval.  \cr
#'   When \code{sample.type="one.sample"}, the computed half-width is based on
#'   a confidence interval for a single mean.  \cr
#'   When \code{sample.type="two.sample"}, the computed half-width is based on
#'   a confidence interval for the difference between two means.  \cr
#'   The default value is \code{sample.type="one.sample"} unless the argument
#'   \code{n2} is supplied.
#' }
#' }
#' @rawRd
#' \details{
#'   If the arguments \code{n.or.n1}, \code{n2}, \code{sigma.hat}, and
#'   \code{conf.level} are not all the same length, they are replicated to be the same length
#'   as the length of the longest argument.
#'
#'   \emph{One-Sample Case} (\code{sample.type="one.sample"}) \cr
#'   Let \eqn{\underline{x} = x_1, x_2, \ldots, x_n} denote a vector of \eqn{n}
#'   observations from a normal distribution with mean \eqn{\mu} and standard deviation
#'   \eqn{\sigma}.  A two-sided \eqn{(1-\alpha)100\%} confidence interval for \eqn{\mu}
#'   is given by:
#'   \deqn{[\hat{\mu} - t(n-1, 1-\alpha/2) \frac{\hat{\sigma}}{\sqrt{n}}, \, \hat{\mu} + t(n-1, 1-\alpha/2) \frac{\hat{\sigma}}{\sqrt{n}}] \;\;\;\;\;\; (1)}
#'   where
#'   \deqn{\hat{\mu} = \bar{x} = \frac{1}{n} \sum_{i=1}^n x_i \;\;\;\;\;\; (2)}
#'   \deqn{\hat{\sigma}^2 = s^2 = \frac{1}{n-1} \sum_{i=1}^n (x_i - \bar{x})^2 \;\;\;\;\;\; (3)}
#'   and \eqn{t(\nu, p)} is the \eqn{p}'th quantile of
#'   \link[stats:TDist]{Student's t-distribution} with \eqn{\nu} degrees of freedom
#'   (Zar, 2010; Gilbert, 1987; Ott, 1995; Helsel and Hirsch, 1992).  Thus, the
#'   half-width of this confidence interval is given by:
#'   \deqn{HW = t(n-1, 1-\alpha/2) \frac{\hat{\sigma}}{\sqrt{n}} \;\;\;\;\;\; (4)}
#'
#'   \emph{Two-Sample Case} (\code{sample.type="two.sample"}) \cr
#'   Let \eqn{\underline{x}_1 = x_{11}, x_{12}, \ldots, x_{1n_1}} denote a vector of
#'   \eqn{n_1} observations from a normal distribution with mean \eqn{\mu_1} and
#'   standard deviation \eqn{\sigma}, and let
#'   \eqn{\underline{x}_2 = x_{21}, x_{22}, \ldots, x_{2n_2}} denote a vector of
#'   \eqn{n_2} observations from a normal distribution with mean \eqn{\mu_2} and
#'   standard deviation \eqn{\sigma}.  A two-sided \eqn{(1-\alpha)100\%} confidence
#'   interval for \eqn{\mu_1 - \mu_2} is given by:
#'   \deqn{[(\hat{\mu}_1 - \hat{\mu}_2) - t(n_1 + n_2 - 2, 1-\alpha/2) \hat{\sigma} \sqrt{\frac{1}{n_1} + \frac{1}{n_2}}, \, (\hat{\mu}_1 - \hat{\mu}_2) + t(n_1 + n_2 - 2, 1-\alpha/2) \hat{\sigma} \sqrt{\frac{1}{n_1} + \frac{1}{n_2}}] \;\;\;\;\;\; (5)}
#'   where
#'   \deqn{\hat{\mu}_1 = \bar{x}_1 = \frac{1}{n_1} \sum_{i=1}^{n_1} x_{1i} \;\;\;\;\;\; (6)}
#'   \deqn{\hat{\mu}_2 = \bar{x}_2 = \frac{1}{n_2} \sum_{i=1}^{n_2} x_{2i} \;\;\;\;\;\; (7)}
#'   \deqn{\hat{\sigma}^2 = s_p^2 = \frac{(n_1 - 1) s_1^2 + (n_2 - 1) s_2^2}{n_1 + n_2 - 2} \;\;\;\;\;\; (8)}
#'   \deqn{s_1^2 = \frac{1}{n_1 - 1} \sum_{i=1}^{n_1} (x_{1i} - \bar{x}_1)^2 \;\;\;\;\;\; (9)}
#'   \deqn{s_2^2 = \frac{1}{n_2 - 1} \sum_{i=1}^{n_2} (x_{2i} - \bar{x}_2)^2 \;\;\;\;\;\; (10)}
#'   (Zar, 2010, p.142; Helsel and Hirsch, 1992, p.135,
#'   Berthouex and Brown, 2002, pp.157--158).  Thus, the half-width of this confidence
#'   interval is given by:
#'   \deqn{HW = t(n_1 + n_2 - 2, 1-\alpha/2) \hat{\sigma} \sqrt{\frac{1}{n_1} + \frac{1}{n_2}} \;\;\;\;\;\; (11)}
#'   Note that for the two-sample case, the function \code{ciNormHalfWidth} assumes the
#'   two populations have the same standard deviation.
#' }
#' @rawRd
#' \value{
#'   a numeric vector of half-widths.
#' }
#' @rawRd
#' \references{
#'   Berthouex, P.M., and L.C. Brown. (2002).
#'   \emph{Statistics for Environmental Engineers}.  Second Edition.
#'   Lewis Publishers, Boca Raton, FL.
#'
#'   Gilbert, R.O. (1987). \emph{Statistical Methods for Environmental Pollution Monitoring}.
#'   Van Nostrand Reinhold, New York, NY.
#'
#'   Helsel, D.R., and R.M. Hirsch. (1992).
#'   \emph{Statistical Methods in Water Resources Research}.
#'   Elsevier, New York, NY, Chapter 7.
#'
#'   Millard, S.P., and N. Neerchal. (2001).  \emph{Environmental Statistics with S-PLUS}.
#'   CRC Press, Boca Raton, FL.
#'
#'   Ott, W.R. (1995). \emph{Environmental Statistics and Data Analysis}.
#'   Lewis Publishers, Boca Raton, FL.
#'
#'   USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
#'   EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.
#'   U.S. Environmental Protection Agency, Washington, D.C. p.21-3.
#'
#'   Zar, J.H. (2010). \emph{Biostatistical Analysis}.
#'   Fifth Edition. Prentice-Hall, Upper Saddle River, NJ,
#'   Chapters 7 and 8.
#' }
#' @rawRd
#' \author{
#'   Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \note{
#'   The normal distribution and lognormal distribution are probably the two most frequently used
#'   distributions to model environmental data.  In order to make any kind of probability
#'   statement about a normally-distributed population (of chemical concentrations for example),
#'   you have to first estimate the mean and standard deviation (the population parameters) of the
#'   distribution.  Once you estimate these parameters, it is often useful to characterize the
#'   uncertainty in the estimate of the mean.  This is done with confidence intervals.
#'
#'   In the course of designing a sampling program, an environmental scientist may wish to determine
#'   the relationship between sample size, confidence level, and half-width if one of the objectives
#'   of the sampling program is to produce confidence intervals.  The functions
#'   \code{ciNormHalfWidth}, \code{\link{ciNormN}}, and \code{\link{plotCiNormDesign}}
#'   can be used to investigate these relationships for the case of normally-distributed observations.
#' }
#' @rawRd
#' \seealso{
#'   \code{\link{ciNormN}}, \code{\link{plotCiNormDesign}}, \code{\link{Normal}},
#'   \code{\link{enorm}}, \code{\link{t.test}} \cr
#'   \link{Estimating Distribution Parameters}.
#' }
#' @rawRd
#' \examples{
#'   # Look at how the half-width of a one-sample confidence interval
#'   # decreases with increasing sample size:
#'
#'   seq(5, 30, by = 5)
#'   #[1] 5 10 15 20 25 30
#'
#'   hw <- ciNormHalfWidth(n.or.n1 = seq(5, 30, by = 5))
#'
#'   round(hw, 2)
#'   #[1] 1.24 0.72 0.55 0.47 0.41 0.37
#'
#'   #----------------------------------------------------------------
#'
#'   # Look at how the half-width of a one-sample confidence interval
#'   # increases with increasing estimated standard deviation:
#'
#'   seq(0.5, 2, by = 0.5)
#'   #[1] 0.5 1.0 1.5 2.0
#'
#'   hw <- ciNormHalfWidth(n.or.n1 = 20, sigma.hat = seq(0.5, 2, by = 0.5))
#'
#'   round(hw, 2)
#'   #[1] 0.23 0.47 0.70 0.94
#'
#'   #----------------------------------------------------------------
#'
#'   # Look at how the half-width of a one-sample confidence interval
#'   # increases with increasing confidence level:
#'
#'   seq(0.5, 0.9, by = 0.1)
#'   #[1] 0.5 0.6 0.7 0.8 0.9
#'
#'   hw <- ciNormHalfWidth(n.or.n1 = 20, conf.level = seq(0.5, 0.9, by = 0.1))
#'
#'   round(hw, 2)
#'   #[1] 0.15 0.19 0.24 0.30 0.39
#'
#'   #==========
#'
#'   # Modifying the example on pages 21-4 to 21-5 of USEPA (2009),
#'   # determine how adding another four months of observations to
#'   # increase the sample size from 4 to 8 will affect the half-width
#'   # of a two-sided 95% confidence interval for the Aldicarb level at
#'   # the first compliance well.
#'   #
#'   # Use the estimated standard deviation from the first four months
#'   # of data.  (The data are stored in EPA.09.Ex.21.1.aldicarb.df.)
#'   # Note that the half-width changes from 34% of the observed mean to
#'   # 18% of the observed mean by increasing the sample size from
#'   # 4 to 8.
#'
#'   EPA.09.Ex.21.1.aldicarb.df
#'   #   Month   Well Aldicarb.ppb
#'   #1      1 Well.1         19.9
#'   #2      2 Well.1         29.6
#'   #3      3 Well.1         18.7
#'   #4      4 Well.1         24.2
#'   #...
#'
#'   mu.hat <- with(EPA.09.Ex.21.1.aldicarb.df,
#'     mean(Aldicarb.ppb[Well=="Well.1"]))
#'
#'   mu.hat
#'   #[1] 23.1
#'
#'   sigma.hat <- with(EPA.09.Ex.21.1.aldicarb.df,
#'     sd(Aldicarb.ppb[Well=="Well.1"]))
#'
#'   sigma.hat
#'   #[1] 4.93491
#'
#'   hw.4 <- ciNormHalfWidth(n.or.n1 = 4, sigma.hat = sigma.hat)
#'
#'   hw.4
#'   #[1] 7.852543
#'
#'   hw.8 <- ciNormHalfWidth(n.or.n1 = 8, sigma.hat = sigma.hat)
#'
#'   hw.8
#'   #[1] 4.125688
#'
#'   100 * hw.4/mu.hat
#'   #[1] 33.99369
#'
#'   100 * hw.8/mu.hat
#'   #[1] 17.86012
#'
#'   #==========
#'
#'   # Clean up
#'   #---------
#'   rm(hw, mu.hat, sigma.hat, hw.4, hw.8)
#' }
#' @rawRd
#' \keyword{ distribution }
#' @rawRd
#' \keyword{ design }
#' @rawRd
#' \keyword{ htest }

ciNormHalfWidth <-
function (n.or.n1, n2 = n.or.n1, sigma.hat = 1, conf.level = 0.95, 
    sample.type = ifelse(missing(n2), "one.sample", "two.sample")) 
{
    sample.type <- match.arg(sample.type, c("one.sample", "two.sample"))
    if (!is.vector(n.or.n1, mode = "numeric") || !is.vector(sigma.hat, 
        mode = "numeric") || !is.vector(conf.level, mode = "numeric")) 
        stop("'n.or.n1', 'sigma.hat', and 'conf.level' must be numeric vectors.")
    if (!all(is.finite(n.or.n1)) || !all(is.finite(sigma.hat)) || 
        !all(is.finite(conf.level))) 
        stop(paste("Missing (NA), Infinite (Inf, -Inf), and", 
            "Undefined (Nan) values are not allowed in", "'n.or.n1', 'sigma.hat', or 'conf.level'"))
    if (any(n.or.n1 < 2)) 
        stop("All values of 'n.or.n1' must be greater than or equal to 2")
    if (any(sigma.hat < .Machine$double.eps)) 
        stop("All values of 'sigma.hat' must be positive.")
    if (any(conf.level <= .Machine$double.eps) || any(conf.level >= 
        1 - .Machine$double.eps)) 
        stop("All values of 'conf.level' must be greater than 0 and less than 1")
    if (sample.type == "two.sample") {
        if (!missing(n2)) {
            if (!is.vector(n2, mode = "numeric")) 
                stop("'n2' must be a numeric vector")
            if (!all(is.finite(n2))) 
                stop(paste("Missing (NA), Infinite (Inf, -Inf), and", 
                  "Undefined (Nan) values are not allowed in 'n2'"))
            if (any(n2 < 2)) 
                stop("All values of 'n2' must be greater than or equal to 2")
        }
        half.width <- qt(1 - (1 - conf.level)/2, n.or.n1 + n2 - 
            2) * sigma.hat * sqrt(1/n.or.n1 + 1/n2)
    }
    else half.width <- (qt(1 - (1 - conf.level)/2, n.or.n1 - 
        1) * sigma.hat)/sqrt(n.or.n1)
    names(half.width) <- NULL
    half.width
}
