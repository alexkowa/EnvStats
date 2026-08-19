#' Half-Width of Confidence Interval for Binomial Proportion or Difference Between Two Proportions
#' @description
#' Compute the half-width of a confidence interval for a binomial proportion or the difference between
#'   two proportions, given the sample size(s), estimated proportion(s), and confidence level.
#' @usage
#' ciBinomHalfWidth(n.or.n1, p.hat.or.p1.hat = 0.5,
#'     n2 = n.or.n1, p2.hat = 0.4, conf.level = 0.95,
#'     sample.type = "one.sample", ci.method = "score",
#'     correct = TRUE, warn = TRUE)
#' @rawRd
#' \arguments{
#'   \item{n.or.n1}{
#'   numeric vector of sample sizes.  \cr
#'   When \code{sample.type="one.sample"}, \code{n.or.n1} denotes \eqn{n},
#'   the number of observations in the single sample. \cr
#'   When \code{sample.type="two.sample"}, \code{n.or.n1} denotes \eqn{n_1},
#'   the number of observations from group 1. \cr
#'   Missing (\code{NA}), undefined (\code{NaN}), and
#'   infinite (\code{Inf}, \code{-Inf}) values are not allowed.
#' }
#'   \item{p.hat.or.p1.hat}{
#'   numeric vector of estimated proportions.  \cr
#'   When \code{sample.type="one.sample"}, \code{p.hat.or.p1.hat} denotes the
#'   estimated value of \eqn{p}, the probability of \dQuote{success}. \cr
#'   When \code{sample.type="two.sample"}, \code{p.hat.or.p1.hat} denotes the
#'   estimated value of \eqn{p_1}, the probability of \dQuote{success} in
#'   group 1.  \cr
#'   Missing (\code{NA}), undefined (\code{NaN}), and infinite
#'   (\code{Inf}, \code{-Inf}) values are not allowed.
#' }
#'   \item{n2}{
#'   numeric vector of sample sizes for group 2.  The default value is the value of \code{n.or.n1}.
#'   This argument is ignored when \code{sample.type="one.sample"}.
#'   Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) values are not allowed.
#' }
#'   \item{p2.hat}{
#'   numeric vector of estimated proportions for group 2.
#'   This argument is ignored when \code{sample.type="one.sample"}.
#'   Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) values are not allowed.
#' }
#'   \item{conf.level}{
#'   numeric vector of numbers between 0 and 1 indicating the confidence level associated with
#'   the confidence interval(s).  The default value is \code{conf.level=0.95}.
#' }
#'   \item{sample.type}{
#'   character string indicating whether this is a one-sample or two-sample confidence interval.
#'   When \code{sample.type="one.sample"}, the computed half-width is based on a confidence interval
#'   for a single proportion.  When \cr
#'   \code{sample.type="two.sample"}, the computed half-width is based
#'   on a confidence interval for the difference between two proportions.
#'   The default value is \code{sample.type="one.sample"} unless the
#'   argument \code{n2} or \code{p2.hat} is supplied.
#' }
#'   \item{ci.method}{
#'   character string indicating which method to use to construct the confidence interval.
#'   Possible values are \code{"score"} (the default), \code{"exact"}, \cr
#'   \code{"adjusted Wald"}, and \code{"Wald"} (the \code{"Wald"} method is
#'   \strong{never} recommended but is included for historical purposes).
#'   The exact method is only available for the one-sample case, i.e.,
#'   when \code{sample.type="one.sample"}.
#' }
#'   \item{correct}{
#'   logical scalar indicating whether to use the continuity correction when \cr
#'   \code{ci.method="score"} or \code{ci.method="Wald"}.  \cr
#'   The default value is \code{correct=TRUE}.
#' }
#'   \item{warn}{
#'   logical scalar indicating whether to issue a warning when \cr
#'   \code{ci.method="Wald"} for cases when the normal approximation to
#'   the binomial distribution probably is not accurate.
#'   The default value is \code{warn=TRUE}.
#' }
#' }
#' @rawRd
#' \details{
#'   If the arguments \code{n.or.n1}, \code{p.hat.or.p1.hat}, \code{n2}, \code{p2.hat}, and
#'   \code{conf.level} are not all the same length, they are replicated to be the same length as
#'   the length of the longest argument.
#'
#'   The values of \code{p.hat.or.p1.hat} and \code{p2.hat} are automatically adjusted
#'   to the closest legitimate values, given the user-supplied values of \code{n.or.n1} and
#'   \code{n2}.  For example, if \code{n.or.n1=5}, legitimate values for
#'   \code{p.hat.or.p1.hat} are 0, 0.2, 0.4, 0.6, 0.8 and 1.  In this case, if the
#'   user supplies \code{p.hat.or.p1.hat=0.45}, then \code{p.hat.or.p1.hat} is reset to \cr
#'   \code{p.hat.or.p1.hat=0.4}, and if the user supplies \code{p.hat.or.p1.hat=0.55}, \cr
#'   then \code{p.hat.or.p1.hat} is reset to \code{p.hat.or.p1.hat=0.6}.  In cases where
#'   the two closest legitimate values are equal distance from the user-suppled value of
#'   \code{p.hat.or.p1.hat} or \code{p2.hat}, the value closest to 0.5 is chosen since
#'   that will tend to yield the wider confidence interval.
#'
#'
#'   \strong{One-Sample Case (\code{sample.type="one.sample"})}.
#'   \describe{
#'
#'   \item{\code{ci.method="score"}}{ The confidence interval for \eqn{p} based on the
#'   score method was developed by Wilson (1927) and is discussed by Newcombe (1998a),
#'   Agresti and Coull (1998), and Agresti and Caffo (2000).  When \code{ci=TRUE} and
#'   \code{ci.method="score"}, the function \code{\link{ebinom}} calls the \R function
#'   \code{\link{prop.test}} to compute the confidence interval.  This method
#'   has been shown to provide the best performance (in terms of actual coverage matching
#'   assumed coverage) of all the methods provided here, although unlike the exact
#'   method, the actual coverage can fall below the assumed coverage.
#'   }
#'
#'   \item{\code{ci.method="exact"}}{ The confidence interval for \eqn{p} based on the
#'   exact (Clopper-Pearson) method is discussed by Newcombe (1998a), Agresti and Coull (1998),
#'   and Zar (2010, pp.543-547).  This is the method used in the \R function
#'   \code{\link{binom.test}}.  This method ensures the actual coverage is greater than
#'   or equal to the assumed coverage.
#'   }
#'
#'   \item{\code{ci.method="Wald"}}{ The confidence interval for \eqn{p} based on the
#'   Wald method (with or without a correction for continuity) is the usual
#'   \dQuote{normal approximation} method and is discussed by Newcombe (1998a),
#'   Agresti and Coull (1998), Agresti and Caffo (2000), and Zar (2010, pp.543-547).
#'   This method is \strong{never} recommended but is included for historical purposes.
#'   }
#'
#'   \item{\code{ci.method="adjusted Wald"}}{ The confidence interval for \eqn{p} based on the
#'   adjusted Wald method is discussed by Agresti and Coull (1998), Agresti and Caffo (2000), and
#'   Zar (2010, pp.543-547).  This is a simple modification of the Wald method and
#'   performs surpringly well.
#'   }
#'   }
#'
#'  \strong{Two-Sample Case (\code{sample.type="two.sample"})}.
#'
#'   \describe{
#'
#'   \item{\code{ci.method="score"}}{ This method is presented in Newcombe (1998b) and
#'   is based on the score method developed by Wilson (1927) for the one-sample case.
#'   This is the method used by the R function \code{\link{prop.test}}.  In a comparison of
#'   11 methods, Newcombe (1998b) showed this method performs remarkably well.
#'   }
#'
#'   \item{\code{ci.method="Wald"}}{ The confidence interval for the difference between two
#'   proportions based on the Wald method (with or without a correction for continuity) is
#'   the usual \dQuote{normal approximation} method and is discussed by Newcombe (1998b),
#'   Agresti and Caffo (2000), and Zar (2010, pp.549-552).  This method is \strong{not}
#'   recommended but is included for historical purposes.
#'   }
#'
#'   \item{\code{ci.method="adjusted Wald"}}{ This method is discussed by Agresti and Caffo (2000),
#'   and Zar (2010, pp.549-552).  This is a simple modification of the Wald method and
#'   performs surpringly well.
#'   }
#'   }
#' }
#' @rawRd
#' \value{
#'   a list with information about the half-widths, sample sizes, and
#'   estimated proportions.
#'
#'   \strong{One-Sample Case (\code{sample.type="one.sample"})}. \cr
#'   When \code{sample.type="one.sample"}, the function \code{ciBinomHalfWidth}
#'   returns a list with these components:
#'
#'   \item{half.width}{the half-width(s) of the confidence interval(s)}
#'   \item{n}{the sample size(s) associated with the confidence interval(s)}
#'   \item{p.hat}{the estimated proportion(s)}
#'   \item{method}{the method used to construct the confidence interval(s)}
#'
#'
#'   \strong{Two-Sample Case (\code{sample.type="two.sample"})}.  \cr
#'   When \code{sample.type="two.sample"}, the function \code{ciBinomHalfWidth}
#'   returns a list with these components:
#'
#'   \item{half.width}{the half-width(s) of the confidence interval(s)}
#'   \item{n1}{the sample size(s) for group 1 associated with the confidence interval(s)}
#'   \item{p1.hat}{the estimated proportion(s) for group 1}
#'   \item{n2}{the sample size(s) for group 2 associated with the confidence interval(s)}
#'   \item{p2.hat}{the estimated proportion(s) for group 2}
#'   \item{method}{the method used to construct the confidence interval(s)}
#' }
#' @rawRd
#' \references{
#'   Agresti, A., and B.A. Coull. (1998). Approximate is Better than "Exact" for Interval Estimation
#'   of Binomial Proportions. \emph{The American Statistician}, \bold{52}(2), 119--126.
#'
#'   Agresti, A., and B. Caffo. (2000). Simple and Effective Confidence Intervals for Proportions
#'   and Differences of Proportions Result from Adding Two Successes and Two Failures. \emph{The
#'   American Statistician}, \bold{54}(4), 280--288.
#'
#'   Berthouex, P.M., and L.C. Brown. (1994). \emph{Statistics for Environmental Engineers}.
#'   Lewis Publishers, Boca Raton, FL, Chapters 2 and 15.
#'
#'   Cochran, W.G. (1977). \emph{Sampling Techniques}. John Wiley and Sons, New York, Chapter 3.
#'
#'   Fisher, R.A., and F. Yates. (1963).
#'   \emph{Statistical Tables for Biological, Agricultural, and Medical Research}. 6th edition.
#'   Hafner, New York, 146pp.
#'
#'   Fleiss, J. L. (1981). \emph{Statistical Methods for Rates and Proportions}. Second Edition.
#'   John Wiley and Sons, New York, Chapters 1-2.
#'
#'   Gilbert, R.O. (1987). \emph{Statistical Methods for Environmental Pollution Monitoring}.
#'   Van Nostrand Reinhold, New York, NY, Chapter 11.
#'
#'   Millard, S.P., and Neerchal, N.K. (2001). \emph{Environmental Statistics with S-PLUS}.
#'   CRC Press, Boca Raton, Florida.
#'
#'   Newcombe, R.G. (1998a). Two-Sided Confidence Intervals for the Single Proportion:  Comparison of
#'   Seven Methods. \emph{Statistics in Medicine}, \bold{17}, 857--872.
#'
#'   Newcombe, R.G. (1998b). Interval Estimation for the Difference Between Independent Proportions:
#'   Comparison of Eleven Methods. \emph{Statistics in Medicine}, \bold{17}, 873--890.
#'
#'   Ott, W.R. (1995). \emph{Environmental Statistics and Data Analysis}.
#'   Lewis Publishers, Boca Raton, FL, Chapter 4.
#'
#'   USEPA. (1989b). \emph{Statistical Analysis of Ground-Water Monitoring Data at RCRA Facilities, Interim Final Guidance}.
#'   EPA/530-SW-89-026. Office of Solid Waste, U.S. Environmental Protection Agency, Washington, D.C.
#'
#'   USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
#'   EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.
#'   U.S. Environmental Protection Agency, Washington, D.C. p.6-38.
#'
#'   Zar, J.H. (2010). \emph{Biostatistical Analysis}. Fifth Edition.
#'   Prentice-Hall, Upper Saddle River, NJ, Chapter 24.
#' }
#' @rawRd
#' \author{
#'   Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \note{
#'   The binomial distribution is used to model processes with binary
#'   (Yes-No, Success-Failure, Heads-Tails, etc.) outcomes.  It is assumed that the outcome of any
#'   one trial is independent of any other trial, and that the probability of \dQuote{success}, \eqn{p},
#'   is the same on each trial.  A binomial discrete random variable \eqn{X} is the number of
#'   \dQuote{successes} in \eqn{n} independent trials.  A special case of the binomial distribution
#'   occurs when \eqn{n=1}, in which case \eqn{X} is also called a Bernoulli random variable.
#'
#'   In the context of environmental statistics, the binomial distribution is sometimes used to model
#'   the proportion of times a chemical concentration exceeds a set standard in a given period of time
#'   (e.g., Gilbert, 1987, p.143), or to compare the proportion of detects in a compliance well vs. a
#'   background well (e.g., USEPA, 1989b, Chapter 8, p.3-7).  (However, USEPA 2009, p.8-27
#'   recommends using the Wilcoxon rank sum test (\code{\link{wilcox.test}}) instead of
#'   comparing proportions.)
#'
#'   In the course of designing a sampling program, an environmental scientist may wish to determine
#'   the relationship between sample size, confidence level, and half-width if one of the objectives of
#'   the sampling program is to produce confidence intervals.  The functions \code{ciBinomHalfWidth},
#'   \code{\link{ciBinomN}}, and \code{\link{plotCiBinomDesign}} can be used to investigate these
#'   relationships for the case of binomial proportions.
#' }
#' @rawRd
#' \seealso{
#'   \code{\link{ciBinomN}}, \code{\link{plotCiBinomDesign}},
#'   \code{\link{ebinom}}, \code{\link{binom.test}}, \code{\link{prop.test}}.
#' }
#' @rawRd
#' \examples{
#'   # Look at how the half-width of a one-sample confidence interval
#'   # decreases with sample size:
#'
#'   ciBinomHalfWidth(n.or.n1 = c(10, 50, 100, 500))
#'   #$half.width
#'   #[1] 0.26340691 0.13355486 0.09616847 0.04365873
#'   #
#'   #$n
#'   #[1]  10  50 100 500
#'   #
#'   #$p.hat
#'   #[1] 0.5 0.5 0.5 0.5
#'   #
#'   #$method
#'   #[1] "Score normal approximation, with continuity correction"
#'
#'   #----------------------------------------------------------------
#'
#'   # Look at how the half-width of a one-sample confidence interval
#'   # tends to decrease as the estimated value of p decreases below
#'   # 0.5 or increases above 0.5:
#'
#'   seq(0.2, 0.8, by = 0.1)
#'   #[1] 0.2 0.3 0.4 0.5 0.6 0.7 0.8
#'
#'   ciBinomHalfWidth(n.or.n1 = 30, p.hat = seq(0.2, 0.8, by = 0.1))
#'   #$half.width
#'   #[1] 0.1536299 0.1707256 0.1801322 0.1684587 0.1801322 0.1707256
#'   #[7] 0.1536299
#'   #
#'   #$n
#'   #[1] 30 30 30 30 30 30 30
#'   #
#'   #$p.hat
#'   #[1] 0.2 0.3 0.4 0.5 0.6 0.7 0.8
#'   #
#'   #$method
#'   #[1] "Score normal approximation, with continuity correction"
#'
#'   #----------------------------------------------------------------
#'
#'   # Look at how the half-width of a one-sample confidence interval
#'   # increases with increasing confidence level:
#'
#'   ciBinomHalfWidth(n.or.n1 = 20, conf.level = c(0.8, 0.9, 0.95, 0.99))
#'   #$half.width
#'   #[1] 0.1377380 0.1725962 0.2007020 0.2495523
#'   #
#'   #$n
#'   #[1] 20 20 20 20
#'   #
#'   #$p.hat
#'   #[1] 0.5 0.5 0.5 0.5
#'   #
#'   #$method
#'   #[1] "Score normal approximation, with continuity correction"
#'
#'   #----------------------------------------------------------------
#'
#'   # Compare the half-widths for a one-sample
#'   # confidence interval based on the different methods:
#'
#'   ciBinomHalfWidth(n.or.n1 = 30, ci.method = "score")$half.width
#'   #[1] 0.1684587
#'
#'   ciBinomHalfWidth(n.or.n1 = 30, ci.method = "exact")$half.width
#'   #[1] 0.1870297
#'
#'   ciBinomHalfWidth(n.or.n1 = 30, ci.method = "adjusted Wald")$half.width
#'   #[1] 0.1684587
#'
#'   ciBinomHalfWidth(n.or.n1 = 30, ci.method = "Wald")$half.width
#'   #[1] 0.1955861
#'
#'   #----------------------------------------------------------------
#'
#'   # Look at how the half-width of a two-sample
#'   # confidence interval decreases with increasing
#'   # sample sizes:
#'
#'   ciBinomHalfWidth(n.or.n1 = c(10, 50, 100, 500), sample.type = "two")
#'   #$half.width
#'   #[1] 0.53385652 0.21402654 0.14719748 0.06335658
#'   #
#'   #$n1
#'   #[1]  10  50 100 500
#'   #
#'   #$p1.hat
#'   #[1] 0.5 0.5 0.5 0.5
#'   #
#'   #$n2
#'   #[1]  10  50 100 500
#'   #
#'   #$p2.hat
#'   #[1] 0.4 0.4 0.4 0.4
#'   #
#'   #$method
#'   #[1] "Score normal approximation, with continuity correction"
#' }
#' @rawRd
#' \keyword{ design }
#' @rawRd
#' \keyword{ distribution }
#' @rawRd
#' \keyword{ htest }

ciBinomHalfWidth <-
function (n.or.n1, p.hat.or.p1.hat = 0.5, n2 = n.or.n1, p2.hat = 0.4, 
    conf.level = 0.95, sample.type = "one.sample", ci.method = "score", 
    correct = TRUE, warn = TRUE) 
{
    if (missing(sample.type)) {
        sample.type <- ifelse(!missing(n2) || !missing(p2.hat), 
            "two.sample", "one.sample")
    }
    sample.type <- match.arg(sample.type, c("one.sample", "two.sample"))
    ci.method <- match.arg(ci.method, c("score", "exact", "adjusted Wald", 
        "Wald"))
    if (!is.vector(n.or.n1, mode = "numeric") || !is.vector(p.hat.or.p1.hat, 
        mode = "numeric") || !is.vector(conf.level, mode = "numeric")) 
        stop("'n.or.n1', 'p.hat', and 'conf.level' must be numeric vectors.")
    if (!all(is.finite(n.or.n1)) || !all(is.finite(p.hat.or.p1.hat)) || 
        !all(is.finite(conf.level))) 
        stop(paste("Missing (NA), Infinite (Inf, -Inf), and", 
            "Undefined (Nan) values are not allowed in", "'n.or.n1', 'p.hat.or.p1.hat', or 'conf.level'"))
    if (any(n.or.n1 < 2)) 
        stop("All values of 'n.or.n1' must be greater than or equal to 2")
    if (any(p.hat.or.p1.hat < .Machine$double.eps) || any(p.hat.or.p1.hat > 
        1 - .Machine$double.eps)) 
        stop("All values of 'p.hat.or.p1.hat' must be strictly between 0 and 1.")
    if (any(conf.level <= .Machine$double.eps) || any(conf.level >= 
        1 - .Machine$double.eps)) 
        stop("All values of 'conf.level' must be greater than 0 and less than 1")
    if (sample.type == "two.sample") {
        if (ci.method == "exact") 
            stop("Exact method not available for two-sample confidence interval")
        if (!missing(n2)) {
            if (!is.vector(n2, mode = "numeric")) 
                stop("'n2' must be a numeric vector")
            if (!all(is.finite(n2))) 
                stop(paste("Missing (NA), Infinite (Inf, -Inf), and", 
                  "Undefined (Nan) values are not allowed in 'n2'"))
            if (any(n2 < 2)) 
                stop("All values of 'n2' must be greater than or equal to 2")
        }
        if (!missing(p2.hat)) {
            if (!is.vector(p2.hat, mode = "numeric")) 
                stop("'p2.hat' must be a numeric vector")
            if (!all(is.finite(p2.hat))) 
                stop(paste("Missing (NA), Infinite (Inf, -Inf), and", 
                  "Undefined (Nan) values are not allowed in 'p2.hat'"))
            if (any(p2.hat < .Machine$double.eps) || any(p2.hat > 
                1 - .Machine$double.eps)) 
                stop("All values of 'p2.hat' must be strictly between 0 and 1")
        }
        arg.mat <- cbind.no.warn(n1 = as.vector(n.or.n1), n2 = as.vector(n2), 
            p1.hat = as.vector(p.hat.or.p1.hat), p2.hat = as.vector(p2.hat), 
            conf.level = as.vector(conf.level))
        for (i in c("n1", "n2", "p1.hat", "p2.hat", "conf.level")) assign(i, 
            arg.mat[, i])
        p1.hat.high <- ceiling(n1 * p1.hat)/n1
        p1.hat.low <- floor(n1 * p1.hat)/n1
        index <- p1.hat.high != p1.hat.low
        index.tied <- abs((p1.hat.high - p1.hat) - (p1.hat - 
            p1.hat.low)) <= .Machine$double.eps
        index.high <- (index & !index.tied & ((p1.hat.high - 
            p1.hat) < (p1.hat - p1.hat.low))) | (index & index.tied & 
            (abs(p1.hat.high - 0.5) <= abs(p1.hat.low - 0.5)))
        index.low <- (index & !index.tied & ((p1.hat.high - p1.hat) > 
            (p1.hat - p1.hat.low))) | (index & index.tied & (abs(p1.hat.high - 
            0.5) > abs(p1.hat.low - 0.5)))
        p1.hat[index.high] <- p1.hat.high[index.high]
        p1.hat[index.low] <- p1.hat.low[index.low]
        x1 <- round(n1 * p1.hat)
        p2.hat.high <- ceiling(n2 * p2.hat)/n2
        p2.hat.low <- floor(n2 * p2.hat)/n2
        index <- p2.hat.high != p2.hat.low
        index.tied <- abs((p2.hat.high - p2.hat) - (p2.hat - 
            p2.hat.low)) <= .Machine$double.eps
        index.high <- (index & !index.tied & ((p2.hat.high - 
            p2.hat) < (p2.hat - p2.hat.low))) | (index & index.tied & 
            (abs(p2.hat.high - 0.5) <= abs(p2.hat.low - 0.5)))
        index.low <- (index & !index.tied & ((p2.hat.high - p2.hat) > 
            (p2.hat - p2.hat.low))) | (index & index.tied & (abs(p2.hat.high - 
            0.5) > abs(p2.hat.low - 0.5)))
        p2.hat[index.high] <- p2.hat.high[index.high]
        p2.hat[index.low] <- p2.hat.low[index.low]
        x2 <- round(n2 * p2.hat)
        switch(ci.method, score = {
            N <- length(n1)
            half.width <- numeric(N)
            o.warn <- options(warn = -1)
            for (i in 1:N) {
                x1.i <- x1[i]
                n1.i <- n1[i]
                x2.i <- x2[i]
                n2.i <- n2[i]
                conf.i <- conf.level[i]
                half.width[i] <- diff(prop.test(x = c(x1.i, x2.i), 
                  n = c(n1.i, n2.i), conf.level = conf.i, correct = correct)$conf.int)/2
            }
            options(o.warn)
        }, `adjusted Wald` = {
            const <- qnorm(1 - (1 - conf.level)/2)^2
            x1.a <- x1 + const/4
            x2.a <- x2 + const/4
            n1.a <- n1 + const/2
            n2.a <- n2 + const/2
            p1.hat.a <- x1.a/n1.a
            p2.hat.a <- x2.a/n2.a
            q1.hat.a <- 1 - p1.hat.a
            q2.hat.a <- 1 - p2.hat.a
            s.hat <- sqrt((p1.hat.a * q1.hat.a)/n1.a + (p2.hat.a * 
                q2.hat.a)/n2.a)
            half.width <- qnorm(1 - (1 - conf.level)/2) * s.hat
        }, Wald = {
            q1.hat <- 1 - p1.hat
            q2.hat <- 1 - p2.hat
            s.hat <- sqrt((p1.hat * q1.hat)/n1 + (p2.hat * q2.hat)/n2)
            cf <- correct * 0.5 * (1/n1 + 1/n2)
            half.width <- qnorm(1 - (1 - conf.level)/2) * s.hat + 
                cf
            if (warn && any(index <- pmin(n1 * p1.hat, n1 * q1.hat, 
                n2 * p2.hat, n2 * q2.hat) < 5)) warning(paste("The sample sizes 'n1' and 'n2' are too small,", 
                "relative to the given values of 'p1.hat' and 'p2.hat',", 
                "for the normal approximation to work well for the following", 
                "element indices:\n\t", paste((1:length(half.width))[index], 
                  collapse = " "), "\n\t"))
        })
        names(half.width) <- names(n1) <- names(n2) <- names(p1.hat) <- names(p2.hat) <- NULL
        ret.val <- list(half.width = half.width, n1 = n1, p1.hat = p1.hat, 
            n2 = n2, p2.hat = p2.hat)
    }
    else {
        arg.mat <- cbind.no.warn(n = as.vector(n.or.n1), p.hat = as.vector(p.hat.or.p1.hat), 
            conf.level = as.vector(conf.level))
        for (i in c("n", "p.hat", "conf.level")) assign(i, arg.mat[, 
            i])
        p.hat.high <- ceiling(n * p.hat)/n
        p.hat.low <- floor(n * p.hat)/n
        index <- p.hat.high != p.hat.low
        index.tied <- abs((p.hat.high - p.hat) - (p.hat - p.hat.low)) <= 
            .Machine$double.eps
        index.high <- (index & !index.tied & ((p.hat.high - p.hat) < 
            (p.hat - p.hat.low))) | (index & index.tied & (abs(p.hat.high - 
            0.5) <= abs(p.hat.low - 0.5)))
        index.low <- (index & !index.tied & ((p.hat.high - p.hat) > 
            (p.hat - p.hat.low))) | (index & index.tied & (abs(p.hat.high - 
            0.5) > abs(p.hat.low - 0.5)))
        p.hat[index.high] <- p.hat.high[index.high]
        p.hat[index.low] <- p.hat.low[index.low]
        x <- round(n * p.hat)
        N <- length(n)
        half.width <- numeric(N)
        switch(ci.method, score = {
            for (i in 1:N) {
                x.i <- x[i]
                n.i <- n[i]
                conf.i <- conf.level[i]
                half.width[i] <- diff(ebinom(x = x.i, size = n.i, 
                  ci = TRUE, ci.type = "two-sided", ci.method = "score", 
                  conf.level = conf.i, correct = correct)$interval$limits)/2
            }
        }, exact = {
            for (i in 1:N) {
                x.i <- x[i]
                n.i <- n[i]
                conf.i <- conf.level[i]
                half.width[i] <- diff(ebinom(x = x.i, size = n.i, 
                  ci = TRUE, ci.type = "two-sided", ci.method = "exact", 
                  conf.level = conf.i)$interval$limits)/2
            }
        }, `adjusted Wald` = {
            for (i in 1:N) {
                x.i <- x[i]
                n.i <- n[i]
                conf.i <- conf.level[i]
                half.width[i] <- diff(ebinom(x = x.i, size = n.i, 
                  ci = TRUE, ci.type = "two-sided", ci.method = "adjusted Wald", 
                  conf.level = conf.i)$interval$limits)/2
            }
        }, Wald = {
            cf <- (correct * 0.5)/n
            half.width <- qnorm(1 - (1 - conf.level)/2) * sqrt((p.hat * 
                (1 - p.hat))/n) + cf
            if (warn && any(index <- pmin(n * p.hat, n * (1 - 
                p.hat)) < 5)) warning(paste("The sample size 'n' is too small,", 
                "relative to the given value of 'p.hat', for the normal", 
                "approximation to work well for the following", 
                "element indices:\n\t", paste((1:length(half.width))[index], 
                  collapse = " "), "\n\t"))
        })
        names(half.width) <- names(n) <- names(p.hat) <- NULL
        ret.val <- list(half.width = half.width, n = n, p.hat = p.hat)
    }
    method <- switch(ci.method, score = {
        ifelse(!correct, "Score normal approximation", "Score normal approximation, with continuity correction")
    }, exact = "Exact", Wald = {
        ifelse(!correct, "Wald normal approximation", "Wald normal approximation, with continuity correction")
    }, `adjusted Wald` = "Adjusted Wald normal approximation")
    c(ret.val, method = method)
}
