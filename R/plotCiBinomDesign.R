#' Plots for Sampling Design Based on Confidence Interval for Binomial Proportion or Difference Between Two Proportions
#' @description
#' Create plots for a sampling design based on a confidence interval for
#'   a binomial proportion or the difference between two proportions.
#' @usage
#' plotCiBinomDesign(x.var = "n", y.var = "half.width",
#'     range.x.var = NULL, n.or.n1 = 25, p.hat.or.p1.hat = 0.5,
#'     n2 = n.or.n1, p2.hat = 0.4, ratio = 1, half.width = 0.05,
#'     conf.level = 0.95, sample.type = "one.sample", ci.method = "score",
#'     correct = TRUE, warn = TRUE, n.or.n1.min = 2,
#'     n.or.n1.max = 10000, tol.half.width = 0.005, tol.p.hat = 0.005,
#'     maxiter = 10000, plot.it = TRUE, add = FALSE, n.points = 100,
#'     plot.col = 1, plot.lwd = 3 * par("cex"), plot.lty = 1,
#'     digits = .Options$digits,
#'     main = NULL, xlab = NULL, ylab = NULL, type = "l", ...)
#' @rawRd
#' \arguments{
#'   \item{x.var}{
#'   character string indicating what variable to use for the x-axis.  Possible values are
#'   \code{"n"} (sample size; the default),
#'   \code{"half.width"} (the half-width of the confidence interval),
#'   \code{"p.hat"} (the estimated probability of \dQuote{success}), and
#'   \code{"conf.level"} (the confidence level).
#' }
#'   \item{y.var}{
#'   character string indicating what variable to use for the y-axis.  Possible values are
#'   \code{"half.width"} (the half-width of the confidence interval; the default), and
#'   \code{"n"} (sample size).
#' }
#'   \item{range.x.var}{
#'   numeric vector of length 2 indicating the range of the x-variable to use for the plot.
#'   The default value depends on the value of \code{x.var}.  \cr
#'   When \code{x.var="n"} the default value is \code{c(10,50)}.  \cr
#'   When \code{x.var="half.width"}, the default value is \code{c(0.03, 0.1)}.  \cr
#'   When \code{x.var="p.hat"}, the default value is \code{c(0.5, 0.9)}.  \cr
#'   When \code{x.var="conf.level"}, the default value is \code{c(0.8, 0.99)}.
#' }
#'   \item{n.or.n1}{
#'   numeric scalar indicating the sample size.  The default value is \code{n.or.n1=25}.
#'   When \code{sample.type="one.sample"}, \code{n.or.n1} denotes the number of observations
#'   in the single sample.  When \code{sample.type="two.sample"}, \code{n.or.n1} denotes the
#'   number of observations from group 1.
#'   Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) values are not allowed.
#'   This argument is ignored if either \code{x.var="n"} or \code{y.var="n"}.
#' }
#'   \item{p.hat.or.p1.hat}{
#'   numeric scalar indicating an estimated proportion.  \cr
#'   When \code{sample.type="one.sample"}, \code{p.hat.or.p1.hat} denotes the estimated value of
#'   \eqn{p}, the probability of \dQuote{success}.  \cr
#'   When \code{sample.type="two.sample"}, \code{p.hat.or.p1.hat} denotes the estimated value of
#'   \eqn{p_1}, the probability of \dQuote{success} in group 1.  \cr
#'   Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) values are not allowed.
#'   This argument is ignored if \code{x.var="p.hat"}.
#' }
#'   \item{n2}{
#'   numeric scalar indicating the sample size for group 2.  The default value is the value of \code{n.or.n1}.
#'   Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) values are not allowed.
#'   This argument is ignored when \cr
#'   \code{sample.type="one.sample"}.
#' }
#'   \item{p2.hat}{
#'   numeric scalar indicating the estimated proportion for group 2.
#'   Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) values are not allowed.
#'   This argument is ignored if \code{sample.type="one.sample"}.
#' }
#'   \item{ratio}{
#'   numeric vector indicating the ratio of sample size in group 2 to sample size in group 1 (\eqn{n_2/n_1}).
#'   The default value is \code{ratio=1}.  All values of \code{ratio} must be greater than or equal to 1.
#'   This argument is only used when \cr
#'   \code{sample.type="two.sample"} and either \code{x.var="n"} or \code{y.var="n"}.
#' }
#'   \item{half.width}{
#'   positive numeric scalar indicating the half-width of the confidence interval.
#'   The default value is \code{half.width=0.05}.
#'   Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) values are not allowed.
#'   This argument is ignored if either \code{x.var="half.width"} or \code{y.var="half.width"}.
#' }
#'   \item{conf.level}{
#'   a numeric scalar between 0 and 1 indicating the confidence level associated with the confidence intervals.
#'   The default value is \code{conf.level=0.95}.  This argument is ignored when
#'   \code{x.var="conf.level"}.
#' }
#'   \item{sample.type}{
#'   character string indicating whether this is a one-sample or two-sample confidence interval.
#'   When \code{sample.type="one.sample"}, the computations for the plot are based on a confidence
#'   interval for a single proportion.  When \cr
#'   \code{sample.type="two.sample"}, the computations for
#'   the plot are based on a confidence interval for the difference between two proportions.
#'   The default value is \code{sample.type="one.sample"} unless the arguments \code{n2},
#'   \code{p2.hat}, and/or \code{ratio} are supplied.
#' }
#'   \item{ci.method}{
#'   character string indicating which method to use to construct the confidence interval.
#'   Possible values are \code{"score"} (the default), \code{"exact"}, \code{"adjusted Wald"},
#'   and \code{"Wald"} (the \code{"Wald"} method is \strong{never} recommended but is included
#'   for historical purposes).  The exact method is only available for the one-sample case, i.e.,
#'   when \code{sample.type="one.sample"}.
#' }
#'   \item{correct}{
#'   logical scalar indicating whether to use the continuity correction when \cr
#'   \code{ci.method="score"} or \code{ci.method="Wald"}.  The default value is \cr
#'   \code{correct=TRUE}.  This argument is ignored if \code{ci.method="exact"} or \cr
#'   \code{ci.method="adjusted Wald"}.
#' }
#'   \item{warn}{
#'   logical scalar indicating whether to issue a warning when \code{ci.method="Wald"}
#'   for cases when the normal approximation to the binomial distribution probably is not accurate.
#'   The default value is \code{warn=TRUE}.
#' }
#'   \item{n.or.n1.min}{
#'   for the case when \code{y.var="n"},
#'   integer indicating the minimum allowed value for
#'   \eqn{n} (\code{sample.type="one.sample"}) or \cr
#'   \eqn{n_1} (\code{sample.type="two.sample"}).  \cr
#'   The default value is \code{n.or.n1.min=2}.
#' }
#'   \item{n.or.n1.max}{
#'   for the case when \code{y.var="n"},
#'   integer indicating the maximum allowed value for
#'   \eqn{n} (\code{sample.type="one.sample"}) or \cr
#'   \eqn{n_1} (\code{sample.type="two.sample"}).  \cr
#'   The default value is \code{n.or.n1.max=10000}.
#' }
#'   \item{tol.half.width}{
#'   for the case when \code{y.var="n"},
#'   numeric scalar indicating the tolerance to use for the half width for
#'   the search algorithm.  The sample sizes are computed so that the actual
#'   half width is less than or equal to \cr
#'   \code{half.width + tol.half.width}.  The default value is \cr
#'   \code{tol.half.width=0.005}.
#' }
#'   \item{tol.p.hat}{
#'   for the case when \code{y.var="n"},
#'   numeric scalar indicating the tolerance to use for the estimated
#'   proportion(s) for the search algorithm.
#'   For the one-sample case, the sample sizes are computed so that
#'   the absolute value of the difference between the user supplied
#'   value of \code{p.hat.or.p1.hat} and the actual estimated proportion is
#'   less than or equal to \code{tol.p.hat}.
#'   For the two-sample case, the sample sizes are computed so that
#'   the absolute value of the difference between the user supplied
#'   value of \code{p.hat.or.p1.hat} and the actual estimated proportion
#'   for group 1 is less than or equal to \code{tol.p.hat}, and the
#'   absolute value of the difference between the user supplied
#'   value of \code{p2.hat} and the actual estimated proportion
#'   for group 2 is less than or equal to \code{tol.p.hat}.
#'   The default value is \code{tol.p.hat=0.005}.
#' }
#'   \item{maxiter}{
#'   for the case when \code{y.var="n"},
#'   integer indicating the maximum number of iterations to use for
#'   the search algorithm.  The default value is \code{maxiter=1000}.
#' }
#'   \item{plot.it}{
#'   a logical scalar indicating whether to create a plot or add to the existing plot
#'   (see description of the argument \code{add} below) on the current graphics device.
#'   If \code{plot.it=FALSE}, no plot is produced, but a list of (x,y) values is returned
#'   (see the VALUE section below).  The default value is \code{plot.it=TRUE}.
#' }
#'   \item{add}{
#'   a logical scalar indicating whether to add the design plot to the existing plot
#'   (\code{add=TRUE}), or to create a plot from scratch (\code{add=FALSE}).
#'   The default value is \code{add=FALSE}.  This argument is ignored if \code{plot.it=FALSE}.
#' }
#'   \item{n.points}{
#'   a numeric scalar specifying how many (x,y) pairs to use to produce the plot.
#'   There are \code{n.points} x-values evenly spaced between \code{range.x.var[1]} and \cr
#'   \code{range.x.var[2]}.  The default value is \code{n.points=100}.  This argument is
#'   ignored when \code{x.var="n"}, in which case the x-values are all the integers between
#'   \code{range.x.var[1]} and \code{range.x.var[2]}.
#' }
#'   \item{plot.col}{
#'   a numeric scalar or character string determining the color of the plotted line or points.  The default value
#'   is \code{plot.col=1}.  See the entry for \code{col} in the help file for \code{\link{par}}
#'   for more information.
#' }
#'   \item{plot.lwd}{
#'   a numeric scalar determining the width of the plotted line.  The default value is
#'   \code{3*par("cex")}.  See the entry for \code{lwd} in the help file for \code{\link{par}}
#'   for more information.
#' }
#'   \item{plot.lty}{
#'   a numeric scalar determining the line type of the plotted line.  The default value is
#'   \code{plot.lty=1}.  See the entry for \code{lty} in the help file for \code{\link{par}}
#'   for more information.
#' }
#'   \item{digits}{
#'   a scalar indicating how many significant digits to print out on the plot.  The default
#'   value is the current setting of \code{\link{options}("digits")}.
#' }
#'   \item{main, xlab, ylab, type,\dots}{
#'   additional graphical parameters (see \code{\link{par}}).
#' }
#' }
#' @rawRd
#' \details{
#'   See the help files for \code{\link{ciBinomHalfWidth}} and \code{\link{ciBinomN}}
#'   for information on how to compute a one-sample confidence interval for
#'   a single binomial proportion or a two-sample confidence interval for the difference between
#'   two proportions, how the half-width is computed when other quantities are fixed, and how
#'   the sample size is computed when other quantities are fixed.
#' }
#' @rawRd
#' \value{
#'   \code{plotCiBinomDesign} invisibly returns a list with components:
#'
#'   \item{x.var}{x-coordinates of the points that have been or would have been plotted}
#'   \item{y.var}{y-coordinates of the points that have been or would have been plotted}
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
#'   U.S. Environmental Protection Agency, Washington, D.C.
#'
#'   Zar, J.H. (2010). \emph{Biostatistical Analysis}. Fifth Edition.
#'   Prentice-Hall, Upper Saddle River, NJ, Chapter 24.
#' }
#' @rawRd
#' \author{
#'     Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
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
#'   \code{\link{ciBinomHalfWidth}}, \code{\link{ciBinomN}},
#'   \code{\link{ebinom}}, \code{\link{binom.test}}, \code{\link{prop.test}},
#'   \code{\link{par}}.
#' }
#' @rawRd
#' \examples{
#'   # Look at the relationship between half-width and sample size
#'   # for a one-sample confidence interval for a binomial proportion,
#'   # assuming an estimated proportion of 0.5 and a confidence level of
#'   # 95%.  The jigsaw appearance of the plot is the result of using the
#'   # score method:
#'
#'   dev.new()
#'   plotCiBinomDesign()
#'
#'   #----------
#'
#'   # Redo the example above, but use the traditional (and inaccurate)
#'   # Wald method.
#'
#'   dev.new()
#'   plotCiBinomDesign(ci.method = "Wald")
#'
#'   #--------------------------------------------------------------------
#'
#'   # Plot sample size vs. the estimated proportion for various half-widths,
#'   # using a 95% confidence level and the adjusted Wald method:
#'
#'   # NOTE:  This example takes several seconds to run so it has been
#'   #        commented out.  Simply remove the pound signs (#) from in front
#'   #        of the R commands to run it.
#'
#'   #dev.new()
#'   #plotCiBinomDesign(x.var = "p.hat", y.var = "n",
#'   #    half.width = 0.04, ylim = c(0, 600), main = "",
#'   #    xlab = expression(hat(p)))
#'   #
#'   #plotCiBinomDesign(x.var = "p.hat", y.var = "n",
#'   #    half.width = 0.05, add = TRUE, plot.col = 2)
#'   #
#'   #plotCiBinomDesign(x.var = "p.hat", y.var = "n",
#'   #    half.width = 0.06, add = TRUE, plot.col = 3)
#'   #
#'   #legend(0.5, 150, paste("Half-Width =", c(0.04, 0.05, 0.06)),
#'   #    lty = rep(1, 3), lwd = rep(2, 3), col=1:3, bty = "n")
#'   #
#'   #mtext(expression(paste("Sample Size vs. ", hat(p),
#'   #  " for Confidence Interval for p")), line = 2.5, cex = 1.25)
#'   #mtext("with Confidence=95\%  and Various Values of Half-Width",
#'   #  line = 1.5, cex = 1.25)
#'   #mtext(paste("CI Method = Score Normal Approximation",
#'   #  "with Continuity Correction"), line = 0.5)
#'
#'   #--------------------------------------------------------------------
#'
#'   # Modifying the example on pages 8-5 to 8-7 of USEPA (1989b),
#'   # look at the relationship between half-width and sample size
#'   # for a 95\% confidence interval for the difference between the
#'   # proportion of detects at the background and compliance wells.
#'   # Use the estimated proportion of detects from the original data.
#'   # (The data are stored in EPA.89b.cadmium.df.)
#'   # Assume equal sample sizes at each well.
#'
#'   EPA.89b.cadmium.df
#'   #   Cadmium.orig Cadmium Censored  Well.type
#'   #1           0.1   0.100    FALSE Background
#'   #2          0.12   0.120    FALSE Background
#'   #3           BDL   0.000     TRUE Background
#'   # ..........................................
#'   #86          BDL   0.000     TRUE Compliance
#'   #87          BDL   0.000     TRUE Compliance
#'   #88          BDL   0.000     TRUE Compliance
#'
#'   p.hat.back <- with(EPA.89b.cadmium.df,
#'     mean(!Censored[Well.type=="Background"]))
#'
#'   p.hat.back
#'   #[1] 0.3333333
#'
#'   p.hat.comp <- with(EPA.89b.cadmium.df,
#'     mean(!Censored[Well.type=="Compliance"]))
#'
#'   p.hat.comp
#'   #[1] 0.375
#'
#'   dev.new()
#'   plotCiBinomDesign(p.hat.or.p1.hat = p.hat.back,
#'       p2.hat = p.hat.comp, digits=3)
#'
#'   #==========
#'
#'   # Clean up
#'   #---------
#'   rm(p.hat.back, p.hat.comp)
#'   graphics.off()
#' }
#' @rawRd
#' \keyword{ design }
#' @rawRd
#' \keyword{ distribution }
#' @rawRd
#' \keyword{ htest }
#' @rawRd
#' \keyword{ hplot }

plotCiBinomDesign <-
function (x.var = "n", y.var = "half.width", range.x.var = NULL, 
    n.or.n1 = 25, p.hat.or.p1.hat = 0.5, n2 = n.or.n1, p2.hat = 0.4, 
    ratio = 1, half.width = 0.05, conf.level = 0.95, sample.type = "one.sample", 
    ci.method = "score", correct = TRUE, warn = TRUE, n.or.n1.min = 2, 
    n.or.n1.max = 10000, tol.half.width = 0.005, tol.p.hat = 0.005, 
    maxiter = 10000, plot.it = TRUE, add = FALSE, n.points = 100, 
    plot.col = 1, plot.lwd = 3 * par("cex"), plot.lty = 1, digits = .Options$digits, 
    main = NULL, xlab = NULL, ylab = NULL, type = "l", ...) 
{
    x.var <- match.arg(x.var, c("n", "half.width", "p.hat", "conf.level"))
    y.var <- match.arg(y.var, c("half.width", "n"))
    if (x.var == y.var) 
        stop("'x.var' and 'y.var' cannot denote the same quantity")
    if (missing(range.x.var)) {
        range.x.var <- switch(x.var, n = c(10, 50), half.width = c(0.03, 
            0.1), p.hat = c(0.5, 0.9), conf.level = c(0.8, 0.99))
    }
    if (is.null(range.x.var) || !all(is.finite(range.x.var)) || 
        !is.vector(range.x.var, mode = "numeric") || length(range.x.var) != 
        2) 
        stop(paste("'range.x.var' must be a numeric vector of length 2", 
            "with no missing (NA), infinite(-Inf, Inf), or undefined(NaN) values"))
    min.x <- range.x.var[1]
    max.x <- range.x.var[2]
    if (min.x >= max.x) 
        stop("The second element of 'range.x.var' must be larger than the first")
    if (missing(sample.type)) {
        sample.type <- ifelse(!missing(n2) || !missing(p2.hat) || 
            !missing(ratio), "two.sample", "one.sample")
    }
    sample.type <- match.arg(sample.type, c("one.sample", "two.sample"))
    ci.method <- match.arg(ci.method, c("score", "exact", "adjusted Wald", 
        "Wald"))
    switch(x.var, n = {
        if (!all(range.x.var == trunc(range.x.var)) || min.x < 
            2) stop(paste("When x.var='n', 'range.x.var' must be", 
            "a vector containing two integers, with the first", 
            "element greater than 1, and the second", "element larger than the first."))
    }, half.width = {
        if (sample.type == "one.sample" && (range.x.var[1] < 
            .Machine$double.eps || range.x.var[2] >= 0.5 || range.x.var[1] >= 
            range.x.var[2])) stop(paste("When x.var='half.width' and sample.type='one.sample',", 
            "'range.x.var' must be", "a vector of two elements, with the first", 
            "element greater than 0, the second element less than 0.5,", 
            "and the second element larger than the first."))
        if (sample.type == "two.sample" && (range.x.var[1] < 
            .Machine$double.eps || range.x.var[2] > 1 - .Machine$double.eps || 
            range.x.var[1] >= range.x.var[2])) stop(paste("When x.var='half.width' and sample.type='one.sample',", 
            "'range.x.var' must be", "a vector of two elements, with the first", 
            "element greater than 0, the second element less than 1,", 
            "and the second element larger than the first."))
    }, p.hat = {
        if (range.x.var[1] < .Machine$double.eps || range.x.var[2] > 
            1 - .Machine$double.eps || range.x.var[1] >= range.x.var[2]) stop(paste("When x.var='p.hat', 'range.x.var' must be", 
            "a vector containing two elements, with the first element", 
            "greater than 0, the second element less than 1,", 
            "and with the second element larger than the first"))
    }, conf.level = {
        if (range.x.var[1] < .Machine$double.eps || range.x.var[2] > 
            1 - .Machine$double.eps || range.x.var[1] >= range.x.var[2]) stop(paste("When x.var='conf.level', 'range.x.var' must be", 
            "a vector of two elements, with the first element", 
            "greater than 0, the second element less than 1, and the second", 
            "element larger than the first."))
    })
    if (x.var != "n" && y.var != "n") {
        if (!is.vector(n.or.n1, mode = "numeric") || length(n.or.n1) != 
            1 || n.or.n1 != trunc(n.or.n1) || n.or.n1 < 2) 
            stop("'n.or.n1' must be an integer greater than 1")
    }
    if (x.var != "half.width" && y.var != "half.width") {
        if (!is.vector(half.width, mode = "numeric") || length(half.width) != 
            1 || half.width < .Machine$double.eps) 
            stop("'half.width' must be a positive numeric scalar")
    }
    if (x.var != "p.hat") {
        if (!is.vector(p.hat.or.p1.hat, mode = "numeric") || 
            length(p.hat.or.p1.hat) != 1 || p.hat.or.p1.hat < 
            .Machine$double.eps || p.hat.or.p1.hat > 1 - .Machine$double.eps) 
            stop("'p.hat.or.p1.hat' must be a positive numeric scalar between 0 and 1")
    }
    if (x.var != "conf.level") {
        if (!is.vector(conf.level, mode = "numeric") || length(conf.level) != 
            1 || conf.level < .Machine$double.eps || conf.level > 
            1 - .Machine$double.eps) 
            stop("'conf.level' must be a numeric scalar between 0 and 1")
    }
    if (sample.type == "two.sample") {
        if (ci.method == "exact") 
            stop("Exact method not available for two-sample confidence interval")
        if (x.var == "n" || y.var == "n") {
            if (!is.vector(ratio, mode = "numeric") || length(ratio) != 
                1 || any(ratio < 1)) 
                stop("'ratio' must be a numeric scalar greater than or equal to 1")
        }
        else if (!is.vector(n2, mode = "numeric") || length(n2) != 
            1 || n2 != trunc(n2) || n2 < 2) {
            stop("'n2' must be an integer greater than 1")
        }
        if (!is.vector(p2.hat, mode = "numeric") || length(p2.hat) != 
            1 || p2.hat < .Machine$double.eps || p2.hat > 1 - 
            .Machine$double.eps) 
            stop("'p2.hat' must be a positive numeric scalar between 0 and 1")
    }
    if (y.var == "n") {
        if (length(n.or.n1.min) != 1 || length(n.or.n1.max) != 
            1 || n.or.n1.min < 2 || n.or.n1.max <= n.or.n1.min || 
            n.or.n1.min != round(n.or.n1.min) || n.or.n1.max != 
            round(n.or.n1.max)) 
            stop(paste("'n.or.n1.min' must be an integer greater than 1", 
                "and 'n.or.n1.max' must be an integer greater than 'n.or.n1.min'"))
        if (length(tol.half.width) != 1 || tol.half.width <= 
            .Machine$double.eps || tol.half.width >= 0.5) 
            stop("'tol.half.width' must be a scalar greater than 0 and less than 0.5")
        if (length(tol.p.hat) != 1 || tol.p.hat <= .Machine$double.eps || 
            tol.p.hat >= 0.5) 
            stop("'tol.p.hat' must be a scalar greater than 0 and less than 0.5")
    }
    if (!is.vector(n.points, mode = "numeric") || length(n.points) != 
        1 || n.points != trunc(n.points) || n.points < 2) 
        stop("'n.points' must be an integer larger than 1")
    type.string <- ifelse(sample.type == "one.sample", "p,", 
        "(p1 - p2),")
    if (plot.it) 
        gen.gp.list <- checkGraphicsPars(...)$gen.gp.list
    combo <- paste(sort(c(x.var, y.var)), collapse = " & ")
    p.string <- ifelse(sample.type == "two.sample", paste(", Estimated p1 = ", 
        format(p.hat.or.p1.hat, digits = digits), ", and Estimated p2 = ", 
        format(p2.hat, digits = digits), sep = ""), paste(", and Estimated p =", 
        format(p.hat.or.p1.hat, digits = digits)))
    ratio.gt.1 <- ratio > 1
    n2.constrained <- !missing(n2) && !is.null(n2)
    is.null.main <- is.null(main)
    switch(combo, `conf.level & half.width` = {
        x <- seq(range.x.var[1], range.x.var[2], length = n.points)
        y <- ciBinomHalfWidth(n.or.n1 = n.or.n1, n2 = n2, p.hat.or.p1.hat = p.hat.or.p1.hat, 
            p2.hat = p2.hat, conf.level = x, sample.type = sample.type, 
            ci.method = ci.method, correct = correct, warn = warn)$half.width
        if (is.null(ylab)) ylab <- "Half-Width"
        if (is.null(xlab)) xlab <- "Confidence Level"
        n.string <- ifelse(sample.type == "two.sample", paste("n1 = ", 
            n.or.n1, ", n2 = ", n2, sep = ""), paste("n =", n.or.n1))
        if (is.null(main)) main <- paste("Half-Width vs. Confidence Level for ", 
            "Confidence Interval for ", type.string, "\nwith ", 
            n.string, p.string, sep = "")
    }, `conf.level & n` = {
        x <- seq(range.x.var[1], range.x.var[2], length = n.points)
        if (sample.type == "two.sample") {
            if (is.null(ylab)) {
                ylab <- ifelse(ratio.gt.1, paste("Sample Size (n1), With n2 =", 
                  ratio, "* n1"), "Sample Size (n1 and n2)")
            }
            y <- ciBinomN(half.width = half.width, p.hat.or.p1.hat = p.hat.or.p1.hat, 
                p2.hat = p2.hat, conf.level = x, sample.type = "two.sample", 
                ratio = ratio, ci.method = ci.method, correct = correct, 
                warn = warn, n.or.n1.min = n.or.n1.min, n.or.n1.max = n.or.n1.max, 
                tol.half.width = tol.half.width, tol.p.hat = tol.p.hat, 
                maxiter = maxiter)$n1
        } else {
            if (is.null(ylab)) ylab <- "Sample Size (n)"
            y <- ciBinomN(half.width = half.width, p.hat.or.p1.hat = p.hat.or.p1.hat, 
                conf.level = x, sample.type = "one.sample", ci.method = ci.method, 
                correct = correct, warn = warn, n.or.n1.min = n.or.n1.min, 
                n.or.n1.max = n.or.n1.max, tol.half.width = tol.half.width, 
                tol.p.hat = tol.p.hat, maxiter = maxiter)$n
        }
        if (is.null(xlab)) xlab <- "Confidence Level"
        if (is.null(main)) main <- paste("Sample Size vs. Confidence Level for ", 
            "Confidence Interval for ", type.string, "\nwith Half-Width = ", 
            format(half.width, digits = digits), p.string, sep = "")
    }, `n & p.hat` = {
        x <- seq(range.x.var[1], range.x.var[2], length = n.points)
        if (sample.type == "two.sample") {
            if (is.null(ylab)) {
                ylab <- ifelse(ratio.gt.1, paste("Sample Size (n1), With n2 =", 
                  ratio, "* n1"), "Sample Size (n1 and n2)")
            }
            y <- ciBinomN(half.width = half.width, p.hat.or.p1.hat = x, 
                p2.hat = p2.hat, conf.level = conf.level, sample.type = "two.sample", 
                ratio = ratio, ci.method = ci.method, correct = correct, 
                warn = warn, n.or.n1.min = n.or.n1.min, n.or.n1.max = n.or.n1.max, 
                tol.half.width = tol.half.width, tol.p.hat = tol.p.hat, 
                maxiter = maxiter)$n1
            if (is.null(xlab)) xlab <- "Estimated p1"
            if (is.null(main)) main <- paste("Sample Size vs. Estimated p1 for ", 
                "Confidence Interval for ", type.string, "\nwith Half-Width = ", 
                format(half.width, digits = digits), " Confidence Level = ", 
                format(conf.level, digits = digits), ", and Estimated p2 = ", 
                p2.hat, sep = "")
        } else {
            if (is.null(ylab)) ylab <- "Sample Size (n)"
            y <- ciBinomN(half.width = half.width, p.hat.or.p1.hat = x, 
                conf.level = conf.level, sample.type = "one.sample", 
                ci.method = ci.method, correct = correct, warn = warn, 
                n.or.n1.min = n.or.n1.min, n.or.n1.max = n.or.n1.max, 
                tol.half.width = tol.half.width, tol.p.hat = tol.p.hat, 
                maxiter = maxiter)$n
            if (is.null(xlab)) xlab <- "Estiamted p"
            if (is.null(main)) main <- paste("Sample Size vs. Estimated p for ", 
                "Confidence Interval for ", type.string, "\nwith Half-Width = ", 
                format(half.width, digits = digits), " and Confidence Level = ", 
                format(conf.level, digits = digits), sep = "")
        }
    }, `half.width & p.hat` = {
        x <- seq(range.x.var[1], range.x.var[2], length = n.points)
        if (sample.type == "two.sample") {
            y <- ciBinomHalfWidth(n.or.n1 = n.or.n1, n2 = n2, 
                p.hat.or.p1.hat = x, p2.hat = p2.hat, conf.level = conf.level, 
                sample.type = "two.sample", ci.method = ci.method, 
                correct = correct, warn = warn)$half.width
            if (is.null(xlab)) xlab <- "Estimated p1"
            if (is.null(main)) main <- paste("Half-Width vs. Estimated p1 for ", 
                "Confidence Interval for ", type.string, "\nwith ", 
                "n1 = ", n.or.n1, ", n2 = ", n2, ",", " Estimated p2 = ", 
                p2.hat, ", and Confidence Level = ", format(conf.level, 
                  digits = digits), sep = "")
        } else {
            y <- ciBinomHalfWidth(n.or.n1 = n.or.n1, p.hat.or.p1.hat = x, 
                conf.level = conf.level, sample.type = "one.sample", 
                ci.method = ci.method, correct = correct, warn = warn)$half.width
            if (is.null(xlab)) xlab <- "Estimated p"
            if (is.null(main)) main <- paste("Half-Width vs. Estimated p for ", 
                "Confidence Interval for ", type.string, "\nwith n = ", 
                n.or.n1, " and Confidence Level = ", format(conf.level, 
                  digits = digits), sep = "")
        }
        if (is.null(ylab)) ylab <- "Half-Width"
    }, `half.width & n` = {
        if (x.var == "n") {
            x <- range.x.var[1]:range.x.var[2]
            if (sample.type == "two.sample") {
                if (n2.constrained) {
                  if (is.null(xlab)) xlab <- paste("Sample Size (n1), With n2 =", 
                    n2)
                } else {
                  n2 <- x
                  if (is.null(xlab)) xlab <- "Sample Size (n1 and n2)"
                }
            } else {
                if (is.null(xlab)) xlab <- "Sample Size (n)"
            }
            y <- ciBinomHalfWidth(n.or.n1 = x, n2 = n2, p.hat.or.p1.hat = p.hat.or.p1.hat, 
                p2.hat = p2.hat, conf.level = conf.level, sample.type = sample.type, 
                ci.method = ci.method, correct = correct, warn = warn)$half.width
            if (is.null(ylab)) ylab <- "Half-Width"
            if (is.null(main)) main <- paste("Half-Width vs. Sample Size for ", 
                "Confidence Interval for ", type.string, "\nwith Confidence Level = ", 
                format(conf.level, digits = digits), p.string, 
                sep = "")
        } else {
            x <- seq(range.x.var[1], range.x.var[2], length = n.points)
            if (sample.type == "two.sample") {
                if (is.null(ylab)) {
                  ylab <- ifelse(ratio.gt.1, paste("Sample Size (n1), With n2 =", 
                    ratio, "* n1"), "Sample Size (n1 and n2)")
                }
                y <- ciBinomN(half.width = x, p.hat.or.p1.hat = p.hat.or.p1.hat, 
                  p2.hat = p2.hat, conf.level = conf.level, sample.type = "two.sample", 
                  ratio = ratio, ci.method = ci.method, correct = correct, 
                  warn = warn, n.or.n1.min = n.or.n1.min, n.or.n1.max = n.or.n1.max, 
                  tol.half.width = tol.half.width, tol.p.hat = tol.p.hat, 
                  maxiter = maxiter)$n1
            } else {
                if (is.null(ylab)) ylab <- "Sample Size (n)"
                y <- ciBinomN(half.width = x, p.hat.or.p1.hat = p.hat.or.p1.hat, 
                  conf.level = conf.level, sample.type = "one.sample", 
                  ci.method = ci.method, correct = correct, warn = warn, 
                  n.or.n1.min = n.or.n1.min, n.or.n1.max = n.or.n1.max, 
                  tol.half.width = tol.half.width, tol.p.hat = tol.p.hat, 
                  maxiter = maxiter)$n
            }
            if (is.null(xlab)) xlab <- "Half-Width"
            if (is.null(main)) main <- paste("Sample Size vs. Half-Width for ", 
                "Confidence Interval for ", type.string, "\nwith Confidence Level = ", 
                format(conf.level, digits = digits), p.string, 
                sep = "")
        }
    })
    if (plot.it) {
        if (!add) {
            plot(x, y, type = "n", main = "", sub = "", ..., 
                xlab = xlab, ylab = ylab)
            arg.list <- c(gen.gp.list, list(main = main))
            do.call("title", arg.list)
            arg.list <- c(list(x = x, y = y), gen.gp.list, list(type = type, 
                col = plot.col, lwd = plot.lwd, lty = plot.lty))
            do.call("lines", arg.list)
            if (is.null.main) {
                method <- switch(ci.method, score = {
                  ifelse(!correct, "Score normal approximation", 
                    "Score normal approximation, with continuity correction")
                }, exact = "Exact", Wald = {
                  ifelse(!correct, "Wald normal approximation", 
                    "Wald normal approximation, with continuity correction")
                }, `adjusted Wald` = "Adjusted Wald normal approximation")
                mtext(paste("CI Method =", method), line = 0, 
                  cex = par("cex"))
            }
        }
        else {
            arg.list <- c(list(x = x, y = y), gen.gp.list, list(type = type, 
                col = plot.col, lwd = plot.lwd, lty = plot.lty))
            do.call("lines", arg.list)
        }
    }
    ret.list <- list(x, y)
    names(ret.list) <- c(x.var, y.var)
    invisible(ret.list)
}
