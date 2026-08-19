#' Rosner's Test for Outliers
#' @aliases Rosner
#' @rawRd \alias{outlier test}
#' @rawRd \alias{Outlier Test}
#' @description
#' Perform Rosner's generalized extreme Studentized deviate test for up to
#'   \eqn{k} potential outliers in a dataset, assuming the data without any outliers come
#'   from a normal (Gaussian) distribution.
#' @usage
#' rosnerTest(x, k = 3, alpha = 0.05, warn = TRUE)
#' @rawRd
#' \arguments{
#'   \item{x}{
#'   numeric vector of observations.
#'   Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf})
#'   values are allowed but will be removed.  There must be at least 10 non-missing, finite
#'   observations in \code{x}.
#' }
#'   \item{k}{
#'   positive integer indicating the number of suspected outliers.  The argument \code{k}
#'   must be between 1 and \eqn{n-2} where \eqn{n} denotes the number of non-missing, finite
#'   values in the arguemnt \code{x}.  The default value is \code{k=3}.
#' }
#'   \item{alpha}{
#'   numeric scalar between 0 and 1 indicating the Type I error associated with the
#'   test of hypothesis.  The default value is \code{alpha=0.05}.
#' }
#'   \item{warn}{
#'   logical scalar indicating whether to issue a warning (\code{warn=TRUE}; the default)
#'   when the number of non-missing, finite values in \code{x} and the value of \code{k} are such
#'   that the assumed Type I error level might not be maintained.  See the DETAILS section below.
#' }
#' }
#' @details
#' This help page has been shortened to keep function help focused on usage,
#' arguments, return values, and examples. Extended method details, formulas,
#' and background material are available in \code{vignette("extended-function-details", package = "EnvStats")}, section \code{rosnerTest}.
#' @rawRd
#' \value{
#'   A list of class \code{"gofOutlier"} containing the results of the hypothesis test.
#'   See the help file for \code{\link{gofOutlier.object}} for details.
#' }
#' @rawRd
#' \references{
#'   Barnett, V., and T. Lewis. (1995).  \emph{Outliers in Statistical Data}.  Third Edition.
#'   John Wiley & Sons, Chichester, UK, pp. 235--236.
#'
#'   Gilbert, R.O. (1987). \emph{Statistical Methods for Environmental Pollution Monitoring}.
#'   Van Nostrand Reinhold, NY, pp.188--191.
#'
#'   McBean, E.A, and F.A. Rovers. (1992).  Estimation of the Probability of Exceedance of
#'   Contaminant Concentrations.  \emph{Ground Water Monitoring Review} \bold{Winter},
#'   pp. 115--119.
#'
#'   McNutt, M. (2014).  Raising the Bar.  \emph{Science} \bold{345}(6192), p. 9.
#'
#'   Rosner, B. (1975).  On the Detection of Many Outliers.
#'   \emph{Technometrics} \bold{17}, 221--227.
#'
#'   Rosner, B. (1983).  Percentage Points for a Generalized ESD Many-Outlier Procedure.
#'   \emph{Technometrics} \bold{25}, 165--172.
#'
#'   USEPA. (2006).  \emph{Data Quality Assessment:  A Reviewer's Guide}.  EPA QA/G-9R.
#'   EPA/240/B-06/002, February 2006.  Office of Environmental Information, U.S.
#'   Environmental Protection Agency, Washington, D.C.
#'
#'   USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA
#'   Facilities, Unified Guidance}.  EPA 530/R-09-007, March 2009.  Office of Resource
#'   Conservation and Recovery Program Implementation and Information Division.  U.S.
#'   Environmental Protection Agency, Washington, D.C., pp. 12-10 to 12-14.
#'
#'   USEPA. (2013a).  \emph{ProUCL Version 5.0.00 Technical Guide}.  EPA/600/R-07/041,
#'   September 2013.  Office of Research and Development.  U.S. Environmental Protection
#'   Agency, Washington, D.C., pp. 190--195.
#'
#'   USEPA. (2013b).  \emph{ProUCL Version 5.0.00 User Guide}.  EPA/600/R-07/041,
#'   September 2013.  Office of Research and Development.  U.S. Environmental Protection
#'   Agency, Washington, D.C., pp. 190--195.
#' }
#' @rawRd
#' \author{
#'   Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \note{
#'   Rosner's test is a commonly used test for \dQuote{outliers} when you are willing to
#'   assume that the data without outliers follows a normal (Gaussian) distribution.  It is
#'   designed to avoid \emph{masking}, which occurs when an outlier goes undetected because
#'   it is close in value to another outlier.
#'
#'   Rosner's test is a kind of discordancy test (Barnett and Lewis, 1995).  The test
#'   statistic of a discordancy test is usually a ratio:  the numerator is the difference
#'   between the suspected outlier and some summary statistic of the data set
#'   (e.g., mean, next largest observation, etc.), while the denominator is always a measure
#'   of spread within the data (e.g., standard deviation, range, etc.).  Both USEPA (2009)
#'   and USEPA (2013a,b) discuss two commonly used discordancy tests:  Dixon's test and
#'   Rosner's test.  Both of these tests assume that all of the data that are not outliers
#'   come from a normal (Gaussian) distribution.
#'
#'   There are many forms of Dixon's test (Barnett and Lewis, 1995).  The one presented in
#'   USEPA (2009) and USEPA (20013a,b) assumes just one outlier (Dixon, 1953).  This test
#'   is vulnerable to "masking" in which the presence of several outliers masks the fact
#'   that even one outlier is present.  There are also other forms of Dixon's test that
#'   allow for more than one outlier based on a sequence of sub-tests, but these tests are
#'   also vulnerable to masking.
#'
#'   Rosner's test allows you to test for several possible outliers and avoids the problem of
#'   masking.  Rosner's test requires you to set the number of suspected outliers, \eqn{k},
#'   in advance.  As in the case of Dixon's test, there are several forms of Rosner's test,
#'   so you need to be aware of which one you are using.  The form of Rosner's test presented in
#'   USEPA (2009) is based on the extreme Studentized deviate (ESD) (Rosner, 1975), whereas the
#'   form of Rosner's test performed by the \pkg{EnvStats} function \code{rosnerTest} and
#'   presented in USEPA (2013a,b) is based on the \bold{generalized} ESD (Rosner, 1983; Gilbert, 1987).
#'   USEPA (2013a, p. 190) cites both Rosner (1975) and Rosner (1983), but presents only the
#'   test given in Rosner (1983).  Rosner's test based on the ESD has the appropriate Type I
#'   error level if there are no outliers in the dataset, but if there are actually say \eqn{m}
#'   outliers, where \eqn{m < k}, then the ESD version of Rosner's test tends to declare
#'   more than \eqn{m} outliers with a probability that is greater than the stated Type I
#'   error level (referred to as \dQuote{swamping}).  Rosner's test based on the
#'   generalized ESD fixes this problem.  USEPA (2013a, pp. 17, 191) incorrectly states that
#'   the generalized ESD version of Rosner's test is vulnerable to masking.  Surprisingly,
#'   the well-known book on statistical outliers by Barnett and Lewis (1995) does not
#'   discuss Rosner's generalized ESD test.
#'
#'   As noted, using Rosner's test requires specifying the number of suspected outliers,
#'   \eqn{k}, in advance.  USEPA (2013a, pp.190-191) states:
#'   \dQuote{A graphical display (Q-Q plot) can be used to identify suspected outliers
#'   needed to perform the Rosner test}, and USEPA (2009, p. 12-11) notes:
#'   \dQuote{A potential drawback of Rosner's test is that the user must first identify
#'   the maximum number of potential outliers (k) prior to running the test.  Therefore,
#'   this requirement makes the test ill-advised as an automatic outlier screening tool,
#'   and somewhat reliant on the user to identify candidate outliers.}
#'
#'   When observations contain non-detect values (NDs), USEPA (2013a, p. 191) states:
#'   \dQuote{one may replace the NDs by their respective detection limits (DLs), DL/2, or may
#'   just ignore them ....}  This is bad advice, as this method of dealing with non-detects
#'   will produce Type I error rates that are not correct. \cr
#'
#'   \bold{OUTLIERS ARE NOT NECESSARILY INCORRECT VALUES}\cr
#'   Whether an observation is an \dQuote{outlier} depends on the underlying assumed
#'   statistical model.  McBean and Rovers (1992) state: \cr
#'   \dQuote{It may be possible to ignore the outlier if a physical rationale is available but,
#'   failing that, the value must be included ....  Note that the use of statistics does not
#'   interpret the facts, it simply makes the facts easier to see.  Therefore, it is incumbent
#'   on the analyst to identify whether or not the high value ... is truly representative of
#'   the chemical being monitored or, instead, is an outlier for reasons such as a result of
#'   sampling or laboratory error.}
#'
#'   USEPA (2006, p.51) states:  \cr
#'   \dQuote{If scientific reasoning does not explain the outlier, it should not be
#'   discarded from the data set.}
#'
#'   Finally, an editorial by the Editor-in-Chief of the journal \emph{Science} deals with
#'   this topic (McNutt, 2014).
#'
#'   You can use the functions \code{\link{qqPlot}} and \code{\link{gofTest}} to explore
#'   other possible statistical models for the data, or you can use nonparametric statistics
#'   if you do not want to assume a particular distribution.
#' }
#' @rawRd
#' \seealso{
#'   \code{\link{gofTest}}, \code{\link{gofOutlier.object}}, \code{\link{print.gofOutlier}},
#'   \link{Normal}, \code{\link{qqPlot}}.
#' }
#' @rawRd
#' \examples{
#'   # Combine 30 observations from a normal distribution with mean 3 and
#'   # standard deviation 2, with 3 observations from a normal distribution
#'   # with mean 10 and standard deviation 1, then run Rosner's Test on these
#'   # data, specifying k=4 potential outliers based on looking at the
#'   # normal Q-Q plot.
#'   # (Note: the call to set.seed simply allows you to reproduce
#'   # this example.)
#'
#'   set.seed(250)
#'
#'   dat <- c(rnorm(30, mean = 3, sd = 2), rnorm(3, mean = 10, sd = 1))
#'
#'   dev.new()
#'   qqPlot(dat)
#'
#'   rosnerTest(dat, k = 4)
#'
#'   #Results of Outlier Test
#'   #-------------------------
#'   #
#'   #Test Method:                     Rosner's Test for Outliers
#'   #
#'   #Hypothesized Distribution:       Normal
#'   #
#'   #Data:                            dat
#'   #
#'   #Sample Size:                     33
#'   #
#'   #Test Statistics:                 R.1 = 2.848514
#'   #                                 R.2 = 3.086875
#'   #                                 R.3 = 3.033044
#'   #                                 R.4 = 2.380235
#'   #
#'   #Test Statistic Parameter:        k = 4
#'   #
#'   #Alternative Hypothesis:          Up to 4 observations are not
#'   #                                 from the same Distribution.
#'   #
#'   #Type I Error:                    5%
#'   #
#'   #Number of Outliers Detected:     3
#'   #
#'   #  i   Mean.i     SD.i      Value Obs.Num    R.i+1 lambda.i+1 Outlier
#'   #1 0 3.549744 2.531011 10.7593656      33 2.848514   2.951949    TRUE
#'   #2 1 3.324444 2.209872 10.1460427      31 3.086875   2.938048    TRUE
#'   #3 2 3.104392 1.856109  8.7340527      32 3.033044   2.923571    TRUE
#'   #4 3 2.916737 1.560335 -0.7972275      25 2.380235   2.908473   FALSE
#'
#'   #----------
#'   # Clean up
#'
#'   rm(dat)
#'   graphics.off()
#'
#'   #--------------------------------------------------------------------
#'
#'   # Example 12-4 of USEPA (2009, page 12-12) gives an example of
#'   # using Rosner's test to test for outliers in napthalene measurements (ppb)
#'   # taken at 5 background wells over 5 quarters.  The data for this example
#'   # are stored in EPA.09.Ex.12.4.naphthalene.df.
#'
#'   EPA.09.Ex.12.4.naphthalene.df
#'   #   Quarter Well Naphthalene.ppb
#'   #1        1 BW.1            3.34
#'   #2        2 BW.1            5.39
#'   #3        3 BW.1            5.74
#'   # ...
#'   #23       3 BW.5            5.53
#'   #24       4 BW.5            4.42
#'   #25       5 BW.5           35.45
#'
#'   longToWide(EPA.09.Ex.12.4.naphthalene.df, "Naphthalene.ppb", "Quarter", "Well",
#'     paste.row.name = TRUE)
#'   #          BW.1 BW.2  BW.3 BW.4  BW.5
#'   #Quarter.1 3.34 5.59  1.91 6.12  8.64
#'   #Quarter.2 5.39 5.96  1.74 6.05  5.34
#'   #Quarter.3 5.74 1.47 23.23 5.18  5.53
#'   #Quarter.4 6.88 2.57  1.82 4.43  4.42
#'   #Quarter.5 5.85 5.39  2.02 1.00 35.45
#'
#'
#'   # Look at Q-Q plots for both the raw and log-transformed data
#'   #------------------------------------------------------------
#'
#'   dev.new()
#'   with(EPA.09.Ex.12.4.naphthalene.df,
#'     qqPlot(Naphthalene.ppb, add.line = TRUE,
#'       main = "Figure 12-6.  Naphthalene Probability Plot"))
#'
#'   dev.new()
#'   with(EPA.09.Ex.12.4.naphthalene.df,
#'     qqPlot(Naphthalene.ppb, dist = "lnorm", add.line = TRUE,
#'       main = "Figure 12-7.  Log Naphthalene Probability Plot"))
#'
#'
#'   # Test for 2 potential outliers on the original scale:
#'   #-----------------------------------------------------
#'
#'   with(EPA.09.Ex.12.4.naphthalene.df, rosnerTest(Naphthalene.ppb, k = 2))
#'
#'   #Results of Outlier Test
#'   #-------------------------
#'   #
#'   #Test Method:                     Rosner's Test for Outliers
#'   #
#'   #Hypothesized Distribution:       Normal
#'   #
#'   #Data:                            Naphthalene.ppb
#'   #
#'   #Sample Size:                     25
#'   #
#'   #Test Statistics:                 R.1 = 3.930957
#'   #                                 R.2 = 4.160223
#'   #
#'   #Test Statistic Parameter:        k = 2
#'   #
#'   #Alternative Hypothesis:          Up to 2 observations are not
#'   #                                 from the same Distribution.
#'   #
#'   #Type I Error:                    5%
#'   #
#'   #Number of Outliers Detected:     2
#'   #
#'   #  i  Mean.i     SD.i Value Obs.Num    R.i+1 lambda.i+1 Outlier
#'   #1 0 6.44240 7.379271 35.45      25 3.930957   2.821681    TRUE
#'   #2 1 5.23375 4.325790 23.23      13 4.160223   2.801551    TRUE
#'
#'   #----------
#'   # Clean up
#'
#'   graphics.off()
#' }
#' @rawRd
#' \keyword{ htest }
#' @rawRd
#' \keyword{ models }

rosnerTest <-
function (x, k = 3, alpha = 0.05, warn = TRUE) 
{
    data.name <- deparse(substitute(x))
    if (!is.numeric(x)) 
        stop("'x' must be a numeric vector")
    obs.num <- 1:length(x)
    if ((bad.obs <- sum(!(x.ok <- is.finite(x)))) > 0) {
        x <- x[x.ok]
        obs.num <- obs.num[x.ok]
        warning(paste(bad.obs, "observations with NA/NaN/Inf in 'x' removed."))
    }
    n <- length(x)
    if (n < 3) 
        stop("There must be at least 3 non-missing finite observations in 'x'")
    if (length(k) != 1 || !is.numeric(k) || !is.finite(k) || 
        k != round(k) || k < 1 || k > (n - 2)) 
        stop(paste("'k' must be a positive integer less than or equal to n-2,", 
            "where 'n' denotes the number of finite, non-missing observations in 'x'"))
    if (length(alpha) != 1 || !is.numeric(alpha) || !is.finite(alpha) || 
        any(alpha <= 0) || any(alpha >= 1)) 
        stop("'alpha' must be a numeric scalar greater than 0 and less than 1")
    if (warn) {
        if (k > 10 | k > floor(n/2)) {
            warning(paste("The true Type I error may be larger than assumed.", 
                "Although the help file for 'rosnerTest' has a table with information", 
                "on the estimated Type I error level,", "simulations were not run for k > 10 or k > floor(n/2).", 
                sep = "\n"))
        }
        else {
            warn.conds <- (alpha > 0.01 & ((n >= 15 & n < 25 & 
                k > 2) | (n < 15 & k > 1))) | (alpha <= 0.01 & 
                (n < 15 & k > 1))
            if (warn.conds) 
                warning(paste("The true Type I error may be larger than assumed.", 
                  "See the help file for 'rosnerTest' for a table with information", 
                  "on the estimated Type I error level."))
        }
    }
    R <- rep(as.numeric(NA), k)
    mean.vec <- rep(as.numeric(NA), k)
    sd.vec <- rep(as.numeric(NA), k)
    x.vec <- rep(as.numeric(NA), k)
    obs.num.vec <- rep(as.numeric(NA), k)
    new.x <- x
    new.obs.num <- obs.num
    for (i in 1:k) {
        mean.vec[i] <- mean(new.x)
        sd.vec[i] <- sd(new.x)
        if (sd.vec[i] > 0) {
            abs.z = abs(new.x - mean.vec[i])/sd.vec[i]
            R[i] <- max(abs.z)
            index <- which(abs.z == R[i])[1]
            x.vec[i] <- new.x[index]
            obs.num.vec[i] <- new.obs.num[index]
            new.x <- new.x[-index]
            new.obs.num <- new.obs.num[-index]
        }
        else {
            R[i:k] <- NA
            break
        }
    }
    num.outlier.vec <- 1:k
    lambda <- rosnerTestLambda(n = n, k = 1:k, alpha = alpha)
    outlier <- R > lambda
    if (any(outlier)) {
        index <- max(num.outlier.vec[outlier], na.rm = TRUE)
        outlier[1:index] <- TRUE
    }
    out.df <- data.frame(num.outlier.vec - 1, mean.vec, sd.vec, 
        x.vec, obs.num.vec, R, lambda, outlier)
    names(out.df) <- c("i", "Mean.i", "SD.i", "Value", "Obs.Num", 
        "R.i+1", "lambda.i+1", "Outlier")
    distribution <- "Normal"
    dist.abb <- "norm"
    stat <- R
    names(stat) <- paste("R", 1:k, sep = ".")
    crit.value <- lambda
    names(crit.value) <- paste("lambda", 1:k, sep = ".")
    n.outliers <- sum(outlier, na.rm = TRUE)
    ret.list <- list(distribution = distribution, statistic = stat, 
        sample.size = n, parameters = c(k = k), alpha = alpha, 
        crit.value = crit.value, n.outliers = n.outliers, alternative = paste("Up to ", 
            k, " observations are not\n", space(33), "from the same Distribution.", 
            sep = ""), method = "Rosner's Test for Outliers", 
        data = x, data.name = data.name, bad.obs = bad.obs, all.stats = out.df)
    oldClass(ret.list) <- "gofOutlier"
    ret.list
}
