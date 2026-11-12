#' Randomization (Permutation) Test to Compare Two Proportions (Fisher's Exact Test)
#' @description
#' Perform a two-sample randomization (permutation) test to compare two proportions.
#'   This is also called Fisher's exact test.
#'
#'   \bold{Note:} You can perform Fisher's exact test in \R using the function
#'   \code{\link{fisher.test}}.
#' @usage
#' twoSamplePermutationTestProportion(x, y, x.and.y = "Binomial Outcomes",
#'     alternative = "two.sided", tol = sqrt(.Machine$double.eps))
#' @rawRd
#' \arguments{
#'   \item{x, y}{
#'   When \code{x.and.y="Binomial Outcomes"} (the default), \code{x} and \code{y} are
#'   vectors of observations from groups 1 and 2, respectively.  The vectors \code{x}
#'   and \code{y} must contain no more than 2 unique values (e.g., \code{0} and \code{1},
#'   \code{FALSE} and \code{TRUE}, \code{"No"} and \code{"Yes"}, etc.).  In this case,
#'   the result of \code{sort(unique(x))[2]} is taken to be the value that indicates a
#'   \dQuote{success} for \code{x} and the result of \code{sort(unique(y))[2]} is taken
#'   to be the value that indicates a \dQuote{success} for \code{y}.  For example, \cr
#'   \code{x = c(FALSE, TRUE, FALSE, TRUE, TRUE)} indicates 3 successes in 5 trials, and
#'   \code{y = c(1, 0, 0, 0)} indicates 1 success in 4 trials.  When \cr
#'   \code{x.and.y="Binomial Outcomes"}, missing (\code{NA}), undefined (\code{NaN}), and
#'   infinite (\code{Inf}, \code{-Inf}) values are allowed but will be removed.
#'
#'   When \code{x.and.y="Number of Successes and Trials"}, \code{x} must be a vector of
#'   length 2 containing the number of successes for groups 1 and 2, respectively, and
#'   \code{y} must be a vector of length 2 that contains the number of trials for groups
#'   1 and 2, respectively.  For example, \code{x = c(3, 1)} and \code{y = c(5, 4)}
#'   indicates 3 successes in 5 trials for group 1 and 1 success in 4 trials for
#'   group 2.
#' }
#'   \item{x.and.y}{
#'   character string indicating the kind of data stored in the vectors \code{x} and
#'   \code{y}.  The possible values are \code{x.and.y="Binomial Outcomes"} (the default),
#'   and \cr
#'   \code{x.and.y="Number Successes and Trials"}.
#' }
#'   \item{alternative}{
#'   character string indicating the kind of alternative hypothesis.  The possible values
#'   are \code{"two.sided"} (the default), \code{"less"}, and \code{"greater"}.
#' }
#'   \item{tol}{
#'   numeric scalar indicating the tolerance to use for computing the p-value for the
#'   two-sample permutation test.  The default value is \cr
#'   \code{tol=sqrt(.Machine$double.eps)}.  See the DETAILS section below for more
#'   information.
#' }
#' }
#' @rawRd
#' \details{
#'   \emph{Randomization Tests} \cr
#'   In 1935, R.A. Fisher introduced the idea of a \bold{\emph{randomization test}}
#'   (Manly, 2007, p. 107; Efron and Tibshirani, 1993, Chapter 15), which is based on
#'   trying to answer the question:  \dQuote{Did the observed pattern happen by chance,
#'   or does the pattern indicate the null hypothesis is not true?}  A randomization
#'   test works by simply enumerating all of the possible outcomes under the null
#'   hypothesis, then seeing where the observed outcome fits in.  A randomization test
#'   is also called a \bold{\emph{permutation test}}, because it involves permuting the
#'   observations during the enumeration procedure (Manly, 2007, p. 3).
#'
#'   In the past, randomization tests have not been used as extensively as they are now
#'   because of the \dQuote{large} computing resources needed to enumerate all of the
#'   possible outcomes, especially for large sample sizes.  The advent of more powerful
#'   personal computers and software has allowed randomization tests to become much
#'   easier to perform.  Depending on the sample size, however, it may still be too
#'   time consuming to enumerate all possible outcomes.  In this case, the randomization
#'   test can still be performed by sampling from the randomization distribution, and
#'   comparing the observed outcome to this sampled permutation distribution.
#'   \cr
#'
#'   \emph{Two-Sample Randomization Test for Proportions} \cr
#'   Let \eqn{\underline{x} = x_1, x_2, \ldots, x_{n_1}} be a vector of \eqn{n_1}
#'   independent and identically distributed (i.i.d.) observations
#'   from a \link[stats:Binomial]{binomial distribution} with parameter \code{size=1} and
#'   probability of success \code{prob=}\eqn{p_1}, and let
#'   \eqn{\underline{y} = y_1, y_2, \ldots, y_{n_2}} be a vector of \eqn{n_2}
#'   i.i.d. observations from a \link[stats:Binomial]{binomial distribution} with
#'   parameter \code{size=1} and probability of success \code{prob=}\eqn{p_2}.
#'
#'   Consider the test of the null hypothesis:
#'   \deqn{H_0: p_1 = p_2 \;\;\;\;\;\; (1)}
#'
#'   The three possible alternative hypotheses are the upper one-sided alternative
#'   (\code{alternative="greater"})
#'   \deqn{H_a: p_1 > p_2 \;\;\;\;\;\; (2)}
#'   the lower one-sided alternative (\code{alternative="less"})
#'   \deqn{H_a: p_1 < p_2 \;\;\;\;\;\; (3)}
#'   and the two-sided alternative
#'   \deqn{H_a: p_1 \ne p_2 \;\;\;\;\;\; (4)}
#'   To perform the test of the null hypothesis (1) versus any of the three
#'   alternatives (2)-(4), you can use the two-sample permutation test, which is also
#'   called \link[stats:fisher.test]{Fisher's exact test}.  When the observations are
#'   from a B(1, \eqn{p}) distribution, the sample mean is an estimate of \eqn{p}.
#'   Fisher's exact test is simply a permutation test for the difference between two
#'   means from two different groups (see \code{\link{twoSamplePermutationTestLocation}}),
#'   where the underlying populations are binomial with size parameter \code{size=1},
#'   but possibly different values of the \code{prob} parameter \eqn{p}.
#'   Fisher's exact test is usually described in terms of testing hypotheses concerning
#'   a 2 x 2 contingency table (van Bell et al., 2004, p. 157;
#'   Hollander and Wolfe, 1999, p. 473; Sheskin, 2011; Zar, 2010, p. 561).
#'   The probabilities associated with the permutation distribution can be computed by
#'   using the \link[stats:Hypergeometric]{hypergeometric distribution}.
#' }
#' @rawRd
#' \value{
#'   A list of class \code{"permutationTest"} containing the results of the hypothesis
#'   test.  See the help file for \code{\link{permutationTest.object}} for details.
#' }
#' @rawRd
#' \references{
#'   Efron, B., and R.J. Tibshirani. (1993).  \emph{An Introduction to the Bootstrap}.
#'   Chapman and Hall, New York, Chapter 15.
#'
#'   Hollander, M., and D.A. Wolfe. (1999). \emph{Nonparametric Statistical Methods}.
#'   Second Edition.  John Wiley and Sons, New York, p.473.
#'
#'   Manly, B.F.J. (2007).  \emph{Randomization, Bootstrap and Monte Carlo Methods in
#'   Biology}.  Third Edition. Chapman & Hall, New York, Chapter 6.
#'
#'   Millard, S.P., and N.K. Neerchal. (2001).
#'   \emph{Environmental Statistics with S-PLUS}.  CRC Press, Boca Raton, FL,
#'   pp.441--446.
#'
#'   Graham, S.L., K.J. Davis, W.H. Hansen, and C.H. Graham. (1975).
#'   Effects of Prolonged Ethylene Thiourea Ingestion on the Thyroid of the Rat.
#'   \emph{Food and Cosmetics Toxicology}, \bold{13}(5), 493--499.
#'
#'   Rodricks, J.V. (1992).  \emph{Calculated Risks: The Toxicity and Human Health
#'   Risks of Chemicals in Our Environment}. Cambridge University
#'   Press, New York.
#'
#'   Rodricks, J.V. (2007).  \emph{Calculated Risks: The Toxicity and Human Health
#'   Risks of Chemicals in Our Environment}. Second Edition.  Cambridge University
#'   Press, New York.
#'
#'   Sheskin, D.J. (2011).  \emph{Handbook of Parametric and Nonparametric
#'   Statistical Procedures}  Fifth Edition. CRC Press, Boca Raton, FL.
#'
#'   van Belle, G., L.D. Fisher, Heagerty, P.J., and Lumley, T. (2004).
#'   \emph{Biostatistics: A Methodology for the Health Sciences, 2nd Edition}.
#'   John Wiley & Sons, New York, p. 157.
#'
#'   Zar, J.H. (2010). \emph{Biostatistical Analysis}.
#'   Fifth Edition. Prentice-Hall, Upper Saddle River, NJ,
#'   p. 561.
#' }
#' @rawRd
#' \author{
#'   Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \note{
#'   Sometimes in environmental data analysis we are interested in determining whether
#'   two probabilities or rates or proportions differ from each other.  For example,
#'   we may ask the question: \dQuote{Does exposure to pesticide X increase the risk of
#'   developing cancer Y?}, where cancer Y may be liver cancer, stomach cancer, or some
#'   other kind of cancer.  One way environmental scientists attempt to answer this kind
#'   of question is by conducting experiments on rodents in which one group (the
#'   \dQuote{treatment} or \dQuote{exposed} group) is exposed to the pesticide and the
#'   other group (the control group) is not.  The incidence of cancer Y in the exposed
#'   group is compared with the incidence of cancer Y in the control group.  (See
#'   Rodricks (2007) for a discussion of extrapolating results from experiments involving
#'   rodents to consequences in humans and the associated difficulties).
#'
#'   Hypothesis tests you can use to compare proportions or probability of
#'   \dQuote{success} between two groups include Fisher's exact test and the test
#'   based on the normal approximation (see the \R help file for
#'   \code{\link{prop.test}}).
#' }
#' @rawRd
#' \seealso{
#'   \code{\link{permutationTest.object}}, \code{\link{plot.permutationTest}}, \cr
#'   \code{\link{twoSamplePermutationTestLocation}},
#'   \code{\link{oneSamplePermutationTest}},
#'   \link{Hypothesis Tests}, \code{\link[boot]{boot}}.
#' }
#' @rawRd
#' \examples{
#'   # Generate 10 observations from a binomial distribution with parameters
#'   # size=1 and prob=0.3, and 20 observations from a binomial distribution
#'   # with parameters size=1 and prob=0.5.  Test the null hypothesis that the
#'   # probability of "success" for the two distributions is the same against the
#'   # alternative that the probability of "success" for group 1 is less than
#'   # the probability of "success" for group 2.
#'   # (Note: the call to set.seed allows you to reproduce this example).
#'
#'   set.seed(23)
#'   dat1 <- rbinom(10, size = 1, prob = 0.3)
#'   dat2 <- rbinom(20, size = 1, prob = 0.5)
#'
#'   test.list <- twoSamplePermutationTestProportion(
#'     dat1, dat2, alternative = "less")
#'
#'   #----------
#'
#'   # Print the results of the test
#'   #------------------------------
#'   test.list
#'
#'   #Results of Hypothesis Test
#'   #--------------------------
#'   #
#'   #Null Hypothesis:                 p.x - p.y = 0
#'   #
#'   #Alternative Hypothesis:          True p.x - p.y is less than 0
#'   #
#'   #Test Name:                       Two-Sample Permutation Test
#'   #                                 Based on Differences in Proportions
#'   #                                 (Fisher's Exact Test)
#'   #
#'   #Estimated Parameter(s):          p.hat.x = 0.60
#'   #                                 p.hat.y = 0.65
#'   #
#'   #Data:                            x = dat1
#'   #                                 y = dat2
#'   #
#'   #Sample Sizes:                    nx = 10
#'   #                                 ny = 20
#'   #
#'   #Test Statistic:                  p.hat.x - p.hat.y = -0.05
#'   #
#'   #P-value:                         0.548026
#'
#'   #----------
#'
#'   # Plot the results of the test
#'   #------------------------------
#'   dev.new()
#'   plot(test.list)
#'
#'   #----------
#'
#'   # Compare to the results of fisher.test
#'   #--------------------------------------
#'   x11 <- sum(dat1)
#'   x21 <- length(dat1) - sum(dat1)
#'   x12 <- sum(dat2)
#'   x22 <- length(dat2) - sum(dat2)
#'   mat <- matrix(c(x11, x12, x21, x22), ncol = 2)
#'   fisher.test(mat, alternative = "less")
#'
#'   #Results of Hypothesis Test
#'   #--------------------------
#'   #
#'   #Null Hypothesis:                 odds ratio = 1
#'   #
#'   #Alternative Hypothesis:          True odds ratio is less than 1
#'   #
#'   #Test Name:                       Fisher's Exact Test for Count Data
#'   #
#'   #Estimated Parameter(s):          odds ratio = 0.8135355
#'   #
#'   #Data:                            mat
#'   #
#'   #P-value:                         0.548026
#'   #
#'   #95% Confidence Interval:         LCL = 0.000000
#'   #                                 UCL = 4.076077
#'
#'   #==========
#'
#'   # Rodricks (1992, p. 133) presents data from an experiment by
#'   # Graham et al. (1975) in which different groups of rats were exposed to
#'   # various concentration levels of ethylene thiourea (ETU), a decomposition
#'   # product of a certain class of fungicides that can be found in treated foods.
#'   # In the group exposed to a dietary level of 250 ppm of ETU, 16 out of 69 rats
#'   # (23%) developed thyroid tumors, whereas in the control group
#'   # (no exposure to ETU) only 2 out of 72 (3%) rats developed thyroid tumors.
#'   # If we use Fisher's exact test to test the null hypothesis that the proportion
#'   # of rats exposed to 250 ppm of ETU who will develop thyroid tumors over their
#'   # lifetime is no greater than the proportion of rats not exposed to ETU who will
#'   # develop tumors, we get a one-sided upper p-value of 0.0002.  Therefore, we
#'   # conclude that the true underlying rate of tumor incidence in the exposed group
#'   # is greater than in the control group.
#'   #
#'   # The data for this example are stored in Graham.et.al.75.etu.df.
#'
#'   # Look at the data
#'   #-----------------
#'
#'   Graham.et.al.75.etu.df
#'   #  dose tumors  n proportion
#'   #1    0      2 72 0.02777778
#'   #2    5      2 75 0.02666667
#'   #3   25      1 73 0.01369863
#'   #4  125      2 73 0.02739726
#'   #5  250     16 69 0.23188406
#'   #6  500     62 70 0.88571429
#'
#'   # Perform the test for a difference in tumor rates
#'   #-------------------------------------------------
#'
#'   Num.Tumors <- with(Graham.et.al.75.etu.df, tumors[c(5, 1)])
#'   Sample.Sizes <- with(Graham.et.al.75.etu.df, n[c(5, 1)])
#'
#'   test.list <- twoSamplePermutationTestProportion(
#'     x = Num.Tumors, y = Sample.Sizes,
#'     x.and.y="Number Successes and Trials", alternative = "greater")
#'
#'   #----------
#'
#'   # Print the results of the test
#'   #------------------------------
#'   test.list
#'
#'   #Results of Hypothesis Test
#'   #--------------------------
#'   #
#'   #Null Hypothesis:                 p.x - p.y = 0
#'   #
#'   #Alternative Hypothesis:          True p.x - p.y is greater than 0
#'   #
#'   #Test Name:                       Two-Sample Permutation Test
#'   #                                 Based on Differences in Proportions
#'   #                                 (Fisher's Exact Test)
#'   #
#'   #Estimated Parameter(s):          p.hat.x = 0.23188406
#'   #                                 p.hat.y = 0.02777778
#'   #
#'   #Data:                            x = Num.Tumors
#'   #                                 n = Sample.Sizes
#'   #
#'   #Sample Sizes:                    nx = 69
#'   #                                 ny = 72
#'   #
#'   #Test Statistic:                  p.hat.x - p.hat.y = 0.2041063
#'   #
#'   #P-value:                         0.0002186462
#'
#'   #----------
#'
#'   # Plot the results of the test
#'   #------------------------------
#'   dev.new()
#'   plot(test.list)
#'
#'   #==========
#'
#'   # Clean up
#'   #---------
#'   rm(test.list, x11, x12, x21, x22, mat, Num.Tumors, Sample.Sizes)
#'   #graphics.off()
#' }
#' @rawRd
#' \keyword{htest}
#' @rawRd
#' \keyword{models}

twoSamplePermutationTestProportion <-
function (x, y, x.and.y = "Binomial Outcomes", alternative = "two.sided", 
    tol = sqrt(.Machine$double.eps)) 
{
    x.and.y <- match.arg(x.and.y, c("Binomial Outcomes", "Number Successes and Trials"))
    alternative <- match.arg(alternative, c("two.sided", "less", 
        "greater"))
    x.name <- deparse(substitute(x))
    y.name <- deparse(substitute(y))
    data.name <- c(x.name, y.name)
    if (x.and.y == "Number Successes and Trials") {
        if (length(x) != 2 || !all(is.finite(x)) || !all(x == 
            trunc(x)) || !all(x >= 0)) 
            stop(paste("When x.and.y=\"Number Successes and Trials\",", 
                "'x' must be a numeric vector", "with two elements, and both elements must be", 
                "non-negative integers"))
        if (length(y) != 2 || !all(is.finite(y)) || !all(y == 
            trunc(y)) || !all(y >= 1) || !all(y >= x)) 
            stop(paste("When x.and.y=\"Number Successes and Trials\",", 
                "'y' must be a numeric vector", "with two elements, both elements must be", 
                "positive integers, and each element of 'y' must be", 
                "at least as large as the corresponding element", 
                "of 'x'"))
        bad.obs <- NULL
        names(data.name) <- c("x", "n")
        x1 <- x[1]
        x2 <- x[2]
        n1 <- y[1]
        n2 <- y[2]
    }
    else {
        x <- as.numeric(x)
        y <- as.numeric(y)
        if ((bad.obs <- sum(!(x.ok <- is.finite(x)))) > 0) {
            is.not.finite.warning(x)
            x <- x[x.ok]
            warning(paste(bad.obs, "observations with NA/NaN/Inf in 'x' removed."))
        }
        if ((bad.obs <- sum(!(y.ok <- is.finite(y)))) > 0) {
            is.not.finite.warning(y)
            y <- y[y.ok]
            warning(paste(bad.obs, "observations with NA/NaN/Inf in 'y' removed."))
        }
        names(data.name) <- c("x", "y")
        n1 <- length(x)
        n2 <- length(y)
        if (n1 < 2 || n2 < 2) 
            stop(paste("When x.and.y=\"Binomial Outcomes\",", 
                "'x' and 'y' must each contain", "at least two non-missing observations"))
        levels.x.y <- sort(unique(c(x, y)))
        if (length(levels.x.y) > 2) {
            stop(paste("When x.and.y=\"Binomial Outcomes\",", 
                "'x' and 'y' must take only at most two unique values"))
        }
        if (length(levels.x.y) == 1) {
            x1 <- 0
            x2 <- 0
        }
        else {
            x1 <- sum(x == levels.x.y[2])
            x2 <- sum(y == levels.x.y[2])
        }
    }
    method <- paste("Two-Sample Permutation Test\n", space(33), 
        "Based on Differences in Proportions\n", space(33), "(Fisher's Exact Test)", 
        sep = "")
    estimate <- c(x1/n1, x2/n2)
    names(estimate) <- c("p.hat.x", "p.hat.y")
    n <- n1 + n2
    n.1s <- x1 + x2
    n.0s <- n - n.1s
    x.1s <- max(0, n1 - n.0s):min(n1, n.1s)
    stat.dist <- x.1s/n1 - (n.1s - x.1s)/n2
    probs.stat.dist <- dhyper(x = x.1s, m = n.1s, n = n.0s, k = n1)
    stat <- -diff(estimate)
    if (alternative == "two.sided") {
        abs.stat.dist <- sort(unique(abs(stat.dist)))
        n.temp <- length(abs.stat.dist)
        probs.abs.stat.dist <- numeric(length(n.temp))
        for (i in 1:n.temp) {
            probs.abs.stat.dist[i] <- sum(probs.stat.dist[abs(stat.dist) == 
                abs.stat.dist[i]])
        }
        stat <- abs(stat)
        names(stat) <- "|p.hat.x - p.hat.y|"
        stat.dist <- abs.stat.dist
        probs.stat.dist <- probs.abs.stat.dist
        p.value <- sum(probs.stat.dist[stat.dist >= stat - tol])
    }
    else {
        names(stat) <- "p.hat.x - p.hat.y"
        if (alternative == "less") 
            p.value <- sum(probs.stat.dist[stat.dist <= stat + 
                tol])
        else p.value <- sum(probs.stat.dist[stat.dist >= stat - 
            tol])
    }
    parameters <- NULL
    null.value <- 0
    names(null.value) <- "p.x - p.y"
    sample.size <- c(nx = n1, ny = n2)
    ret.list <- list(statistic = stat, parameters = parameters, 
        p.value = p.value, estimate = estimate, null.value = null.value, 
        alternative = alternative, method = method, estimation.method = NULL, 
        sample.size = sample.size, data.name = data.name, bad.obs = bad.obs, 
        stat.dist = stat.dist, probs.stat.dist = probs.stat.dist, 
        exact = TRUE)
    oldClass(ret.list) <- "permutationTest"
    ret.list
}
