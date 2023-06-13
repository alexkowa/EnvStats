\name{simulateMvMatrix}
\alias{simulateMvMatrix}
\title{
  Simulate a Multivariate Matrix Based on a Specified Rank Correlation Mat
}
\description{
  Simulate a multivariate matrix of random numbers from specified theoretical
  probability distributions and/or empirical probability distributions based on
  a specified rank correlation matrix, using either Latin Hypercube sampling or
  simple random sampling.
}
\usage{
  simulateMvMatrix(n, distributions = c(Var.1 = "norm", Var.2 = "norm"),
    param.list = list(Var.1 = list(mean = 0, sd = 1), Var.2 = list(mean = 0, sd = 1)),
    cor.mat = diag(length(distributions)), sample.method = "SRS", seed = NULL,
    left.tail.cutoff = ifelse(is.finite(supp.min), 0, .Machine$double.eps),
    right.tail.cutoff = ifelse(is.finite(supp.max), 0, .Machine$double.eps),
    tol.1 = .Machine$double.eps, tol.symmetry = .Machine$double.eps,
    tol.recip.cond.num = .Machine$double.eps, max.iter = 10)
}
\arguments{
  \item{n}{
  a positive integer indicating the number of random vectors (i.e., the number of
  rows of the matrix) to generate.
}
  \item{distributions}{
  a character vector of length \eqn{k} denoting the distribution abbreviations for
  each of the \eqn{k} distributions.  If there is a \code{names} attribute associated
  with this character vector, these names will be the column names of the resulting
  matrix.  The default value of \code{distributions} is
  \code{c(Var.1="norm", Var.2="norm")}, indicating that \eqn{k=2}, both
  distributions are the normal distribution, and the column names of the
  resulting \eqn{n \times k} matrix will be \code{"Var.1"} and \code{"Var.2"}.
  See the help file for \code{\link{Distribution.df}} for a list of possible
  distribution abbreviations.

  Alternatively, the character string \code{"emp"} may be used to denote sampling
  from an empirical distribution based on a set of observations.  The vector
  containing the observations is specified in the argument \code{param.list}.
}
  \item{param.list}{
  a list containing \eqn{k} lists that specify the values for the parameters of
  the \eqn{k} distributions.  If \code{param.list} has a \code{names} attribute
  (not necessary), the \code{names} attribute should be exactly the same as the
  \code{names} attribute of the argument \code{distributions}.  The default value
  of \code{param.list} is \cr
  \code{list(Var.1=list(mean=0, sd=1), Var.2=list(mean=0, sd=1))}.
  See the help file for \code{\link{Distribution.df}} for the names and
  possible values of the parameters associated with each distribution.

  Alternatively, if you specify an empirical distribution for the \eqn{j}'th
  distribution by setting the \eqn{j}'th element of \code{distribution} to
  \code{"emp"}, then the \eqn{j}'th component of \code{param.list} must be a
  list of the form \code{list(obs=}\emph{name}\code{)}, where \emph{name} denotes the
  name of the vector containing the observations to use for the empirical
  distribution.  In this case, you may also supply arguments to the
  \code{\link{qemp}} function through the \eqn{j}'th component of \code{param.list}.
  For example, you may set this component to
  \code{list(obs=}\emph{name}\code{, discrete=T)} to
  specify an empirical distribution based on a discrete random variable.
}
  \item{cor.mat}{
  a \eqn{k \times k} matrix specifying the rank correlations between the
  \eqn{k} distributions.  This argument must be a positive definite symmetric
  matrix, with all 1's on the diagonal.  All elements on the off-diagonal must be
  between -1 and 1. The default value is the \eqn{k \times k} identity matrix,
  specifying no rank correlation between any of the variables.
}
  \item{sample.method}{
  a character vector of length 1 or \eqn{k} indicating, for each distribution,
  whether to use Latin Hypercube sampling or simple random sampling.  If
  \code{sample.method} is of length 1, it is replicated to length \eqn{k}.
  Each element of \code{sample.method} must be the character string
  \code{"LHS"} (Latin Hypercube sampling) or \code{"SRS"} (simple random sampling),
  or an abbreviation of one of these strings. The default value is \code{"SRS"},
  indicating simple random sampling for each distribution.  Note that by specifying
  \code{sample.method} as a vector of length \eqn{k}, you may use different sampling
  methods for different distributions.
}
  \item{seed}{
  integer to supply to the \R function \code{\link{set.seed}}.
  The default value is \code{seed=NULL}, in which case the random seed is
  not set but instead based on the current value of \code{.Random.seed}.
}
  \item{left.tail.cutoff}{
  a numeric vector of length \eqn{k} indicating, for each distribution,
  what proportion of the left-tail of the probability distribution to omit
  for Latin Hypercube sampling.  All elements of \code{left.tail.cutoff}
  must be between 0 and 1.
  For densities with a finite support minimum (e.g., \link{Lognormal} or
  \link{Empirical}) the default value is \code{left.tail.cutoff=0};
  for densities with a support minimum of \eqn{-\infty}, the default value is
  \code{left.tail.cutoff=.Machine$double.eps}.
  The \eqn{j}'th element of this argument is ignored if
  the \eqn{j}'th element of \code{sample.method} is equal to \code{"SRS"}.
}
  \item{right.tail.cutoff}{
  a numeric vector of length \eqn{k} indicating, for each distribution,
  what proportion of the right-tail of the probability distribution to omit
  for Latin Hypercube sampling.  All elements of \code{right.tail.cutoff}
  must be between 0 and 1.
  For densities with a finite support maximum (e.g., \link{Beta} or
  \link{Empirical}) the default value is \code{right.tail.cutoff=0};
  for densities with a support maximum of \eqn{\infty}, the default value
  is \code{right.tail.cutoff=.Machine$double.eps}.
  The \eqn{j}'th element of this argument is ignored if
  the \eqn{j}'th element of \code{sample.method} is equal to \code{"SRS"}.
}
  \item{tol.1}{
  a positive numeric scalar indicating the allowable absolute deviation
  from 1 for the diagonal elements of \code{cor.mat}.  The default value
  is \code{.Machine$double.eps}.
}
  \item{tol.symmetry}{
  a positive numeric scalar indicating the allowable absolute deviation from
  0 for the difference between symmetric elements of \code{cor.mat}
  (e.g., \cr
  \code{abs(cor.mat[3,2]-cor.mat[2,3])}. The default value is \cr
  \code{.Machine$double.eps}.
}
  \item{tol.recip.cond.num}{
  a positive numeric scalar indicating the allowable minimum value of the
  reciprocal of the condition number for \code{cor.mat}.  The condition number
  is defined to be the largest eigen value divided by the smallest eigen value.
  The reciprocal of the condition number is some number between 0 and 1.
  This value must be sufficiently large for \code{cor.mat} to be of full rank
  (i.e., to not be singular).  The default value of \code{tol.recip.cond.num}
  is \code{.Machine$double.eps}.
}
  \item{max.iter}{
  a positive integer indicating the maximum number of iterations to use to
  produce the \eqn{R} matrix in the algorithm to create the output matrix.
  The sample correlation matrix of \eqn{R} must be positive definite.
  The number of iterations will rarely be more than 2 for moderate to
  large sample sizes (e.g., \eqn{n > 2k}).  The default value is
  \code{max.iter=10}.  See the DETAILS section below for more information on
  the \eqn{R} matrix.
}
}
\details{
  \bold{Motivation} \cr
  In risk assessment and Monte Carlo simulation, the outcome variable of interest,
  say \eqn{Y}, is usually some function of one or more other random variables:
  \deqn{Y = h(\underline{X}) = h(X_1, X_2, \ldots, X_k) \;\;\;\;\;\; (1)}
  For example, \eqn{Y} may be the incremental lifetime cancer risk due to
  ingestion of soil contaminated with benzene (Thompson et al., 1992;
  Hamed and Bedient, 1997).  In this case the random vector \eqn{\underline{X}}
  may represent observations from several kinds of distributions that characterize
  exposure and dose-response, such as benzene concentration in the soil,
  soil ingestion rate, average body weight, the cancer potency factor for benzene,
  etc.  These distributions may or may not be assumed to be independent of one
  another (Smith et al., 1992; Bukowski et al., 1995).  Often, input variables in a
  Monte Carlo simulation are in fact known to be correlated, such as body weight
  and dermal area.

  Characterizing the joint distribution of a random vector \eqn{\underline{X}},
  where different elements of \eqn{\underline{X}} come from different distributions,
  is usually mathematically complex or impossible unless the elements
  (random variables) of \eqn{\underline{X}} are independent.
  Iman and Conover (1982) present an algorithm for creating a set of \eqn{n}
  multivariate observations with a rank correlation matrix that is approximately
  equal to a specified rank correlation matrix.  This method allows for different
  probability distributions for each element of the multivariate vector.  The
  details of this algorithm are as follows.
  \cr

  \bold{Algorithm} \cr
  \enumerate{
  \item Specify \eqn{n}, the desired number of random vectors (i.e., number of
    rows of the \eqn{n \times k} output matrix).  This is specified by the
    argument \code{n} for the function \code{simulateMvMatrix}.

  \item Create \eqn{C}, the desired \eqn{k \times k} correlation matrix.  This is
    specified by the argument \code{cor.mat}.

  \item Compute \eqn{P}, where \eqn{P} is a lower triangular \eqn{k \times k}
    matrix and
    \deqn{PP^{'} = C \;\;\;\;\;\; (2)}
    where \eqn{P^{'}} denotes the transpose of \eqn{P}.  The function
    \code{simulateMvMatrix} uses the Cholesky decomposition to compute P
    (see the \R help file for \code{\link{chol}}).

 \item Create \eqn{R}, an \eqn{n \times k} matrix, whose columns represent
    \eqn{k} independent permutations of van der Waerden scores.  That is, each
    column of \eqn{R} is a random permutation of the scores
    \deqn{\Phi^{-1}(\frac{i}{n+1}), \; i = 1, 2, \ldots, n \;\;\;\;\;\; (3)}
    where \eqn{\Phi} denotes the cumulative distribution function of the standard
    normal distribution.

  \item Compute \eqn{T}, the \eqn{k \times k} Pearson sample correlation matrix
    of \eqn{R}.
    Make sure \eqn{T} is positive definite; if it is not, then repeat step 4.

  \item Compute \eqn{Q}, where \eqn{Q} is a lower triangular \eqn{k \times k}
    matrix and
    \deqn{QQ^{'} = T \;\;\;\;\;\; (4)}
    The function \code{simulateMvMatrix} uses the Cholesky decomposition to compute
    \eqn{Q} (see the \R help file for \code{\link{chol}}).

  \item Compute the lower triangular \eqn{k \times k} matrix \eqn{S}, where
    \deqn{S = PQ^{-1} \;\;\;\;\;\; (5)}

  \item Compute the matrix \eqn{R^{*}}, where
    \deqn{R^{*} = RS^{'} \;\;\;\;\;\; (6)}

  \item Generate an \eqn{n \times k} matrix of random numbers \eqn{\underline{X}},
    where each column of \eqn{\underline{X}} comes from the distribution specified
    by the arguments \code{distributions} and \code{param.list}.  Generate each
    column of random numbers independently of the other columns.  If the \eqn{j}'th
    element of \code{sample.method} equals \code{"SRS"}, use simple random sampling
    to generate the random numbers for the \eqn{j}'th column of \eqn{\underline{X}}.
    If the \eqn{j}'th element of \code{sample.method} equals \code{"LHS"}, use
    Latin Hypercube sampling to generate the random numbers for the \eqn{j}'th column
    of \eqn{\underline{X}}.  At this stage in the algorithm, the function
    \code{simulateMvMatrix} calls the function \code{\link{simulateVector}} to
    create each column of \eqn{\underline{X}}.

  \item Order the observations within each column of \eqn{\underline{X}} so that
    the order of the ranks within each column of \eqn{\underline{X}} matches the
    order of the ranks within each column of \eqn{R^{*}}.  This way, \eqn{\underline{X}}
    and \eqn{R^{*}} have exactly the same sample rank correlation matrix.
  }

  \bold{Explanation} \cr
  Iman and Conover (1982) present two algorithms for computing an \eqn{n \times k}
  output matrix with a specified rank correlation.  The algorithm presented above is
  the second, more complicated one.  In order to explain the reasoning behind this
  algorithm, we need to explain the simple algorithm first.
  \cr

  \emph{Simple Algorithm} \cr
  Let \eqn{R_i} denote the \eqn{i}'th row vector of the matrix \eqn{R}, the
  matrix of scores.  This row vector has a population correlation matrix of \eqn{I},
  where \eqn{I} denotes the \eqn{k \times k} identity matrix.  Thus, the
  \eqn{1 \times k} vector \eqn{R_i P^{'}} has a population correlation matrix equal to
  \eqn{C}. Therefore, if we define \eqn{R^{*}} by
  \deqn{R^{*} = RP^{'} \;\;\;\;\;\; (7)}
  each row of \eqn{R^{*}} has the same multivariate distribution with population
  correlation matrix \eqn{C}.  The rank correlation matrix of \eqn{R^{*}} should
  therefore be close to \eqn{C}.  Ordering the columns of \eqn{\underline{X}} as
  described in Step 10 above will yield a matrix of observations with the
  specified distributions and the exact same rank correlation matrix as the
  rank correlation matrix of \eqn{R^{*}}.

  Iman and Conover (1982) use van der Waerden scores instead of raw ranks to create
  \eqn{R} because van der Waerden scores yield more "natural-looking" pairwise
  scatterplots.

  If the Pearson sample correlation matrix of \eqn{R}, denoted \eqn{T} in Step 5
  above, is exactly equal to the true population correlation matrix \eqn{I},
  then the sample correlation matrix of \eqn{R^{*}} is exactly equal to \eqn{C},
  and the rank correlation matrix of \eqn{R^{*}} is approximately equal to \eqn{C}.
  The Pearson sample correlation matrix of \eqn{R}, however, is an estimate of the
  true population correlation matrix \eqn{I}, and is therefore
  \dQuote{bouncing around} \eqn{I}.  Likewise, the Pearson sample correlation matrix
  of \eqn{R^{*}} is an estimate of the true population correlation matrix \eqn{C},
  and is therefore bouncing around \eqn{C}.  Using this simple algorithm, the
  Pearson sample correlation matrix of \eqn{R^{*}}, as \eqn{R^{*}} is defined in
  Equation (7) above, may not be \dQuote{close} enough to the desired rank
  correlation matrix \eqn{C}, and thus the rank correlation of \eqn{R^{*}} will not
  be close enough to \eqn{C}.  Iman and Conover (1982), therefore present a more
  complicated algorithm.
  \cr

  \emph{More Complicated Algorithm} \cr
  To get around the problem mentioned above, Iman and Conover (1982) find a
  \eqn{k \times k} lower triangular matrix \eqn{S} such that the matrix \eqn{R^{*}}
  as defined in Equation (6) above has a correlation matrix exactly equal to \eqn{C}.
  The formula for \eqn{S} is given in Steps 6 and 7 of the algorithm above.

  Iman and Conover (1982, p.330) note that even if the desired rank correlation matrix
  \eqn{C} is in fact the identity matrix \eqn{I}, this method of generating the
  matrix will produce a matrix with an associated rank correlation that more closely
  resembles \eqn{I} than you would get by simply generating random numbers within
  each column of \eqn{\underline{X}}.
}
\value{
  A numeric matrix of dimension \eqn{n \times k} of random numbers,
  where the \eqn{j}'th column of numbers comes from the distribution
  specified by the \eqn{j}'th elements of the arguments \code{distributions}
  and \code{param.list}, and the rank correlation of this matrix is
  approximately equal to the argument \code{cor.mat}.  The value of \eqn{n}
  is determined by the argument \code{n}, and the value of \eqn{k} is
  determined by the length of the argument \code{distributions}.
}
\references{
  Bukowski, J., L. Korn, and D. Wartenberg. (1995).
  Correlated Inputs in Quantitative Risk Assessment: The Effects of Distributional
  Shape.  \emph{Risk Analysis} \bold{15}(2), 215--219.

  Hamed, M., and P.B. Bedient. (1997).  On the Effect of Probability Distributions
  of Input Variables in Public Health Risk Assessment.  \emph{Risk Analysis}
  \bold{17}(1), 97--105.

  Iman, R.L., and W.J. Conover. (1980).  Small Sample Sensitivity Analysis
  Techniques for Computer Models, With an Application to Risk Assessment
  (with Comments).  \emph{Communications in Statistics--Volume A, Theory and Methods},
  \bold{9}(17), 1749--1874.

  Iman, R.L., and W.J. Conover. (1982).  A Distribution-Free Approach to Inducing
  Rank Correlation Among Input Variables.
  \emph{Communications in Statistics--Volume B, Simulation and Computation},
  \bold{11}(3), 311--334.

  Iman, R.L., and J.M. Davenport. (1982).  Rank Correlation Plots For Use With
  Correlated Input Variables.
  \emph{Communications in Statistics--Volume B, Simulation and Computation},
  \bold{11}(3), 335--360.

  Iman, R.L., and J.C. Helton. (1988).  An Investigation of Uncertainty and
  Sensitivity Analysis Techniques for Computer Models.  \emph{Risk Analysis}
  \bold{8}(1), 71--90.

  Iman, R.L. and J.C. Helton. (1991).  The Repeatability of Uncertainty and
  Sensitivity Analyses for Complex Probabilistic Risk Assessments.
  \emph{Risk Analysis} \bold{11}(4), 591--606.

  McKay, M.D., R.J. Beckman., and W.J. Conover. (1979).  A Comparison of Three
  Methods for Selecting Values of Input Variables in the Analysis of Output
  From a Computer Code.  \emph{Technometrics} \bold{21}(2), 239--245.

  Millard, S.P. (2013).  \emph{EnvStats: an R Package for Environmental Statistics}.
  Springer, New York.  \url{https://link.springer.com/book/10.1007/978-1-4614-8456-1}.

  Smith, A.E., P.B. Ryan, and J.S. Evans. (1992).  The Effect of Neglecting
  Correlations When Propagating Uncertainty and Estimating the Population
  Distribution of Risk.  \emph{Risk Analysis} \bold{12}(4), 467--474.

  Thompson, K.M., D.E. Burmaster, and E.A.C. Crouch. (1992).  Monte Carlo Techniques
  for Quantitative Uncertainty Analysis in Public Health Risk Assessments.
  \emph{Risk Analysis} \bold{12}(1), 53--63.

  Vose, D. (2008).  \emph{Risk Analysis:  A Quantitative Guide}.  Third Edition.
  John Wiley & Sons, West Sussex, UK, 752 pp.
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  Monte Carlo simulation and risk assessment often involve looking at the
  distribution or characteristics of the distribution of some outcome variable
  that depends upon several input variables (see Equation (1) above).  Usually
  these input variables can be considered random variables.  An important part
  of both sensitivity analysis and uncertainty analysis involves looking at how
  the distribution of the outcome variable changes with changing assumptions on
  the input variables.  One important assumption is the correlation between the
  input random variables.

  Often, the input random variables are assumed to be independent when in fact they
  are know to be correlated (Smith et al., 1992; Bukowski et al., 1995).  It is
  therefore important to assess the effect of the assumption of independence on the
  distribution of the outcome variable.  One way to assess the effect of this
  assumption is to run the Monte Carlo simulation assuming independence and then
  also run it assuming certain forms of correlations among the input variables.

  Iman and Davenport (1982) present a series of scatterplots showing \dQuote{typical}
  scatterplots with various distributions on the \eqn{x}- and \eqn{y}-axes and
  various assumed rank correlations.  These plots are meant to aid in developing
  reasonable estimates of rank correlation between input variables.  These plots can
  easily be produced using the \code{simulateMvMatrix} and \code{\link{plot}} functions.
}
\seealso{
  \link{Probability Distributions and Random Numbers}, \link{Empirical},
  \code{\link{simulateVector}}, \code{\link{cor}}, \code{\link{set.seed}}.
}
\examples{
  # Generate 5 observations from a standard bivariate normal distribution
  # with a rank correlation matrix (approximately) equal to the 2 x 2
  # identity matrix, using simple random sampling for each
  # marginal distribution.

  simulateMvMatrix(5, seed = 47)
  #           Var.1       Var.2
  #[1,]  0.01513086  0.03960243
  #[2,] -1.08573747  0.09147291
  #[3,] -0.98548216  0.49382018
  #[4,] -0.25204590 -0.92245624
  #[5,] -1.46575030 -1.82822917

  #==========

  # Look at the observed rank correlation matrix for 100 observations
  # from a standard bivariate normal distribution with a rank correlation matrix
  # (approximately) equal to the 2 x 2 identity matrix. Compare this observed
  # rank correlation matrix with the observed rank correlation matrix based on
  # generating two independent sets of standard normal random numbers.
  # Note that the cross-correlation is closer to 0 for the matrix created with
  # simulateMvMatrix.

  cor(simulateMvMatrix(100, seed = 47), method = "spearman")
  #             Var.1        Var.2
  #Var.1  1.000000000 -0.005976598
  #Var.2 -0.005976598  1.000000000

  cor(matrix(simulateVector(200, seed = 47), 100 , 2), method = "spearman")
  #            [,1]        [,2]
  #[1,]  1.00000000 -0.05374137
  #[2,] -0.05374137  1.00000000

  #==========

  # Generate 1000 observations from a bivariate distribution, where the first
  # distribution is a normal distribution with parameters mean=10 and sd=2,
  # the second distribution is a lognormal distribution with parameters
  # mean=10 and cv=1, and the desired rank correlation between the two
  # distributions is 0.8.  Look at the observed rank correlation matrix, and
  # plot the results.

  mat <- simulateMvMatrix(1000,
    distributions = c(N.10.2 = "norm", LN.10.1 = "lnormAlt"),
    param.list = list(N.10.2  = list(mean=10, sd=2),
                      LN.10.1 = list(mean=10, cv=1)),
    cor.mat = matrix(c(1, .8, .8, 1), 2, 2), seed = 47)

  round(cor(mat, method = "spearman"), 2)
  #        N.10.2 LN.10.1
  #N.10.2    1.00    0.78
  #LN.10.1   0.78    1.00

  dev.new()
  plot(mat, xlab = "Observations from N(10, 2)",
    ylab = "Observations from LN(mean=10, cv=1)",
    main = "Lognormal vs. Normal Deviates with Rank Correlation 0.8")

  #----------

  # Repeat the last example, but use Latin Hypercube sampling for both
  # distributions. Note the wider range on the y-axis.

  mat.LHS <- simulateMvMatrix(1000,
    distributions = c(N.10.2 = "norm", LN.10.1 = "lnormAlt"),
    param.list = list(N.10.2  = list(mean=10, sd=2),
                      LN.10.1 = list(mean=10, cv=1)),
    cor.mat = matrix(c(1, .8, .8, 1), 2, 2),
    sample.method = "LHS", seed = 298)

  round(cor(mat.LHS, method = "spearman"), 2)
  #        N.10.2 LN.10.1
  #N.10.2    1.00    0.79
  #LN.10.1   0.79    1.00

  dev.new()
  plot(mat.LHS, xlab = "Observations from N(10, 2)",
    ylab = "Observations from LN(mean=10, cv=1)",
    main = paste("Lognormal vs. Normal Deviates with Rank Correlation 0.8",
      "(Latin Hypercube Sampling)", sep = "\n"))

  #==========

  # Generate 1000 observations from a multivariate distribution, where the
  # first distribution is a normal distribution with parameters
  # mean=10 and sd=2, the second distribution is a lognormal distribution
  # with parameters mean=10 and cv=1, the third distribution is a beta
  # distribution with parameters shape1=2 and shape2=3, and the fourth
  # distribution is an empirical distribution of 100 observations that
  # we'll generate from a Pareto distribution with parameters
  # location=10 and shape=2. Set the desired rank correlation matrix to:

  cor.mat <- matrix(c(1, .8, 0, .5, .8, 1, 0, .7,
    0, 0, 1, .2, .5, .7, .2, 1), 4, 4)

  cor.mat
  #     [,1] [,2] [,3] [,4]
  #[1,]  1.0  0.8  0.0  0.5
  #[2,]  0.8  1.0  0.0  0.7
  #[3,]  0.0  0.0  1.0  0.2
  #[4,]  0.5  0.7  0.2  1.0

  # Use Latin Hypercube sampling for each variable, look at the observed
  # rank correlation matrix, and plot the results.

  pareto.rns <- simulateVector(100, "pareto",
    list(location = 10, shape = 2), sample.method = "LHS",
    seed = 56)

  mat <- simulateMvMatrix(1000,
    distributions = c(Normal = "norm", Lognormal = "lnormAlt",
      Beta = "beta", Empirical = "emp"),
    param.list = list(Normal = list(mean=10, sd=2),
                      Lognormal = list(mean=10, cv=1),
                      Beta = list(shape1 = 2, shape2 = 3),
                      Empirical = list(obs = pareto.rns)),
    cor.mat = cor.mat, seed = 47, sample.method = "LHS")

  round(cor(mat, method = "spearman"), 2)
  #          Normal Lognormal  Beta Empirical
  #Normal      1.00      0.78 -0.01      0.47
  #Lognormal   0.78      1.00 -0.01      0.67
  #Beta       -0.01     -0.01  1.00      0.19
  #Empirical   0.47      0.67  0.19      1.00

  dev.new()
  pairs(mat)

  #==========

  # Clean up
  #---------
  rm(mat, mat.LHS, pareto.rns)
  graphics.off()
}
\keyword{ distribution }
\keyword{ datagen }
