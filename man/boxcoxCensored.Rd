\name{boxcoxCensored}
\alias{boxcoxCensored}
\alias{Box-Cox Censored}
\alias{BoxCox Censored}
\alias{Box-Cox Transformation for Censored Data}
\alias{BoxCox Transformation for Censored Data}
\title{
  Boxcox Power Transformation for Type I Censored Data
}
\description{
  Compute the value(s) of an objective for one or more Box-Cox power transformations, 
  or to compute an optimal power transformation based on a specified objective, based on 
  Type I censored data.
}
\usage{
  boxcoxCensored(x, censored, censoring.side = "left", 
    lambda = {if (optimize) c(-2, 2) else seq(-2, 2, by = 0.5)}, optimize = FALSE, 
    objective.name = "PPCC", eps = .Machine$double.eps, 
    include.x.and.censored = TRUE, prob.method = "michael-schucany", 
    plot.pos.con = 0.375) 
}
\arguments{
  \item{x}{
  a numeric vector of positive numbers.  
  Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{-Inf, Inf}) 
  values are allowed but will be removed.
}
  \item{censored}{
  numeric or logical vector indicating which values of \code{x} are censored.  This must be the 
  same length as \code{x}.  If the mode of \code{censored} is \code{"logical"}, \code{TRUE} values 
  correspond to elements of \code{x} that are censored, and \code{FALSE} values correspond to 
  elements of \code{x} that are not censored.  If the mode of \code{censored} is \code{"numeric"}, 
  it must contain only \code{1}'s and \code{0}'s; \code{1} corresponds to \code{TRUE} and 
  \code{0} corresponds to \code{FALSE}.  Missing (\code{NA}) values are allowed but will be removed.
}
  \item{censoring.side}{
  character string indicating on which side the censoring occurs.  The possible values are 
  \code{"left"} (the default) and \code{"right"}. 
}
  \item{lambda}{
  numeric vector of finite values indicating what powers to use for the 
  Box-Cox transformation.  When \code{optimize=FALSE}, the default value is \cr
  \code{lambda=seq(-2, 2, by=0.5)}.  When \code{optimize=TRUE}, \code{lambda} 
  must be a vector with two values indicating the range over which the 
  optimization will occur and the range of these two values must include 1.  
  In this case, the default value is \code{lambda=c(-2, 2)}.
}
  \item{optimize}{
  logical scalar indicating whether to simply evalute the objective function at the 
  given values of \code{lambda} (\code{optimize=FALSE}; the default), or to compute 
  the optimal power transformation within the bounds specified by 
  \code{lambda} (\code{optimize=TRUE}).
}
  \item{objective.name}{
  character string indicating what objective to use. The possible values are
  \code{"PPCC"} (probability plot correlation coefficient; the default), 
  \code{"Shapiro-Wilk"} (the Shapiro-Wilk goodness-of-fit statistic), and 
  \code{"Log-Likelihood"} (the log-likelihood function). 
}
  \item{eps}{
  finite, positive numeric scalar.  When the absolute value of \code{lambda} is less 
  than \code{eps}, lambda is assumed to be 0 for the Box-Cox transformation.  
  The default value is \code{eps=.Machine$double.eps}.
}
  \item{include.x.and.censored}{
  logical scalar indicating whether to include the finite, non-missing values of 
  the argument \code{x} and the corresponding values of \code{censored} with the 
  returned object.  The default value is \code{include.x.and.censored=TRUE}.
}
  \item{prob.method}{
  for multiply censored data, 
  character string indicating what method to use to compute the plotting positions 
  (empirical probabilities) when \cr
  \code{objective.name="PPCC"}.  Possible values are: \cr

  \code{"kaplan-meier"} (product-limit method of Kaplan and Meier (1958)), \cr
  \code{"modified kaplan-meier"} (same as \code{"kaplan-meier"} with the maximum value included), \cr
  \code{"nelson"} (hazard plotting method of Nelson (1972)), \cr 
  \code{"michael-schucany"} (generalization of the product-limit method due to Michael and Schucany (1986)), and \cr 
  \code{"hirsch-stedinger"} (generalization of the product-limit method due to Hirsch and Stedinger (1987)). \cr

  The default value is \code{prob.method="michael-schucany"}. \cr 

  The \code{"nelson"} method is only available for \code{censoring.side="right"}, and 
  the \code{"modified kaplan-meier"} is only available for \code{censoring.side="left"}.  
  See the DETAILS section for more explanation.

  This argument is ignored if \code{objective.name} is not equal to \code{"PPCC"} 
  and/or the data are singly censored.
}
  \item{plot.pos.con}{
  for multiply censored data, 
  numeric scalar between 0 and 1 containing the value of the plotting position 
  constant when \code{objective.name="PPCC"}.  
  The default value is \code{plot.pos.con=0.375}.  See the DETAILS section 
  for more information.  
  This argument is used only if \code{prob.method} is equal to 
  \code{"michael-schucany"} or \code{"hirsch-stedinger"}.

  This argument is ignored if \code{objective.name} is not equal to \code{"PPCC"} 
  and/or the data are singly censored.
}
}
\details{
  Two common assumptions for several standard parametric hypothesis tests are:
  \enumerate{
  \item The observations all come from a normal distribution.
  \item The observations all come from distributions with the same variance.
  }
  For example, the standard one-sample t-test assumes all the observations 
  come from the same normal distribution, and the standard two-sample t-test 
  assumes that all the observations come from a normal distribution with the same 
  variance, although the mean may differ between the two groups.  

  When the original data do not satisfy the above assumptions, data transformations 
  are often used to attempt to satisfy these assumptions.  
  Box and Cox (1964) presented a formalized method for deciding on a data 
  transformation.  Given a random variable \eqn{X} from some distribution with 
  only positive values, the Box-Cox family of power transformations is defined as:
  \tabular{llll}{
  \eqn{Y} \tab = \tab \eqn{\frac{X^\lambda - 1}{\lambda}} \tab \eqn{\lambda \ne 0} \cr
          \tab   \tab                                     \tab                     \cr
          \tab   \tab \eqn{log(X)}                        \tab \eqn{\lambda = 0 \;\;\;\;\;\; (1)}
  }
  where \eqn{Y} is assumed to come from a normal distribution.  This transformation is 
  continuous in \eqn{\lambda}.  Note that this transformation also preserves ordering.  
  See the help file for \code{\link{boxcoxTransform}} for more information on data 
  transformations.

  Box and Cox (1964) proposed choosing the appropriate value of \eqn{\lambda} based on 
  maximizing the likelihood function.  Alternatively, an appropriate value of 
  \eqn{\lambda} can be chosen based on another objective, such as maximizing the 
  probability plot correlation coefficient or the Shapiro-Wilk goodness-of-fit 
  statistic.

  Shumway et al. (1989) investigated extending the method of Box and Cox (1964) to 
  the case of Type I censored data, motivated by the desire to produce estimated 
  means and confidence intervals for air monitoring data that included censored 
  values.

  In the case when \code{optimize=TRUE}, the function \code{boxcoxCensored} calls the 
  \R function \code{\link{nlminb}} to minimize the negative value of the 
  objective (i.e., maximize the objective) over the range of possible values of 
  \eqn{\lambda} specified in the argument \code{lambda}.  The starting value for 
  the optimization is always \eqn{\lambda=1} (i.e., no transformation).

  The next section explains assumptions and notation, and the section after that 
  explains how the objective is computed for the various options for 
  \code{objective.name}. 
  \cr

  \bold{Assumptions and Notation} \cr
  Let \eqn{\underline{x}} denote a random sample of \eqn{N} observations from 
  some continuous distribution.  Assume \eqn{n} (\eqn{0 < n < N}) of these 
  observations are known and \eqn{c} (\eqn{c=N-n}) of these observations are 
  all censored below (left-censored) or all censored above (right-censored) at 
  \eqn{k} fixed censoring levels
  \deqn{T_1, T_2, \ldots, T_K; \; K \ge 1 \;\;\;\;\;\; (2)}
  For the case when \eqn{K \ge 2}, the data are said to be Type I 
  \bold{\emph{multiply censored}}.  For the case when \eqn{K=1}, 
  set \eqn{T = T_1}.  If the data are left-censored 
  and all \eqn{n} known observations are greater 
  than or equal to \eqn{T}, or if the data are right-censored and all \eqn{n} 
  known observations are less than or equal to \eqn{T}, then the data are 
  said to be Type I \bold{\emph{singly censored}} (Nelson, 1982, p.7), otherwise 
  they are considered to be Type I multiply censored.

  Let \eqn{c_j} denote the number of observations censored below or above censoring 
  level \eqn{T_j} for \eqn{j = 1, 2, \ldots, K}, so that
  \deqn{\sum_{i=1}^K c_j = c \;\;\;\;\;\; (3)}
  Let \eqn{x_{(1)}, x_{(2)}, \ldots, x_{(N)}} denote the \dQuote{ordered} observations, 
  where now \dQuote{observation} means either the actual observation (for uncensored 
  observations) or the censoring level (for censored observations).  For 
  right-censored data, if a censored observation has the same value as an 
  uncensored one, the uncensored observation should be placed first.  
  For left-censored data, if a censored observation has the same value as an 
  uncensored one, the censored observation should be placed first.

  Note that in this case the quantity \eqn{x_{(i)}} does not necessarily represent 
  the \eqn{i}'th \dQuote{largest} observation from the (unknown) complete sample.

  Finally, let \eqn{\Omega} (omega) denote the set of \eqn{n} subscripts in the 
  \dQuote{ordered} sample that correspond to uncensored observations, and let 
  \eqn{\Omega_j} denote the set of \eqn{c_j} subscripts in the \dQuote{ordered} 
  sample that correspond to the censored observations censored at censoring level 
  \eqn{T_j} for \eqn{j = 1, 2, \ldots, k}.

  We assume that there exists some value of \eqn{\lambda} such that the transformed 
  observations
  \tabular{llll}{
  \eqn{y_i} \tab = \tab \eqn{\frac{x_i^\lambda - 1}{\lambda}} \tab \eqn{\lambda \ne 0} \cr
            \tab   \tab                                       \tab                     \cr
            \tab   \tab \eqn{log(x_i)}                        \tab \eqn{\lambda = 0 \;\;\;\;\;\; (4)}
  }
  (\eqn{i = 1, 2, \ldots, n}) form a random sample of Type I censored data from a 
  normal distribution.

  Note that for the censored observations, Equation (4) becomes:
  \tabular{llll}{
  \eqn{y_{(i)} = T_j^*} \tab = \tab \eqn{\frac{T_j^\lambda - 1}{\lambda}} \tab \eqn{\lambda \ne 0} \cr
                        \tab   \tab                                       \tab                     \cr
                        \tab   \tab \eqn{log(T_j)}                        \tab \eqn{\lambda = 0 \;\;\;\;\;\; (5)}
  }  
  where \eqn{i \in \Omega_j}.
  \cr

  \bold{Computing the Objective} \cr

  \emph{Objective Based on Probability Plot Correlation Coefficient} (\code{objective.name="PPCC"}) \cr
  When \code{objective.name="PPCC"}, the objective is computed as the value of the 
  normal probability plot correlation coefficient based on the transformed data 
  (see the description of the Probability Plot Correlation Coefficient (PPCC) 
  goodness-of-fit test in the help file for \code{\link{gofTestCensored}}).  That is, 
  the objective is the correlation coefficient for the normal 
  \link[=qqPlotCensored]{quantile-quantile plot} for the transformed data.  
  Large values of the PPCC tend to indicate a good fit to a normal distribution.
  \cr
 
  \emph{Objective Based on Shapiro-Wilk Goodness-of-Fit Statistic} (\code{objective.name="Shapiro-Wilk"}) \cr
  When \code{objective.name="Shapiro-Wilk"}, the objective is computed as the value of 
  the Shapiro-Wilk goodness-of-fit statistic based on the transformed data 
  (see the description of the Shapiro-Wilk test in the help file for 
  \code{\link{gofTestCensored}}).  Large values of the Shapiro-Wilk statistic tend to 
  indicate a good fit to a normal distribution.
  \cr
 
  \emph{Objective Based on Log-Likelihood Function} (\code{objective.name="Log-Likelihood"}) \cr
  When \code{objective.name="Log-Likelihood"}, the objective is computed as the value 
  of the log-likelihood function.  Assuming the transformed observations in 
  Equation (4) above come from a normal distribution with mean \eqn{\mu} and 
  standard deviation \eqn{\sigma}, we can use the change of variable formula to 
  write the log-likelihood function as follows.

  For Type I left censored data, the likelihood function is given by:
  \deqn{log[L(\lambda, \mu, \sigma)] = log[{N \choose c_1 c_2 \ldots c_k n}] + \sum_{j=1}^k c_j log[F(T_j^*)] + \sum_{i \in \Omega} log\{f[y_{(i)}]\} + (\lambda - 1) \sum_{i \in \Omega} log[x_{(i)}] \;\;\;\;\;\; (6)}
  where \eqn{f} and \eqn{F} denote the probability density function (pdf) and 
  cumulative distribution function (cdf) of the population. That is,
  \deqn{f(t) = \phi(\frac{t-\mu}{\sigma}) \;\;\;\;\;\; (7)}
  \deqn{F(t) = \Phi(\frac{t-\mu}{\sigma}) \;\;\;\;\;\; (8)}
  where \eqn{\phi} and \eqn{\Phi} denote the pdf and cdf of the standard normal 
  distribution, respectively (Shumway et al., 1989).  For left singly 
  censored data, Equation (6) simplifies to:
  \deqn{log[L(\lambda, \mu, \sigma)] = log[{N \choose c}] + c log[F(T^*)] + \sum_{i = c+1}^N log\{f[y_{(i)}]\} + (\lambda - 1) \sum_{i = c+1}^N log[x_{(i)}] \;\;\;\;\;\; (9)}

  Similarly, for Type I right censored data, the likelihood function is given by:
  \deqn{log[L(\lambda, \mu, \sigma)] = log[{N \choose c_1 c_2 \ldots c_k n}] + \sum_{j=1}^k c_j log[1 - F(T_j^*)] + \sum_{i \in \Omega} log\{f[y_{(i)}]\} + (\lambda - 1) \sum_{i \in \Omega} log[x_{(i)}] \;\;\;\;\;\; (10)}
  and for right singly censored data this simplifies to:
  \deqn{log[L(\lambda, \mu, \sigma)] = log[{N \choose c}] + c log[1 - F(T^*)] + \sum_{i = 1}^n log\{f[y_{(i)}]\} + (\lambda - 1) \sum_{i = 1}^n log[x_{(i)}] \;\;\;\;\;\; (11)}

  For a fixed value of \eqn{\lambda}, the log-likelihood function 
  is maximized by replacing \eqn{\mu} and \eqn{\sigma} with their maximum likelihood 
  estimators (see the section \emph{Maximum Likelihood Estimation} in the help file 
  for \code{\link{enormCensored}}).  

  Thus, when \code{optimize=TRUE}, Equation (6) or (10) is maximized by iteratively 
  solving for \eqn{\lambda} using the MLEs for \eqn{\mu} and \eqn{\sigma}.  
  When \code{optimize=FALSE}, the value of the objective is computed by using 
  Equation (6) or (10), using the values of \eqn{\lambda} specified in the 
  argument \code{lambda}, and using the MLEs of \eqn{\mu} and \eqn{\sigma}.
}
\value{
  \code{boxcoxCensored} returns a list of class \code{"boxcoxCensored"} 
  containing the results.  
  See the help file for \code{\link{boxcoxCensored.object}} for details.
}
\references{
  Berthouex, P.M., and L.C. Brown. (2002). 
  \emph{Statistics for Environmental Engineers, Second Edition}. 
  Lewis Publishers, Boca Raton, FL.

  Box, G.E.P., and D.R. Cox. (1964).  An Analysis of Transformations 
  (with Discussion).  \emph{Journal of the Royal Statistical Society, Series B} 
  \bold{26}(2), 211--252.

  Cohen, A.C. (1991).  Truncated and Censored Samples.  \emph{Marcel Dekker}, 
  New York, New York, pp.50--59.

  Draper, N., and H. Smith. (1998). \emph{Applied Regression Analysis}. Third Edition. 
  John Wiley and Sons, New York, pp.47-53.

  Gilbert, R.O. (1987). \emph{Statistical Methods for Environmental Pollution 
  Monitoring}. Van Nostrand Reinhold, NY.

  Helsel, D.R., and R.M. Hirsch. (1992).  
  \emph{Statistical Methods in Water Resources Research}. 
  Elsevier, New York, NY.

  Hinkley, D.V., and G. Runger. (1984).  The Analysis of Transformed Data 
  (with Discussion).  \emph{Journal of the American Statistical Association} 
  \bold{79}, 302--320.

  Hoaglin, D.C., F.M. Mosteller, and J.W. Tukey, eds. (1983).  
  \emph{Understanding Robust and Exploratory Data Analysis}.  
  John Wiley and Sons, New York, Chapter 4.

  Hoaglin, D.C. (1988).  Transformations in Everyday Experience. 
  \emph{Chance} \bold{1}, 40--45.

  Johnson, N. L., S. Kotz, and A.W. Kemp. (1992).  \emph{Univariate 
  Discrete Distributions, Second Edition}.  John Wiley and Sons, New York, 
  p.163.

  Johnson, R.A., and D.W. Wichern. (2007).  
  \emph{Applied Multivariate Statistical Analysis, Sixth Edition}.  
  Pearson Prentice Hall, Upper Saddle River, NJ, pp.192--195.

  Shumway, R.H., A.S. Azari, and P. Johnson. (1989).  
  Estimating Mean Concentrations Under Transformations for Environmental 
  Data With Detection Limits.  \emph{Technometrics} \bold{31}(3), 347--356.

  Stoline, M.R. (1991).  An Examination of the Lognormal and Box and Cox 
  Family of Transformations in Fitting Environmental Data.  
  \emph{Environmetrics} \bold{2}(1), 85--106.

  van Belle, G., L.D. Fisher, Heagerty, P.J., and Lumley, T. (2004). 
  \emph{Biostatistics: A Methodology for the Health Sciences, 2nd Edition}. 
  John Wiley & Sons, New York.

  Zar, J.H. (2010). \emph{Biostatistical Analysis}. 
  Fifth Edition. Prentice-Hall, Upper Saddle River, NJ, 
  Chapter 13.
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  Data transformations are often used to induce normality, homoscedasticity, 
  and/or linearity, common assumptions of parametric statistical tests and 
  estimation procedures.  Transformations are not \dQuote{tricks} used by the 
  data analyst to hide what is going on, but rather useful tools for 
  understanding and dealing with data (Berthouex and Brown, 2002, p.61).  
  Hoaglin (1988) discusses \dQuote{hidden} transformations that are used everyday, 
  such as the pH scale for measuring acidity.  Johnson and Wichern (2007, p.192) 
  note that "Transformations are nothing more than a reexpression of the data 
  in different units."

  Shumway et al. (1989) investigated extending the method of Box and Cox (1964) to 
  the case of Type I censored data, motivated by the desire to produce estimated 
  means and confidence intervals for air monitoring data that included censored values.  

  Stoline (1991) compared the goodness-of-fit of Box-Cox transformed data (based on 
  using the \dQuote{optimal} power transformation from a finite set of values between 
  -1.5 and 1.5) with log-transformed data for 17 groundwater chemistry variables.  
  Using the Probability Plot Correlation Coefficient statistic for censored data as a 
  measure of goodness-of-fit (see \code{\link{gofTest}}), Stoline (1991) found that 
  only 6 of the variables were adequately modeled by a Box-Cox transformation 
  (p >0.10 for these 6 variables).  Of these variables, five were adequately modeled 
  by a a log transformation.  Ten of variables were \dQuote{marginally} fit by an 
  optimal Box-Cox transformation, and of these 10 only 6 were marginally fit by a 
  log transformation.  Based on these results, Stoline (1991) recommends checking 
  the assumption of lognormality before automatically assuming environmental data fit 
  a lognormal distribution.

  One problem with data transformations is that translating results on the 
  transformed scale back to the original scale is not always straightforward.  
  Estimating quantities such as means, variances, and confidence limits in the 
  transformed scale and then transforming them back to the original scale 
  usually leads to biased and inconsistent estimates (Gilbert, 1987, p.149; 
  van Belle et al., 2004, p.400).  For example, exponentiating the confidence 
  limits for a mean based on log-transformed data does not yield a 
  confidence interval for the mean on the original scale.  Instead, this yields 
  a confidence interval for the median (see the help file for 
  \code{\link{elnormAltCensored}}).  
  It should be noted, however, that quantiles (percentiles) and rank-based 
  procedures are invariant to monotonic transformations 
  (Helsel and Hirsch, 1992, p.12).

  Finally, there is no guarantee that a Box-Cox tranformation based on the 
  \dQuote{optimal} value of \eqn{\lambda} will provide an adequate transformation 
  to allow the assumption of approximate normality and constant variance.  Any 
  set of transformed data should be inspected relative to the assumptions you 
  want to make about it (Johnson and Wichern, 2007, p.194).
}
\seealso{
  \code{\link{boxcoxCensored.object}}, \code{\link{plot.boxcoxCensored}}, 
  \code{\link{print.boxcoxCensored}}, 
  \code{\link{boxcox}}, \link{Data Transformations}, \link{Goodness-of-Fit Tests}.
}
\examples{
  # Generate 15 observations from a lognormal distribution with 
  # mean=10 and cv=2 and censor the observations less than 2.  
  # Then generate 15 more observations from this distribution and 
  # censor the observations less than 4.  
  # Then Look at some values of various objectives for various transformations.  
  # Note that for both the PPCC objective the optimal value is about -0.3, 
  # whereas for the Log-Likelihood objective it is about 0.3.
  # (Note: the call to set.seed simply allows you to reproduce this example.)

  set.seed(250) 

  x.1 <- rlnormAlt(15, mean = 10, cv = 2) 
  censored.1 <- x.1 < 2
  x.1[censored.1] <- 2

  x.2 <- rlnormAlt(15, mean = 10, cv = 2) 
  censored.2 <- x.2 < 4
  x.2[censored.2] <- 4

  x <- c(x.1, x.2)
  censored <- c(censored.1, censored.2)

  #--------------------------
  # Using the PPCC objective:
  #--------------------------

  boxcoxCensored(x, censored) 

  #Results of Box-Cox Transformation
  #Based on Type I Censored Data
  #---------------------------------
  #
  #Objective Name:                  PPCC
  #
  #Data:                            x
  #
  #Censoring Variable:              censored
  #
  #Censoring Side:                  left
  #
  #Censoring Level(s):              2 4 
  #
  #Sample Size:                     30
  #
  #Percent Censored:                26.7%
  #
  # lambda      PPCC
  #   -2.0 0.8954683
  #   -1.5 0.9338467
  #   -1.0 0.9643680
  #   -0.5 0.9812969
  #    0.0 0.9776834
  #    0.5 0.9471025
  #    1.0 0.8901990
  #    1.5 0.8187488
  #    2.0 0.7480494


  boxcoxCensored(x, censored, optimize = TRUE)

  #Results of Box-Cox Transformation
  #Based on Type I Censored Data
  #---------------------------------
  #
  #Objective Name:                  PPCC
  #
  #Data:                            x
  #
  #Censoring Variable:              censored
  #
  #Censoring Side:                  left
  #
  #Censoring Level(s):              2 4 
  #
  #Sample Size:                     30
  #
  #Percent Censored:                26.7%
  #
  #Bounds for Optimization:         lower = -2
  #                                 upper =  2
  #
  #Optimal Value:                   lambda = -0.3194799
  #
  #Value of Objective:              PPCC = 0.9827546


  #-----------------------------------
  # Using the Log-Likelihodd objective
  #-----------------------------------

  boxcoxCensored(x, censored, objective.name = "Log-Likelihood") 

  #Results of Box-Cox Transformation
  #Based on Type I Censored Data
  #---------------------------------
  #
  #Objective Name:                  Log-Likelihood
  #
  #Data:                            x
  #
  #Censoring Variable:              censored
  #
  #Censoring Side:                  left
  #
  #Censoring Level(s):              2 4 
  #
  #Sample Size:                     30
  #
  #Percent Censored:                26.7%
  #
  # lambda Log-Likelihood
  #   -2.0      -95.38785
  #   -1.5      -84.76697
  #   -1.0      -75.36204
  #   -0.5      -68.12058
  #    0.0      -63.98902
  #    0.5      -63.56701
  #    1.0      -66.92599
  #    1.5      -73.61638
  #    2.0      -82.87970


  boxcoxCensored(x, censored, objective.name = "Log-Likelihood", 
    optimize = TRUE) 

  #Results of Box-Cox Transformation
  #Based on Type I Censored Data
  #---------------------------------
  #
  #Objective Name:                  Log-Likelihood
  #
  #Data:                            x
  #
  #Censoring Variable:              censored
  #
  #Censoring Side:                  left
  #
  #Censoring Level(s):              2 4 
  #
  #Sample Size:                     30
  #
  #Percent Censored:                26.7%
  #
  #Bounds for Optimization:         lower = -2
  #                                 upper =  2
  #
  #Optimal Value:                   lambda = 0.3049744
  #
  #Value of Objective:              Log-Likelihood = -63.2733

  #----------

  # Plot the results based on the PPCC objective
  #---------------------------------------------
  boxcox.list <- boxcoxCensored(x, censored)
  dev.new()
  plot(boxcox.list)

  #Look at QQ-Plots for the candidate values of lambda
  #---------------------------------------------------
  plot(boxcox.list, plot.type = "Q-Q Plots", same.window = FALSE) 

  #==========

  # Clean up
  #---------
  rm(x.1, censored.1, x.2, censored.2, x, censored, boxcox.list)
  graphics.off()
}
\keyword{ univar }
\keyword{ models }
