\name{boxcox}
\alias{boxcox}
\alias{boxcox.default}
\alias{boxcox.lm}
\alias{Box-Cox}
\alias{BoxCox}
\alias{Box-Cox Transformation}
\alias{BoxCox Transformation}
\title{
  Boxcox Power Transformation
}
\description{
  \code{boxcox} is a generic function used to compute the value(s) of an objective 
  for one or more Box-Cox power transformations, or to compute an optimal power 
  transformation based on a specified objective.  The function invokes particular 
  \code{\link{methods}} which depend on the \code{\link{class}} of the first 
  argument. 

  Currently, there is a default method and a method for objects of class \code{"lm"}.
}
\usage{
boxcox(x, ...)

\method{boxcox}{default}(x, 
    lambda = {if (optimize) c(-2, 2) else seq(-2, 2, by = 0.5)}, 
    optimize = FALSE, objective.name = "PPCC", 
    eps = .Machine$double.eps, include.x = TRUE, ...)

\method{boxcox}{lm}(x, 
    lambda = {if (optimize) c(-2, 2) else seq(-2, 2, by = 0.5)}, 
    optimize = FALSE, objective.name = "PPCC", 
    eps = .Machine$double.eps, include.x = TRUE, ...) 
}
\arguments{
  \item{x}{
  an object of class \code{"lm"} for which the response variable is all positive 
  numbers, or else a numeric vector of positive numbers.  When \code{x} is an 
  object of class \code{"lm"}, the object must have been created with a 
  call to the function \code{\link{lm}} that includes the \code{data} argument.  
  When \code{x} is a numeric vector of positive observations, missing (\code{NA}), 
  undefined (\code{NaN}), and infinite (\code{-Inf, Inf}) values are allowed but 
  will be removed.
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
  \item{include.x}{
  logical scalar indicating whether to include the finite, non-missing values of 
  the argument \code{x} with the returned object. The default value is 
  \code{include.x=TRUE}.
}
  \item{\dots}{
  optional arguments for possible future methods.  Currently not used.
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
  are often used to attempt to satisfy these assumptions.  The rest of this section 
  is divided into two parts:  one that discusses Box-Cox transformations in the 
  context of the original observations, and one that  discusses Box-Cox 
  transformations in the context of linear models.
  \cr

  \bold{Box-Cox Transformations Based on the Original Observations} \cr
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

  Let \eqn{\underline{x} = x_1, x_2, \ldots, x_n} denote a random sample of 
  \eqn{n} observations from some distribution and assume that there exists some 
  value of \eqn{\lambda} such that the transformed observations
  \tabular{llll}{
  \eqn{y_i} \tab = \tab \eqn{\frac{x_i^\lambda - 1}{\lambda}} \tab \eqn{\lambda \ne 0} \cr
            \tab   \tab                                       \tab                     \cr
            \tab   \tab \eqn{log(x_i)}                        \tab \eqn{\lambda = 0 \;\;\;\;\;\; (2)}
  }
  (\eqn{i = 1, 2, \ldots, n}) form a random sample from a normal distribution.

  Box and Cox (1964) proposed choosing the appropriate value of \eqn{\lambda} based on 
  maximizing the likelihood function.  Alternatively, an appropriate value of 
  \eqn{\lambda} can be chosen based on another objective, such as maximizing the 
  probability plot correlation coefficient or the Shapiro-Wilk goodness-of-fit 
  statistic.

  In the case when \code{optimize=TRUE}, the function \code{boxcox} calls the 
  \R function \code{\link{nlminb}} to minimize the negative value of the 
  objective (i.e., maximize the objective) over the range of possible values of 
  \eqn{\lambda} specified in the argument \code{lambda}.  The starting value for 
  the optimization is always \eqn{\lambda=1} (i.e., no transformation).

  The rest of this sub-section explains how the objective is computed for the 
  various options for \code{objective.name}. 
  \cr
 
  \emph{Objective Based on Probability Plot Correlation Coefficient} (\code{objective.name="PPCC"}) \cr
  When \code{objective.name="PPCC"}, the objective is computed as the value of the 
  normal probability plot correlation coefficient based on the transformed data 
  (see the description of the Probability Plot Correlation Coefficient (PPCC) 
  goodness-of-fit test in the help file for \code{\link{gofTest}}).  That is, 
  the objective is the correlation coefficient for the normal 
  \link[=qqPlot]{quantile-quantile plot} for the transformed data.  
  Large values of the PPCC tend to indicate a good fit to a normal distribution.
  \cr
 
  \emph{Objective Based on Shapiro-Wilk Goodness-of-Fit Statistic} (\code{objective.name="Shapiro-Wilk"}) \cr
  When \code{objective.name="Shapiro-Wilk"}, the objective is computed as the value of 
  the Shapiro-Wilk goodness-of-fit statistic based on the transformed data 
  (see the description of the Shapiro-Wilk test in the help file for 
  \code{\link{gofTest}}).  Large values of the Shapiro-Wilk statistic tend to 
  indicate a good fit to a normal distribution.
  \cr
 
  \emph{Objective Based on Log-Likelihood Function} (\code{objective.name="Log-Likelihood"}) \cr
  When \code{objective.name="Log-Likelihood"}, the objective is computed as the value 
  of the log-likelihood function.  Assuming the transformed observations in 
  Equation (2) above come from a normal distribution with mean \eqn{\mu} and 
  standard deviation \eqn{\sigma}, we can use the change of variable formula to 
  write the log-likelihood function as:
  \deqn{log[L(\lambda, \mu, \sigma)] = \frac{-n}{2}log(2\pi) - \frac{n}{2}log(\sigma^2) - \frac{1}{2\sigma^2} \sum_{i=1}^n (y_i - \mu)^2 + (\lambda - 1) \sum_{i=1}^n log(x_i) \;\;\;\;\;\; (3)}
  where \eqn{y_i} is defined in Equation (2) above (Box and Cox, 1964). 
  For a fixed value of \eqn{\lambda}, the log-likelihood function 
  is maximized by replacing \eqn{\mu} and \eqn{\sigma} with their maximum likelihood 
  estimators:
  \deqn{\hat{\mu} = \frac{1}{n} \sum_{i=1}^n y_i \;\;\;\;\;\; (4)}
  \deqn{\hat{\sigma} = [\frac{1}{n} \sum_{i=1}^n (y_i - \bar{y})^2]^{1/2} \;\;\;\;\;\; (5)}
  Thus, when \code{optimize=TRUE}, Equation (3) is maximized by iteratively solving for 
  \eqn{\lambda} using the values for \eqn{\mu} and \eqn{\sigma} given in 
  Equations (4) and (5).  When \code{optimize=FALSE}, the value of the objective is 
  computed by using Equation (3), using the values of \eqn{\lambda} specified in the 
  argument \code{lambda}, and using the values for \eqn{\mu} and \eqn{\sigma} given 
  in Equations (4) and (5).
  \cr

  \bold{Box-Cox Transformation for Linear Models} \cr
  In the case of a standard linear regression model with \eqn{n} observations and 
  \eqn{p} predictors:
  \deqn{Y_i = \beta_0 + \beta_1 X_{i1} + \ldots + \beta_p X_{ip} + \epsilon_i, \; i=1,2,\ldots,n \;\;\;\;\;\; (6)}
  the standard assumptions are:
  \enumerate{
  \item The error terms \eqn{\epsilon_i} come from a normal distribution with mean 0.
  \item The variance is the same for all of the error terms and does not depend on 
        the predictor variables.
  }
  Assuming \eqn{Y} is a random variable from some distribution that may depend on 
  the predictor variables and \eqn{Y} takes on only positive values, the Box-Cox 
  family of power transformations is defined as:
  \tabular{llll}{
  \eqn{Y^*} \tab = \tab \eqn{\frac{Y^\lambda - 1}{\lambda}} \tab \eqn{\lambda \ne 0} \cr
            \tab   \tab                                     \tab                     \cr
            \tab   \tab \eqn{log(Y)}                        \tab \eqn{\lambda = 0 \;\;\;\;\;\; (7)}
  }
  where \eqn{Y^*} becomes the new response variable and the errors are now 
  assumed to come from a normal distribution with a mean of 0 and a constant variance.

  In this case, the objective is computed as described above, but it is based on the 
  residuals from the fitted linear model in which the response variable is now 
  \eqn{Y^*} instead of \eqn{Y}.
}
\value{
  When \code{x} is an object of class \code{"lm"}, \code{boxcox} returns 
  a list of class \code{"boxcoxLm"} containing the results.  See 
  the help file for \code{\link{boxcoxLm.object}} for details.

  When \code{x} is simply a numeric vector of positive numbers, 
  \code{boxcox} returns a list of class \code{"boxcox"} containing the 
  results.  See the help file for \code{\link{boxcox.object}} for details.
}
\references{
  Berthouex, P.M., and L.C. Brown. (2002). 
  \emph{Statistics for Environmental Engineers, Second Edition}. 
  Lewis Publishers, Boca Raton, FL.

  Box, G.E.P., and D.R. Cox. (1964).  An Analysis of Transformations 
  (with Discussion).  \emph{Journal of the Royal Statistical Society, Series B} 
  \bold{26}(2), 211--252.

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

  In the case of a linear model, there are at least two approaches to improving 
  a model fit:  transform the \eqn{Y} and/or \eqn{X} variable(s), and/or use 
  more predictor variables.  Often in environmental data analysis, we assume the 
  observations come from a lognormal distribution and automatically take 
  logarithms of the data.  For a simple linear regression 
  (i.e., one predictor variable), if regression diagnostic plots indicate that a 
  straight line fit is not adequate, but that the variance of the errors 
  appears to be fairly constant, you may only need to transform the predictor 
  variable \eqn{X} or perhaps use a quadratic or cubic model in \eqn{X}.  
  On the other hand, if the diagnostic plots indicate that the constant 
  variance and/or normality assumptions are suspect, you probably need to consider 
  transforming the response variable \eqn{Y}.  Data transformations for 
  linear regression models are discussed in Draper and Smith (1998, Chapter 13) 
  and Helsel and Hirsch (1992, pp. 228-229).

  One problem with data transformations is that translating results on the 
  transformed scale back to the original scale is not always straightforward.  
  Estimating quantities such as means, variances, and confidence limits in the 
  transformed scale and then transforming them back to the original scale 
  usually leads to biased and inconsistent estimates (Gilbert, 1987, p.149; 
  van Belle et al., 2004, p.400).  For example, exponentiating the confidence 
  limits for a mean based on log-transformed data does not yield a 
  confidence interval for the mean on the original scale.  Instead, this yields 
  a confidence interval for the median (see the help file for \code{\link{elnormAlt}}).  
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
  \code{\link{boxcox.object}}, \code{\link{plot.boxcox}}, \code{\link{print.boxcox}}, 
  \code{\link{boxcoxLm.object}}, \code{\link{plot.boxcoxLm}}, \code{\link{print.boxcoxLm}}, 
  \code{\link{boxcoxTransform}}, \link{Data Transformations}, 
  \link{Goodness-of-Fit Tests}.
}
\examples{
  # Generate 30 observations from a lognormal distribution with 
  # mean=10 and cv=2.  Look at some values of various objectives 
  # for various transformations.  Note that for both the PPCC and 
  # the Log-Likelihood objective, the optimal value of lambda is 
  # about 0, indicating that a log transformation is appropriate.  
  # (Note: the call to set.seed simply allows you to reproduce this example.)

  set.seed(250) 
  x <- rlnormAlt(30, mean = 10, cv = 2) 

  dev.new()
  hist(x, col = "cyan")

  # Using the PPCC objective:
  #--------------------------

  boxcox(x) 
  #Results of Box-Cox Transformation
  #---------------------------------
  #
  #Objective Name:                  PPCC
  #
  #Data:                            x
  #
  #Sample Size:                     30
  #
  # lambda      PPCC
  #   -2.0 0.5423739
  #   -1.5 0.6402782
  #   -1.0 0.7818160
  #   -0.5 0.9272219
  #    0.0 0.9921702
  #    0.5 0.9581178
  #    1.0 0.8749611
  #    1.5 0.7827009
  #    2.0 0.7004547

  boxcox(x, optimize = TRUE)
  #Results of Box-Cox Transformation
  #---------------------------------
  #
  #Objective Name:                  PPCC
  #
  #Data:                            x
  #
  #Sample Size:                     30
  #
  #Bounds for Optimization:         lower = -2
  #                                 upper =  2
  #
  #Optimal Value:                   lambda = 0.04530789
  #
  #Value of Objective:              PPCC = 0.9925919


  # Using the Log-Likelihodd objective
  #-----------------------------------

  boxcox(x, objective.name = "Log-Likelihood") 
  #Results of Box-Cox Transformation
  #---------------------------------
  #
  #Objective Name:                  Log-Likelihood
  #
  #Data:                            x
  #
  #Sample Size:                     30
  #
  # lambda Log-Likelihood
  #   -2.0     -154.94255
  #   -1.5     -128.59988
  #   -1.0     -106.23882
  #   -0.5      -90.84800
  #    0.0      -85.10204
  #    0.5      -88.69825
  #    1.0      -99.42630
  #    1.5     -115.23701
  #    2.0     -134.54125

  boxcox(x, objective.name = "Log-Likelihood", optimize = TRUE) 
  #Results of Box-Cox Transformation
  #---------------------------------
  #
  #Objective Name:                  Log-Likelihood
  #
  #Data:                            x
  #
  #Sample Size:                     30
  #
  #Bounds for Optimization:         lower = -2
  #                                 upper =  2
  #
  #Optimal Value:                   lambda = 0.0405156
  #
  #Value of Objective:              Log-Likelihood = -85.07123

  #----------

  # Plot the results based on the PPCC objective
  #---------------------------------------------
  boxcox.list <- boxcox(x)
  dev.new()
  plot(boxcox.list)

  #Look at QQ-Plots for the candidate values of lambda
  #---------------------------------------------------
  plot(boxcox.list, plot.type = "Q-Q Plots", same.window = FALSE) 

  #==========

  # The data frame Environmental.df contains daily measurements of 
  # ozone concentration, wind speed, temperature, and solar radiation
  # in New York City for 153 consecutive days between May 1 and 
  # September 30, 1973.  In this example, we'll plot ozone vs. 
  # temperature and look at the Q-Q plot of the residuals.  Then 
  # we'll look at possible Box-Cox transformations.  The "optimal" one 
  # based on the PPCC looks close to a log-transformation 
  # (i.e., lambda=0).  The power that produces the largest PPCC is 
  # about 0.2, so a cube root (lambda=1/3) transformation might work too.

  head(Environmental.df)
  #           ozone radiation temperature wind
  #05/01/1973    41       190          67  7.4
  #05/02/1973    36       118          72  8.0
  #05/03/1973    12       149          74 12.6
  #05/04/1973    18       313          62 11.5
  #05/05/1973    NA        NA          56 14.3
  #05/06/1973    28        NA          66 14.9

  tail(Environmental.df)
  #           ozone radiation temperature wind
  #09/25/1973    14        20          63 16.6
  #09/26/1973    30       193          70  6.9
  #09/27/1973    NA       145          77 13.2
  #09/28/1973    14       191          75 14.3
  #09/29/1973    18       131          76  8.0
  #09/30/1973    20       223          68 11.5


  # Fit the model with the raw Ozone data
  #--------------------------------------
  ozone.fit <- lm(ozone ~ temperature, data = Environmental.df) 

  # Plot Ozone vs. Temperature, with fitted line 
  #---------------------------------------------
  dev.new()
  with(Environmental.df, 
    plot(temperature, ozone, xlab = "Temperature (degrees F)",
      ylab = "Ozone (ppb)", main = "Ozone vs. Temperature"))
  abline(ozone.fit) 

  # Look at the Q-Q Plot for the residuals 
  #---------------------------------------
  dev.new()
  qqPlot(ozone.fit$residuals, add.line = TRUE) 

  # Look at Box-Cox transformations of Ozone 
  #-----------------------------------------
  boxcox.list <- boxcox(ozone.fit) 
  boxcox.list 
  #Results of Box-Cox Transformation
  #---------------------------------
  #
  #Objective Name:                  PPCC
  #
  #Linear Model:                    ozone.fit
  #
  #Sample Size:                     116
  #
  # lambda      PPCC
  #   -2.0 0.4286781
  #   -1.5 0.4673544
  #   -1.0 0.5896132
  #   -0.5 0.8301458
  #    0.0 0.9871519
  #    0.5 0.9819825
  #    1.0 0.9408694
  #    1.5 0.8840770
  #    2.0 0.8213675

  # Plot PPCC vs. lambda based on Q-Q plots of residuals 
  #-----------------------------------------------------
  dev.new()
  plot(boxcox.list) 

  # Look at Q-Q plots of residuals for the various transformation 
  #--------------------------------------------------------------
  plot(boxcox.list, plot.type = "Q-Q Plots", same.window = FALSE)

  # Compute the "optimal" transformation
  #-------------------------------------
  boxcox(ozone.fit, optimize = TRUE)
  #Results of Box-Cox Transformation
  #---------------------------------
  #
  #Objective Name:                  PPCC
  #
  #Linear Model:                    ozone.fit
  #
  #Sample Size:                     116
  #
  #Bounds for Optimization:         lower = -2
  #                                 upper =  2
  #
  #Optimal Value:                   lambda = 0.2004305
  #
  #Value of Objective:              PPCC = 0.9940222

  #==========

  # Clean up
  #---------
  rm(x, boxcox.list, ozone.fit)
  graphics.off()
}
\keyword{ univar }
\keyword{ models }
