\name{boxcoxTransform}
\alias{boxcoxTransform}
\title{
  Apply a Box-Cox Power Transformation to a Set of Data
}
\description{
  Apply a Box-Cox power transformation to a set of data to attempt to induce 
  normality and homogeneity of variance.
}
\usage{
  boxcoxTransform(x, lambda, eps = .Machine$double.eps)
}
\arguments{
  \item{x}{
  a numeric vector of positive numbers.
}
  \item{lambda}{
  finite numeric scalar indicating what power to use for the 
  Box-Cox transformation.
}
  \item{eps}{
  finite, positive numeric scalar.  When the absolute value of \code{lambda} is less 
  than \code{eps}, lambda is assumed to be 0 for the Box-Cox transformation.  
  The default value is \code{eps=.Machine$double.eps}.
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
  variance, although the mean may differ between the two groups.  For standard 
  linear regression models, these assumptions can be stated as:  the error terms 
  all come from a normal distribution with mean 0 and and a constant variance.

  Often, especially with environmental data, the above assumptions do not hold 
  because the original data are skewed and/or they follow a distribution that is 
  not really shaped like a normal distribution.  It is sometimes possible, however, 
  to transform the original data so that the transformed observations in fact come 
  from a normal distribution or close to a normal distribution.  The transformation 
  may also induce homogeneity of variance and, for the case of a linear regression 
  model, a linear relationship between the response and predictor variable(s).

  Sometimes, theoretical considerations indicate an appropriate transformation.  
  For example, count data often follow a \link[stats:Poisson]{Poisson distribution}, 
  and it can be shown that taking the square root of observations from a Poisson 
  distribution tends to make these data look more bell-shaped 
  (Johnson et al., 1992, p.163; Johnson and Wichern, 2007, p.192; Zar, 2010, p.291).  
  A common example in the environmental field is that chemical concentration data 
  often appear to come from a \link[=LognormalAlt]{lognormal distribution} or some 
  other positively-skewed distribution (e.g., \link[=GammaAlt]{gamma}).  
  In this case, taking the logarithm of the observations often appears to yield 
  normally distributed data.

  Ideally, a data transformation is chosen based on knowledge of the process 
  generating the data, as well as graphical tools such as 
  \link[=qqPlot]{quantile-quantile plots} and histograms.

  Box and Cox (1964) presented a formalized method for deciding on a data 
  transformation.  Given a random variable \eqn{X} from some distribution with 
  only positive values, the Box-Cox family of power transformations is defined as:
  \tabular{llll}{
  \eqn{Y} \tab = \tab \eqn{\frac{X^\lambda - 1}{\lambda}} \tab \eqn{\lambda \ne 0} \cr
          \tab   \tab \eqn{log(X)}                        \tab \eqn{\lambda = 0 \;\;\;\;\;\; (1)}
  }
  where \eqn{Y} is assumed to come from a normal distribution.  This transformation is 
  continuous in \eqn{\lambda}.  Note that this transformation also preserves ordering;  
  that is, if \eqn{X_1 < X_2} then \eqn{Y_1 < Y_2}.  

  Box and Cox (1964) proposed choosing the appropriate value of \eqn{\lambda} 
  based on maximizing a likelihood function.  See the help file for 
  \code{\link{boxcox}} for details.

  Note that for non-zero values of \eqn{\lambda}, instead of using the formula of 
  Box and Cox in Equation (1), you may simply use the power transformation:
  \deqn{Y = X^\lambda \;\;\;\;\;\; (2)}
  since these two equations differ only by a scale difference and origin shift, 
  and the essential character of the transformed distribution remains unchanged.

  The value \eqn{\lambda=1} corresponds to no transformation.  Values of 
  \eqn{\lambda} less than 1 shrink large values of \eqn{X}, and are therefore 
  useful for transforming positively-skewed (right-skewed) data.  Values of 
  \eqn{\lambda} larger than 1 inflate large values of \eqn{X}, and are therefore 
  useful for transforming negatively-skewed (left-skewed) data 
  (Helsel and Hirsch, 1992, pp.13-14; Johnson and Wichern, 2007, p.193).  
  Commonly used values of \eqn{\lambda} include 0 (log transformation), 
  0.5 (square-root transformation), -1 (reciprocal), and -0.5 (reciprocal root).

  It is often recommend that when dealing with several similar data sets, it is best 
  to find a common transformation that works reasonably well for all the data sets, 
  rather than using slightly different transformations for each data set 
  (Helsel and Hirsch, 1992, p.14; Shumway et al., 1989).
}
\value{
  numeric vector of transformed observations.
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
  such as the pH scale for measuring acidity.

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
}
\seealso{
  \code{\link{boxcox}}, \link{Data Transformations}, \link{Goodness-of-Fit Tests}.
}
\examples{
  # Generate 30 observations from a lognormal distribution with 
  # mean=10 and cv=2, then look at some normal quantile-quantile 
  # plots for various transformations.
  # (Note: the call to set.seed simply allows you to reproduce this example.)

  set.seed(250) 
  x <- rlnormAlt(30, mean = 10, cv = 2)

  dev.new() 
  qqPlot(x, add.line = TRUE)

  dev.new()
  qqPlot(boxcoxTransform(x, lambda = 0.5), add.line = TRUE) 

  dev.new()
  qqPlot(boxcoxTransform(x, lambda = 0), add.line = TRUE) 
  

  # Clean up
  #---------
  rm(x)
}
\keyword{ univar }
\keyword{ models }

