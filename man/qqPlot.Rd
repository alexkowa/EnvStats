\name{qqPlot}
\alias{qqPlot}
\title{
  Quantile-Quantile (Q-Q) Plot
}
\description{
  Produces a quantile-quantile (Q-Q) plot, also called a probability plot.  
  The \code{qqPlot} function is a modified version of the \R functions 
  \code{\link{qqnorm}} and \code{\link{qqplot}}.  
  The \pkg{EnvStats} function \code{qqPlot} allows the user to specify a number of 
  different distributions in addition to the normal distribution, and to optionally 
  estimate the distribution parameters of the fitted distribution.
}
\usage{
  qqPlot(x, y = NULL, distribution = "norm", param.list = list(mean = 0, sd = 1), 
    estimate.params = plot.type == "Tukey Mean-Difference Q-Q", 
    est.arg.list = NULL, plot.type = "Q-Q", plot.pos.con = NULL, plot.it = TRUE, 
    equal.axes = qq.line.type == "0-1" || estimate.params, add.line = FALSE, 
    qq.line.type = "least squares", duplicate.points.method = "standard", 
    points.col = 1, line.col = 1, line.lwd = par("cex"), line.lty = 1, 
    digits = .Options$digits, ..., main = NULL, xlab = NULL, ylab = NULL, 
    xlim = NULL, ylim = NULL)
}
\arguments{
  \item{x}{
  numeric vector of observations.  When \code{y} is not supplied, \code{x} represents a sample 
  from the hypothesized distribution specifed by \code{distribution}.  When \code{y} is supplied, 
  the distribution of \code{x} is compared with the distribuiton of \code{y}.  
  Missing (\code{NA}), undefined (\code{NaN}), and 
  infinite (\code{Inf}, \code{-Inf}) values are allowed but will be removed.
}
  \item{y}{
  optional numeric vector of observations (not necessarily the same lenght as \code{x}).  
  Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) 
  values are allowed but will be removed.
}
  \item{distribution}{
  when \code{y} is not supplied, 
  a character string denoting the distribution abbreviation.  The default value is 
  \code{distribution="norm"}.  See the help file for \cr
  \code{\link{Distribution.df}} for a 
  list of possible distribution abbreviations.  This argument is ignored if \code{y} 
  is supplied.
}
  \item{param.list}{
  when \code{y} is not supplied, 
  a list with values for the parameters of the distribution.  The default value is 
  \code{param.list=list(mean=0, sd=1)}.  See the help file for \code{\link{Distribution.df}} 
  for the names and possible values of the parameters associated with each distribution.  
  This argument is ignored if \code{y} is supplied or \code{estimate.params=TRUE}.
}
  \item{estimate.params}{
  when \code{y} is not supplied, 
  a logical scalar indicating whether to compute quantiles based on estimating the 
  distribution parameters (\code{estimate.params=TRUE}) or using the known 
  distribution parameters specified in \code{param.list} \cr
  (\code{estimate.params=FALSE}).  The default value of \code{estimate.params} 
  is \code{FALSE} if \code{plot.type="Q-Q"} because the default configuration is a standard normal 
  (mean=0, sd=1) Q-Q plot, which will yield roughly a straight line if the observations in 
  \code{x} are from any normal distribution.  The default value of \code{estimate.params} 
  is \code{TRUE} if \code{plot.type="Tukey Mean-Difference Q-Q"}.  The argument \cr
  \code{estimate.params} 
  is ignored if \code{y} is supplied.
}
  \item{est.arg.list}{
  when \code{y} is not supplied and \code{estimate.params=TRUE}, 
  a list whose components are optional arguments associated with the function used to estimate 
  the parameters of the assumed distribution (see the help file 
  \link[=FcnsByCatEstDistParams]{Estimating Distribution Parameters}).  
  For example, all functions used to estimate distribution parameters have an optional argument 
  called \code{method} that specifies the method to use to estimate the parameters.  
  (See the help file for \code{\link{Distribution.df}} for a list of available estimation 
  methods for each distribution.)  To override the default estimation method, supply the argument 
  \code{est.arg.list} with a component called \code{method}; for example 
  \code{est.arg.list=list(method="mle")}.  The default value is 
  \code{est.arg.list=NULL} so that all default values for the estimating function are used.  
  This argument is ignored if \code{estimate.params=FALSE} or \code{y} is supplied.
}
  \item{plot.type}{
  a character string denoting the kind of plot.  Possible values are \code{"Q-Q"} 
  (Quantile-Quantile plot, the default) and \code{"Tukey Mean-Difference Q-Q"} 
  (Tukey mean-difference Q-Q plot).  This argument may be abbreviated (e.g., 
  \code{plot.type="T"} to indicate a Tukey mean-difference Q-Q plot).
}
  \item{plot.pos.con}{
  numeric scalar between 0 and 1 containing the value of the plotting position constant.  
  The default value of \code{plot.pos.con} depends on whether the argument \code{y} is supplied, 
  and if not the value of the argument \code{distribution}.  When \code{y} is supplied, the default 
  value is \code{plot.pos.con=0.5}, corresponding to Hazen plotting positions.  When \code{y} is 
  not supplied, for the normal, lognormal, three-parameter lognormal, zero-modified normal, and 
  zero-modified lognormal distributions, the default value is \code{plot.pos.con=0.375}.  
  For the Type I extreme value (Gumbel) distribution (\code{distribution="evd"}), 
  the default value is \cr
  \code{plot.pos.con=0.44}.  For all other distributions, the default value is \cr 
  \code{plot.pos.con=0.4}. 
}
  \item{plot.it}{
  a logical scalar indicating whether to create a plot on the current graphics device.   
  The default value is \code{plot.it=TRUE}.
}
  \item{equal.axes}{
  a logical scalar indicating whether to use the same range on the \eqn{x}- and \eqn{y}-axes 
  when \code{plot.type="Q-Q"}.  The default value is \code{TRUE} if \code{qq.line.type="0-1"} or 
  \code{estimate.params=TRUE}, otherwise it is \code{FALSE}.  This argument is ignored if 
  \code{plot.type="Tukey Mean-Difference Q-Q"}.
}
  \item{add.line}{
  a logical scalar indicating whether to add a line to the plot.  If \code{add.line=TRUE} and 
  \code{plot.type="Q-Q"}, a line determined by the value of \code{qq.line.type} is added to the plot.  
  If \code{add.line=TRUE} and \cr
  \code{plot.type="Tukey Mean-Difference Q-Q"}, a horizontal line at 
  \eqn{y=0} is added to the plot.  The default value is \code{add.line=FALSE}.
}
  \item{qq.line.type}{
  character string determining what kind of line to add to the Q-Q plot.  Possible values are 
  \code{"least squares"} (the default), \code{"0-1"} and \code{"robust"}.  For the value 
  \code{"least squares"}, a least squares line is fit and added.  For the value \code{"0-1"}, 
  a line with intercept 0 and slope 1 is added.  For the value \code{"robust"}, a line is fit through 
  the first and third quartiles of the \code{x} and \code{y} data.  This argument is ignored if 
  \code{add.line=FALSE} or \code{plot.type="Tukey Mean-Difference Q-Q"}.
}
  \item{duplicate.points.method}{
  a character string denoting how to plot points with duplicate \eqn{(x,y)} values.  Possible values 
  are \code{"standard"} (the default), \code{"jitter"}, and \code{"number"}.  For the value 
  \code{"standard"}, a single plotting symbol is plotted (this is the default behavior of \R).  
  For the value \code{"jitter"}, a separate plotting symbol is plotted for each duplicate point, where 
  the plotting symbols cluster around the true value of \eqn{x} and \eqn{y}.  For the value 
  \code{"number"}, a single number is plotted at \eqn{(x,y)} that represents how many duplicate points 
  are at that \eqn{(x,y)} coordinate.
}
  \item{points.col}{
  a numeric scalar or character string determining the color of the points in the plot.  
  The default value is \code{points.col=1}.  See the entry for \code{col} in the help file for 
  \code{\link{par}} for more information.
}
  \item{line.col}{
  a numeric scalar or character string determining the color of the line in the plot.  
  The default value is \code{points.col=1}.  See the entry for \code{col} in the help file for 
  \code{\link{par}} for more information.  This argument is ignored if \code{add.line=FALSE}.
}
  \item{line.lwd}{
  a numeric scalar determining the width of the line in the plot.  The default value is 
  \code{line.lwd=par("cex")}.  See the entry for \code{lwd} in the help file for \code{\link{par}} 
  for more information.  This argument is ignored if \code{add.line=FALSE}.
}
  \item{line.lty}{
  a numeric scalar determining the line type of the line in the plot.  The default value is 
  \code{line.lty=1}.  See the entry for \code{lty} in the help file for \code{\link{par}} 
  for more information.  This argument is ignored if \code{add.line=FALSE}.
}
  \item{digits}{
  a scalar indicating how many significant digits to print for the distribution parameters.  
  The default value is \code{digits=.Options$digits}.
}
  \item{main, xlab, ylab, xlim, ylim, \dots}{
  additional graphical parameters (see \code{\link{par}}).
}
}
\details{
  If \code{y} is not supplied, the vector \code{x} is assumed to be a sample from the probability 
  distribution specified by the argument \code{distribution} (and \code{param.list} if 
  \code{estimate.params=FALSE}).  When \code{plot.type="Q-Q"}, the quantiles of \code{x} are 
  plotted on the \eqn{y}-axis against the quantiles of the assumed distribution on the \eqn{x}-axis.  

  If \code{y} is supplied and \code{plot.type="Q-Q"}, the empirical quantiles of \code{y} are 
  plotted against the empirical quantiles of \code{x}.  

  When \code{plot.type="Tukey Mean-Difference Q-Q"}, the difference of the quantiles is plotted on 
  the \eqn{y}-axis against the mean of the quantiles on the \eqn{x}-axis. 

  \bold{Special Distributions} \cr
  When \code{y} is not supplied and the argument \code{distribution} specifies one of the 
  following distributions, the function \code{qqPlot} behaves in the manner described below.

  \describe{
    \item{\code{"lnorm"}}{\emph{Lognormal Distribution}.  The log-transformed quantiles are 
      plotted against quantiles from a Normal (Gaussian) distribution.}
    \item{\code{"lnormAlt"}}{\emph{Lognormal Distribution (alternative parameterization).}  
      The untransformed quantiles are plotted against quantiles from a 
      Lognormal distribution.}
    \item{\code{"lnorm3"}}{\emph{Three-Parameter Lognormal Distribution}.  The quantiles of 
      \code{log(x-threshold)} are plotted against quantiles from a Normal (Gaussian) distribution.  
      The value of \code{threshold} is either specified in the argument \code{param.list}, or, 
      if \code{estimate.params=TRUE}, then it is estimated.}
    \item{\code{"zmnorm"}}{\emph{Zero-Modified Normal Distribution}.  The quantiles of the 
      non-zero values (i.e., \code{x[x!=0]}) are plotted against quantiles from a Normal 
      (Gaussian) distribution.}
    \item{\code{"zmlnorm"}}{\emph{Zero-Modified Lognormal Distribution}.  The quantiles of the 
      log-transformed positive values (i.e., \code{log(x[x>0])}) are plotted against quantiles 
      from a Normal (Gaussian) distribution.}
    \item{\code{"zmlnormAlt"}}{\emph{Lognormal Distribution (alternative parameterization).}  
      The quantiles of the untransformed positive values (i.e., \code{x[x>0]}) are 
      plotted against quantiles from a Lognormal distribution.}
  }

  \bold{Explanation of Q-Q Plots} \cr
  A \bold{\emph{probability plot}} or \bold{\emph{quantile-quantile (Q-Q) plot}} 
  is a graphical display invented by Wilk and Gnanadesikan (1968) to compare a 
  data set to a particular probability distribution or to compare it to another 
  data set. The idea is that if two population distributions are exactly the same, 
  then they have the same quantiles (percentiles), so a plot of the quantiles for 
  the first distribution vs. the quantiles for the second distribution will fall 
  on the 0-1 line (i.e., the straight line \eqn{y = x} with intercept 0 and slope 1).  
  If the two distributions have the same shape and spread but different locations, 
  then the plot of the quantiles will fall on the line \eqn{y = x + b} 
  (parallel to the 0-1 line) where \eqn{b} denotes the difference in locations.  
  If the distributions have different locations and differ by a multiplicative 
  constant \eqn{m}, then the plot of the quantiles will fall on the line 
  \eqn{y = mx + b} (D'Agostino, 1986a, p. 25; Helsel and Hirsch, 1986, p. 42).  
  Various kinds of differences between distributions will yield various kinds of 
  deviations from a straight line.
  \cr

  \emph{Comparing Observations to a Hypothesized Distribution} \cr
  Let \eqn{\underline{x} = x_1, x_2, \ldots, x_n} denote the observations 
  in a random sample of size \eqn{n} from some unknown distribution with 
  cumulative distribution function \eqn{F()}, and let 
  \eqn{x_{(1)}, x_{(2)}, \ldots, x_{(n)}} denote the ordered observations.   
  Depending on the particular formula used for the empirical cdf 
  (see \code{\link{ecdfPlot}}), the \eqn{i}'th order statistic is an 
  estimate of the \eqn{i/(n+1)}'th, \eqn{(i-0.5)/n}'th, etc., quantile.  
  For the moment, assume the \eqn{i}'th order statistic is an estimate of the 
  \eqn{i/(n+1)}'th quantile, that is:
  \deqn{\hat{F}[x_{(i)}] = \hat{p}_i = \frac{i}{n+1} \;\;\;\;\;\; (1)}
  so
  \deqn{x_{(i)} \approx F^{-1}(\hat{p}_i) \;\;\;\;\;\; (2)}
  If we knew the form of the true cdf \eqn{F}, then the plot of 
  \eqn{x_{(i)}} vs. \eqn{F^{-1}(\hat{p}_i)} would form approximately 
  a straight line based on Equation (2) above.  A probability plot is a plot of
  \eqn{x_{(i)}} vs. \eqn{F_0^{-1}(\hat{p}_i)}, where \eqn{F_0} denotes the 
  cdf associated with the hypothesized distribution.  The probability plot 
  should fall roughly on the line \eqn{y=x} if \eqn{F=F_0}.  If \eqn{F} and \eqn{F_0} 
  merely differ by a shift in location and scale, that is, if 
  \eqn{F[(x - \mu) / \sigma] = F_0(x)}, then the plot should fall roughly on the 
  line \eqn{y = \sigma x + \mu}.

  The quantity \eqn{\hat{p}_i = i/(n+1)} in Equation (1) above is called the 
  \bold{\emph{plotting position}} for the probability plot.  This particular 
  formula for the plotting position is appealing because it can be shown that 
  for any continuous distribution
  \deqn{E\{F[x_{(i)}]\} = \frac{i}{n+1} \;\;\;\;\;\; (3)}
  (Nelson, 1982, pp. 299-300; Stedinger et al., 1993).  That is, the \eqn{i}'th 
  plotting position defined as in Equation (1) is the expected value of the true 
  cdf evaluated at the \eqn{i}'th order statistic.  Many authors and practitioners, 
  however, prefer to use a plotting position that satisfies:
  \deqn{F^{-1}(\hat{p}_i) = E[x_{(i)}] \;\;\;\;\;\; (4)}
  or one that satisfies
  \deqn{F^{-1}(\hat{p}_i) = M[x_{(i)}] = F^{-1}\{M[u_{(i)}]\} \;\;\;\;\;\; (5)}
  where \eqn{M[x_{(i)}]} denotes the median of the distribution of the \eqn{i}'th 
  order statistic, and \eqn{u_{(i)}} denotes the \eqn{i}'th order statistic in a 
  random sample of \eqn{n} \link[stats:Uniform]{uniform (0,1)} random variates.

  The plotting positions in Equation (4) are often approximated since the expected 
  value of the \eqn{i}'th order statistic is often difficult and time-consuming 
  to compute.  Note that these plotting positions will differ for different 
  distributions.

  The plotting positions in Equation (5) were recommended by Filliben (1975) because 
  they require computing or approximating only the medians of 
  \link[stats:Uniform]{uniform (0,1)} order statistics, no matter what the form 
  of the assumed cdf \eqn{F_0}.  Also, the median may be preferred as a measure of 
  central tendency because the distributions of most order statistics are skewed.

  Most plotting positions can be written as:
  \deqn{\hat{p}_i = \frac{i - a}{n - 2a + 1} \;\;\;\;\;\; (6)}
  where \eqn{0 \le a \le 1} (D'Agostino, 1986a, p.25; Stedinger et al., 1993).  
  The quantity \eqn{a} is sometimes called the \dQuote{plotting position constant}, and 
  is determined by the argument \code{plot.pos.con} in the function \code{qqPlot}.  
  The table below, adapted from Stedinger et al. (1993), displays commonly used 
  plotting positions based on equation (6) for several distributions.
  \tabular{llll}{
              \tab                 \tab \bold{Distribution} \tab                     \cr
              \tab                 \tab \bold{Often Used}   \tab                     \cr
  \bold{Name} \tab \bold{\emph{a}} \tab \bold{With}         \tab \bold{References}   \cr
  Weibull     \tab 0               \tab Weibull,            \tab Weibull (1939),     \cr
              \tab                 \tab Uniform             \tab Stedinger et al. (1993) \cr
  Median      \tab 0.3175          \tab Several             \tab Filliben (1975), \cr
              \tab                 \tab                     \tab Vogel (1986) \cr
  Blom        \tab 0.375           \tab Normal              \tab Blom (1958), \cr
              \tab                 \tab and Others          \tab Looney and Gulledge (1985) \cr
  Cunnane     \tab 0.4             \tab Several             \tab Cunnane (1978), \cr
              \tab                 \tab                     \tab Chowdhury et al. (1991) \cr
  Gringorten  \tab 0.44            \tab Gumbel              \tab Gringorton (1963), \cr
              \tab                 \tab                     \tab Vogel (1986) \cr
  Hazen       \tab 0.5             \tab Several             \tab Hazen (1914), \cr
              \tab                 \tab                     \tab Chambers et al. (1983), \cr
              \tab                 \tab                     \tab Cleveland (1993) 
  }
  For moderate and large sample sizes, there is very little difference in 
  visual appearance of the Q-Q plot for different choices of plotting positions.
  \cr

  \emph{Comparing Two Data Sets} \cr
  Let \eqn{\underline{x} = x_1, x_2, \ldots, x_n} denote the observations 
  in a random sample of size \eqn{n} from some unknown distribution with 
  cumulative distribution function \eqn{F()}, and let 
  \eqn{x_{(1)}, x_{(2)}, \ldots, x_{(n)}} denote the ordered observations.  Similarly, 
  let \eqn{\underline{y} = y_1, y_2, \ldots, y_m} denote the observations 
  in a random sample of size \eqn{m} from some unknown distribution with 
  cumulative distribution function \eqn{G()}, and let 
  \eqn{y_{(1)}, y_{(2)}, \ldots, y_{(m)}} denote the ordered observations.
  Suppose we are interested in investigating whether the shape of the distribution 
  with cdf \eqn{F} is the same as the shape of the distribution with cdf \eqn{G} 
  (e.g., \eqn{F} and \eqn{G} may both be normal distributions but differ in mean 
  and standard deviation).

  When \eqn{n = m}, we can visually explore this question by plotting 
  \eqn{y_{(i)}} vs. \eqn{x_{(i)}}, for \eqn{i = 1, 2, \ldots, n}.  
  The values in \eqn{\underline{y}} are spread out in a certain way depending 
  on the true distribution:  they may be more or less symmetric about some value 
  (the population mean or median) or they may be skewed to the right or left; 
  they may be concentrated close to the mean or median (platykurtic) or there may 
  be several observations \dQuote{far away} from the mean or median on either side 
  (leptokurtic).  Similarly, the values in \eqn{\underline{x}} are spread out in a 
  certain way.  If the values in \eqn{\underline{x}} and \eqn{\underline{y}} are 
  spread out in the same way, then the plot of \eqn{y_{(i)}} vs. \eqn{x_{(i)}} 
  will be approximately a straight line.  If the cdf \eqn{F} is exactly the same 
  as the cdf \eqn{G}, then the plot of \eqn{y_{(i)}} vs. \eqn{x_{(i)}} will fall 
  roughly on the straight line \eqn{y = x}.  If \eqn{F} and \eqn{G} differ by a 
  shift in location and scale, that is, if \eqn{F[(x-\mu)/\sigma] = G(x)}, then 
  the plot will fall roughly on the line \eqn{y = \sigma x + \mu}.

  When \eqn{n > m}, a slight adjustment has to be made to produce the plot. Let 
  \eqn{\hat{p}_1, \hat{p}_2, \ldots, \hat{p}_m} denote the plotting positions 
  corresponding to the \eqn{m} empirical quantiles for the \eqn{y}'s and let
  \eqn{\hat{p}^*_1, \hat{p}^*_2, \ldots, \hat{p}^*_n} denote the plotting positions 
  corresponding the \eqn{n} empirical quantiles for the \eqn{x}'s. Then we plot
  \eqn{y_{(j)}} vs. \eqn{x^*_{(j)}} for \eqn{j = 1, 2, \ldots, m} where
  \deqn{x^*_{(j)} = (1 - r) x_{(i)} + r x_{(i+1)} \;\;\;\;\;\; (7)}
  \deqn{r = \frac{\hat{p}_j - \hat{p}^*_i}{\hat{p}^*_{i+1} - \hat{p}^*_i} \;\;\;\;\;\; (8)}
  \deqn{\hat{p}^*_i \le \hat{p}_j \le \hat{p}^*_{i+1} \;\;\;\;\;\; (9)}
  That is, the values for the \eqn{x^*_{(j)}}'s are determined by linear interpolation 
  based on the values of the plotting positions for \eqn{\underline{x}} and 
  \eqn{\underline{y}}.  

  A similar adjustment is made when \eqn{n < m}.

  Note that the \R function \code{\link{qqplot}} uses a different method than 
  the one in Equation (7) above; it uses linear interpolation based on 
  \code{1:n} and \code{m} by calling the \code{\link{approx}} function.
}
\value{
  \code{qqPlot} returns a list with components \code{x} and \code{y}, giving the \eqn{(x,y)} 
  coordinates of the points that have been or would have been plotted.  There are four cases to 
  consider:

  1. The argument \code{y} is not supplied and \code{plot.type="Q-Q"}.

  \item{x}{the quantiles from the theoretical distribution.}
  \item{y}{the observed quantiles (order statistics) based on the data in the argument \code{x}.}
  \cr

  2. The argument \code{y} is not supplied and \code{plot.type="Tukey Mean-Difference Q-Q"}.

  \item{x}{the averages of the observed and theoretical quantiles.}
  \item{y}{the differences between the observed quantiles (order statistics) and the theoretical quantiles.}
  \cr

  3. The argument \code{y} is supplied and \code{plot.type="Q-Q"}.

  \item{x}{the observed quantiles based on the data in the argument \code{x}.  
    Note that these are adjusted quantiles if the number of observations in the 
    argument \code{x} is greater then the number of observations in the argument \code{y}.}
  \item{y}{the observed quantiles based on the data in the argument \code{y}.  
    Note that these are adjusted quantiles if the number of observations in the 
    argument \code{y} is greater then the number of observations in the argument \code{x}.}
  \cr

  4. The argument \code{y} is supplied and \code{plot.type="Tukey Mean-Difference Q-Q"}.

  \item{x}{the averages of the quantiles based on the argument \code{x} and the quantiles based 
    on the argument \code{y}.}
  \item{y}{the differences between the quantiles based on the argument \code{x} and the quantiles based 
    on the argument \code{y}.}
}
\references{
  Chambers, J.M., W.S. Cleveland, B. Kleiner, and P.A. Tukey. (1983). 
  \emph{Graphical Methods for Data Analysis}. Duxbury Press, Boston, MA, pp.11-16.

  Cleveland, W.S. (1993). \emph{Visualizing Data}. Hobart Press, Summit, New Jersey, 360pp.

  D'Agostino, R.B. (1986a). Graphical Analysis. 
  In: D'Agostino, R.B., and M.A. Stephens, eds. \emph{Goodness-of Fit Techniques}. 
  Marcel Dekker, New York, Chapter 2, pp.7-62.
}
\author{
    Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  A \emph{quantile-quantile (Q-Q) plot}, also called a \emph{probability plot}, is a plot of the observed 
  order statistics from a random sample (the empirical quantiles) against their (estimated) 
  mean or median values based on an assumed distribution, or against the empirical quantiles 
  of another set of data (Wilk and Gnanadesikan, 1968).  Q-Q plots are used to assess whether 
  data come from a particular distribution, or whether two datasets have the same parent 
  distribution.  If the distributions have the same shape (but not necessarily the same 
  location or scale parameters), then the plot will fall roughly on a straight line.  If the 
  distributions are exactly the same, then the plot will fall roughly on the straight line \eqn{y=x}.

  A \emph{Tukey mean-difference Q-Q plot}, also called an \emph{m-d plot}, is a modification of a 
  Q-Q plot. Rather than plotting observed quantiles vs. theoretical quantiles or observed 
  \eqn{y}-quantiles vs. observed \eqn{x}-quantiles, a Tukey mean-difference Q-Q plot plots 
  the difference between the quantiles on the \eqn{y}-axis vs. the average of the quantiles on 
  the \eqn{x}-axis (Cleveland, 1993, pp.22-23).  If the two sets of quantiles come from the same 
  parent distribution, then the points in this plot should fall roughly along the horizontal line 
  \eqn{y=0}.  If one set of quantiles come from the same distribution with a shift in median, then 
  the points in this plot should fall along a horizontal line above or below the line \eqn{y=0}.  
  A Tukey mean-difference Q-Q plot enhances our perception of how the points in the Q-Q plot deviate 
  from a straight line, because it is easier to judge deviations from a horizontal line than from a 
  line with a non-zero slope.

  In a Q-Q plot, the extreme points have more variability than points toward the center.  A U-shaped 
  Q-Q plot indicates that the underlying distribution for the observations on the \eqn{y}-axis is 
  skewed to the right relative to the underlying distribution for the observations on the \eqn{x}-axis.  
  An upside-down-U-shaped Q-Q plot indicates the \eqn{y}-axis distribution is skewed left relative to 
  the \eqn{x}-axis distribution.  An S-shaped Q-Q plot indicates the \eqn{y}-axis distribution has 
  shorter tails than the \eqn{x}-axis distribution.  Conversely, a plot that is bent down on the 
  left and bent up on the right indicates that the \eqn{y}-axis distribution has longer tails than 
  the \eqn{x}-axis distribution.
}
\seealso{
  \code{\link{ppoints}}, \code{\link{ecdfPlot}}, \code{\link{Distribution.df}}, 
  \code{\link{qqPlotGestalt}}, \code{\link{qqPlotCensored}}, \code{\link{qqnorm}}.
}
\examples{
  # The guidance document USEPA (1994b, pp. 6.22--6.25) 
  # contains measures of 1,2,3,4-Tetrachlorobenzene (TcCB) 
  # concentrations (in parts per billion) from soil samples 
  # at a Reference area and a Cleanup area.  These data are strored 
  # in the data frame EPA.94b.tccb.df.  
  #
  # Create an Q-Q plot for the reference area data first assuming a 
  # normal distribution, then a lognormal distribution, then a 
  # gamma distribution.
  
  # Assume a normal distribution
  #-----------------------------

  dev.new()
  with(EPA.94b.tccb.df, qqPlot(TcCB[Area == "Reference"]))

  dev.new()
  with(EPA.94b.tccb.df, qqPlot(TcCB[Area == "Reference"], add.line = TRUE))

  dev.new()
  with(EPA.94b.tccb.df, qqPlot(TcCB[Area == "Reference"], 
    plot.type = "Tukey", add.line = TRUE))


  # The Q-Q plot based on assuming a normal distribution shows a U-shape,
  # indicating the Reference area TcCB data are skewed to the right
  # compared to a normal distribuiton.

  # Assume a lognormal distribution
  #--------------------------------

  dev.new()
  with(EPA.94b.tccb.df, 
    qqPlot(TcCB[Area == "Reference"], dist = "lnorm", 
      digits = 2, points.col = "blue", add.line = TRUE))

  dev.new()
  with(EPA.94b.tccb.df, 
    qqPlot(TcCB[Area == "Reference"], dist = "lnorm", 
      digits = 2, plot.type = "Tukey", points.col = "blue", 
      add.line = TRUE))

  # Alternative parameterization

  dev.new()
  with(EPA.94b.tccb.df, 
    qqPlot(TcCB[Area == "Reference"], dist = "lnormAlt", 
      estimate.params = TRUE, digits = 2, points.col = "blue", 
      add.line = TRUE))

  dev.new()
  with(EPA.94b.tccb.df, 
    qqPlot(TcCB[Area == "Reference"], dist = "lnormAlt", 
      digits = 2, plot.type = "Tukey", points.col = "blue", 
      add.line = TRUE))


  # The lognormal distribution appears to be an adequate fit.
  # Now look at a Q-Q plot assuming a gamma distribution.
  #----------------------------------------------------------

  dev.new()
  with(EPA.94b.tccb.df, 
    qqPlot(TcCB[Area == "Reference"], dist = "gamma", 
      estimate.params = TRUE, digits = 2, points.col = "blue", 
      add.line = TRUE))

  dev.new()
  with(EPA.94b.tccb.df, 
    qqPlot(TcCB[Area == "Reference"], dist = "gamma", 
      digits = 2, plot.type = "Tukey", points.col = "blue", 
      add.line = TRUE))

  # Alternative Parameterization

  dev.new()
  with(EPA.94b.tccb.df, 
    qqPlot(TcCB[Area == "Reference"], dist = "gammaAlt", 
      estimate.params = TRUE, digits = 2, points.col = "blue", 
      add.line = TRUE))

  dev.new()
  with(EPA.94b.tccb.df, 
    qqPlot(TcCB[Area == "Reference"], dist = "gammaAlt", 
      digits = 2, plot.type = "Tukey", points.col = "blue", 
      add.line = TRUE))

  #-------------------------------------------------------------------------------------

  # Generate 20 observations from a gamma distribution with parameters 
  # shape=2 and scale=2, then create a normal (Gaussian) Q-Q plot for these data. 
  # (Note: the call to set.seed simply allows you to reproduce this example.)

  set.seed(357) 
  dat <- rgamma(20, shape=2, scale=2) 
  dev.new()
  qqPlot(dat, add.line = TRUE)

  # Now assume a gamma distribution and estimate the parameters
  #------------------------------------------------------------

  dev.new()
  qqPlot(dat, dist = "gamma", estimate.params = TRUE, add.line = TRUE)

  # Clean up
  #---------
  rm(dat)
  graphics.off()
}
\keyword{distribution}
\keyword{hplot}
