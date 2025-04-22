\name{ciTableMean}
\alias{ciTableMean}
\title{
  Table of Confidence Intervals for Mean or Difference Between Two Means
}
\description{
  Create a table of confidence intervals for the mean of a normal distribution 
  or the difference between two means following Bacchetti (2010), by varying 
  the estimated standard deviation and the estimated mean or differene between 
  the two estimated means given the sample size(s).
}
\usage{
  ciTableMean(n1 = 10, n2 = n1, diff.or.mean = 2:0, SD = 1:3, 
    sample.type = "two.sample", ci.type = "two.sided", conf.level = 0.95, 
    digits = 1)
}
\arguments{
  \item{n1}{
  positive integer greater than 1 specifying the sample size when \cr
  \code{sample.type="one.sample"} or the sample size for group 1 when \cr
  \code{sample.type="two.sample"}.  The default value is \code{n1=10}.
}
  \item{n2}{
  positive integer greater than 1 specifying the sample size for group 2 when 
  \code{sample.type="two.sample"}.  The default value is \code{n2=n1}, i.e., 
  equal sample sizes.  This argument is ignored when \code{sample.type="one.sample"}.
}
  \item{diff.or.mean}{
  numeric vector indicating either the assumed difference between the two sample means 
  when \code{sample.type="two.sample"} or the value of the sample mean when 
  \code{sample.type="one.sample"}.  The default value is \code{diff.or.mean=2:0}.  
  Missing (\code{NA}), undefined (\code{NaN}), an infinite 
  (\code{-Inf}, \code{Inf}) values are not allowed.
}
  \item{SD}{
  numeric vector of positive values specifying the assumed estimated standard 
  deviation.  The default value is \code{SD=1:3}.  
  Missing (\code{NA}), undefined (\code{NaN}), an infinite 
  (\code{-Inf}, \code{Inf}) values are not allowed.
}
  \item{sample.type}{
  character string specifying whether to create confidence intervals for the difference 
  between two means (\code{sample.type="two.sample"}; the default) or confidence 
  intervals for a single mean (\code{sample.type="one.sample"}).
}
  \item{ci.type}{
  character string indicating what kind of confidence interval to compute.  The 
  possible values are \code{"two-sided"} (the default), \code{"lower"}, and 
  \code{"upper"}.
}
  \item{conf.level}{
  a scalar between 0 and 1 indicating the confidence level of the confidence interval.  
  The default value is \code{conf.level=0.95}.
}
  \item{digits}{
  positive integer indicating how many decimal places to display in the table.  The 
  default value is \code{digits=1}.
}
}
\details{
  Following Bacchetti (2010) (see NOTE below), the function \code{ciTableMean} 
  allows you to perform sensitivity analyses while planning future studies by 
  producing a table of confidence intervals for the mean or the difference 
  between two means by varying the estimated standard deviation and the 
  estimated mean or differene between the two estimated means given the 
  sample size(s).

  \emph{One Sample Case} (\code{sample.type="one.sample"}) \cr
  Let \eqn{\underline{x} = (x_1, x_2, \ldots, x_n)} be a vector of 
  \eqn{n} observations from an \link[stats:Normal]{normal (Gaussian) distribution} with 
  parameters \code{mean=}\eqn{\mu} and \code{sd=}\eqn{\sigma}.

  The usual confidence interval for \eqn{\mu} is constructed as follows.  
  If \code{ci.type="two-sided"}, the \eqn{(1-\alpha)}100\% confidence interval for 
  \eqn{\mu} is given by:
  \deqn{[\hat{\mu} - t(n-1, 1-\alpha/2) \frac{\hat{\sigma}}{\sqrt{n}}, \, \hat{\mu} + t(n-1, 1-\alpha/2) \frac{\hat{\sigma}}{\sqrt{n}}] \;\;\;\;\;\; (1)}
  where 
  \deqn{\hat{\mu} = \bar{x} = \frac{1}{n} \sum_{i=1}^n x_i \;\;\;\;\;\; (2)}
  \deqn{\hat{\sigma}^2 = s^2 = \frac{1}{n-1} \sum_{i=1}^n (x_i - \bar{x})^2 \;\;\;\;\;\; (3)}
  and \eqn{t(\nu, p)} is the \eqn{p}'th quantile of 
  \link[stats:TDist]{Student's t-distribution} with \eqn{\nu} degrees of freedom 
  (Zar, 2010; Gilbert, 1987; Ott, 1995; Helsel and Hirsch, 1992).

  If \code{ci.type="lower"}, the \eqn{(1-\alpha)}100\% confidence interval for 
  \eqn{\mu} is given by:
  \deqn{[\hat{\mu} - t(n-1, 1-\alpha) \frac{\hat{\sigma}}{\sqrt{n}}, \, \infty] \;\;\;\; (4)}
  and if \code{ci.type="upper"}, the confidence interval is given by:
  \deqn{[-\infty, \, \hat{\mu} + t(n-1, 1-\alpha/2) \frac{\hat{\sigma}}{\sqrt{n}}] \;\;\;\; (5)}

  For the one-sample case, the argument \code{n1} corresponds to \eqn{n} in 
  Equation (1), the argument \cr
  \code{diff.or.mean} corresponds to 
  \eqn{\hat{\mu} = \bar{x}} in Equation (2), and the argument \code{SD} corresponds to 
  \eqn{\hat{\sigma} = s} in Equation (3).
  \cr

  \emph{Two Sample Case} (\code{sample.type="two.sample"}) \cr
  Let \eqn{\underline{x}_1 = (x_{11}, x_{21}, \ldots, x_{n_11})} be a vector of 
  \eqn{n_1} observations from an \link[stats:Normal]{normal (Gaussian) distribution} 
  with parameters \code{mean=}\eqn{\mu_1} and \code{sd=}\eqn{\sigma}, and let 
  \eqn{\underline{x}_2 = (x_{12}, x_{22}, \ldots, x_{n_22})} be a vector of 
  \eqn{n_2} observations from an \link[stats:Normal]{normal (Gaussian) distribution} 
  with parameters \code{mean=}\eqn{\mu_2} and \code{sd=}\eqn{\sigma}.

  The usual confidence interval for the difference between the two population means 
  \eqn{\mu_1 - \mu_2} is constructed as follows.  
  If \code{ci.type="two-sided"}, the \eqn{(1-\alpha)}100\% confidence interval for 
  \eqn{\mu_1 - \mu_2} is given by:
  \deqn{[(\hat{\mu}_1 - \hat{\mu}_2) - t(n_1 + n_2 -2, 1-\alpha/2) \hat{\sigma}\sqrt{\frac{1}{n_1} + \frac{1}{n_2}}, \; (\hat{\mu}_1 - \hat{\mu}_2) + t(n_1 + n_2 -2, 1-\alpha/2) \hat{\sigma}\sqrt{\frac{1}{n_1} + \frac{1}{n_2}}] \;\;\;\;\;\; (6)}
  where 
  \deqn{\hat{\mu}_1 = \bar{x}_1 = \frac{1}{n_1} \sum_{i=1}^{n_1} x_{i1} \;\;\;\;\;\; (7)}
  \deqn{\hat{\mu}_2 = \bar{x}_2 = \frac{1}{n_2} \sum_{i=1}^{n_2} x_{i2} \;\;\;\;\;\; (8)}
  \deqn{\hat{\sigma}^2 = s_p^2 = \frac{(n_1-1) s_1^2 + (n_2-1)s_2^2}{n_1 + n_2 - 2} \;\;\;\;\;\; (9)}
  \deqn{s_1^2 = \frac{1}{n_1-1} \sum_{i=1}^{n_1} (x_{i1} - \bar{x}_1)^2 \;\;\;\;\;\; (10)}
  \deqn{s_2^2 = \frac{1}{n_2-1} \sum_{i=1}^{n_2} (x_{i2} - \bar{x}_2)^2 \;\;\;\;\;\; (11)}
  and \eqn{t(\nu, p)} is the \eqn{p}'th quantile of 
  \link[stats:TDist]{Student's t-distribution} with \eqn{\nu} degrees of freedom 
  (Zar, 2010; Gilbert, 1987; Ott, 1995; Helsel and Hirsch, 1992).

  If \code{ci.type="lower"}, the \eqn{(1-\alpha)}100\% confidence interval for 
  \eqn{\mu_1 - \mu_2} is given by:
  \deqn{[(\hat{\mu}_1 - \hat{\mu}_2) - t(n_1 + n_2 -2, 1-\alpha) \hat{\sigma}\sqrt{\frac{1}{n_1} + \frac{1}{n_2}}, \; \infty] \;\;\;\;\;\; (12)}
  and if \code{ci.type="upper"}, the confidence interval is given by:
  \deqn{[-\infty, \; (\hat{\mu}_1 - \hat{\mu}_2) - t(n_1 + n_2 -2, 1-\alpha) \hat{\sigma}\sqrt{\frac{1}{n_1} + \frac{1}{n_2}}] \;\;\;\;\;\; (13)}

  For the two-sample case, the arguments \code{n1} and \code{n2} correspond to 
  \eqn{n_1} and \eqn{n_2} in Equation (6), the argument \code{diff.or.mean} corresponds 
  to \eqn{\hat{\mu_1} - \hat{\mu_2} = \bar{x}_1 - \bar{x}_2} in Equations (7) and (8), 
  and the argument \code{SD} corresponds to \eqn{\hat{\sigma} = s_p} in Equation (9).
}
\value{
  a data frame with the rows varying the standard deviation and the columns 
  varying the estimated mean or difference between the means.  Elements of the 
  data frame are character strings indicating the confidence intervals.
}
\references{
  Bacchetti, P. (2010).  Current sample size conventions: Flaws, Harms, and 
    Alternatives.  \emph{BMC Medicine} \bold{8}, 17--23.

  Berthouex, P.M., and L.C. Brown. (2002). 
  \emph{Statistics for Environmental Engineers}.  Second Edition.   
  Lewis Publishers, Boca Raton, FL.

  Gilbert, R.O. (1987). \emph{Statistical Methods for Environmental Pollution Monitoring}. 
  Van Nostrand Reinhold, New York, NY.

  Helsel, D.R., and R.M. Hirsch. (1992). 
  \emph{Statistical Methods in Water Resources Research}. 
  Elsevier, New York, NY.

  Millard, S.P., and N.K. Neerchal. (2001). \emph{Environmental Statistics with S-PLUS}. 
  CRC Press, Boca Raton, FL.

  Ott, W.R. (1995). \emph{Environmental Statistics and Data Analysis}. 
  Lewis Publishers, Boca Raton, FL.

  Zar, J.H. (2010). \emph{Biostatistical Analysis}. Fifth Edition. 
  Prentice-Hall, Upper Saddle River, NJ.
}
\author{
    Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  Bacchetti (2010) presents strong arguments against the current convention in 
  scientific research for computing sample size that is based on formulas that 
  use a fixed Type I error (usually 5\%) and a fixed minimal power (often 80\%) 
  without regard to costs.  He notes that a key input to these formulas is a 
  measure of variability (usually a standard deviation) that is difficult to 
  measure accurately "unless there is so much preliminary data that the study 
  isn't really needed."  Also, study designers often avoid defining what a 
  scientifically meaningful difference is by presenting sample size results in 
  terms of the effect size (i.e., the difference of interest divided by the 
  elusive standard deviation).  Bacchetti (2010) encourages study designers to use 
  simple tables in a sensitivity analysis to see what results of a study may look 
  like for low, moderate, and high rates of variability and large, intermediate, 
  and no underlying differences in the populations or processes being studied.
}
\seealso{
  \code{\link{enorm}}, \code{\link{t.test}}, \code{\link{ciTableProp}}, 
  \code{\link{ciNormHalfWidth}}, \code{\link{ciNormN}}, 
  \code{\link{plotCiNormDesign}}.
}
\examples{
  # Show how potential confidence intervals for the difference between two means 
  # will look assuming standard deviations of 1, 2, or 3, differences between 
  # the two means of 2, 1, or 0, and a sample size of 10 in each group.

  ciTableMean()
  #          Diff=2      Diff=1      Diff=0
  #SD=1 [ 1.1, 2.9] [ 0.1, 1.9] [-0.9, 0.9]
  #SD=2 [ 0.1, 3.9] [-0.9, 2.9] [-1.9, 1.9]
  #SD=3 [-0.8, 4.8] [-1.8, 3.8] [-2.8, 2.8]

  #==========

  # Show how a potential confidence interval for a mean will look assuming 
  # standard deviations of 1, 2, or 5, a sample mean of 5, 3, or 1, and 
  # a sample size of 15.

  ciTableMean(n1 = 15, diff.or.mean = c(5, 3, 1), SD = c(1, 2, 5), sample.type = "one")
  #          Mean=5      Mean=3      Mean=1
  #SD=1 [ 4.4, 5.6] [ 2.4, 3.6] [ 0.4, 1.6]
  #SD=2 [ 3.9, 6.1] [ 1.9, 4.1] [-0.1, 2.1]
  #SD=5 [ 2.2, 7.8] [ 0.2, 5.8] [-1.8, 3.8]

  #==========

  # The data frame EPA.09.Ex.16.1.sulfate.df contains sulfate concentrations
  # (ppm) at one background and one downgradient well. The estimated
  # mean and standard deviation for the background well are 536 and 27 ppm,
  # respectively, based on a sample size of n = 8 quarterly samples taken over 
  # 2 years.  A two-sided 95% confidence interval for this mean is [514, 559], 
  # which has a  half-width of 23 ppm.
  #
  # The estimated mean and standard deviation for the downgradient well are 
  # 608 and 18 ppm, respectively, based on a sample size of n = 6 quarterly 
  # samples.  A two-sided 95% confidence interval for the difference between 
  # this mean and the background mean is [44, 100] ppm.
  #
  # Suppose we want to design a future sampling program and are interested in 
  # the size of the confidence interval for the difference between the two means.  
  # We will use ciTableMean to generate a table of possible confidence intervals 
  # by varying the assumed standard deviation and assumed differences between 
  # the means.


  # Look at the data
  #-----------------

  EPA.09.Ex.16.1.sulfate.df
  #   Month Year    Well.type Sulfate.ppm
  #1    Jan 1995   Background         560
  #2    Apr 1995   Background         530
  #3    Jul 1995   Background         570
  #4    Oct 1995   Background         490
  #5    Jan 1996   Background         510
  #6    Apr 1996   Background         550
  #7    Jul 1996   Background         550
  #8    Oct 1996   Background         530
  #9    Jan 1995 Downgradient          NA
  #10   Apr 1995 Downgradient          NA
  #11   Jul 1995 Downgradient         600
  #12   Oct 1995 Downgradient         590
  #13   Jan 1996 Downgradient         590
  #14   Apr 1996 Downgradient         630
  #15   Jul 1996 Downgradient         610
  #16   Oct 1996 Downgradient         630


  # Compute the estimated mean and standard deviation for the 
  # background well.
  #-----------------------------------------------------------

  Sulfate.back <- with(EPA.09.Ex.16.1.sulfate.df,
    Sulfate.ppm[Well.type == "Background"])

  enorm(Sulfate.back, ci = TRUE)

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            Normal
  #
  #Estimated Parameter(s):          mean = 536.2500
  #                                 sd   =  26.6927
  #
  #Estimation Method:               mvue
  #
  #Data:                            Sulfate.back
  #
  #Sample Size:                     8
  #
  #Confidence Interval for:         mean
  #
  #Confidence Interval Method:      Exact
  #
  #Confidence Interval Type:        two-sided
  #
  #Confidence Level:                95%
  #
  #Confidence Interval:             LCL = 513.9343
  #                                 UCL = 558.5657


  # Compute the estimated mean and standard deviation for the 
  # downgradient well.
  #----------------------------------------------------------

  Sulfate.down <- with(EPA.09.Ex.16.1.sulfate.df,
    Sulfate.ppm[Well.type == "Downgradient"])

  enorm(Sulfate.down, ci = TRUE)

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            Normal
  #
  #Estimated Parameter(s):          mean = 608.33333
  #                                 sd   =  18.34848
  #
  #Estimation Method:               mvue
  #
  #Data:                            Sulfate.down
  #
  #Sample Size:                     6
  #
  #Number NA/NaN/Inf's:             2
  #
  #Confidence Interval for:         mean
  #
  #Confidence Interval Method:      Exact
  #
  #Confidence Interval Type:        two-sided
  #
  #Confidence Level:                95%
  #
  #Confidence Interval:             LCL = 589.0778
  #                                 UCL = 627.5889


  # Compute the estimated difference between the means and the confidence 
  # interval for the difference:
  #----------------------------------------------------------------------

  print(t.test(Sulfate.down, Sulfate.back, var.equal = TRUE))

  #Results of Hypothesis Test
  #--------------------------
  #
  #Null Hypothesis:                 difference in means = 0
  #
  #Alternative Hypothesis:          True difference in means is not equal to 0
  #
  #Test Name:                        Two Sample t-test
  #
  #Estimated Parameter(s):          mean of x = 608.3333
  #                                 mean of y = 536.2500
  #
  #Data:                            Sulfate.down and Sulfate.back
  #
  #Test Statistic:                  t = 5.660985
  #
  #Test Statistic Parameter:        df = 12
  #
  #P-value:                         0.0001054306
  #
  #95% Confidence Interval:         LCL = 44.33974
  #                                 UCL = 99.82693


  # Use ciTableMean to look how the confidence interval for the difference
  # between the background and downgradient means in a future study using eight
  # quarterly samples at each well varies with assumed value of the pooled standard
  # deviation and the observed difference between the sample means. 
  #--------------------------------------------------------------------------------

  # Our current estimate of the pooled standard deviation is 24 ppm:

  summary(lm(Sulfate.ppm ~ Well.type, data = EPA.09.Ex.16.1.sulfate.df))$sigma
  #[1] 23.57759

  # We can see that if this is overly optimistic and in our next study the 
  # pooled standard deviation is around 50 ppm, then if the observed difference 
  # between the means is 50 ppm, the lower end of the confidence interval for 
  # the difference between the two means will include 0, so we may want to 
  # increase our sample size.

  ciTableMean(n1 = 8, n2 = 8, diff = c(100, 50, 0), SD = c(15, 25, 50), digits = 0)

  #        Diff=100    Diff=50     Diff=0
  #SD=15 [ 84, 116] [ 34,  66] [-16,  16]
  #SD=25 [ 73, 127] [ 23,  77] [-27,  27]
  #SD=50 [ 46, 154] [ -4, 104] [-54,  54]
  
  #==========

  # Clean up
  #---------
  rm(Sulfate.back, Sulfate.down)
}
\keyword{ distribution }
\keyword{ design }
\keyword{ htest }
