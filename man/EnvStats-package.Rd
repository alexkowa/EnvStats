\name{EnvStats-package}
\alias{EnvStats-package}
\alias{EnvStats}
\docType{package}
\title{
  Package for Environmental Statistics, Including US EPA Guidance
}
\description{
  A comprehensive R package for environmental statistics and 
  the successor to the S-PLUS module \emph{EnvironmentalStats for S-PLUS} 
  (first released in April, 1997).  \pkg{EnvStats} provides a set of powerful 
  functions for graphical and statistical analyses of environmental data, with 
  a focus on analyzing chemical concentrations and physical parameters, usually in 
  the context of mandated environmental monitoring.  It includes major environmental 
  statistical methods found in the literature and regulatory guidance documents, 
  and extensive help that explains what these methods do, how to use them, 
  and where to find them in the literature.  It also includes numerous built-in data 
  sets from regulatory guidance documents and environmental statistics literature, and 
  scripts reproducing analyses presented in the user's manual (Millard, 2013).

  For a complete list of functions and datasets, you can do any of the following:

  \itemize{
    \item See the help file \link[=FcnsByCat]{Functions By Category} for a listing of 
          functions by category.
    \item If you are in the on-line help, scroll to the bottom of this help page and 
          click on the \bold{Index} link.
    \item Type \code{library(help="EnvStats")} at the command prompt.
  }

  \bold{Note:}  The names of all \pkg{EnvStats} functions start with a lowercase letter, and 
  the names of all \pkg{EnvStats} datasets and data objects start an uppercase letter.  
  You can type \code{newsEnvStats()} at the \R command prompt for the latest news for  
  the \pkg{EnvStats} package.
}
\details{
\tabular{ll}{
Package: \tab EnvStats\cr
Type: \tab Package\cr
Version: \tab 2.0.1\cr
Date: \tab 2015-05-24\cr
License: \tab GPL (>=3)\cr
LazyLoad: \tab yes\cr
}

  A companion file \bold{EnvStats-manual.pdf} containing a listing of all the current help 
  files is located on the \R CRAN web site at 
  \url{http://cran.r-project.org/web/packages/EnvStats/EnvStats.pdf} and also in the 
  \bold{doc} subdirectory of the directory where the \pkg{EnvStats} package 
  was installed.  For example, if you installed R under Windows, this file might be located 
  in the directory \bold{C:\\Program Files\\R-*.**.*\\library\\EnvStats\\doc}, where 
  \bold{*.**.*} denotes the version of R you are using (e.g., 3.2.0) or in the directory 
  \bold{C:\\Users\\\emph{Name}\\Documents\\R\\win-library\\*.**.*\\EnvStats\\doc}, where 
  \bold{\emph{Name}} denotes your user name on the Windows operating system.

  EnvStats comes with companion scripts, located in the \bold{scripts} subdirectory of the 
  directory where the package was installed.  One set of scripts lets you reproduce the examples in 
  the User's Manual (currently is still in preparation).  There are also scripts that let you reproduce 
  examples from US EPA guidance documents.

  See the \bold{References} section below for documentation for the predecessor to \pkg{EnvStats}, 
  \emph{EnvironmentalStats for S-PLUS} for Windows. 

  Features of EnvStats include:

  \itemize{

    \item 
    New functions for computing \link[=FcnsByCatSumStats]{summary statistics} and 
    creating \link[=FcnsByCatSumStats]{summary plots} to compare the distributions 
    of groups side-by-side.
  

    \item 
    New \link[=FcnsByCatProbDists]{probability distributions} have been added to the 
    ones already available in \R, including the extreme value distribution and the 
    zero-modified lognormal (delta) distribution.  You can compute quantities associated 
    with these probability distributions (probability density functions, cumulative 
    distribution functions, and quantiles), and generate random numbers from these 
    distributions.


    \item 
    \link[=FcnsByCatPlotProbDists]{Plot probability distributions} so you can see how they 
    change with the value of the distribution parameter(s).


    \item 
    \link[=FcnsByCatEstDistParams]{Estimate distribution parameters} and 
    \link[=FcnsByCatEstDistQuants]{distribution quantiles}, and compute confidence intervals 
    for commonly used probability distributions, including special methods for the 
    lognormal and gamma distributions.


    \item 
    Perform and plot the results of \link[=FcnsByCatGOFTests]{goodness-of-fit tests}:
      \itemize{
        \item Observed and Fitted Distributions
        \item Quantile-Quantile Plots
        \item Results of Shaprio-Wilk test, Kolmogorov-Smirnov test, etc.
      }
    Includes a new generalized goodness-of-fit test for any continuous distribution.    


    \item
    Functions for assessing optimal \link[=FcnsByCatDataTrans]{Box-Cox data transformations}.


    \item 
    Compute parametric and non-parametric 
    \link[=FcnsByCatPredInts]{prediction intervals, simultaneous prediction intervals}, 
    and \link[=FcnsByCatTolInts]{tolerance intervals}.


    \item 
    New functions for \link[=FcnsByCatHypothTests]{hypothesis tests}, including:
      \itemize{
        \item Nonparametric estimation and tests for seasonal trend
        \item Fisher's one-sample randomization (permutation) test for location
        \item Quantile test to detect a shift in the tail of one population relative to another
        \item Two-sample linear rank tests
        \item Test for serial correlation based on von Neumann rank test
      }


    \item 
    Perform \link[=FcnsByCatCalibration]{calibration} based on a machine signal to determine 
    decision and detection limits and report estimated concentrations along with confidence 
    intervals.


    \item 
    Easily perform \link[=FcnsByCatPower]{power and sample size} computations and create companion 
    plots for sampling designs based on confidence intervals, hypothesis tests, prediction intervals, 
    and tolerance intervals.


    \item  
    Handle singly and multiply \link[=FcnsByCatCensoredData]{censored (less-than-detection-limit) data}:
      \itemize{
        \item Empirical CDF and Quantile-Quantile Plots 
        \item Parameter/Quantile Estimation and Confidence Intervals 
        \item Prediction and Tolerance Intervals 
        \item Goodness-of-Fit Tests 
        \item Optimal Box-Cox Transformations 
        \item Two-Sample Rank Tests 
      }


    \item 
    Functions for performing 
    \link[=FcnsByCatMCandRisk]{Monte Carlo simulation and probabilistic risk assessement}.


    \item 
    Reproduce specific examples in EPA guidance documents by using 
    built-in data sets from these documents and running companion scripts.
  }
}
\author{
Steven P. Millard

Maintainer: Steven P. Millard <EnvStats@ProbStatInfo.com>
}
\references{
  Millard, S.P. (2013). \emph{EnvStats: An R Package for Environmental Statistics}. 
  Springer, New York.

  Millard, S.P. (2002). \emph{EnvironmentalStats for S-PLUS: User's Manual for Version 2.0}. 
  Second Edition. Springer-Verlag, New York.
  
  Millard, S.P., and N.K. Neerchal. (2001). \emph{Environmental Statistics with S-PLUS}. 
  CRC Press, Boca Raton, FL.
}
\keyword{ package }
\examples{
  # Look at plots and summary statistics for the TcCB data given in 
  # USEPA (1994b), (the data are stored in EPA.94b.tccb.df). 
  # Arbitrarily set the one censored observation to the censoring level. 
  # Group by the variable Area.

  EPA.94b.tccb.df
  #    TcCB.orig   TcCB Censored      Area
  #1        0.22   0.22    FALSE Reference
  #2        0.23   0.23    FALSE Reference
  #...
  #46       1.20   1.20    FALSE Reference
  #47       1.33   1.33    FALSE Reference
  #48      <0.09   0.09     TRUE   Cleanup
  #49       0.09   0.09    FALSE   Cleanup
  #...
  #123     51.97  51.97    FALSE   Cleanup
  #124    168.64 168.64    FALSE   Cleanup


  # First plot the data
  #--------------------
  dev.new()
  stripChart(TcCB ~ Area, data = EPA.94b.tccb.df, 
    xlab = "Area", ylab = "TcCB (ppb)")
  mtext("TcCB Concentrations by Area", line = 3, cex = 1.25, font = 2)

  dev.new()
  stripChart(log10(TcCB) ~ Area, data = EPA.94b.tccb.df, 
    p.value = TRUE, 
    xlab = "Area", ylab = expression(paste(log[10], " [ TcCB (ppb) ]")))
  mtext(expression(paste(log[10], "(TcCB) Concentrations by Area")), 
    line = 3, cex = 1.25, font = 2)

  #--------------------------------------------------------------------

  # Now compute summary statistics
  #-------------------------------
  
  sum(EPA.94b.tccb.df$Censored) 
  #[1] 1 

  with(EPA.94b.tccb.df, TcCB[Censored])
  #0.09 

  # Summary statistics will treat the one censored value 
  # as assuming the detection limit.

  summaryFull(TcCB ~ Area, data = EPA.94b.tccb.df)
  #                             Cleanup  Reference
  #N                             77       47      
  #Mean                           3.915    0.5985 
  #Median                         0.43     0.54   
  #10% Trimmed Mean               0.6846   0.5728 
  #Geometric Mean                 0.5784   0.5382 
  #Skew                           7.717    0.9019 
  #Kurtosis                      62.67     0.132  
  #Min                            0.09     0.22   
  #Max                          168.6      1.33   
  #Range                        168.5      1.11   
  #1st Quartile                   0.23     0.39   
  #3rd Quartile                   1.1      0.75   
  #Standard Deviation            20.02     0.2836 
  #Geometric Standard Deviation   3.898    1.597  
  #Interquartile Range            0.87     0.36   
  #Median Absolute Deviation      0.3558   0.2669 
  #Coefficient of Variation       5.112    0.4739

  summaryStats(TcCB ~ Area, data = EPA.94b.tccb.df, digits = 1)
  #           N Mean   SD Median Min   Max
  #Cleanup   77  3.9 20.0    0.4 0.1 168.6
  #Reference 47  0.6  0.3    0.5 0.2   1.3

  #----------------------------------------------------------------

  # Compute Shapiro-Wilk Goodness-of-Fit statistic for the 
  # Reference Area TcCB data assuming a lognormal distribution
  #-----------------------------------------------------------
  
  sw.list <- gofTest(TcCB ~ 1, data = EPA.94b.tccb.df, 
    subset = Area == "Reference", dist = "lnorm")
  sw.list

  # Results of Goodness-of-Fit Test
  # -------------------------------
  #
  # Test Method:                     Shapiro-Wilk GOF
  #
  # Hypothesized Distribution:       Lognormal
  #
  # Estimated Parameter(s):          meanlog = -0.6195712
  #                                  sdlog   =  0.4679530
  #
  # Estimation Method:               mvue
  #
  # Data:                            TcCB
  #
  # Subset With:                     Area == "Reference"
  #
  # Data Source:                     EPA.94b.tccb.df
  #
  # Sample Size:                     47
  #
  # Test Statistic:                  W = 0.978638
  #
  # Test Statistic Parameter:        n = 47
  #
  # P-value:                         0.5371935
  #
  # Alternative Hypothesis:          True cdf does not equal the
  #                                  Lognormal Distribution.

  #----------

  # Plot results of GOF test
  dev.new()
  plot(sw.list)

  #----------------------------------------------------------------

  # Based on the Reference Area data, estimate 90th percentile 
  # and compute a 95\% confidence limit for the 90th percentile 
  # assuming a lognormal distribution.
  #------------------------------------------------------------

  with(EPA.94b.tccb.df, 
    eqlnorm(TcCB[Area == "Reference"], p = 0.9, ci = TRUE))

  # Results of Distribution Parameter Estimation
  # --------------------------------------------
  #
  # Assumed Distribution:            Lognormal
  #
  # Estimated Parameter(s):          meanlog = -0.6195712
  #                                  sdlog   =  0.4679530
  #
  # Estimation Method:               mvue
  #
  # Estimated Quantile(s):           90'th \%ile = 0.9803307
  #
  # Quantile Estimation Method:      qmle
  #
  # Data:                            TcCB[Area == "Reference"]
  #
  # Sample Size:                     47
  #
  # Confidence Interval for:         90'th \%ile
  #
  # Confidence Interval Method:      Exact
  #
  # Confidence Interval Type:        two-sided
  #
  # Confidence Level:                95\%
  #
  # Confidence Interval:             LCL = 0.8358791
                                     UCL = 1.2154977
  #----------

  # Cleanup
  rm(TcCB.ref, sw.list)
}
