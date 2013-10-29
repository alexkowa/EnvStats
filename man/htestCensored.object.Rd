\name{htestCensored.object}
\alias{htestCensored.object}
\docType{class}
\title{
  S3 Class "htestCensored"
}
\description{
  This class of objects is returned by \pkg{EnvStats} functions that perform 
  hypothesis tests based on \bold{\emph{censored}} data.  
  Objects of class \code{"htestCensored"} are lists that contain information about 
  the null and alternative hypotheses, the censoring side, the censoring levels, 
  the percentage of observations that are censored, 
  the estimated distribution parameters (if applicable), the test statistic, 
  the p-value, and (optionally, if applicable) 
  confidence intervals for distribution parameters.
}
\details{
  Objects of S3 class \code{"htestCensored"} are returned by 
  the functions listed in the section \bold{Hypothesis Tests}  
  in the help file 
  \link[=FcnsByCatCensoredData]{EnvStats Functions for Censored Data}.  
  Currently, the only function listed is 
  \code{\link{twoSampleLinearRankTestCensored}}. 
}
\section{Methods}{
  Generic functions that have methods for objects of class 
  \code{"htestCensored"} include: \cr
  \code{\link{print}}.
}
\value{
  \strong{Required Components} \cr
  The following components must be included in a legitimate list of 
  class \code{"htestCensored"}.

  \item{statistic}{
  numeric scalar containing the value of the test statistic, with a 
  \code{names} attribute indicating the null distribution.
}
  \item{parameters}{
  numeric vector containing the parameter(s) associated with the null distribution of 
  the test statistic.  This vector has a \code{names} attribute describing its 
  element(s).
}
  \item{p.value}{
  numeric scalar containing the p-value for the test under the null hypothesis.
}
  \item{null.value}{
  numeric vector containing the value(s) of the population parameter(s) specified by 
  the null hypothesis.  This vector has a \code{names} attribute describing its 
  elements.
}
  \item{alternative}{
  character string indicating the alternative hypothesis (the value of the input 
  argument \code{alternative}).  Possible values are \code{"greater"}, \code{"less"}, 
  or \code{"two-sided"}.
}
  \item{method}{
  character string giving the name of the test used.
} 
  \item{sample.size}{
  numeric scalar containing the number of non-missing observations in the sample used 
  for the hypothesis test.
}
  \item{data.name}{
  character string containing the actual name(s) of the input data.
}
  \item{bad.obs}{
  the number of missing (\code{NA}), undefined (\code{NaN}) and/or infinite 
  (\code{Inf}, \code{-Inf}) values that were removed from the data object prior to 
  performing the hypothesis test.
} 
  \item{censoring.side}{character string indicating whether the data are 
    left- or right-censored.
}
  \item{censoring.name}{character string indicating the name of the data object 
    used to identify which values are censored.
}
  \item{censoring.levels}{numeric scalar or vector indicating the censoring level(s).
}
  \item{percent.censored}{numeric scalar indicating the percent of non-missing 
    observations that are censored.
}

  \strong{Optional Components} \cr
  The following component may optionally be included in an object of 
  of class \code{"htestCensored"}:

  \item{estimate}{
  numeric vector containing the value(s) of the estimated population parameter(s) 
  involved in the null hypothesis.  This vector has a \code{names} attribute 
  describing its element(s).
}
  \item{estimation.method}{
  character string containing the method used to compute the estimated distribution 
  parameter(s).  The value of this component will depend on the available estimation 
  methods (see \code{\link{Distribution.df}}).
}
  \item{interval}{
  a list containing information about a confidence, prediction, or tolerance interval.
}
}
\note{
  Since objects of class \code{"htestCensored"} are lists, you may extract 
  their components with the \code{$} and \code{[[} operators.
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\seealso{
  \code{\link{print.htestCensored}}, \link{Censored Data}.
}
\examples{
  # Create an object of class "htestCensored", then print it out. 
  #--------------------------------------------------------------

  htestCensored.obj <-   with(EPA.09.Ex.16.5.PCE.df, 
    twoSampleLinearRankTestCensored(
      x = PCE.ppb[Well.type == "Compliance"], 
      x.censored = Censored[Well.type == "Compliance"], 
      y = PCE.ppb[Well.type == "Background"], 
      y.censored = Censored[Well.type == "Background"], 
      test = "tarone-ware", alternative = "greater"))

  mode(htestCensored.obj) 
  #[1] "list" 

  class(htestCensored.obj) 
  #[1] "htest" 

  names(htestCensored.obj) 
  # [1] "statistic"         "parameters"        "p.value"          
  # [4] "estimate"          "null.value"        "alternative"      
  # [7] "method"            "estimation.method" "sample.size"      
  #[10] "data.name"         "bad.obs"           "censoring.side"   
  #[13] "censoring.name"    "censoring.levels"  "percent.censored" 
 
  htestCensored.obj 
  
  #Results of Hypothesis Test
  #Based on Censored Data
  #--------------------------
  #
  #Null Hypothesis:                 Fy(t) = Fx(t)
  #
  #Alternative Hypothesis:          Fy(t) > Fx(t) for at least one t
  #
  #Test Name:                       Two-Sample Linear Rank Test:
  #                                 Tarone-Ware Test
  #                                 with Hypergeometric Variance
  #
  #Censoring Side:                  left
  #
  #Data:                            x = PCE.ppb[Well.type == "Compliance"]
  #                                 y = PCE.ppb[Well.type == "Background"]
  #
  #Censoring Variable:              x = Censored[Well.type == "Compliance"]
  #                                 y = Censored[Well.type == "Background"]
  #
  #Sample Sizes:                    nx = 8
  #                                 ny = 6
  #
  #Percent Censored:                x = 12.5%
  #                                 y = 50.0%
  #
  #Test Statistics:                 nu     =  8.458912
  #                                 var.nu = 20.912407
  #                                 z      =  1.849748
  #
  #P-value:                         0.03217495

  #==========

  # Extract the test statistics
  #----------------------------

  htestCensored.obj$statistic
  #       nu    var.nu         z 
  # 8.458912 20.912407  1.849748

  #==========

  # Clean up
  #---------
  rm(htestCensored.obj)
}
\keyword{classes}
