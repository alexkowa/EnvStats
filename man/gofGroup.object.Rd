\name{gofGroup.object}
\alias{gofGroup.object}
\alias{gofGroup}
\docType{class}
\title{
  S3 Class "gofGroup"
}
\description{
  Objects of S3 class \code{"gofGroup"} are returned by the \pkg{EnvStats} function 
  \code{\link{gofGroupTest}}.
}
\details{
  Objects of S3 class \code{"gofGroup"} are lists that contain 
  information about the assumed distribution, the estimated or 
  user-supplied distribution parameters, and the test statistic 
  and p-value.
}
\section{Methods}{
  Generic functions that have methods for objects of class 
  \code{"gofGroup"} include: \cr
  \code{\link{print}}, \code{\link{plot}}.
}
\value{
  \strong{Required Components} \cr
  The following components must be included in a legitimate list of 
  class \code{"gofGroup"}.

  \item{distribution}{a character string indicating the name of the 
    assumed distribution (see \cr
    \code{\link{Distribution.df}}).}
  \item{dist.abb}{a character string containing the abbreviated name 
    of the distribution (see \cr
    \code{\link{Distribution.df}}).}
  \item{statistic}{a numeric scalar with a names attribute containing 
    the name and value of the goodness-of-fit statistic.}
  \item{sample.size}{a numeric scalar containing the number 
    of non-missing observations in the sample used for the 
    goodness-of-fit test.}
  \item{parameters}{numeric vector with a names attribute containing 
    the name(s) and value(s) of the parameter(s) associated with the 
    test statistic given in the \code{statistic} component.}
  \item{p.value}{numeric scalar containing the p-value associated with 
    the goodness-of-fit statistic.}
  \item{alternative}{character string indicating the alternative hypothesis.}
  \item{method}{character string indicating the name of the 
    goodness-of-fit test (e.g., \cr
    \code{"Wilk-Shapiro GOF (Normal Scores)"}).}
  \item{data.name}{character string indicating the name of the data 
    object used for the goodness-of-fit test.}
  \item{grouping.variable}{character string indicating the name of the variable 
    defining the groups.}
  \item{bad.obs}{numeric vector indicating the number of missing (\code{NA}), 
    undefined (\code{NaN}) and/or infinite (\code{Inf}, \code{-Inf}) 
    values that were removed from each group and the grouping variable 
    prior to performing the goodness-of-fit test.}
  \item{n.groups}{numeric scalar containing the number of groups.}
  \item{group.names}{character vector containing the levels of the grouping 
    variable, i.e., the names of each of the groups.}
  \item{group.scores}{numeric vector containing the individual statistics for 
    each group.}

  \strong{Optional Component} \cr
  The following component is included when \code{\link{gofGroupTest}} is 
  called with a formula for the first argument and a \code{data} argument.

  \item{parent.of.data}{character string indicating the name of the object supplied 
    in the \code{data} argument.}
}
\note{
  Since objects of class \code{"gofGroup"} are lists, you may extract 
  their components with the \code{$} and \code{[[} operators.
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\seealso{
  \code{\link{gofGroupTest}}, \code{\link{print.gofGroup}}, \code{\link{plot.gofGroup}}, 
  \link[=FcnsByCatGOFTests]{Goodness-of-Fit Tests}, 
  \code{\link{Distribution.df}}.
}
\examples{
  # Create an object of class "gofGroup", then print it out. 

  # Example 10-4 of USEPA (2009, page 10-20) gives an example of 
  # simultaneously testing the assumption of normality for nickel 
  # concentrations (ppb) in groundwater collected at 4 monitoring 
  # wells over 5 months.  The data for this example are stored in 
  # EPA.09.Ex.10.1.nickel.df.

  gofGroup.obj <- gofGroupTest(Nickel.ppb ~ Well, 
    data = EPA.09.Ex.10.1.nickel.df)

  mode(gofGroup.obj) 
  #[1] "list" 

  class(gofGroup.obj) 
  #[1] "gofGroup" 

  names(gofGroup.obj) 
  # [1] "distribution"      "dist.abb"          "statistic"        
  # [4] "sample.size"       "parameters"        "p.value"          
  # [7] "alternative"       "method"            "data.name"        
  #[10] "grouping.variable" "parent.of.data"    "bad.obs"          
  #[13] "n.groups"          "group.names"       "group.scores"

  gofGroup.obj 
  #Results of Group Goodness-of-Fit Test
  #-------------------------------------
  #
  #Test Method:                     Wilk-Shapiro GOF (Normal Scores)
  #
  #Hypothesized Distribution:       Normal
  #
  #Data:                            Nickel.ppb
  #
  #Grouping Variable:               Well
  #
  #Data Source:                     EPA.09.Ex.10.1.nickel.df
  #
  #Number of Groups:                4
  #
  #Sample Sizes:                    Well.1 = 5
  #                                 Well.2 = 5
  #                                 Well.3 = 5
  #                                 Well.4 = 5
  #
  #Test Statistic:                  z (G) = -3.658696
  #
  #P-values for
  #Individual Tests:                Well.1 = 0.03510747
  #                                 Well.2 = 0.02385344
  #                                 Well.3 = 0.01120775
  #                                 Well.4 = 0.10681461
  #
  #P-value for
  #Group Test:                      0.0001267509
  #
  #Alternative Hypothesis:          At least one group
  #                                 does not come from a
  #                                 Normal Distribution.  

  #==========

  # Extract the p-values
  #---------------------

  gofGroup.obj$p.value
  #      Well.1       Well.2       Well.3       Well.4        z (G) 
  #0.0351074733 0.0238534406 0.0112077511 0.1068146088 0.0001267509 

  #==========

  # Plot the results of the test
  #-----------------------------

  dev.new()
  plot(gofGroup.obj)

  #==========

  # Clean up
  #---------
  rm(gofGroup.obj)
  graphics.off()
}
\keyword{classes}
