\name{gof.object}
\alias{gof.object}
\alias{gof}
\docType{class}
\title{
  S3 Class "gof"
}
\description{
  Objects of S3 class \code{"gof"} are returned by the \pkg{EnvStats} function 
  \code{\link{gofTest}} when just the \code{x} argument is supplied.
}
\details{
  Objects of S3 class \code{"gof"} are lists that contain 
  information about the assumed distribution, the estimated or 
  user-supplied distribution parameters, and the test statistic 
  and p-value.
}
\section{Methods}{
  Generic functions that have methods for objects of class 
  \code{"gof"} include: \cr
  \code{\link{print}}, \code{\link{plot}}.
}
\value{
  \strong{Required Components} \cr
  The following components must be included in a legitimate list of 
  class \code{"gof"}.

  \item{distribution}{a character string indicating the name of the 
    assumed distribution (see \cr
    \code{\link{Distribution.df}}).}
  \item{dist.abb}{a character string containing the abbreviated name 
    of the distribution (see \cr
    \code{\link{Distribution.df}}).}
  \item{distribution.parameters}{a numeric vector 
    with a names attribute containing the names and values of the 
    estimated or user-supplied distribution parameters associated 
    with the assumed distribution.}
  \item{n.param.est}{a scalar indicating the number of distribution 
    parameters estimated prior to performing the goodness-of-fit 
    test. The value of this component will be \code{0} if the parameters 
    were supplied by the user.}
  \item{estimation.method}{a character string indicating the method 
    used to compute the estimated parameters.  The value of this 
    component will depend on the available estimation methods 
    (see \code{\link{Distribution.df}}).  The value of this component 
    will be \code{NULL} if the parameters were supplied by the user.}
  \item{statistic}{a numeric scalar with a names attribute containing 
    the name and value of the goodness-of-fit statistic.}
  \item{sample.size}{a numeric scalar containing the number 
    of non-missing observations in the sample used for the 
    goodness-of-fit test.}
  \item{parameters}{numeric vector with a names attribute containing 
    the name(s) and value(s) of the parameter(s) associated with the 
    test statistic given in the \code{statistic} component.}
  \item{z.value}{(except when \code{test="chisq"} or \code{test="ks"}) 
    numeric scalar containing the z-value associated with the 
    goodness-of-fit statistic.}
  \item{p.value}{numeric scalar containing the p-value associated with 
    the goodness-of-fit statistic.}
  \item{alternative}{character string indicating the alternative hypothesis.}
  \item{method}{character string indicating the name of the 
    goodness-of-fit test (e.g., \cr
    \code{"Shapiro-Wilk GOF"}).}
  \item{data}{numeric vector containing the data actually used for 
    the goodness-of-fit test (i.e., the original data without any 
    missing or infinite values).}
  \item{data.name}{character string indicating the name of the data 
    object used for the goodness-of-fit test.}
  \item{bad.obs}{numeric scalar indicating the number of missing (\code{NA}), 
    undefined (\code{NaN}) and/or infinite (\code{Inf}, \code{-Inf}) 
    values that were removed from the data object prior to performing 
    the goodness-of-fit test.}
  \cr

  \emph{NOTE}: when the function \code{\link{gofTest}} is called with 
  both arguments \code{x} and \code{y} and \code{test="ks"}, it 
  returns an object of class \code{\link[=gofTwoSample.object]{"gofTwoSample"}}.  
  No specific parametric distribution is assumed, so the value of the component 
  \code{distribution} is \code{"Equal"} and the following components 
  are omitted: \code{dist.abb}, \code{distribution.parameters}, 
  \code{n.param.est}, \code{estimation.method}, and \code{z.value}. \cr

  \strong{Optional Components} \cr
  The following components are included in the result of 
  calling \code{\link{gofTest}} with the argument \cr
  \code{test="chisq"} and may be used by the function 
  \code{\link{plot.gof}}:

  \item{cut.points}{numeric vector containing the cutpoints used to 
    define the cells.}
  \item{counts}{numeric vector containing the observed number of 
    counts for each cell.}
  \item{expected}{numeric vector containing the expected number of 
    counts for each cell.}
  \item{X2.components}{numeric vector containing the contribution of 
    each cell to the chi-square statistic.}
}
\note{
  Since objects of class \code{"gof"} are lists, you may extract 
  their components with the \code{$} and \code{[[} operators.
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\seealso{
  \code{\link{gofTest}}, \code{\link{print.gof}}, \code{\link{plot.gof}}, 
  \link[=FcnsByCatGOFTests]{Goodness-of-Fit Tests}, 
  \code{\link{Distribution.df}}, \code{\link{gofCensored.object}}.
}
\examples{
  # Create an object of class "gof", then print it out. 
  # (Note: the call to set.seed simply allows you to reproduce 
  # this example.)

  set.seed(250) 

  dat <- rnorm(20, mean = 3, sd = 2) 

  gof.obj <- gofTest(dat) 

  mode(gof.obj) 
  #[1] "list" 

  class(gof.obj) 
  #[1] "gof" 

  names(gof.obj) 
  # [1] "distribution"            "dist.abb"               
  # [3] "distribution.parameters" "n.param.est"            
  # [5] "estimation.method"       "statistic"              
  # [7] "sample.size"             "parameters"             
  # [9] "z.value"                 "p.value"                
  #[11] "alternative"             "method"                 
  #[13] "data"                    "data.name"              
  #[15] "bad.obs" 

  gof.obj 
  
  #Results of Goodness-of-Fit Test
  #-------------------------------
  #
  #Test Method:                     Shapiro-Wilk GOF
  #
  #Hypothesized Distribution:       Normal
  #
  #Estimated Parameter(s):          mean = 2.861160
  #                                 sd   = 1.180226
  #
  #Estimation Method:               mvue
  #
  #Data:                            dat
  #
  #Sample Size:                     20
  #
  #Test Statistic:                  W = 0.9640724
  #
  #Test Statistic Parameter:        n = 20
  #
  #P-value:                         0.6279872
  #
  #Alternative Hypothesis:          True cdf does not equal the
  #                                 Normal Distribution.

  #==========

  # Extract the p-value
  #--------------------

  gof.obj$p.value
  #[1] 0.6279872

  #==========

  # Plot the results of the test
  #-----------------------------

  dev.new()
  plot(gof.obj)

  #==========

  # Clean up
  #---------
  rm(dat, gof.obj)
  graphics.off()
}
\keyword{classes}
