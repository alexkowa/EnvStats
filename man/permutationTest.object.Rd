\name{permutationTest.object}
\alias{permutationTest.object}
\docType{class}
\title{
  S3 Class "permutationTest"
}
\description{
  This class of objects is returned by functions that perform permutation tests.  
  Objects of class \code{"permutationTest"} are lists that contain information about 
  the null and alternative hypotheses, the estimated distribution parameters, the 
  test statistic and the p-value.  They also contain the permutation distribution 
  of the statistic (or a sample of the permutation distribution).
}
\details{
  Objects of S3 class \code{"permutationTest"} are returned by any of the 
  \pkg{EnvStats} functions that perform permutation tests.  Currently, these are:  
  \code{\link{oneSamplePermutationTest}},  \cr
  \code{\link{twoSamplePermutationTestLocation}}, and 
  \code{\link{twoSamplePermutationTestProportion}}. 
}
\section{Methods}{
  Generic functions that have methods for objects of class 
  \code{"permutationTest"} include: \cr
  \code{\link{print}}, \code{\link{plot}}.
}
\value{
  A legitimate list of class \code{"permutationTest"} includes the components 
  listed in the help file for \code{\link{htest.object}}.  In addition, the following 
  components must be included in a legitimate list of class \code{"permutationTest"}:

  \strong{Required Components} \cr
  The following components must be included in a legitimate list of 
  class \code{"permutationTest"}.

  \item{stat.dist}{
  numeric vector containing values of the statistic for the permutation distribution.  
  When \code{exact=FALSE}, the vector is comprised of values sampled from the full 
  permutation distribution.
}
  \item{exact}{
  logical scalar indicating whether the exact permutation distribution was used for 
  the test (\code{exact=TRUE}), or if instead the permutation distribution was 
  sampled (\code{exact=FALSE}).
}

  \strong{Optional Components} \cr
  The following component may optionally be included in an object of 
  of class \code{"permutationTest"}:

  \item{seed}{
  integer or vector of integers indicating the seed that was used for sampling the 
  permutation distribution.  This component is present only if \code{exact=FALSE}.
} 
  \item{prob.stat.dist}{
  numeric vector containing the probabilities associated with each element of 
  the component \code{stat.dist}.  This component is only returned by the 
  function \code{\link{twoSamplePermutationTestProportion}}.
}
}
\note{
  Since objects of class \code{"permutationTest"} are lists, you may extract 
  their components with the \code{$} and \code{[[} operators.
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\seealso{
  \code{\link{print.permutationTest}}, \code{\link{plot.permutationTest}}, 
  \code{\link{oneSamplePermutationTest}},  \cr
  \code{\link{twoSamplePermutationTestLocation}},  
  \code{\link{twoSamplePermutationTestProportion}}, \link{Hypothesis Tests}.
}
\examples{
  # Create an object of class "permutationTest", then print it and plot it. 
  #------------------------------------------------------------------------

  set.seed(23) 

  dat <- rlogis(10, location = 7, scale = 2) 

  permutationTest.obj <- oneSamplePermutationTest(dat, mu = 5, 
    alternative = "greater", exact = TRUE) 

  mode(permutationTest.obj) 
  #[1] "list" 

  class(permutationTest.obj) 
  #[1] "permutationTest" 

  names(permutationTest.obj) 
  # [1] "statistic"         "parameters"        "p.value"          
  # [4] "estimate"          "null.value"        "alternative"      
  # [7] "method"            "estimation.method" "sample.size"      
  #[10] "data.name"         "bad.obs"           "stat.dist"        
  #[13] "exact" 

  #==========

  # Print the results of the test 
  #------------------------------
  permutationTest.obj 

  #Results of Hypothesis Test
  #--------------------------
  #
  #Null Hypothesis:                 Mean (Median) = 5
  #
  #Alternative Hypothesis:          True Mean (Median) is greater than 5
  #
  #Test Name:                       One-Sample Permutation Test
  #                                 (Exact)
  #
  #Estimated Parameter(s):          Mean = 9.977294
  #
  #Data:                            dat
  #
  #Sample Size:                     10
  #
  #Test Statistic:                  Sum(x - 5) = 49.77294
  #
  #P-value:                         0.001953125

  #==========

  # Plot the results of the test 
  #-----------------------------
  dev.new()
  plot(permutationTest.obj)

  #==========

  # Extract the test statistic
  #---------------------------

  permutationTest.obj$statistic
  #Sum(x - 5) 
  #  49.77294

  #==========

  # Clean up
  #---------
  rm(permutationTest.obj)
  graphics.off()
}
\keyword{classes}
