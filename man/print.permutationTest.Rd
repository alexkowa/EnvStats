\name{print.permutationTest}
\alias{print.permutationTest}
\title{
  Print Output of Permutation Tests
}
\description{
  Formats and prints the results of performing a permutation test.  This method is 
  automatically called by \code{\link[base]{print}} when given an object of class 
  \code{"permutationTest"}.  Currently, the \pkg{EnvStats} functions that perform 
  permutation tests and produce objects of class \code{"permutationTest"} are:
  \code{\link{oneSamplePermutationTest}},  
  \code{\link{twoSamplePermutationTestLocation}}, and \cr
  \code{\link{twoSamplePermutationTestProportion}}. 
}
\usage{
  \method{print}{permutationTest}(x, ...)
}
\arguments{
  \item{x}{
  an object of class \code{"permutationTest"}.  
  See \code{\link{permutationTest.object}} for details.
}
  \item{\dots}{
  arguments that can be supplied to the \code{\link[base]{format}} function.
}
}
\details{
  This is the \code{"permutationTest"} method for the generic function 
  \code{\link[base]{print}}.  Prints null and alternative hypotheses, 
  name of the test, estimated population 
  parameter(s) involved in the null hypothesis, estimation method (if present), 
  data name, sample size (if present), number of missing observations removed 
  prior to performing the test (if present), value of the test statistic, 
  parameters associated with the null distribution of the test statistic, 
  and p-value associated with the test statistic.
}
\value{
  Invisibly returns the input \code{x}.
}
\references{
  Chambers, J. M. and Hastie, T. J. (1992).  \emph{Statistical Models in S}.  
  Wadsworth & Brooks/Cole. 
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\seealso{
  \code{\link{permutationTest.object}}, \code{\link{oneSamplePermutationTest}},  
  \code{\link{twoSamplePermutationTestLocation}},  
  \code{\link{twoSamplePermutationTestProportion}}, \link{Hypothesis Tests}, 
  \code{\link[base]{print}}.
}
\keyword{print}