\name{FcnsByCatGOFTests}
\alias{FcnsByCatGOFTests}
\alias{Goodness-of-Fit Tests}
\alias{EnvStats Functions for Goodness-of-Fit Tests}
\alias{GOF}
\title{
  EnvStats Functions for Goodness-of-Fit Tests
}
\description{
  The \pkg{EnvStats} functions listed below are useful for performing goodness-of-fit tests for 
  user-specified probability distributions.
}
\details{
  \bold{Goodness-of-Fit Tests}

  \tabular{ll}{
  \emph{Function Name}                    \tab \emph{Description} \cr
  \code{\link{gofTest}}                   \tab Perform a goodness-of-fit test for a specified probability distribution. \cr
                                          \tab The resulting object is of class \code{\link[=gof.object]{"gof"}} unless the test is the \cr
                                          \tab two-sample Kolmogorov-Smirnov test, in which case the resulting \cr 
                                          \tab object is of class \code{\link[=gofTwoSample.object]{"gofTwoSample"}}. \cr
  \code{\link{plot.gof}}                  \tab S3 class method for plotting an object of class \code{\link[=gof.object]{"gof"}}. \cr
  \code{\link{print.gof}}                 \tab S3 class method for printing an object of class \code{\link[=gof.object]{"gof"}}. \cr
  \code{\link{plot.gofTwoSample}}         \tab S3 class method for plotting an object of class \code{\link[=gofTwoSample.object]{"gofTwoSample"}}. \cr
  \code{\link{print.gofTwoSample}}        \tab S3 class method for printing an object of class \code{\link[=gofTwoSample.object]{"gofTwoSample"}}. \cr
  \code{\link{gofGroupTest}}              \tab Perform a goodness-of-fit test to determine whether data in a set of groups \cr
                                          \tab appear to all come from the same probability distribution \cr
                                          \tab (with possibly different parameters for each group). \cr
                                          \tab The resulting object is of class \code{\link[=gofGroup.object]{"gofGroup"}}. \cr
  \code{\link{plot.gofGroup}}             \tab S3 class method for plotting an object of class \code{\link[=gofGroup.object]{"gofGroup"}}. \cr
  \code{\link{print.gofGroup}}            \tab S3 class method for printing an object of class \code{\link[=gofGroup.object]{"gofGroup"}}. \cr
  }


  \bold{Tests for Outliers}

  \tabular{ll}{
  \emph{Function Name}                    \tab \emph{Description} \cr
  \code{\link{rosnerTest}}                \tab Perform Rosner's test for outliers assuming a normal (Gaussian) distribution. \cr
  \code{\link{print.gofOutlier}}          \tab S3 class method for printing an object of class \code{\link[=gofOutlier.object]{"gofOutlier"}}. \cr
  }


  \bold{Choose a Distribution}

  \tabular{ll}{
  \emph{Function Name}                    \tab \emph{Description} \cr
  \code{\link{distChoose}}                \tab Choose best fitting distribution based on goodness-of-fit tests. \cr
  \code{\link{print.distChoose}}          \tab S3 class method for printing an object of class \code{\link[=distChoose.object]{"distChoose"}}. \cr
  }


}
\keyword{ package }


