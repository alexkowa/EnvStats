\name{ProUCL.Crit.Vals.for.KS.Test.for.Gamma.array}
\alias{ProUCL.Crit.Vals.for.KS.Test.for.Gamma.array}
\docType{data}
\title{
  ProUCL Critical Values for Kolmogorov-Smirnov Goodness-of-Fit Test for Gamma Distribution
}
\description{
  Critical Values for the Kolmogorov-Smirnov Goodness-of-Fit Test for a Gamma Distribution, as 
  presented in Tables A-2, A-4, and A-6 on pages 284, 286, and 288, respectively, of 
  USEPA (2015).
}
\usage{data("ProUCL.Crit.Vals.for.KS.Test.for.Gamma.array")}
\format{
  An array of dimensions 32 by 11 by 3, with 
  the first dimension indicating the sample size (between 5 and 1000), 
  the second dimension indicating the value of the maximum likelihood estimate 
  of the shape parameter (between 0.025 and 50), 
  and the third dimension indicating the assumed significance level (0.01, 0.05, and 0.10).
}
\details{
  See USEPA (2015, pp.281-282) for more information.  The data in this array are used when 
  the function \code{\link{gofTest}} is called with \code{test="proucl.ks.gamma"}.
}
\source{
  USEPA. (2015).  \emph{ProUCL Version 5.1.002 Technical Guide}.  EPA/600/R-07/041, October 2015.  
  Office of Research and Development. U.S. Environmental Protection Agency, Washington, D.C., 
  pp. 284, 286, and 288.
}
\references{
  USEPA. (2002).  \emph{Estimation of the Exposure Point Concentration Term Using a 
  Gamma Distribution}.  
  EPA/600/R-02/084. October 2002. Technology Support Center for Monitoring and 
  Site Characterization, Office of Research and Development, Office of Solid Waste and 
  Emergency Response, U.S. Environmental Protection Agency, Washington, D.C.

  USEPA. (2015).  \emph{ProUCL Version 5.1.002 Technical Guide}.  EPA/600/R-07/041, October 2015.  
  Office of Research and Development. U.S. Environmental Protection Agency, Washington, D.C.
}
\keyword{datasets}
