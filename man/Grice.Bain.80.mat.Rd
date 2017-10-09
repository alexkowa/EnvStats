\name{Grice.Bain.80.mat}
\alias{Grice.Bain.80.mat}
\docType{data}
\title{
  Adjusted Alpha Levels to Compute Confidence Intervals for the Mean of a Gamma Distribution
}
\description{
  Adjusted alpha levels to compute confidence intervals for the mean of a gamma distribution, as 
  presented in Table 2 of Grice and Bain (1980).
}
\usage{data("Grice.Bain.80.mat")}
\format{
  A matrix of dimensions 5 by 7, with 
  the first dimension indicating the sample size (between 5 and Inf), 
  and the second dimension indicating the assumed significance level 
  associated with the confidence interval (between 0.005 and 0.25).
  The assumed confidence level is 1 - assumed significance level.
}
\details{
  See Grice and Bain (1980) and the help file for \code{\link{egamma}} 
  for more information.  The data in this matrix are used when 
  the function \code{\link{egamma}} is called with \code{ci.method="chisq.adj"}.
}
\source{
  Grice, J.V., and L.J. Bain. (1980).  Inferences Concerning the Mean of the Gamma Distribution.  
  \emph{Journal of the American Statistical Association} \bold{75}, 929-933.
}
\references{
  Grice, J.V., and L.J. Bain. (1980).  Inferences Concerning the Mean of the Gamma Distribution.  
  \emph{Journal of the American Statistical Association} \bold{75}, 929-933.

  USEPA. (2002).  \emph{Estimation of the Exposure Point Concentration Term Using a 
  Gamma Distribution}.  
  EPA/600/R-02/084. October 2002. Technology Support Center for Monitoring and 
  Site Characterization, Office of Research and Development, Office of Solid Waste and 
  Emergency Response, U.S. Environmental Protection Agency, Washington, D.C.

  USEPA. (2015).  \emph{ProUCL Version 5.1.002 Technical Guide}.  EPA/600/R-07/041, October 2015.  
  Office of Research and Development. U.S. Environmental Protection Agency, Washington, D.C.
}
\examples{
  # Look at Grice.Bain.80.mat

  Grice.Bain.80.mat
  #         alpha.eq.005 alpha.eq.01 alpha.eq.025 alpha.eq.05 alpha.eq.075
  #n.eq.5         0.0000      0.0000       0.0010      0.0086       0.0234
  #n.eq.10        0.0003      0.0015       0.0086      0.0267       0.0486
  #n.eq.20        0.0017      0.0046       0.0159      0.0380       0.0619
  #n.eq.40        0.0030      0.0070       0.0203      0.0440       0.0685
  #n.eq.Inf       0.0050      0.0100       0.0250      0.0500       0.0750

  #         alpha.eq.10 alpha.eq.25
  #n.eq.5        0.0432      0.2038
  #n.eq.10       0.0724      0.2294
  #n.eq.20       0.0866      0.2403
  #n.eq.40       0.0934      0.2453
  #n.eq.Inf      0.1000      0.2500
}
\keyword{datasets}
