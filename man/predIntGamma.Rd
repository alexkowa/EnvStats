\name{predIntGamma}
\alias{predIntGamma}
\alias{predIntGammaAlt}
\title{
  Prediction Interval for Gamma Distribution
}
\description{
  Construct a prediction interval for the next \eqn{k} observations or 
  next set of \eqn{k} transformed means for a gamma distribution.
}
\usage{
  predIntGamma(x, n.transmean = 1, k = 1, method = "Bonferroni", 
    pi.type = "two-sided", conf.level = 0.95, est.method = "mle", 
    normal.approx.transform = "kulkarni.powar")

  predIntGammaAlt(x, n.transmean = 1, k = 1, method = "Bonferroni", 
    pi.type = "two-sided", conf.level = 0.95, est.method = "mle",
    normal.approx.transform = "kulkarni.powar")
}
\arguments{
  \item{x}{
  numeric vector of non-negative observations. Missing (\code{NA}), undefined (\code{NaN}), and 
  infinite (\code{Inf}, \code{-Inf}) values are allowed but will be removed.
}
  \item{n.transmean}{
  positive integer specifying the sample size associated with the \code{k} 
  future transformed means (see the DETAILS section for an explanation of 
  what the transformation is).  The default value is \code{n.transmean=1} 
  (i.e., predicting future observations).  Note that all future 
  transformed means must be based on the same sample size. 
}
  \item{k}{
  positive integer specifying the number of future observations or means 
  the prediction interval should contain with confidence level \code{conf.level}.  
  The default value is \code{k=1}.
}
  \item{method}{
  character string specifying the method to use if the number of future observations 
  or averages (\code{k}) is greater than 1.  The possible values are \code{"Bonferroni"} 
  (approximate method based on Bonferonni inequality; the default), and \cr
  \code{"exact"} (exact method due to Dunnett, 1955).  See the DETAILS section for more information.  
  This argument is ignored if \code{k=1}.
}
  \item{pi.type}{
  character string indicating what kind of prediction interval to compute.  
  The possible values are \code{"two-sided"} (the default), \code{"lower"}, and 
  \code{"upper"}.
}
  \item{conf.level}{
  a scalar between 0 and 1 indicating the confidence level associated with the prediction 
  interval.  The default value is \code{conf.level=0.95}.
}
  \item{est.method}{
  character string specifying the method of estimation for the shape and scale 
  distribution parameters.  The possible values are 
  \code{"mle"} (maximum likelihood; the default), 
  \code{"bcmle"} (bias-corrected mle), \code{"mme"} (method of moments), and 
  \code{"mmue"} (method of moments based on the unbiased estimator of variance). 
  See the DETAILS section of the help file for \code{\link{egamma}} for more information.
}
  \item{normal.approx.transform}{
  character string indicating which power transformation to use.  
  Possible values are \code{"kulkarni.powar"} (the default), \code{"cube.root"}, and \cr
  \code{"fourth.root"}.  See the DETAILS section for more informaiton.
}
}
\details{
  If \code{x} contains any missing (\code{NA}), undefined (\code{NaN}) or 
  infinite (\code{Inf}, \code{-Inf}) values, they will be removed prior to 
  performing the estimation.

  The function \code{predIntGamma} returns a prediction interval as well as 
  estimates of the shape and scale parameters.  
  The function \code{predIntGammaAlt} returns a prediction interval as well as 
  estimates of the mean and coefficient of variation.

  Following Krishnamoorthy et al. (2008), the prediction interval is 
  computed by:
  \enumerate{
  \item using a power transformation on the original data to induce approximate 
    normality, 
  \item calling \code{\link{predIntNorm}} with the transformed data to 
    compute the prediction interval, and then 
  \item back-transforming the interval to create a prediction interval 
    on the original scale.  
  }
  The argument \code{normal.approx.transform} determines which transformation is used. 
  The value \code{normal.approx.transform="cube.root"} uses 
  the cube root transformation suggested by Wilson and Hilferty (1931) and used by 
  Krishnamoorthy et al. (2008) and Singh et al. (2010b), and the value 
  \code{normal.approx.transform="fourth.root"} uses the fourth root transformation suggested 
  by Hawkins and Wixley (1986) and used by Singh et al. (2010b).  
  The default value \code{normal.approx.transform="kulkarni.powar"} 
  uses the "Optimum Power Normal Approximation Method" of Kulkarni and Powar (2010).  
  The "optimum" power \eqn{p} is determined by:
  \tabular{ll}{
  \eqn{p = -0.0705 - 0.178 \, shape + 0.475 \, \sqrt{shape}} \tab if \eqn{shape \le 1.5} \cr
  \eqn{p = 0.246} \tab if \eqn{shape > 1.5} \cr
  }
  where \eqn{shape} denotes the estimate of the shape parameter.  Although 
  Kulkarni and Powar (2010) use the maximum likelihood estimate of shape to 
  determine the power \eqn{p}, for the functions \cr
  \code{predIntGamma} and \code{predIntGammaAlt} the power \eqn{p} is based on whatever estimate of 
  shape is used (e.g., \code{est.method="mle"}, \code{est.method="bcmle"}, etc.).

  When the argument \code{n.transmean} is larger than 1 (i.e., you are 
  constructing a prediction interval for future means, not just single 
  observations), in order to properly compare a future mean with the 
  prediction limits, you must follow these steps:
  \enumerate{
    \item Take the observations that will be used to compute the mean and 
          transform them by raising them to the power given by the value in the component 
          \code{interval$normal.transform.power} (see the section VALUE below).
    \item Compute the mean of the transformed observations.
    \item Take the mean computed in step 2 above and raise it to the inverse of the power 
          originally used to transform the observations.
  }
}
\value{
  A list of class \code{"estimate"} containing the estimated parameters, 
  the prediction interval, and other information.  See \code{\link{estimate.object}} 
  for details.  

  In addition to the usual components contained in an object of class 
  \code{"estimate"}, the returned value also includes two additional 
  components within the \code{"interval"} component:

  \item{n.transmean}{the value of \code{n.transmean} supplied in the 
    call to \code{predIntGamma} or \code{predIntGammaAlt}.}
  \item{normal.transform.power}{the value of the power used to 
    transform the original data to approximate normality.}  
}
\references{
  Berthouex, P.M., and L.C. Brown. (2002). \emph{Statistics for Environmental Engineers}. 
  Lewis Publishers, Boca Raton.

  Draper, N., and H. Smith. (1998). \emph{Applied Regression Analysis}. Third Edition. 
  John Wiley and Sons, New York.

  Evans, M., N. Hastings, and B. Peacock. (1993). \emph{Statistical Distributions}. 
  Second Edition. John Wiley and Sons, New York, Chapter 18.

  Gibbons, R.D., D.K. Bhaumik, and S. Aryal. (2009). 
  \emph{Statistical Methods for Groundwater Monitoring}, Second Edition.  
  John Wiley & Sons, Hoboken.

  Hawkins, D. M., and R.A.J. Wixley. (1986). A Note on the Transformation of 
  Chi-Squared Variables to Normality. \emph{The American Statistician}, \bold{40}, 
  296--298.

  Johnson, N.L., S. Kotz, and N. Balakrishnan. (1994). 
  \emph{Continuous Univariate Distributions, Volume 1}. Second Edition. 
  John Wiley and Sons, New York, Chapter 17.

  Krishnamoorthy K., T. Mathew, and S. Mukherjee. (2008). Normal-Based Methods for a 
  Gamma Distribution: Prediction and Tolerance Intervals and Stress-Strength Reliability. 
  \emph{Technometrics}, \bold{50}(1), 69--78.

  Krishnamoorthy K., and T. Mathew. (2009). 
  \emph{Statistical Tolerance Regions: Theory, Applications, and Computation}. 
  John Wiley and Sons, Hoboken.

  Kulkarni, H.V., and S.K. Powar. (2010). A New Method for Interval Estimation of the Mean 
  of the Gamma Distribution. \emph{Lifetime Data Analysis}, \bold{16}, 431--447.

  Millard, S.P., and N.K. Neerchal. (2001). \emph{Environmental Statistics with S-PLUS}. 
  CRC Press, Boca Raton.

  Singh, A., A.K. Singh, and R.J. Iaci. (2002). 
  \emph{Estimation of the Exposure Point Concentration Term Using a Gamma Distribution}.  
  EPA/600/R-02/084. October 2002. Technology Support Center for Monitoring and 
  Site Characterization, Office of Research and Development, Office of Solid Waste and 
  Emergency Response, U.S. Environmental Protection Agency, Washington, D.C.

  Singh, A., R. Maichle, and N. Armbya. (2010a). 
  \emph{ProUCL Version 4.1.00 User Guide (Draft)}. EPA/600/R-07/041, May 2010. 
  Office of Research and Development, U.S. Environmental Protection Agency, Washington, D.C.

  Singh, A., N. Armbya, and A. Singh. (2010b). 
  \emph{ProUCL Version 4.1.00 Technical Guide (Draft)}. EPA/600/R-07/041, May 2010.  
  Office of Research and Development, U.S. Environmental Protection Agency, Washington, D.C.

  Wilson, E.B., and M.M. Hilferty. (1931). The Distribution of Chi-Squares. 
  \emph{Proceedings of the National Academy of Sciences}, \bold{17}, 684--688.

  USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
  EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.  
  U.S. Environmental Protection Agency, Washington, D.C.

  USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
  EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
  U.S. Environmental Protection Agency, Washington, D.C. 

  Zar, J.H. (2010). \emph{Biostatistical Analysis}. Fifth Edition. 
  Prentice-Hall, Upper Saddle River, New Jersey.
}
\author{
    Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  The \link[stats:GammaDist]{gamma distribution} takes values on the positive real line. 
  Special cases of the gamma are the \link[=Exponential]{exponential} distribution and 
  the \link[=Chisquare]{chi-square} distributions. Applications of the gamma include 
  life testing, statistical ecology, queuing theory, inventory control, and precipitation 
  processes. A gamma distribution starts to resemble a normal distribution as the 
  shape parameter a tends to infinity.

  Some EPA guidance documents (e.g., Singh et al., 2002; Singh et al., 2010a,b) strongly 
  recommend against using a lognormal model for environmental data and recommend trying a 
  gamma distribuiton instead.

  Prediction intervals have long been applied to quality control and 
  life testing problems (Hahn, 1970b,c; Hahn and Nelson, 1973), and are often discussed 
  in the context of linear regression (Draper and Smith, 1998; Zar, 2010).  Prediction 
  intervals are useful for analyzing data from groundwater detection monitoring programs 
  at hazardous and solid waste facilities.  References that discuss prediction intervals 
  in the context of environmental monitoring include:  Berthouex and Brown (2002, Chapter 21), 
  Gibbons et al. (2009), Millard and Neerchal (2001, Chapter 6), Singh et al. (2010b), 
  and USEPA (2009).
}
\section{Warning}{
  It is possible for the lower prediction limit based on the transformed data to be less than 0.  
  In this case, the lower prediction limit on the original scale is set to 0 and a warning is 
  issued stating that the normal approximation is not accurate in this case.
}
\seealso{
  \code{\link{GammaDist}}, \code{\link{GammaAlt}}, \code{\link{estimate.object}}, 
  \code{\link{egamma}}, \code{\link{predIntNorm}}, 
  \code{\link{tolIntGamma}}.
}
\examples{
  # Generate 20 observations from a gamma distribution with parameters 
  # shape=3 and scale=2, then create a prediciton interval for the 
  # next observation. 
  # (Note: the call to set.seed simply allows you to reproduce this 
  # example.)

  set.seed(250) 
  dat <- rgamma(20, shape = 3, scale = 2) 
  predIntGamma(dat)

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            Gamma
  #
  #Estimated Parameter(s):          shape = 2.203862
  #                                 scale = 2.174928
  #
  #Estimation Method:               mle
  #
  #Data:                            dat
  #
  #Sample Size:                     20
  #
  #Prediction Interval Method:      exact using
  #                                 Kulkarni & Powar (2010)
  #                                 transformation to Normality
  #                                 based on mle of 'shape'
  #
  #Normal Transform Power:          0.246
  #
  #Prediction Interval Type:        two-sided
  #
  #Confidence Level:                95%
  #
  #Number of Future Observations:   1
  #
  #Prediction Interval:             LPL =  0.5371569
  #                                 UPL = 15.5313783

  #--------------------------------------------------------------------

  # Using the same data as in the previous example, create an upper 
  # one-sided prediction interval for the next three averages of 
  # order 2 (i.e., each mean is based on 2 future observations), and 
  # use the bias-corrected estimate of shape.

  pred.list <- predIntGamma(dat, n.transmean = 2, k = 3, 
    pi.type = "upper", est.method = "bcmle")

  pred.list

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            Gamma
  #
  #Estimated Parameter(s):          shape = 1.906616
  #                                 scale = 2.514005
  #
  #Estimation Method:               bcmle
  #
  #Data:                            dat
  #
  #Sample Size:                     20
  #
  #Prediction Interval Method:      Bonferroni using
  #                                 Kulkarni & Powar (2010)
  #                                 transformation to Normality
  #                                 based on bcmle of 'shape'
  #
  #Normal Transform Power:          0.246
  #
  #Prediction Interval Type:        upper
  #
  #Confidence Level:                95%
  #
  #Number of Future
  #Transformed Means:               3
  #
  #Sample Size for
  #Transformed Means:               2
  #
  #Prediction Interval:             LPL =  0.00000
  #                                 UPL = 12.17404

  #--------------------------------------------------------------------

  # Continuing with the above example, assume the distribution shifts 
  # in the future to a gamma distribution with shape = 5 and scale = 2.
  # Create 6 future observations from this distribution, and create 3 
  # means by pairing the observations sequentially.  Note we must first 
  # transform these observations using the power 0.246, then compute the 
  # means based on the transformed data, and then transform the means 
  # back to the original scale and compare them to the upper prediction 
  # limit of 12.17

  set.seed(427)
  new.dat <- rgamma(6, shape = 5, scale = 2)

  p <- pred.list$interval$normal.transform.power
  p
  #[1] 0.246
 
  new.dat.trans <- new.dat^p
  means.trans <- c(mean(new.dat.trans[1:2]), mean(new.dat.trans[3:4]), 
    mean(new.dat.trans[5:6]))
  means <- means.trans^(1/p)
  means
  #[1] 11.74214 17.05299 11.65272

  any(means > pred.list$interval$limits["UPL"])
  #[1] TRUE

  #----------

  # Clean up
  rm(dat, pred.list, new.dat, p, new.dat.trans, means.trans, means)

  #--------------------------------------------------------------------

  # Reproduce part of the example on page 73 of 
  # Krishnamoorthy et al. (2008), which uses alkalinity concentrations 
  # reported in Gibbons (1994) and Gibbons et al. (2009) to construct a 
  # one-sided upper 90% prediction limit.

  predIntGamma(Gibbons.et.al.09.Alkilinity.vec, pi.type = "upper", 
    conf.level = 0.9, normal.approx.transform = "cube.root")

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            Gamma
  #
  #Estimated Parameter(s):          shape = 9.375013
  #                                 scale = 6.202461
  #
  #Estimation Method:               mle
  #
  #Data:                            Gibbons.et.al.09.Alkilinity.vec
  #
  #Sample Size:                     27
  #
  #Prediction Interval Method:      exact using
  #                                 Wilson & Hilferty (1931) cube-root
  #                                 transformation to Normality
  #
  #Normal Transform Power:          0.3333333
  #
  #Prediction Interval Type:        upper
  #
  #Confidence Level:                90%
  #
  #Number of Future Observations:   1
  #
  #Prediction Interval:             LPL =  0.0000
  #                                 UPL = 85.3495
}
\keyword{ distribution }
\keyword{ htest }
