\name{tolIntGamma}
\alias{tolIntGamma}
\alias{tolIntGammaAlt}
\title{
  Tolerance Interval for a Gamma Distribution
}
\description{
  Construct a \eqn{\beta}-content or \eqn{\beta}-expectation tolerance 
  interval for a \link[stats:GammaDist]{gamma distribution}.
}
\usage{
  tolIntGamma(x, coverage = 0.95, cov.type = "content", 
    ti.type = "two-sided", conf.level = 0.95, method = "exact", 
    est.method = "mle", normal.approx.transform = "kulkarni.powar")

  tolIntGammaAlt(x, coverage = 0.95, cov.type = "content", 
    ti.type = "two-sided", conf.level = 0.95, method = "exact", 
    est.method = "mle", normal.approx.transform = "kulkarni.powar")
}
\arguments{
  \item{x}{
  numeric vector of non-negative observations. Missing (\code{NA}), undefined (\code{NaN}), and 
  infinite (\code{Inf}, \code{-Inf}) values are allowed but will be removed.
}
  \item{coverage}{
  a scalar between 0 and 1 indicating the desired coverage of the tolerance interval.  
  The default value is \code{coverage=0.95}.  If \code{cov.type="expectation"}, 
  this argument is ignored.
}
  \item{cov.type}{
  character string specifying the coverage type for the tolerance interval.  
  The possible values are \code{"content"} (\eqn{\beta}-content; the default), and 
  \code{"expectation"} (\eqn{\beta}-expectation).  See the DETAILS section for more 
  information.
}
  \item{ti.type}{
  character string indicating what kind of tolerance interval to compute.  
  The possible values are \code{"two-sided"} (the default), \code{"lower"}, and 
  \code{"upper"}.
}
  \item{conf.level}{
  a scalar between 0 and 1 indicating the confidence level associated with the tolerance 
  interval.  The default value is \code{conf.level=0.95}.
}
  \item{method}{
  for the case of a two-sided tolerance interval, a character string specifying the 
  method for constructing the two-sided normal distribution tolerance interval using 
  the transformed data.  This argument is ignored if \code{ti.type="lower"} or 
  \code{ti.type="upper"}.  The possible values are \code{"exact"} (the default) and \cr
  \code{"wald.wolfowitz"} (the Wald-Wolfowitz approximation).  See the DETAILS section 
  of the help file for \code{\link{tolIntNorm}} for more information.
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
  The function \code{tolIntGamma} returns a tolerance interval as well as 
  estimates of the shape and scale parameters.  
  The function \code{tolIntGammaAlt} returns a tolerance interval as well as 
  estimates of the mean and coefficient of variation.

  The tolerance interval is computed by 1) using a power transformation on the original 
  data to induce approximate normality, 2) using \code{\link{tolIntNorm}} to compute 
  the tolerance interval, and then 3) back-transforming the interval to create a tolerance 
  interval on the original scale.  (Krishnamoorthy et al., 2008).  
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
  \code{tolIntGamma} and \code{tolIntGammaAlt} the power \eqn{p} is based on 
  whatever estimate of shape is used (e.g., \code{est.method="mle"}, \code{est.method="bcmle"}, etc.).
}
\value{
  A list of class \code{"estimate"} containing the estimated parameters, 
  the tolerance interval, and other information.  See \code{\link{estimate.object}} 
  for details.

  In addition to the usual components contained in an object of class 
  \code{"estimate"}, the returned value also includes an additional 
  component within the \code{"interval"} component:

  \item{normal.transform.power}{the value of the power used to 
    transform the original data to approximate normality.}  
}
\references{
  Berthouex, P.M., and L.C. Brown. (2002). \emph{Statistics for Environmental Engineers}. 
  Lewis Publishers, Boca Raton.

  Draper, N., and H. Smith. (1998). \emph{Applied Regression Analysis}. Third Edition. 
  John Wiley and Sons, New York.

  Ellison, B.E. (1964). On Two-Sided Tolerance Intervals for a Normal Distribution. 
  \emph{Annals of Mathematical Statistics} \bold{35}, 762-772.

  Evans, M., N. Hastings, and B. Peacock. (1993). \emph{Statistical Distributions}. 
  Second Edition. John Wiley and Sons, New York, Chapter 18.

  Gibbons, R.D., D.K. Bhaumik, and S. Aryal. (2009). 
  \emph{Statistical Methods for Groundwater Monitoring}, Second Edition.  
  John Wiley & Sons, Hoboken.

  Guttman, I. (1970). \emph{Statistical Tolerance Regions: Classical and Bayesian}. 
  Hafner Publishing Co., Darien, CT.

  Hahn, G.J. (1970b). Statistical Intervals for a Normal Population, Part I: Tables, Examples 
  and Applications. \emph{Journal of Quality Technology} \bold{2}(3), 115-125.

  Hahn, G.J. (1970c). Statistical Intervals for a Normal Population, Part II: Formulas, Assumptions, 
  Some Derivations. \emph{Journal of Quality Technology} \bold{2}(4), 195-206.

  Hahn, G.J., and W.Q. Meeker. (1991). \emph{Statistical Intervals: A Guide for Practitioners}. 
  John Wiley and Sons, New York.

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
}
\author{
    Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  The gamma distribution takes values on the positive real line. 
  Special cases of the gamma are the \link[=Exponential]{exponential} distribution and 
  the \link[=Chisquare]{chi-square} distributions. Applications of the gamma include 
  life testing, statistical ecology, queuing theory, inventory control, and precipitation 
  processes. A gamma distribution starts to resemble a normal distribution as the 
  shape parameter a tends to infinity.

  Some EPA guidance documents (e.g., Singh et al., 2002; Singh et al., 2010a,b) strongly 
  recommend against using a lognormal model for environmental data and recommend trying a 
  gamma distribuiton instead.

  Tolerance intervals have long been applied to quality control and 
  life testing problems (Hahn, 1970b,c; Hahn and Meeker, 1991).  References that 
  discuss tolerance intervals in the context of environmental monitoring include:  
  Berthouex and Brown (2002, Chapter 21), Gibbons et al. (2009), 
  Millard and Neerchal (2001, Chapter 6), Singh et al. (2010b), and USEPA (2009).
}
\section{Warning}{
  It is possible for the lower tolerance limit based on the transformed data to be less than 0.  
  In this case, the lower tolerance limit on the original scale is set to 0 and a warning is 
  issued stating that the normal approximation is not accurate in this case.
}
\seealso{
  \code{\link{GammaDist}}, \code{\link{estimate.object}}, 
  \code{\link{egamma}}, \code{\link{tolIntNorm}}, 
  \code{\link{predIntGamma}}.
}
\examples{
  # Generate 20 observations from a gamma distribution with parameters 
  # shape=3 and scale=2, then create a tolerance interval. 
  # (Note: the call to set.seed simply allows you to reproduce this 
  # example.)

  set.seed(250) 
  dat <- rgamma(20, shape = 3, scale = 2) 
  tolIntGamma(dat)

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
  #Tolerance Interval Coverage:     95%
  #
  #Coverage Type:                   content
  #
  #Tolerance Interval Method:       Exact using
  #                                 Kulkarni & Powar (2010)
  #                                 transformation to Normality
  #                                 based on mle of 'shape'
  #
  #Tolerance Interval Type:         two-sided
  #
  #Confidence Level:                95%
  #
  #Number of Future Observations:   1
  #
  #Tolerance Interval:              LTL =  0.2340438
  #                                 UTL = 21.2996464

  #--------------------------------------------------------------------

  # Using the same data as in the previous example, create an upper 
  # one-sided tolerance interval and use the bias-corrected estimate of 
  # shape.

  tolIntGamma(dat, ti.type = "upper", est.method = "bcmle")

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
  #Tolerance Interval Coverage:     95%
  #
  #Coverage Type:                   content
  #
  #Tolerance Interval Method:       Exact using
  #                                 Kulkarni & Powar (2010)
  #                                 transformation to Normality
  #                                 based on bcmle of 'shape'
  #
  #Tolerance Interval Type:         upper
  #
  #Confidence Level:                95%
  #
  #Tolerance Interval:              LTL =  0.00000
  #                                 UTL = 17.72107

  #----------

  # Clean up
  rm(dat)
  
  #--------------------------------------------------------------------

  # Example 17-3 of USEPA (2009, p. 17-17) shows how to construct a 
  # beta-content upper tolerance limit with 95% coverage and 95% 
  # confidence  using chrysene data and assuming a lognormal 
  # distribution.  Here we will use the same chrysene data but assume a 
  # gamma distribution.

  attach(EPA.09.Ex.17.3.chrysene.df)
  Chrysene <- Chrysene.ppb[Well.type == "Background"]

  #----------
  # First perform a goodness-of-fit test for a gamma distribution

  gofTest(Chrysene, dist = "gamma")

  #Results of Goodness-of-Fit Test
  #-------------------------------
  #
  #Test Method:                     Shapiro-Wilk GOF Based on 
  #                                 Chen & Balakrisnan (1995)
  #
  #Hypothesized Distribution:       Gamma
  #
  #Estimated Parameter(s):          shape = 2.806929
  #                                 scale = 5.286026
  #
  #Estimation Method:               mle
  #
  #Data:                            Chrysene
  #
  #Sample Size:                     8
  #
  #Test Statistic:                  W = 0.9156306
  #
  #Test Statistic Parameter:        n = 8
  #
  #P-value:                         0.3954223
  #
  #Alternative Hypothesis:          True cdf does not equal the
  #                                 Gamma Distribution.

  #----------
  # Now compute the upper tolerance limit

  tolIntGamma(Chrysene, ti.type = "upper", coverage = 0.95, 
    conf.level = 0.95)

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            Gamma
  #
  #Estimated Parameter(s):          shape = 2.806929
  #                                 scale = 5.286026
  #
  #Estimation Method:               mle
  #
  #Data:                            Chrysene
  #
  #Sample Size:                     8
  #
  #Tolerance Interval Coverage:     95%
  #
  #Coverage Type:                   content
  #
  #Tolerance Interval Method:       Exact using
  #                                 Kulkarni & Powar (2010)
  #                                 transformation to Normality
  #                                 based on mle of 'shape'
  #
  #Tolerance Interval Type:         upper
  #
  #Confidence Level:                95%
  #
  #Tolerance Interval:              LTL =  0.00000
  #                                 UTL = 69.32425

  #----------
  # Compare this upper tolerance limit of 69 ppb to the upper tolerance limit 
  # assuming a lognormal distribution.

  tolIntLnorm(Chrysene, ti.type = "upper", coverage = 0.95, 
    conf.level = 0.95)$interval$limits["UTL"]

  #    UTL 
  #90.9247

  #----------
  # Clean up

  rm(Chrysene)
  detach("EPA.09.Ex.17.3.chrysene.df")

  #--------------------------------------------------------------------

  # Reproduce some of the example on page 73 of 
  # Krishnamoorthy et al. (2008), which uses alkalinity concentrations 
  # reported in Gibbons (1994) and Gibbons et al. (2009) to construct 
  # two-sided and one-sided upper tolerance limits for various values 
  # of coverage using a 95% confidence level.

  tolIntGamma(Gibbons.et.al.09.Alkilinity.vec, ti.type = "upper", 
    coverage = 0.9, normal.approx.transform = "cube.root")

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
  #Tolerance Interval Coverage:     90%
  #
  #Coverage Type:                   content
  #
  #Tolerance Interval Method:       Exact using
  #                                 Wilson & Hilferty (1931) cube-root
  #                                 transformation to Normality
  #
  #Tolerance Interval Type:         upper
  #
  #Confidence Level:                95%
  #
  #Tolerance Interval:              LTL =  0.00000
  #                                 UTL = 97.70502

  tolIntGamma(Gibbons.et.al.09.Alkilinity.vec,  
    coverage = 0.99, normal.approx.transform = "cube.root")

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
  #Tolerance Interval Coverage:     99%
  #
  #Coverage Type:                   content
  #
  #Tolerance Interval Method:       Exact using
  #                                 Wilson & Hilferty (1931) cube-root
  #                                 transformation to Normality
  #
  #Tolerance Interval Type:         two-sided
  #
  #Confidence Level:                95%
  #
  #Tolerance Interval:              LTL =  13.14318
  #                                 UTL = 148.43876
}
\keyword{ distribution }
\keyword{ htest }
