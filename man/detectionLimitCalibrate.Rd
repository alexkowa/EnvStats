\name{detectionLimitCalibrate}
\alias{detectionLimitCalibrate}
\alias{Detection Limit}
\alias{detection limit}
\title{
  Determine Detection Limit
}
\description{
  Determine the detection limit based on using a calibration line (or curve) and
  inverse regression.
}
\usage{
  detectionLimitCalibrate(object, coverage = 0.99, simultaneous = TRUE)
}
\arguments{
  \item{object}{
  an object of class \code{"calibrate"} that is the result of calling the function
  \code{\link{calibrate}}.
}
  \item{coverage}{
  optional numeric scalar between 0 and 1 indicating the confidence level associated with
  the prediction intervals used in determining the detection limit.
  The default value is \code{coverage=0.99}.
}
  \item{simultaneous}{
  optional logical scalar indicating whether to base the prediction intervals on
  simultaneous or non-simultaneous prediction limits.  The default value is \cr
  \code{simultaneous=TRUE}.
}
}
\details{
  The idea of a decision limit and detection limit is directly related to calibration and
  can be framed in terms of a hypothesis test, as shown in the table below.
  The null hypothesis is that the chemical is not present in the physical sample, i.e.,
  \eqn{H_0: C = 0}, where C denotes the concentration.

  \tabular{lcc}{
  \bold{Your Decision}         \tab \bold{\eqn{H_0} True (\eqn{C = 0})} \tab \bold{\eqn{H_0} False (\eqn{C > 0})} \cr
                               \tab                                     \tab                                      \cr
  Reject \eqn{H_0}             \tab Type I Error                        \tab                                      \cr
    (Declare Chemical Present) \tab (Probability = \eqn{\alpha})        \tab                                      \cr
                               \tab                                     \tab                                      \cr
  Do Not Reject \eqn{H_0}      \tab                                     \tab Type II Error                        \cr
    (Declare Chemical Absent)  \tab                                     \tab (Probability = \eqn{\beta})          \cr
  }

  Ideally, you would like to minimize both the Type I and Type II error rates.
  Just as we use critical values to compare against the test statistic for a hypothesis test,
  we need to use a critical signal level \eqn{S_D} called the \strong{decision limit} to decide
  whether the chemical is present or absent.  If the signal is less than or equal to \eqn{S_D}
  we will declare the chemical is absent, and if the signal is greater than \eqn{S_D} we will
  declare the chemical is present.

  First, suppose no chemical is present (i.e., the null hypothesis is true).
  If we want to guard against the mistake of declaring that the chemical is present when in fact it is
  absent (Type I error), then we should choose \eqn{S_D} so that the probability of this happening is
  some small value \eqn{\alpha}.  Thus, the value of \eqn{S_D} depends on what we want to use for
  \eqn{\alpha} (the Type I error rate), and the true (but unknown) value of \eqn{\sigma}
  (the standard deviation of the errors assuming a constant standard deviation)
  (Massart et al., 1988, p. 111).

  When the true concentration is 0, the decision limit is the (1-\eqn{\alpha})100th percentile of the
  distribution of the signal \emph{S}.  Note that the decision limit is on the scale of and in units
  of the signal \emph{S}.

  Now suppose that in fact the chemical is present in some concentration \emph{C}
  (i.e., the null hypothesis is false).  If we want to guard against the mistake of
  declaring that the chemical is absent when in fact it is present (Type II error),
  then we need to determine a minimal concentration \eqn{C_DL} called the \strong{detection limit (DL)}
  that we know will yield a signal less than the decision limit \eqn{S_D} only a small fraction of the
  time (\eqn{\beta}).

  In practice we do not know the true value of the standard deviation of the errors (\eqn{\sigma}),
  so we cannot compute the true decision limit.  Also, we do not know the true values of the
  intercept and slope of the calibration line, so we cannot compute the true detection limit.
  Instead, we usually set \eqn{\alpha = \beta} and estimate the decision and detection limits
  by computing prediction limits for the calibration line and using inverse regression.

  The estimated detection limit corresponds to the upper confidence bound on concentration given that the
  signal is equal to the estimated decision limit.  Currie (1997) discusses other ways to define the
  detection limit, and Glaser et al. (1981) define a quantity called the method detection limit.
}
\value{
  A numeric vector of length 2 indicating the signal detection limit and the concentration
  detection limit. This vector has two attributes called \code{coverage}
  and \code{simultaneous} indicating the values of these arguments that were used in the
  call to \code{detectionLimitCalibrate}.
}
\references{
  Clark, M.J.R., and P.H. Whitfield. (1994). Conflicting Perspectives About Detection Limits and About the Censoring of Environmental Data.
  \emph{Water Resources Bulletin} \bold{30}(6), 1063--1079.

  Clayton, C.A., J.W. Hines, and P.D. Elkins. (1987). Detection Limits with Specified Assurance Probabilities.
  \emph{Analytical Chemistry} \bold{59}, 2506--2514.

  Code of Federal Regulations. (1996). Definition and Procedure for the Determination of the Method Detection Limit--Revision 1.11.
  Title 40, Part 136, Appendix B, 7-1-96 Edition, pp.265--267.

  Currie, L.A. (1968). Limits for Qualitative Detection and Quantitative Determination: Application to Radiochemistry.
  \emph{Annals of Chemistry} \bold{40}, 586--593.

  Currie, L.A. (1988). \emph{Detection in Analytical Chemistry: Importance, Theory, and Practice}.
  American Chemical Society, Washington, D.C.

  Currie, L.A. (1995). Nomenclature in Evaluation of Analytical Methods Including Detection and Quantification Capabilities.
  \emph{Pure & Applied Chemistry} \bold{67}(10), 1699-1723.

  Currie, L.A. (1996). Foundations and Future of Detection and Quantification Limits.
  \emph{Proceedings of the Section on Statistics and the Environment}, American Statistical Association, Alexandria, VA.

  Currie, L.A. (1997). Detection: International Update, and Some Emerging Di-Lemmas Involving Calibration, the Blank, and Multiple Detection Decisions.
  \emph{Chemometrics and Intelligent Laboratory Systems} \bold{37}, 151-181.

  Davis, C.B. (1994). Environmental Regulatory Statistics. In Patil, G.P., and C.R. Rao, eds.,
  \emph{Handbook of Statistics, Vol. 12: Environmental Statistics}.
  North-Holland, Amsterdam, a division of Elsevier, New York, NY,
  Chapter 26, 817--865.

  Davis, C.B. (1997). Challenges in Regulatory Environmetrics.
  \emph{Chemometrics and Intelligent Laboratory Systems} \bold{37}, 43--53.

  Gibbons, R.D. (1995). Some Statistical and Conceptual Issues in the Detection of Low-Level Environmental Pollutants
  (with Discussion). \emph{Environmetrics} \bold{2}, 125-167.

  Gibbons, R.D., D.E. Coleman, and R.F. Maddalone. (1997a). An Alternative Minimum Level Definition for Analytical Quantification.
  \emph{Environmental Science & Technology} \bold{31}(7), 2071--2077.
  Comments and Discussion in Volume \bold{31}(12), 3727--3731, and Volume \bold{32}(15), 2346--2353.

  Gibbons, R.D., D.E. Coleman, and R.F. Maddalone. (1997b). Response to Comment on
  \dQuote{An Alternative Minimum Level Definition for Analytical Quantification}.
  \emph{Environmental Science and Technology} \bold{31}(12), 3729--3731.

  Gibbons, R.D., D.E. Coleman, and R.F. Maddalone. (1998). Response to Comment on
  \dQuote{An Alternative Minimum Level Definition for Analytical Quantification}.
  \emph{Environmental Science and Technology} \bold{32}(15), 2349--2353.

  Gibbons, R.D., N.E. Grams, F.H. Jarke, and K.P. Stoub. (1992). Practical Quantitation Limits.
  \emph{Chemometrics Intelligent Laboratory Systems} \bold{12}, 225--235.

  Gibbons, R.D., F.H. Jarke, and K.P. Stoub. (1991). Detection Limits: For Linear Calibration Curves with Increasing Variance and Multiple Future Detection Decisions.
  In Tatsch, D.E., editor. \emph{Waste Testing and Quality Assurance: Volume 3}.
  American Society for Testing and Materials, Philadelphi, PA.

  Gibbons, R.D., D.K. Bhaumik, and S. Aryal. (2009).
  \emph{Statistical Methods for Groundwater Monitoring}. Second Edition.
  John Wiley & Sons, Hoboken.  Chapter 6, p. 111.

  Helsel, D.R. (2012). \emph{Statistics for Censored Environmental Data Using Minitab and R,
  Second Edition}.  John Wiley & Sons, Hoboken, New Jersey.  Chapter 3, p. 22.

  Glasser, J.A., D.L. Foerst, G.D. McKee, S.A. Quave, and W.L. Budde. (1981). Trace Analyses for Wastewaters.
  \emph{Environmental Science and Technology} \bold{15}, 1426--1435.

  Hubaux, A., and G. Vos. (1970). Decision and Detection Limits for Linear Calibration Curves.
  \emph{Annals of Chemistry} \bold{42}, 849--855.

  Kahn, H.D., C.E. White, K. Stralka, and R. Kuznetsovski. (1997). Alternative Estimates of Detection.
  \emph{Proceedings of the Twentieth Annual EPA Conference on Analysis of Pollutants in the Environment, May 7-8, Norfolk, VA}.
  U.S. Environmental Protection Agency, Washington, D.C.

  Kahn, H.D., W.A. Telliard, and C.E. White. (1998). Comment on \dQuote{An Alternative
  Minimum Level Definition for Analytical Quantification} (with Response).
  \emph{Environmental Science & Technology} \bold{32}(5), 2346--2353.

  Kaiser, H. (1965). Zum Problem der Nachweisgrenze. Fresenius' Z. \emph{Anal. Chem.} \bold{209}, 1.

  Keith, L.H. (1991). \emph{Environmental Sampling and Analysis: A Practical Guide}.
  Lewis Publishers, Boca Raton, FL, Chapter 10.

  Kimbrough, D.E. (1997). Comment on \dQuote{An Alternative Minimum Level Definition for Analytical Quantification} (with Response).
  \emph{Environmental Science & Technology} \bold{31}(12), 3727--3731.

  Lambert, D., B. Peterson, and I. Terpenning. (1991). Nondetects, Detection Limits, and the Probability of Detection.
  \emph{Journal of the American Statistical Association} \bold{86}(414), 266--277.

  Massart, D.L., B.G.M. Vandeginste, S.N. Deming, Y. Michotte, and L. Kaufman. (1988).
  \emph{Chemometrics: A Textbook}. Elsevier, New York, Chapter 7.

  Millard, S.P., and Neerchal, N.K. (2001). \emph{Environmental Statistics with S-PLUS}.
  CRC Press, Boca Raton, Florida.

  Porter, P.S., R.C. Ward, and H.F. Bell. (1988). The Detection Limit.
  \emph{Environmental Science & Technology} \bold{22}(8), 856--861.

  Rocke, D.M., and S. Lorenzato. (1995). A Two-Component Model for Measurement Error in Analytical Chemistry.
  \emph{Technometrics} \bold{37}(2), 176--184.

  Singh, A. (1993). Multivariate Decision and Detection Limits.
  \emph{Analytica Chimica Acta} \bold{277}, 205-214.

  Spiegelman, C.H. (1997). A Discussion of Issues Raised by Lloyd Currie and a Cross Disciplinary View of Detection Limits and Estimating Parameters That Are Often At or Near Zero.
  \emph{Chemometrics and Intelligent Laboratory Systems} \bold{37}, 183--188.

  USEPA. (1987c). List (Phase 1) of Hazardous Constituents for Ground-Water Monitoring; Final Rule.
  \emph{Federal Register} \bold{52}(131), 25942--25953 (July 9, 1987).

  Zorn, M.E., R.D. Gibbons, and W.C. Sonzogni. (1997). Weighted Least-Squares Approach to Calculating Limits of Detection and Quantification by Modeling Variability as a Function of Concentration.
  \emph{Analytical Chemistry} \bold{69}, 3069--3075.
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  Perhaps no other topic in environmental statistics has generated as much confusion or controversy as
  the topic of detection limits.  After decades of disparate terminology, ISO and IUPAC provided harmonized
  guidance on the topic in 1995 (Currie, 1997).  Intuitively, the idea of a detection limit is simple to
  grasp:  the \strong{detection limit} is \dQuote{the smallest amount or concentration of a particular
  substance that can be reliably detected in a given type of sample or medium by a specific measurement
  process} (Currie, 1997, p. 152).  Unfortunately, because of the exceedingly complex nature of measuring
  chemical concentrations, this simple idea is difficult to apply in practice.

  Detection and quantification capabilities are fundamental performance characteristics of the
  \strong{Chemical Measurement Process (CMP)} (Currie, 1996, 1997).  In this help file we discuss
  some currently accepted definitions of the terms decision, detection, and quantification limits.
  For more details, the reader should consult the references listed in this help file.

  The \strong{quantification limit} is defined as the concentration \emph{C} at which the
  coefficient of variation (also called relative standard deviation or RSD) for the
  distribution of the signal \emph{S} is some small value, usually taken to be 10\%
  (Currie, 1968, 1997). In practice the quantification limit is difficult to estimate
  because we have to estimate both the mean and the standard deviation of the signal \emph{S}
  for any particular concentration, and usually the standard deviation varies with concentration.
  Variations of the quantification limit include the quantitation limit (Keith, 1991, p. 109),
  minimum level (USEPA, 1993), and alternative minimum level (Gibbons et al., 1997a).
}
\seealso{
  \code{\link{calibrate}}, \code{\link{inversePredictCalibrate}}, \code{\link{pointwise}}.
}
\examples{
  # The data frame EPA.97.cadmium.111.df contains calibration
  # data for cadmium at mass 111 (ng/L) that appeared in
  # Gibbons et al. (1997b) and were provided to them by the U.S. EPA.
  #
  # The Example section in the help file for calibrate shows how to
  # plot these data along with the fitted calibration line and 99%
  # non-simultaneous prediction limits.
  #
  # For the current example, we will compute the decision limit (7.68)
  # and detection limit (12.36 ng/L) based on using alpha = beta = 0.01
  # and a linear calibration line with constant variance. See
  # Millard and Neerchal (2001, pp.566-575) for more details on this
  # example.

  calibrate.list <- calibrate(Cadmium ~ Spike, data = EPA.97.cadmium.111.df)

  detectionLimitCalibrate(calibrate.list, simultaneous = FALSE)
  #        Decision Limit (Signal) Detection Limit (Concentration)
  #                       7.677842                       12.364670
  #attr(,"coverage")
  #[1] 0.99
  #attr(,"simultaneous")
  #[1] FALSE

  #----------

  # Clean up
  #---------
  rm(calibrate.list)
}
\keyword{ models }
\keyword{ regression }
