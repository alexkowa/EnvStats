#' EnvStats: Package for Environmental Statistics
#'
#' Graphical and statistical analyses of environmental data, with a focus on
#' chemical concentrations and physical parameters encountered in mandated
#' environmental monitoring.
#' @keywords internal
#' @name EnvStats-package
#' @rawNamespace importFrom("grDevices", "dev.new", "devAskNewPage", "nclass.Sturges")
#' @rawNamespace importFrom("graphics", "Axis", "abline", "axis", "box", "hist", "lines", "mtext", "par", "plot", "points", "polygon", "rect", "segments", "text", "title", "xinch", "yinch")
#' @rawNamespace importFrom("stats", "anova", "aov", "approx", "ar.yw", "arima", "binom.test", "cor", "dbeta", "dchisq", "dhyper", "dlnorm", "dnorm", "dpois", "formula", "integrate", "kruskal.test", "ks.test", "lm", "mad", "median", "model.frame", "na.pass", "nlminb", "optim", "pbeta", "pbinom", "pchisq", "pf", "pgamma", "phyper", "plnorm", "pnbinom", "pnorm", "ppoints", "ppois", "prop.test", "pt", "qbeta", "qbinom", "qchisq", "qexp", "qf", "qgamma", "qgeom", "qhyper", "qlnorm", "qlogis", "qnbinom", "qnorm", "qpois", "qt", "quantile", "qunif", "qweibull", "rbinom", "rchisq", "residuals", "rlnorm", "rnorm", "runif", "sd", "summary.aov", "t.test", "terms", "time", "uniroot", "update", "var")
#' @rawNamespace importFrom("utils", "combn", "find", "modifyList")
#' @rawNamespace export(anovaPE, aovN, aovPower, base, boxcox, boxcoxCensored, boxcoxTransform, calibrate, cdfCompare, cdfCompareCensored, cdfPlot, chenTTest, ciBinomHalfWidth, ciBinomN, ciNormHalfWidth, ciNormN, ciNparConfLevel, ciNparN, ciTableMean, ciTableProp, cv, dchi, demp, detectionLimitCalibrate, devd, dgammaAlt, dgevd, distChoose, distChooseCensored, dlnormAlt, dlnormMix, dlnormMixAlt, dlnormTrunc, dlnormTruncAlt, dlnorm3, dnormMix, dnormTrunc, dpareto, dtri, dzmlnorm, dzmlnormAlt, dzmnorm, ebeta, ebinom, ecdfPlot, ecdfPlotCensored, eevd, eexp, egamma, egammaAlt, egammaCensored, egammaAltCensored, egeom, egevd, ehyper, elnorm, elnormAlt, elnormCensored, elnormAltCensored, elnorm3, elogis, enbinom, enorm, enormCensored, enparCensored, epareto, epdfPlot, epois, epoisCensored, eqbeta, eqbinom, eqevd, eqexp, eqgamma, eqgammaAlt, eqgeom, eqgevd, eqhyper, eqlnorm, eqlnormCensored, eqlnorm3, eqlogis, eqnbinom, eqnorm, eqnormCensored, eqnpar, eqpareto, eqpois, equnif, eqweibull, eqzmlnorm, eqzmlnormAlt, eqzmnorm, errorBar, eunif, evNormOrdStats, evNormOrdStatsScalar, eweibull, ezmlnorm, ezmlnormAlt, ezmnorm, geom_stripchart, geoMean, geoSD, gofTest, gofGroupTest, gofTestCensored, gpqCiNormSinglyCensored, gpqCiNormMultiplyCensored, gpqTolIntNormSinglyCensored, gpqTolIntNormMultiplyCensored, inversePredictCalibrate, iqr, kendallTrendTest, kendallSeasonalTrendTest, kurtosis, lMoment, linearTrendTestN, linearTrendTestPower, linearTrendTestScaledMds, longToWide, newsEnvStats, oneSamplePermutationTest, pchi, pdfPlot, pemp, pevd, pgammaAlt, pgevd, plnormAlt, plnormMix, plnormMixAlt, plnormTrunc, plnormTruncAlt, plnorm3, plotAovDesign, plotCiBinomDesign, plotCiNormDesign, plotCiNparDesign, plotLinearTrendTestDesign, plotPredIntLnormAltSimultaneousTestPowerCurve, plotPredIntLnormAltTestPowerCurve, plotPredIntNormDesign, plotPredIntNormSimultaneousTestPowerCurve, plotPredIntNormTestPowerCurve, plotPredIntNparDesign, plotPredIntNparSimultaneousDesign, plotPredIntNparSimultaneousTestPowerCurve, plotPropTestDesign, plotTTestDesign, plotTTestLnormAltDesign, plotTolIntNormDesign, plotTolIntNparDesign, plot.boxcox, plot.boxcoxLm, plot.boxcoxCensored, plot.gof, plot.gofCensored, plot.gofGroup, plot.gofTwoSample, plot.permutationTest, pnormMix, pnormTrunc, pointwise, ppareto, ppointsCensored, predict, predict.default, predict.lm, predIntGamma, predIntGammaAlt, predIntGammaSimultaneous, predIntGammaAltSimultaneous, predIntLnorm, predIntLnormAlt, predIntLnormAltSimultaneousTestPower, predIntLnormAltTestPower, predIntLnormSimultaneous, predIntLnormAltSimultaneous, predIntNorm, predIntNormHalfWidth, predIntNormK, predIntNormN, predIntNormSimultaneous, predIntNormSimultaneousK, predIntNormSimultaneousTestPower, predIntNormTestPower, predIntNpar, predIntNparConfLevel, predIntNparN, predIntNparSimultaneous, predIntNparSimultaneousConfLevel, predIntNparSimultaneousN, predIntNparSimultaneousTestPower, predIntPois, print.boxcox, print.boxcoxCensored, print.boxcoxLm, print.estimate, print.estimateCensored, print.gof, print.gofCensored, print.gofGroup, print.gofOutlier, print.htestEnvStats, print.htestCensored, print.permutationTest, print.summaryStats, print.gofTwoSample, propTestMdd, propTestN, propTestPower, ptri, pwMoment, pzmlnorm, pzmlnormAlt, pzmnorm, qchi, qemp, qevd, qgammaAlt, qgevd, qlnormAlt, qlnormMix, qlnormMixAlt, qlnormTrunc, qlnormTruncAlt, qlnorm3, qnormMix, qnormTrunc, qpareto, qqPlot, qqPlotCensored, qqPlotGestalt, qtri, quantileTest, quantileTestPValue, qzmlnorm, qzmlnormAlt, qzmnorm, rchi, remp, revd, rgammaAlt, rgevd, rlnormAlt, rlnormMix, rlnormMixAlt, rlnormTrunc, rlnormTruncAlt, rlnorm3, rnormMix, rnormTrunc, rosnerTest, rpareto, rtri, rzmlnorm, rzmlnormAlt, rzmnorm, serialCorrelationTest, signTest, simulateMvMatrix, simulateVector, skewness, stat_n_text, StatNText, stat_mean_sd_text, StatMeanSDText, stat_median_iqr_text, StatMedianIQRText, stat_test_text, StatTestText, stripChart, summaryFull, summaryStats, tTestAlpha, tTestN, tTestPower, tTestLnormAltN, tTestLnormAltPower, tTestLnormAltRatioOfMeans, tTestScaledMdd, tolIntGamma, tolIntGammaAlt, tolIntLnorm, tolIntLnormAlt, tolIntLnormCensored, tolIntNorm, tolIntNormHalfWidth, tolIntNormK, tolIntNormN, tolIntNormCensored, tolIntNpar, tolIntNparConfLevel, tolIntNparCoverage, tolIntNparN, tolIntPois, twoSampleLinearRankTest, twoSampleLinearRankTestCensored, twoSamplePermutationTestLocation, twoSamplePermutationTestProportion, varGroupTest, varTest, zTestGevdShape, enpar)
#' @rawNamespace S3method(boxcox, default)
#' @rawNamespace S3method(boxcox, lm)
#' @rawNamespace S3method(distChoose, default)
#' @rawNamespace S3method(distChoose, formula)
#' @rawNamespace S3method(gofTest, default)
#' @rawNamespace S3method(gofTest, formula)
#' @rawNamespace S3method(gofGroupTest, default)
#' @rawNamespace S3method(gofGroupTest, formula)
#' @rawNamespace S3method(gofGroupTest, data.frame)
#' @rawNamespace S3method(gofGroupTest, matrix)
#' @rawNamespace S3method(gofGroupTest, list)
#' @rawNamespace S3method(kendallSeasonalTrendTest, default)
#' @rawNamespace S3method(kendallSeasonalTrendTest, data.frame)
#' @rawNamespace S3method(kendallSeasonalTrendTest, matrix)
#' @rawNamespace S3method(kendallSeasonalTrendTest, formula)
#' @rawNamespace S3method(kendallTrendTest, default)
#' @rawNamespace S3method(kendallTrendTest, formula)
#' @rawNamespace S3method(plot, boxcox)
#' @rawNamespace S3method(plot, boxcoxLm)
#' @rawNamespace S3method(plot, boxcoxCensored)
#' @rawNamespace S3method(plot, gof)
#' @rawNamespace S3method(plot, gofCensored)
#' @rawNamespace S3method(plot, gofGroup)
#' @rawNamespace S3method(plot, gofTwoSample)
#' @rawNamespace S3method(plot, permutationTest)
#' @rawNamespace S3method(predict, default)
#' @rawNamespace S3method(predict, lm)
#' @rawNamespace S3method(print, boxcox)
#' @rawNamespace S3method(print, boxcoxCensored)
#' @rawNamespace S3method(print, boxcoxLm)
#' @rawNamespace S3method(print, distChoose)
#' @rawNamespace S3method(print, estimate)
#' @rawNamespace S3method(print, estimateCensored)
#' @rawNamespace S3method(print, gof)
#' @rawNamespace S3method(print, gofCensored)
#' @rawNamespace S3method(print, gofGroup)
#' @rawNamespace S3method(print, gofOutlier)
#' @rawNamespace S3method(print, htestCensored)
#' @rawNamespace S3method(print, permutationTest)
#' @rawNamespace S3method(print, summaryStats)
#' @rawNamespace S3method(print, gofTwoSample)
#' @rawNamespace S3method(print, distChooseCensored)
#' @rawNamespace S3method(print, intervalEstimate)
#' @rawNamespace S3method(print, intervalEstimateCensored)
#' @rawNamespace S3method(print, htestEnvStats)
#' @rawNamespace S3method(serialCorrelationTest, default)
#' @rawNamespace S3method(serialCorrelationTest, lm)
#' @rawNamespace S3method(stripChart, default)
#' @rawNamespace S3method(stripChart, formula)
#' @rawNamespace S3method(summaryFull, default)
#' @rawNamespace S3method(summaryFull, data.frame)
#' @rawNamespace S3method(summaryFull, matrix)
#' @rawNamespace S3method(summaryFull, list)
#' @rawNamespace S3method(summaryFull, formula)
#' @rawNamespace S3method(summaryStats, default)
#' @rawNamespace S3method(summaryStats, factor)
#' @rawNamespace S3method(summaryStats, data.frame)
#' @rawNamespace S3method(summaryStats, matrix)
#' @rawNamespace S3method(summaryStats, list)
#' @rawNamespace S3method(summaryStats, formula)
#' @rawNamespace S3method(summaryStats, logical)
#' @rawNamespace S3method(summaryStats, character)
#' @rawNamespace S3method(varGroupTest, default)
#' @rawNamespace S3method(varGroupTest, data.frame)
#' @rawNamespace S3method(varGroupTest, matrix)
#' @rawNamespace S3method(varGroupTest, list)
#' @rawNamespace S3method(varGroupTest, formula)
NULL


#' Trichloroethylene Concentrations Before and After Remedation
#' @name ACE.13.TCE.df
#' @description
#' Trichloroethylene (TCE) concentrations (mg/L) at 10 groundwater monitoring wells
#'   before and after remediation.
#' @usage
#' data(ACE.13.TCE.df)
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 20 observations on the following 3 variables.
#'   \describe{
#'     \item{\code{TCE.mg.per.L}}{TCE concentrations}
#'     \item{\code{Well}}{a factor indicating the well number}
#'     \item{\code{Period}}{a factor indicating the period (before vs. after remediation)}
#'   }
#' }
#' @rawRd
#' \source{
#'   USACE. (2013).  \emph{Environmental Quality - Environmental Statistics}.
#'   Engineer Manual EM 200-1-16, 31 May 2013.  Department of the Army,
#'   U.S. Army Corps of Engineers, Washington, D.C. 20314-1000, p. M-10.
#'   \url{https://www.publications.usace.army.mil/Portals/76/Publications/EngineerManuals/EM_200-1-16.pdf}.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Randomly sampled measurements of an analyte in soil samples.
#' @name BJC.2000.df
#' @description
#' Analyte concentrations (\eqn{\mu}g/g) in 11 discrete environmental soil samples.
#' @usage
#' BJC.2000.df
#'     data(BJC.2000.df)
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 11 observations on the following 4 variables.
#'   \describe{
#'     \item{\code{Analyte.char}}{Character vector indicating lead concentrations.
#'       Nondetects indicated with the letter U after the measure (e.g., 0.10U)}
#'     \item{\code{Analyte}}{numeric vector indicating analyte concentration.}
#'     \item{\code{Censored}}{logical vector indicating censoring status.}
#'     \item{\code{Detect}}{numeric vector of 0s (nondetects) and 1s (detects)
#'      indicating censoring status.}
#'   }
#' }
#' @rawRd
#' \source{
#'   BJC. (2000).  \emph{Improved Methods for Calculating Concentrations Used
#'   in Exposure Assessments}. BJC/OR-416, Prepared by the Lockheed Martin
#'   Energy Research Corporation. Prepared for the U.S. Department of
#'   Energy Office of Environmental Management. Bechtel Jacobs Company, LLC.
#'   January, 2000. \url{https://rais.ornl.gov/documents/bjc_or416.pdf}.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Lead concentration in soil samples.
#' @name Beal.2010.Pb.df
#' @description
#' Lead (Pb) concentrations (mg/kg) in 29 discrete environmental soil samples
#'   from a site suspected to be contaminated with lead.
#' @usage
#' Beal.2010.Pb.df
#'     data(Beal.2010.Pb.df)
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 29 observations on the following 3 variables.
#'   \describe{
#'     \item{\code{Pb.char}}{Character vector indicating lead concentrations.
#'       Nondetects indicated with the less-than sign (e.g., <1)}
#'     \item{\code{Pb}}{numeric vector indicating lead concentration.}
#'     \item{\code{Censored}}{logical vector indicating censoring status.}
#'   }
#' }
#' @rawRd
#' \source{
#'   Beal, D. (2010).  \emph{A Macro for Calculating Summary Statistics on
#'   Left Censored Environmental Data Using the Kaplan-Meier Method}.
#'   Paper SDA-09, presented at Southeast SAS Users Group 2010, September 26-28,
#'   Savannah, GA. \url{https://analytics.ncsu.edu/sesug/2010/SDA09.Beal.pdf}.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Benthic Data from Monitoring Program in Chesapeake Bay
#' @name Benthic.df
#' @description
#' Benthic data from a monitoring program in the Chesapeake Bay,
#'   Maryland, covering July 1994 - December 1991.
#' @usage
#' Benthic.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 585 observations on the following 7 variables.
#'   \describe{
#'     \item{\code{Site.ID}}{Site ID}
#'     \item{\code{Stratum}}{Stratum Number (101-131)}
#'     \item{\code{Latitude}}{Latitude (degrees North)}
#'     \item{\code{Longitude}}{Longitude (negative values; degrees West)}
#'     \item{\code{Index}}{Benthic Index (between 1 and 5)}
#'     \item{\code{Salinity}}{Salinity (ppt)}
#'     \item{\code{Silt}}{Silt Content (\% clay in soil)}
#'   }
#' }
#' @rawRd
#' \details{
#'   Data from the Long Term Benthic Monitoring Program of the Chesapeake Bay.
#'   The data consist of measurements of benthic characteristics and a computed
#'   index of benthic health for several locations in the bay. Sampling methods
#'   and designs of the program are discussed in Ranasinghe et al. (1992).
#'
#'   The data represent observations collected at 585 separate point locations (sites).
#'   The sites are divided into 31 different strata, numbered 101 through 131, each
#'   strata consisting of geographically close sites of similar degradation conditions.
#'   The benthic index values range from 1 to 5 on a continuous scale, where high values
#'   correspond to healthier benthos. Salinity was measured in parts per thousand (ppt),
#'   and silt content is expressed as a percentage of clay in the soil with high numbers
#'   corresponding to muddy areas.
#'
#'   The United States Environmental Protection Agency (USEPA) established an initiative
#'   for the Chesapeake Bay in partnership with the states bordering the bay in 1984.
#'   The goal of the initiative is the restoration (abundance, health, and diversity)
#'   of living resources to the bay by reducing nutrient loadings, reducing toxic
#'   chemical impacts, and enhancing habitats.  USEPA's Chesapeake Bay Program Office
#'   is responsible for implementing this initiative and has established an extensive
#'   monitoring program that includes traditional water chemistry sampling, as well as
#'   collecting data on living resources to measure progress towards meeting the
#'   restoration goals.
#'
#'   Sampling benthic invertebrate assemblages has been an integral part of the
#'   Chesapeake Bay monitoring program due to their ecological importance and their
#'   value as biological indicators. The condition of benthic assemblages is a measure
#'   of the ecological health of the bay, including the effects of multiple types of
#'   environmental stresses.  Nevertheless, regional-scale assessment of ecological
#'   status and trends using benthic assemblages are limited by the fact that benthic
#'   assemblages are strongly influenced by naturally variable habitat elements, such as
#'   salinity, sediment type, and depth.  Also, different state agencies and USEPA programs
#'   use different sampling methodologies, limiting the ability to integrate data into a
#'   unified assessment.  To circumvent these limitations, USEPA has standardized benthic
#'   data from several different monitoring programs into a single database, and from
#'   that database developed a Restoration Goals Benthic Index that identifies whether
#'   benthic restoration goals are being met.
#' }
#' @rawRd
#' \source{
#'   Ranasinghe, J.A., L.C. Scott, and R. Newport. (1992).
#'   \emph{Long-term Benthic Monitoring and Assessment Program for the Maryland Portion of the Bay},
#'   Jul 1984-Dec 1991. Report prepared for the Maryland Department of the Environment and
#'   the Maryland Department of Natural Resources by Versar, Inc., Columbia, MD.
#' }
#' @rawRd
#' \examples{
#'   attach(Benthic.df)
#'
#'   # Show station locations
#'   #-----------------------
#'   dev.new()
#'   plot(Longitude, Latitude,
#'       xlab = "-Longitude (Degrees West)",
#'       ylab = "Latitude",
#'       main = "Sampling Station Locations")
#'
#'
#'   # Scatterplot matrix of benthic index, salinity, and silt
#'   #--------------------------------------------------------
#'   dev.new()
#'   pairs(~ Index + Salinity + Silt, data = Benthic.df)
#'
#'
#'   # Contour and perspective plots based on loess fit
#'   # showing only predicted values within the convex hull
#'   # of station locations
#'   #-----------------------------------------------------
#'   library(sp)
#'
#'   loess.fit <- loess(Index ~ Longitude * Latitude,
#'       data=Benthic.df, normalize=FALSE, span=0.25)
#'   lat <- Benthic.df$Latitude
#'   lon <- Benthic.df$Longitude
#'   Latitude <- seq(min(lat), max(lat), length=50)
#'   Longitude <- seq(min(lon), max(lon), length=50)
#'   predict.list <- list(Longitude=Longitude,
#'       Latitude=Latitude)
#'   predict.grid <- expand.grid(predict.list)
#'   predict.fit <- predict(loess.fit, predict.grid)
#'   index.chull <- chull(lon, lat)
#'   inside <- point.in.polygon(point.x = predict.grid$Longitude,
#'       point.y = predict.grid$Latitude,
#'       pol.x = lon[index.chull],
#'       pol.y = lat[index.chull])
#'   predict.fit[inside == 0] <- NA
#'
#'   dev.new()
#'   contour(Longitude, Latitude, predict.fit,
#'       levels=seq(1, 5, by=0.5), labcex=0.75,
#'       xlab="-Longitude (degrees West)",
#'       ylab="Latitude (degrees North)")
#'   title(main=paste("Contour Plot of Benthic Index",
#'       "Based on Loess Smooth", sep="\n"))
#'
#'   dev.new()
#'   persp(Longitude, Latitude, predict.fit,
#'       xlim = c(-77.3, -75.9), ylim = c(38.1, 39.5), zlim = c(0, 6),
#'       theta = -45, phi = 30, d = 0.5,
#'       xlab="-Longitude (degrees West)",
#'       ylab="Latitude (degrees North)",
#'       zlab="Benthic Index", ticktype = "detailed")
#'   title(main=paste("Surface Plot of Benthic Index",
#'       "Based on Loess Smooth", sep="\n"))
#'
#'   detach("Benthic.df")
#'
#'   rm(loess.fit, lat, lon, Latitude, Longitude, predict.list,
#'       predict.grid, predict.fit, index.chull, inside)
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Abstract: Castillo and Hadi (1994)
#' @name CastilloAndHadi1994
#' @rawRd \alias{Castillo and Hadi 1994}
#' @description
#' Detailed abstract of the manuscript: \cr\cr
#'
#'   Castillo, E., and A. Hadi. (1994).  Parameter and Quantile Estimation for the
#'   Generalized Extreme-Value Distribution.  \emph{Environmetrics} \bold{5}, 417--432.
#' @rawRd
#' \concept{Castillo}
#' @rawRd
#' \concept{Abstract}
#' @rawRd
#' \details{
#'   \bold{Abstract} \cr
#'   Castillo and Hadi (1994) introduce a new way to estimate the parameters and
#'   quantiles of the \link[=GEVD]{generalized extreme value distribution} (GEVD)
#'   with parameters \code{location=}\eqn{\eta}, \code{scale=}\eqn{\theta}, and
#'   \code{shape=}\eqn{\kappa}.  The estimator is based on a two-stage procedure using
#'   order statistics,  denoted here by \dQuote{TSOE}, which stands for
#'   two-stage order-statistics estimator.  Castillo and Hadi (1994) compare the TSOE
#'   to the maximum likelihood estimator (MLE; Jenkinson, 1969; Prescott and Walden, 1983)
#'   and probability-weighted moments estimator (PWME;
#'   \link[=HoskingEtAl1985]{Hosking et al., 1985)}.
#'
#'   Castillo and Hadi (1994) note that for some samples the likelihood may not have
#'   a local maximum, and also when \eqn{\kappa > 1} the likelihood can be made
#'   infinite so the MLE does not exist.  They also note, as do
#'   \link[=HoskingEtAl1985]{Hosking et al., 1985)}, that when \eqn{\kappa \le -1},
#'   the moments and probability-weighed moments of the GEVD do not exist, hence
#'   neither does the PWME.  (Hosking et al., however, claim that in practice the
#'   shape parameter usually lies between -1/2 and 1/2.)  On the other hand, the
#'   TSOE exists for all values of \eqn{\kappa}.
#'
#'   Based on computer simulations, Castillo and Hadi (1994) found that the
#'   performance (bias and root mean squared error) of the TSOE is comparable to the
#'   PWME for values of \eqn{\kappa} in the range \eqn{-1/2 \le \kappa \le 1/2}.
#'   They also found that the TSOE is superior to the PWME for large values of
#'   \eqn{\kappa}.  Their results, however, are based on using the PWME computed
#'   using the approximation given in equation (14) of Hosking et al. (1985, p.253).
#'   The true PWME is computed using equation (12) of Hosking et al. (1985, p.253).
#'   Hosking et al. (1985) introduced the approximation as a matter of computational
#'   convenience, and noted that it is valid in the range \eqn{-1/2 \le \kappa \le 1/2}.
#'   If Castillo and Hadi (1994) had used the true PWME for values of \eqn{\kappa}
#'   larger than 1/2, they probably would have gotten very different results for the
#'   PWME.  (Note: the function \code{\link{egevd}} with \code{method="pwme"} uses
#'   the exact equation (12) of Hosking et al. (1985), not the approximation (14)).
#'
#'   Castillo and Hadi (1994) suggest using the bootstrap or jackknife to obtain
#'   variance estimates and confidence intervals for the distribution parameters
#'   based on the TSOE.
#'   \cr
#'
#'   \bold{More Details}
#'   Let \eqn{\underline{x} = (x_1, x_2, \ldots, x_n)} be a vector of
#'   \eqn{n} observations from a \link[=GEVD]{generalized extreme value distribution} with
#'   parameters \code{location=}\eqn{\eta}, \code{scale=}\eqn{\theta}, and
#'   \code{shape=}\eqn{\kappa} with cumulative distribution function \eqn{F}.
#'   Also, let \eqn{x(1), x(2), \ldots, x(n)} denote the ordered values of
#'   \eqn{\underline{x}}.
#'
#'
#'   \emph{First Stage} \cr
#'   Castillo and Hadi (1994) propose as initial estimates of the distribution
#'   parameters the solutions to the following set of simultaneous equations based
#'   on just three observations from the total sample of size \eqn{n}:
#'   \deqn{F[x(1); \eta, \theta, \kappa] = p_{1,n}}
#'   \deqn{F[x(j); \eta, \theta, \kappa] = p_{j,n}}
#'   \deqn{F[x(n); \eta, \theta, \kappa] = p_{n,n} \;\;\;\; (1)}
#'   where \eqn{2 \le j \le n-1}, and
#'   \deqn{p_{i,n} = \hat{F}[x(i); \eta, \theta, \kappa]}
#'   denotes the \eqn{i}'th plotting position for a sample of size \eqn{n}; that is, a
#'   nonparametric estimate of the value of \eqn{F} at \eqn{x(i)}.  Typically,
#'   plotting positions have the form:
#'   \deqn{p_{i,n} = \frac{i-a}{n+b} \;\;\;\; (2)}
#'   where \eqn{b > -a > -1}.  In their simulation studies, Castillo and Hadi (1994)
#'   used a=0.35, b=0.
#'
#'   Since \eqn{j} is arbitrary in the above set of equations (1), denote the solutions
#'   to these equations by:
#'   \deqn{\hat{\eta}_j, \hat{\theta}_j, \hat{\kappa}_j}
#'   There are thus \eqn{n-2} sets of estimates.
#'
#'   Castillo and Hadi (1994) show that the estimate of the shape parameter, \eqn{\kappa},
#'   is the solution to the equation:
#'   \deqn{\frac{x(j) - x(n)}{x(1) - x(n)} = \frac{1 - A_{jn}^\kappa}{1 - A_{1n}^\kappa} \;\;\;\; (3)}
#'   where
#'   \deqn{A_{ik} = C_i / C_k \;\;\;\; (4)}
#'   \deqn{C_i = -log(p_{i,n}) \;\;\;\; (5)}
#'   Castillo and Hadi (1994) show how to easily solve equation (3) using the method of
#'   bisection.
#'
#'   Once the estimate of the shape parameter is obtained, the other estimates are given
#'   by:
#'   \deqn{\hat{\theta}_j = \frac{\hat{\kappa}_j [x(1) - x(n)]}{(C_n)^{\hat{\kappa}_j} - (C_1)^{\hat{\kappa}_j}} \;\;\;\; (6)}
#'   \deqn{\hat{\eta}_j = x(1) - \frac{\hat{\theta}_j [1 - (C_1)^{\hat{\kappa}_j}]}{\hat{\kappa}_j} \;\;\;\; (7)}
#'   \cr
#'
#'   \emph{Second Stage} \cr
#'   Apply a robust function to the \eqn{n-2} sets of estimates obtained in the
#'   first stage.  Castillo and Hadi (1994) suggest using either the median or the
#'   least median of squares (using a column of 1's as the predictor variable;
#'   see the help file for \link[MASS]{lmsreg} in the package \pkg{MASS}).  Using
#'   the median, for example, the final distribution parameter estimates are
#'   given by:
#'   \deqn{\hat{\eta} = Median(\hat{\eta}_2, \hat{\eta}_3, \ldots, \hat{\eta}_{n-1})}
#'   \deqn{\hat{\theta} = Median(\hat{\theta}_2, \hat{\theta}_3, \ldots, \hat{\theta}_{n-1})}
#'   \deqn{\hat{\kappa} = Median(\hat{\kappa}_2, \hat{\kappa}_3, \ldots, \hat{\kappa}_{n-1})}
#' }
#' @rawRd
#' \references{
#'   Forbes, C., M. Evans, N. Hastings, and B. Peacock. (2011).  Statistical Distributions.
#'   Fourth Edition. John Wiley and Sons, Hoboken, NJ.
#'
#'   Hosking, J.R.M. (1985).  Algorithm AS 215: Maximum-Likelihood Estimation of the
#'   Parameters of the Generalized Extreme-Value Distribution.
#'   \emph{Applied Statistics} \bold{34}(3), 301--310.
#'
#'   Jenkinson, A.F. (1969).  Statistics of Extremes. \emph{Technical Note 98},
#'   World Meteorological Office, Geneva.
#'
#'   Johnson, N. L., S. Kotz, and N. Balakrishnan. (1995).
#'   \emph{Continuous Univariate Distributions, Volume 2}.
#'   Second Edition. John Wiley and Sons, New York.
#'
#'   Prescott, P., and A.T. Walden. (1983).  Maximum Likelihood Estimation of the
#'   Three-Parameter Generalized Extreme-Value Distribution from Censored Samples.
#'   \emph{Journal of Statistical Computing and Simulation} \bold{16}, 241--250.
#' }
#' @rawRd
#' \author{
#'     Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \seealso{
#'   \link[=GEVD]{Generalized Extreme Value Distribution}, \code{\link{egevd}},
#'   \link[=HoskingEtAl1985]{Hosking et al., 1985)}.
#' }
#' @rawRd
#' \keyword{ distribution }
NULL

#' The Chi Distribution
#' @name Chi
#' @aliases dchi pchi qchi rchi
#' @description
#' Density, distribution function, quantile function, and random generation for the
#'   chi distribution.
#' @usage
#' dchi(x, df)
#'   pchi(q, df)
#'   qchi(p, df)
#'   rchi(n, df)
#' @rawRd
#' \arguments{
#'   \item{x}{
#'   vector of (positive) quantiles.
#' }
#'   \item{q}{
#'   vector of (positive) quantiles.
#' }
#'   \item{p}{
#'   vector of probabilities between 0 and 1.
#' }
#'   \item{n}{
#'   sample size.  If \code{length(n)} is larger than 1, then \code{length(n)}
#'   random values are returned.
#' }
#'   \item{df}{
#'   vector of (positive) degrees of freedom (> 0).  Non-integer values are allowed.
#' }
#' }
#' @rawRd
#' \details{
#'   Elements of \code{x}, \code{q}, \code{p}, or \code{df} that are missing will
#'   cause the corresponding elements of the result to be missing.
#'
#'   The chi distribution with \eqn{n} degrees of freedom is the distribution of the
#'   positive square root of a random variable having a
#'   \link[stats:Chisquare]{chi-squared} distribution with \eqn{n} degrees of freedom.
#'
#'   The chi density function is given by:
#'   \deqn{f(x, \nu) = g(x^2, \nu) 2x,    x > 0}
#'   where \eqn{g(x,\nu)} denotes the density function of a chi-square random variable
#'   with \eqn{n} degrees of freedom.
#' }
#' @rawRd
#' \value{
#'   density (\code{dchi}), probability (\code{pchi}), quantile (\code{qchi}), or
#'   random sample (\code{rchi}) for the chi distribution with \code{df} degrees of freedom.
#' }
#' @rawRd
#' \references{
#'   Forbes, C., M. Evans, N. Hastings, and B. Peacock. (2011).  Statistical Distributions.
#'   Fourth Edition. John Wiley and Sons, Hoboken, NJ.
#'
#'   Johnson, N. L., S. Kotz, and N. Balakrishnan. (1995).
#'   \emph{Continuous Univariate Distributions, Volume 1}.
#'   Second Edition. John Wiley and Sons, New York.
#' }
#' @rawRd
#' \author{
#'   Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \note{
#'   The chi distribution takes on positive real values.  It is important because
#'   for a sample of \eqn{n} observations from a \link[stats:Normal]{normal} distribution,
#'   the sample standard deviation multiplied by the square root of the degrees of
#'   freedom \eqn{\nu} and divided by the true standard deviation follows a chi
#'   distribution with \eqn{\nu} degrees of freedom.  The chi distribution is also
#'   used in computing exact prediction intervals for the next \eqn{k} observations
#'   from a normal distribution (see \code{\link{predIntNorm}}).
#' }
#' @rawRd
#' \seealso{
#'   \link{Chisquare}, \link{Normal}, \code{\link{predIntNorm}},
#'   \link{Probability Distributions and Random Numbers}.
#' }
#' @rawRd
#' \examples{
#'   # Density of a chi distribution with 4 degrees of freedom, evaluated at 3:
#'
#'   dchi(3, 4)
#'   #[1] 0.1499715
#'
#'   #----------
#'
#'   # The 95'th percentile of a chi distribution with 10 degrees of freedom:
#'
#'   qchi(.95, 10)
#'   #[1] 4.278672
#'
#'   #----------
#'
#'   # The cumulative distribution function of a chi distribution with
#'   # 5 degrees of freedom evaluated at 3:
#'
#'   pchi(3, 5)
#'   #[1] 0.8909358
#'
#'   #----------
#'
#'   # A random sample of 2 numbers from a chi distribution with 7 degrees of freedom.
#'   # (Note: the call to set.seed simply allows you to reproduce this example.)
#'
#'   set.seed(20)
#'   rchi(2, 7)
#'   #[1] 3.271632 2.035179
#' }
#' @rawRd
#' \keyword{ distribution }
#' @rawRd
#' \keyword{ datagen }
NULL

#' Data Frame Summarizing Available Probability Distributions and Estimation Methods
#' @name Distribution.df
#' @description
#' Data frame summarizing information about available probability
#'   distributions in \R and the \pkg{EnvStats} package, and which
#'   distributions have associated functions for estimating distribution
#'   parameters.
#' @usage
#' Distribution.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 35 rows corresponding to 35 different available
#'   probability distributions, and 25 columns containing information
#'   associated with these probability distributions.
#'   \describe{
#'     \item{\code{Name}}{a character vector containing the name of
#'       the probability distribution (see the column labeled \bold{Name}
#'       in the table below).}
#'     \item{\code{Type}}{a character vector indicating the type of
#'       distribution (see the column labeled \bold{Type} in the table below).
#'       Possible values are \code{"Finite Discrete"}, \code{"Discrete"},
#'       \code{"Continuous"}, and \code{"Mixed"}.}
#'     \item{\code{Support.Min}}{a character vector indicating the minimum value
#'       the random variable can assume (see the column labeled \bold{Range} in
#'       the table below).  The reason this is a character vector instead of a
#'       numeric vector is because some distributions have a lower bound that
#'       depends on the value of a distribution parameter.  For example,
#'       the minimum value for a \link{Uniform} distribution is given by the
#'       value of the parameter \code{min}.}
#'     \item{\code{Support.Max}}{a character vector indicating the maximum value
#'       the random variable can assume (see the column labeled \bold{Range} in
#'       the table below).  The reason this is a character vector instead of a
#'       numeric vector is because some distributions have an upper bound that
#'       depends on the value of a distribution parameter.  For example,
#'       the maximum value for a \link{Uniform} distribution is given by the value
#'       of the parameter \code{max}.}
#'     \item{\code{Estimation.Method(s)}}{a character vector indicating the
#'       names of the methods available to estimate the distribution parameter(s)
#'       (see the column labeled \bold{Estimation Method(s)} in the table below).
#'       Possible values include \code{"mle"} (maximum likelihood), \code{"mme"}
#'       (method of moments), \code{"mmue"} (method of moments based on the
#'       unbiased estimate of variance), \code{"mvue"} (minimum variance unbiased),
#'       \code{"qmle"} (quasi-mle), etc., or some combination of these.  In
#'       cases where an estimator is more than one kind, a slash (\code{/}) is
#'       used to denote all methods covered by the single estimator.  For example,
#'       for the Binomial distribution, the sample proportion is the maximum
#'       likelihood, method of moments, and minimum variance unbiased estimator,
#'       so this method is denoted as \code{"mle/mme/mvue"}.  See the help files
#'       for the specific function listed under
#'       \link[=FcnsByCatEstDistParams]{Estimating Distribution Parameters} for an
#'       explanation of each of these estimation methods.}
#'     \item{\code{Quantile.Estimation.Method(s)}}{a character vector indicating
#'       the names of the methods available to estimate the distribution
#'       quantiles.  For many distributions, these are the same as
#'       \code{Estimation.Method(s)}.  See the help files for the specific
#'       function listed under
#'       \link[=FcnsByCatEstDistQuants]{Estimating Distribution Quantiles} for an
#'       explanation of each of these estimation methods.}
#'     \item{\code{Prediction.Interval.Method(s)}}{a character vector indicating
#'       the names of the methods available to create prediction intervals.  See
#'       the help files for the specific function listed under
#'       \link[=FcnsByCatPredInts]{Prediction Intervals} for an explanation of each of
#'       these estimation methods.}
#'     \item{\code{Singly.Censored.Estimation.Method(s)}}{a character vector
#'       indicating the names of the methods available to estimate the
#'       distribution parameter(s) for Type I singly-censored data.  See the
#'       help files for the specific function listed under
#'       \emph{Estimating Distribution Parameters} in the help file for
#'       \link[=FcnsByCatCensoredData]{Censored Data}
#'       for an explanation of each of these estimation methods.}
#'     \item{\code{Multiply.Censored.Estimation.Method(s)}}{a character vector
#'       indicating the names of the methods available to estimate the
#'       distribution parameter(s) for Type I multiply-censored data.  See the
#'       help files for the specific function listed under
#'       \emph{Estimating Distribution Parameters} in the help file for
#'       \link[=FcnsByCatCensoredData]{Censored Data}
#'       for an explanation of each of these estimation methods.}
#'     \item{\code{Number.parameters}}{a numeric vector indicating the number of
#'       parameters associated with the distribution (see the column labeled
#'       \bold{Parameters} in the table below).}
#'     \item{\code{Parameter.1}}{the columns labeled
#'       \code{Parameter.1}, \code{Parameter.2}, \ldots, \code{Parameter.5} are
#'       character vectors containing the names of the distribution parameters
#'       (see the column labeled \bold{Parameters} in the table below).  If a
#'       distribution has \eqn{n} parameters and \eqn{n < 5}, then the columns
#'       labeled \code{Parameter.n+1}, \ldots, \code{Parameter.5} are empty.  For
#'       example, the \link{Normal} distribution has only two parameters
#'       associated with it (\code{mean} and \code{sd}), so the fields in
#'       \code{Parameter.3}, \code{Parameter.4}, and \code{Parameter.5} are
#'       empty.}
#'     \item{\code{Parameter.2}}{see \code{Parameter.1}}
#'     \item{\code{Parameter.3}}{see \code{Parameter.1}}
#'     \item{\code{Parameter.4}}{see \code{Parameter.1}}
#'     \item{\code{Parameter.5}}{see \code{Parameter.1}}
#'     \item{\code{Parameter.1.Min}}{the columns labeled \code{Parameter.1.Min},
#'       \code{Parameter.2.Min}, \ldots, \cr
#'       \code{Parameter.5.Min} are character
#'       vectors containing the minimum values that can be assumed by the
#'       distribution parameters (see the column labeled \bold{Parameter Range(s)}
#'       in the table below).
#'
#'       The reason these are character vectors instead of numeric vectors is
#'       because some parameters have a lower bound of \code{0} but must be
#'       strictly bigger than \code{0} (e.g., the parameter \code{sd} for the
#'       \link{Normal} distribution), in which case the lower bound is
#'       \code{.Machine$double.eps}, which may vary from machine to machine.
#'       Also, some parameters have a lower bound that depends on the value of
#'       another parameter.  For example, the parameter \code{max} for a
#'       \link{Uniform} distribution is bounded below by the value of the
#'       parameter \code{min}.
#'
#'       If a distribution has \eqn{n} parameters and \eqn{n < 5}, then the
#'       columns labeled \code{Parameter.n+1.Min}, \ldots, \code{Parameter.5.Min}
#'       have the missing value code (\code{NA}).  For example, the \link{Normal}
#'       distribution has only two parameters associated with it (\code{mean}
#'       and \code{sd}) so the fields in \cr
#'       \code{Parameter.3.Min}, \code{Parameter.4.Min}, and \code{Parameter.5.Min}
#'       have \code{NA}s in them.}
#'     \item{\code{Parameter.2.Min}}{see \code{Parameter.1.Min}}
#'     \item{\code{Parameter.3.Min}}{see \code{Parameter.1.Min}}
#'     \item{\code{Parameter.4.Min}}{see \code{Parameter.1.Min}}
#'     \item{\code{Parameter.5.Min}}{see \code{Parameter.1.Min}}
#'     \item{\code{Parameter.1.Max}}{the columns labeled \code{Parameter.1.Max},
#'       \code{Parameter.2.Max}, \ldots, \cr
#'       \code{Parameter.5.Max} are character
#'       vectors containing the maximum values that can be assumed by the
#'       distribution parameters (see the column labeled \bold{Parameter Range(s)}
#'       in the table below).
#'
#'       The reason these are character vectors instead of numeric vectors is
#'       because some parameters have an upper bound that depends on the value
#'       of another parameter.  For example, the parameter \code{min} for a
#'       \link{Uniform} distribution is bounded above by the value of the
#'       parameter \code{max}.
#'
#'       If a distribution has \eqn{n} parameters and \eqn{n < 5}, then the
#'       columns labeled \code{Parameter.n+1.Max}, \ldots, \code{Parameter.5.Max}
#'       have the missing value code (\code{NA}).  For example, the \link{Normal}
#'       distribution has only two parameters associated with it (\code{mean}
#'       and \code{sd}) so the fields in \cr
#'       \code{Parameter.3.Max}, \code{Parameter.4.Max}, and \code{Parameter.5.Max}
#'       have \code{NA}s in them.}
#'     \item{\code{Parameter.2.Max}}{see \code{Parameter.1.Max}}
#'     \item{\code{Parameter.3.Max}}{see \code{Parameter.1.Max}}
#'     \item{\code{Parameter.4.Max}}{see \code{Parameter.1.Max}}
#'     \item{\code{Parameter.5.Max}}{see \code{Parameter.1.Max}}
#'   }
#' }
#' @details
#' This help page has been shortened to keep function help focused on usage,
#' arguments, return values, and examples. Extended method details, formulas,
#' and background material are available in \code{vignette("extended-function-details", package = "EnvStats")}, section \code{Distribution.df}.
#' @rawRd
#' \source{
#'   The \pkg{EnvStats} package.
#' }
#' @rawRd
#' \references{
#'   Millard, S.P. (2013). \emph{EnvStats: An R Package for Environmental Statistics}.
#'   Springer, New York.  \url{https://link.springer.com/book/10.1007/978-1-4614-8456-1}.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Concentrations in Exhibit 2 of 2002d USEPA Guidance Document
#' @name EPA.02d.Ex.2.ug.per.L.vec
#' @description
#' Concentrations (\eqn{mu}g/L) from an exposure unit.
#' @usage
#' data(EPA.02d.Ex.2.ug.per.L.vec)
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   a numeric vector of concentrations (\eqn{mu}g/L)
#' }
#' @rawRd
#' \source{
#'   USEPA. (2002d).  \emph{Calculating Upper Confidence Limits for Exposure Point Concentrations at
#'   Hazardous Waste Sites}.  OSWER 9285.6-10, December 2002.  Office of Emergency and Remedial Response,
#'   U.S. Environmental Protection Agency, Washington, D.C., p. 9.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Concentrations in Exhibit 4 of 2002d USEPA Guidance Document
#' @name EPA.02d.Ex.4.mg.per.kg.vec
#' @description
#' Concentrations (mg/kg) from an exposure unit.
#' @usage
#' data(EPA.02d.Ex.4.mg.per.kg.vec)
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   a numeric vector of concentrations (mg/kg)
#' }
#' @rawRd
#' \source{
#'   USEPA. (2002d).  \emph{Calculating Upper Confidence Limits for Exposure Point Concentrations at
#'   Hazardous Waste Sites}.  OSWER 9285.6-10, December 2002.  Office of Emergency and Remedial Response,
#'   U.S. Environmental Protection Agency, Washington, D.C., p. 11.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Concentrations in Exhibit 6 of 2002d USEPA Guidance Document
#' @name EPA.02d.Ex.6.mg.per.kg.vec
#' @description
#' Concentrations (mg/kg) from an exposure unit.
#' @usage
#' data(EPA.02d.Ex.6.mg.per.kg.vec)
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   a numeric vector of concentrations (mg/kg)
#' }
#' @rawRd
#' \source{
#'   USEPA. (2002d).  \emph{Calculating Upper Confidence Limits for Exposure Point Concentrations at
#'   Hazardous Waste Sites}.  OSWER 9285.6-10, December 2002.  Office of Emergency and Remedial Response,
#'   U.S. Environmental Protection Agency, Washington, D.C., p. 13.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Concentrations in Exhibit 9 of 2002d USEPA Guidance Document
#' @name EPA.02d.Ex.9.mg.per.L.vec
#' @description
#' Concentrations (mg/L) from an exposure unit.
#' @usage
#' data(EPA.02d.Ex.9.mg.per.L.vec)
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   a numeric vector of concentrations (mg/L)
#' }
#' @rawRd
#' \source{
#'   USEPA. (2002d).  \emph{Calculating Upper Confidence Limits for Exposure Point Concentrations at
#'   Hazardous Waste Sites}.  OSWER 9285.6-10, December 2002.  Office of Emergency and Remedial Response,
#'   U.S. Environmental Protection Agency, Washington, D.C., p. 16.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Nickel Concentrations from Example 10-1 of 2009 USEPA Guidance Document
#' @name EPA.09.Ex.10.1.nickel.df
#' @description
#' Nickel concentrations (ppb) from four wells (five observations per year for each well).
#'   The Guidance Document has the label \dQuote{Year} instead of \dQuote{Well};
#'   corrected in Errata.
#' @usage
#' EPA.09.Ex.10.1.nickel.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 20 observations on the following 3 variables.
#'   \describe{
#'     \item{\code{Month}}{a numeric vector indicating the month the sample was taken}
#'     \item{\code{Well}}{a factor indicating the well number}
#'     \item{\code{Nickel.ppb}}{a numeric vector of nickel concentrations (ppb)}
#'   }
#' }
#' @rawRd
#' \source{
#'   USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
#'   EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery, Program Implementation and Information Division.
#'   U.S. Environmental Protection Agency, Washington, D.C., p.10-12.
#'
#'   USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
#'   EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
#'   U.S. Environmental Protection Agency, Washington, D.C.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Arsenic Concentrations from Example 11-1 of 2009 USEPA Guidance Document
#' @name EPA.09.Ex.11.1.arsenic.df
#' @description
#' Arsenic concentrations (ppb) at six wells (four observations per well).
#' @usage
#' EPA.09.Ex.11.1.arsenic.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 24 observations on the following 3 variables.
#'   \describe{
#'     \item{\code{Arsenic.ppb}}{a numeric vector of arsenic concentrations (ppb)}
#'     \item{\code{Month}}{a factor indicating the month of collection}
#'     \item{\code{Well}}{a factor indicating the well number}
#'   }
#' }
#' @rawRd
#' \source{
#'   USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
#'   EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.
#'   U.S. Environmental Protection Agency, Washington, D.C. p.11-3.
#'
#'   USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
#'   EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
#'   U.S. Environmental Protection Agency, Washington, D.C.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Carbon Tetrachloride Concentrations from Example 12-1 of 2009 USEPA Guidance Document
#' @name EPA.09.Ex.12.1.ccl4.df
#' @description
#' Carbon tetrachloride (CCL4) concentrations (ppb) at five background wells (four measures at each well).
#' @usage
#' EPA.09.Ex.12.1.ccl4.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 20 observations on the following 2 variables.
#'   \describe{
#'     \item{\code{Well}}{a factor indicating the well number}
#'     \item{\code{CCL4.ppb}}{a numeric vector of CCL4 concentrations (ppb)}
#'   }
#' }
#' @rawRd
#' \source{
#'   USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
#'   EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.
#'   U.S. Environmental Protection Agency, Washington, D.C. p.12-3.
#'
#'   USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
#'   EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
#'   U.S. Environmental Protection Agency, Washington, D.C.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Naphthalene Concentrations from Example 12-4 of 2009 USEPA Guidance Document
#' @name EPA.09.Ex.12.4.naphthalene.df
#' @description
#' Naphthalene concentrations (ppb) at five background wells (five quarterly measures at each well).
#' @usage
#' EPA.09.Ex.12.4.naphthalene.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 25 observations on the following 3 variables.
#'   \describe{
#'     \item{\code{Quarter}}{a numeric vector indicating the quarter the sample was taken}
#'     \item{\code{Well}}{a factor indicating the well number}
#'     \item{\code{Naphthalene.ppb}}{a numeric vector of naphthalene concentrations (ppb)}
#'   }
#' }
#' @rawRd
#' \source{
#'   USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
#'   EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.
#'   U.S. Environmental Protection Agency, Washington, D.C. p.12-12.
#'
#'   USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
#'   EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
#'   U.S. Environmental Protection Agency, Washington, D.C.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Iron Concentrations from Example 13-1 of 2009 USEPA Guidance Document
#' @name EPA.09.Ex.13.1.iron.df
#' @description
#' Dissolved iron (Fe) concentrations (ppm) at six upgradient wells (four quarterly measures at each well).
#' @usage
#' EPA.09.Ex.13.1.iron.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 24 observations on the following 4 variables.
#'   \describe{
#'     \item{\code{Month}}{a numeric vector indicating the month the sample was taken}
#'     \item{\code{Year}}{a numeric vector indicating the year the sample was taken}
#'     \item{\code{Well}}{a factor indicating the well number}
#'     \item{\code{Iron.ppm}}{a numeric vector if iron concentrations (ppm)}
#'   }
#' }
#' @rawRd
#' \source{
#'   USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
#'   EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.
#'   U.S. Environmental Protection Agency, Washington, D.C. p.13-3.
#'
#'   USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
#'   EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
#'   U.S. Environmental Protection Agency, Washington, D.C.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Manganese Concentrations from Example 14-1 of 2009 USEPA Guidance Document
#' @name EPA.09.Ex.14.1.manganese.df
#' @description
#' Manganese concentrations (ppm) at four background wells (eight quarterly measures at each well).
#' @usage
#' EPA.09.Ex.14.1.manganese.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 32 observations on the following 3 variables.
#'   \describe{
#'     \item{\code{Quarter}}{a numeric vector indicating the quarter the sample was taken}
#'     \item{\code{Well}}{a factor indicating the well number}
#'     \item{\code{Manganese.ppm}}{a numeric vector of manganese concentrations (ppm)}
#'   }
#' }
#' @rawRd
#' \source{
#'   USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
#'   EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.
#'   U.S. Environmental Protection Agency, Washington, D.C. p.14-5.
#'
#'   USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
#'   EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
#'   U.S. Environmental Protection Agency, Washington, D.C.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Alkalinity Measures from Example 14-3 of 2009 USEPA Guidance Document
#' @name EPA.09.Ex.14.3.alkalinity.df
#' @description
#' Alkalinity measures (mg/L) collected from leachate at a solid waste landfill during a four and a half year period.
#' @usage
#' EPA.09.Ex.14.3.alkalinity.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 54 observations on the following 2 variables.
#'   \describe{
#'     \item{\code{Date}}{a Date object indicating the date of collection}
#'     \item{\code{Alkalinity.mg.per.L}}{a numeric vector of alkalinity measures (mg/L)}
#'   }
#' }
#' @rawRd
#' \source{
#'   USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
#'   EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.
#'   U.S. Environmental Protection Agency, Washington, D.C. p.14-14.
#'
#'   USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
#'   EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
#'   U.S. Environmental Protection Agency, Washington, D.C.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Arsenic Concentrations from Example 14-4 of 2009 USEPA Guidance Document
#' @name EPA.09.Ex.14.4.arsenic.df
#' @description
#' Sixteen quarterly measures of arsenic concentrations (ppb).
#' @usage
#' EPA.09.Ex.14.4.arsenic.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 16 observations on the following 4 variables.
#'   \describe{
#'     \item{\code{Sample.Date}}{a factor indicating the month and year of collection}
#'     \item{\code{Month}}{a factor indicating the month of collection}
#'     \item{\code{Year}}{a factor indicating the year of collection}
#'     \item{\code{Arsenic.ppb}}{a numeric vector of arsenic concentrations (ppb)}
#'   }
#' }
#' @rawRd
#' \source{
#'   USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
#'   EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.
#'   U.S. Environmental Protection Agency, Washington, D.C. p.14-18.
#'
#'   USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
#'   EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
#'   U.S. Environmental Protection Agency, Washington, D.C.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Analyte Concentrations from Example 14-8 of 2009 USEPA Guidance Document
#' @name EPA.09.Ex.14.8.df
#' @description
#' Monthly unadjusted and adjusted analyte concentrations over a 3-year period.  Adjusted
#'   concentrations are computed by subtracting the monthly mean and adding the overall mean.
#' @usage
#' EPA.09.Ex.14.8.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 36 observations on the following 4 variables.
#'   \describe{
#'     \item{\code{Month}}{a factor indicating the month of collection}
#'     \item{\code{Year}}{a numeric vector indicating the year of collection}
#'     \item{\code{Unadj.Conc}}{a numeric vector of unadjusted concentrations}
#'     \item{\code{Adj.Conc}}{a numeric vector adjusted concentrations}
#'   }
#' }
#' @rawRd
#' \source{
#'   USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
#'   EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.
#'   U.S. Environmental Protection Agency, Washington, D.C. p.14-32.
#'
#'   USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
#'   EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
#'   U.S. Environmental Protection Agency, Washington, D.C.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Manganese Concentrations from Example 15-1 of 2009 USEPA Guidance Document
#' @name EPA.09.Ex.15.1.manganese.df
#' @description
#' Manganese concentrations (ppb) at five background wells (five measures at each well).
#' @usage
#' EPA.09.Ex.15.1.manganese.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 25 observations on the following 5 variables.
#'   \describe{
#'     \item{\code{Sample}}{a numeric vector indicating the sample number (1-5)}
#'     \item{\code{Well}}{a factor indicating the well number}
#'     \item{\code{Manganese.Orig.ppb}}{a character vector of the original manganese concentrations (ppb)}
#'     \item{\code{Manganese.ppb}}{a numeric vector of manganese concentrations with non-detects coded to their detecion limit}
#'     \item{\code{Censored}}{a logical vector indicating which observations are censored}
#'   }
#' }
#' @rawRd
#' \source{
#'   USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
#'   EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.
#'   U.S. Environmental Protection Agency, Washington, D.C. p.15-10.
#'
#'   USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
#'   EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
#'   U.S. Environmental Protection Agency, Washington, D.C.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Sulfate Concentrations from Example 16-1 of 2009 USEPA Guidance Document
#' @name EPA.09.Ex.16.1.sulfate.df
#' @description
#' Sulfate concentrations (ppm) at one background well and one downgradient well
#'   (eight quarterly measures at each well).
#' @usage
#' EPA.09.Ex.16.1.sulfate.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 16 observations on the following 4 variables.
#'   \describe{
#'     \item{\code{Month}}{a factor indicating the month of collection}
#'     \item{\code{Year}}{a factor indicating the year of collection}
#'     \item{\code{Well.type}}{a factor indicating the well type (background vs. downgradient)}
#'     \item{\code{Sulfate.ppm}}{a numeric vector of sulfate concentrations (ppm)}
#'   }
#' }
#' @rawRd
#' \source{
#'   USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
#'   EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.
#'   U.S. Environmental Protection Agency, Washington, D.C. p.16-6.
#'
#'   USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
#'   EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
#'   U.S. Environmental Protection Agency, Washington, D.C.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Benzene Concentrations from Example 16-2 of 2009 USEPA Guidance Document
#' @name EPA.09.Ex.16.2.benzene.df
#' @description
#' Benzene concentrations (ppb) at one background and one downgradient well (eight monthly measures at each well).
#' @usage
#' EPA.09.Ex.16.2.benzene.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 16 observations on the following 3 variables.
#'   \describe{
#'     \item{\code{Month}}{a factor indicating the month of collection}
#'     \item{\code{Well.type}}{a factor indicating the well type (background vs. downgradient)}
#'     \item{\code{Benzene.ppb}}{a numeric vector of benzene concentrations (ppb)}
#'   }
#' }
#' @rawRd
#' \source{
#'   USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
#'   EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.
#'   U.S. Environmental Protection Agency, Washington, D.C. p.16-9.
#'
#'   USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
#'   EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
#'   U.S. Environmental Protection Agency, Washington, D.C.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Copper Concentrations from Example 16-4 of 2009 USEPA Guidance Document
#' @name EPA.09.Ex.16.4.copper.df
#' @description
#' Copper concentrations (ppb) at two background wells and one compliance well (six measures at each well).
#' @usage
#' EPA.09.Ex.16.4.copper.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 18 observations on the following 4 variables.
#'   \describe{
#'     \item{\code{Month}}{a factor indicating the month of collection}
#'     \item{\code{Well}}{a factor indicating the well number}
#'     \item{\code{Well.type}}{a factor indicating the well type (background vs. compliance)}
#'     \item{\code{Copper.ppb}}{a numeric vector of copper concentrations (ppb)}
#'   }
#' }
#' @rawRd
#' \source{
#'   USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
#'   EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.
#'   U.S. Environmental Protection Agency, Washington, D.C. p.16-19.
#'
#'   USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
#'   EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
#'   U.S. Environmental Protection Agency, Washington, D.C.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Tetrachloroethylene Concentrations from Example 16-5 of 2009 USEPA Guidance Document
#' @name EPA.09.Ex.16.5.PCE.df
#' @description
#' Tetrachloroethylene (PCE) concentrations (ppb) at one background well and one compliance well.
#' @usage
#' EPA.09.Ex.16.5.PCE.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 14 observations on the following 4 variables.
#'   \describe{
#'     \item{\code{Well.type}}{a factor with levels \code{Background} \code{Compliance}}
#'     \item{\code{PCE.Orig.ppb}}{a character vector of original PCE concentrations (ppb)}
#'     \item{\code{PCE.ppb}}{a numeric vector of PCE concentrations (ppb) with nondetects set to their detection limit}
#'     \item{\code{Censored}}{a logical vector indicating which observations are censored}
#'   }
#' }
#' @rawRd
#' \source{
#'   USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
#'   EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.
#'   U.S. Environmental Protection Agency, Washington, D.C. p.16-22.
#'
#'   USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
#'   EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
#'   U.S. Environmental Protection Agency, Washington, D.C.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Log-transformed Lead Concentrations from Example 17-1 of 2009 USEPA Guidance Document
#' @name EPA.09.Ex.17.1.loglead.df
#' @description
#' Log-transformed lead concentrations (ppb) at two background and four compliance wells (four quarterly measures at each well).
#' @usage
#' EPA.09.Ex.17.1.loglead.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 24 observations on the following 4 variables.
#'   \describe{
#'     \item{\code{Month}}{a factor indicating the month of collection; \code{1} = Jan, \code{2} = Apr, \code{3} = Jul, \code{4} = Oct}
#'     \item{\code{Well}}{a factor indicating the well number}
#'     \item{\code{Well.type}}{a factor indicating the well type (background vs. compliance)}
#'     \item{\code{LogLead}}{a numeric vector of log-transformed lead concentrations (ppb)}
#'   }
#' }
#' @rawRd
#' \source{
#'   USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
#'   EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.
#'   U.S. Environmental Protection Agency, Washington, D.C. p.17-7.
#'
#'   USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
#'   EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
#'   U.S. Environmental Protection Agency, Washington, D.C.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Toluene Concentrations from Example 17-2 of 2009 USEPA Guidance Document
#' @name EPA.09.Ex.17.2.toluene.df
#' @description
#' Toluene concentrations (ppb) at two background and three compliance wells (five monthly measures at each well).
#' @usage
#' EPA.09.Ex.17.2.toluene.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 25 observations on the following 6 variables.
#'   \describe{
#'     \item{\code{Month}}{a factor indicating the month of collection}
#'     \item{\code{Well}}{a factor indicating the well number}
#'     \item{\code{Well.type}}{a factor indicating the well type (background vs. compliance)}
#'     \item{\code{Toluene.ppb.orig}}{a character vector of original toluene concentrations (ppb)}
#'     \item{\code{Toluene.ppb}}{a numeric vector of toluene concentrations (ppb) with nondetects set to their detection limit}
#'     \item{\code{Censored}}{a logical vector indicating which observations are censored}
#'   }
#' }
#' @rawRd
#' \source{
#'   USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
#'   EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.
#'   U.S. Environmental Protection Agency, Washington, D.C. p.17-13.
#'
#'   USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
#'   EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
#'   U.S. Environmental Protection Agency, Washington, D.C.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Chrysene Concentrations from Example 17-3 of 2009 USEPA Guidance Document
#' @name EPA.09.Ex.17.3.chrysene.df
#' @description
#' Chrysene concentrations (ppb) at two background and three compliance wells (four monthly measures at each well).
#' @usage
#' EPA.09.Ex.17.3.chrysene.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 20 observations on the following 4 variables.
#'   \describe{
#'     \item{\code{Month}}{a factor indicating the month of collection}
#'     \item{\code{Well}}{a factor indicating the well number}
#'     \item{\code{Well.type}}{a factor indicating the well type (background vs. compliance)}
#'     \item{\code{Chrysene.ppb}}{a numeric vector of chrysene concentrations (ppb)}
#'   }
#' }
#' @rawRd
#' \source{
#'   USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
#'   EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.
#'   U.S. Environmental Protection Agency, Washington, D.C. p.17-17.
#'
#'   USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
#'   EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
#'   U.S. Environmental Protection Agency, Washington, D.C.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Log-transformed Chrysene Concentrations from Example 17-3 of 2009 USEPA Guidance Document
#' @name EPA.09.Ex.17.3.log.chrysene.df
#' @description
#' Log-transformed chrysene concentrations (ppb) at two background and three compliance wells (four monthly measures at each well).
#' @usage
#' EPA.09.Ex.17.3.log.chrysene.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 20 observations on the following 4 variables.
#'   \describe{
#'     \item{\code{Month}}{a factor indicating the month of collection}
#'     \item{\code{Well}}{a factor indicating the well number}
#'     \item{\code{Well.type}}{a factor indicating the well type (background vs. compliance)}
#'     \item{\code{Log.Chrysene.ppb}}{a numeric vector of log-transformed chrysene concentrations (ppb)}
#'   }
#' }
#' @rawRd
#' \source{
#'   USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
#'   EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.
#'   U.S. Environmental Protection Agency, Washington, D.C. p.17-18.
#'
#'   USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
#'   EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
#'   U.S. Environmental Protection Agency, Washington, D.C.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Copper Concentrations from Example 17-4 of 2009 USEPA Guidance Document
#' @name EPA.09.Ex.17.4.copper.df
#' @description
#' Copper concentrations (ppb) at three background and two compliance wells
#'   (eight monthly measures at the background wells, four monthly measures at the compliance wells).
#' @usage
#' EPA.09.Ex.17.4.copper.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 40 observations on the following 6 variables.
#'   \describe{
#'     \item{\code{Month}}{a factor indicating the month of collection}
#'     \item{\code{Well}}{a factor indicating the well number}
#'     \item{\code{Well.type}}{a factor indicating the well type (background vs. compliance)}
#'     \item{\code{Copper.ppb.orig}}{a character vector of original copper concentrations (ppb)}
#'     \item{\code{Copper.ppb}}{a numeric vector of copper concentrations with nondetects set to their detection limit}
#'     \item{\code{Censored}}{a logical vector indicating which observations are censored}
#'   }
#' }
#' @rawRd
#' \source{
#'   USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
#'   EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.
#'   U.S. Environmental Protection Agency, Washington, D.C. p.17-21.
#'
#'   USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
#'   EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
#'   U.S. Environmental Protection Agency, Washington, D.C.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Chloride Concentrations from Example 17-5 of 2009 USEPA Guidance Document
#' @name EPA.09.Ex.17.5.chloride.df
#' @description
#' Chloride concentrations (ppm) collected over a five-year period at a solid waste landfill.
#' @usage
#' EPA.09.Ex.17.5.chloride.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 19 observations on the following 4 variables.
#'   \describe{
#'     \item{\code{Date}}{a Date object indicating the date of collection}
#'     \item{\code{Chloride.ppm}}{a numeric vector of chloride concentrations (ppm)}
#'     \item{\code{Elapsed.Days}}{a numeric vector indicating the number of days since January 1, 2002}
#'     \item{\code{Residuals}}{a numeric vector of residuals from a linear regression trend fit}
#'   }
#' }
#' @rawRd
#' \source{
#'   USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
#'   EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.
#'   U.S. Environmental Protection Agency, Washington, D.C. p.17-26.
#'
#'   USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
#'   EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
#'   U.S. Environmental Protection Agency, Washington, D.C.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Sulfate Concentrations from Example 17-6 of 2009 USEPA Guidance Document
#' @name EPA.09.Ex.17.6.sulfate.df
#' @description
#' Sulfate concentrations (ppm) collected over several years.
#'   The date of collection is simply indicated by month and year of collection.
#'   The column \code{Date} is a Date object where the day of the month has been arbitrarily set to 1.
#' @usage
#' EPA.09.Ex.17.6.sulfate.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 23 observations on the following 6 variables.
#'   \describe{
#'     \item{\code{Sample.No}}{a numeric vector indicating the sample number}
#'     \item{\code{Year}}{a numeric vector indicating the year of collection}
#'     \item{\code{Month}}{a numeric vector indicating the month of collection}
#'     \item{\code{Sampling.Date}}{a numeric vector indicating the year and month of collection}
#'     \item{\code{Date}}{a Date object indicating the date of collection, where the day of the month is arbitrarily set to 1}
#'     \item{\code{Sulfate.ppm}}{a numeric vector of sulfate concentrations (ppm)}
#'   }
#' }
#' @rawRd
#' \source{
#'   USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
#'   EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.
#'   U.S. Environmental Protection Agency, Washington, D.C. p.17-33.
#'
#'   USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
#'   EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
#'   U.S. Environmental Protection Agency, Washington, D.C.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Sodium Concentrations from Example 17-7 of 2009 USEPA Guidance Document
#' @name EPA.09.Ex.17.7.sodium.df
#' @description
#' Sodium concentrations (ppm) collected over several years.  The sample dates are
#'   recorded as the year of collection (2-digit format) plus a fractional part indicating when during the year the sample was collected.
#' @usage
#' EPA.09.Ex.17.7.sodium.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 10 observations on the following 2 variables.
#'   \describe{
#'     \item{\code{Year}}{a numeric vector indicating the year of collection (a fractional number)}
#'     \item{\code{Sodium.ppm}}{a numeric vector of sodium concentrations (ppm)}
#'   }
#' }
#' @rawRd
#' \source{
#'   USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
#'   EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.
#'   U.S. Environmental Protection Agency, Washington, D.C. p.17-36.
#'
#'   USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
#'   EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
#'   U.S. Environmental Protection Agency, Washington, D.C.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Arsenic Concentrations from Example 18-1 of 2009 USEPA Guidance Document
#' @name EPA.09.Ex.18.1.arsenic.df
#' @description
#' Arsenic concentrations (ppb) in a single well at a solid waste landfill.  Four
#'   observations per year over four years.  Years 1-3 are the background period and
#'   Year 4 is the compliance period.
#' @usage
#' EPA.09.Ex.18.1.arsenic.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 16 observations on the following 3 variables.
#'   \describe{
#'     \item{\code{Year}}{a factor indicating the year of collection}
#'     \item{\code{Sampling.Period}}{a factor indicating the sampling period (background vs. compliance)}
#'     \item{\code{Arsenic.ppb}}{a numeric vector of arsenic concentrations (ppb)}
#'   }
#' }
#' @rawRd
#' \source{
#'   USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
#'   EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.
#'   U.S. Environmental Protection Agency, Washington, D.C. p.18-10.
#'
#'   USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
#'   EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
#'   U.S. Environmental Protection Agency, Washington, D.C.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Chrysene Concentrations from Example 18-2 of 2009 USEPA Guidance Document
#' @name EPA.09.Ex.18.2.chrysene.df
#' @description
#' Chrysene concentrations (ppb) at two background wells and one compliance well (four monthly measures at each well).
#' @usage
#' EPA.09.Ex.18.2.chrysene.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 12 observations on the following 4 variables.
#'   \describe{
#'     \item{\code{Month}}{a factor indicating the month of collection}
#'     \item{\code{Well}}{a factor indicating the well number}
#'     \item{\code{Well.type}}{a factor indicating the well type (background vs. compliance)}
#'     \item{\code{Chrysene.ppb}}{a numeric vector of chrysene concentrations (ppb)}
#'   }
#' }
#' @rawRd
#' \source{
#'   USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
#'   EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.
#'   U.S. Environmental Protection Agency, Washington, D.C. p.18-15.
#'
#'   USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
#'   EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
#'   U.S. Environmental Protection Agency, Washington, D.C.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Trichloroethylene Concentrations from Example 18-3 of 2009 USEPA Guidance Document
#' @name EPA.09.Ex.18.3.TCE.df
#' @description
#' Trichloroethylene (TCE) concentrations (ppb) at three background wells and one
#'   compliance well.  Six monthly measures at each background well, three monthly
#'   measures at the compliance well.
#' @usage
#' EPA.09.Ex.18.3.TCE.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 24 observations on the following 6 variables.
#'   \describe{
#'     \item{\code{Month}}{a factor indicating the month of collection}
#'     \item{\code{Well}}{a factor indicating the well number}
#'     \item{\code{Well.type}}{a factor indicating the well type (background vs. compliance)}
#'     \item{\code{TCE.ppb.orig}}{a character vector of original TCE concentrations (ppb)}
#'     \item{\code{TCE.ppb}}{a numeric vector of TCE concentrations (ppb) with nondetects set to their detection limit}
#'     \item{\code{Censored}}{a logical vector indicating which observations are censored}
#'   }
#' }
#' @rawRd
#' \source{
#'   USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
#'   EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.
#'   U.S. Environmental Protection Agency, Washington, D.C. p.18-19.
#'
#'   USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
#'   EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
#'   U.S. Environmental Protection Agency, Washington, D.C.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Xylene Concentrations from Example 18-4 of 2009 USEPA Guidance Document
#' @name EPA.09.Ex.18.4.xylene.df
#' @description
#' Xylene concentrations (ppb) at three background wells and one compliance well.  Eight
#'   monthly measures at each complaince well; three monthly measures at the compliance well.
#' @usage
#' EPA.09.Ex.18.4.xylene.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 32 observations on the following 6 variables.
#'   \describe{
#'     \item{\code{Month}}{a factor indicating the month of collection}
#'     \item{\code{Well}}{a factor indicating the well number}
#'     \item{\code{Well.type}}{a factor indicating the well type (background vs. compliance)}
#'     \item{\code{Xylene.ppb.orig}}{a character vector of original xylene concentrations (ppb)}
#'     \item{\code{Xylene.ppb}}{a numeric vector of xylene concentrations (ppb) with nondetects set to their detection limit}
#'     \item{\code{Censored}}{a logical vector indicating which observations are censored}
#'   }
#' }
#' @rawRd
#' \source{
#'   USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
#'   EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.
#'   U.S. Environmental Protection Agency, Washington, D.C. p.18-22.
#'
#'   USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
#'   EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
#'   U.S. Environmental Protection Agency, Washington, D.C.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Sulfate Concentrations from Example 19-1 of 2009 USEPA Guidance Document
#' @name EPA.09.Ex.19.1.sulfate.df
#' @description
#' Sulfate concentrations (mg/L) at four background wells.
#' @usage
#' EPA.09.Ex.19.1.sulfate.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 25 observations on the following 7 variables.
#'   \describe{
#'     \item{\code{Well}}{a factor indicating the well number}
#'     \item{\code{Month}}{a numeric vector indicating the month of collection}
#'     \item{\code{Day}}{a numeric vector indicating the day of the month of collection}
#'     \item{\code{Year}}{a numeric vector indicating the year of collection}
#'     \item{\code{Date}}{a Date object indicating the date of collection}
#'     \item{\code{Sulfate.mg.per.l}}{a numeric vector of sulfate concentrations (mg/L)}
#'     \item{\code{log.Sulfate.mg.per.l}}{a numeric vector of log-transformed sulfate concentrations (mg/L)}
#'   }
#' }
#' @rawRd
#' \source{
#'   USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
#'   EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.
#'   U.S. Environmental Protection Agency, Washington, D.C. p.19-17.
#'
#'   USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
#'   EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
#'   U.S. Environmental Protection Agency, Washington, D.C.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Chloride Concentrations from Example 19-2 of 2009 USEPA Guidance Document
#' @name EPA.09.Ex.19.2.chloride.df
#' @description
#' Chloride concentrations (mg/L) at 10 compliance wells at a solid waste landfill.
#'   One year of quarterly measures at each well.
#' @usage
#' EPA.09.Ex.19.2.chloride.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 40 observations on the following 2 variables.
#'   \describe{
#'     \item{\code{Well}}{a factor indicating the well number}
#'     \item{\code{Chloride.mg.per.l}}{a numeric vector of chloride concentrations (mg/L)}
#'   }
#' }
#' @rawRd
#' \source{
#'   USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
#'   EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.
#'   U.S. Environmental Protection Agency, Washington, D.C. p.19-19.
#'
#'   USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
#'   EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
#'   U.S. Environmental Protection Agency, Washington, D.C.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Mercury Concentrations from Example 19-5 of 2009 USEPA Guidance Document
#' @name EPA.09.Ex.19.5.mercury.df
#' @description
#' Mercury concentrations (ppb) at four background and two compliance wells.
#' @usage
#' EPA.09.Ex.19.5.mercury.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 36 observations on the following 6 variables.
#'   \describe{
#'     \item{\code{Event}}{a factor indicating the time of collection}
#'     \item{\code{Well}}{a factor indicating the well number}
#'     \item{\code{Well.type}}{a factor indicating the well type (background vs. compliance)}
#'     \item{\code{Mercury.ppb.orig}}{a character vector of original mercury concentrations (ppb)}
#'     \item{\code{Mercury.ppb}}{a numeric vector of mercury concentrations (ppb) with nondetects set to their detection limit}
#'     \item{\code{Censored}}{a logical vector indicating which observations are censored}
#'   }
#' }
#' @rawRd
#' \source{
#'   USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
#'   EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.
#'   U.S. Environmental Protection Agency, Washington, D.C. p.19-33.
#'
#'   USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
#'   EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
#'   U.S. Environmental Protection Agency, Washington, D.C.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Nickel Concentrations from Example 20-1 of 2009 USEPA Guidance Document
#' @name EPA.09.Ex.20.1.nickel.df
#' @description
#' Nickel concentrations (ppb) at a single well.  Eight monthly measures during
#'   the background period and eight monthly measures during the compliance period.
#' @usage
#' EPA.09.Ex.20.1.nickel.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 16 observations on the following 4 variables.
#'   \describe{
#'     \item{\code{Month}}{a factor indicating the month of collection}
#'     \item{\code{Year}}{a factor indicating the year of collection}
#'     \item{\code{Period}}{a factor indicating the period (baseline vs. compliance)}
#'     \item{\code{Nickel.ppb}}{a numeric vector of nickel concentrations (ppb)}
#'   }
#' }
#' @rawRd
#' \source{
#'   USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
#'   EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.
#'   U.S. Environmental Protection Agency, Washington, D.C. p.20-4.
#'
#'   USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
#'   EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
#'   U.S. Environmental Protection Agency, Washington, D.C.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Aldicarb Concentrations from Example 21-1 of 2009 USEPA Guidance Document
#' @name EPA.09.Ex.21.1.aldicarb.df
#' @description
#' Aldicarb concentrations (ppb) at three compliance wells (four monthly measures at each well).
#' @usage
#' EPA.09.Ex.21.1.aldicarb.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 12 observations on the following 3 variables.
#'   \describe{
#'     \item{\code{Month}}{a factor indicating the month of collection}
#'     \item{\code{Well}}{a factor indicating the well number}
#'     \item{\code{Aldicarb.ppb}}{a numeric vector of aldicarb concentrations (ppb)}
#'   }
#' }
#' @rawRd
#' \source{
#'   USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
#'   EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.
#'   U.S. Environmental Protection Agency, Washington, D.C. p.21-4.
#'
#'   USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
#'   EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
#'   U.S. Environmental Protection Agency, Washington, D.C.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Benzene Concentrations from Example 21-2 of 2009 USEPA Guidance Document
#' @name EPA.09.Ex.21.2.benzene.df
#' @description
#' Benzene concentrations (ppb) collected at a landfill that previously handled smelter waste and is now undergoing remediation efforts.
#' @usage
#' EPA.09.Ex.21.2.benzene.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 8 observations on the following 4 variables.
#'   \describe{
#'     \item{\code{Month}}{a numeric vector indicating the month of collection}
#'     \item{\code{Benzene.ppb.orig}}{a character vector of original benzene concentrations (ppb)}
#'     \item{\code{Benzene.ppb}}{a numeric vector of benzene concentrations (ppb) with nondetects set to their detection limit}
#'     \item{\code{Censored}}{a logical vector indicating which observations are censored}
#'   }
#' }
#' @rawRd
#' \source{
#'   USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
#'   EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.
#'   U.S. Environmental Protection Agency, Washington, D.C. p.21-7.
#'
#'   USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
#'   EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
#'   U.S. Environmental Protection Agency, Washington, D.C.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Beryllium Concentrations from Example 21-5 of 2009 USEPA Guidance Document
#' @name EPA.09.Ex.21.5.beryllium.df
#' @description
#' Beryllium concentrations (ppb) at one well (four years of quarterly measures).
#' @usage
#' data(EPA.09.Ex.21.5.beryllium.df)
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 16 observations on the following 3 variables.
#'   \describe{
#'     \item{\code{Year}}{a factor indicating the year of collection}
#'     \item{\code{Quarter}}{a factor indicating the quarter of collection}
#'     \item{\code{Beryllium.ppb}}{a numeric vector of beryllium concentrations (ppb)}
#'   }
#' }
#' @rawRd
#' \source{
#'   USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
#'   EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.
#'   U.S. Environmental Protection Agency, Washington, D.C. p.21-18.
#'
#'   USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
#'   EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
#'   U.S. Environmental Protection Agency, Washington, D.C.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Nitrate Concentrations from Example 21-6 of 2009 USEPA Guidance Document
#' @name EPA.09.Ex.21.6.nitrate.df
#' @description
#' Nitrate concentrations (mg/L) at a well used for drinking water.
#' @usage
#' EPA.09.Ex.21.6.nitrate.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 12 observations on the following 5 variables.
#'   \describe{
#'     \item{\code{Sampling.Date}}{a character vector indicating the sampling date}
#'     \item{\code{Date}}{a Date object indicating the sampling date}
#'     \item{\code{Nitrate.mg.per.l.orig}}{a character vector of original nitrate concentrations (mg/L)}
#'     \item{\code{Nitrate.mg.per.l}}{a numeric vector of nitrate concentrations (mg/L) with nondetects set to their detection limit}
#'     \item{\code{Censored}}{a logical vector indicating which observations are censored}
#'   }
#' }
#' @rawRd
#' \source{
#'   USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
#'   EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.
#'   U.S. Environmental Protection Agency, Washington, D.C. p.21-22.
#'
#'   USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
#'   EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
#'   U.S. Environmental Protection Agency, Washington, D.C.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Trichloroethylene Concentrations from Example 21-7 of 2009 USEPA Guidance Document
#' @name EPA.09.Ex.21.7.TCE.df
#' @description
#' Trichloroethylene (TCE) concentrations (ppb) at a site undergoing remediation.
#' @usage
#' EPA.09.Ex.21.7.TCE.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 10 observations on the following 2 variables.
#'   \describe{
#'     \item{\code{Month}}{a numeric vector indicating the month of collection}
#'     \item{\code{TCE.ppb}}{a numeric vector of TCE concentrations (ppb)}
#'   }
#' }
#' @rawRd
#' \source{
#'   USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
#'   EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.
#'   U.S. Environmental Protection Agency, Washington, D.C. p.21-26.
#'
#'   USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
#'   EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
#'   U.S. Environmental Protection Agency, Washington, D.C.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Vinyl Chloride Concentrations from Example 22-1 of 2009 USEPA Guidance Document
#' @name EPA.09.Ex.22.1.VC.df
#' @description
#' Vinyl Chloride (VC) concentrations (ppb) during detection monitoring for two
#'   compliance wells.  Four years of quarterly measures at each well.  Compliance
#'   monitoring began with Year 2 of the sampling record.
#' @usage
#' EPA.09.Ex.22.1.VC.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 32 observations on the following 5 variables.
#'   \describe{
#'     \item{\code{Year}}{a factor indicating the year of collection}
#'     \item{\code{Quarter}}{a factor indicating the quarter of collection}
#'     \item{\code{Period}}{a factor indicating the period (background vs. compliance)}
#'     \item{\code{Well}}{a factor indicating the well number}
#'     \item{\code{VC.ppb}}{a numeric vector of VC concentrations (ppb)}
#'   }
#' }
#' @rawRd
#' \source{
#'   USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
#'   EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.
#'   U.S. Environmental Protection Agency, Washington, D.C. p.22-6.
#'
#'   USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
#'   EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
#'   U.S. Environmental Protection Agency, Washington, D.C.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Specific Conductance from Example 22-2 of 2009 USEPA Guidance Document
#' @name EPA.09.Ex.22.2.Specific.Conductance.df
#' @description
#' Specific conductance (\eqn{\mu}mho) collected over several years at two wells at a hazardous waste facility.
#' @usage
#' EPA.09.Ex.22.2.Specific.Conductance.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 43 observations on the following 3 variables.
#'   \describe{
#'     \item{\code{Well}}{a factor indicating the well number}
#'     \item{\code{Date}}{a Date object indicating the date of collection}
#'     \item{\code{Specific.Conductance.umho}}{a numeric vector of specific conductance (\eqn{\mu}mho)}
#'   }
#' }
#' @rawRd
#' \source{
#'   USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
#'   EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.
#'   U.S. Environmental Protection Agency, Washington, D.C. p.22-11.
#'
#'   USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
#'   EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
#'   U.S. Environmental Protection Agency, Washington, D.C.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Sulfate Concentrations from Example 6-3 of 2009 USEPA Guidance Document
#' @name EPA.09.Ex.6.3.sulfate.df
#' @description
#' Sulfate concentrations (ppm) at two background wells (five quarterly measures at each well).
#' @usage
#' EPA.09.Ex.6.3.sulfate.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 10 observations on the following 4 variables.
#'   \describe{
#'     \item{\code{Month}}{a numeric vector indicating the month the observations was taken}
#'     \item{\code{Year}}{a numeric vector indicating the year the observation was taken}
#'     \item{\code{Well}}{a factor indicating the well number}
#'     \item{\code{Sulfate.ppm}}{a numeric vector of sulfate concentrations (ppm)}
#'   }
#' }
#' @rawRd
#' \source{
#'   USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
#'   EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.
#'   U.S. Environmental Protection Agency, Washington, D.C. p.6-20.
#'
#'   USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
#'   EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
#'   U.S. Environmental Protection Agency, Washington, D.C.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Arsenic concentrations from Example 7.1 of 2009 USEPA Guidance Document
#' @name EPA.09.Ex.7.1.arsenic.df
#' @description
#' Arsenic concentrations (\eqn{\mu}g/L) at a single well, consisting of:
#'   8 historical observations,
#'   4 future observations for Case 1, and
#'   4 future observations for Case 2.
#' @usage
#' EPA.09.Ex.7.1.arsenic.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 16 observations on the following 2 variables.
#'   \describe{
#'     \item{\code{Data.Source}}{a factor with levels \code{Historical}, \code{Case.1}, \code{Case.2}}
#'     \item{\code{Arsenic.ug.per.l}}{a numeric vector of arsenic concentrations (\eqn{\mu}g/L)}
#'   }
#' }
#' @rawRd
#' \source{
#'   USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
#'   EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.
#'   U.S. Environmental Protection Agency, Washington, D.C. p.7-26.
#'
#'   USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
#'   EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
#'   U.S. Environmental Protection Agency, Washington, D.C.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Trichloroethene concentrations in Table 9.1 of 2009 USEPA Guidance Document
#' @name EPA.09.Table.9.1.TCE.df
#' @description
#' Time series of trichloroethene (TCE) concentrations (mg/L) taken at 2 separate
#'   wells.  Some observations are annotated with a data qualifier of \code{U} (nondetect)
#'   or \code{J} (estimated detected concentration).
#' @usage
#' EPA.09.Table.9.1.TCE.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 30 observations on the following 5 variables.
#'   \describe{
#'     \item{\code{Date.Collected}}{a factor indicating the date of collection}
#'     \item{\code{Date}}{a Date object indicating the date of collection}
#'     \item{\code{Well}}{a factor indicating the well number}
#'     \item{\code{TCE.mg.per.L}}{a numeric vector indicating the TCE concnetrations (mg/L)}
#'     \item{\code{Data.Qualifier}}{a factor indicating the data qualifier}
#'   }
#' }
#' @rawRd
#' \source{
#'   USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
#'   EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.
#'   U.S. Environmental Protection Agency, Washington, D.C. p.9-3.
#'
#'   USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
#'   EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
#'   U.S. Environmental Protection Agency, Washington, D.C.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Arsenic, Mercury and Strontium Concentrations in Table 9-3 of 2009 USEPA Guidance Document
#' @name EPA.09.Table.9.3.df
#' @description
#' Arsenic, mercury, and strontium concentrations (mg/L) from a single well
#'   collected approximately quarterly.  Nondetects are indicated by the
#'   data qualifier \code{U}.
#' @usage
#' EPA.09.Table.9.3.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 15 observations on the following 8 variables.
#'   \describe{
#'     \item{\code{Date.Collected}}{a factor indicating the date of collection}
#'     \item{\code{Date}}{a Date object indicating the date of collection}
#'     \item{\code{Arsenic.mg.per.L}}{a numeric vector of arsenic concentrations (mg/L)}
#'     \item{\code{Arsenic.Data.Qualifier}}{a factor indicating the data qualifier for arsenic}
#'     \item{\code{Mercury.mg.per.L}}{a numeric vector of mercury concentrations (mg/L)}
#'     \item{\code{Mercury.Data.Qualifier}}{a factor indicating the data qualifier for mercury}
#'     \item{\code{Strontium.mg.per.L}}{a numeric vector of strontium concentrations}
#'     \item{\code{Strontium.Data.Qualifier}}{a factor indicating the data qualifier for strontium}
#'   }
#' }
#' @rawRd
#' \source{
#'   USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
#'   EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.
#'   U.S. Environmental Protection Agency, Washington, D.C. p.9-13.
#'
#'   USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
#'   EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
#'   U.S. Environmental Protection Agency, Washington, D.C.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Nickel Concentrations in Table 9-4 of 2009 USEPA Guidance Document
#' @name EPA.09.Table.9.4.nickel.vec
#' @description
#' Nickel concentrations (ppb) from a single well.
#' @usage
#' EPA.09.Table.9.4.nickel.vec
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   a numeric vector of nickel concentrations (ppb)
#' }
#' @rawRd
#' \source{
#'   USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
#'   EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.
#'   U.S. Environmental Protection Agency, Washington, D.C. p.9-18.
#'
#'   USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
#'   EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
#'   U.S. Environmental Protection Agency, Washington, D.C.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Aldicarb Concentrations from 1989 USEPA Guidance Document
#' @name EPA.89b.aldicarb1.df
#' @description
#' Aldicarb concentrations (ppb) at three compliance wells (four monthly samples at each well).
#' @usage
#' EPA.89b.aldicarb1.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 12 observations on the following 3 variables.
#'   \describe{
#'     \item{\code{Aldicarb}}{Aldicarb concentrations (ppb)}
#'     \item{\code{Month}}{a factor indicating the month of collection}
#'     \item{\code{Well}}{a factor indicating the well number}
#'   }
#' }
#' @rawRd
#' \source{
#'   USEPA. (1989b). \emph{Statistical Analysis of Ground-Water Monitoring Data at RCRA Facilities, Interim Final Guidance}.
#'   EPA/530-SW-89-026. Office of Solid Waste, U.S. Environmental Protection Agency,
#'   Washington, D.C. p.6-4.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Aldicarb Concentrations from 1989 USEPA Guidance Document
#' @name EPA.89b.aldicarb2.df
#' @aliases Aldicarb
#' @description
#' Aldicarb concentrations (ppm) at three compliance wells (four monthly samples at each well).
#' @usage
#' EPA.89b.aldicarb2.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 12 observations on the following 3 variables.
#'   \describe{
#'     \item{\code{Aldicarb}}{Aldicarb concentrations (ppm)}
#'     \item{\code{Month}}{a factor indicating the month of collection}
#'     \item{\code{Well}}{a factor indicating the well number}
#'   }
#' }
#' @rawRd
#' \source{
#'   USEPA. (1989b). \emph{Statistical Analysis of Ground-Water Monitoring Data at RCRA Facilities, Interim Final Guidance}.
#'   EPA/530-SW-89-026. Office of Solid Waste, U.S. Environmental Protection Agency,
#'   Washington, D.C. p.6-13.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Benzene Concentrations from 1989 USEPA Guidance Document
#' @name EPA.89b.benzene.df
#' @aliases Benzene
#' @description
#' Benzene concentrations (ppm) at one background and five compliance wells (four monthly samples for each well).
#' @usage
#' EPA.89b.benzene.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 24 observations on the following 6 variables.
#'   \describe{
#'     \item{\code{Benzene.orig}}{a character vector of the original observations}
#'     \item{\code{Benzene}}{a numeric vector with \code{<1} observations coded as \code{1}}
#'     \item{\code{Censored}}{a logical vector indicating which observations are censored}
#'     \item{\code{Month}}{a factor indicating the month of collection}
#'     \item{\code{Well}}{a factor indicating the well number}
#'     \item{\code{Well.type}}{a factor indicating the well type (background vs. compliance)}
#'   }
#' }
#' @rawRd
#' \source{
#'   USEPA. (1989b). \emph{Statistical Analysis of Ground-Water Monitoring Data at RCRA Facilities, Interim Final Guidance}. EPA/530-SW-89-026. Office of Solid Waste, U.S. Environmental Protection Agency, Washington, D.C. p.5-18.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Cadmium Concentrations from 1989 USEPA Guidance Document
#' @name EPA.89b.cadmium.df
#' @aliases Cadmium
#' @description
#' Cadmium concentrations (mg/L) at one set of background and one set of compliance
#'   wells.  Nondetects reported as "BDL".  Detection limit not given.
#' @usage
#' EPA.89b.cadmium.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 88 observations on the following 4 variables.
#'   \describe{
#'     \item{\code{Cadmium.orig}}{a character vector of the original cadmium observations (mg/L)}
#'     \item{\code{Cadmium}}{a numeric vector with \code{BDL} coded as \code{0}}
#'     \item{\code{Censored}}{a logical vector indicating which observations are censored}
#'     \item{\code{Well.type}}{a factor indicating the well type (background vs. compliance)}
#'   }
#' }
#' @rawRd
#' \source{
#'   USEPA. (1989b). \emph{Statistical Analysis of Ground-Water Monitoring Data at RCRA Facilities, Interim Final Guidance}. EPA/530-SW-89-026. Office of Solid Waste, U.S. Environmental Protection Agency, Washington, D.C. p.8-6.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Chlordane Concentrations from 1989 USEPA Guidance Document
#' @name EPA.89b.chlordane1.df
#' @aliases Chlordane
#' @description
#' Chlordane concentrations (ppm) in 24 water samples.  Two possible phases:  dissolved
#'   (18 observations) and immiscible (6 observations).
#' @usage
#' EPA.89b.chlordane1.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 24 observations on the following 2 variables.
#'   \describe{
#'     \item{\code{Chlordane}}{Chlordane concentrations (ppm)}
#'     \item{\code{Phase}}{a factor indicating the phase (dissolved vs. immiscible)}
#'   }
#' }
#' @rawRd
#' \source{
#'   USEPA. (1989b). \emph{Statistical Analysis of Ground-Water Monitoring Data at RCRA Facilities, Interim Final Guidance}. EPA/530-SW-89-026. Office of Solid Waste, U.S. Environmental Protection Agency, Washington, D.C. p.4-8.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Chlordane Concentrations from 1989 USEPA Guidance Document
#' @name EPA.89b.chlordane2.df
#' @description
#' Chlordane concentrations (ppb) at one background and one compliance well.  Observations
#'   taken during four separate months over two years.  Four replicates taken for each
#'   \dQuote{month/year/well type} combination.
#' @usage
#' data(EPA.89b.chlordane2.df)
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 32 observations on the following 5 variables.
#'   \describe{
#'     \item{\code{Chlordane}}{Chlordane concentration (ppb)}
#'     \item{\code{Month}}{a factor indicating the month of collection}
#'     \item{\code{Year}}{a numeric vector indicating the year of collection (85 or 86)}
#'     \item{\code{Replicate}}{a factor indicating the replicate number}
#'     \item{\code{Well.type}}{a factor indicating the well type (background vs. compliance)}
#'   }
#' }
#' @rawRd
#' \source{
#'   USEPA. (1989b). \emph{Statistical Analysis of Ground-Water Monitoring Data at RCRA Facilities, Interim Final Guidance}. EPA/530-SW-89-026. Office of Solid Waste, U.S. Environmental Protection Agency, Washington, D.C. p.5-27.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' EDB Concentrations from 1989 USEPA Guidance Document
#' @name EPA.89b.edb.df
#' @description
#' EDB concentrations (ppb) at three compliance wells (four monthly samples at each well).
#' @usage
#' EPA.89b.edb.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 12 observations on the following 3 variables.
#'   \describe{
#'     \item{\code{EDB}}{EDB concentrations (ppb)}
#'     \item{\code{Month}}{a factor indicating the month of collection}
#'     \item{\code{Well}}{a factor indicating the well number}
#'   }
#' }
#' @rawRd
#' \source{
#'   USEPA. (1989b). \emph{Statistical Analysis of Ground-Water Monitoring Data at RCRA Facilities, Interim Final Guidance}. EPA/530-SW-89-026. Office of Solid Waste, U.S. Environmental Protection Agency, Washington, D.C. p.6-6.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Lead Concentrations from 1989 USEPA Guidance Document
#' @name EPA.89b.lead.df
#' @description
#' Lead concentrations (ppm) at two background and four compliance wells
#'   (four monthly samples for each well).
#' @usage
#' EPA.89b.lead.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 24 observations on the following 4 variables.
#'   \describe{
#'     \item{\code{Lead}}{Lead concentrations (ppm)}
#'     \item{\code{Month}}{a factor indicating the month of collection}
#'     \item{\code{Well}}{a factor indicating the well number}
#'     \item{\code{Well.type}}{a factor indicating the well type (background vs. compliance)}
#'   }
#' }
#' @rawRd
#' \source{
#'   USEPA. (1989b). \emph{Statistical Analysis of Ground-Water Monitoring Data at RCRA Facilities, Interim Final Guidance}. EPA/530-SW-89-026. Office of Solid Waste, U.S. Environmental Protection Agency, Washington, D.C. p.5-23.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Log-transformed Lead Concentrations from 1989 USEPA Guidance Document
#' @name EPA.89b.loglead.df
#' @description
#' Log-transformed lead concentrations (\eqn{\mu}g/L) at two background and four
#'   compliance wells (four monthly samples for each well).
#' @usage
#' EPA.89b.loglead.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 24 observations on the following 4 variables.
#'   \describe{
#'     \item{\code{LogLead}}{Natural logarithm of lead concentrations (\eqn{\mu}g/L)}
#'     \item{\code{Month}}{a factor indicating the month of collection}
#'     \item{\code{Well}}{a factor indicating the well number}
#'     \item{\code{Well.type}}{a factor indicating the well type (background vs. compliance)}
#'   }
#' }
#' @rawRd
#' \source{
#'   USEPA. (1989b). \emph{Statistical Analysis of Ground-Water Monitoring Data at RCRA Facilities, Interim Final Guidance}. EPA/530-SW-89-026. Office of Solid Waste, U.S. Environmental Protection Agency, Washington, D.C. p.5-11.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Manganese Concentrations from 1989 USEPA Guidance Document
#' @name EPA.89b.manganese.df
#' @description
#' Manganese concentrations at six monitoring wells (four monthly samples for each well).
#' @usage
#' EPA.89b.manganese.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 24 observations on the following 3 variables.
#'   \describe{
#'     \item{\code{Manganese}}{Manganese concentrations}
#'     \item{\code{Month}}{a factor indicating the month of collection}
#'     \item{\code{Well}}{a factor indicating the well number}
#'   }
#' }
#' @rawRd
#' \source{
#'   USEPA. (1989b). \emph{Statistical Analysis of Ground-Water Monitoring Data at RCRA Facilities, Interim Final Guidance}. EPA/530-SW-89-026. Office of Solid Waste, U.S. Environmental Protection Agency, Washington, D.C. p.4-19.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Sulfate Concentrations from 1989 USEPA Guidance Document
#' @name EPA.89b.sulfate.df
#' @description
#' Sulfate concentrations (mg/L).  Nondetects reported as \code{<1450}.
#' @usage
#' data(EPA.89b.sulfate.df)
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 24 observations on the following 3 variables.
#'   \describe{
#'     \item{\code{Sulfate.orig}}{a character vector of original sulfate concentration (mg/L)}
#'     \item{\code{Sulfate}}{a numeric vector of sulfate concentations with \code{<1450} coded as \code{1450}}
#'     \item{\code{Censored}}{a logical vector indicating which observations are censored}
#'   }
#' }
#' @rawRd
#' \source{
#'   USEPA. (1989b). \emph{Statistical Analysis of Ground-Water Monitoring Data at RCRA Facilities, Interim Final Guidance}.
#'   EPA/530-SW-89-026. Office of Solid Waste, U.S. Environmental Protection Agency, Washington, D.C. p.8-9.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' T-29 Concentrations from 1989 USEPA Guidance Document
#' @name EPA.89b.t29.df
#' @description
#' T-29 concentrations (ppm) at two compliance wells (four monthly samples at each well,
#'   four replicates within each month).  Detection limit is not given.
#' @usage
#' EPA.89b.t29.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 32 observations on the following 6 variables.
#'   \describe{
#'     \item{\code{T29.orig}}{a character vector of the original T-29 concentrations (ppm)}
#'     \item{\code{T29}}{a numeric vector of T-29 concentrations with \code{<?} coded as \code{0}}
#'     \item{\code{Censored}}{a logical vector indicating which observations are censored}
#'     \item{\code{Month}}{a factor indicating the month of collection}
#'     \item{\code{Replicate}}{a factor indicating the replicate number}
#'     \item{\code{Well}}{a factor indicating the well number}
#'   }
#' }
#' @rawRd
#' \source{
#'   USEPA. (1989b). \emph{Statistical Analysis of Ground-Water Monitoring Data at RCRA Facilities, Interim Final Guidance}.
#'   EPA/530-SW-89-026. Office of Solid Waste, U.S. Environmental Protection Agency, Washington, D.C. p.6-10.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Total Organic Carbon Concentrations from 1989 USEPA Guidance Document
#' @name EPA.89b.toc.vec
#' @description
#' Numeric vector containing total organic carbon (TOC) concentrations (mg/L).
#' @usage
#' EPA.89b.toc.vec
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A numeric vector with 19 elements containing TOC concentrations (mg/L).
#' }
#' @rawRd
#' \source{
#'   USEPA. (1989b).
#'   \emph{Statistical Analysis of Ground-Water Monitoring Data at RCRA Facilities, Interim Final Guidance}.
#'   EPA/530-SW-89-026. Office of Solid Waste, U.S. Environmental Protection Agency, Washington, D.C. p.8-13.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Arsenic Concentrations from 1992 USEPA Guidance Document
#' @name EPA.92c.arsenic1.df
#' @description
#' Arsenic concentrations (ppm) at six monitoring wells (four monthly samples for each well).
#' @usage
#' EPA.92c.arsenic1.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 24 observations on the following 3 variables.
#'   \describe{
#'     \item{\code{Arsenic}}{Arsenic concentrations (ppm)}
#'     \item{\code{Month}}{a factor indicating the month of collection}
#'     \item{\code{Well}}{a factor indicating the well number}
#'   }
#' }
#' @rawRd
#' \source{
#'   USEPA. (1992c). \emph{Statistical Analysis of Ground-Water Monitoring Data at RCRA Facilities: Addendum to Interim Final Guidance}. Office of Solid Waste, U.S. Environmental Protection Agency, Washington, D.C.  p.21.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Arsenic Concentrations from 1992 USEPA Guidance Document
#' @name EPA.92c.arsenic2.df
#' @description
#' Arsenic concentrations (ppb) at three background wells and one compliance well
#'   (six monthly samples for each well; first four missing at compliance well).  Nondetects
#'   reported as \code{<5}.
#' @usage
#' EPA.92c.arsenic2.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 24 observations on the following 6 variables.
#'   \describe{
#'     \item{\code{Arsenic.orig}}{a character vector of original arsenic concentrations (ppb)}
#'     \item{\code{Arsenic}}{a numeric vector of arsenic concentrations with \code{<5} coded as \code{5}}
#'     \item{\code{Censored}}{a logical vector indicating which observations are censored}
#'     \item{\code{Month}}{a factor indicating the month of collection}
#'     \item{\code{Well}}{a factor indicating the well number}
#'     \item{\code{Well.type}}{a factor indicating the well type (background vs. compliance)}
#'   }
#' }
#' @rawRd
#' \source{
#'   USEPA. (1992c). \emph{Statistical Analysis of Ground-Water Monitoring Data at RCRA Facilities: Addendum to Interim Final Guidance}. Office of Solid Waste, U.S. Environmental Protection Agency, Washington, D.C.  p.60.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Arsenic Concentrations from 1992 USEPA Guidance Document
#' @name EPA.92c.arsenic3.df
#' @description
#' Arsenic concentrations at one background and one compliance monitoring well.  Three
#'   years of observations for background well, two years of observations for
#'   compliance well, four samples per year for each well.
#' @usage
#' EPA.92c.arsenic3.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 20 observations on the following 3 variables.
#'   \describe{
#'     \item{\code{Arsenic}}{a numeric vector of arsenic concentrations}
#'     \item{\code{Year}}{a factor indicating the year of collection}
#'     \item{\code{Well.type}}{a factor indicating the well type (background vs. compliance)}
#'   }
#' }
#' @rawRd
#' \source{
#'   USEPA. (1992c). \emph{Statistical Analysis of Ground-Water Monitoring Data at RCRA Facilities: Addendum to Interim Final Guidance}. Office of Solid Waste, U.S. Environmental Protection Agency, Washington, D.C.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Benzene Concentrations from 1992 USEPA Guidance Document
#' @name EPA.92c.benzene1.df
#' @description
#' Benzene concentrations (ppb) at six background wells
#'   (six monthly samples for each well).  Nondetects reported as \code{<2}.
#' @usage
#' EPA.92c.benzene1.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 36 observations on the following 5 variables.
#'   \describe{
#'     \item{\code{Benzene.orig}}{a character vector of original benzene concentrations (ppb)}
#'     \item{\code{Benzene}}{a numeric vector of benzene concentrations with \code{<2} coded as \code{2}}
#'     \item{\code{Censored}}{a logical vector indicating which observations are censored}
#'     \item{\code{Month}}{a factor indicating the month of collection}
#'     \item{\code{Well}}{a factor indicating the well number}
#'   }
#' }
#' @rawRd
#' \source{
#'   USEPA. (1992c). \emph{Statistical Analysis of Ground-Water Monitoring Data at RCRA Facilities: Addendum to Interim Final Guidance}. Office of Solid Waste, U.S. Environmental Protection Agency, Washington, D.C.  p.36.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Benzene Concentrations from 1992 USEPA Guidance Document
#' @name EPA.92c.benzene2.df
#' @description
#' Benzene concentrations (ppb) at one background and one compliance well.  Four
#'   observations per month for each well.  Background well sampled in months 1,2, and 3;
#'   compliance well sampled in months 4 and 5.
#' @usage
#' EPA.92c.benzene2.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 20 observations on the following 3 variables.
#'   \describe{
#'     \item{\code{Benzene}}{a numeric vector of benzene concentrations (ppb)}
#'     \item{\code{Month}}{a factor indicating the month of collection}
#'     \item{\code{Well.type}}{a factor indicating the well type (background vs. compliance)}
#'   }
#' }
#' @rawRd
#' \source{
#'   USEPA. (1992c). \emph{Statistical Analysis of Ground-Water Monitoring Data at RCRA Facilities: Addendum to Interim Final Guidance}. Office of Solid Waste, U.S. Environmental Protection Agency, Washington, D.C.  p.56.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Carbon Tetrachloride Concentrations from 1992 USEPA Guidance Document
#' @name EPA.92c.ccl4.df
#' @description
#' Carbon tetrachloride (CCL4) concentrations (ppb) at five wells (four monthly samples at each well).
#' @usage
#' EPA.92c.ccl4.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 20 observations on the following 3 variables.
#'   \describe{
#'     \item{\code{CCL4}}{a numeric vector of carbon tetrachloride concentrations (ppb)}
#'     \item{\code{Month}}{a factor indicating the month of collection}
#'     \item{\code{Well}}{a factor indicating the well number}
#'   }
#' }
#' @rawRd
#' \source{
#'   USEPA. (1992c). \emph{Statistical Analysis of Ground-Water Monitoring Data at RCRA Facilities: Addendum to Interim Final Guidance}. Office of Solid Waste, U.S. Environmental Protection Agency, Washington, D.C.  p.80.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Chrysene Concentrations from 1992 USEPA Guidance Document
#' @name EPA.92c.chrysene.df
#' @description
#' Chrysene concentrations (ppb) at five compliance wells (four monthly samples for each well).
#' @usage
#' EPA.92c.chrysene.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 20 observations on the following 3 variables.
#'   \describe{
#'     \item{\code{Chrysene}}{a numeric vector of chrysene concentrations (ppb)}
#'     \item{\code{Month}}{a factor indicating the month of collection}
#'     \item{\code{Well}}{a factor indicating the well number}
#'   }
#' }
#' @rawRd
#' \source{
#'   USEPA. (1992c). \emph{Statistical Analysis of Ground-Water Monitoring Data at RCRA Facilities: Addendum to Interim Final Guidance}. Office of Solid Waste, U.S. Environmental Protection Agency, Washington, D.C.  p.52.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Copper Concentrations from 1992 USEPA Guidance Document
#' @name EPA.92c.copper1.df
#' @description
#' Copper concentrations (ppb) at two background and one compliance wells (six monthly samples for each well).
#' @usage
#' EPA.92c.copper1.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 18 observations on the following 4 variables.
#'   \describe{
#'     \item{\code{Copper}}{a numeric vector of copper concentrations (ppb)}
#'     \item{\code{Month}}{a factor indicating the month of collection}
#'     \item{\code{Well}}{a factor indicating the well number}
#'     \item{\code{Well.type}}{a factor indicating the well type (background vs. compliance)}
#'   }
#' }
#' @rawRd
#' \source{
#'   USEPA. (1992c). \emph{Statistical Analysis of Ground-Water Monitoring Data at RCRA Facilities: Addendum to Interim Final Guidance}. Office of Solid Waste, U.S. Environmental Protection Agency, Washington, D.C.  p.47.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Copper Concentrations from 1992 USEPA Guidance Document
#' @name EPA.92c.copper2.df
#' @description
#' Copper concentrations (ppb) at three background and two compliance wells
#'   (eight monthly samples for each well; first four missing at compliance wells).
#'   Nondetects reported as \code{<5}.
#' @usage
#' EPA.92c.copper2.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 40 observations on the following 6 variables.
#'   \describe{
#'     \item{\code{Copper.orig}}{a character vector of original copper concentrations (ppb)}
#'     \item{\code{Copper}}{a numeric vector of copper concentrations with \code{<5} coded as \code{5}}
#'     \item{\code{Censored}}{a logical vector indicating which observations are censored}
#'     \item{\code{Month}}{a factor indicating the month of collection}
#'     \item{\code{Well}}{a factor indicating the well number}
#'     \item{\code{Well.type}}{a factor indicating the well type (background vs. compliance)}
#'   }
#' }
#' @rawRd
#' \source{
#'   USEPA. (1992c). \emph{Statistical Analysis of Ground-Water Monitoring Data at RCRA Facilities: Addendum to Interim Final Guidance}. Office of Solid Waste, U.S. Environmental Protection Agency, Washington, D.C.  p.55.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Log-transformed Nickel Concentrations from 1992 USEPA Guidance Document
#' @name EPA.92c.lognickel1.df
#' @description
#' Log-transformed nickel concentrations (ppb) at four monitoring wells (five monthly samples for each well).
#' @usage
#' EPA.92c.lognickel1.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 20 observations on the following 3 variables.
#'   \describe{
#'     \item{\code{LogNickel}}{a numeric vector of log-transformed nickel concentrations (ppb)}
#'     \item{\code{Month}}{a factor indicating the month of collection}
#'     \item{\code{Well}}{a factor indicating the well number}
#'   }
#' }
#' @rawRd
#' \source{
#'   USEPA. (1992c). \emph{Statistical Analysis of Ground-Water Monitoring Data at RCRA Facilities: Addendum to Interim Final Guidance}. Office of Solid Waste, U.S. Environmental Protection Agency, Washington, D.C.  p.15.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Nickel Concentrations from 1992 USEPA Guidance Document
#' @name EPA.92c.nickel1.df
#' @description
#' Nickel concentrations (ppb) at four monitoring wells (five monthly samples for each well).
#' @usage
#' EPA.92c.nickel1.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 20 observations on the following 3 variables.
#'   \describe{
#'     \item{\code{Nickel}}{a numeric vector of nickel concentrations (ppb)}
#'     \item{\code{Month}}{a factor indicating the month of collection}
#'     \item{\code{Well}}{a factor indicating the well number}
#'   }
#' }
#' @rawRd
#' \source{
#'   USEPA. (1992c). \emph{Statistical Analysis of Ground-Water Monitoring Data at RCRA Facilities: Addendum to Interim Final Guidance}. Office of Solid Waste, U.S. Environmental Protection Agency, Washington, D.C.  p.7.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Nickel Concentrations from 1992 USEPA Guidance Document
#' @name EPA.92c.nickel2.df
#' @description
#' Nickel concentrations (ppb) at a monitoring well (eight months of samples, two samples for each sampling occasion).
#' @usage
#' EPA.92c.nickel2.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 16 observations on the following 3 variables.
#'   \describe{
#'     \item{\code{Nickel}}{a numeric vector of nickel concentrations (ppb)}
#'     \item{\code{Month}}{a factor indicating the month of collection}
#'     \item{\code{Sample}}{a factor indicating the sample (replicate) number}
#'   }
#' }
#' @rawRd
#' \source{
#'   USEPA. (1992c). \emph{Statistical Analysis of Ground-Water Monitoring Data at RCRA Facilities: Addendum to Interim Final Guidance}. Office of Solid Waste, U.S. Environmental Protection Agency, Washington, D.C.  p.78.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Toluene Concentrations from 1992 USEPA Guidance Document
#' @name EPA.92c.toluene.df
#' @description
#' Toluene concentrations (ppb) at two background and three compliance wells
#'   (five monthly samples at each well).  Nondetects reported as \code{<5}.
#' @usage
#' EPA.92c.toluene.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 25 observations on the following 6 variables.
#'   \describe{
#'     \item{\code{Toluene.orig}}{a character vector of original toluene concentrations (ppb)}
#'     \item{\code{Toluene}}{a numeric vector of toluene concentrations with \code{<5} coded as \code{5}}
#'     \item{\code{Censored}}{a logical vector indicating which observations are censored}
#'     \item{\code{Month}}{a factor indicating the month of collection}
#'     \item{\code{Well}}{a factor indicating the well number}
#'     \item{\code{Well.type}}{a factor indicating the well type (background vs. compliance)}
#'   }
#' }
#' @rawRd
#' \source{
#'   USEPA. (1992c). \emph{Statistical Analysis of Ground-Water Monitoring Data at RCRA Facilities: Addendum to Interim Final Guidance}. Office of Solid Waste, U.S. Environmental Protection Agency, Washington, D.C.  p.43.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Zinc Concentrations from 1992 USEPA Guidance Document
#' @name EPA.92c.zinc.df
#' @description
#' Zinc concentrations (ppb) at five background wells
#'   (eight samples for each well).  Nondetects reported as \code{<7}.
#' @usage
#' EPA.92c.zinc.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 40 observations on the following 5 variables.
#'   \describe{
#'     \item{\code{Zinc.orig}}{a character vector of original zinc concentrations (ppb)}
#'     \item{\code{Zinc}}{a numeric vector of zinc concentrations with \code{<7} coded as \code{7}}
#'     \item{\code{Censored}}{a logical vector indicating which observations are censored}
#'     \item{\code{Sample}}{a factor indicating the sample number}
#'     \item{\code{Well}}{a factor indicating the well number}
#'   }
#' }
#' @rawRd
#' \source{
#'   USEPA. (1992c). \emph{Statistical Analysis of Ground-Water Monitoring Data at RCRA Facilities: Addendum to Interim Final Guidance}. Office of Solid Waste, U.S. Environmental Protection Agency, Washington, D.C.  p.30.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Chromium Concentrations from 1992 USEPA Guidance Document
#' @name EPA.92d.chromium.df
#' @description
#' Chromium concentrations (mg/kg) in soil samples collected randomly over a Superfund site.
#' @usage
#' EPA.92d.chromium.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 15 observations on the following variable.
#'   \describe{
#'     \item{\code{Cr}}{a numeric vector of chromium concentrations (mg/kg)}
#'   }
#' }
#' @rawRd
#' \source{
#'   USEPA. (1992d). \emph{Supplemental Guidance to RAGS: Calculating the Concentration Term}.
#'   Publication 9285.7-081, May 1992. Intermittent Bulletin, Volume 1, Number 1.
#'   Office of Emergency and Remedial Response, Hazardous Site Evaluation Division, OS-230.
#'   Office of Solid Waste and Emergency Response,
#'   U.S. Environmental Protection Agency, Washington, D.C.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Chromium Concentrations from 1992 USEPA Guidance Document
#' @name EPA.92d.chromium.vec
#' @description
#' Chromium concentrations (mg/kg) in soil samples collected randomly over a Superfund site.
#' @usage
#' EPA.92d.chromium.vec
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A numeric vector with 15 observations.
#' }
#' @rawRd
#' \source{
#'   USEPA. (1992d). \emph{Supplemental Guidance to RAGS: Calculating the Concentration Term}.
#'   Publication 9285.7-081, May 1992. Intermittent Bulletin, Volume 1, Number 1.
#'   Office of Emergency and Remedial Response, Hazardous Site Evaluation Division, OS-230.
#'   Office of Solid Waste and Emergency Response,
#'   U.S. Environmental Protection Agency, Washington, D.C.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Lead Concentrations from 1994 USEPA Guidance Document
#' @name EPA.94b.lead.df
#' @description
#' Lead concentrations (mg/Kg) in soil samples at a reference area and a
#'   cleanup area.  Nondetects reported as \code{<39}.  There are 14 observations
#'   for each area.
#' @usage
#' EPA.94b.lead.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 28 observations on the following 4 variables.
#'   \describe{
#'     \item{\code{Lead.orig}}{a character vector of original lead concentrations (mg/Kg)}
#'     \item{\code{Lead}}{a numeric vector of lead concentrations with \code{<39} coded as \code{39}}
#'     \item{\code{Censored}}{a logical vector indicating which observations are censored}
#'     \item{\code{Area}}{a factor indicating the area (cleanup vs. reference)}
#'   }
#' }
#' @rawRd
#' \source{
#'   USEPA. (1994b). \emph{Statistical Methods for Evaluating the Attainment of Cleanup Standards, Volume 3: Reference-Based Standards for Soils and Solid Media}.
#'   EPA/230-R-94-004. Office of Policy, Planning, and Evaluation, U.S. Environmental Protection Agency, Washington, D.C. pp.6.20--6.21.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' 1,2,3,4-Tetrachlorobenzene Concentrations from 1994 USEPA Guidance Document
#' @name EPA.94b.tccb.df
#' @description
#' 1,2,3,4-Tetrachlorobenzene (TcCB) concentrations (ppb) in soil samples at a
#'   reference area and a cleanup area.  There are 47 observations for the reference area
#'   and 77 for the cleanup area.  There is only one nondetect in the dataset (it's in the
#'   cleanup area), and it is reported as \code{ND}.  Here it is assumed the nondetect is
#'   less than the smallest reported value, which is 0.09 ppb.  Note that on page 6.23 of
#'   USEPA (1994b), a value of 25.5 for the Cleanup Unit was erroneously omitted.
#' @usage
#' EPA.94b.tccb.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 124 observations on the following 4 variables.
#'   \describe{
#'     \item{\code{TcCB.orig}}{a character vector with the original tetrachlorobenzene concentrations (ppb)}
#'     \item{\code{TcCB}}{a numeric vector of tetrachlorobenzene with \code{<0.99} coded as \code{0.99}}
#'     \item{\code{Censored}}{a logical vector indicating which observations are censored}
#'     \item{\code{Area}}{a factor indicating the area (cleanup vs. reference)}
#'   }
#' }
#' @rawRd
#' \source{
#'   USEPA. (1994b). \emph{Statistical Methods for Evaluating the Attainment of Cleanup Standards, Volume 3: Reference-Based Standards for Soils and Solid Media}.
#'   EPA/230-R-94-004. Office of Policy, Planning, and Evaluation, U.S. Environmental Protection Agency, Washington, D.C. pp.6.22-6.25.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Calibration Data for Cadmium at Mass 111
#' @name EPA.97.cadmium.111.df
#' @description
#' Calibration data for cadmium at mass 111 (ng/L; method 1638 ICPMS) that appeared in
#'   Gibbons et al. (1997b) and were provided to them by the U.S. EPA.
#' @usage
#' EPA.97.cadmium.111.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 35 observations on the following 2 variables.
#'   \describe{
#'     \item{\code{Cadmium}}{Observed concentation of cadmium (ng/L)}
#'     \item{\code{Spike}}{\dQuote{True} concentration of cadmium taken from a standard (ng/L)}
#'   }
#' }
#' @rawRd
#' \source{
#'   Gibbons, R.D., D.E. Coleman, and R.F. Maddalone. (1997b). Response to Comment on "An Alternative Minimum Level Definition for Analytical Quantification".
#'   \emph{Environmental Science and Technology}, \bold{31}(12), 3729--3731.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' The Extreme Value (Gumbel) Distribution
#' @name EVD
#' @aliases devd pevd qevd revd
#' @rawRd \alias{Extreme Value Distribution}
#' @rawRd \alias{Gumbel Distribution}
#' @description
#' Density, distribution function, quantile function, and random generation
#'   for the (largest) extreme value distribution.
#' @usage
#' devd(x, location = 0, scale = 1)
#'   pevd(q, location = 0, scale = 1)
#'   qevd(p, location = 0, scale = 1)
#'   revd(n, location = 0, scale = 1)
#' @rawRd
#' \arguments{
#'   \item{x}{
#'   vector of quantiles.
#' }
#'   \item{q}{
#'   vector of quantiles.
#' }
#'   \item{p}{
#'   vector of probabilities between 0 and 1.
#' }
#'   \item{n}{
#'   sample size.  If \code{length(n)} is larger than 1, then \code{length(n)}
#'   random values are returned.
#' }
#'   \item{location}{
#'   vector of location parameters.
#' }
#'   \item{scale}{
#'   vector of positive scale parameters.
#' }
#' }
#' @rawRd
#' \details{
#'   Let \eqn{X} be an extreme value random variable with parameters
#'   \code{location=}\eqn{\eta} and \code{scale=}\eqn{\theta}.
#'   The density function of \eqn{X} is given by:
#'   \deqn{f(x; \eta, \theta) = \frac{1}{\theta} e^{-(x-\eta)/\theta} exp[-e^{-(x-\eta)/\theta}]}
#'   where \eqn{-\infty < x, \eta < \infty} and \eqn{\theta > 0}.
#'
#'   The cumulative distribution function of \eqn{X} is given by:
#'   \deqn{F(x; \eta, \theta) = exp[-e^{-(x-\eta)/\theta}]}
#'
#'   The \eqn{p^{th}} quantile of \eqn{X} is given by:
#'   \deqn{x_{p} = \eta - \theta log[-log(p)]}
#'
#'   The mode, mean, variance, skew, and kurtosis of \eqn{X} are given by:
#'   \deqn{Mode(X) = \eta}
#'   \deqn{E(X) = \eta + \epsilon \theta}
#'   \deqn{Var(X) = \theta^2 \pi^2 / 6}
#'   \deqn{Skew(X) = \sqrt{\beta_1} = 1.139547}
#'   \deqn{Kurtosis(X) = \beta_2 = 5.4}
#'   where \eqn{\epsilon} denotes \link[=EulersConstant]{Euler's constant},
#'   which is equivalent to \code{-\link{digamma}(1)}.
#' }
#' @rawRd
#' \value{
#'   density (\code{devd}), probability (\code{pevd}), quantile (\code{qevd}), or
#'   random sample (\code{revd}) for the extreme value distribution with
#'   location parameter(s) determined by \code{location} and scale
#'   parameter(s) determined by \code{scale}.
#' }
#' @rawRd
#' \references{
#'   Forbes, C., M. Evans, N. Hastings, and B. Peacock. (2011).  Statistical Distributions.
#'   Fourth Edition. John Wiley and Sons, Hoboken, NJ.
#'
#'   Johnson, N. L., S. Kotz, and N. Balakrishnan. (1995).
#'   \emph{Continuous Univariate Distributions, Volume 2}.
#'   Second Edition. John Wiley and Sons, New York.
#' }
#' @rawRd
#' \author{
#'   Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \note{
#'   There are three families of extreme value distributions.  The one
#'   described here is the Type I, also called the Gumbel extreme value
#'   distribution or simply Gumbel distribution.  The name
#'   \dQuote{extreme value} comes from the fact that this distribution is
#'   the limiting distribution (as \eqn{n} approaches infinity) of the
#'   greatest value among \eqn{n} independent random variables each
#'   having the same continuous distribution.
#'
#'   The Gumbel extreme value distribution is related to the
#'   \link[stats:Exponential]{exponential distribution} as follows.
#'   Let \eqn{Y} be an \link[stats:Exponential]{exponential} random variable
#'   with parameter \code{rate=}\eqn{\lambda}.  Then \eqn{X = \eta - log(Y)}
#'   has an extreme value distribution with parameters
#'   \code{location=}\eqn{\eta} and \code{scale=}\eqn{1/\lambda}.
#'
#'   The distribution described above and used by \code{devd}, \code{pevd},
#'   \code{qevd}, and \code{revd} is the \emph{largest} extreme value
#'   distribution.  The smallest extreme value distribution is the limiting
#'   distribution (as \eqn{n} approaches infinity) of the smallest value among
#'   \eqn{n} independent random variables each having the same continuous distribution.
#'   If \eqn{X} has a largest extreme value distribution with parameters \cr
#'   \code{location=}\eqn{\eta} and \code{scale=}\eqn{\theta}, then
#'   \eqn{Y = -X} has a smallest extreme value distribution with parameters
#'   \code{location=}\eqn{-\eta} and \code{scale=}\eqn{\theta}.  The smallest
#'   extreme value distribution is related to the
#'   \link[stats:Weibull]{Weibull distribution} as follows.
#'   Let \eqn{Y} be a \link[stats:Weibull]{Weibull random variable} with parameters
#'   \code{shape=}\eqn{\beta} and \code{scale=}\eqn{\alpha}.  Then \eqn{X = log(Y)}
#'   has a smallest extreme value distribution with parameters \code{location=}\eqn{log(\alpha)}
#'   and \code{scale=}\eqn{1/\beta}.
#'
#'   The extreme value distribution has been used extensively to model the distribution
#'   of streamflow, flooding, rainfall, temperature, wind speed, and other
#'   meteorological variables, as well as material strength and life data.
#' }
#' @rawRd
#' \seealso{
#'   \code{\link{eevd}}, \code{\link{GEVD}},
#'   \link{Probability Distributions and Random Numbers}.
#' }
#' @rawRd
#' \examples{
#'   # Density of an extreme value distribution with location=0, scale=1,
#'   # evaluated at 0.5:
#'
#'   devd(.5)
#'   #[1] 0.3307043
#'
#'   #----------
#'
#'   # The cdf of an extreme value distribution with location=1, scale=2,
#'   # evaluated at 0.5:
#'
#'   pevd(.5, 1, 2)
#'   #[1] 0.2769203
#'
#'   #----------
#'
#'   # The 25'th percentile of an extreme value distribution with
#'   # location=-2, scale=0.5:
#'
#'   qevd(.25, -2, 0.5)
#'   #[1] -2.163317
#'
#'   #----------
#'
#'   # Random sample of 4 observations from an extreme value distribution with
#'   # location=5, scale=2.
#'   # (Note: the call to set.seed simply allows you to reproduce this example.)
#'
#'   set.seed(20)
#'   revd(4, 5, 2)
#'   #[1] 9.070406 7.669139 4.511481 5.903675
#' }
#' @rawRd
#' \keyword{ distribution }
#' @rawRd
#' \keyword{ datagen }
NULL

#' The Empirical Distribution Based on a Set of Observations
#' @name Empirical
#' @aliases demp pemp qemp remp
#' @description
#' Density, distribution function, quantile function, and random generation for
#'   the empirical distribution based on a set of observations
#' @usage
#' demp(x, obs, discrete = FALSE, density.arg.list = NULL)
#'   pemp(q, obs, discrete = FALSE,
#'     prob.method = ifelse(discrete, "emp.probs", "plot.pos"),
#'     plot.pos.con = 0.375)
#'   qemp(p, obs, discrete = FALSE,
#'     prob.method = ifelse(discrete, "emp.probs", "plot.pos"),
#'     plot.pos.con = 0.375)
#'   remp(n, obs)
#' @rawRd
#' \arguments{
#'   \item{x}{
#'   vector of quantiles.
#' }
#'   \item{q}{
#'   vector of quantiles.
#' }
#'   \item{p}{
#'   vector of probabilities between 0 and 1.
#' }
#'   \item{n}{
#'   sample size.  If \code{length(n)} is larger than 1, then \code{length(n)}
#'   random values are returned.
#' }
#'   \item{obs}{
#'   numeric vector of observations.  Missing (\code{NA}), undefined (\code{NaN}), and
#'   infinite (\code{Inf}, \code{-Inf}) values are allowed but will be removed.
#' }
#'   \item{discrete}{
#'   logical scalar indicating whether the assumed parent distribution of \code{x} is
#'   discrete (\code{discrete=TRUE}) or continuous (\code{discrete=FALSE}).  The
#'   default value is \code{FALSE}.
#' }
#'   \item{density.arg.list}{
#'   list with arguments to the \R \code{\link{density}} function.  The default value is
#'   \code{NULL}.  (See the help file for \code{\link{density}}
#'   for more information on the arguments to density.)  The argument
#'   \code{density.arg.list} is ignored if \code{discrete=TRUE}.
#' }
#'   \item{prob.method}{
#'   character string indicating what method to use to compute the empirical
#'   probabilities.  Possible values are \code{"emp.probs"} (empirical probabilities,
#'   default if \code{discrete=TRUE}) and \code{"plot.pos"} (plotting positions,
#'   default if \code{discrete=FALSE}).  See the DETAILS section for more explanation.
#' }
#'   \item{plot.pos.con}{
#'   numeric scalar between 0 and 1 containing the value of the plotting position
#'   constant.  The default value is \code{plot.pos.con=0.375}.  See the DETAILS
#'   section for more information. This argument is ignored if
#'   \code{prob.method="emp.probs"}.
#' }
#' }
#' @rawRd
#' \details{
#'   Let \eqn{x_1, x_2, \ldots, x_n} denote a random sample of n observations
#'   from some unknown probability distribution (i.e., the elements of the argument
#'   \code{obs}), and let \eqn{x_{(i)}} denote the \eqn{i^{th}} order statistic, that is,
#'   the \eqn{i^{th}} largest observation, for \eqn{i = 1, 2, \ldots, n}.
#'
#'   \emph{Estimating Density} \cr
#'   The function \code{demp} computes the empirical probability density function.  If
#'   the observations are assumed to come from a discrete distribution, the probability
#'   density (mass) function is estimated by:
#'   \deqn{\hat{f}(x) = \widehat{Pr}(X = x) = \frac{\sum^n_{i=1} I_{[x]}(x_i)}{n}}
#'   where \eqn{I} is the indicator function:
#'   \tabular{lll}{
#'     \eqn{I_{[x]}(y) =} \tab \eqn{1} \tab if \eqn{y = x}, \cr
#'                        \tab \eqn{0} \tab if \eqn{y \ne x}
#'   }
#'   That is, the estimated probability of observing the value \eqn{x} is simply the
#'   observed proportion of observations equal to \eqn{x}.
#'
#'   If the observations are assumed to come from a continuous distribution, the
#'   function \code{demp} calls the \R function \code{\link{density}} to compute the
#'   estimated density based on the values specified in the argument \code{obs},
#'   and then uses linear interpolation to estimate the density at the values
#'   specified in the argument \code{x}.  See the \R help file for
#'   \code{\link{density}} for more information on how the empirical density is
#'   computed in the continuous case.
#'
#'   \emph{Estimating Probabilities} \cr
#'   The function \code{pemp} computes the estimated cumulative distribution function
#'   (cdf), also called the empirical cdf (ecdf).  If the observations are assumed to
#'   come from a discrete distribution, the value of the cdf evaluated at the \eqn{i^{th}}
#'   order statistic is usually estimated by:
#'   \deqn{\hat{F}[x_{(i)}] = \widehat{Pr}(X \le x_{(i)}) = \hat{p}_i =
#'     \frac{\sum^n_{j=1} I_{(-\infty, x_{(i)}]}(x_j)}{n}}
#'   where:
#'   \tabular{lll}{
#'     \eqn{I_{(-\infty, x]}(y) =} \tab \eqn{1} \tab if \eqn{y \le x}, \cr
#'                                 \tab \eqn{0} \tab if \eqn{y > x}
#'   }
#'   (D'Agostino, 1986a).  That is, the estimated value of the cdf at the \eqn{i^{th}}
#'   order statistic is simply the observed proportion of observations less than or
#'   equal to the \eqn{i^{th}} order statistic.  This estimator is sometimes called the
#'   \dQuote{empirical probabilities} estimator and is intuitively appealing.
#'   The function \code{pemp} uses the above equations to compute the empirical cdf when
#'   \code{prob.method="emp.probs"}.
#'
#'   For any general value of \eqn{x}, when the observations are assumed to come from a
#'   discrete distribution, the value of the cdf is estimated by:
#'   \tabular{lll}{
#'     \eqn{\hat{F}(x) =} \tab \eqn{0}         \tab if \eqn{x < x_{(1)}}, \cr
#'                        \tab \eqn{\hat{p}_i} \tab if \eqn{x_{(i)} \le x < x_{(i+1)}}, \cr
#'                        \tab \eqn{1}         \tab if \eqn{x \ge x_{(n)}}
#'   }
#'   The function \code{pemp} uses the above equation when \code{discrete=TRUE}.
#'
#'   If the observations are assumed to come from a continuous distribution, the value
#'   of the cdf evaluated at the \eqn{i^{th}} order statistic is usually estimated by:
#'   \deqn{\hat{F}[x_{(i)}] = \hat{p}_i = \frac{i - a}{n - 2a + 1}}
#'   where \eqn{a} denotes the plotting position constant and \eqn{0 \le a \le 1}
#'   (Cleveland, 1993, p.18; D'Agostino, 1986a, pp.8,25).  The estimators defined by
#'   the above equation are called \emph{plotting positions} and are used to construct
#'   \link[=qqPlot]{probability plots}.  The function \code{pemp} uses the above equation
#'   when \cr
#'   \code{prob.method="plot.pos"}.
#'
#'   For any general value of \eqn{x}, the value of the cdf is estimated by linear
#'   interpolation:
#'   \tabular{lll}{
#'     \eqn{\hat{F}(x) =} \tab \eqn{\hat{p}_1} \tab if \eqn{x < x_{(1)}}, \cr
#'                        \tab \eqn{(1 - r)\hat{p}_i + r\hat{p}_{i+1}} \tab if \eqn{x_{(i)} \le x < x_{(i+1)}}, \cr
#'                        \tab \eqn{\hat{p}_n} \tab if \eqn{x \ge x_{(n)}}
#'   }
#'   where
#'   \deqn{r = \frac{x - x_{(i)}}{x_{(i+1)} - x_{(i)}}}
#'   (Chambers et al., 1983).  The function \code{pemp} uses the above two equations
#'   when \code{discrete=FALSE}.
#'
#'   \emph{Estimating Quantiles} \cr
#'   The function \code{qemp} computes the estimated quantiles based on the observed
#'   data.  If the observations are assumed to come from a discrete distribution, the
#'   \eqn{p^{th}} quantile is usually estimated by:
#'   \tabular{lll}{
#'     \eqn{\hat{x}_p =} \tab \eqn{x_{(1)}} \tab if \eqn{p \le \hat{p}_1}, \cr
#'                       \tab \eqn{x_{(i)}} \tab if \eqn{\hat{p}_{i-1} < p \le \hat{p}_i}, \cr
#'                       \tab \eqn{x_n}     \tab if \eqn{p > \hat{p}_n}
#'   }
#'   The function \code{qemp} uses the above equation when \code{discrete=TRUE}.
#'
#'   If the observations are assumed to come from a continuous distribution, the
#'   \eqn{p^{th}} quantile is usually estimated by linear interpolation:
#'   \tabular{lll}{
#'     \eqn{\hat{x}_p =} \tab \eqn{x_{(1)}} \tab if \eqn{p \le \hat{p}_1}, \cr
#'                       \tab \eqn{(1 - r)x_{(i-1)} + rx_{(i)}} \tab if \eqn{\hat{p}_{i-1} < p \le \hat{p}_i}, \cr
#'                       \tab \eqn{x_n} \tab if \eqn{p > \hat{p}_n} \cr
#'   }
#'   where
#'   \deqn{r = \frac{p - \hat{p}_{i-1}}{\hat{p}_i - \hat{p}_{i-1}}}
#'   The function \code{qemp} uses the above two equations when \code{discrete=FALSE}.
#'
#'   \emph{Generating Random Numbers From the Empirical Distribution} \cr
#'   The function \code{remp} simply calls the \R function \code{\link{sample}} to
#'   sample the elements of \code{obs} with replacement.
#' }
#' @rawRd
#' \value{
#'   density (\code{demp}), probability (\code{pemp}), quantile (\code{qemp}), or
#'   random sample (\code{remp}) for the empirical distribution based on the data
#'   contained in the vector \code{obs}.
#' }
#' @rawRd
#' \references{
#'   Chambers, J.M., W.S. Cleveland, B. Kleiner, and P.A. Tukey. (1983).
#'   \emph{Graphical Methods for Data Analysis}.  Duxbury Press, Boston, MA,
#'   pp.11--16.
#'
#'   Cleveland, W.S. (1993).  \emph{Visualizing Data}.  Hobart Press, Summit,
#'   New Jersey, 360pp.
#'
#'   D'Agostino, R.B. (1986a).  Graphical Analysis.
#'   In: D'Agostino, R.B., and M.A. Stephens, eds. \emph{Goodness-of Fit Techniques}.
#'   Marcel Dekker, New York, Chapter 2, pp.7--62.
#'
#'   Scott, D. W. (1992).
#'   \emph{Multivariate Density Estimation:  Theory, Practice and Visualization}.
#'   John Wiley and Sons, New York.
#'
#'   Sheather, S. J. and Jones M. C. (1991).  A Reliable Data-Based Bandwidth Selection
#'   Method for Kernel Density Estimation.
#'   \emph{Journal of the Royal Statististical Society B}, 683--690.
#'
#'   Silverman, B.W. (1986).  \emph{Density Estimation for Statistics and Data Analysis}.
#'   Chapman and Hall, London.
#'
#'   Wegman, E.J. (1972).  Nonparametric Probability Density Estimation.
#'   \emph{Technometrics} \bold{14}, 533-546.
#' }
#' @rawRd
#' \author{
#'   Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \note{
#'   The function \code{demp} let's you perform nonparametric density estimation.
#'   The function \code{pemp} computes the value of the empirical cumulative
#'   distribution function (ecdf) for user-specified quantiles.  The ecdf is a
#'   nonparametric estimate of the true cdf (see \code{\link{ecdfPlot}}).  The
#'   function \code{qemp} computes nonparametric estimates of quantiles
#'   (see the help files for \code{\link{eqnpar}} and \code{\link{quantile}}).
#'   The function \code{remp} let's you sample a set of observations with replacement,
#'   which is often done while bootstrapping or performing some other kind of
#'   Monte Carlo simulation.
#' }
#' @rawRd
#' \seealso{
#'   \code{\link{density}}, \code{\link{approx}}, \code{\link{epdfPlot}},
#'   \code{\link{ecdfPlot}}, \code{\link{cdfCompare}}, \code{\link{qqplot}},
#'   \code{\link{eqnpar}}, \code{\link{quantile}}, \code{\link{sample}}, \cr
#'   \code{\link{simulateVector}}, \code{\link{simulateMvMatrix}}.
#' }
#' @rawRd
#' \examples{
#'   # Create a set of 100 observations from a gamma distribution with
#'   # parameters shape=4 and scale=5.
#'   # (Note: the call to set.seed simply allows you to reproduce this example.)
#'
#'   set.seed(3)
#'   obs <- rgamma(100, shape=4, scale=5)
#'
#'   # Now plot the empirical distribution (with a histogram) and the true distribution:
#'
#'   dev.new()
#'   hist(obs, col = "cyan", xlim = c(0, 65), freq = FALSE,
#'     ylab = "Relative Frequency")
#'
#'   pdfPlot('gamma', list(shape = 4, scale = 5), add = TRUE)
#'
#'   box()
#'
#'   # Now plot the empirical distribution (based on demp) with the
#'   # true distribution:
#'
#'   x <- qemp(p = seq(0, 1, len = 100), obs = obs)
#'   y <- demp(x, obs)
#'
#'   dev.new()
#'   plot(x, y, xlim = c(0, 65), type = "n",
#'     xlab = "Value of Random Variable",
#'     ylab = "Relative Frequency")
#'   lines(x, y, lwd = 2, col = "cyan")
#'
#'   pdfPlot('gamma', list(shape = 4, scale = 5), add = TRUE)
#'
#'   # Alternatively, you can create the above plot with the function
#'   # epdfPlot:
#'
#'   dev.new()
#'   epdfPlot(obs, xlim = c(0, 65), epdf.col = "cyan",
#'     xlab = "Value of Random Variable",
#'     main = "Empirical and Theoretical PDFs")
#'
#'   pdfPlot('gamma', list(shape = 4, scale = 5), add = TRUE)
#'
#'
#'
#'
#'   # Clean Up
#'   #---------
#'   rm(obs, x, y)
#'
#' }
#' @rawRd
#' \keyword{ distribution }
#' @rawRd
#' \keyword{ datagen }
NULL

#' Internal EnvStats Objects
#' @name EnvStats-internal
#' @aliases StatNText StatMeanSDText StatMedianIQRText StatTestText
#' @description
#' Internal EnvStats objects
#' @rawRd
#' \details{
#'   These are not to be called by the user.  They have been exported to allow advanced users to
#'   see their structure.
#' }
#' @rawRd
#' \keyword{ internal }
NULL

#' Atmospheric Environmental Conditions in New York City
#' @name Environmental
#' @aliases Environmental.df Air.df
#' @description
#' Daily measurements of ozone concentration, wind speed, temperature, and solar radiation in New York City for 153 consecutive days between May 1 and September 30, 1973.
#' @usage
#' Environmental.df
#'   Air.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   The data frame \code{Environmental.df} has 153 observations on the following 4 variables.
#'   \describe{
#'     \item{\code{ozone}}{Average ozone concentration (of hourly measurements) of in parts per billion.}
#'     \item{\code{radiation}}{Solar radiation (from 08:00 to 12:00) in langleys.}
#'     \item{\code{temperature}}{Maximum daily temperature in degrees Fahrenheit.}
#'     \item{\code{wind}}{Average wind speed (at 07:00 and 10:00) in miles per hour.}
#'   }
#'   Row names are the dates the data were collected.
#'
#'   The data frame \code{Air.df} is the same as \code{Environmental.df} except that the
#'   column \code{ozone} is the cube root of average ozone concentration.
#' }
#' @rawRd
#' \details{
#'   Data on ozone (ppb), solar radiation (langleys), temperature (degrees Fahrenheit), and wind speed (mph)
#'   for 153 consecutive days between May 1 and September 30, 1973.  These data are a superset of the data
#'   contained in the data frame \code{environmental} in the package \pkg{lattice}.
#' }
#' @rawRd
#' \source{Chambers et al. (1983), pp. 347-349.}
#' @rawRd
#' \references{
#'   Chambers, J.M., W.S. Cleveland, B. Kleiner, and P.A. Tukey. (1983). \emph{Graphical Methods for Data Analysis}. Duxbury Press, Boston, MA, 395pp.
#'
#'   Cleveland, W.S. (1993). \emph{Visualizing Data}. Hobart Press, Summit, New Jersey, 360pp.
#'
#'   Cleveland, W.S. (1994). \emph{The Elements of Graphing Data}. Revised Edition. Hobart Press, Summit, New Jersey, 297pp.
#' }
#' @rawRd
#' \examples{
#' # Scatterplot matrix
#' pairs(Environmental.df)
#'
#' pairs(Air.df)
#'
#'
#' # Time series plot for ozone
#' attach(Environmental.df)
#' dates <- as.Date(row.names(Environmental.df), format = "\%m/\%d/\%Y")
#' plot(dates, ozone, type = "l",
#'     xlab = "Time (Year = 1973)", ylab = "Ozone (ppb)",
#'     main = "Time Series Plot of Daily Ozone Measures")
#' detach("Environmental.df")
#' rm(dates)
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Euler's Constant
#' @name EulersConstant
#' @rawRd \alias{Eulers Constant}
#' @description
#' Explanation of Euler's Constant.
#' @rawRd
#' \concept{Euler's Constant}
#' @rawRd
#' \details{
#'   Euler's Constant, here denoted \eqn{\epsilon}, is a real-valued number that can
#'   be defined in several ways.  Johnson et al. (1992, p. 5) use the definition:
#'   \deqn{\epsilon = \lim_{n \to \infty}[1 + \frac{1}{2} + \frac{1}{3} + \ldots + \frac{1}{n} - log(n)]}
#'   and note that it can also be expressed as
#'   \deqn{\epsilon = -\Psi(1)}
#'   where \eqn{\Psi()} is the \link[base:Special]{digamma function}
#'   (Johnson et al., 1992, p.8).
#'
#'   The value of Euler's Constant, to 10 decimal places, is 0.5772156649.
#'
#'   The expression for the mean of a
#'   \link[=EVD]{Type I extreme value (Gumbel) distribution} involves Euler's
#'   constant; hence Euler's constant is used to compute the method of moments
#'   estimators for this distribution (see \code{\link{eevd}}).
#' }
#' @rawRd
#' \references{
#'   Johnson, N. L., S. Kotz, and A.W. Kemp. (1992).
#'   \emph{Univariate Discrete Distributions}.  Second Edition.
#'   John Wiley and Sons, New York, pp.4-8.
#'
#'   Johnson, N. L., S. Kotz, and N. Balakrishnan. (1995).
#'   \emph{Continuous Univariate Distributions, Volume 2}.
#'   Second Edition. John Wiley and Sons, New York.
#' }
#' @rawRd
#' \author{
#'     Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \seealso{
#'   \link[=EVD]{Extreme Value Distribution}, \code{\link{eevd}}.
#' }
#' @rawRd
#' \keyword{ distribution }
NULL

#' EnvStats Functions Listed by Category
#' @name FcnsByCat
#' @rawRd \alias{Functions By Category}
#' @description
#' Hyperlink list of \pkg{EnvStats} functions by category.
#' @rawRd
#' \concept{EnvStats Functions By Category}
#' @rawRd
#' \details{
#'   \itemize{
#'     \item \link[=FcnsByCatCalibration]{Calibration}
#'     \item \link[=FcnsByCatCensoredData]{Censored Data}
#'     \item \link[=FcnsByCatDataTrans]{Data Transformations}
#'     \item \link[=FcnsByCatEstDistParams]{Estimating Distribution Parameters}
#'     \item \link[=FcnsByCatEstDistQuants]{Estimating Distribution Quantiles}
#'     \item \link[=FcnsByCatGOFTests]{Goodness-of-Fit Tests}
#'     \item \link[=FcnsByCatHypothTests]{Hypothesis Tests}
#'     \item \link[=FcnsByCatMCandRisk]{Monte Carlo Simulation and Risk Assessment}
#'     \item \link[=FcnsByCatPlotProbDists]{Plotting Probability Distributions}
#'     \item \link[=FcnsByCatPlotUsingggplot2]{Plotting Using ggplot2}
#'     \item \link[=FcnsByCatPower]{Power and Sample Size Calculations}
#'     \item \link[=FcnsByCatPredInts]{Prediction Intervals}
#'     \item \link[=FcnsByCatPrintPlot]{Printing and Plotting Methods}
#'     \item \link[=FcnsByCatProbDists]{Probability Distributions and Random Numbers}
#'     \item \link[=FcnsByCatSumStats]{Summary Statistics}
#'     \item \link[=FcnsByCatTolInts]{Tolerance Intervals}
#'     \item \link[=FcnsByCatTrend]{Trend Analysis}
#'   }
#' }
#' @rawRd
#' \keyword{ package }
NULL

#' EnvStats Functions for Calibration
#' @name FcnsByCatCalibration
#' @aliases Calibration
#' @description
#' The \pkg{EnvStats} functions listed below are useful for performing calibration and
#'   inverse prediction to determine the concentration of a chemical based on a machine signal.
#' @rawRd
#' \concept{Inverse Prediction}
#' @rawRd
#' \details{
#'   \tabular{ll}{
#'     \emph{Function Name}                  \tab \emph{Description} \cr
#'     \code{\link{anovaPE}}                 \tab Compute lack-of-fit and pure error ANOVA table for a \cr
#'                                           \tab linear model. \cr
#'     \code{\link{calibrate}}               \tab Fit a calibration line or curve. \cr
#'     \code{\link{detectionLimitCalibrate}} \tab Determine detection limit based on using a calibration \cr
#'                                           \tab line (or curve) and inverse regression. \cr
#'     \code{\link{inversePredictCalibrate}} \tab Predict concentration using a calibration line (or curve) \cr
#'                                           \tab and inverse regression. \cr
#'     \code{\link{pointwise}}               \tab Pointwise confidence limits for predictions. \cr
#'     \code{\link{predict.lm}}              \tab Predict method for linear model fits. \cr
#'   }
#' }
#' @rawRd
#' \keyword{ package }
NULL

#' EnvStats Functions for Censored Data
#' @name FcnsByCatCensoredData
#' @rawRd \alias{Censored Data}
#' @rawRd \alias{EnvStats Functions for Censored Data}
#' @rawRd \alias{Functions for Censored Data}
#' @description
#' The \pkg{EnvStats} functions listed below are useful for dealing with Type I censored data.
#' @rawRd
#' \concept{Less Than Detection Limit}
#' @details
#' This help page has been shortened to keep function help focused on usage,
#' arguments, return values, and examples. Extended method details, formulas,
#' and background material are available in \code{vignette("extended-function-details", package = "EnvStats")}, section \code{FcnsByCatCensoredData}.
#' @rawRd
#' \keyword{ package }
NULL

#' EnvStats Functions for Data Transformations
#' @name FcnsByCatDataTrans
#' @rawRd \alias{Data Transformations}
#' @description
#' The \pkg{EnvStats} functions listed below are useful for deciding on data transformations.
#' @rawRd
#' \details{
#'   \tabular{ll}{
#'   \emph{Function Name}                    \tab \emph{Description} \cr
#'   \code{\link{boxcox}}                    \tab Compute values of an objective for Box-Cox transformations, or \cr
#'                                           \tab compute optimal transformation based on raw observations \cr
#'                                           \tab or residuals from a linear model. \cr
#'   \code{\link{boxcoxTransform}}           \tab Apply a Box-Cox Power transformation to a set of data. \cr
#'   \code{\link{plot.boxcox}}               \tab Plotting method for an object of class \code{\link[=boxcox.object]{"boxcox"}}. \cr
#'   \code{\link{plot.boxcoxLm}}             \tab Plotting method for an object of class \code{\link[=boxcoxLm.object]{"boxcoxLm"}}. \cr
#'   \code{\link{print.boxcox}}              \tab Printing method for an object of class \code{\link[=boxcox.object]{"boxcox"}}. \cr
#'   \code{\link{print.boxcoxLm}}            \tab Printing method for an object of class \code{\link[=boxcoxLm.object]{"boxcoxLm"}}. \cr
#'   }
#' }
#' @rawRd
#' \keyword{ package }
NULL

#' EnvStats Functions for Estimating Distribution Parameters
#' @name FcnsByCatEstDistParams
#' @rawRd \alias{Estimating Distribution Parameters}
#' @description
#' The \pkg{EnvStats} functions listed below are useful for estimating distribution parameters
#'   and optionally constructing confidence intervals.
#' @rawRd
#' \concept{Estimation}
#' @rawRd
#' \details{
#'   \tabular{ll}{
#'   \emph{Function Name}                    \tab \emph{Description} \cr
#'   \code{\link{ebeta}}                     \tab Estimate parameters of a \link{Beta} distribution \cr
#'   \code{\link{ebinom}}                    \tab Estimate parameter of a \link{Binomial} distribution \cr
#'   \code{\link{eexp}}                      \tab Estimate parameter of an \link{Exponential} distribution \cr
#'   \code{\link{eevd}}                      \tab Estimate parameters of an \link[=EVD]{Extreme Value} distribution \cr
#'   \code{\link{egamma}}                    \tab Estimate shape and scale parameters of a \link[=GammaDist]{Gamma} distribution \cr
#'   \code{\link{egammaAlt}}                 \tab Estimate mean and CV parameters of a \link[=GammaAlt]{Gamma} distribution \cr
#'   \code{\link{egevd}}                     \tab Estimate parameters of a \link[=GEVD]{Generalized Extreme Value} distribution \cr
#'   \code{\link{egeom}}                     \tab Estimate parameter of a \link{Geometric} distribution \cr
#'   \code{\link{ehyper}}                    \tab Estimate parameter of a \link{Hypergeometric} distribution \cr
#'   \code{\link{elogis}}                    \tab Estimate parameters of a \link{Logistic} distribution \cr
#'   \code{\link{elnorm}}                    \tab Estimate parameters of a \link{Lognormal} distribution (log-scale) \cr
#'   \code{\link{elnormAlt}}                 \tab Estimate parameters of a \link[=LognormalAlt]{Lognormal} distribution (original scale) \cr
#'   \code{\link{elnorm3}}                   \tab Estimate parameters of a \link[=Lognormal3]{Three-Parameter Lognormal} distribution \cr
#'   \code{\link{enbinom}}                   \tab Estimate parameter of a \link[=NegBinomial]{Negative Binomial} distribution \cr
#'   \code{\link{enorm}}                     \tab Estimate parameters of a \link{Normal} distribution \cr
#'   \code{\link{enpar}}                     \tab Estimate Mean, Standard Deviation, and Standard Error Nonparametrically \cr
#'   \code{\link{epareto}}                   \tab Estimate parameters of a \link{Pareto} distribution \cr
#'   \code{\link{epois}}                     \tab Estimate parameter of a \link{Poisson} distribution \cr
#'   \code{\link{eunif}}                     \tab Estimate parameters of a \link{Uniform} distribution \cr
#'   \code{\link{eweibull}}                  \tab Estimate parameters of a \link{Weibull} distribution \cr
#'   \code{\link{ezmlnorm}}                  \tab Estimate parameters of a \link[=DeltaDist]{Zero-Modified Lognormal (Delta)} \cr
#'                                           \tab distribution (log-Scale) \cr
#'   \code{\link{ezmlnormAlt}}               \tab Estimate parameters of a \link[=DeltaDistAlt]{Zero-Modified Lognormal (Delta)} \cr
#'                                           \tab distribution (original Scale) \cr
#'   \code{\link{ezmnorm}}                   \tab Estimate parameters of a \link[=ZeroModifiedNormal]{Zero-Modified Normal} distribution \cr
#'   }
#' }
#' @rawRd
#' \keyword{ package }
NULL

#' EnvStats Functions for Estimating Distribution Quantiles
#' @name FcnsByCatEstDistQuants
#' @rawRd \alias{Estimating Distribution Quantiles}
#' @description
#' The \pkg{EnvStats} functions listed below are useful for estimating distribution quantiles
#'   and, for some functions, optionally constructing confidence intervals for a quantile.
#' @rawRd
#' \concept{Estimation}
#' @rawRd
#' \concept{Quantile}
#' @rawRd
#' \concept{Percentile}
#' @rawRd
#' \details{
#'   \tabular{ll}{
#'   \emph{Function Name}                    \tab \emph{Description} \cr
#'   \code{\link{eqbeta}}                    \tab Estimate quantiles of a \link{Beta} distribution. \cr
#'   \code{\link{eqbinom}}                   \tab Estimate quantiles of a \link{Binomial} distribution. \cr
#'   \code{\link{eqexp}}                     \tab Estimate quantiles of an \link{Exponential} distribution. \cr
#'   \code{\link{eqevd}}                     \tab Estimate quantiles of an \link[=EVD]{Extreme Value} distribution. \cr
#'   \code{\link{eqgamma}}                   \tab Estimate quantiles of a \link[=GammaDist]{Gamma} distribution \cr
#'                                           \tab using the Shape and Scale Parameterization, and optionally \cr
#'                                           \tab construct a confidence interval for a quantile. \cr
#'   \code{\link{eqgammaAlt}}                \tab Estimate quantiles of a \link[=GammaAlt]{Gamma} distribution \cr
#'                                           \tab using the mean and CV Parameterization, and optionally \cr
#'                                           \tab construct a confidence interval for a quantile. \cr
#'   \code{\link{eqgevd}}                    \tab Estimate quantiles of a \link[=GEVD]{Generalized Extreme Value} distribution. \cr
#'   \code{\link{eqgeom}}                    \tab Estimate quantiles of a \link{Geometric} distribution. \cr
#'   \code{\link{eqhyper}}                   \tab Estimate quantiles of a \link{Hypergeometric} distribution. \cr
#'   \code{\link{eqlogis}}                   \tab Estimate quantiles of a \link{Logistic} distribution. \cr
#'   \code{\link{eqlnorm}}                   \tab Estimate quantiles of a \link{Lognormal} distribution (log-scale), \cr
#'                                           \tab and optionally construct a confidence interval for a quantile. \cr
#'   \code{\link{eqlnorm3}}                  \tab Estimate quantiles of a \link[=Lognormal3]{Three-Parameter Lognormal} distribution. \cr
#'   \code{\link{eqnbinom}}                  \tab Estimate quantiles of a \link[=NegBinomial]{Negative Binomial} distribution. \cr
#'   \code{\link{eqnorm}}                    \tab Estimate quantiles of a \link{Normal} distribution, \cr
#'                                           \tab and optionally construct a confidence interval for a quantile. \cr
#'   \code{\link{eqpareto}}                  \tab Estimate quantiles of a \link{Pareto} distribution. \cr
#'   \code{\link{eqpois}}                    \tab Estimate quantiles of a \link{Poisson} distribution, \cr
#'                                           \tab and optionally construct a confidence interval for a quantile. \cr
#'   \code{\link{equnif}}                    \tab Estimate quantiles of a \link{Uniform} distribution. \cr
#'   \code{\link{eqweibull}}                 \tab Estimate quantiles of a \link{Weibull} distribution. \cr
#'   \code{\link{eqzmlnorm}}                 \tab Estimate quantiles of a \link[=DeltaDist]{Zero-Modified Lognormal (Delta)} \cr
#'                                           \tab distribution (log-scale). \cr
#'   \code{\link{eqzmlnormAlt}}              \tab Estimate quantiles of a \link[=DeltaDistAlt]{Zero-Modified Lognormal (Delta)} \cr
#'                                           \tab distribution (original scale). \cr
#'   \code{\link{eqzmnorm}}                  \tab Estimate quantiles of a \link[=ZeroModifiedNormal]{Zero-Modified Normal} distribution. \cr
#'   }
#' }
#' @rawRd
#' \keyword{ package }
NULL

#' EnvStats Functions for Goodness-of-Fit Tests
#' @name FcnsByCatGOFTests
#' @aliases GOF
#' @rawRd \alias{Goodness-of-Fit Tests}
#' @rawRd \alias{EnvStats Functions for Goodness-of-Fit Tests}
#' @description
#' The \pkg{EnvStats} functions listed below are useful for performing goodness-of-fit tests for
#'   user-specified probability distributions.
#' @rawRd
#' \details{
#'   \bold{Goodness-of-Fit Tests}
#'
#'   \tabular{ll}{
#'   \emph{Function Name}                    \tab \emph{Description} \cr
#'   \code{\link{gofTest}}                   \tab Perform a goodness-of-fit test for a specified probability distribution. \cr
#'                                           \tab The resulting object is of class \code{\link[=gof.object]{"gof"}} unless the test is the \cr
#'                                           \tab two-sample Kolmogorov-Smirnov test, in which case the resulting \cr
#'                                           \tab object is of class \code{\link[=gofTwoSample.object]{"gofTwoSample"}}. \cr
#'   \code{\link{plot.gof}}                  \tab S3 class method for plotting an object of class \code{\link[=gof.object]{"gof"}}. \cr
#'   \code{\link{print.gof}}                 \tab S3 class method for printing an object of class \code{\link[=gof.object]{"gof"}}. \cr
#'   \code{\link{plot.gofTwoSample}}         \tab S3 class method for plotting an object of class \code{\link[=gofTwoSample.object]{"gofTwoSample"}}. \cr
#'   \code{\link{print.gofTwoSample}}        \tab S3 class method for printing an object of class \code{\link[=gofTwoSample.object]{"gofTwoSample"}}. \cr
#'   \code{\link{gofGroupTest}}              \tab Perform a goodness-of-fit test to determine whether data in a set of groups \cr
#'                                           \tab appear to all come from the same probability distribution \cr
#'                                           \tab (with possibly different parameters for each group). \cr
#'                                           \tab The resulting object is of class \code{\link[=gofGroup.object]{"gofGroup"}}. \cr
#'   \code{\link{plot.gofGroup}}             \tab S3 class method for plotting an object of class \code{\link[=gofGroup.object]{"gofGroup"}}. \cr
#'   \code{\link{print.gofGroup}}            \tab S3 class method for printing an object of class \code{\link[=gofGroup.object]{"gofGroup"}}. \cr
#'   }
#'
#'
#'   \bold{Tests for Outliers}
#'
#'   \tabular{ll}{
#'   \emph{Function Name}                    \tab \emph{Description} \cr
#'   \code{\link{rosnerTest}}                \tab Perform Rosner's test for outliers assuming a normal (Gaussian) distribution. \cr
#'   \code{\link{print.gofOutlier}}          \tab S3 class method for printing an object of class \code{\link[=gofOutlier.object]{"gofOutlier"}}. \cr
#'   }
#'
#'
#'   \bold{Choose a Distribution}
#'
#'   \tabular{ll}{
#'   \emph{Function Name}                    \tab \emph{Description} \cr
#'   \code{\link{distChoose}}                \tab Choose best fitting distribution based on goodness-of-fit tests. \cr
#'   \code{\link{print.distChoose}}          \tab S3 class method for printing an object of class \code{\link[=distChoose.object]{"distChoose"}}. \cr
#'   }
#'
#'
#' }
#' @rawRd
#' \keyword{ package }
NULL

#' EnvStats Functions for Hypothesis Tests
#' @name FcnsByCatHypothTests
#' @rawRd \alias{Hypothesis Tests}
#' @description
#' The \pkg{EnvStats} functions listed below are useful for performing hypothesis tests not
#'   already built into \R.  See \link{Power and Sample Size Calculations} for a list of
#'   functions you can use to perform power and sample size calculations based on various
#'   hypothesis tests.
#' @rawRd
#' \details{
#'   For goodness-of-fit tests, see \link[=FcnsByCatGOFTests]{Goodness-of-Fit Tests}.
#'
#'   \tabular{ll}{
#'   \emph{Function Name}                             \tab \emph{Description} \cr
#'   \code{\link{chenTTest}}                          \tab Chen's modified one-sided t-test for skewed \cr
#'                                                    \tab distributions. \cr
#'   \code{\link{kendallTrendTest}}                   \tab Nonparametric test for monotonic trend \cr
#'                                                    \tab based on Kendall's tau statistic (and \cr
#'                                                    \tab optional confidence interval for slope). \cr
#'   \code{\link{kendallSeasonalTrendTest}}           \tab Nonparametric test for monotonic trend \cr
#'                                                    \tab within each season based on Kendall's tau \cr
#'                                                    \tab statistic (and optional confidence interval \cr
#'                                                    \tab for slope). \cr
#'   \code{\link{oneSamplePermutationTest}}           \tab Fisher's one-sample randomization \cr
#'                                                    \tab (permutation) test for location. \cr
#'   \code{\link{quantileTest}}                       \tab Two-sample rank test to detect a shift in \cr
#'                                                    \tab a proportion of the \dQuote{treated} population. \cr
#'   \code{\link{quantileTestPValue}}                 \tab Compute p-value associated with a specified \cr
#'                                                    \tab combination of \eqn{m}, \eqn{n}, \eqn{r} and \eqn{k} \cr
#'                                                    \tab for the quantile test. \cr
#'                                                    \tab Useful for determining \eqn{r} and \eqn{k} for a \cr
#'                                                    \tab given significance level \eqn{\alpha}. \cr
#'   \code{\link{serialCorrelationTest}}              \tab Test for the presence of serial correlation. \cr
#'   \code{\link{signTest}}                           \tab One- or paired-sample sign test on the \cr
#'                                                    \tab median. \cr
#'   \code{\link{twoSampleLinearRankTest}}            \tab Two-sample linear rank test to detect a \cr
#'                                                    \tab shift in the \dQuote{treated} population. \cr
#'   \code{\link{twoSamplePermutationTestLocation}}   \tab Two-sample or paired-sample randomization \cr
#'                                                    \tab (permutation) test for location. \cr
#'   \code{\link{twoSamplePermutationTestProportion}} \tab Randomization (permutation) test to compare \cr
#'                                                    \tab two proportions (Fisher's exact test). \cr
#'   \code{\link{varTest}}                            \tab One-sample test on variance or two-sample \cr
#'                                                    \tab test to compare variances. \cr
#'   \code{\link{varGroupTest}}                       \tab Test for homogeneity of variance among two \cr
#'                                                    \tab or more groups. \cr
#'   \code{\link{zTestGevdShape}}                     \tab Estimate the shape parameter of a \cr
#'                                                    \tab Generalized Extreme Value distribution and \cr
#'                                                    \tab test the null hypothesis that the true \cr
#'                                                    \tab value is equal to 0. \cr
#'   }
#' }
#' @rawRd
#' \keyword{ package }
NULL

#' EnvStats Functions for Monte Carlo Simulation and Risk Assessment
#' @name FcnsByCatMCandRisk
#' @rawRd \alias{Monte Carlo Simulation and Risk Assessment}
#' @description
#' The \pkg{EnvStats} functions listed below are useful for performing Monte Carlo simulations
#'   and risk assessment.
#' @rawRd
#' \details{
#'   \tabular{ll}{
#'   \emph{Function Name}                    \tab \emph{Description} \cr
#'   \link{Empirical}                        \tab Empirical distribution based on a set of observations. \cr
#'   \code{\link{simulateVector}}            \tab Simulate a vector of random numbers from a specified theoretical \cr
#'                                           \tab probability distribution or empirical probability distribution \cr
#'                                           \tab using either Latin hypercube sampling or simple random sampling. \cr
#'   \code{\link{simulateMvMatrix}}          \tab Simulate a multivariate matrix of random numbers from specified \cr
#'                                           \tab theoretical probability distributions and/or empirical probability \cr
#'                                           \tab distributions based on a specified rank correlation matrix, using \cr
#'                                           \tab either Latin hypercube sampling or simple random sampling. \cr
#'   }
#' }
#' @rawRd
#' \keyword{ package }
NULL

#' EnvStats Functions for Plotting Probability Distributions
#' @name FcnsByCatPlotProbDists
#' @rawRd \alias{Plotting Probability Distributions}
#' @rawRd \alias{Plot Probability Distributions}
#' @description
#' The \pkg{EnvStats} functions listed below are useful for plotting probability distributions.
#' @rawRd
#' \details{
#'   \tabular{ll}{
#'   \emph{Function Name}                    \tab \emph{Description} \cr
#'   \code{\link{cdfCompare}}                \tab Plot two cumulative distribution functions with the same \eqn{x}-axis \cr
#'                                           \tab in order to compare them. \cr
#'   \code{\link{cdfPlot}}                   \tab Plot a cumulative distribution function. \cr
#'   \code{\link{ecdfPlot}}                  \tab Plot empirical cumulative distribution function. \cr
#'   \code{\link{epdfPlot}}                  \tab Plot empirical probability density function. \cr
#'   \code{\link{pdfPlot}}                   \tab Plot probability density function. \cr
#'   \code{\link{qqPlot}}                    \tab Produce a quantile-quantile (Q-Q) plot, also called a probability plot. \cr
#'   \code{\link{qqPlotGestalt}}             \tab Plot several Q-Q plots from the same distribution in order to \cr
#'                                           \tab develop a Gestalt of Q-Q plots for that distribution. \cr
#'  }
#' }
#' @rawRd
#' \keyword{ package }
NULL

#' EnvStats Functions for Creating Plots Using the ggplot2 Package
#' @name FcnsByCatPlotUsingggplot2
#' @rawRd \alias{Plotting Using ggplot2}
#' @rawRd \alias{Plot Using ggplot2}
#' @description
#' The \pkg{EnvStats} functions listed below are useful for creating plots with the
#'   \pkg{\link[ggplot2]{ggplot2}} package.
#' @rawRd
#' \details{
#'   \tabular{ll}{
#'   \emph{Function Name}                    \tab \emph{Description} \cr
#'   \code{\link{geom_stripchart}}           \tab Adaptation of the \pkg{EnvStats} function \code{\link{stripChart}}, \cr
#'                                           \tab used to create a strip plot using functions from the package \cr
#'                                           \tab \pkg{\link[ggplot2]{ggplot2}}. \cr
#'   \code{\link{stat_n_text}}               \tab Add text indicating the sample size \cr
#'                                           \tab to a \pkg{\link[ggplot2]{ggplot2}} plot. \cr
#'   \code{\link{stat_mean_sd_text}}         \tab Add text indicating the mean and standard deviation \cr
#'                                           \tab to a \pkg{\link[ggplot2]{ggplot2}} plot. \cr
#'   \code{\link{stat_median_iqr_text}}      \tab Add text indicating the median and interquartile range \cr
#'                                           \tab to a \pkg{\link[ggplot2]{ggplot2}} plot. \cr
#'   \code{\link{stat_test_text}}            \tab Add text indicating the results of a hypothesis test \cr
#'                                           \tab comparing groups to a \pkg{\link[ggplot2]{ggplot2}} plot. \cr
#'  }
#' }
#' @rawRd
#' \keyword{ package }
NULL

#' EnvStats Functions for Power and Sample Size Calculations
#' @name FcnsByCatPower
#' @rawRd \alias{Power and Sample Size Calculations}
#' @rawRd \alias{Power and Sample Size}
#' @description
#' The \pkg{EnvStats} functions listed below are useful for power and sample size calculations.
#' @details
#' This help page has been shortened to keep function help focused on usage,
#' arguments, return values, and examples. Extended method details, formulas,
#' and background material are available in \code{vignette("extended-function-details", package = "EnvStats")}, section \code{FcnsByCatPower}.
#' @rawRd
#' \keyword{ package }
NULL

#' EnvStats Functions for Prediction Intervals
#' @name FcnsByCatPredInts
#' @rawRd \alias{Prediction Intervals}
#' @description
#' The \pkg{EnvStats} functions listed below are useful for computing prediction intervals and
#'   simultaneous prediction intervals.  See \link[=FcnsByCatPower]{Power and Sample Size} for a
#'   list of functions useful for computing power and sample size for a design based on a
#'   prediction interval width, or a design based on a hypothesis test for future observations
#'   falling outside of a prediciton interval.
#' @rawRd
#' \details{
#'   \tabular{ll}{
#'   \emph{Function Name}                    \tab \emph{Description} \cr
#'   \code{\link{predIntGamma}},             \tab Prediction interval for the next \eqn{k} \cr
#'   \code{\link{predIntGammaAlt}}           \tab observations or next set of \eqn{k} means for a \cr
#'                                           \tab Gamma distribution. \cr
#'   \code{\link{predIntGammaSimultaneous}}, \tab Construct a simultaneous prediction interval for the \cr
#'   \code{\link{predIntGammaAltSimultaneous}} \tab next \eqn{r} sampling occasions based on a \cr
#'                                           \tab Gamma distribution. \cr
#'                                           \tab \cr
#'   \code{\link{predIntLnorm}},             \tab Prediction interval for the next \eqn{k} \cr
#'   \code{\link{predIntLnormAlt}}           \tab observations or geometric means from a \cr
#'                                           \tab Lognormal distribution. \cr
#'   \code{\link{predIntLnormSimultaneous}}, \tab Construct a simultaneous prediction interval for the \cr
#'   \code{\link{predIntLnormAltSimultaneous}} \tab next \eqn{r} sampling occasions based on a \cr
#'                                           \tab Lognormal distribution. \cr
#'                                           \tab \cr
#'   \code{\link{predIntNorm}}               \tab Prediction interval for the next \eqn{k} observations \cr
#'                                           \tab or means from a Normal (Gaussian) distribution. \cr
#'   \code{\link{predIntNormK}}              \tab Compute the value of \eqn{K} for a prediction interval \cr
#'                                           \tab for a Normal distribution. \cr
#'   \code{\link{predIntNormSimultaneous}}   \tab Construct a simultaneous prediction interval for the \cr
#'                                           \tab next \eqn{r} sampling occasions based on a \cr
#'                                           \tab Normal distribution. \cr
#'   \code{\link{predIntNormSimultaneousK}}  \tab Compute the value of \eqn{K} for a simultaneous \cr
#'                                           \tab prediction interval for the next \eqn{r} sampling \cr
#'                                           \tab occasions based on a Normal distribution. \cr
#'                                           \tab \cr
#'   \code{\link{predIntNpar}}               \tab Nonparametric prediction interval for the next \eqn{k} \cr
#'                                           \tab of \eqn{K} observations. \cr
#'   \code{\link{predIntNparSimultaneous}}   \tab Construct a nonparametric simultaneous prediction \cr
#'                                           \tab interval for the next \eqn{r} sampling occasions. \cr
#'                                           \tab \cr
#'   \code{\link{predIntPois}}               \tab Prediction interval for the next \eqn{k} observations \cr
#'                                           \tab or sums from a Poisson distribution. \cr
#'   }
#' }
#' @rawRd
#' \keyword{ package }
NULL

#' EnvStats Functions for Printing and Plotting Objects of Various S3 Classes
#' @name FcnsByCatPrintPlot
#' @rawRd \alias{Printing and Plotting Methods}
#' @description
#' The \pkg{EnvStats} functions listed below are printing and plotting methods for various S3 classes.
#' @rawRd
#' \details{
#'   \bold{Printing Methods}
#'
#'   \tabular{ll}{
#'   \emph{Function Name}                    \tab \emph{Description} \cr
#'   \code{\link{print.boxcox}}              \tab Print an object that inherits from class \code{\link[=boxcox.object]{"boxcox"}}. \cr
#'   \code{\link{print.boxcoxCensored}}      \tab Print an object that inherits from class \cr
#'                                           \tab \code{\link[=boxcoxCensored.object]{"boxcoxCensored"}}. \cr
#'   \code{\link{print.boxcoxLm}}            \tab Print an object that inherits from class \code{\link[=boxcoxLm.object]{"boxcoxLm"}}. \cr
#'                                           \tab \cr
#'   \code{\link{print.estimate}}            \tab Print an object that inherits from class \code{\link[=estimate.object]{"estimate"}}. \cr
#'   \code{\link{print.estimateCensored}}    \tab Print an object that inherits from class \cr
#'                                           \tab \code{\link[=estimateCensored.object]{"estimateCensored"}}. \cr
#'                                           \tab \cr
#'   \code{\link{print.gof}}                 \tab Print an object that inherits from class \code{\link[=gof.object]{"gof"}}. \cr
#'   \code{\link{print.gofCensored}}         \tab Print an object that inherits from class \code{\link[=gofCensored.object]{"gofCensored"}}. \cr
#'   \code{\link{print.gofGroup}}            \tab Print an object that inherits from class \code{\link[=gofGroup.object]{"gofGroup"}}. \cr
#'   \code{\link{print.gofTwoSample}}        \tab Print an object that inherits from class \cr
#'                                           \tab \code{\link[=gofTwoSample.object]{"gofTwoSample"}}. \cr
#'                                           \tab \cr
#'   \code{\link{print.htestEnvStats}}       \tab Print an object that inherits from class \code{\link[=htest.htestEnvStats.object]{"htestEnvStats"}}. \cr
#'   \code{\link{print.htestCensored}}       \tab Print an object that inherits from class \cr
#'                                           \tab \code{\link[=htestCensored.object]{"htestCensored"}}. \cr
#'   \code{\link{print.permutationTest}}     \tab Print an object that inherits from class \cr
#'                                           \tab \code{\link[=permutationTest.object]{"permutationTest"}}. \cr
#'                                           \tab \cr
#'   \code{\link{print.summaryStats}}        \tab Print an object that inherits from class \cr
#'                                           \tab \code{\link[=summaryStats.object]{"summaryStats"}}. \cr
#'   }
#'
#'
#'   \bold{Plotting Methods}
#'
#'   \tabular{ll}{
#'   \emph{Function Name}                    \tab \emph{Description} \cr
#'   \code{\link{plot.boxcox}}               \tab Plot an object that inherits from class \code{\link[=boxcox.object]{"boxcox"}}. \cr
#'   \code{\link{plot.boxcoxCensored}}       \tab Plot an object that inherits from class \code{\link[=boxcoxCensored.object]{"boxcoxCensored"}}. \cr
#'   \code{\link{plot.boxcoxLm}}             \tab Plot an object that inherits from class \code{\link[=boxcoxLm.object]{"boxcoxLm"}}. \cr
#'                                           \tab \cr
#'   \code{\link{plot.gof}}                  \tab Plot an object that inherits from class \code{\link[=gof.object]{"gof"}}. \cr
#'   \code{\link{plot.gofCensored}}          \tab Plot an object that inherits from class \code{\link[=gofCensored.object]{"gofCensored"}}. \cr
#'   \code{\link{plot.gofGroup}}             \tab Plot an object that inherits from class \code{\link[=gofGroup.object]{"gofGroup"}}. \cr
#'   \code{\link{plot.gofTwoSample}}         \tab Plot an object that inherits from class \code{\link[=gofTwoSample.object]{"gofTwoSample"}}. \cr
#'                                           \tab \cr
#'   \code{\link{plot.permutationTest}}      \tab Plot an object that inherits from class \code{\link[=permutationTest.object]{"permutationTest"}}. \cr
#'   }
#' }
#' @rawRd
#' \keyword{ package }
NULL

#' EnvStats Probability Distributions and Random Numbers
#' @name FcnsByCatProbDists
#' @rawRd \alias{Probability Distributions}
#' @rawRd \alias{Probability Distributions and Random Numbers}
#' @description
#' Listed below are all of the probability distributions available in \R and \pkg{EnvStats}.
#'   Distributions with a description in \bold{bold} are new ones that are part of \pkg{EnvStats}.
#'   For each distribution, there are functions for generating:  values for the probability
#'   density function, values for the cumulative distribution function, quantiles, and
#'   random numbers.
#'
#'   The data frame \code{\link{Distribution.df}} contains information about
#'   all of these probability distributions.
#' @rawRd
#' \details{
#'   \tabular{ll}{
#'   \emph{Distribution Abbreviation}        \tab \emph{Description} \cr
#'   \code{\link[stats:Beta]{beta}}          \tab Beta distribution. \cr
#'   \code{\link[stats:Binomial]{binom}}     \tab Binomial distribution. \cr
#'   \code{\link[stats:Cauchy]{cauchy}}      \tab Cauchy distribution. \cr
#'   \code{\link[=Chi]{chi}}                 \tab \bold{Chi distribution}. \cr
#'   \code{\link[stats:Chisquare]{chisq}}    \tab Chi-squared distribution. \cr
#'   \code{\link[stats:Exponential]{exp}}    \tab Exponential distribution. \cr
#'   \code{\link[=EVD]{evd}}                 \tab \bold{Extreme value distribution}. \cr
#'   \code{\link[stats:FDist]{f}}            \tab F-distribution. \cr
#'   \code{\link[stats:GammaDist]{gamma}}    \tab Gamma distribution.\cr
#'   \code{\link[=GammaAlt]{gammAlt}}        \tab \bold{Gamma distribution parameterized with mean and CV}. \cr
#'   \code{\link[=GEVD]{gevd}}               \tab \bold{Generalized extreme value distribution}. \cr
#'   \code{\link[stats:Geometric]{geom}}     \tab Geometric distribution. \cr
#'   \code{\link[stats:Hypergeometric]{hyper}} \tab Hypergeometric distribution. \cr
#'   \code{\link[stats:Logistic]{logis}}     \tab Logistic distribution. \cr
#'   \code{\link[stats:Lognormal]{lnorm}}    \tab Lognormal distribution. \cr
#'   \code{\link[=LognormalAlt]{lnormAlt}}   \tab \bold{Lognormal distribution parameterized with mean and CV}. \cr
#'   \code{\link[=LognormalMix]{lnormMix}}   \tab \bold{Mixture of two lognormal distributions}. \cr
#'   \code{\link[=LognormalMixAlt]{lnormMixAlt}} \tab \bold{Mixture of two lognormal distributions} \cr
#'                                           \tab \bold{parameterized by their means and CVs}. \cr
#'   \code{\link[=Lognormal3]{lnorm3}}       \tab \bold{Three-parameter lognormal distribution}. \cr
#'   \code{\link[=LognormalTrunc]{lnormTrunc}} \tab \bold{Truncated lognormal distribution}. \cr
#'   \code{\link[=LognormalTruncAlt]{lnormTruncAlt}} \tab \bold{Truncated lognormal distribution} \cr
#'                                           \tab \bold{parameterized by mean and CV}. \cr
#'   \code{\link[stats:NegBinomial]{nbinom}} \tab Negative binomial distribution. \cr
#'   \code{\link[stats:Normal]{norm}}        \tab Normal distribution. \cr
#'   \code{\link[=NormalMix]{normMix}}       \tab \bold{Mixture of two normal distributions}. \cr
#'   \code{\link[=NormalTrunc]{normTrunc}}   \tab \bold{Truncated normal distribution}. \cr
#'   \code{\link[=Pareto]{pareto}}           \tab \bold{Pareto distribution}. \cr
#'   \code{\link[stats:Poisson]{pois}}       \tab Poisson distribution. \cr
#'   \code{\link[stats:TDist]{t}}            \tab Student's t-distribution. \cr
#'   \code{\link[=Triangular]{tri}}          \tab \bold{Triangular distribution}. \cr
#'   \code{\link[stats:Uniform]{unif}}       \tab Uniform distribution. \cr
#'   \code{\link[stats:Weibull]{weibull}}    \tab Weibull distribution. \cr
#'   \code{\link[stats:Wilcoxon]{wilcox}}    \tab Wilcoxon rank sum distribution. \cr
#'   \code{\link[=ZeroModifiedLognormal]{zmlnorm}} \tab \bold{Zero-modified lognormal (delta) distribution}. \cr
#'   \code{\link[=ZeroModifiedLognormalAlt]{zmlnormAlt}} \tab \bold{Zero-modified lognormal (delta) distribution} \cr
#'                                           \tab \bold{parameterized with mean and CV}. \cr
#'   \code{\link[=ZeroModifiedNormal]{zmnorm}} \tab \bold{Zero-modified normal distribution}. \cr
#'   }
#'
#'   In addition, the functions \code{\link{evNormOrdStats}} and
#'   \code{\link{evNormOrdStatsScalar}} compute expected values of order statistics
#'   from a standard normal distribution.
#' }
#' @rawRd
#' \keyword{ package }
NULL

#' EnvStats Functions for Summary Statistics and Plots
#' @name FcnsByCatSumStats
#' @rawRd \alias{Summary Statistics}
#' @rawRd \alias{Summary Plots}
#' @description
#' The \pkg{EnvStats} functions listed below create summary statistics and plots.
#' @rawRd
#' \details{
#'   \bold{Summary Statistics} \cr
#'   \R comes with several functions for computing summary statistics, including
#'   \code{\link{mean}}, \code{\link{var}}, \code{\link{median}}, \code{\link{range}},
#'   \code{\link{quantile}}, and \code{\link{summary}}.  The following functions in
#'   \pkg{EnvStats} complement these \R functions.
#'
#'   \tabular{ll}{
#'   \emph{Function Name}        \tab \emph{Description} \cr
#'   \code{\link{cv}}            \tab Coefficient of variation \cr
#'   \code{\link{geoMean}}       \tab Geometric mean \cr
#'   \code{\link{geoSD}}         \tab Geometric standard deviation \cr
#'   \code{\link{iqr}}           \tab Interquartile range \cr
#'   \code{\link{kurtosis}}      \tab Kurtosis \cr
#'   \code{\link{lMoment}}       \tab \eqn{L}-moments \cr
#'   \code{\link{pwMoment}}      \tab Probability-weighted moments \cr
#'   \code{\link{skewness}}      \tab Skew \cr
#'   \code{\link{summaryFull}}   \tab Extensive summary statistics \cr
#'   \code{\link{summaryStats}}  \tab Summary statistics \cr
#'   }
#'
#'
#'   \bold{Summary Plots} \cr
#'   \R comes with several functions for creating plots to summarize data, including
#'   \code{\link[graphics]{hist}}, \code{\link[graphics]{barplot}}, \code{\link[graphics]{boxplot}},
#'   \code{\link[graphics]{dotchart}}, \code{\link[graphics]{stripchart}}, and numerous others.
#'
#'   The help file \link[=FcnsByCatPlotProbDists]{Plotting Probability Distributions}
#'   lists several \pkg{EnvStats} functions useful for producing summary plots as well.
#'
#'   In addition, the \pkg{EnvStats} function \code{\link{stripChart}} is a modification
#'   of \code{\link[graphics]{stripchart}} that allows you to include summary statistics
#'   on the plot itself.
#'
#'   Finally, the help file \link[=FcnsByCatPlotUsingggplot2]{Plotting Using ggplot2} lists
#'   several \pkg{EnvStats} functions for adding information to plots produced with the
#'   \code{\link[ggplot2]{ggplot}} function, including the function \code{\link{geom_stripchart}},
#'   which is an adaptation of the \pkg{EnvStats} function \code{\link{stripChart}}.
#' }
#' @rawRd
#' \keyword{ package }
NULL

#' EnvStats Functions for Tolerance Intervals
#' @name FcnsByCatTolInts
#' @rawRd \alias{Tolerance Intervals}
#' @description
#' The \pkg{EnvStats} functions listed below are useful for computing tolerance intervals.
#'   See \link[=FcnsByCatPower]{Power and Sample Size} for a list of functions useful for
#'   computing power and sample size for a design based on a tolerance interval width.
#' @rawRd
#' \details{
#'
#'   \tabular{ll}{
#'   \emph{Function Name}                    \tab \emph{Description} \cr
#'   \code{\link{tolIntGamma}},              \tab Tolerance interval for a Gamma distribution. \cr
#'   \code{\link{tolIntGammaAlt}}            \tab \cr
#'                                           \tab \cr
#'   \code{\link{tolIntLnorm}},              \tab Tolerance interval for a lognormal distribution. \cr
#'   \code{\link{tolIntLnormAlt}}            \tab \cr
#'                                           \tab \cr
#'   \code{\link{tolIntNorm}}                \tab Tolerance interval for a Normal (Gaussian) distribution. \cr
#'   \code{\link{tolIntNormK}}               \tab Compute the constant \eqn{K} for a Normal (Gaussian) \cr
#'                                           \tab tolerance interval. \cr
#'                                           \tab \cr
#'   \code{\link{tolIntNpar}}                \tab Nonparametric tolerance interval. \cr
#'                                           \tab \cr
#'   \code{\link{tolIntPois}}                \tab Tolerance interval for a Poisson distribution. \cr
#'   }
#' }
#' @rawRd
#' \keyword{ package }
NULL

#' EnvStats Functions for Trend Analysis
#' @name FcnsByCatTrend
#' @rawRd \alias{Trend Analysis}
#' @description
#' See \link{Hypothesis Tests}.
#' @rawRd
#' \keyword{ package }
NULL

#' The Generalized Extreme Value Distribution
#' @name GEVD
#' @aliases dgevd pgevd qgevd rgevd
#' @rawRd \alias{Generalized Extreme Value Distribution}
#' @description
#' Density, distribution function, quantile function, and random generation
#'   for the generalized extreme value distribution.
#' @usage
#' dgevd(x, location = 0, scale = 1, shape = 0)
#'   pgevd(q, location = 0, scale = 1, shape = 0)
#'   qgevd(p, location = 0, scale = 1, shape = 0)
#'   rgevd(n, location = 0, scale = 1, shape = 0)
#' @rawRd
#' \arguments{
#'   \item{x}{
#'   vector of quantiles.
#' }
#'   \item{q}{
#'   vector of quantiles.
#' }
#'   \item{p}{
#'   vector of probabilities between 0 and 1.
#' }
#'   \item{n}{
#'   sample size.  If \code{length(n)} is larger than 1, then \code{length(n)}
#'   random values are returned.
#' }
#'   \item{location}{
#'   vector of location parameters.
#' }
#'   \item{scale}{
#'   vector of positive scale parameters.
#' }
#'   \item{shape}{
#'   vector of shape parameters.
#' }
#' }
#' @rawRd
#' \details{
#'   Let \eqn{X} be a generalized extreme value random variable with parameters
#'   \code{location=}\eqn{\eta}, \code{scale=}\eqn{\theta}, and \code{shape=}\eqn{\kappa}.
#'   When the shape parameter \eqn{\kappa = 0}, the generalized extreme value distribution
#'   reduces to the \link[=EVD]{extreme value distribution}.  When the shape parameter
#'   \eqn{\kappa \ne 0}, the cumulative distribution function of \eqn{X} is given by:
#'   \deqn{F(x; \eta, \theta, \kappa) = exp\{-[1 - \kappa(x-\eta)/\theta]^{1/\kappa}\}}
#'   where \eqn{-\infty < \eta, \kappa < \infty} and \eqn{\theta > 0}.
#'   When \eqn{\kappa > 0}, the range of \eqn{x} is:
#'   \deqn{-\infty < x \le \eta + \theta/\kappa}
#'   and when \eqn{\kappa < 0} the range of \eqn{x} is:
#'   \deqn{\eta + \theta/\kappa \le x < \infty}
#'
#'   The \eqn{p^th} quantile of \eqn{X} is given by:
#'   \deqn{x_{p} = \eta + \frac{\theta \{1 - [-log(p)]^{\kappa}\}}{\kappa}}
#' }
#' @rawRd
#' \value{
#'   density (\code{devd}), probability (\code{pevd}), quantile (\code{qevd}), or
#'   random sample (\code{revd}) for the generalized extreme value distribution with
#'   location parameter(s) determined by \code{location}, scale parameter(s)
#'   determined by \code{scale}, and shape parameter(s) determined by \code{shape}.
#' }
#' @rawRd
#' \references{
#'   Forbes, C., M. Evans, N. Hastings, and B. Peacock. (2011).  Statistical Distributions.
#'   Fourth Edition. John Wiley and Sons, Hoboken, NJ.
#'
#'   Jenkinson, A.F. (1955).  The Frequency Distribution of the Annual Maximum
#'   (or Minimum) of Meteorological Events.  \emph{Quarterly Journal of the Royal
#'   Meteorological Society}, \bold{81}, 158--171.
#'
#'   Johnson, N. L., S. Kotz, and N. Balakrishnan. (1995).
#'   \emph{Continuous Univariate Distributions, Volume 2}.
#'   Second Edition. John Wiley and Sons, New York.
#' }
#' @rawRd
#' \author{
#'   Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \note{
#'   Two-parameter \link[=EVD]{extreme value distributions (EVD)} have been applied
#'   extensively since the 1930's to several fields of study, including the distributions
#'   of hydrological and meteorological variables, human lifetimes, and strength of
#'   materials.  The three-parameter generalized extreme value distribution (GEVD) was
#'   introduced by Jenkinson (1955) to model annual maximum and minimum values of
#'   meteorological events.  Since then, it has been used extensively in the hydological
#'   and meteorological fields.
#'
#'   The three families of EVDs are all special kinds of GEVDs.  When the shape
#'   parameter \eqn{\kappa = 0}, the GEVD reduces to the
#'   \link[=EVD]{Type I extreme value (Gumbel) distribution}.  (The function
#'   \code{\link{zTestGevdShape}} allows you to test the null hypothesis that the shape
#'   parameter is equal to 0.)  When \eqn{\kappa > 0}, the GEVD is the same as the Type II
#'   extreme value distribution, and when \eqn{\kappa < 0} it is the same as the
#'   Type III extreme value distribution.
#' }
#' @rawRd
#' \seealso{
#'   \code{\link{egevd}}, \code{\link{zTestGevdShape}}, \code{\link{EVD}},
#'   \link{Probability Distributions and Random Numbers}.
#' }
#' @rawRd
#' \examples{
#'   # Density of a generalized extreme value distribution with
#'   # location=0, scale=1, and shape=0, evaluated at 0.5:
#'
#'   dgevd(.5)
#'   #[1] 0.3307043
#'
#'   #----------
#'
#'   # The cdf of a generalized extreme value distribution with
#'   # location=1, scale=2, and shape=0.25, evaluated at 0.5:
#'
#'   pgevd(.5, 1, 2, 0.25)
#'   #[1] 0.2795905
#'
#'   #----------
#'
#'   # The 90'th percentile of a generalized extreme value distribution with
#'   # location=-2, scale=0.5, and shape=-0.25:
#'
#'   qgevd(.9, -2, 0.5, -0.25)
#'   #[1] -0.4895683
#'
#'   #----------
#'
#'   # Random sample of 4 observations from a generalized extreme value
#'   # distribution with location=5, scale=2, and shape=1.
#'   # (Note: the call to set.seed simply allows you to reproduce this example.)
#'
#'   set.seed(20)
#'   rgevd(4, 5, 2, 1)
#'   #[1] 6.738692 6.473457 4.446649 5.727085
#' }
#' @rawRd
#' \keyword{ distribution }
#' @rawRd
#' \keyword{ datagen }
NULL

#' The Gamma Distribution (Alternative Parameterization)
#' @name GammaAlt
#' @aliases dgammaAlt pgammaAlt qgammaAlt rgammaAlt
#' @rawRd \alias{Gamma Distribution}
#' @description
#' Density, distribution function, quantile function, and random generation
#'   for the gamma distribution with parameters \code{mean} and \code{cv}.
#' @usage
#' dgammaAlt(x, mean, cv = 1, log = FALSE)
#'   pgammaAlt(q, mean, cv = 1, lower.tail = TRUE, log.p = FALSE)
#'   qgammaAlt(p, mean, cv = 1, lower.tail = TRUE, log.p = FALSE)
#'   rgammaAlt(n, mean, cv = 1)
#' @rawRd
#' \arguments{
#'   \item{x}{
#'   vector of quantiles.
#' }
#'   \item{q}{
#'   vector of quantiles.
#' }
#'   \item{p}{
#'   vector of probabilities between 0 and 1.
#' }
#'   \item{n}{
#'   sample size.  If \code{length(n)} is larger than 1, then \code{length(n)}
#'   random values are returned.
#' }
#'   \item{mean}{
#'   vector of (positive) means of the distribution of the random variable.
#' }
#'   \item{cv}{
#'   vector of (positive) coefficients of variation of the random variable.
#' }
#'   \item{log, log.p}{
#'   logical; if \code{TRUE}, probabilities/densities \eqn{p} are returned as \eqn{log(p)}.
#' }
#'   \item{lower.tail}{
#'   logical; if \code{TRUE} (default), probabilities are \eqn{P[X \le x]},
#'   otherwise, \eqn{P[X > x]}.
#' }
#' }
#' @rawRd
#' \details{
#'   Let \eqn{X} be a random variable with a gamma distribution with parameters
#'   \code{shape=}\eqn{\alpha} and \code{scale=}\eqn{\beta}.  The relationship
#'   between these parameters and the mean (\code{mean=}\eqn{\mu}) and coefficient
#'   of variation (\code{cv=}\eqn{\tau}) of this distribution is given by:
#'   \deqn{\alpha = \tau^{-2} \;\;\;\;\;\; (1)}
#'   \deqn{\beta = \mu/\alpha \;\;\;\;\;\; (2)}
#'   \deqn{\mu = \alpha\beta \;\;\;\;\;\; (3)}
#'   \deqn{\tau = \alpha^{-1/2} \;\;\;\;\;\; (4)}
#'   Thus, the functions \code{dgammaAlt}, \code{pgammaAlt}, \code{qgammaAlt}, and
#'   \code{rgammaAlt} call the \R functions \code{\link{dgamma}},
#'   \code{\link{pgamma}}, \code{\link{qgamma}}, and \code{\link{rgamma}},
#'   respectively, using the values for the \code{shape} and \code{scale} parameters
#'   given by:  \code{shape <- cv^-2}, \code{scale <- mean/shape}.
#' }
#' @rawRd
#' \value{
#'   \code{dgammaAlt} gives the density, \code{pgammaAlt} gives the distribution function,
#'   \code{qgammaAlt} gives the quantile function, and \code{rgammaAlt} generates random
#'   deviates.
#'
#'   Invalid arguments will result in return value \code{NaN}, with a warning.
#' }
#' @rawRd
#' \references{
#'   Forbes, C., M. Evans, N. Hastings, and B. Peacock. (2011).
#'   \emph{Statistical Distributions, Fourth Edition}.
#'   John Wiley and Sons, Hoboken, NJ.
#'
#'   Johnson, N. L., S. Kotz, and N. Balakrishnan. (1994).
#'   \emph{Continuous Univariate Distributions, Volume 1}.
#'   Second Edition. John Wiley and Sons, New York.
#'
#'   Singh, A., A.K. Singh, and R.J. Iaci. (2002).  Estimation of the Exposure
#'   Point Concentration Term Using a Gamma Distribution.  EPA/600/R-02/084.
#'   October 2002.  Technology Support Center for Monitoring and Site Characterization,
#'   Office of Research and Development, Office of Solid Waste and Emergency Response,
#'   U.S. Environmental Protection Agency, Washington, D.C.
#'
#'   Singh, A., R. Maichle, and N. Armbya. (2010a).  \emph{ProUCL Version 4.1.00
#'   User Guide (Draft)}.  EPA/600/R-07/041, May 2010.  Office of Research and
#'   Development, U.S. Environmental Protection Agency, Washington, D.C.
#'
#'   Singh, A., N. Armbya, and A. Singh. (2010b).  \emph{ProUCL Version 4.1.00
#'   Technical Guide (Draft)}.  EPA/600/R-07/041, May 2010.  Office of Research and
#'   Development, U.S. Environmental Protection Agency, Washington, D.C.
#' }
#' @rawRd
#' \author{
#'   Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \note{
#'   The gamma distribution takes values on the positive real line.  Special cases of
#'   the gamma are the \link[stats:Exponential]{exponential distribution} and the
#'   \link[stats:Chisquare]{chi-square distribution}.  Applications of the gamma include
#'   life testing, statistical ecology, queuing theory, inventory control and
#'   precipitation processes.  A gamma distribution starts to resemble a normal
#'   distribution as the shape parameter \eqn{\alpha} tends to infinity or
#'   the cv parameter \eqn{\tau} tends to 0.
#'
#'   Some EPA guidance documents (e.g., Singh et al., 2002; Singh et al., 2010a,b)
#'   discourage using the assumption of a \link[stats:Lognormal]{lognormal distribution}
#'   for some types of environmental data and recommend instead assessing whether
#'   the data appear to fit a gamma distribution.
#' }
#' @rawRd
#' \seealso{
#'   \link{GammaDist}, \code{\link{egammaAlt}},
#'   \link{Probability Distributions and Random Numbers}.
#' }
#' @rawRd
#' \examples{
#'   # Density of a gamma distribution with parameters mean=10 and cv=2,
#'   # evaluated at 7:
#'
#'   dgammaAlt(7, mean = 10, cv = 2)
#'   #[1] 0.02139335
#'
#'   #----------
#'
#'   # The cdf of a gamma distribution with parameters mean=10 and cv=2,
#'   # evaluated at 12:
#'
#'   pgammaAlt(12, mean = 10, cv = 2)
#'   #[1] 0.7713307
#'
#'   #----------
#'
#'   # The 25'th percentile of a gamma distribution with parameters
#'   # mean=10 and cv=2:
#'
#'   qgammaAlt(0.25, mean = 10, cv = 2)
#'   #[1] 0.1056871
#'
#'   #----------
#'
#'   # A random sample of 4 numbers from a gamma distribution with
#'   # parameters mean=10 and cv=2.
#'   # (Note: the call to set.seed simply allows you to reproduce this example.)
#'
#'   set.seed(10)
#'   rgammaAlt(4, mean = 10, cv = 2)
#'   #[1] 3.772004230 1.889028078 0.002987823 8.179824976
#' }
#' @rawRd
#' \keyword{ distribution }
#' @rawRd
#' \keyword{ datagen }
NULL

#' Alkilinity Data from Gibbons et al. (2009)
#' @name Gibbons.et.al.09.Alkilinity.vec
#' @description
#' Alkilinity concentrations (mg/L) in groundwater.
#' @usage
#' data(Gibbons.et.al.09.Alkilinity.vec)
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A numeric vector with 27 elements.
#' }
#' @rawRd
#' \source{
#'   Gibbons, R.D., D.K. Bhaumik, and S. Aryal. (2009).
#'   \emph{Statistical Methods for Groundwater Monitoring}. Second Edition.
#'   John Wiley & Sons, Hoboken.  Table 5.5, p. 107.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Vinyl Chloride Data from Gibbons et al. (2009)
#' @name Gibbons.et.al.09.Vinyl.Chloride.vec
#' @description
#' Vinyl chloride concentrations (\eqn{mu}g/L) in groundwater from upgradient
#'   background monitoring wells.
#' @usage
#' data(Gibbons.et.al.09.Vinyl.Chloride.vec)
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A numeric vector with 34 elements.
#' }
#' @rawRd
#' \source{
#'   Gibbons, R.D., D.K. Bhaumik, and S. Aryal. (2009).
#'   \emph{Statistical Methods for Groundwater Monitoring}. Second Edition.
#'   John Wiley & Sons, Hoboken.  Table 4.3, p. 87.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Ethylene Thiourea Dose-Response Data
#' @name Graham.et.al.75.etu.df
#' @description
#' These data are the results of an experiment in which different groups of rats were
#'   exposed to different concentration levels of ethylene thiourea (ETU), which is a
#'   decomposition product of a certain class of fungicides that can be found in treated
#'   foods (Graham et al., 1975; Rodricks, 1992, p.133).  In this experiment, the outcome of
#'   concern was the number of rats that developed thyroid tumors.
#' @usage
#' Graham.et.al.75.etu.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 6 observations on the following 4 variables.
#'   \describe{
#'     \item{\code{dose}}{a numeric vector of dose (ppm/day) of ETU.}
#'     \item{\code{tumors}}{a numeric vector indicating number of rats that developed thyroid tumors.}
#'     \item{\code{n}}{a numeric vector indicating the number of rats in the dose group.}
#'     \item{\code{proportion}}{a numeric vector indicating proportion of rats that developed thyroid tumors.}
#'   }
#' }
#' @rawRd
#' \source{
#'   Graham, S.L., K.J. Davis, W.H. Hansen, and C.H. Graham. (1975).
#'   Effects of Prolonged Ethylene Thiourea Ingestion on the Thyroid of the Rat.
#'   \emph{Food and Cosmetics Toxicology}, \bold{13}(5), 493--499.
#' }
#' @rawRd
#' \references{
#'   Rodricks, J.V. (1992). \emph{Calculated Risks: The Toxicity and Human Health Risks of Chemicals in Our Environment}.
#'   Cambridge University Press, New York, p.133.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Adjusted Alpha Levels to Compute Confidence Intervals for the Mean of a Gamma Distribution
#' @name Grice.Bain.80.mat
#' @description
#' Adjusted alpha levels to compute confidence intervals for the mean of a gamma distribution, as
#'   presented in Table 2 of Grice and Bain (1980).
#' @usage
#' data("Grice.Bain.80.mat")
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A matrix of dimensions 5 by 7, with
#'   the first dimension indicating the sample size (between 5 and Inf),
#'   and the second dimension indicating the assumed significance level
#'   associated with the confidence interval (between 0.005 and 0.25).
#'   The assumed confidence level is 1 - assumed significance level.
#' }
#' @rawRd
#' \details{
#'   See Grice and Bain (1980) and the help file for \code{\link{egamma}}
#'   for more information.  The data in this matrix are used when
#'   the function \code{\link{egamma}} is called with \code{ci.method="chisq.adj"}.
#' }
#' @rawRd
#' \source{
#'   Grice, J.V., and L.J. Bain. (1980).  Inferences Concerning the Mean of the Gamma Distribution.
#'   \emph{Journal of the American Statistical Association} \bold{75}, 929-933.
#' }
#' @rawRd
#' \references{
#'   Grice, J.V., and L.J. Bain. (1980).  Inferences Concerning the Mean of the Gamma Distribution.
#'   \emph{Journal of the American Statistical Association} \bold{75}, 929-933.
#'
#'   USEPA. (2002).  \emph{Estimation of the Exposure Point Concentration Term Using a
#'   Gamma Distribution}.
#'   EPA/600/R-02/084. October 2002. Technology Support Center for Monitoring and
#'   Site Characterization, Office of Research and Development, Office of Solid Waste and
#'   Emergency Response, U.S. Environmental Protection Agency, Washington, D.C.
#'
#'   USEPA. (2015).  \emph{ProUCL Version 5.1.002 Technical Guide}.  EPA/600/R-07/041, October 2015.
#'   Office of Research and Development. U.S. Environmental Protection Agency, Washington, D.C.
#' }
#' @rawRd
#' \examples{
#'   # Look at Grice.Bain.80.mat
#'
#'   Grice.Bain.80.mat
#'   #         alpha.eq.005 alpha.eq.01 alpha.eq.025 alpha.eq.05 alpha.eq.075
#'   #n.eq.5         0.0000      0.0000       0.0010      0.0086       0.0234
#'   #n.eq.10        0.0003      0.0015       0.0086      0.0267       0.0486
#'   #n.eq.20        0.0017      0.0046       0.0159      0.0380       0.0619
#'   #n.eq.40        0.0030      0.0070       0.0203      0.0440       0.0685
#'   #n.eq.Inf       0.0050      0.0100       0.0250      0.0500       0.0750
#'
#'   #         alpha.eq.10 alpha.eq.25
#'   #n.eq.5        0.0432      0.2038
#'   #n.eq.10       0.0724      0.2294
#'   #n.eq.20       0.0866      0.2403
#'   #n.eq.40       0.0934      0.2453
#'   #n.eq.Inf      0.1000      0.2500
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Example of Multiply Left-censored Data from Literature
#' @name Helsel.Cohn.88.app.b.df
#' @description
#' Made up multiply left-censored data.  There are 9 observations out of a total of 18
#'   that are reported as <\eqn{DL}, where \eqn{DL} denotes a detection limit.  There are
#'   2 distinct detection limits.
#' @usage
#' Helsel.Cohn.88.app.b.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 18 observations on the following 3 variables.
#'   \describe{
#'     \item{\code{Conc.orig}}{a character vector of original observations}
#'     \item{\code{Conc}}{a numeric vector of observations with censored values coded to censoring levels}
#'     \item{\code{Censored}}{a logical vector indicating which values are censored}
#'   }
#' }
#' @rawRd
#' \source{
#'   Helsel, D.R., and T.A. Cohn. (1988). Estimation of Descriptive Statistics for Multiply Censored Water Quality Data.
#'   \emph{Water Resources Research} \bold{24}(12), 1997--2004, Appendix B.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Silver Concentrations From An Interlab Comparison
#' @name Helsel.Cohn.88.silver.df
#' @description
#' Silver concentrations (mg/L) from an interlab comparison.  There are 34 observations
#'   out of a total of 56 that are reported as <DL, where DL denotes a detection limit.  There
#'   are 12 distinct detection limits.
#' @usage
#' Helsel.Cohn.88.silver.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 56 observations on the following 4 variables.
#'   \describe{
#'     \item{\code{Ag.orig}}{a character vector of original silver concentrations (mg/L)}
#'     \item{\code{Ag}}{a numeric vector with nondetects coded to the detection limit}
#'     \item{\code{Censored}}{a logical vector indicating which observations are censored}
#'     \item{\code{log.Ag}}{the natural logarithm of \code{Ag}}
#'   }
#' }
#' @rawRd
#' \source{
#'   Helsel, D.R., and T.A. Cohn. (1988). Estimation of Descriptive Statistics for Multiply Censored Water Quality Data.
#'   \emph{Water Resources Research} \bold{24}(12), 1997--2004.
#' }
#' @rawRd
#' \references{
#'   Janzer, V.J. (1986). \emph{Report of the U.S. Geological Survey's Analytical Evaluation Program--Standard Reference Water Samples M6, M94, T95, N16, P8, and SED3}.
#'   Technical Report, Branch of Quality Assurance, U.S. Geological Survey, Arvada, CO.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Paired Counts of Mayfly Nymphs Above and Below Industrial Outfalls
#' @name Helsel.Hirsch.02.Mayfly.df
#' @description
#' Counts of mayfly nymphs at low flow in 12 small streams.
#'   In each stream, counts were recorded above and below industrial outfalls.
#' @usage
#' data(Helsel.Hirsch.02.Mayfly.df)
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 24 observations on the following 3 variables.
#'   \describe{
#'     \item{\code{Mayfly.Count}}{Number of mayfly nymphs counted}
#'     \item{\code{Stream}}{a factor indicating the stream number}
#'     \item{\code{Location}}{a factor indicating the location of the count (above vs. below)}
#'   }
#' }
#' @rawRd
#' \source{
#'   Helsel, D.R., and R.M. Hirsch. (2002). \emph{Statistical Methods in Water Resources Research}.
#'   Techniques of Water Resources Investigations, Book 4, Chapter A3.  U.S. Geological Survey,
#'   139--140.  \url{https://pubs.usgs.gov/tm/04/a03/tm4a3.pdf}.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Abstract: Hosking et al. (1985)
#' @name HoskingEtAl1985
#' @rawRd \alias{Hosking et al 1985}
#' @description
#' Detailed abstract of the manuscript: \cr \cr
#'   Hosking, J.R.M., J.R. Wallis, and E.F. Wood. (1985).  Estimation of the
#'   Generalized Extreme-Value Distribution by the Method of Probability-Weighted
#'   Moments.  \emph{Technometrics} \bold{27}(3), 251--261.
#' @rawRd
#' \concept{Hosking}
#' @rawRd
#' \concept{Abstract}
#' @details
#' This help page has been shortened to keep function help focused on usage,
#' arguments, return values, and examples. Extended method details, formulas,
#' and background material are available in \code{vignette("extended-function-details", package = "EnvStats")}, section \code{HoskingEtAl1985}.
#' @rawRd
#' \references{
#'   Forbes, C., M. Evans, N. Hastings, and B. Peacock. (2011).  Statistical Distributions.
#'   Fourth Edition. John Wiley and Sons, Hoboken, NJ.
#'
#'   Greenwood, J.A., J.M. Landwehr, N.C. Matalas, and J.R. Wallis. (1979).
#'   Probability Weighted Moments: Definition and Relation to Parameters of Several
#'   Distributions Expressible in Inverse Form.  \emph{Water Resources Research}
#'   \bold{15}(5), 1049--1054.
#'
#'   Hoeffding, W. (1948).  A Class of Statistics with Asymptotically Normal
#'   Distribution.  \emph{Annals of Mathematical Statistics} \bold{19}, 293--325.
#'
#'   Hosking, J.R.M. (1985).  Algorithm AS 215: Maximum-Likelihood Estimation of the
#'   Parameters of the Generalized Extreme-Value Distribution.
#'   \emph{Applied Statistics} \bold{34}(3), 301--310.
#'
#'   Hosking, J.R.M. (1990).  \eqn{L}-Moments:  Analysis and Estimation of
#'   Distributions Using Linear Combinations of Order Statistics.  \emph{Journal of
#'   the Royal Statistical Society, Series B} \bold{52}(1), 105--124.
#'
#'   Hosking, J.R.M., and J.R. Wallis (1995).  A Comparison of Unbiased and
#'   Plotting-Position Estimators of \eqn{L} Moments.  \emph{Water Resources
#'   Research} \bold{31}(8), 2019--2025.
#'
#'   Jenkinson, A.F. (1969).  Statistics of Extremes. \emph{Technical Note 98},
#'   World Meteorological Office, Geneva.
#'
#'   Johnson, N. L., S. Kotz, and A.W. Kemp. (1992).
#'   \emph{Univariate Discrete Distributions}.  Second Edition.
#'   John Wiley and Sons, New York, pp.4-8.
#'
#'   Johnson, N. L., S. Kotz, and N. Balakrishnan. (1995).
#'   \emph{Continuous Univariate Distributions, Volume 2}.
#'   Second Edition. John Wiley and Sons, New York.
#'
#'   Lehmann, E.L. (1975).  \emph{Nonparametrics:  Statistical Methods Based on Ranks}.
#'   Holden-Day, Oakland, CA, 457pp.
#' }
#' @rawRd
#' \author{
#'     Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \seealso{
#'   \link[=GEVD]{Generalized Extreme Value Distribution}, \code{\link{egevd}}.
#' }
#' @rawRd
#' \keyword{ distribution }
NULL

#' Fecal Coliform Data from the Illinois River
#' @name Lin.Evans.80.df
#' @description
#' Lin and Evans (1980) reported fecal coliform measures (organisms per 100 ml) from the
#'   Illinois River taken between 1971 and 1976.  The object \code{Lin.Evans.80.df} is a
#'   small subset of these data that were reported by Helsel and Hirsch (1992, p.162).
#' @usage
#' Lin.Evans.80.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 24 observations on the following 2 variables.
#'   \describe{
#'     \item{\code{Fecal.Coliform}}{a numeric vector of fecal coliform measure (organisms per 100 ml).}
#'     \item{\code{Season}}{an ordered factor indicating the season of collection}
#'   }
#' }
#' @rawRd
#' \source{
#'   Helsel, D.R., and R.M. Hirsch. (1992). \emph{Statistical Methods in Water Resources Research}.
#'   Elsevier, New York, NY, p.162.
#' }
#' @rawRd
#' \references{
#'   Lin, S.D., and R.L. Evans. (1980). \emph{Coliforms and fecal streptococcus in the Illinois River at Peoria, 1971-1976}.
#'   Illinois State Water Survey Report of Investigations No. 93. Urbana, IL, 28pp.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' The Three-Parameter Lognormal Distribution
#' @name Lognormal3
#' @aliases dlnorm3 plnorm3 qlnorm3 rlnorm3
#' @rawRd \alias{Three Parameter Lognormal}
#' @description
#' Density, distribution function, quantile function, and random generation
#'   for the three-parameter lognormal distribution with parameters \code{meanlog},
#'   \code{sdlog}, and \code{threshold}.
#' @usage
#' dlnorm3(x, meanlog = 0, sdlog = 1, threshold = 0)
#'   plnorm3(q, meanlog = 0, sdlog = 1, threshold = 0)
#'   qlnorm3(p, meanlog = 0, sdlog = 1, threshold = 0)
#'   rlnorm3(n, meanlog = 0, sdlog = 1, threshold = 0)
#' @rawRd
#' \arguments{
#'   \item{x}{
#'   vector of quantiles.
#' }
#'   \item{q}{
#'   vector of quantiles.
#' }
#'   \item{p}{
#'   vector of probabilities between 0 and 1.
#' }
#'   \item{n}{
#'   sample size.  If \code{length(n)} is larger than 1, then \code{length(n)}
#'   random values are returned.
#' }
#'   \item{meanlog}{
#'   vector of means of the distribution of the random variable on the log scale.
#'   The default is \code{meanlog=0}.
#' }
#'   \item{sdlog}{
#'   vector of (positive) standard deviations of the random variable on the log scale.
#'   The default is \code{sdlog=1}.
#' }
#'   \item{threshold}{
#'   vector of thresholds of the random variable on the log scale.  The default
#'   is \code{threshold=0}.
#' }
#' }
#' @rawRd
#' \details{
#'   The three-parameter lognormal distribution is simply the usual
#'   \link[stats:Lognormal]{two-parameter lognormal distribution} with a
#'   location shift.
#'
#'   Let \eqn{X} be a random variable with a three-parameter lognormal distribution
#'   with parameters \code{meanlog=}\eqn{\mu}, \code{sdlog=}\eqn{\sigma}, and
#'   \code{threshold=}\eqn{\gamma}.  Then the random variable \eqn{Y = X - \gamma}
#'   has a \link[stats:Lognormal]{lognormal distribution} with parameters
#'   \code{meanlog=}\eqn{\mu} and \code{sdlog=}\eqn{\sigma}.  Thus,
#'   \itemize{
#'     \item  \code{dlnorm3} calls \code{\link{dlnorm}} using the arguments
#'            \code{x = x - threshold}, \code{meanlog = meanlog},
#'            \code{sdlog = sdlog}
#'     \item \code{plnorm3} calls \code{\link{plnorm}} using the arguments
#'           \code{q = q - threshold}, \code{meanlog = meanlog}, \code{sdlog = sdlog}
#'     \item \code{qlnorm3} calls \code{\link{qlnorm}} using the arguments
#'           \code{q = q}, \code{meanlog = meanlog}, \code{sdlog = sdlog} and then adds
#'           the argument \code{threshold} to the result.
#'     \item \code{rlnorm3} calls \code{\link{rlnorm}} using the arguments
#'           \code{n = n}, \code{meanlog = meanlog}, \code{sdlog = sdlog} and then adds
#'           the argument \code{threshold} to the result.
#'   }
#'
#'   The threshold parameter \eqn{\gamma} affects only the location of the
#'   three-parameter lognormal distribution; it has no effect on the variance
#'   or the shape of the distribution.
#'
#'   Denote the mean, variance, and coefficient of variation of \eqn{Y = X - \gamma} by:
#'   \deqn{E(Y) = \theta}
#'   \deqn{Var(Y) = \eta^2}
#'   \deqn{CV(Y) = \tau = \eta/\theta}
#'   Then the mean, variance, and coefficient of variation of \eqn{X} are given by:
#'   \deqn{E(X) = \theta + \eta}
#'   \deqn{Var(X) = \eta^2}
#'   \deqn{CV(X) = \frac{\eta}{\theta + \gamma} = \frac{\tau \theta}{\theta + \gamma}}
#'   The relationships between the parameters \eqn{\mu}, \eqn{\sigma},
#'   \eqn{\theta}, \eqn{\eta}, and \eqn{\tau} are as follows:
#'   \deqn{\theta = \beta \sqrt{\omega}}
#'   \deqn{\eta = \beta \sqrt{\omega (\omega - 1)}}
#'   \deqn{\tau = \sqrt{\omega - 1}}
#'   \deqn{\mu = log(\frac{\theta}{\sqrt{\tau^2 + 1}})}
#'   \deqn{\sigma = \sqrt{log(\tau^2 + 1)}}
#'   where
#'   \deqn{\beta = e^\mu, \omega = exp(\sigma^2)}
#'
#'   Since quantiles of a distribution are preserved under monotonic transformations,
#'   the median of \eqn{X} is:
#'   \deqn{Median(X) = \gamma + \beta}
#' }
#' @rawRd
#' \value{
#'   \code{dlnorm3} gives the density, \code{plnorm3} gives the distribution function,
#'   \code{qlnorm3} gives the quantile function, and \code{rlnorm3} generates random
#'   deviates.
#' }
#' @rawRd
#' \references{
#'   Aitchison, J., and J.A.C. Brown (1957).  \emph{The Lognormal Distribution
#'   (with special references to its uses in economics)}.  Cambridge University
#'   Press, London, 176pp.
#'
#'   Crow, E.L., and K. Shimizu. (1988).  \emph{Lognormal Distributions:
#'   Theory and Applications}. Marcel Dekker, New York, 387pp.
#'
#'   Forbes, C., M. Evans, N. Hastings, and B. Peacock. (2011).  Statistical Distributions.
#'   Fourth Edition. John Wiley and Sons, Hoboken, NJ.
#'
#'   Johnson, N. L., S. Kotz, and N. Balakrishnan. (1994).
#'   \emph{Continuous Univariate Distributions, Volume 1}.
#'   Second Edition. John Wiley and Sons, New York.
#'
#'   Ott, W.R. (1990).  A Physical Explanation of the Lognormality of Pollutant
#'   Concentrations.  \emph{Journal of the Air and Waste Management Association} \bold{40},
#'   1378--1383.
#'
#'   Ott, W.R. (1995).  \emph{Environmental Statistics and Data Analysis}.
#'   Lewis Publishers, Boca Raton, FL, Chapter 9.
#'
#'   Royston, J.P. (1992b).  Estimation, Reference Ranges and Goodness of Fit for the
#'   Three-Parameter Log-Normal Distribution.  \emph{Statistics in Medicine} \bold{11},
#'   897--912.
#'
#'   Wicksell, S.D. (1917).  On Logarithmic Correlation with an Application to the
#'   Distribution of Ages at First Marriage.  \emph{Medd. Lunds. Astr. Obs.} \bold{84},
#'   1--21.
#' }
#' @rawRd
#' \author{
#'   Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \note{
#'   The \link[stats:Lognormal]{two-parameter lognormal distribution} is the
#'   distribution of a random variable whose logarithm is normally distributed.
#'   The two major characteristics of the two-parameter lognormal distribution
#'   are that it is bounded below at 0, and it is skewed to the right.  The
#'   three-parameter lognormal distribution is a generalization of the two-parameter
#'   lognormal distribution in which the distribution is shifted so that the
#'   threshold parameter is some arbitrary number, not necessarily 0.
#'
#'   The three-parameter lognormal distribution was introduced by Wicksell (1917) in a
#'   study of the distribution of ages at first marriage.  Both the two- and
#'   three-parameter lognormal distributions have been used in a variety of fields,
#'   including economics and business, industry, biology, ecology, atmospheric science,
#'   and geology (Crow and Shimizu, 1988).  Royston (1992) has discussed the application
#'   of the three-parameter lognormal distribution in the field of medicine.
#'
#'   The two-parameter lognormal distribution is often used to characterize chemical
#'   concentrations in the environment.  Ott (1990) has shown mathematically how a
#'   series of successive random dilutions gives rise to a distribution that can be
#'   approximated by a two-parameter lognormal distribution.
#'
#'   The three-pararameter lognormal distribution starts to resemble a normal
#'   distribution as the parameter \eqn{\sigma} (the standard deviation of
#'   \eqn{log(X-\gamma)} tends to 0.
#' }
#' @rawRd
#' \seealso{
#'   \link[stats:Lognormal]{Lognormal}, \code{\link{elnorm3}},
#'   \link{Probability Distributions and Random Numbers}.
#' }
#' @rawRd
#' \examples{
#'   # Density of the three-parameter lognormal distribution with
#'   # parameters meanlog=1, sdlog=2, and threshold=10, evaluated at 10.5:
#'
#'   dlnorm3(10.5, 1, 2, 10)
#'   #[1] 0.278794
#'
#'   #----------
#'
#'   # The cdf of the three-parameter lognormal distribution with
#'   # parameters meanlog=2, sdlog=3, and threshold=5, evaluated at 9:
#'
#'   plnorm3(9, 2, 3, 5)
#'   #[1] 0.4189546
#'
#'   #----------
#'
#'   # The median of the three-parameter lognormal distribution with
#'   # parameters meanlog=2, sdlog=3, and threshold=20:
#'
#'   qlnorm3(0.5, 2, 3, 20)
#'   #[1] 27.38906
#'
#'   #----------
#'
#'   # Random sample of 3 observations from the three-parameter lognormal
#'   # distribution with parameters meanlog=2, sdlog=1, and threshold=-5.
#'   # (Note: the call to set.seed simply allows you to reproduce this example.)
#'
#'   set.seed(20)
#'   rlnorm3(3, 2, 1, -5)
#'   #[1] 18.6339749 -0.8873173 39.0561521
#' }
#' @rawRd
#' \keyword{ distribution }
#' @rawRd
#' \keyword{ datagen }
NULL

#' The Lognormal Distribution (Alternative Parameterization)
#' @name LognormalAlt
#' @aliases dlnormAlt plnormAlt qlnormAlt rlnormAlt
#' @description
#' Density, distribution function, quantile function, and random generation
#'   for the lognormal distribution with parameters \code{mean} and \code{cv}.
#' @usage
#' dlnormAlt(x, mean = exp(1/2), cv = sqrt(exp(1) - 1), log = FALSE)
#'   plnormAlt(q, mean = exp(1/2), cv = sqrt(exp(1) - 1),
#'       lower.tail = TRUE, log.p = FALSE)
#'   qlnormAlt(p, mean = exp(1/2), cv = sqrt(exp(1) - 1),
#'       lower.tail = TRUE, log.p = FALSE)
#'   rlnormAlt(n, mean = exp(1/2), cv = sqrt(exp(1) - 1))
#' @rawRd
#' \arguments{
#'   \item{x}{
#'   vector of quantiles.
#' }
#'   \item{q}{
#'   vector of quantiles.
#' }
#'   \item{p}{
#'   vector of probabilities between 0 and 1.
#' }
#'   \item{n}{
#'   sample size.  If \code{length(n)} is larger than 1, then \code{length(n)}
#'   random values are returned.
#' }
#'   \item{mean}{
#'   vector of (positive) means of the distribution of the random variable.
#' }
#'   \item{cv}{
#'   vector of (positive) coefficients of variation of the random variable.
#' }
#'   \item{log, log.p}{
#'   logical; if \code{TRUE}, probabilities/densities \eqn{p} are returned as \eqn{log(p)}.
#' }
#'   \item{lower.tail}{
#'   logical; if \code{TRUE} (default), probabilities are \eqn{P[X \le x]},
#'   otherwise, \eqn{P[X > x]}.
#' }
#' }
#' @rawRd
#' \details{
#'   Let \eqn{X} be a random variable with a \link[stats:Lognormal]{lognormal distribution}
#'   with parameters \code{meanlog=}\eqn{\mu} and \code{sdlog=}\eqn{\sigma}.  That is,
#'   \eqn{\mu} and \eqn{\sigma} denote the mean and standard deviation of the random variable
#'   on the log scale.  The relationship between these parameters and the
#'   mean (\code{mean=}\eqn{\theta}) and coefficient of variation (\code{cv=}\eqn{\tau})
#'   of the distribution on the original scale is given by:
#'   \deqn{\mu = log(\frac{\theta}{\sqrt{\tau^2 + 1}}) \;\;\;\; (1)}
#'   \deqn{\sigma = [log(\tau^2 + 1)]^{1/2} \;\;\;\; (2)}
#'   \deqn{\theta = exp[\mu + (\sigma^2/2)] \;\;\;\; (3)}
#'   \deqn{\tau = [exp(\sigma^2) - 1]^{1/2} \;\;\;\; (4)}
#'   Thus, the functions \code{dlnormAlt}, \code{plnormAlt}, \code{qlnormAlt}, and
#'   \code{rlnormAlt} call the \R functions \code{\link{dlnorm}},
#'   \code{\link{plnorm}}, \code{\link{qlnorm}}, and \code{\link{rlnorm}},
#'   respectively using the following values for the \code{meanlog} and \code{sdlog}
#'   parameters: \cr
#'   \code{sdlog <- sqrt(log(1 + cv^2))}, \cr
#'   \code{meanlog <- log(mean) - (sdlog^2)/2}
#' }
#' @rawRd
#' \value{
#'   \code{dlnormAlt} gives the density, \code{plnormAlt} gives the distribution function,
#'   \code{qlnormAlt} gives the quantile function, and \code{rlnormAlt} generates random
#'   deviates.
#' }
#' @rawRd
#' \references{
#'   Forbes, C., M. Evans, N. Hastings, and B. Peacock. (2011).  Statistical Distributions.
#'   Fourth Edition. John Wiley and Sons, Hoboken, NJ.
#'
#'   Johnson, N. L., S. Kotz, and N. Balakrishnan. (1994).
#'   \emph{Continuous Univariate Distributions, Volume 1}.
#'   Second Edition. John Wiley and Sons, New York.
#'
#'   Limpert, E., W.A. Stahel, and M. Abbt. (2001).  Log-Normal Distributions Across the
#'   Sciences:  Keys and Clues.  \emph{BioScience} \bold{51}, 341--352.
#'
#'   Ott, W.R. (1995). \emph{Environmental Statistics and Data Analysis}.
#'   Lewis Publishers, Boca Raton, FL.
#'
#'   Singh, A., R. Maichle, and N. Armbya. (2010a).  \emph{ProUCL Version 4.1.00
#'   User Guide (Draft)}.  EPA/600/R-07/041, May 2010.  Office of Research and
#'   Development, U.S. Environmental Protection Agency, Washington, D.C.
#'
#'   Singh, A., N. Armbya, and A. Singh. (2010b).  \emph{ProUCL Version 4.1.00
#'   Technical Guide (Draft)}.  EPA/600/R-07/041, May 2010.  Office of Research and
#'   Development, U.S. Environmental Protection Agency, Washington, D.C.
#' }
#' @rawRd
#' \author{
#'   Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \note{
#'   The two-parameter \link[stats:Lognormal]{lognormal distribution} is the distribution
#'   of a random variable whose logarithm is normally distributed.  The two major
#'   characteristics of the lognormal distribution are that it is bounded below at 0,
#'   and it is skewed to the right.
#'
#'   Because the empirical distribution of many variables is inherently positive and
#'   skewed to the right (e.g., size of organisms, amount of rainfall, size of income,
#'   etc.), the lognormal distribution has been widely applied in several fields,
#'   including economics, business, industry, biology, ecology, atmospheric science, and
#'   geology (Aitchison and Brown, 1957; Crow and Shimizu, 1988).
#'
#'   Gibrat (1930) derived the lognormal distribution from theoretical assumptions,
#'   calling it the "law of proportionate effect", but Kapteyn (1903) had described a
#'   machine that was the mechanical equivalent.  The basic idea is that the
#'   Central Limit Theorem states that the distribution of the sum of several
#'   independent random variables tends to look like a normal distribution, no matter
#'   what the underlying distribution(s) of the original random variables, hence the
#'   product of several independent random variables tends to look like a lognormal
#'   distribution.
#'
#'   The lognormal distribution is often used to characterize chemical concentrations
#'   in the environment.  Ott (1990) has shown mathematically how a series of
#'   successive random dilutions gives rise to a distribution that can be approximated
#'   by a lognormal distribution.
#'
#'   A lognormal distribution starts to resemble a normal distribution as the
#'   parameter \eqn{\sigma} (the standard deviation of the log of the distribution)
#'   tends to 0.
#'
#'   Some EPA guidance documents (e.g., Singh et al., 2002; Singh et al., 2010a,b)
#'   discourage using the assumption of a lognormal distribution for some types of
#'   environmental data and recommend instead assessing whether the data appear to
#'   fit a gamma distribution.
#' }
#' @rawRd
#' \seealso{
#'   \link[stats:Lognormal]{Lognormal}, \code{\link{elnormAlt}},
#'   \link{Probability Distributions and Random Numbers}.
#' }
#' @rawRd
#' \examples{
#'   # Density of the lognormal distribution with parameters
#'   # mean=10 and cv=1, evaluated at 5:
#'
#'   dlnormAlt(5, mean = 10, cv = 1)
#'   #[1] 0.08788173
#'
#'   #----------
#'
#'   # The cdf of the lognormal distribution with parameters mean=2 and cv=3,
#'   # evaluated at 4:
#'
#'   plnormAlt(4, 2, 3)
#'   #[1] 0.8879132
#'
#'   #----------
#'
#'   # The median of the lognormal distribution with parameters
#'   # mean=10 and cv=1:
#'
#'   qlnormAlt(0.5, mean = 10, cv = 1)
#'   #[1] 7.071068
#'
#'   #----------
#'
#'   # Random sample of 3 observations from a lognormal distribution with
#'   # parameters mean=10 and cv=1.
#'   # (Note: the call to set.seed simply allows you to reproduce this example.)
#'
#'   set.seed(20)
#'   rlnormAlt(3, mean = 10, cv = 1)
#'   #[1] 18.615797  4.341402 31.265293
#' }
#' @rawRd
#' \keyword{ distribution }
#' @rawRd
#' \keyword{ datagen }
NULL

#' Mixture of Two Lognormal Distributions
#' @name LognormalMix
#' @aliases dlnormMix plnormMix qlnormMix rlnormMix
#' @description
#' Density, distribution function, quantile function, and random generation
#'   for a mixture of two lognormal distribution with parameters
#'   \code{meanlog1}, \code{sdlog1}, \code{meanlog2}, \code{sdlog2}, and \code{p.mix}.
#' @usage
#' dlnormMix(x, meanlog1 = 0, sdlog1 = 1, meanlog2 = 0, sdlog2 = 1, p.mix = 0.5)
#'   plnormMix(q, meanlog1 = 0, sdlog1 = 1, meanlog2 = 0, sdlog2 = 1, p.mix = 0.5)
#'   qlnormMix(p, meanlog1 = 0, sdlog1 = 1, meanlog2 = 0, sdlog2 = 1, p.mix = 0.5)
#'   rlnormMix(n, meanlog1 = 0, sdlog1 = 1, meanlog2 = 0, sdlog2 = 1, p.mix = 0.5)
#' @rawRd
#' \arguments{
#'   \item{x}{
#'   vector of quantiles.
#' }
#'   \item{q}{
#'   vector of quantiles.
#' }
#'   \item{p}{
#'   vector of probabilities between 0 and 1.
#' }
#'   \item{n}{
#'   sample size.  If \code{length(n)} is larger than 1, then \code{length(n)}
#'   random values are returned.
#' }
#'   \item{meanlog1}{
#'   vector of means of the first lognormal random variable on the log scale.
#'   The default is \code{meanlog1=0}.
#' }
#'   \item{sdlog1}{
#'   vector of standard deviations of the first lognormal random variable on
#'   the log scale.  The default is \code{sdlog1=1}.
#' }
#'   \item{meanlog2}{
#'   vector of means of the second lognormal random variable on the log scale.
#'   The default is \code{meanlog2=0}.
#' }
#'   \item{sdlog2}{
#'   vector of standard deviations of the second lognormal random variable on
#'   the log scale.  The default is \code{sdlog2=1}.
#' }
#'   \item{p.mix}{
#'   vector of probabilities between 0 and 1 indicating the mixing proportion.
#'   For \code{rlnormMix} this must be a single, non-missing number.
#' }
#' }
#' @rawRd
#' \details{
#'   Let \eqn{f(x; \mu, \sigma)} denote the density of a
#'   \link[stats:Lognormal]{lognormal random variable} with parameters
#'   \code{meanlog=}\eqn{\mu} and \code{sdlog=}\eqn{\sigma}.  The density, \eqn{g}, of a
#'   lognormal mixture random variable with parameters \code{meanlog1=}\eqn{\mu_1},
#'   \code{sdlog1=}\eqn{\sigma_1}, \code{meanlog2=}\eqn{\mu_2},
#'   \code{sdlog2=}\eqn{\sigma_2}, and \code{p.mix=}\eqn{p} is given by:
#'   \deqn{g(x; \mu_1, \sigma_1, \mu_2, \sigma_2, p) =
#'     (1 - p) f(x; \mu_1, \sigma_1) + p f(x; \mu_2, \sigma_2)}
#' }
#' @rawRd
#' \value{
#'   \code{dlnormMix} gives the density, \code{plnormMix} gives the distribution function,
#'   \code{qlnormMix} gives the quantile function, and \code{rlnormMix} generates random
#'   deviates.
#' }
#' @rawRd
#' \references{
#'   Gilliom, R.J., and D.R. Helsel. (1986).  Estimation of Distributional Parameters
#'   for Censored Trace Level Water Quality Data: 1. Estimation Techniques.
#'   \emph{Water Resources Research} \bold{22}, 135-146.
#'
#'   Johnson, N. L., S. Kotz, and A.W. Kemp. (1992).  \emph{Univariate Discrete
#'   Distributions}. Second Edition. John Wiley and Sons, New York, pp.53-54, and
#'   Chapter 8.
#'
#'   Johnson, N. L., S. Kotz, and N. Balakrishnan. (1994).
#'   \emph{Continuous Univariate Distributions, Volume 1}.
#'   Second Edition. John Wiley and Sons, New York.
#' }
#' @rawRd
#' \author{
#'   Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \note{
#'   A lognormal mixture distribution is often used to model positive-valued data
#'   that appear to be \dQuote{contaminated}; that is, most of the values appear to
#'   come from a single lognormal distribution, but a few \dQuote{outliers} are
#'   apparent.  In this case, the value of \code{meanlog2} would be larger than the
#'   value of \code{meanlog1}, and the mixing proportion \code{p.mix} would be fairly
#'   close to 0 (e.g., \code{p.mix=0.1}).  The value of the second standard deviation
#'   (\code{sdlog2}) may or may not be the same as the value for the first
#'   (\code{sdlog1}).
#' }
#' @rawRd
#' \seealso{
#'   \link[stats:Lognormal]{Lognormal},  \link{NormalMix},
#'   \link{Probability Distributions and Random Numbers}.
#' }
#' @rawRd
#' \examples{
#'   # Density of a lognormal mixture with parameters meanlog1=0, sdlog1=1,
#'   # meanlog2=2, sdlog2=3, p.mix=0.5, evaluated at 1.5:
#'
#'   dlnormMix(1.5, meanlog1 = 0, sdlog1 = 1, meanlog2 = 2, sdlog2 = 3, p.mix = 0.5)
#'   #[1] 0.1609746
#'
#'   #----------
#'
#'   # The cdf of a lognormal mixture with parameters meanlog1=0, sdlog1=1,
#'   # meanlog2=2, sdlog2=3, p.mix=0.2, evaluated at 4:
#'
#'   plnormMix(4, 0, 1, 2, 3, 0.2)
#'   #[1] 0.8175281
#'
#'   #----------
#'
#'   # The median of a lognormal mixture with parameters meanlog1=0, sdlog1=1,
#'   # meanlog2=2, sdlog2=3, p.mix=0.2:
#'
#'   qlnormMix(0.5, 0, 1, 2, 3, 0.2)
#'   #[1] 1.156891
#'
#'   #----------
#'
#'   # Random sample of 3 observations from a lognormal mixture with
#'   # parameters meanlog1=0, sdlog1=1, meanlog2=3, sdlog2=4, p.mix=0.2.
#'   # (Note: the call to set.seed simply allows you to reproduce this example.)
#'
#'   set.seed(20)
#'   rlnormMix(3, 0, 1, 2, 3, 0.2)
#'   #[1] 0.08975283 1.07591103 7.85482514
#' }
#' @rawRd
#' \keyword{ distribution }
#' @rawRd
#' \keyword{ datagen }
NULL

#' Mixture of Two Lognormal Distributions (Alternative Parameterization)
#' @name LognormalMixAlt
#' @aliases dlnormMixAlt plnormMixAlt qlnormMixAlt rlnormMixAlt
#' @description
#' Density, distribution function, quantile function, and random generation
#'   for a mixture of two lognormal distribution with parameters
#'   \code{mean1}, \code{cv1}, \code{mean2}, \code{cv2}, and \code{p.mix}.
#' @usage
#' dlnormMixAlt(x, mean1 = exp(1/2), cv1 = sqrt(exp(1) - 1),
#'       mean2 = exp(1/2), cv2 = sqrt(exp(1) - 1), p.mix = 0.5)
#'   plnormMixAlt(q, mean1 = exp(1/2), cv1 = sqrt(exp(1) - 1),
#'       mean2 = exp(1/2), cv2 = sqrt(exp(1) - 1), p.mix = 0.5)
#'   qlnormMixAlt(p, mean1 = exp(1/2), cv1 = sqrt(exp(1) - 1),
#'       mean2 = exp(1/2), cv2 = sqrt(exp(1) - 1), p.mix = 0.5)
#'   rlnormMixAlt(n, mean1 = exp(1/2), cv1 = sqrt(exp(1) - 1),
#'       mean2 = exp(1/2), cv2 = sqrt(exp(1) - 1), p.mix = 0.5)
#' @rawRd
#' \arguments{
#'   \item{x}{
#'   vector of quantiles.
#' }
#'   \item{q}{
#'   vector of quantiles.
#' }
#'   \item{p}{
#'   vector of probabilities between 0 and 1.
#' }
#'   \item{n}{
#'   sample size.  If \code{length(n)} is larger than 1, then \code{length(n)}
#'   random values are returned.
#' }
#'   \item{mean1}{
#'   vector of means of the first lognormal random variable.  The default is \cr
#'   \code{meanlog1=sqrt(exp(1) - 1)}.
#' }
#'   \item{cv1}{
#'   vector of coefficient of variations of the first lognormal random variable.
#'   The default is \code{sdlog1=sqrt(exp(1) - 1)}.
#' }
#'   \item{mean2}{
#'   vector of means of the second lognormal random variable.  The default is \cr
#'   \code{mean2=sqrt(exp(1) - 1)}.
#' }
#'   \item{cv2}{
#'   vector of coefficient of variations of the second lognormal random variable.
#'   The default is \code{sdlog2=sqrt(exp(1) - 1)}.
#' }
#'   \item{p.mix}{
#'   vector of probabilities between 0 and 1 indicating the mixing proportion.
#'   For \code{rlnormMixAlt} this must be a single, non-missing number.
#' }
#' }
#' @rawRd
#' \details{
#'   Let \eqn{f(x; \eta, \theta)} denote the density of a
#'   \link[=LognormalAlt]{lognormal random variable} with parameters
#'   \code{mean=}\eqn{\eta} and \code{cv=}\eqn{\theta}.  The density, \eqn{g}, of a
#'   lognormal mixture random variable with parameters \code{mean1=}\eqn{\eta_1},
#'   \code{cv1=}\eqn{\theta_1}, \code{mean2=}\eqn{\eta_2},
#'   \code{cv2=}\eqn{\theta_2}, and \code{p.mix=}\eqn{p} is given by:
#'   \deqn{g(x; \eta_1, \theta_1, \eta_2, \theta_2, p) =
#'     (1 - p) f(x; \eta_1, \theta_1) + p f(x; \eta_2, \theta_2)}
#'
#'   The default values for \code{mean1} and \code{cv1} correspond to a
#'   \link[stats:Lognormal]{lognormal distribution} with parameters
#'   \code{meanlog=0} and \code{sdlog=1}.  Similarly for the default values
#'   of \code{mean2} and \code{cv2}.
#' }
#' @rawRd
#' \value{
#'   \code{dlnormMixAlt} gives the density, \code{plnormMixAlt} gives the distribution
#'   function, \code{qlnormMixAlt} gives the quantile function, and
#'   \code{rlnormMixAlt} generates random deviates.
#' }
#' @rawRd
#' \references{
#'   Gilliom, R.J., and D.R. Helsel. (1986).  Estimation of Distributional Parameters
#'   for Censored Trace Level Water Quality Data: 1. Estimation Techniques.
#'   \emph{Water Resources Research} \bold{22}, 135-146.
#'
#'   Johnson, N. L., S. Kotz, and A.W. Kemp. (1992).  \emph{Univariate Discrete
#'   Distributions}. Second Edition. John Wiley and Sons, New York, pp.53-54, and
#'   Chapter 8.
#'
#'   Johnson, N. L., S. Kotz, and N. Balakrishnan. (1994).
#'   \emph{Continuous Univariate Distributions, Volume 1}.
#'   Second Edition. John Wiley and Sons, New York.
#' }
#' @rawRd
#' \author{
#'   Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \note{
#'   A lognormal mixture distribution is often used to model positive-valued data
#'   that appear to be \dQuote{contaminated}; that is, most of the values appear to
#'   come from a single lognormal distribution, but a few \dQuote{outliers} are
#'   apparent.  In this case, the value of \code{mean2} would be larger than the
#'   value of \code{mean1}, and the mixing proportion \code{p.mix} would be fairly
#'   close to 0 (e.g., \code{p.mix=0.1}).
#' }
#' @rawRd
#' \seealso{
#'   \link{LognormalAlt}, \link{LognormalMix}, \link[stats:Lognormal]{Lognormal},
#'   \link{NormalMix},  \link{Probability Distributions and Random Numbers}.
#' }
#' @rawRd
#' \examples{
#'   # Density of a lognormal mixture with parameters mean=2, cv1=3,
#'   # mean2=4, cv2=5, p.mix=0.5, evaluated at 1.5:
#'
#'   dlnormMixAlt(1.5, mean1 = 2, cv1 = 3, mean2 = 4, cv2 = 5, p.mix = 0.5)
#'   #[1] 0.1436045
#'
#'   #----------
#'
#'   # The cdf of a lognormal mixture with parameters mean=2, cv1=3,
#'   # mean2=4, cv2=5, p.mix=0.5, evaluated at 1.5:
#'
#'   plnormMixAlt(1.5, mean1 = 2, cv1 = 3, mean2 = 4, cv2 = 5, p.mix = 0.5)
#'   #[1] 0.6778064
#'
#'   #----------
#'
#'   # The median of a lognormal mixture with parameters mean=2, cv1=3,
#'   # mean2=4, cv2=5, p.mix=0.5:
#'
#'   qlnormMixAlt(0.5, 2, 3, 4, 5, 0.5)
#'   #[1] 0.6978355
#'
#'   #----------
#'
#'   # Random sample of 3 observations from a lognormal mixture with
#'   # parameters mean1=2, cv1=3, mean2=4, cv2=5, p.mix=0.5.
#'   # (Note: the call to set.seed simply allows you to reproduce this example.)
#'
#'   set.seed(20)
#'   rlnormMixAlt(3, 2, 3, 4, 5, 0.5)
#'   #[1]  0.70672151 14.43226313  0.05521329
#' }
#' @rawRd
#' \keyword{ distribution }
#' @rawRd
#' \keyword{ datagen }
NULL

#' The Truncated Lognormal Distribution
#' @name LognormalTrunc
#' @aliases dlnormTrunc plnormTrunc qlnormTrunc rlnormTrunc
#' @description
#' Density, distribution function, quantile function, and random generation
#'   for the truncated lognormal distribution with parameters \code{meanlog},
#'   \code{sdlog}, \code{min}, and \code{max}.
#' @usage
#' dlnormTrunc(x, meanlog = 0, sdlog = 1, min = 0, max = Inf)
#'   plnormTrunc(q, meanlog = 0, sdlog = 1, min = 0, max = Inf)
#'   qlnormTrunc(p, meanlog = 0, sdlog = 1, min = 0, max = Inf)
#'   rlnormTrunc(n, meanlog = 0, sdlog = 1, min = 0, max = Inf)
#' @rawRd
#' \arguments{
#'   \item{x}{
#'   vector of quantiles.
#' }
#'   \item{q}{
#'   vector of quantiles.
#' }
#'   \item{p}{
#'   vector of probabilities between 0 and 1.
#' }
#'   \item{n}{
#'   sample size.  If \code{length(n)} is larger than 1, then \code{length(n)}
#'   random values are returned.
#' }
#'   \item{meanlog}{
#'   vector of means of the distribution of the non-truncated random variable
#'   on the log scale.
#'   The default is \code{meanlog=0}.
#' }
#'   \item{sdlog}{
#'   vector of (positive) standard deviations of the non-truncated random variable
#'   on the log scale.
#'   The default is \code{sdlog=1}.
#' }
#'   \item{min}{
#'   vector of minimum values for truncation on the left.  The default value is
#'   \code{min=0}.
#' }
#'   \item{max}{
#'   vector of maximum values for truncation on the right.  The default value is
#'   \code{max=Inf}.
#' }
#' }
#' @rawRd
#' \details{
#'   See the help file for \link[stats:Lognormal]{the lognormal distribution}
#'   for information about the density and cdf of a lognormal distribution.
#'
#'   \emph{Probability Density and Cumulative Distribution Function} \cr
#'   Let \eqn{X} denote a random variable with density function \eqn{f(x)} and
#'   cumulative distribution function \eqn{F(x)}, and let
#'   \eqn{Y} denote the truncated version of \eqn{X} where \eqn{Y} is truncated
#'   below at \code{min=}\eqn{A} and above at\code{max=}\eqn{B}.  Then the density
#'   function of \eqn{Y}, denoted \eqn{g(y)}, is given by:
#'   \deqn{g(y) = frac{f(y)}{F(B) - F(A)}, A \le y \le B}
#'   and the cdf of Y, denoted \eqn{G(y)}, is given by:
#'   \tabular{lll}{
#'     \eqn{G(y) =}  \tab  0                                    \tab for \eqn{y < A} \cr
#'                   \tab \eqn{\frac{F(y) - F(A)}{F(B) - F(A)}} \tab for \eqn{A \le y \le B} \cr
#'                   \tab 1                                     \tab for \eqn{y > B} \cr
#'   }
#'
#'   \emph{Quantiles} \cr
#'   The \eqn{p^{th}} quantile \eqn{y_p} of \eqn{Y} is given by:
#'   \tabular{lll}{
#'     \eqn{y_p =}  \tab \eqn{A}                                \tab for \eqn{p = 0} \cr
#'                  \tab \eqn{F^{-1}\{p[F(B) - F(A)] + F(A)\} } \tab for \eqn{0 < p < 1} \cr
#'                  \tab \eqn{B}                                \tab for \eqn{p = 1} \cr
#'   }
#'
#'   \emph{Random Numbers} \cr
#'   Random numbers are generated using the inverse transformation method:
#'   \deqn{y = G^{-1}(u)}
#'   where \eqn{u} is a random deviate from a uniform \eqn{[0, 1]} distribution.
#' }
#' @rawRd
#' \value{
#'   \code{dlnormTrunc} gives the density, \code{plnormTrunc} gives the distribution function,
#'   \code{qlnormTrunc} gives the quantile function, and \code{rlnormTrunc} generates random
#'   deviates.
#' }
#' @rawRd
#' \references{
#'   Forbes, C., M. Evans, N. Hastings, and B. Peacock. (2011).  Statistical Distributions.
#'   Fourth Edition. John Wiley and Sons, Hoboken, NJ.
#'
#'   Johnson, N. L., S. Kotz, and N. Balakrishnan. (1994).
#'   \emph{Continuous Univariate Distributions, Volume 1}.
#'   Second Edition. John Wiley and Sons, New York.
#'
#'   Schneider, H. (1986).  \emph{Truncated and Censored Samples from Normal Populations}.
#'   Marcel Dekker, New York, Chapter 2.
#' }
#' @rawRd
#' \author{
#'   Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \note{
#'   A truncated lognormal distribution is sometimes used as an input distribution
#'   for probabilistic risk assessment.
#' }
#' @rawRd
#' \seealso{
#'   \link[stats:Lognormal]{Lognormal},
#'   \link{Probability Distributions and Random Numbers}.
#' }
#' @rawRd
#' \examples{
#'   # Density of a truncated lognormal distribution with parameters
#'   # meanlog=1, sdlog=0.75, min=0, max=10, evaluated at 2 and 4:
#'
#'   dlnormTrunc(c(2, 4), 1, 0.75, 0, 10)
#'   #[1] 0.2551219 0.1214676
#'
#'   #----------
#'
#'   # The cdf of a truncated lognormal distribution with parameters
#'   # meanlog=1, sdlog=0.75, min=0, max=10, evaluated at 2 and 4:
#'
#'   plnormTrunc(c(2, 4), 1, 0.75, 0, 10)
#'   #[1] 0.3558867 0.7266934
#'
#'   #----------
#'
#'   # The median of a truncated lognormal distribution with parameters
#'   # meanlog=1, sdlog=0.75, min=0, max=10:
#'
#'   qlnormTrunc(.5, 1, 0.75, 0, 10)
#'   #[1] 2.614945
#'
#'   #----------
#'
#'   # A random sample of 3 observations from a truncated lognormal distribution
#'   # with parameters meanlog=1, sdlog=0.75, min=0, max=10.
#'   # (Note: the call to set.seed simply allows you to reproduce this example.)
#'
#'   set.seed(20)
#'   rlnormTrunc(3, 1, 0.75, 0, 10)
#'   #[1] 5.754805 4.372218 1.706815
#' }
#' @rawRd
#' \keyword{ distribution }
#' @rawRd
#' \keyword{ datagen }
NULL

#' The Truncated Lognormal Distribution (Alternative Parameterization)
#' @name LognormalTruncAlt
#' @aliases dlnormTruncAlt plnormTruncAlt qlnormTruncAlt rlnormTruncAlt
#' @description
#' Density, distribution function, quantile function, and random generation
#'   for the truncated lognormal distribution with parameters \code{mean},
#'   \code{cv}, \code{min}, and \code{max}.
#' @usage
#' dlnormTruncAlt(x, mean = exp(1/2), cv = sqrt(exp(1) - 1), min = 0, max = Inf)
#'   plnormTruncAlt(q, mean = exp(1/2), cv = sqrt(exp(1) - 1), min = 0, max = Inf)
#'   qlnormTruncAlt(p, mean = exp(1/2), cv = sqrt(exp(1) - 1), min = 0, max = Inf)
#'   rlnormTruncAlt(n, mean = exp(1/2), cv = sqrt(exp(1) - 1), min = 0, max = Inf)
#' @rawRd
#' \arguments{
#'   \item{x}{
#'   vector of quantiles.
#' }
#'   \item{q}{
#'   vector of quantiles.
#' }
#'   \item{p}{
#'   vector of probabilities between 0 and 1.
#' }
#'   \item{n}{
#'   sample size.  If \code{length(n)} is larger than 1, then \code{length(n)}
#'   random values are returned.
#' }
#'   \item{mean}{
#'   vector of means of the distribution of the non-truncated random variable.
#'   The default is \code{mean=exp(1/2)}.
#' }
#'   \item{cv}{
#'   vector of (positive) coefficient of variations of the non-truncated random variable.
#'   The default is \code{cv=sqrt(exp(1)-1)}.
#' }
#'   \item{min}{
#'   vector of minimum values for truncation on the left.  The default value is
#'   \code{min=0}.
#' }
#'   \item{max}{
#'   vector of maximum values for truncation on the right.  The default value is
#'   \code{max=Inf}.
#' }
#' }
#' @rawRd
#' \details{
#'   See the help file for \link{LognormalAlt} for information about the
#'   density and cdf of a lognormal distribution with this alternative
#'   parameterization.
#'
#'   Let \eqn{X} denote a random variable with density function \eqn{f(x)} and
#'   cumulative distribution function \eqn{F(x)}, and let
#'   \eqn{Y} denote the truncated version of \eqn{X} where \eqn{Y} is truncated
#'   below at \code{min=}\eqn{A} and above at\code{max=}\eqn{B}.  Then the density
#'   function of \eqn{Y}, denoted \eqn{g(y)}, is given by:
#'   \deqn{g(y) = frac{f(y)}{F(B) - F(A)}, A \le y \le B}
#'   and the cdf of Y, denoted \eqn{G(y)}, is given by:
#'   \tabular{lll}{
#'     \eqn{G(y) =}  \tab  0                                    \tab for \eqn{y < A} \cr
#'                   \tab \eqn{\frac{F(y) - F(A)}{F(B) - F(A)}} \tab for \eqn{A \le y \le B} \cr
#'                   \tab 1                                     \tab for \eqn{y > B} \cr
#'   }
#'
#'   The \eqn{p^{th}} quantile \eqn{y_p} of \eqn{Y} is given by:
#'   \tabular{lll}{
#'     \eqn{y_p =}  \tab \eqn{A}                                \tab for \eqn{p = 0} \cr
#'                  \tab \eqn{F^{-1}\{p[F(B) - F(A)] + F(A)\} } \tab for \eqn{0 < p < 1} \cr
#'                  \tab \eqn{B}                                \tab for \eqn{p = 1} \cr
#'   }
#'
#'   Random numbers are generated using the inverse transformation method:
#'   \deqn{y = G^{-1}(u)}
#'   where \eqn{u} is a random deviate from a uniform \eqn{[0, 1]} distribution.
#' }
#' @rawRd
#' \value{
#'   \code{dlnormTruncAlt} gives the density, \code{plnormTruncAlt} gives the distribution function,
#'   \code{qlnormTruncAlt} gives the quantile function, and \code{rlnormTruncAlt} generates random
#'   deviates.
#' }
#' @rawRd
#' \references{
#'   Forbes, C., M. Evans, N. Hastings, and B. Peacock. (2011).  Statistical Distributions.
#'   Fourth Edition. John Wiley and Sons, Hoboken, NJ.
#'
#'   Johnson, N. L., S. Kotz, and N. Balakrishnan. (1994).
#'   \emph{Continuous Univariate Distributions, Volume 1}.
#'   Second Edition. John Wiley and Sons, New York.
#'
#'   Schneider, H. (1986).  \emph{Truncated and Censored Samples from Normal Populations}.
#'   Marcel Dekker, New York, Chapter 2.
#' }
#' @rawRd
#' \author{
#'   Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \note{
#'   A truncated lognormal distribution is sometimes used as an input distribution
#'   for probabilistic risk assessment.
#' }
#' @rawRd
#' \seealso{
#'   \link{LognormalAlt},
#'   \link{Probability Distributions and Random Numbers}.
#' }
#' @rawRd
#' \examples{
#'   # Density of a truncated lognormal distribution with parameters
#'   # mean=10, cv=1, min=0, max=20, evaluated at 2 and 12:
#'
#'   dlnormTruncAlt(c(2, 12), 10, 1, 0, 20)
#'   #[1] 0.08480874 0.03649884
#'
#'   #----------
#'
#'   # The cdf of a truncated lognormal distribution with parameters
#'   # mean=10, cv=1, min=0, max=20, evaluated at 2 and 12:
#'
#'   plnormTruncAlt(c(2, 4), 10, 1, 0, 20)
#'   #[1] 0.07230627 0.82467603
#'
#'   #----------
#'
#'   # The median of a truncated lognormal distribution with parameters
#'   # mean=10, cv=1, min=0, max=20:
#'
#'   qlnormTruncAlt(.5, 10, 1, 0, 20)
#'   #[1] 6.329505
#'
#'   #----------
#'
#'   # A random sample of 3 observations from a truncated lognormal distribution
#'   # with parameters mean=10, cv=1, min=0, max=20.
#'   # (Note: the call to set.seed simply allows you to reproduce this example.)
#'
#'   set.seed(20)
#'   rlnormTruncAlt(3, 10, 1, 0, 20)
#'   #[1]  6.685391 17.445387 18.543553
#' }
#' @rawRd
#' \keyword{ distribution }
#' @rawRd
#' \keyword{ datagen }
NULL

#' Copper and Zinc Concentrations in Shallow Ground Water
#' @name Millard.Deverel.88.df
#' @description
#' Copper and zinc concentrations (mg/L) in shallow ground water from two geological
#'   zones (Alluvial Fan and Basin-Trough) in the San Joaquin Valley, CA.  There are 68
#'   samples from the Alluvial Fan zone and 50 from the Basin-Trough zone.  Some
#'   observations are reported as <\eqn{DL}, where \eqn{DL} denotes a detection limit.  There
#'   are multiple detection limits for both the copper and zinc data in each of the
#'   geological zones.
#' @usage
#' Millard.Deverel.88.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 118 observations on the following 8 variables.
#'   \describe{
#'     \item{\code{Cu.orig}}{a character vector of original copper concentrations (mg/L)}
#'     \item{\code{Cu}}{a numeric vector of copper concentrations with nondetects coded to their detection limit}
#'     \item{\code{Cu.censored}}{a logical vector indicating which copper concentrations are censored}
#'     \item{\code{Zn.orig}}{a character vector of original zinc concentrations (mg/L)}
#'     \item{\code{Zn}}{a numeric vector of zinc concentrations with nondetects coded to their detection limit}
#'     \item{\code{Zn.censored}}{a logical vector indicating which zinc concentrations are censored}
#'     \item{\code{Zone}}{a factor indicating the zone (alluvial fan vs. basin trough)}
#'     \item{\code{Location}}{a numeric vector indicating the sampling location}
#'   }
#' }
#' @rawRd
#' \source{
#'   Millard, S.P., and S.J. Deverel. (1988). Nonparametric Statistical Methods for Comparing Two Sites Based on Data With Multiple Nondetect Limits.
#'   \emph{Water Resources Research}, \bold{24}(12), 2087-2098.
#' }
#' @rawRd
#' \references{
#'   Deverel, S.J., R.J. Gilliom, R. Fujii, J.A. Izbicki, and J.C. Fields. (1984).
#'   \emph{Areal Distribution of Selenium and Other Inorganic Constituents in Shallow Ground Water of the San Luis Drain Service Area, San Joaquin, California: A Preliminary Study}.
#'   U.S. Geological Survey Water Resources Investigative Report 84-4319.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Modified 1,2,3,4-Tetrachlorobenzene Data with Censored Values
#' @name Modified.TcCB.df
#' @description
#' Artificial 1,2,3,4-Tetrachlorobenzene (TcCB) concentrations with censored values;
#'   based on the reference area data stored in \code{\link{EPA.94b.tccb.df}}.  The data
#'   frame \code{\link{EPA.94b.tccb.df}} contains TcCB concentrations (ppb) in soil samples
#'   at a reference area and a cleanup area.  The data frame \cr
#'   \code{Modified.TcCB.df} contains a modified version of the data from the reference area.
#'   For this data set, the concentrations of TcCB less than 0.5 ppb have been recoded as
#'   \code{<0.5}.
#' @usage
#' Modified.TcCB.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 47 observations on the following 3 variables.
#'   \describe{
#'     \item{\code{TcCB.orig}}{a character vector of original TcCB concentrations (ppb)}
#'     \item{\code{TcCB}}{a numeric vector with censored observations set to their detection level}
#'     \item{\code{Censored}}{a logical vector indicating which observations are censored}
#'   }
#' }
#' @rawRd
#' \source{
#'   Millard, S.P., and N.K. Neerchal. (2001). \emph{Environmental Statistics with S-PLUS}.
#'   CRC Press, Boca Raton, FL, p.595.
#' }
#' @rawRd
#' \references{
#'   USEPA. (1994b). \emph{Statistical Methods for Evaluating the Attainment of Cleanup Standards, Volume 3: Reference-Based Standards for Soils and Solid Media}.
#'   EPA/230-R-94-004. Office of Policy, Planning, and Evaluation, U.S. Environmental Protection Agency, Washington, D.C.
#' }
#' @rawRd
#' \seealso{
#'   \code{\link{EPA.94b.tccb.df}}.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' NIOSH Air Lead Levels Data
#' @name NIOSH.89.air.lead.vec
#' @description
#' Air lead levels collected by the National Institute for Occupational Safety and Health
#'   (NIOSH) at 15 different areas within the Alma American Labs, Fairply, CO,
#'   for health hazard evaluation (HETA 89-052) on Februay 23, 1989.
#' @usage
#' NIOSH.89.air.lead.vec
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A numeric vector with 15 elements containing air lead concentrations (\eqn{\mu g/m^3}).
#' }
#' @rawRd
#' \source{
#'   Krishnamoorthy, K., T. Matthew, and G. Ramachandran. (2006).
#'   Generalized P-Values and Confidence Intervals:  A Novel Approach for Analyzing
#'   Lognormally Distributed Exposure Data.
#'   \emph{Journal of Occupational and Environmental Hygiene}, \bold{3}, 642--650.
#' }
#' @rawRd
#' \references{
#'   Zou, G.Y., C.Y. Huo, and J. Taleban. (2009).  Simple Confidence Intervals for
#'   Lognormal Means and their Differences with Environmental Applications.
#'   \emph{Environmetrics}, \bold{20}, 172--180.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Mixture of Two Normal Distributions
#' @name NormalMix
#' @aliases dnormMix pnormMix qnormMix rnormMix
#' @description
#' Density, distribution function, quantile function, and random generation
#'   for a mixture of two normal distribution with parameters
#'   \code{mean1}, \code{sd1}, \code{mean2}, \code{sd2}, and \code{p.mix}.
#' @usage
#' dnormMix(x, mean1 = 0, sd1 = 1, mean2 = 0, sd2 = 1, p.mix = 0.5)
#'   pnormMix(q, mean1 = 0, sd1 = 1, mean2 = 0, sd2 = 1, p.mix = 0.5)
#'   qnormMix(p, mean1 = 0, sd1 = 1, mean2 = 0, sd2 = 1, p.mix = 0.5)
#'   rnormMix(n, mean1 = 0, sd1 = 1, mean2 = 0, sd2 = 1, p.mix = 0.5)
#' @rawRd
#' \arguments{
#'   \item{x}{
#'   vector of quantiles.
#' }
#'   \item{q}{
#'   vector of quantiles.
#' }
#'   \item{p}{
#'   vector of probabilities between 0 and 1.
#' }
#'   \item{n}{
#'   sample size.  If \code{length(n)} is larger than 1, then \code{length(n)}
#'   random values are returned.
#' }
#'   \item{mean1}{
#'   vector of means of the first normal random variable.
#'   The default is \code{mean1=0}.
#' }
#'   \item{sd1}{
#'   vector of standard deviations of the first normal random variable.
#'   The default is \code{sd1=1}.
#' }
#'   \item{mean2}{
#'   vector of means of the second normal random variable.
#'   The default is \code{mean2=0}.
#' }
#'   \item{sd2}{
#'   vector of standard deviations of the second normal random variable.
#'   The default is \code{sd2=1}.
#' }
#'   \item{p.mix}{
#'   vector of probabilities between 0 and 1 indicating the mixing proportion.
#'   For \code{rnormMix} this must be a single, non-missing number.
#' }
#' }
#' @rawRd
#' \details{
#'   Let \eqn{f(x; \mu, \sigma)} denote the density of a
#'   \link[stats:Normal]{normal random variable} with parameters
#'   \code{mean=}\eqn{\mu} and \code{sd=}\eqn{\sigma}.  The density, \eqn{g}, of a
#'   normal mixture random variable with parameters \code{mean1=}\eqn{\mu_1},
#'   \code{sd1=}\eqn{\sigma_1}, \code{mean2=}\eqn{\mu_2},
#'   \code{sd2=}\eqn{\sigma_2}, and \code{p.mix=}\eqn{p} is given by:
#'   \deqn{g(x; \mu_1, \sigma_1, \mu_2, \sigma_2, p) =
#'     (1 - p) f(x; \mu_1, \sigma_1) + p f(x; \mu_2, \sigma_2)}
#' }
#' @rawRd
#' \value{
#'   \code{dnormMix} gives the density, \code{pnormMix} gives the distribution function,
#'   \code{qnormMix} gives the quantile function, and \code{rnormMix} generates random
#'   deviates.
#' }
#' @rawRd
#' \references{
#'   Johnson, N. L., S. Kotz, and A.W. Kemp. (1992).  \emph{Univariate Discrete
#'   Distributions}. Second Edition. John Wiley and Sons, New York, pp.53-54, and
#'   Chapter 8.
#'
#'   Johnson, N. L., S. Kotz, and N. Balakrishnan. (1994).
#'   \emph{Continuous Univariate Distributions, Volume 1}.
#'   Second Edition. John Wiley and Sons, New York.
#' }
#' @rawRd
#' \author{
#'   Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \note{
#'   A normal mixture distribution is sometimes used to model data
#'   that appear to be \dQuote{contaminated}; that is, most of the values appear to
#'   come from a single normal distribution, but a few \dQuote{outliers} are
#'   apparent.  In this case, the value of \code{mean2} would be larger than the
#'   value of \code{mean1}, and the mixing proportion \code{p.mix} would be fairly
#'   close to 0 (e.g., \code{p.mix=0.1}).  The value of the second standard deviation
#'   (\code{sd2}) may or may not be the same as the value for the first
#'   (\code{sd1}).
#'
#'   Another application of the normal mixture distribution is to bi-modal data;
#'   that is, data exhibiting two modes.
#' }
#' @rawRd
#' \seealso{
#'   \link[stats:Normal]{Normal},  \link{LognormalMix},
#'   \link{Probability Distributions and Random Numbers}.
#' }
#' @rawRd
#' \examples{
#'   # Density of a normal mixture with parameters mean1=0, sd1=1,
#'   #  mean2=4, sd2=2, p.mix=0.5, evaluated at 1.5:
#'
#'   dnormMix(1.5, mean2=4, sd2=2)
#'   #[1] 0.1104211
#'
#'   #----------
#'
#'   # The cdf of a normal mixture with parameters mean1=10, sd1=2,
#'   # mean2=20, sd2=2, p.mix=0.1, evaluated at 15:
#'
#'   pnormMix(15, 10, 2, 20, 2, 0.1)
#'   #[1] 0.8950323
#'
#'   #----------
#'
#'   # The median of a normal mixture with parameters mean1=10, sd1=2,
#'   # mean2=20, sd2=2, p.mix=0.1:
#'
#'   qnormMix(0.5, 10, 2, 20, 2, 0.1)
#'   #[1] 10.27942
#'
#'   #----------
#'
#'   # Random sample of 3 observations from a normal mixture with
#'   # parameters mean1=0, sd1=1, mean2=4, sd2=2, p.mix=0.5.
#'   # (Note: the call to set.seed simply allows you to reproduce this example.)
#'
#'   set.seed(20)
#'   rnormMix(3, mean2=4, sd2=2)
#'   #[1] 0.07316778 2.06112801 1.05953620
#' }
#' @rawRd
#' \keyword{ distribution }
#' @rawRd
#' \keyword{ datagen }
NULL

#' The Truncated Normal Distribution
#' @name NormalTrunc
#' @aliases dnormTrunc pnormTrunc qnormTrunc rnormTrunc
#' @description
#' Density, distribution function, quantile function, and random generation
#'   for the truncated normal distribution with parameters \code{mean},
#'   \code{sd}, \code{min}, and \code{max}.
#' @usage
#' dnormTrunc(x, mean = 0, sd = 1, min = -Inf, max = Inf)
#'   pnormTrunc(q, mean = 0, sd = 1, min = -Inf, max = Inf)
#'   qnormTrunc(p, mean = 0, sd = 1, min = -Inf, max = Inf)
#'   rnormTrunc(n, mean = 0, sd = 1, min = -Inf, max = Inf)
#' @rawRd
#' \arguments{
#'   \item{x}{
#'   vector of quantiles.
#' }
#'   \item{q}{
#'   vector of quantiles.
#' }
#'   \item{p}{
#'   vector of probabilities between 0 and 1.
#' }
#'   \item{n}{
#'   sample size.  If \code{length(n)} is larger than 1, then \code{length(n)}
#'   random values are returned.
#' }
#'   \item{mean}{
#'   vector of means of the distribution of the non-truncated random variable.
#'   The default is \code{mean=0}.
#' }
#'   \item{sd}{
#'   vector of (positive) standard deviations of the non-truncated random variable.
#'   The default is \code{sd=1}.
#' }
#'   \item{min}{
#'   vector of minimum values for truncation on the left.  The default value is
#'   \code{min=-Inf}.
#' }
#'   \item{max}{
#'   vector of maximum values for truncation on the right.  The default value is
#'   \code{max=Inf}.
#' }
#' }
#' @rawRd
#' \details{
#'   See the help file for \link[stats:Normal]{the normal distribution}
#'   for information about the density and cdf of a normal distribution.
#'
#'   \emph{Probability Density and Cumulative Distribution Function} \cr
#'   Let \eqn{X} denote a random variable with density function \eqn{f(x)} and
#'   cumulative distribution function \eqn{F(x)}, and let
#'   \eqn{Y} denote the truncated version of \eqn{X} where \eqn{Y} is truncated
#'   below at \code{min=}\eqn{A} and above at\code{max=}\eqn{B}.  Then the density
#'   function of \eqn{Y}, denoted \eqn{g(y)}, is given by:
#'   \deqn{g(y) = frac{f(y)}{F(B) - F(A)}, A \le y \le B}
#'   and the cdf of Y, denoted \eqn{G(y)}, is given by:
#'   \tabular{lll}{
#'     \eqn{G(y) =}  \tab  0                                    \tab for \eqn{y < A} \cr
#'                   \tab \eqn{\frac{F(y) - F(A)}{F(B) - F(A)}} \tab for \eqn{A \le y \le B} \cr
#'                   \tab 1                                     \tab for \eqn{y > B} \cr
#'   }
#'
#'   \emph{Quantiles} \cr
#'   The \eqn{p^{th}} quantile \eqn{y_p} of \eqn{Y} is given by:
#'   \tabular{lll}{
#'     \eqn{y_p =}  \tab \eqn{A}                                \tab for \eqn{p = 0} \cr
#'                  \tab \eqn{F^{-1}\{p[F(B) - F(A)] + F(A)\} } \tab for \eqn{0 < p < 1} \cr
#'                  \tab \eqn{B}                                \tab for \eqn{p = 1} \cr
#'   }
#'
#'   \emph{Random Numbers} \cr
#'   Random numbers are generated using the inverse transformation method:
#'   \deqn{y = G^{-1}(u)}
#'   where \eqn{u} is a random deviate from a uniform \eqn{[0, 1]} distribution.
#'
#'   \emph{Mean and Variance} \cr
#'   The expected value of a truncated normal random variable with parameters
#'   \code{mean=}\eqn{\mu}, \code{sd=}\eqn{\sigma}, \code{min=}\eqn{A}, and
#'   \code{max=}\eqn{B} is given by:
#'   \deqn{E(Y) = \mu + \sigma^2 \frac{f(A) - f(B)}{F(B) - F(A)}}
#'   (Johnson et al., 1994, p.156; Schneider, 1986, p.17).
#'
#'   The variance of this random variable is given by:
#'   \deqn{\sigma^2 + \sigma^3 \{z_A f(A) - z_B f(B) - \sigma[f(A) - f(B)]^2 \}}
#'   where
#'   \deqn{z_A = \frac{A - \mu}{\sigma}; \, z_B = \frac{B - \mu}{\sigma}}
#'   (Johnson et al., 1994, p.158; Schneider, 1986, p.17).
#' }
#' @rawRd
#' \value{
#'   \code{dnormTrunc} gives the density, \code{pnormTrunc} gives the distribution function,
#'   \code{qnormTrunc} gives the quantile function, and \code{rnormTrunc} generates random
#'   deviates.
#' }
#' @rawRd
#' \references{
#'   Forbes, C., M. Evans, N. Hastings, and B. Peacock. (2011).  Statistical Distributions.
#'   Fourth Edition. John Wiley and Sons, Hoboken, NJ.
#'
#'   Johnson, N. L., S. Kotz, and N. Balakrishnan. (1994).
#'   \emph{Continuous Univariate Distributions, Volume 1}.
#'   Second Edition. John Wiley and Sons, New York.
#'
#'   Schneider, H. (1986).  \emph{Truncated and Censored Samples from Normal Populations}.
#'   Marcel Dekker, New York, Chapter 2.
#' }
#' @rawRd
#' \author{
#'   Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \note{
#'   A truncated normal distribution is sometimes used as an input distribution
#'   for probabilistic risk assessment.
#' }
#' @rawRd
#' \seealso{
#'   \link[stats:Normal]{Normal},
#'   \link{Probability Distributions and Random Numbers}.
#' }
#' @rawRd
#' \examples{
#'   # Density of a truncated normal distribution with parameters
#'   # mean=10, sd=2, min=8, max=13, evaluated at 10 and 11.5:
#'
#'   dnormTrunc(c(10, 11.5), 10, 2, 8, 13)
#'   #[1] 0.2575358 0.1943982
#'
#'   #----------
#'
#'   # The cdf of a truncated normal distribution with parameters
#'   # mean=10, sd=2, min=8, max=13, evaluated at 10 and 11.5:
#'
#'   pnormTrunc(c(10, 11.5), 10, 2, 8, 13)
#'   #[1] 0.4407078 0.7936573
#'
#'   #----------
#'
#'   # The median of a truncated normal distribution with parameters
#'   # mean=10, sd=2, min=8, max=13:
#'
#'   qnormTrunc(.5, 10, 2, 8, 13)
#'   #[1] 10.23074
#'
#'   #----------
#'
#'   # A random sample of 3 observations from a truncated normal distribution
#'   # with parameters mean=10, sd=2, min=8, max=13.
#'   # (Note: the call to set.seed simply allows you to reproduce this example.)
#'
#'   set.seed(20)
#'   rnormTrunc(3, 10, 2, 8, 13)
#'   #[1] 11.975223 11.373711  9.361258
#' }
#' @rawRd
#' \keyword{ distribution }
#' @rawRd
#' \keyword{ datagen }
NULL

#' Ammonium Concentration in Precipitation Measured at Olympic National Park Hoh Ranger Station
#' @name Olympic.NH4.df
#' @description
#' Ammonium (NH\eqn{_4}) concentration (mg/L) in precipitation measured at
#'   Olympic National Park, Hoh Ranger Station (WA14), weekly or every other week
#'   from January 6, 2009 through December 20, 2011.
#' @usage
#' Olympic.NH4.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 102 observations on the following 6 variables.
#'   \describe{
#'     \item{\code{Date.On}}{Start of collection period.
#'       Date on which the sample bucket was installed on the collector.}
#'     \item{\code{Date.Off}}{End of collection period.
#'       Date on which the sample bucket was removed from the collector.}
#'     \item{\code{Week}}{a numeric vector indicating the cumulative week number
#'       starting from January 1, 2009.}
#'     \item{\code{NH4.Orig.mg.per.L}}{a character vector of the original NH\eqn{_4}
#'       concentrations reported either as the observed value or less than some
#'       detection limit.  For values reported as less than a detection limit,
#'       the value reported is the actual limit of detection or, in the case of a
#'       diluted sample, the product of the detection limit value and the
#'       dilution factor.}
#'     \item{\code{NH4.mg.per.L}}{a numeric vector of NH\eqn{_4} concentrations with
#'       non-detects coded to their detection limit.}
#'     \item{\code{Censored}}{a logical vector indicating which observations are censored.}
#'   }
#' }
#' @rawRd
#' \details{
#'   \describe{
#'   \item{Station}{Olympic National Park-Hoh Ranger Station (WA14)}
#'   \item{Location}{Jefferson County, Washington}
#'   \item{Latitude}{47.8597}
#'   \item{Longitude}{-123.9325}
#'   \item{Elevation}{182 meters}
#'   \item{USGS 1:24000 Map Name}{Owl Mountain}
#'   \item{Operating Agency}{Olympic National Park}
#'   \item{Sponsoring Agency}{NPS-Air Resources Division}
#'   }
#' }
#' @rawRd
#' \source{
#'   National Atmospheric Deposition Program, National Trends Network (NADP/NTN). \cr
#'   \url{https://nadp.slh.wisc.edu/sites/ntn-WA14/}
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Ozone Concentrations in the Northeast U.S.
#' @name Ozone.NE.df
#' @description
#' Ozone concentrations in 41 U.S. cities based on daily maxima collected between June and August 1974.
#' @usage
#' Ozone.NE.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 41 observations on the following 5 variables.
#'   \describe{
#'     \item{\code{Median}}{median of daily maxima ozone concentration (ppb).}
#'     \item{\code{Quartile}}{Upper quartile (i.e., 75th percentile) of daily maxima ozone concentration (ppb).}
#'     \item{\code{City}}{a factor indicating the city}
#'     \item{\code{Longitude}}{negative longitude of the city}
#'     \item{\code{Latitude}}{latitude of the city}
#'   }
#' }
#' @rawRd
#' \source{
#'   Cleveland, W.S., Kleiner, B., McRae, J.E., Warner, J.L., and Pasceri, P.E. (1975).
#'   \emph{The Analysis of Ground-Level Ozone Data from New Jersey, New York, Connecticut, and
#'   Massachusetts: Data Quality Assessment and Temporal and Geographical Properties}.
#'   Bell Laboratories Memorandum.
#'
#'   The original data were collected by the New Jersey Department of Environmental Protection,
#'   the New York State Department of Environmental Protection, the Boyce Thompson Institute
#'   (Yonkers, for New York data), the Connecticut Department of Environmental Protection, and
#'   the Massachusetts Department of Public Health.
#' }
#' @rawRd
#' \examples{
#'   summary(Ozone.NE.df)
#'   #     Median          Quartile               City      Longitude
#'   # Min.   : 34.00   Min.   : 48.00   Asbury Park: 1   Min.   :-74.71
#'   # 1st Qu.: 58.00   1st Qu.: 79.75   Babylon    : 1   1st Qu.:-73.74
#'   # Median : 65.00   Median : 90.00   Bayonne    : 1   Median :-73.17
#'   # Mean   : 68.15   Mean   : 95.10   Boston     : 1   Mean   :-72.94
#'   # 3rd Qu.: 80.00   3rd Qu.:112.25   Bridgeport : 1   3rd Qu.:-72.08
#'   # Max.   :100.00   Max.   :145.00   Cambridge  : 1   Max.   :-71.05
#'   #                  NA's   :  1.00   (Other)    :35
#'   #    Latitude
#'   # Min.   :40.22
#'   # 1st Qu.:40.97
#'   # Median :41.56
#'   # Mean   :41.60
#'   # 3rd Qu.:42.25
#'   # Max.   :43.32
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' The Pareto Distribution
#' @name Pareto
#' @aliases dpareto ppareto qpareto rpareto
#' @description
#' Density, distribution function, quantile function, and random generation
#'   for the Pareto distribution with parameters \code{location} and \code{shape}.
#' @usage
#' dpareto(x, location, shape = 1)
#'   ppareto(q, location, shape = 1)
#'   qpareto(p, location, shape = 1)
#'   rpareto(n, location, shape = 1)
#' @rawRd
#' \arguments{
#'   \item{x}{
#'   vector of quantiles.
#' }
#'   \item{q}{
#'   vector of quantiles.
#' }
#'   \item{p}{
#'   vector of probabilities between 0 and 1.
#' }
#'   \item{n}{
#'   sample size.  If \code{length(n)} is larger than 1, then \code{length(n)}
#'   random values are returned.
#' }
#'   \item{location}{
#'   vector of (positive) location parameters.
#' }
#'   \item{shape}{
#'   vector of (positive) shape parameters.  The default is \code{shape=1}.
#' }
#' }
#' @rawRd
#' \details{
#'   Let \eqn{X} be a Pareto random variable with parameters \code{location=}\eqn{\eta}
#'   and \code{shape=}\eqn{\theta}.  The density function of \eqn{X} is given by:
#'   \deqn{f(x; \eta, \theta) = \frac{\theta \eta^\theta}{x^{\theta + 1}}, \; \eta > 0, \; \theta > 0, \; x \ge \eta}
#'   The cumulative distribution function of \eqn{X} is given by:
#'   \deqn{F(x; \eta, \theta) = 1 - (\frac{\eta}{x})^\theta}
#'   and the \eqn{p}'th quantile of \eqn{X} is given by:
#'   \deqn{x_p = \eta (1 - p)^{-1/\theta}, \; 0 \le p \le 1}
#'   The mode, mean, median, variance, and coefficient of variation of \eqn{X} are given by:
#'   \deqn{Mode(X) = \eta}
#'   \deqn{E(X) = \frac{\theta \eta}{\theta - 1}, \; \theta > 1}
#'   \deqn{Median(X) = x_{0.5} = 2^{1/\theta} \eta}
#'   \deqn{Var(X) = \frac{\theta \eta^2}{(\theta - 1)^2 (\theta - 1)}, \; \theta > 2}
#'   \deqn{CV(X) = [\theta (\theta - 2)]^{-1/2}, \; \theta > 2}
#' }
#' @rawRd
#' \value{
#'   \code{dpareto} gives the density, \code{ppareto} gives the distribution function,
#'   \code{qpareto} gives the quantile function, and \code{rpareto} generates random
#'   deviates.
#' }
#' @rawRd
#' \references{
#'   Forbes, C., M. Evans, N. Hastings, and B. Peacock. (2011).  Statistical Distributions.
#'   Fourth Edition. John Wiley and Sons, Hoboken, NJ.
#'
#'   Johnson, N. L., S. Kotz, and N. Balakrishnan. (1994).
#'   \emph{Continuous Univariate Distributions, Volume 1}.
#'   Second Edition. John Wiley and Sons, New York.
#' }
#' @rawRd
#' \author{
#'   Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \note{
#'   The Pareto distribution is named after Vilfredo Pareto (1848-1923), a professor
#'   of economics.  It is derived from Pareto's law, which states that the number of
#'   persons \eqn{N} having income \eqn{\ge x} is given by:
#'   \deqn{N = A x^{-\theta}}
#'   where \eqn{\theta} denotes Pareto's constant and is the shape parameter for the
#'   probability distribution.
#'
#'   The Pareto distribution takes values on the positive real line.  All values must be
#'   larger than the \dQuote{location} parameter \eqn{\eta}, which is really a threshold
#'   parameter.  There are three kinds of Pareto distributions.  The one described here
#'   is the Pareto distribution of the first kind.  Stable Pareto distributions have
#'   \eqn{0 < \theta < 2}.  Note that the \eqn{r}'th moment only exists if
#'   \eqn{r < \theta}.
#'
#'   The Pareto distribution is related to the
#'   \link[stats:Exponential]{exponential distribution} and
#'   \link[stats:Logistic]{logistic distribution} as follows.
#'   Let \eqn{X} denote a Pareto random variable with \code{location=}\eqn{\eta} and
#'   \code{shape=}\eqn{\theta}.  Then \eqn{log(X/\eta)} has an exponential distribution
#'   with parameter \code{rate=}\eqn{\theta}, and \eqn{-log\{ [(X/\eta)^\theta] - 1 \}}
#'   has a logistic distribution with parameters \code{location=}\eqn{0} and
#'   \code{scale=}\eqn{1}.
#'
#'   The Pareto distribution has a very long right-hand tail.  It is often applied in
#'   the study of socioeconomic data, including the distribution of income, firm size,
#'   population, and stock price fluctuations.
#' }
#' @rawRd
#' \seealso{
#'   \code{\link{epareto}}, \code{\link{eqpareto}}, \link[stats]{Exponential},
#'   \link{Probability Distributions and Random Numbers}.
#' }
#' @rawRd
#' \examples{
#'   # Density of a Pareto distribution with parameters location=1 and shape=1,
#'   # evaluated at 2, 3 and 4:
#'
#'   dpareto(2:4, 1, 1)
#'   #[1] 0.2500000 0.1111111 0.0625000
#'
#'   #----------
#'
#'   # The cdf of a Pareto distribution with parameters location=2 and shape=1,
#'   # evaluated at 3, 4, and 5:
#'
#'   ppareto(3:5, 2, 1)
#'   #[1] 0.3333333 0.5000000 0.6000000
#'
#'   #----------
#'
#'   # The 25'th percentile of a Pareto distribution with parameters
#'   # location=1 and shape=1:
#'
#'   qpareto(0.25, 1, 1)
#'   #[1] 1.333333
#'
#'   #----------
#'
#'   # A random sample of 4 numbers from a Pareto distribution with parameters
#'   # location=3 and shape=2.
#'   # (Note: the call to set.seed simply allows you to reproduce this example.)
#'
#'   set.seed(10)
#'   rpareto(4, 3, 2)
#'   #[1] 4.274728 3.603148 3.962862 5.415322
#' }
#' @rawRd
#' \keyword{ distribution }
#' @rawRd
#' \keyword{ datagen }
NULL

#' Real dataset from ProUCL 5.2.0.
#' @name ProUCL.5.2.TRS.df
#' @description
#' A real data set of size n=55 with 18.8\% Nondetects (=10).
#'   The name of the Excel file that comes with ProUCL 5.2.0 and
#'   contains these data is \bold{TRS-Real-data-with-NDs.xls}.
#' @usage
#' ProUCL.5.2.TRS.df
#'     data(ProUCL.5.2.TRS.df)
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 55 observations on the following 3 variables.
#'   \describe{
#'     \item{\code{Value}}{numeric vector indicating the concentration.}
#'     \item{\code{Detect}}{numeric vector of 0s (nondetects) and 1s (detects)
#'      indicating censoring status.}
#'     \item{\code{Censored}}{logical vector indicating censoring status.}
#'   }
#' }
#' @rawRd
#' \source{
#'   USEPA. (2022a).  \emph{ProUCL Version 5.2.0 Technical Guide:
#'   Statistical Software for Environmental Applications for Data Sets with and
#'   without Nondetect Observations}.  Prepared by:  Neptune and Company, Inc.,
#'   1435 Garrison Street, Suite 201, Lakewood, CO 80215.  p. 143.
#'   \url{https://www.epa.gov/land-research/proucl-software}.
#'
#'   USEPA. (2022b).  \emph{ProUCL Version 5.2.0 User Guide:
#'   Statistical Software for Environmental Applications for Data Sets with and
#'   without Nondetect Observations}.  Prepared by:  Neptune and Company, Inc.,
#'   1435 Garrison Street, Suite 201, Lakewood, CO 80215.  p. 6-115.
#'   \url{https://www.epa.gov/land-research/proucl-software}.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' ProUCL Critical Values for Anderson-Darling Goodness-of-Fit Test for Gamma Distribution
#' @name ProUCL.Crit.Vals.for.AD.Test.for.Gamma.array
#' @description
#' Critical Values for the Anderson-Darling Goodness-of-Fit Test for a Gamma Distribution, as
#'   presented in Tables A-1, A-3, and A-5 on pages 283, 285, and 287, respectively, of
#'   USEPA (2015).
#' @usage
#' data("ProUCL.Crit.Vals.for.AD.Test.for.Gamma.array")
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   An array of dimensions 32 by 11 by 3, with
#'   the first dimension indicating the sample size (between 5 and 1000),
#'   the second dimension indicating the value of the maximum likelihood estimate
#'   of the shape parameter (between 0.025 and 50),
#'   and the third dimension indicating the assumed significance level (0.01, 0.05, and 0.10).
#' }
#' @rawRd
#' \details{
#'   See USEPA (2015, pp.281-282) and the help file for \code{\link{gofTest}}
#'   for more information.  The data in this array are used when
#'   the function \code{\link{gofTest}} is called with \code{test="proucl.ad.gamma"}.
#'   The letter k is used to indicate the value of the estimated shape parameter.
#' }
#' @rawRd
#' \source{
#'   USEPA. (2015).  \emph{ProUCL Version 5.1.002 Technical Guide}.  EPA/600/R-07/041, October 2015.
#'   Office of Research and Development. U.S. Environmental Protection Agency, Washington, D.C.,
#'   pp. 283, 285, and 287.
#' }
#' @rawRd
#' \references{
#'   USEPA. (2002).  \emph{Estimation of the Exposure Point Concentration Term Using a
#'   Gamma Distribution}.
#'   EPA/600/R-02/084. October 2002. Technology Support Center for Monitoring and
#'   Site Characterization, Office of Research and Development, Office of Solid Waste and
#'   Emergency Response, U.S. Environmental Protection Agency, Washington, D.C.
#'
#'   USEPA. (2015).  \emph{ProUCL Version 5.1.002 Technical Guide}.  EPA/600/R-07/041, October 2015.
#'   Office of Research and Development. U.S. Environmental Protection Agency, Washington, D.C.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' ProUCL Critical Values for Kolmogorov-Smirnov Goodness-of-Fit Test for Gamma Distribution
#' @name ProUCL.Crit.Vals.for.KS.Test.for.Gamma.array
#' @description
#' Critical Values for the Kolmogorov-Smirnov Goodness-of-Fit Test for a Gamma Distribution, as
#'   presented in Tables A-2, A-4, and A-6 on pages 284, 286, and 288, respectively, of
#'   USEPA (2015).
#' @usage
#' data("ProUCL.Crit.Vals.for.KS.Test.for.Gamma.array")
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   An array of dimensions 32 by 11 by 3, with
#'   the first dimension indicating the sample size (between 5 and 1000),
#'   the second dimension indicating the value of the maximum likelihood estimate
#'   of the shape parameter (between 0.025 and 50),
#'   and the third dimension indicating the assumed significance level (0.01, 0.05, and 0.10).
#' }
#' @rawRd
#' \details{
#'   See USEPA (2015, pp.281-282) for more information.  The data in this array are used when
#'   the function \code{\link{gofTest}} is called with \code{test="proucl.ks.gamma"}.
#' }
#' @rawRd
#' \source{
#'   USEPA. (2015).  \emph{ProUCL Version 5.1.002 Technical Guide}.  EPA/600/R-07/041, October 2015.
#'   Office of Research and Development. U.S. Environmental Protection Agency, Washington, D.C.,
#'   pp. 284, 286, and 288.
#' }
#' @rawRd
#' \references{
#'   USEPA. (2002).  \emph{Estimation of the Exposure Point Concentration Term Using a
#'   Gamma Distribution}.
#'   EPA/600/R-02/084. October 2002. Technology Support Center for Monitoring and
#'   Site Characterization, Office of Research and Development, Office of Solid Waste and
#'   Emergency Response, U.S. Environmental Protection Agency, Washington, D.C.
#'
#'   USEPA. (2015).  \emph{ProUCL Version 5.1.002 Technical Guide}.  EPA/600/R-07/041, October 2015.
#'   Office of Research and Development. U.S. Environmental Protection Agency, Washington, D.C.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Carbon Monoxide Emissions from Oil Refinery.
#' @name Refinery.CO.df
#' @description
#' Carbon monoxide (CO) emissions (ppm) from an oil refinery near San Francisco.
#'   The refinery submitted 31 daily measurements from its stack for the period
#'   April 16, 1993 through May 16, 1993 to the Bay Area Air Quality Management
#'   District (BAAQMD).  The BAAQMD made nine of its own indepent measurements for
#'   the period September 11, 1990 through March 30, 1993.
#' @usage
#' data(Refinery.CO.df)
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 40 observations on the following 3 variables.
#'   \describe{
#'     \item{\code{CO.ppm}}{a numeric vector of CO emissions (ppm)}
#'     \item{\code{Source}}{a factor indicating the source of the measurment (\code{BAAQMD} or \code{refinery}}
#'     \item{\code{Date}}{a Date object indicating the date the measurement was taken}
#'   }
#' }
#' @rawRd
#' \source{
#'   Data and Story Library, http://lib.stat.cmu.edu/DASL/Datafiles/Refinery.html.
#' }
#' @rawRd
#' \references{
#'   Zou, G.Y., C.Y. Huo, and J. Taleban. (2009).  Simple Confidence Intervals for
#'   Lognormal Means and their Differences with Environmental Applications.
#'   \emph{Environmetrics}, \bold{20}, 172--180.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Ammonia Nitrogen Concentrations in the Skagit River, Marblemount, Washington
#' @name Skagit.NH3_N.df
#' @description
#' Ammonia nitrogen (NH\eqn{_3}---N) concentration (mg/L) in the Skagit River
#'   measured monthly from January 1978 through December 2010 at the
#'   Marblemount, Washington monitoring station.
#' @usage
#' Skagit.NH3_N.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 396 observations on the following 6 variables.
#'   \describe{
#'     \item{\code{Date}}{Date of collection.}
#'     \item{\code{NH3_N.Orig.mg.per.L}}{a character vector of the ammonia
#'       nitrogen concentrations where values for non-detects are preceeded
#'       with the less-than sign (<).}
#'     \item{\code{NH3_N.mg.per.L}}{a numeric vector of ammonia nitrogen
#'       concentrations; non-detects have been coded to their detection limit.}
#'     \item{\code{DQ1}}{factor of data qualifier values.
#'       \itemize{
#'         \item \code{U} = The analyte was not detected at or above the reported result.
#'         \item \code{J} = The analyte was positively identified. The associated numerical result is an estimate.
#'         \item \code{UJ} = The analyte was not detected at or above the reported estimated result.
#'       }
#'     }
#'     \item{\code{DQ2}}{factor of data qualifier values.
#'       An asterisk (\code{*}) indicates a possible quality problem for the result.}
#'     \item{\code{Censored}}{a logical vector indicating which observations are censored.}
#'   }
#' }
#' @rawRd
#' \details{
#'   Station 04A100 - Skagit R \@ Marblemount.
#'   Located at the bridge on the Casdace River Road where
#'   Highway 20 (North Cascades Highway) turns 90 degrees in Marblemount.
#' }
#' @rawRd
#' \source{
#'   Washington State Deparment of Ecology. \cr
#'   \url{https://ecology.wa.gov/research-data/monitoring-assessment/water-quality-monitoring}
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' Total Phosphorus Data from Chesapeake Bay
#' @name Total.P.df
#' @description
#' Monthly estimated total phosphorus mass (mg) within a water column at two different
#'   stations for the 5-year time period October 1984 to September 1989 from a study on
#'   phosphorus concentration conducted in the Chesapeake Bay.
#' @usage
#' Total.P.df
#' @rawRd
#' \docType{data}
#' @rawRd
#' \format{
#'   A data frame with 60 observations on the following 4 variables.
#'   \describe{
#'     \item{\code{CB3.1}}{a numeric vector of phosphorus concentrations at station CB3.1}
#'     \item{\code{CB3.3e}}{a numeric vector phosphorus concentrations at station CB3.3e}
#'     \item{\code{Month}}{a factor indicating the month the observation was taken}
#'     \item{\code{Year}}{a numeric vector indicating the year an observation was taken}
#'   }
#' }
#' @rawRd
#' \source{
#'   Neerchal, N. K., and S. L. Brunenmeister. (1993). Estimation of Trend in Chesapeake Bay Water Quality Data.
#'   In Patil, G.P., and C.R. Rao, eds., \emph{Handbook of Statistics, Vol. 6: Multivariate Environmental Statistics}.
#'   North-Holland, Amsterdam, Chapter 19, 407-422.
#' }
#' @rawRd
#' \keyword{datasets}
NULL

#' The Triangular Distribution
#' @name Triangular
#' @aliases dtri ptri qtri rtri
#' @description
#' Density, distribution function, quantile function, and random generation
#'   for the triangular distribution with parameters \code{min}, \code{max},
#'   and \code{mode}.
#' @usage
#' dtri(x, min = 0, max = 1, mode = 1/2)
#'   ptri(q, min = 0, max = 1, mode = 1/2)
#'   qtri(p, min = 0, max = 1, mode = 1/2)
#'   rtri(n, min = 0, max = 1, mode = 1/2)
#' @rawRd
#' \arguments{
#'   \item{x}{
#'   vector of quantiles.  Missing values (\code{NA}s) are allowed.
#' }
#'   \item{q}{
#'   vector of quantiles.  Missing values (\code{NA}s) are allowed.
#' }
#'   \item{p}{
#'   vector of probabilities between 0 and 1.  Missing values (\code{NA}s) are allowed.
#' }
#'   \item{n}{
#'   sample size.  If \code{length(n)} is larger than 1, then \code{length(n)}
#'   random values are returned.
#' }
#'   \item{min}{
#'   vector of minimum values of the distribution of the random variable.
#'   The default value is \code{min=0}.
#' }
#'   \item{max}{
#'   vector of maximum values of the random variable.
#'   The default value is \code{max=1}.
#' }
#'   \item{mode}{
#'   vector of modes of the random variable.
#'   The default value is \code{mode=1/2}.
#' }
#' }
#' @rawRd
#' \details{
#'   Let \eqn{X} be a triangular random variable with parameters \code{min=}\eqn{a},
#'   \code{max=}\eqn{b}, and \code{mode=}\eqn{c}.
#'
#'
#'   \emph{Probability Density and Cumulative Distribution Function} \cr
#'   The density function of \eqn{X} is given by:
#'   \tabular{lll}{
#'     \eqn{f(x; a, b, c) =}  \tab  \eqn{\frac{2(x-a)}{(b-a)(c-a)}}  \tab for \eqn{a \le x \le c} \cr
#'                            \tab  \eqn{\frac{2(b-x)}{(b-a)(b-c)}}  \tab for \eqn{c \le x \le b} \cr
#'   }
#'   where \eqn{a < c < b}.
#'
#'   The cumulative distribution function of \eqn{X} is given by:
#'   \tabular{lll}{
#'     \eqn{F(x; a, b, c) =}  \tab  \eqn{\frac{(x-a)^2}{(b-a)(c-a)}}  \tab for \eqn{a \le x \le c} \cr
#'                            \tab  \eqn{1 - \frac{(b-x)^2}{(b-a)(b-c)}}  \tab for \eqn{c \le x \le b} \cr
#'   }
#'   where \eqn{a < c < b}.
#'
#'   \emph{Quantiles} \cr
#'   The \eqn{p^th} quantile of \eqn{X} is given by:
#'   \tabular{lll}{
#'     \eqn{x_p =}  \tab  \eqn{a + \sqrt{(b-a)(c-a)p}}    \tab for \eqn{0 \le p \le F(c)} \cr
#'                  \tab  \eqn{b - \sqrt{(b-a)(b-c)(1-p}} \tab for \eqn{F(c) \le p \le 1} \cr
#'   }
#'   where \eqn{0 \le p \le 1}.
#'
#'   \emph{Random Numbers} \cr
#'   Random numbers are generated using the inverse transformation method:
#'   \deqn{x = F^{-1}(u)}
#'   where \eqn{u} is a random deviate from a uniform \eqn{[0, 1]} distribution.
#'
#'   \emph{Mean and Variance} \cr
#'   The mean and variance of \eqn{X} are given by:
#'   \deqn{E(X) = \frac{a + b + c}{3}}
#'   \deqn{Var(X) = \frac{a^2 + b^2 + c^2 - ab - ac - bc}{18}}
#' }
#' @rawRd
#' \value{
#'   \code{dtri} gives the density, \code{ptri} gives the distribution function,
#'   \code{qtri} gives the quantile function, and \code{rtri} generates random
#'   deviates.
#' }
#' @rawRd
#' \references{
#'   Forbes, C., M. Evans, N. Hastings, and B. Peacock. (2011).  Statistical Distributions.
#'   Fourth Edition. John Wiley and Sons, Hoboken, NJ.
#'
#'   Johnson, N. L., S. Kotz, and N. Balakrishnan. (1995).
#'   \emph{Continuous Univariate Distributions, Volume 2}.
#'   Second Edition. John Wiley and Sons, New York.
#' }
#' @rawRd
#' \author{
#'   Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \note{
#'   The triangular distribution is so named because of the shape of its probability
#'   density function.  The average of two independent identically distributed
#'   uniform random variables with parameters \code{min=}\eqn{\alpha} and
#'   \code{max=}\eqn{\beta} has a triangular distribution with parameters
#'   \code{min=}\eqn{\alpha}, \code{max=}\eqn{\beta}, and
#'   \code{mode=}\eqn{(\beta-\alpha)/2}.
#'
#'   The triangular distribution is sometimes used as an input distribution in
#'   probability risk assessment.
#' }
#' @rawRd
#' \seealso{
#'   \link[stats:Uniform]{Uniform},
#'   \link{Probability Distributions and Random Numbers}.
#' }
#' @rawRd
#' \examples{
#'   # Density of a triangular distribution with parameters
#'   # min=10, max=15, and mode=12, evaluated at 12, 13 and 14:
#'
#'   dtri(12:14, 10, 15, 12)
#'   #[1] 0.4000000 0.2666667 0.1333333
#'
#'   #----------
#'
#'   # The cdf of a triangular distribution with parameters
#'   # min=2, max=7, and mode=5, evaluated at 3, 4, and 5:
#'
#'   ptri(3:5, 2, 7, 5)
#'   #[1] 0.06666667 0.26666667 0.60000000
#'
#'   #----------
#'
#'   # The 25'th percentile of a triangular distribution with parameters
#'   # min=1, max=4, and mode=3:
#'
#'   qtri(0.25, 1, 4, 3)
#'   #[1] 2.224745
#'
#'   #----------
#'
#'   # A random sample of 4 numbers from a triangular distribution with
#'   # parameters min=3 , max=20, and mode=12.
#'   # (Note: the call to set.seed simply allows you to reproduce this example.)
#'
#'   set.seed(10)
#'   rtri(4, 3, 20, 12)
#'   #[1] 11.811593  9.850955 11.081885 13.539496
#' }
#' @rawRd
#' \keyword{ distribution }
#' @rawRd
#' \keyword{ datagen }
NULL

#' The Zero-Modified Lognormal (Delta) Distribution
#' @name ZeroModifiedLognormal
#' @aliases DeltaDist dzmlnorm pzmlnorm qzmlnorm rzmlnorm
#' @rawRd \alias{Zero-Modified Lognormal}
#' @rawRd \alias{Zero-Modified Lognormal (Delta)}
#' @rawRd \alias{Zero Modified Lognormal}
#' @rawRd \alias{Zero Modified Lognormal (Delta)}
#' @rawRd \alias{Delta Distribution}
#' @description
#' Density, distribution function, quantile function, and random generation
#'   for the zero-modified lognormal distribution with parameters \code{meanlog},
#'   \code{sdlog}, and \code{p.zero}.
#'
#'   The zero-modified lognormal (delta) distribution is the mixture of a
#'   lognormal distribution with a positive probability mass at 0.
#' @usage
#' dzmlnorm(x, meanlog = 0, sdlog = 1, p.zero = 0.5)
#'   pzmlnorm(q, meanlog = 0, sdlog = 1, p.zero = 0.5)
#'   qzmlnorm(p, meanlog = 0, sdlog = 1, p.zero = 0.5)
#'   rzmlnorm(n, meanlog = 0, sdlog = 1, p.zero = 0.5)
#' @rawRd
#' \arguments{
#'   \item{x}{
#'   vector of quantiles.
#' }
#'   \item{q}{
#'   vector of quantiles.
#' }
#'   \item{p}{
#'   vector of probabilities between 0 and 1.
#' }
#'   \item{n}{
#'   sample size.  If \code{length(n)} is larger than 1, then \code{length(n)}
#'   random values are returned.
#' }
#'   \item{meanlog}{
#'   vector of means of the normal (Gaussian) part of the distribution on the
#'   log scale.  The default is \code{meanlog=0}.
#' }
#'   \item{sdlog}{
#'   vector of (positive) standard deviations of the normal (Gaussian)
#'   part of the distribution on the log scale.  The default is \code{sdlog=1}.
#' }
#'   \item{p.zero}{
#'   vector of probabilities between 0 and 1 indicating the probability the random
#'   variable equals 0.  For \code{rzmlnorm} this must be a single, non-missing number.
#' }
#' }
#' @rawRd
#' \details{
#'   The zero-modified lognormal (delta) distribution is the mixture of a
#'   lognormal distribution with a positive probability mass at 0.  This distribution
#'   was introduced without a name by Aitchison (1955), and the name
#'   \eqn{\Delta}-distribution was coined by Aitchison and Brown (1957, p.95).
#'   It is a special case of a \dQuote{zero-modified} distribution
#'   (see Johnson et al., 1992, p. 312).
#'
#'   Let \eqn{f(x; \mu, \sigma)} denote the density of a
#'   \link[stats:Lognormal]{lognormal random variable} \eqn{X} with parameters
#'   \code{meanlog=}\eqn{\mu} and \code{sdlog=}\eqn{\sigma}.  The density function of a
#'   zero-modified lognormal (delta) random variable \eqn{Y} with parameters
#'   \code{meanlog=}\eqn{\mu}, \code{sdlog=}\eqn{\sigma}, and \code{p.zero=}\eqn{p},
#'   denoted \eqn{h(y; \mu, \sigma, p)}, is given by:
#'   \tabular{lll}{
#'     \eqn{h(y; \mu, \sigma, p) =}  \tab  \eqn{p}  \tab for \eqn{y = 0} \cr
#'                                   \tab  \eqn{(1 - p) f(y; \mu, \sigma)} \tab for \eqn{y > 0}
#'   }
#'   Note that \eqn{\mu} is \emph{not} the mean of the zero-modified lognormal
#'   distribution on the log scale; it is the mean of the lognormal part of the
#'   distribution on the log scale.  Similarly, \eqn{\sigma} is
#'   \emph{not} the standard deviation of the zero-modified lognormal distribution
#'   on the log scale; it is the standard deviation of the lognormal part of the
#'   distribution on the log scale.
#'
#'   Let \eqn{\gamma} and \eqn{\delta} denote the mean and standard deviation of the
#'   overall zero-modified lognormal distribution on the log scale.  Aitchison (1955)
#'   shows that:
#'   \deqn{E[log(Y)] = \gamma = (1 - p) \mu}
#'   \deqn{Var[log(Y)] = \delta^2 = (1 - p) \sigma^2 + p (1-p) \mu^2}
#'   Note that when \code{p.zero=}\eqn{p}\code{=0}, the zero-modified lognormal
#'   distribution simplifies to the lognormal distribution.
#' }
#' @rawRd
#' \value{
#'   \code{dzmlnorm} gives the density, \code{pzmlnorm} gives the distribution function,
#'   \code{qzmlnorm} gives the quantile function, and \code{rzmlnorm} generates random
#'   deviates.
#' }
#' @rawRd
#' \references{
#'   Aitchison, J. (1955).  On the Distribution of a Positive Random Variable Having
#'   a Discrete Probability Mass at the Origin.  \emph{Journal of the American
#'   Statistical Association} \bold{50}, 901-908.
#'
#'   Aitchison, J., and J.A.C. Brown (1957).  \emph{The Lognormal Distribution
#'   (with special reference to its uses in economics)}.  Cambridge University Press,
#'   London. pp.94-99.
#'
#'   Crow, E.L., and K. Shimizu. (1988).  \emph{Lognormal Distributions:
#'   Theory and Applications}.  Marcel Dekker, New York, pp.47-51.
#'
#'   Gibbons, RD., D.K. Bhaumik, and S. Aryal. (2009).  \emph{Statistical Methods
#'   for Groundwater Monitoring}.  Second Edition.  John Wiley and Sons, Hoboken, NJ.
#'
#'   Gilliom, R.J., and D.R. Helsel. (1986).  Estimation of Distributional Parameters
#'   for Censored Trace Level Water Quality Data: 1. Estimation Techniques.
#'   \emph{Water Resources Research} \bold{22}, 135-146.
#'
#'   Helsel, D.R. (2012).  \emph{Statistics for Censored Environmental Data Using
#'   Minitab and R}.  Second Edition.  John Wiley and Sons, Hoboken, NJ, Chapter 1.
#'
#'   Johnson, N. L., S. Kotz, and A.W. Kemp. (1992).  \emph{Univariate Discrete Distributions}.
#'   Second Edition. John Wiley and Sons, New York, p.312.
#'
#'   Owen, W., and T. DeRouen. (1980).  Estimation of the Mean for Lognormal Data
#'   Containing Zeros and Left-Censored Values, with Applications to the Measurement
#'   of Worker Exposure to Air Contaminants.  \emph{Biometrics} \bold{36}, 707-719.
#'
#'   USEPA (1992c).  \emph{Statistical Analysis of Ground-Water Monitoring Data at
#'   RCRA Facilities: Addendum to Interim Final Guidance}.  Office of Solid Waste,
#'   Permits and State Programs Division, US Environmental Protection Agency,
#'   Washington, D.C.
#'
#'   USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
#'   EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.
#'   U.S. Environmental Protection Agency, Washington, D.C.
#' }
#' @rawRd
#' \author{
#'   Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \note{
#'   The zero-modified lognormal (delta) distribution is sometimes used to
#'   model chemical concentrations for which some observations are reported as
#'   \dQuote{Below Detection Limit} (the nondetects are assumed equal to 0).
#'   See, for example, Gilliom and Helsel (1986), Owen and DeRouen (1980), and
#'   Gibbons et al. (2009, Chapter 12).  USEPA (2009, Chapter 15) recommends this
#'   strategy only in specific situations, and Helsel (2012, Chapter 1) strongly
#'   discourages this approach to dealing with non-detects.
#'
#'   A variation of the zero-modified lognormal (delta) distribution is the
#'   \link[=ZeroModifiedNormal]{zero-modified normal distribution}, in which a
#'   normal distribution is mixed with a positive probability mass at 0.
#'
#'   One way to try to assess whether a zero-modified lognormal (delta),
#'   zero-modified normal, censored normal, or censored lognormal is the best
#'   model for the data is to construct both censored and detects-only probability
#'   plots (see \code{\link{qqPlotCensored}}).
#' }
#' @rawRd
#' \seealso{
#'   \link[=ZeroModifiedLognormalAlt]{Zero-Modified Lognormal (Alternative Parameterization)},
#'   \link[stats:Lognormal]{Lognormal}, \link{LognormalAlt},
#'   \link[=ZeroModifiedNormal]{Zero-Modified Normal},
#'   \code{\link{ezmlnorm}}, \link{Probability Distributions and Random Numbers}.
#' }
#' @rawRd
#' \examples{
#'   # Density of the zero-modified lognormal (delta) distribution with
#'   # parameters meanlog=0, sdlog=1, and p.zero=0.5, evaluated at
#'   # 0, 0.5, 1, 1.5, and 2:
#'
#'   dzmlnorm(seq(0, 2, by = 0.5))
#'   #[1] 0.50000000 0.31374804 0.19947114 0.12248683
#'   #[5] 0.07843701
#'
#'   #----------
#'
#'   # The cdf of the zero-modified lognormal (delta) distribution with
#'   # parameters meanlog=1, sdlog=2, and p.zero=0.1, evaluated at 4:
#'
#'   pzmlnorm(4, 1, 2, .1)
#'   #[1] 0.6189203
#'
#'   #----------
#'
#'   # The median of the zero-modified lognormal (delta) distribution with
#'   # parameters meanlog=2, sdlog=3, and p.zero=0.1:
#'
#'   qzmlnorm(0.5, 2, 3, 0.1)
#'   #[1] 4.859177
#'
#'   #----------
#'
#'   # Random sample of 3 observations from the zero-modified lognormal
#'   # (delta) distribution with parameters meanlog=1, sdlog=2, and p.zero=0.4.
#'   # (Note: The call to set.seed simply allows you to reproduce this example.)
#'
#'   set.seed(20)
#'   rzmlnorm(3, 1, 2, 0.4)
#'   #[1] 0.000000 0.000000 3.146641
#' }
#' @rawRd
#' \keyword{ distribution }
#' @rawRd
#' \keyword{ datagen }
NULL

#' The Zero-Modified Lognormal (Delta) Distribution (Alternative Parameterization)
#' @name ZeroModifiedLognormalAlt
#' @aliases DeltaDistAlt dzmlnormAlt pzmlnormAlt qzmlnormAlt rzmlnormAlt
#' @rawRd \alias{Delta Distribution (Alternative Parameterization)}
#' @rawRd \alias{Zero-Modified Lognormal (Alternative Parameterization)}
#' @rawRd \alias{Zero Modified Lognormal (Alternative Parameterization)}
#' @description
#' Density, distribution function, quantile function, and random generation
#'   for the zero-modified lognormal distribution with parameters \code{mean},
#'   \code{cv}, and \code{p.zero}.
#'
#'   The zero-modified lognormal (delta) distribution is the mixture of a
#'   lognormal distribution with a positive probability mass at 0.
#' @usage
#' dzmlnormAlt(x, mean = exp(1/2), cv = sqrt(exp(1) - 1), p.zero = 0.5)
#'   pzmlnormAlt(q, mean = exp(1/2), cv = sqrt(exp(1) - 1), p.zero = 0.5)
#'   qzmlnormAlt(p, mean = exp(1/2), cv = sqrt(exp(1) - 1), p.zero = 0.5)
#'   rzmlnormAlt(n, mean = exp(1/2), cv = sqrt(exp(1) - 1), p.zero = 0.5)
#' @rawRd
#' \arguments{
#'   \item{x}{
#'   vector of quantiles.
#' }
#'   \item{q}{
#'   vector of quantiles.
#' }
#'   \item{p}{
#'   vector of probabilities between 0 and 1.
#' }
#'   \item{n}{
#'   sample size.  If \code{length(n)} is larger than 1, then \code{length(n)}
#'   random values are returned.
#' }
#'   \item{mean}{
#'   vector of means of the lognormal part of the distribution on the.
#'   The default is \code{mean=exp(1/2)}.
#' }
#'   \item{cv}{
#'   vector of (positive) coefficients of variation of the lognormal
#'   part of the distribution.  The default is \code{cv=sqrt(exp(1) - 1)}.
#' }
#'   \item{p.zero}{
#'   vector of probabilities between 0 and 1 indicating the probability the random
#'   variable equals 0.  For \code{rzmlnormAlt} this must be a single, non-missing number.
#' }
#' }
#' @rawRd
#' \details{
#'   The zero-modified lognormal (delta) distribution is the mixture of a
#'   lognormal distribution with a positive probability mass at 0.  This distribution
#'   was introduced without a name by Aitchison (1955), and the name
#'   \eqn{\Delta}-distribution was coined by Aitchison and Brown (1957, p.95).
#'   It is a special case of a \dQuote{zero-modified} distribution
#'   (see Johnson et al., 1992, p. 312).
#'
#'   Let \eqn{f(x; \theta, \tau)} denote the density of a
#'   \link[=LognormalAlt]{lognormal random variable} \eqn{X} with parameters
#'   \code{mean=}\eqn{\theta} and \code{cv=}\eqn{\tau}.  The density function of a
#'   zero-modified lognormal (delta) random variable \eqn{Y} with parameters
#'   \code{mean=}\eqn{\theta}, \code{cv=}\eqn{\tau}, and \code{p.zero=}\eqn{p},
#'   denoted \eqn{h(y; \theta, \tau, p)}, is given by:
#'   \tabular{lll}{
#'     \eqn{h(y; \theta, \tau, p) =} \tab  \eqn{p}  \tab for \eqn{y = 0} \cr
#'                                   \tab  \eqn{(1 - p) f(y; \theta, \tau)} \tab for \eqn{y > 0}
#'   }
#'   Note that \eqn{\theta} is \emph{not} the mean of the zero-modified lognormal
#'   distribution; it is the mean of the lognormal part of the distribution.
#'   Similarly, \eqn{\tau} is \emph{not} the coefficient of variation of the
#'   zero-modified lognormal distribution; it is the coefficient of variation of the
#'   lognormal part of the distribution.
#'
#'   Let \eqn{\gamma}, \eqn{\delta}, and \eqn{\omega} denote the mean,
#'   standard deviation, and coefficient of variation of the overall zero-modified
#'   lognormal distribution.  Let \eqn{\eta} denote the standard deviation of the
#'   lognormal part of the distribution, so that \eqn{\eta = \theta \tau}.
#'   Aitchison (1955) shows that:
#'   \deqn{E(Y) = \gamma = (1 - p) \theta}
#'   \deqn{Var(Y) = \delta^2 = (1 - p) \eta^2 + p (1-p) \theta^2}
#'   so that
#'   \deqn{\omega = \sqrt{(\tau^2 + p) / (1 - p)}}
#'   Note that when \code{p.zero=}\eqn{p}\code{=0}, the zero-modified lognormal
#'   distribution simplifies to the lognormal distribution.
#' }
#' @rawRd
#' \value{
#'   \code{dzmlnormAlt} gives the density, \code{pzmlnormAlt} gives the distribution function,
#'   \code{qzmlnormAlt} gives the quantile function, and \code{rzmlnormAlt} generates random
#'   deviates.
#' }
#' @rawRd
#' \references{
#'   Aitchison, J. (1955).  On the Distribution of a Positive Random Variable Having
#'   a Discrete Probability Mass at the Origin.  \emph{Journal of the American
#'   Statistical Association} \bold{50}, 901-908.
#'
#'   Aitchison, J., and J.A.C. Brown (1957).  \emph{The Lognormal Distribution
#'   (with special reference to its uses in economics)}.  Cambridge University Press,
#'   London. pp.94-99.
#'
#'   Crow, E.L., and K. Shimizu. (1988).  \emph{Lognormal Distributions:
#'   Theory and Applications}.  Marcel Dekker, New York, pp.47-51.
#'
#'   Gibbons, RD., D.K. Bhaumik, and S. Aryal. (2009).  \emph{Statistical Methods
#'   for Groundwater Monitoring}.  Second Edition.  John Wiley and Sons, Hoboken, NJ.
#'
#'   Gilliom, R.J., and D.R. Helsel. (1986).  Estimation of Distributional Parameters
#'   for Censored Trace Level Water Quality Data: 1. Estimation Techniques.
#'   \emph{Water Resources Research} \bold{22}, 135-146.
#'
#'   Helsel, D.R. (2012).  \emph{Statistics for Censored Environmental Data Using
#'   Minitab and R}.  Second Edition.  John Wiley and Sons, Hoboken, NJ, Chapter 1.
#'
#'   Johnson, N. L., S. Kotz, and A.W. Kemp. (1992).  \emph{Univariate Discrete Distributions}.
#'   Second Edition. John Wiley and Sons, New York, p.312.
#'
#'   Owen, W., and T. DeRouen. (1980).  Estimation of the Mean for Lognormal Data
#'   Containing Zeros and Left-Censored Values, with Applications to the Measurement
#'   of Worker Exposure to Air Contaminants.  \emph{Biometrics} \bold{36}, 707-719.
#'
#'   USEPA (1992c).  \emph{Statistical Analysis of Ground-Water Monitoring Data at
#'   RCRA Facilities: Addendum to Interim Final Guidance}.  Office of Solid Waste,
#'   Permits and State Programs Division, US Environmental Protection Agency,
#'   Washington, D.C.
#'
#'   USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
#'   EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.
#'   U.S. Environmental Protection Agency, Washington, D.C.
#' }
#' @rawRd
#' \author{
#'   Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \note{
#'   The zero-modified lognormal (delta) distribution is sometimes used to
#'   model chemical concentrations for which some observations are reported as
#'   \dQuote{Below Detection Limit} (the nondetects are assumed equal to 0).
#'   See, for example, Gilliom and Helsel (1986), Owen and DeRouen (1980), and
#'   Gibbons et al. (2009, Chapter 12).  USEPA (2009, Chapter 15) recommends this
#'   strategy only in specific situations, and Helsel (2012, Chapter 1) strongly
#'   discourages this approach to dealing with non-detects.
#'
#'   A variation of the zero-modified lognormal (delta) distribution is the
#'   \link[=ZeroModifiedNormal]{zero-modified normal distribution}, in which a
#'   normal distribution is mixed with a positive probability mass at 0.
#'
#'   One way to try to assess whether a zero-modified lognormal (delta),
#'   zero-modified normal, censored normal, or censored lognormal is the best
#'   model for the data is to construct both censored and detects-only probability
#'   plots (see \code{\link{qqPlotCensored}}).
#' }
#' @rawRd
#' \seealso{
#'   \link[=ZeroModifiedLognormal]{Zero-Modified Lognormal}, \link{LognormalAlt},
#'   \code{\link{ezmlnormAlt}}, \link{Probability Distributions and Random Numbers}.
#' }
#' @rawRd
#' \examples{
#'   # Density of the zero-modified lognormal (delta) distribution with
#'   # parameters mean=10, cv=1, and p.zero=0.5, evaluated at
#'   # 9, 10, and 11:
#'
#'   dzmlnormAlt(9:11, mean = 10, cv = 1, p.zero = 0.5)
#'   #[1] 0.02552685 0.02197043 0.01891924
#'
#'   #----------
#'
#'   # The cdf of the zero-modified lognormal (delta) distribution with
#'   # parameters mean=10, cv=2, and p.zero=0.1, evaluated at 8:
#'
#'   pzmlnormAlt(8, 10, 2, .1)
#'   #[1] 0.709009
#'
#'   #----------
#'
#'   # The median of the zero-modified lognormal (delta) distribution with
#'   # parameters mean=10, cv=2, and p.zero=0.1:
#'
#'   qzmlnormAlt(0.5, 10, 2, 0.1)
#'   #[1] 3.74576
#'
#'   #----------
#'
#'   # Random sample of 3 observations from the zero-modified lognormal
#'   # (delta) distribution with parameters mean=10, cv=2, and p.zero=0.4.
#'   # (Note: The call to set.seed simply allows you to reproduce this example.)
#'
#'   set.seed(20)
#'   rzmlnormAlt(3, 10, 2, 0.4)
#'   #[1] 0.000000 0.000000 4.907131
#' }
#' @rawRd
#' \keyword{ distribution }
#' @rawRd
#' \keyword{ datagen }
NULL

#' The Zero-Modified Normal Distribution
#' @name ZeroModifiedNormal
#' @aliases dzmnorm pzmnorm qzmnorm rzmnorm
#' @rawRd \alias{Zero-Modified Normal}
#' @rawRd \alias{Zero Modified Normal}
#' @description
#' Density, distribution function, quantile function, and random generation
#'   for the zero-modified normal distribution with parameters \code{mean},
#'   \code{sd}, and \code{p.zero}.
#'
#'   The zero-modified normal distribution is the mixture of a normal distribution
#'   with a positive probability mass at 0.
#' @usage
#' dzmnorm(x, mean = 0, sd = 1, p.zero = 0.5)
#'   pzmnorm(q, mean = 0, sd = 1, p.zero = 0.5)
#'   qzmnorm(p, mean = 0, sd = 1, p.zero = 0.5)
#'   rzmnorm(n, mean = 0, sd = 1, p.zero = 0.5)
#' @rawRd
#' \arguments{
#'   \item{x}{
#'   vector of quantiles.
#' }
#'   \item{q}{
#'   vector of quantiles.
#' }
#'   \item{p}{
#'   vector of probabilities between 0 and 1.
#' }
#'   \item{n}{
#'   sample size.  If \code{length(n)} is larger than 1, then \code{length(n)}
#'   random values are returned.
#' }
#'   \item{mean}{
#'   vector of means of the normal (Gaussian) part of the distribution.
#'   The default is \code{mean=0}.
#' }
#'   \item{sd}{
#'   vector of (positive) standard deviations of the normal (Gaussian)
#'   part of the distribution.  The default is \code{sd=1}.
#' }
#'   \item{p.zero}{
#'   vector of probabilities between 0 and 1 indicating the probability the random
#'   variable equals 0.  For \code{rzmnorm} this must be a single, non-missing number.
#' }
#' }
#' @rawRd
#' \details{
#'   The zero-modified normal distribution is the mixture of a normal distribution
#'   with a positive probability mass at 0.
#'
#'   Let \eqn{f(x; \mu, \sigma)} denote the density of a
#'   \link[stats:Normal]{normal (Gaussian) random variable} \eqn{X} with parameters
#'   \code{mean=}\eqn{\mu} and \code{sd=}\eqn{\sigma}.  The density function of a
#'   zero-modified normal random variable \eqn{Y} with parameters \code{mean=}\eqn{\mu},
#'   \code{sd=}\eqn{\sigma}, and \code{p.zero=}\eqn{p}, denoted \eqn{h(y; \mu, \sigma, p)},
#'   is given by:
#'   \tabular{lll}{
#'     \eqn{h(y; \mu, \sigma, p) =}  \tab  \eqn{p}  \tab for \eqn{y = 0} \cr
#'                                   \tab  \eqn{(1 - p) f(y; \mu, \sigma)} \tab for \eqn{y \ne 0}
#'   }
#'   Note that \eqn{\mu} is \emph{not} the mean of the zero-modified normal distribution;
#'   it is the mean of the normal part of the distribution.  Similarly, \eqn{\sigma} is
#'   \emph{not} the standard deviation of the zero-modified normal distribution; it is
#'   the standard deviation of the normal part of the distribution.
#'
#'   Let \eqn{\gamma} and \eqn{\delta} denote the mean and standard deviation of the
#'   overall zero-modified normal distribution.  Aitchison (1955) shows that:
#'   \deqn{E(Y) = \gamma = (1 - p) \mu}
#'   \deqn{Var(Y) = \delta^2 = (1 - p) \sigma^2 + p (1-p) \mu^2}
#'   Note that when \code{p.zero=}\eqn{p}\code{=0}, the zero-modified normal
#'   distribution simplifies to the normal distribution.
#' }
#' @rawRd
#' \value{
#'   \code{dzmnorm} gives the density, \code{pzmnorm} gives the distribution function,
#'   \code{qzmnorm} gives the quantile function, and \code{rzmnorm} generates random
#'   deviates.
#' }
#' @rawRd
#' \references{
#'   Aitchison, J. (1955).  On the Distribution of a Positive Random Variable Having
#'   a Discrete Probability Mass at the Origin.  \emph{Journal of the American
#'   Statistical Association} \bold{50}, 901-908.
#'
#'   Gilliom, R.J., and D.R. Helsel. (1986).  Estimation of Distributional Parameters
#'   for Censored Trace Level Water Quality Data: 1. Estimation Techniques.
#'   \emph{Water Resources Research} \bold{22}, 135-146.
#'
#'   Gibbons, RD., D.K. Bhaumik, and S. Aryal. (2009).  \emph{Statistical Methods
#'   for Groundwater Monitoring}.  Second Edition.  John Wiley and Sons, Hoboken, NJ.
#'
#'   Helsel, D.R. (2012).  \emph{Statistics for Censored Environmental Data Using
#'   Minitab and R}.  Second Edition.  John Wiley and Sons, Hoboken, NJ, Chapter 1.
#'
#'   Johnson, N. L., S. Kotz, and A.W. Kemp. (1992).  \emph{Univariate Discrete Distributions}.
#'   Second Edition. John Wiley and Sons, New York, p.312.
#'
#'   Owen, W., and T. DeRouen. (1980).  Estimation of the Mean for Lognormal Data
#'   Containing Zeros and Left-Censored Values, with Applications to the Measurement
#'   of Worker Exposure to Air Contaminants.  \emph{Biometrics} \bold{36}, 707-719.
#'
#'   USEPA (1992c).  \emph{Statistical Analysis of Ground-Water Monitoring Data at
#'   RCRA Facilities: Addendum to Interim Final Guidance}.  Office of Solid Waste,
#'   Permits and State Programs Division, US Environmental Protection Agency,
#'   Washington, D.C.
#'
#'   USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
#'   EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.
#'   U.S. Environmental Protection Agency, Washington, D.C.
#' }
#' @rawRd
#' \author{
#'   Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \note{
#'   The zero-modified normal distribution is sometimes used to model chemical
#'   concentrations for which some observations are reported as
#'   \dQuote{Below Detection Limit}.  See, for example USEPA (1992c, pp.27-34) and
#'   Gibbons et al. (2009, Chapter 12).  Note, however, that USEPA (1992c) has been
#'   superseded by USEPA (2009) which recommends this strategy only in specific
#'   situations (see Chapter 15 of the document).  This strategy is strongly
#'   discouraged by Helsel (2012, Chapter 1).
#'
#'   In cases where you want to model chemical concentrations for which some
#'   observations are reported as \dQuote{Below Detection Limit} and you want to treat
#'   the non-detects as equal to 0, it will usually be more appropriate to model the
#'   data with a \link[=ZeroModifiedLognormal]{zero-modified lognormal (delta)
#'   distribution} since chemical concentrations are bounded below at
#'   0 (e.g., Gilliom and Helsel, 1986; Owen and DeRouen, 1980).
#'
#'   One way to try to assess whether a zero-modified lognormal (delta),
#'   zero-modified normal, censored normal, or censored lognormal is the best
#'   model for the data is to construct both censored and detects-only probability
#'   plots (see \code{\link{qqPlotCensored}}).
#' }
#' @rawRd
#' \seealso{
#'   \link[=ZeroModifiedLognormal]{Zero-Modified Lognormal}, \link[stats:Normal]{Normal},
#'   \code{\link{ezmnorm}}, \link{Probability Distributions and Random Numbers}.
#' }
#' @rawRd
#' \examples{
#'   # Density of the zero-modified normal distribution with parameters
#'   # mean=2, sd=1, and p.zero=0.5, evaluated at 0, 0.5, 1, 1.5, and 2:
#'
#'   dzmnorm(seq(0, 2, by = 0.5), mean = 2)
#'   #[1] 0.5000000 0.0647588 0.1209854 0.1760327 0.1994711
#'
#'   #----------
#'
#'   # The cdf of the zero-modified normal distribution with parameters
#'   # mean=3, sd=2, and p.zero=0.1, evaluated at 4:
#'
#'   pzmnorm(4, 3, 2, .1)
#'   #[1] 0.7223162
#'
#'   #----------
#'
#'   # The median of the zero-modified normal distribution with parameters
#'   # mean=3, sd=1, and p.zero=0.1:
#'
#'   qzmnorm(0.5, 3, 1, 0.1)
#'   #[1] 2.86029
#'
#'   #----------
#'
#'   # Random sample of 3 observations from the zero-modified normal distribution
#'   # with parameters mean=3, sd=1, and p.zero=0.4.
#'   # (Note: The call to set.seed simply allows you to reproduce this example.)
#'
#'   set.seed(20)
#'   rzmnorm(3, 3, 1, 0.4)
#'   #[1] 0.000000 0.000000 3.073168
#' }
#' @rawRd
#' \keyword{ distribution }
#' @rawRd
#' \keyword{ datagen }
NULL

#' S3 Class "boxcox"
#' @name boxcox.object
#' @description
#' Objects of S3 class \code{"boxcox"} are returned by the \pkg{EnvStats}
#'   function \code{\link{boxcox}}, which computes objective values for
#'   user-specified powers, or computes the optimal power for the specified
#'   objective.
#' @rawRd
#' \docType{class}
#' @rawRd
#' \details{
#'   Objects of class \code{"boxcox"} are lists that contain
#'   information about the powers that were used, the objective that was used,
#'   the values of the objective for the given powers, and whether an
#'   optimization was specified.
#' }
#' @rawRd
#' \section{Methods}{
#'   Generic functions that have methods for objects of class
#'   \code{"boxcox"} include: \cr
#'   \code{\link{plot}}, \code{\link{print}}.
#' }
#' @rawRd
#' \value{
#'   \strong{Required Components} \cr
#'   The following components must be included in a legitimate list of
#'   class \code{"boxcox"}.
#'
#'   \item{lambda}{
#'     Numeric vector containing the powers used in the Box-Cox transformations.
#'     If the value of the \code{optimize} component is \code{FALSE}, then
#'     \code{lambda} contains the values of all of the powers at which the objective
#'     was evaluated.  If the value of the \code{optimize} component is \code{TRUE},
#'     then \code{lambda} is a scalar containing the value of the power that
#'     maximizes the objective.}
#'   \item{objective}{
#'     Numeric vector containing the value(s) of the objective for the given value(s)
#'     of \eqn{\lambda} that are stored in the component \code{lambda}.}
#'   \item{objective.name}{
#'     character string indicating the objective that was used. The possible values are
#'     \code{"PPCC"} (probability plot correlation coefficient; the default),
#'     \code{"Shapiro-Wilk"} (the Shapiro-Wilk goodness-of-fit statistic), and
#'     \code{"Log-Likelihood"} (the log-likelihood function).}
#'   \item{optimize}{
#'     logical scalar indicating whether the objective was simply evaluted at the
#'     given values of \code{lambda} (\code{optimize=FALSE}), or instead
#'     the optimal power transformation was computed within the bounds specified by
#'     \code{lambda} (\code{optimize=TRUE}).}
#'   \item{optimize.bounds}{
#'     Numeric vector of length 2 with a names attribute indicating the bounds within
#'     which the optimization took place.  When \code{optimize=FALSE}, this contains
#'     missing values.}
#'   \item{eps}{
#'     finite, positive numeric scalar indicating what value of \code{eps} was used.
#'     When the absolute value of \code{lambda} is less
#'     than \code{eps}, lambda is assumed to be 0 for the Box-Cox transformation.}
#'   \item{sample.size}{
#'     Numeric scalar indicating the number of finite, non-missing observations.}
#'   \item{data.name}{
#'     The name of the data object used for the Box-Cox computations.}
#'   \item{bad.obs}{
#'     The number of missing (\code{NA}), undefined (\code{NaN}) and/or infinite
#'     (\code{Inf}, \code{-Inf}) values that were removed from the data object
#'     prior to performing the Box-Cox computations.} \cr
#'
#'   \strong{Optional Component} \cr
#'   The following component may optionally be included in a legitimate
#'   list of class \code{"boxcox"}.  It must be included if you want to call the
#'   function \code{\link{plot.boxcox}} and specify Q-Q plots or
#'   Tukey Mean-Difference Q-Q plots.
#'
#'   \item{data}{
#'     Numeric vector containing the data actually used for the Box-Cox
#'     computations (i.e., the original data without any missing or infinite values).}
#' }
#' @rawRd
#' \note{
#'   Since objects of class \code{"boxcox"} are lists, you may extract
#'   their components with the \code{$} and \code{[[} operators.
#' }
#' @rawRd
#' \author{
#'   Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \seealso{
#'   \code{\link{boxcox}}, \code{\link{plot.boxcox}}, \code{\link{print.boxcox}},
#'   \code{\link{boxcoxLm.object}}.
#' }
#' @rawRd
#' \examples{
#'   # Create an object of class "boxcox", then print it out.
#'   # (Note: the call to set.seed simply allows you to reproduce this example.)
#'
#'   set.seed(250)
#'   x <- rlnormAlt(30, mean = 10, cv = 2)
#'
#'   dev.new()
#'   hist(x, col = "cyan")
#'
#'   boxcox.list <- boxcox(x)
#'
#'   data.class(boxcox.list)
#'   #[1] "boxcox"
#'
#'   names(boxcox.list)
#'   # [1] "lambda"          "objective"       "objective.name"
#'   # [4] "optimize"        "optimize.bounds" "eps"
#'   # [7] "data"            "sample.size"     "data.name"
#'   #[10] "bad.obs"
#'
#'   boxcox.list
#'   #Results of Box-Cox Transformation
#'   #---------------------------------
#'   #
#'   #Objective Name:                  PPCC
#'   #
#'   #Data:                            x
#'   #
#'   #Sample Size:                     30
#'   #
#'   # lambda      PPCC
#'   #   -2.0 0.5423739
#'   #   -1.5 0.6402782
#'   #   -1.0 0.7818160
#'   #   -0.5 0.9272219
#'   #    0.0 0.9921702
#'   #    0.5 0.9581178
#'   #    1.0 0.8749611
#'   #    1.5 0.7827009
#'   #    2.0 0.7004547
#'
#'   boxcox(x, optimize = TRUE)
#'   #Results of Box-Cox Transformation
#'   #---------------------------------
#'   #
#'   #Objective Name:                  PPCC
#'   #
#'   #Data:                            x
#'   #
#'   #Sample Size:                     30
#'   #
#'   #Bounds for Optimization:         lower = -2
#'   #                                 upper =  2
#'   #
#'   #Optimal Value:                   lambda = 0.04530789
#'   #
#'   #Value of Objective:              PPCC = 0.9925919
#'
#'   #----------
#'
#'   # Clean up
#'   #---------
#'   rm(x, boxcox.list)
#' }
#' @rawRd
#' \keyword{classes}
NULL

#' S3 Class "boxcoxCensored"
#' @name boxcoxCensored.object
#' @description
#' Objects of S3 class \code{"boxcoxCensored"} are returned by the \pkg{EnvStats}
#'   function \code{\link{boxcoxCensored}}, which computes objective values for
#'   user-specified powers, or computes the optimal power for the specified
#'   objective, based on Type I censored data.
#' @rawRd
#' \docType{class}
#' @rawRd
#' \details{
#'   Objects of class \code{"boxcoxCensored"} are lists that contain
#'   information about the powers that were used, the objective that was used,
#'   the values of the objective for the given powers, and whether an
#'   optimization was specified.
#' }
#' @rawRd
#' \section{Methods}{
#'   Generic functions that have methods for objects of class
#'   \code{"boxcoxCensored"} include: \cr
#'   \code{\link{plot}}, \code{\link{print}}.
#' }
#' @rawRd
#' \value{
#'   \strong{Required Components} \cr
#'   The following components must be included in a legitimate list of
#'   class \code{"boxcoxCensored"}.
#'
#'   \item{lambda}{
#'     Numeric vector containing the powers used in the Box-Cox transformations.
#'     If the value of the \code{optimize} component is \code{FALSE}, then
#'     \code{lambda} contains the values of all of the powers at which the objective
#'     was evaluated.  If the value of the \code{optimize} component is \code{TRUE},
#'     then \code{lambda} is a scalar containing the value of the power that
#'     maximizes the objective.}
#'   \item{objective}{
#'     Numeric vector containing the value(s) of the objective for the given value(s)
#'     of \eqn{\lambda} that are stored in the component \code{lambda}.}
#'   \item{objective.name}{
#'     Character string indicating the objective that was used. The possible values are
#'     \code{"PPCC"} (probability plot correlation coefficient; the default),
#'     \code{"Shapiro-Wilk"} (the Shapiro-Wilk goodness-of-fit statistic), and
#'     \code{"Log-Likelihood"} (the log-likelihood function).}
#'   \item{optimize}{
#'     Logical scalar indicating whether the objective was simply evaluted at the
#'     given values of \code{lambda} (\code{optimize=FALSE}), or instead
#'     the optimal power transformation was computed within the bounds specified by
#'     \code{lambda} (\code{optimize=TRUE}).}
#'   \item{optimize.bounds}{
#'     Numeric vector of length 2 with a names attribute indicating the bounds within
#'     which the optimization took place.  When \code{optimize=FALSE}, this contains
#'     missing values.}
#'   \item{eps}{
#'     Finite, positive numeric scalar indicating what value of \code{eps} was used.
#'     When the absolute value of \code{lambda} is less
#'     than \code{eps}, lambda is assumed to be 0 for the Box-Cox transformation.}
#'   \item{sample.size}{
#'     Numeric scalar indicating the number of finite, non-missing observations.}
#'   \item{censoring.side}{
#'     Character string indicating the censoring side.  Possible values are
#'     \code{"left"} and \code{"right"}.}
#'   \item{censoring.levels}{
#'     Numeric vector containing the censoring levels.}
#'   \item{percent.censored}{Numeric scalar indicating the percent of observations
#'     that are censored.}
#'   \item{data.name}{
#'     The name of the data object used for the Box-Cox computations.}
#'   \item{censoring.name}{
#'     The name of the data object indicating which observations are censored.}
#'   \item{bad.obs}{
#'     The number of missing (\code{NA}), undefined (\code{NaN}) and/or infinite
#'     (\code{Inf}, \code{-Inf}) values that were removed from the data object
#'     prior to performing the Box-Cox computations.} \cr
#'
#'   \strong{Optional Component} \cr
#'   The following components may optionally be included in a legitimate
#'   list of class \code{"boxcoxCensored"}.  They must be included if you want to
#'   call the function \code{\link{plot.boxcoxCensored}} and specify Q-Q plots or
#'   Tukey Mean-Difference Q-Q plots.
#'
#'   \item{data}{
#'     Numeric vector containing the data actually used for the Box-Cox
#'     computations (i.e., the original data without any missing or infinite values).}
#'   \item{censored}{
#'     Logical vector indicating which of the vales in the component \code{data}
#'     are censored.}
#' }
#' @rawRd
#' \note{
#'   Since objects of class \code{"boxcoxCensored"} are lists, you may extract
#'   their components with the \code{$} and \code{[[} operators.
#' }
#' @rawRd
#' \author{
#'   Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \seealso{
#'   \code{\link{boxcoxCensored}}, \code{\link{plot.boxcoxCensored}},
#'   \code{\link{print.boxcoxCensored}}.
#' }
#' @rawRd
#' \examples{
#'   # Create an object of class "boxcoxCensored", then print it out.
#'   # (Note: the call to set.seed simply allows you to reproduce this example.)
#'
#'   set.seed(250)
#'
#'   x.1 <- rlnormAlt(15, mean = 10, cv = 2)
#'   censored.1 <- x.1 < 2
#'   x.1[censored.1] <- 2
#'
#'   x.2 <- rlnormAlt(15, mean = 10, cv = 2)
#'   censored.2 <- x.2 < 4
#'   x.2[censored.2] <- 4
#'
#'   x <- c(x.1, x.2)
#'   censored <- c(censored.1, censored.2)
#'
#'   boxcox.list <- boxcoxCensored(x, censored)
#'
#'   data.class(boxcox.list)
#'   #[1] "boxcoxCensored"
#'
#'   names(boxcox.list)
#'   # [1] "lambda"           "objective"        "objective.name"
#'   # [4] "optimize"         "optimize.bounds"  "eps"
#'   # [7] "data"             "censored"         "sample.size"
#'   #[10] "censoring.side"   "censoring.levels" "percent.censored"
#'   #[13] "data.name"        "censoring.name"   "bad.obs"
#'
#'   boxcox.list
#'
#'   #Results of Box-Cox Transformation
#'   #Based on Type I Censored Data
#'   #---------------------------------
#'   #
#'   #Objective Name:                  PPCC
#'   #
#'   #Data:                            x
#'   #
#'   #Censoring Variable:              censored
#'   #
#'   #Censoring Side:                  left
#'   #
#'   #Censoring Level(s):              2 4
#'   #
#'   #Sample Size:                     30
#'   #
#'   #Percent Censored:                26.7%
#'   #
#'   # lambda      PPCC
#'   #   -2.0 0.8954683
#'   #   -1.5 0.9338467
#'   #   -1.0 0.9643680
#'   #   -0.5 0.9812969
#'   #    0.0 0.9776834
#'   #    0.5 0.9471025
#'   #    1.0 0.8901990
#'   #    1.5 0.8187488
#'   #    2.0 0.7480494
#'
#'   boxcox.list2 <- boxcox(x, optimize = TRUE)
#'   names(boxcox.list2)
#'   # [1] "lambda"          "objective"       "objective.name"
#'   # [4] "optimize"        "optimize.bounds" "eps"
#'   # [7] "data"            "sample.size"     "data.name"
#'   #[10] "bad.obs"
#'
#'   boxcox.list2
#'   #Results of Box-Cox Transformation
#'   #---------------------------------
#'   #
#'   #Objective Name:                  PPCC
#'   #
#'   #Data:                            x
#'   #
#'   #Sample Size:                     30
#'   #
#'   #Bounds for Optimization:         lower = -2
#'   #                                 upper =  2
#'   #
#'   #Optimal Value:                   lambda = -0.5826431
#'   #
#'   #Value of Objective:              PPCC = 0.9755402
#'
#'   #==========
#'
#'   # Clean up
#'   #---------
#'   rm(x.1, censored.1, x.2, censored.2, x, censored, boxcox.list, boxcox.list2)
#' }
#' @rawRd
#' \keyword{classes}
NULL

#' S3 Class "boxcoxLm"
#' @name boxcoxLm.object
#' @description
#' Objects of S3 class \code{"boxcoxLm"} are returned by the \pkg{EnvStats}
#'   function \code{\link{boxcox}} when the argument \code{x} is an object
#'   of class \code{"lm"}.  In this case, \code{\link{boxcox}} computes
#'   values of an objective function for user-specified powers, or computes the
#'   optimal power for the specified objective, based on residuals from the linear model.
#' @rawRd
#' \docType{class}
#' @rawRd
#' \details{
#'   Objects of class \code{"boxcoxLm"} are lists that contain
#'   information about the \code{"lm"} object that was suplied,
#'   the powers that were used, the objective that was used,
#'   the values of the objective for the given powers, and whether an
#'   optimization was specified.
#' }
#' @rawRd
#' \section{Methods}{
#'   Generic functions that have methods for objects of class
#'   \code{"boxcoxLm"} include: \cr
#'   \code{\link{plot}}, \code{\link{print}}.
#' }
#' @rawRd
#' \value{
#'   The following components must be included in a legitimate list of
#'   class \code{"boxcoxLm"}.
#'
#'   \item{lambda}{
#'     Numeric vector containing the powers used in the Box-Cox transformations.
#'     If the value of the \code{optimize} component is \code{FALSE}, then
#'     \code{lambda} contains the values of all of the powers at which the objective
#'     was evaluated.  If the value of the \code{optimize} component is \code{TRUE},
#'     then \code{lambda} is a scalar containing the value of the power that
#'     maximizes the objective.}
#'   \item{objective}{
#'     Numeric vector containing the value(s) of the objective for the given value(s)
#'     of \eqn{\lambda} that are stored in the component \code{lambda}.}
#'   \item{objective.name}{
#'     character string indicating the objective that was used. The possible values are
#'     \code{"PPCC"} (probability plot correlation coefficient; the default),
#'     \code{"Shapiro-Wilk"} (the Shapiro-Wilk goodness-of-fit statistic), and
#'     \code{"Log-Likelihood"} (the log-likelihood function).}
#'   \item{optimize}{
#'     logical scalar indicating whether the objective was simply evaluted at the
#'     given values of \code{lambda} (\code{optimize=FALSE}), or instead
#'     the optimal power transformation was computed within the bounds specified by
#'     \code{lambda} (\code{optimize=TRUE}).}
#'   \item{optimize.bounds}{
#'     Numeric vector of length 2 with a names attribute indicating the bounds within
#'     which the optimization took place.  When \code{optimize=FALSE}, this contains
#'     missing values.}
#'   \item{eps}{
#'     finite, positive numeric scalar indicating what value of \code{eps} was used.
#'     When the absolute value of \code{lambda} is less
#'     than \code{eps}, lambda is assumed to be 0 for the Box-Cox transformation.}
#'   \item{lm.obj}{
#'     the value of the argument \code{x} provided to \code{\link{boxcox}}
#'     (an object that must inherit from class \code{"lm"}).}
#'   \item{sample.size}{
#'     Numeric scalar indicating the number of finite, non-missing observations.}
#'   \item{data.name}{
#'     The name of the data object used for the Box-Cox computations.}
#' }
#' @rawRd
#' \note{
#'   Since objects of class \code{"boxcoxLm"} are lists, you may extract
#'   their components with the \code{$} and \code{[[} operators.
#' }
#' @rawRd
#' \author{
#'   Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \seealso{
#'   \code{\link{boxcox}}, \code{\link{plot.boxcoxLm}}, \code{\link{print.boxcoxLm}},
#'   \code{\link{boxcox.object}}.
#' }
#' @rawRd
#' \examples{
#'   # Create an object of class "boxcoxLm", then print it out.
#'
#'   # The data frame Environmental.df contains daily measurements of
#'   # ozone concentration, wind speed, temperature, and solar radiation
#'   # in New York City for 153 consecutive days between May 1 and
#'   # September 30, 1973.  In this example, we'll plot ozone vs.
#'   # temperature and look at the Q-Q plot of the residuals.  Then
#'   # we'll look at possible Box-Cox transformations.  The "optimal" one
#'   # based on the PPCC looks close to a log-transformation
#'   # (i.e., lambda=0).  The power that produces the largest PPCC is
#'   # about 0.2, so a cube root (lambda=1/3) transformation might work too.
#'
#'   # Fit the model with the raw Ozone data
#'   #--------------------------------------
#'   ozone.fit <- lm(ozone ~ temperature, data = Environmental.df)
#'
#'   # Plot Ozone vs. Temperature, with fitted line
#'   #---------------------------------------------
#'   dev.new()
#'   with(Environmental.df,
#'     plot(temperature, ozone, xlab = "Temperature (degrees F)",
#'       ylab = "Ozone (ppb)", main = "Ozone vs. Temperature"))
#'   abline(ozone.fit)
#'
#'   # Look at the Q-Q Plot for the residuals
#'   #---------------------------------------
#'   dev.new()
#'   qqPlot(ozone.fit$residuals, add.line = TRUE)
#'
#'   # Look at Box-Cox transformations of Ozone
#'   #-----------------------------------------
#'   boxcox.list <- boxcox(ozone.fit)
#'   boxcox.list
#'   #Results of Box-Cox Transformation
#'   #---------------------------------
#'   #
#'   #Objective Name:                  PPCC
#'   #
#'   #Linear Model:                    ozone.fit
#'   #
#'   #Sample Size:                     116
#'   #
#'   # lambda      PPCC
#'   #   -2.0 0.4286781
#'   #   -1.5 0.4673544
#'   #   -1.0 0.5896132
#'   #   -0.5 0.8301458
#'   #    0.0 0.9871519
#'   #    0.5 0.9819825
#'   #    1.0 0.9408694
#'   #    1.5 0.8840770
#'   #    2.0 0.8213675
#'
#'   #----------
#'
#'   # Clean up
#'   #---------
#'   rm(ozone.fit, boxcox.list)
#' }
#' @rawRd
#' \keyword{classes}
NULL

#' S3 Class "calibrate"
#' @name calibrate.object
#' @description
#' Objects of S3 class \code{"calibrate"} are returned by the \pkg{EnvStats}
#'   function \code{\link{calibrate}}, which fits a calibration line or curve based
#'   on linear regression.
#' @rawRd
#' \docType{class}
#' @rawRd
#' \details{
#'   Objects of class \code{"calibrate"} are lists that inherit from
#'   \code{\link{class}} \code{"\link{lm}"} and include a component called
#'   \code{x} that stores the model matrix (the values of the predictor variables
#'   for the final calibration model).
#' }
#' @rawRd
#' \section{Methods}{
#'   Generic functions that have methods for objects of class
#'   \code{"calibrate"} include: \cr
#'   NONE AT PRESENT.
#' }
#' @rawRd
#' \value{
#'   See the help file for \code{\link{lm}}.
#'
#'   \strong{Required Components} \cr
#'   Besides the usual components in the list returned by the function \code{\link{lm}},
#'   the following components must be included in a legitimate list of
#'   class \code{"calibrate"}.
#'
#'   \item{x}{
#'     the model matrix from the linear model fit.}
#' }
#' @rawRd
#' \note{
#'   Since objects of class \code{"calibrate"} are lists, you may extract
#'   their components with the \code{$} and \code{[[} operators.
#' }
#' @rawRd
#' \author{
#'   Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \seealso{
#'   \code{\link{calibrate}}, \code{\link{inversePredictCalibrate}},
#'   \code{\link{detectionLimitCalibrate}}.
#' }
#' @rawRd
#' \examples{
#'   # Create an object of class "calibrate", then print it out.
#'
#'   # The data frame EPA.97.cadmium.111.df contains calibration data for
#'   # cadmium at mass 111 (ng/L) that appeared in Gibbons et al. (1997b)
#'   # and were provided to them by the U.S. EPA.
#'
#'   calibrate.list <- calibrate(Cadmium ~ Spike, data = EPA.97.cadmium.111.df)
#'
#'   names(calibrate.list)
#'
#'   calibrate.list
#'
#'
#'   #----------
#'
#'   # Clean up
#'   #---------
#'   rm(calibrate.list)
#' }
#' @rawRd
#' \keyword{classes}
NULL

#' S3 Class "distChoose"
#' @name distChoose.object
#' @description
#' Objects of S3 class \code{"distChoose"} are returned by the \pkg{EnvStats} function
#'   \code{\link{distChoose}}.
#' @rawRd
#' \docType{class}
#' @rawRd
#' \details{
#'   Objects of S3 class \code{"distChoose"} are lists that contain
#'   information about the candidate distributions, the estimated distribution
#'   parameters for each candidate distribution, and the test statistics and
#'   p-values associated with each candidate distribution.
#' }
#' @rawRd
#' \section{Methods}{
#'   Generic functions that have methods for objects of class
#'   \code{"distChoose"} include: \cr
#'   \code{\link{print}}.
#' }
#' @rawRd
#' \value{
#'   \strong{Required Components} \cr
#'   The following components must be included in a legitimate list of
#'   class \code{"distChoose"}.
#'
#'   \item{choices}{a character vector containing the full names
#'     of the candidate distributions. (see \code{\link{Distribution.df}}).}
#'   \item{method}{a character string denoting which method was used.}
#'   \item{decision}{a character vector containing the full name of the chosen distribution.}
#'   \item{alpha}{a numeric scalar between 0 and 1 specifying the Type I error
#'     associated with each goodness-of-fit test.}
#'   \item{distribution.parameters}{a numeric vector containing the estimated
#'     parameters associated with the chosen distribution.}
#'   \item{estimation.method}{a character string indicating the method
#'     used to compute the estimated parameters associated with the chosen
#'     distribution.  The value of this component will depend on the
#'     available estimation methods (see \code{\link{Distribution.df}}).}
#'   \item{sample.size}{a numeric scalar containing the number
#'     of non-missing observations in the sample used for the
#'     goodness-of-fit tests.}
#'   \item{test.results}{a list with the same number of components as the number
#'     of elements in the component \code{choices}.  The names of the list are the
#'     distribution abbreviations of the candidate distributions.
#'     (See the help file for \code{\link{Distribution.df}} for a list
#'       of distributions and their abbreviations.)
#'     Each component is an object of class \code{\link[=gof.object]{gof}}
#'     containing the results of the goodness-of-fit test for that particular
#'     hypothesized distribution.}
#'   \item{data.name}{character string indicating the name of the data
#'     object used for the goodness-of-fit tests.}
#'   \cr
#'
#'   \strong{Optional Components} \cr
#'   The following component is included in the result of
#'   calling \code{\link{distChoose}} when the argument \code{keep.data=TRUE}:
#'
#'   \item{data}{numeric vector containing the data actually used for
#'     the goodness-of-fit tests (i.e., the original data without any
#'     missing or infinite values).}
#'   \cr
#'
#'   The following component is included in the result of
#'   calling \code{\link{distChoose}} when missing (\code{NA}),
#'   undefined (\code{NaN}) and/or infinite (\code{Inf}, \code{-Inf})
#'   values are present:
#'
#'   \item{bad.obs}{numeric scalar indicating the number of missing (\code{NA}),
#'     undefined (\code{NaN}) and/or infinite (\code{Inf}, \code{-Inf})
#'     values that were removed from the data object prior to choosing
#'     a distribution.}
#' }
#' @rawRd
#' \note{
#'   Since objects of class \code{"distChoose"} are lists, you may extract
#'   their components with the \code{$} and \code{[[} operators.
#' }
#' @rawRd
#' \author{
#'   Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \seealso{
#'   \code{\link{distChoose}}, \code{\link{print.distChoose}},
#'   \link[=FcnsByCatGOFTests]{Goodness-of-Fit Tests},
#'   \code{\link{Distribution.df}}.
#' }
#' @rawRd
#' \examples{
#'   # Create an object of class "distChoose", then print it out.
#'   # (Note: the call to set.seed simply allows you to reproduce
#'   # this example.)
#'
#'   set.seed(47)
#'   dat <- rgamma(20, shape = 2, scale = 3)
#'
#'   distChoose.obj <- distChoose(dat)
#'
#'   mode(distChoose.obj)
#'   #[1] "list"
#'
#'   class(distChoose.obj)
#'   #[1] "distChoose"
#'
#'   names(distChoose.obj)
#'   #[1] "choices"                 "method"
#'   #[3] "decision"                "alpha"
#'   #[5] "distribution.parameters" "estimation.method"
#'   #[7] "sample.size"             "test.results"
#'   #[9] "data"                    "data.name"
#'
#'   distChoose.obj
#'
#'   #Results of Choosing Distribution
#'   #--------------------------------
#'   #
#'   #Candidate Distributions:         Normal
#'   #                                 Gamma
#'   #                                 Lognormal
#'   #
#'   #Choice Method:                   Shapiro-Wilk
#'   #
#'   #Type I Error per Test:           0.05
#'   #
#'   #Decision:                        Gamma
#'   #
#'   #Estimated Parameter(s):          shape = 1.909462
#'   #                                 scale = 4.056819
#'   #
#'   #Estimation Method:               MLE
#'   #
#'   #Data:                            dat
#'   #
#'   #Sample Size:                     20
#'   #
#'   #Test Results:
#'   #
#'   #  Normal
#'   #    Test Statistic:              W = 0.9097488
#'   #    P-value:                     0.06303695
#'   #
#'   #  Gamma
#'   #    Test Statistic:              W = 0.9834958
#'   #    P-value:                     0.970903
#'   #
#'   #  Lognormal
#'   #    Test Statistic:              W = 0.9185006
#'   #    P-value:                     0.09271768
#'
#'   #==========
#'
#'   # Extract the choices
#'   #--------------------
#'
#'   distChoose.obj$choices
#'   #[1] "Normal"    "Gamma"     "Lognormal"
#'
#'   #==========
#'
#'   # Clean up
#'   #---------
#'   rm(dat, distChoose.obj)
#' }
#' @rawRd
#' \keyword{classes}
NULL

#' S3 Class "distChooseCensored"
#' @name distChooseCensored.object
#' @description
#' Objects of S3 class \code{"distChooseCensored"} are returned by the \pkg{EnvStats} function
#'   \code{\link{distChooseCensored}}.
#' @rawRd
#' \docType{class}
#' @rawRd
#' \details{
#'   Objects of S3 class \code{"distChooseCensored"} are lists that contain
#'   information about the candidate distributions, the estimated distribution
#'   parameters for each candidate distribution, and the test statistics and
#'   p-values associated with each candidate distribution.
#' }
#' @rawRd
#' \section{Methods}{
#'   Generic functions that have methods for objects of class
#'   \code{"distChooseCensored"} include: \cr
#'   \code{\link{print}}.
#' }
#' @rawRd
#' \value{
#'   \strong{Required Components} \cr
#'   The following components must be included in a legitimate list of
#'   class \code{"distChooseCensored"}.
#'
#'   \item{choices}{a character vector containing the full names
#'     of the candidate distributions. (see \code{\link{Distribution.df}}).}
#'   \item{method}{a character string denoting which method was used.}
#'   \item{decision}{a character vector containing the full name of the chosen distribution.}
#'   \item{alpha}{a numeric scalar between 0 and 1 specifying the Type I error
#'     associated with each goodness-of-fit test.}
#'   \item{distribution.parameters}{a numeric vector containing the estimated
#'     parameters associated with the chosen distribution.}
#'   \item{estimation.method}{a character string indicating the method
#'     used to compute the estimated parameters associated with the chosen
#'     distribution.  The value of this component will depend on the
#'     available estimation methods (see \code{\link{Distribution.df}}).}
#'   \item{sample.size}{a numeric scalar containing the number
#'     of non-missing observations in the sample used for the
#'     goodness-of-fit tests.}
#'   \item{censoring.side}{character string indicating whether the data are
#'     left- or right-censored.}
#'   \item{censoring.levels}{numeric scalar or vector indicating the censoring level(s).}
#'   \item{percent.censored}{numeric scalar indicating the percent of non-missing
#'     observations that are censored.}
#'   \item{test.results}{a list with the same number of components as the number
#'     of elements in the component \code{choices}.  The names of the list are the
#'     distribution abbreviations of the candidate distributions.
#'     (See the help file for \code{\link{Distribution.df}} for a list
#'       of distributions and their abbreviations.)
#'     Each component is an object of class \code{\link[=gofCensored.object]{gofCensored}}
#'     containing the results of the goodness-of-fit test for that particular
#'     hypothesized distribution.}
#'   \item{data.name}{character string indicating the name of the data
#'     object used for the goodness-of-fit tests.}
#'   \item{censoring.name}{character string indicating the name of the data object
#'     used to identify which values are censored.}
#'   \cr
#'
#'   \strong{Optional Components} \cr
#'   The following components are included in the result of
#'   calling \code{\link{distChooseCensored}} when the argument \code{keep.data=TRUE}:
#'
#'   \item{data}{numeric vector containing the data actually used for
#'     the goodness-of-fit tests (i.e., the original data without any
#'     missing or infinite values).}
#'   \item{censored}{logical vector containing the censoring status for the
#'     data actually used for the goodness-of-fit tests
#'    (i.e., the original data without any missing or infinite values).}
#'   \cr
#'
#'   The following component is included in the result of
#'   calling \code{\link{distChooseCensored}} when missing (\code{NA}),
#'   undefined (\code{NaN}) and/or infinite (\code{Inf}, \code{-Inf})
#'   values are present:
#'
#'   \item{bad.obs}{numeric scalar indicating the number of missing (\code{NA}),
#'     undefined (\code{NaN}) and/or infinite (\code{Inf}, \code{-Inf})
#'     values that were removed from the data object prior to choosing
#'     a distribution.}
#' }
#' @rawRd
#' \note{
#'   Since objects of class \code{"distChooseCensored"} are lists, you may extract
#'   their components with the \code{$} and \code{[[} operators.
#' }
#' @rawRd
#' \author{
#'   Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \seealso{
#'   \code{\link{distChooseCensored}}, \code{\link{print.distChooseCensored}},
#'   \link[=FcnsByCatCensoredData]{Censored Data},
#'   \link[=FcnsByCatGOFTests]{Goodness-of-Fit Tests},
#'   \code{\link{Distribution.df}}.
#' }
#' @rawRd
#' \examples{
#'   # Create an object of class "distChooseCensored", then print it out.
#'   # (Note: the call to set.seed simply allows you to reproduce
#'   # this example.)
#'
#'   set.seed(598)
#'
#'   dat <- rgammaAlt(30, mean = 10, cv = 1)
#'   censored <- dat < 5
#'   dat[censored] <- 5
#'
#'   distChooseCensored.obj <- distChooseCensored(dat, censored,
#'     method = "sw", choices = c("norm", "gammaAlt", "lnormAlt"))
#'
#'   mode(distChooseCensored.obj)
#'   #[1] "list"
#'
#'   class(distChooseCensored.obj)
#'   #[1] "distChooseCensored"
#'
#'   names(distChooseCensored.obj)
#'   # [1] "choices"                 "method"
#'   # [3] "decision"                "alpha"
#'   # [5] "distribution.parameters" "estimation.method"
#'   # [7] "sample.size"             "censoring.side"
#'   # [9] "censoring.levels"        "percent.censored"
#'   #[11] "test.results"            "data"
#'   #[13] "censored"                "data.name"
#'   #[15] "censoring.name"
#'
#'   distChooseCensored.obj
#'
#'   #Results of Choosing Distribution
#'   #--------------------------------
#'   #
#'   #Candidate Distributions:         Normal
#'   #                                 Gamma
#'   #                                 Lognormal
#'   #
#'   #Choice Method:                   Shapiro-Wilk
#'   #
#'   #Type I Error per Test:           0.05
#'   #
#'   #Decision:                        Gamma
#'   #
#'   #Estimated Parameter(s):          mean = 12.4911448
#'   #                                 cv   =  0.7617343
#'   #
#'   #Estimation Method:               MLE
#'   #
#'   #Data:                            dat.censored
#'   #
#'   #Sample Size:                     30
#'   #
#'   #Censoring Side:                  left
#'   #
#'   #Censoring Variable:              censored
#'   #
#'   #Censoring Level(s):              5
#'   #
#'   #Percent Censored:                23.33333%
#'   #
#'   #Test Results:
#'   #
#'   #  Normal
#'   #    Test Statistic:              W = 0.9372741
#'   #    P-value:                     0.1704876
#'   #
#'   #  Gamma
#'   #    Test Statistic:              W = 0.9613711
#'   #    P-value:                     0.522329
#'   #
#'   #  Lognormal
#'   #    Test Statistic:              W = 0.9292406
#'   #    P-value:                     0.114511
#'
#'   #==========
#'
#'   # Extract the choices
#'   #--------------------
#'
#'   distChooseCensored.obj$choices
#'   #[1] "Normal"    "Gamma"     "Lognormal"
#'
#'   #==========
#'
#'   # Clean up
#'   #---------
#'   rm(dat, censored, distChooseCensored.obj)
#' }
#' @rawRd
#' \keyword{classes}
NULL

#' S3 Class "estimate"
#' @name estimate.object
#' @aliases estimate
#' @description
#' Objects of S3 class \code{"estimate"} are returned by any of the
#'   \pkg{EnvStats} functions that estimate the parameters or quantiles of a
#'   probability distribution and optionally construct confidence,
#'   prediction, or tolerance intervals based on a sample of data
#'   assumed to come from that distribution.
#' @rawRd
#' \docType{class}
#' @rawRd
#' \details{
#'   Objects of S3 class \code{"estimate"} are lists that contain
#'   information about the estimated distribution parameters,
#'   quantiles, and intervals.  The names of the \pkg{EnvStats}
#'   functions that produce objects of class \code{"estimate"}
#'   have the following forms:
#'
#'   \tabular{ll}{
#'   \bold{Form of Function Name} \tab \bold{Result} \cr
#'   \code{e}\emph{abb} \tab Parameter Estimation \cr
#'   \code{eq}\emph{abb} \tab Quantile Estimation \cr
#'   \code{predInt}\emph{Abb} \tab Prediction Interval \cr
#'   \code{tolInt}\emph{Abb} \tab Tolerance Interval \cr
#'   }
#'
#'   where \emph{abb} denotes the abbreviation of the name of a
#'   probability distribution (see the help file for
#'   \code{\link{Distribution.df}} for a list of available probability
#'   distributions and their abbreviations), and \emph{Abb} denotes the
#'   same thing as \emph{abb} except the first letter of the abbreviation
#'   for the probability distribution is capitalized.
#'
#'   See the help files
#'   \link{Estimating Distribution Parameters} and
#'   \link{Estimating Distribution Quantiles}
#'   for lists of functions that estimate distribution parameters
#'   and quantiles.  See the help files \link{Prediction Intervals}
#'   and \link{Tolerance Intervals} for lists of functions
#'   that create prediction and tolerance intervals.
#'
#'   For example:
#'   \itemize{
#'   \item The function \code{\link{enorm}} returns an object of class
#'     \code{"estimate"} (a list) with information about the estimated
#'     mean and standard deviation of the assumed normal (Gaussian)
#'     distribution, as well as an optional confidence interval for
#'     the mean.
#'   \item The function \code{\link{eqnorm}} returns a list of class
#'     \code{"estimate"} with information about the estimated mean and
#'     standard deviation of the assumed normal distribution, the
#'     estimated user-specified quantile(s), and an optional confidence
#'     interval for a single quantile.
#'   \item The function \code{\link{predIntNorm}} returns a list of class
#'     \code{"estimate"} with information about the estimated mean and
#'     standard deviation of the assumed normal distribution, along with a
#'     prediction interval for a user-specified number of future
#'     observations (or means, medians, or sums).
#'   \item The function \code{\link{tolIntNorm}} returns a list of class
#'     \code{"estimate"} with information about the estimated mean and
#'     standard deviation of the assumed normal distribution, along with a
#'     tolerance interval.
#'   }
#' }
#' @rawRd
#' \section{Methods}{
#'   Generic functions that have methods for objects of class
#'   \code{"estimate"} include: \cr
#'   \code{\link{print}}.
#' }
#' @rawRd
#' \value{
#'   \strong{Required Components} \cr
#'   The following components must be included in a legitimate list of
#'   class \code{"estimate"}.
#'
#'   \item{distribution}{character string indicating the name of the
#'     assumed distribution (this equals \code{"Nonparametric"}) for
#'     nonparametric procedures).}
#'   \item{sample.size}{numeric scalar indicating the sample size used
#'     to estimate the parameters or quantiles.}
#'   \item{data.name}{character string indicating the name of the data
#'     object used to compute the estimated parameters or quantiles.}
#'   \item{bad.obs}{numeric scalar indicating the number of missing (\code{NA}),
#'     undefined (\code{NaN}) and/or infinite (\code{Inf}, \code{-Inf})
#'     values that were removed from the data object prior to performing
#'     the estimation.} \cr
#'
#'   \strong{Optional Components} \cr
#'   The following components may optionally be included in a legitimate
#'   list of class \code{"estimate"}.
#'
#'   \item{parameters}{(parametric estimation only) a numeric vector
#'     with a names attribute containing the names and values of the
#'     estimated distribution parameters.}
#'   \item{n.param.est}{(parametric estimation only) a scalar indicating
#'     the number of distribution parameters estimated.}
#'   \item{method}{(parametric estimation only) a character string
#'     indicating the method used to compute the estimated parameters.}
#'   \item{quantiles}{a numeric vector of estimated quantiles.}
#'   \item{quantile.method}{a character string indicating the method of
#'     quantile estimation.}
#'   \item{interval}{a list of class \code{"intervalEstimate"} containing
#'     information on a confidence, tolerance, or prediction interval.} \cr
#'
#'   All lists of class \code{"intervalEstimate"} contain the following
#'   component:
#'
#'   \item{name}{a character string inidicating the kind of interval.
#'     Possible values are: \cr
#'     \code{"Confidence"}, \code{"Tolerance"}, or \code{"Prediction"}.} \cr
#'
#'   The number and names of the other components in a list of class
#'   \code{"intervalEstimate"} depends on the kind of interval it is.
#'   These components may include:
#'
#'   \item{parameter}{a character string indicating the parameter for
#'     which the interval is constructed (e.g., \code{"mean"},
#'     \code{"95'th \%ile"}, etc.).}
#'   \item{limits}{a numeric vector containing the lower and upper
#'     bounds of the interval.}
#'   \item{type}{the type of interval (i.e., \code{"two-sided"},
#'     \code{"lower"}, or \code{"upper"}).}
#'   \item{method}{the method used to construct the interval
#'     (e.g., \code{"normal.approx"}).}
#'   \item{conf.level}{the confidence level associated with the interval.}
#'   \item{sample.size}{the sample size associated with the interval.}
#'   \item{dof}{(parametric intervals only) the degrees of freedom
#'     associated with the interval.}
#'   \item{limit.ranks}{(nonparametric intervals only) the rank(s) of
#'     the order statistic(s) used to construct the interval.}
#'   \item{m}{(prediction intervals only) the total number of future
#'     observations (\code{n.mean=1}, \code{n.median=1}, or
#'     \code{n.sum=1}) or averages (\code{n.mean>1}), medians \cr
#'     (\code{n.median>1}), or sums (\code{n.sum>1}).}
#'   \item{k}{(prediction intervals only) the minimum number of future
#'     observations \cr
#'     (\code{n.mean=1}, \code{n.median=1}, or \code{n.sum=1}),
#'     or averages (\code{n.mean>1}), medians \cr
#'     (\code{n.median>1}) or sums (\code{n.sum>1}) out of the total \code{m}
#'     that the interval should contain.}
#'   \item{n.mean}{(prediction intervals only) the sample size associated
#'     with the future averages that should be contained in the interval.}
#'   \item{n.median}{(prediction intervals only) the sample size associated
#'     with the future medians that should be contained in the interval.}
#'   \item{n.sum}{(Poisson prediction intervals only) the sample size
#'     associated with the future sums that should be contained in the
#'     interval.}
#'   \item{rule}{(simultaneous prediction intervals only) the rule used to
#'     construct the simultaneous prediction interval.}
#'   \item{delta.over.sigma}{(simultaneous prediction intervals only) numeric
#'     scalar indicating the ratio \eqn{\Delta / \sigma}. The quantity
#'     \eqn{\Delta} (delta) denotes the difference between the mean of
#'     the population that was sampled to construct the prediction interval,
#'     and the mean of the population that will be sampled to produce the
#'     future observations.  The quantity \eqn{\sigma} (sigma) denotes the
#'     population standard deviation for both populations.}
#' }
#' @rawRd
#' \note{
#'   Since objects of class \code{"estimate"} are lists, you may extract
#'   their components with the \code{$} and \code{[[} operators.
#' }
#' @rawRd
#' \author{
#'   Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \seealso{
#'   \link{Estimating Distribution Parameters}, \link{Estimating Distribution Quantiles},
#'   \code{\link{Distribution.df}}, \link{Prediction Intervals},
#'   \link{Tolerance Intervals}, \code{\link{estimateCensored.object}}.
#' }
#' @rawRd
#' \examples{
#'   # Create an object of class "estimate", then print it out.
#'   # (Note: the call to set.seed simply allows you to reproduce
#'   # this example.)
#'
#'   set.seed(250)
#'
#'   dat <- rnorm(20, mean = 3, sd = 2)
#'
#'   estimate.obj <- enorm(dat, ci = TRUE)
#'
#'   mode(estimate.obj)
#'   #[1] "list"
#'
#'   class(estimate.obj)
#'   #[1] "estimate"
#'
#'   names(estimate.obj)
#'   #[1] "distribution" "sample.size"  "parameters"
#'   #[4] "n.param.est"  "method"       "data.name"
#'   #[7] "bad.obs"      "interval"
#'
#'   names(estimate.obj$interval)
#'   #[1] "name"        "parameter"   "limits"
#'   #[4] "type"        "method"      "conf.level"
#'   #[7] "sample.size" "dof"
#'
#'   estimate.obj
#'
#'   #Results of Distribution Parameter Estimation
#'   #--------------------------------------------
#'   #
#'   #Assumed Distribution:            Normal
#'   #
#'   #Estimated Parameter(s):          mean = 2.861160
#'   #                                 sd   = 1.180226
#'   #
#'   #Estimation Method:               mvue
#'   #
#'   #Data:                            dat
#'   #
#'   #Sample Size:                     20
#'   #
#'   #Confidence Interval for:         mean
#'   #
#'   #Confidence Interval Method:      Exact
#'   #
#'   #Confidence Interval Type:        two-sided
#'   #
#'   #Confidence Level:                95%
#'   #
#'   #Confidence Interval:             LCL = 2.308798
#'   #                                 UCL = 3.413523
#'
#'   #----------
#'
#'   # Extract the confidence limits for the mean
#'
#'   estimate.obj$interval$limits
#'   #     LCL      UCL
#'   #2.308798 3.413523
#'
#'   #----------
#'
#'   # Clean up
#'
#'   rm(dat, estimate.obj)
#' }
#' @rawRd
#' \keyword{classes}
NULL

#' S3 Class "estimateCensored"
#' @name estimateCensored.object
#' @aliases estimateCensored
#' @description
#' Objects of S3 class \code{"estimateCensored"} are returned by any of the
#'   \pkg{EnvStats} functions that estimate the parameters or quantiles of a
#'   probability distribution and optionally construct confidence,
#'   prediction, or tolerance intervals based on a sample of \bold{\emph{censored}}
#'   data assumed to come from that distribution.
#' @rawRd
#' \docType{class}
#' @rawRd
#' \details{
#'   Objects of S3 class \code{"estimateCensored"} are lists that contain
#'   information about the estimated distribution parameters,
#'   quantiles, and (if present) intervals, as well as the censoring side,
#'   censoring levels and percentage of censored observations.
#'   The names of the \pkg{EnvStats}
#'   functions that produce objects of class \code{"estimateCensored"}
#'   have the following forms:
#'
#'   \tabular{ll}{
#'   \bold{Form of Function Name} \tab \bold{Result} \cr
#'   \code{e}\emph{abb}\code{Censored} \tab Parameter Estimation \cr
#'   \code{eq}\emph{abb}\code{Censored} \tab Quantile Estimation \cr
#'   \code{predInt}\emph{Abb}\code{Censored} \tab Prediction Interval \cr
#'   \code{tolInt}\emph{Abb}\code{Censored} \tab Tolerance Interval
#'   }
#'
#'   where \emph{abb} denotes the abbreviation of the name of a
#'   probability distribution (see the help file for
#'   \code{\link{Distribution.df}} for a list of available probability
#'   distributions and their abbreviations), and \emph{Abb} denotes the
#'   same thing as \emph{abb} except the first letter of the abbreviation
#'   for the probability distribution is capitalized.
#'
#'   See the sections \bold{Estimating Distribution Parameters},
#'   \bold{Estimating Distribution Quantiles}, and
#'   \bold{Prediction and Tolerance Intervals} in the help file
#'   \link[=FcnsByCatCensoredData]{EnvStats Functions for Censored Data}
#'   for a list of functions that estimate distribution parameters, estimate
#'   distribution quantiles, create prediction intervals, or create tolerance intervals
#'   using censored data.
#'
#'   For example:
#'   \itemize{
#'   \item The function \code{\link{enormCensored}} returns an object of class
#'     \code{"estimateCensored"} (a list) with information about the estimated
#'     mean and standard deviation of the assumed normal (Gaussian)
#'     distribution, information about the amount and side of censoring, and also an
#'     optional confidence interval for the mean.
#'   \item The function \code{\link{eqnormCensored}} returns a list of class
#'     \code{"estimateCensored"} with information about the estimated mean and
#'     standard deviation of the assumed normal distribution, information about the
#'     amount and side of censoring, the
#'     estimated user-specified quantile(s), and an optional confidence
#'     interval for a single quantile.
#'   \item The function \code{\link{tolIntNormCensored}} returns a list of class
#'     \code{"estimateCensored"} with information about the estimated mean and
#'     standard deviation of the assumed normal distribution, information about the amount
#'     and side of censoring, and the computed tolerance interval.
#'   }
#' }
#' @rawRd
#' \section{Methods}{
#'   Generic functions that have methods for objects of class
#'   \code{"estimateCensored"} include: \cr
#'   \code{\link{print}}.
#' }
#' @rawRd
#' \value{
#'   \strong{Required Components} \cr
#'   The following components must be included in a legitimate list of
#'   class \code{"estimateCensored"}.
#'
#'   \item{distribution}{character string indicating the name of the
#'     assumed distribution (this equals \code{"Nonparametric"}) for
#'     nonparametric procedures).}
#'   \item{sample.size}{numeric scalar indicating the sample size used
#'       to estimate the parameters or quantiles.}
#'   \item{censoring.side}{character string indicating whether the data are
#'     left- or right-censored.}
#'   \item{censoring.levels}{numeric scalar or vector indicating the censoring level(s).}
#'   \item{percent.censored}{numeric scalar indicating the percent of non-missing
#'     observations that are censored.}
#'   \item{data.name}{character string indicating the name of the data
#'     object used to compute the estimateCensored parameters or quantiles.}
#'   \item{censoring.name}{character string indicating the name of the data object
#'     used to identify which values are censored.}
#'   \item{bad.obs}{numeric scalar indicating the number of missing (\code{NA}),
#'     undefined (\code{NaN}) and/or infinite (\code{Inf}, \code{-Inf})
#'     values that were removed from the data object prior to performing
#'     the estimation.} \cr
#'
#'   \strong{Optional Components} \cr
#'   The following components may optionally be included in a legitimate
#'   list of class \code{"estimateCensored"}.
#'
#'   \item{parameters}{(parametric estimation only) a numeric vector
#'     with a names attribute containing the names and values of the
#'     estimateCensored distribution parameters.}
#'   \item{n.param.est}{(parametric estimation only) a scalar indicating
#'     the number of distribution parameters estimateCensored.}
#'   \item{method}{(parametric estimation only) a character string
#'     indicating the method used to compute the estimateCensored parameters.}
#'   \item{quantiles}{a numeric vector of estimateCensored quantiles.}
#'   \item{quantile.method}{a character string indicating the method of
#'     quantile estimation.}
#'   \item{interval}{a list of class \code{"intervalEstimate"} containing
#'     information on a confidence, tolerance, or prediction interval.} \cr
#'
#'   All lists of class \code{"intervalEstimateCensored"} contain the following
#'   component:
#'
#'   \item{name}{a character string inidicating the kind of interval.
#'     Possible values are: \cr
#'     \code{"Confidence"}, \code{"Tolerance"}, or \code{"Prediction"}.} \cr
#'
#'   The number and names of the other components in a list of class
#'   \code{"intervalEstimate"} depends on the kind of interval it is.
#'   These components may include:
#'
#'   \item{parameter}{a character string indicating the parameter for
#'     which the interval is constructed (e.g., \code{"mean"},
#'     \code{"95'th \%ile"}, etc.).}
#'   \item{limits}{a numeric vector containing the lower and upper
#'     bounds of the interval.}
#'   \item{type}{the type of interval (i.e., \code{"two-sided"},
#'     \code{"lower"}, or \code{"upper"}).}
#'   \item{method}{the method used to construct the interval
#'     (e.g., \code{"normal.approx"}).}
#'   \item{conf.level}{the confidence level associated with the interval.}
#'   \item{sample.size}{the sample size associated with the interval.}
#'   \item{dof}{(parametric intervals only) the degrees of freedom
#'     associated with the interval.}
#'   \item{limit.ranks}{(nonparametric intervals only) the rank(s) of
#'     the order statistic(s) used to construct the interval.}
#'   \item{m}{(prediction intervals only) the total number of future
#'     observations (\code{n.mean=1}, \code{n.median=1}, or
#'     \code{n.sum=1}) or averages (\code{n.mean>1}), medians \cr
#'     (\code{n.median>1}), or sums (\code{n.sum>1}).}
#'   \item{k}{(prediction intervals only) the minimum number of future
#'     observations \cr
#'     (\code{n.mean=1}, \code{n.median=1}, or \code{n.sum=1}),
#'     or averages (\code{n.mean>1}), medians \cr
#'     (\code{n.median>1}) or sums (\code{n.sum>1}) out of the total \code{m}
#'     that the interval should contain.}
#'   \item{n.mean}{(prediction intervals only) the sample size associated
#'     with the future averages that should be contained in the interval.}
#'   \item{n.median}{(prediction intervals only) the sample size associated
#'     with the future medians that should be contained in the interval.}
#'   \item{n.sum}{(Poisson prediction intervals only) the sample size
#'     associated with the future sums that should be contained in the
#'     interval.}
#'   \item{rule}{(simultaneous prediction intervals only) the rule used to
#'     construct the simultaneous prediction interval.}
#'   \item{delta.over.sigma}{(simultaneous prediction intervals only) numeric
#'     scalar indicating the ratio \eqn{\Delta / \sigma}. The quantity
#'     \eqn{\Delta} (delta) denotes the difference between the mean of
#'     the population that was sampled to construct the prediction interval,
#'     and the mean of the population that will be sampled to produce the
#'     future observations.  The quantity \eqn{\sigma} (sigma) denotes the
#'     population standard deviation for both populations.}
#' }
#' @rawRd
#' \note{
#'   Since objects of class \code{"estimateCensored"} are lists, you may extract
#'   their components with the \code{$} and \code{[[} operators.
#' }
#' @rawRd
#' \author{
#'   Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \seealso{
#'   \link[=FcnsByCatCensoredData]{EnvStats Functions for Censored Data},
#'   \code{\link{Distribution.df}}, \code{\link{estimate.object}}.
#' }
#' @rawRd
#' \examples{
#'   # Create an object of class "estimateCensored", then print it out.
#'   # (Note: the call to set.seed simply allows you to reproduce
#'   # this example.)
#'
#'   set.seed(250)
#'
#'   dat <- rnorm(20, mean = 100, sd = 20)
#'   censored <- dat < 90
#'   dat[censored] <- 90
#'
#'   estimateCensored.obj <- enormCensored(dat, censored, ci = TRUE)
#'
#'   mode(estimateCensored.obj)
#'   #[1] "list"
#'
#'   class(estimateCensored.obj)
#'   #[1] "estimateCensored"
#'
#'   names(estimateCensored.obj)
#'   # [1] "distribution"     "sample.size"      "censoring.side"   "censoring.levels"
#'   # [5] "percent.censored" "parameters"       "n.param.est"      "method"
#'   # [9] "data.name"        "censoring.name"   "bad.obs"          "interval"
#'   #[13] "var.cov.params"
#'
#'   names(estimateCensored.obj$interval)
#'   #[1] "name"       "parameter"  "limits"     "type"       "method"     "conf.level"
#'
#'
#'   estimateCensored.obj
#'
#'   #Results of Distribution Parameter Estimation
#'   #Based on Type I Censored Data
#'   #--------------------------------------------
#'   #
#'   #Assumed Distribution:            Normal
#'   #
#'   #Censoring Side:                  left
#'   #
#'   #Censoring Level(s):              90
#'   #
#'   #Estimated Parameter(s):          mean = 96.52796
#'   #                                 sd   = 14.62275
#'   #
#'   #Estimation Method:               MLE
#'   #
#'   #Data:                            dat
#'   #
#'   #Censoring Variable:              censored
#'   #
#'   #Sample Size:                     20
#'   #
#'   #Percent Censored:                25%
#'   #
#'   #Confidence Interval for:         mean
#'   #
#'   #Confidence Interval Method:      Profile Likelihood
#'   #
#'   #Confidence Interval Type:        two-sided
#'   #
#'   #Confidence Level:                95%
#'   #
#'   #Confidence Interval:             LCL =  88.82415
#'   #                                 UCL = 103.27604
#'
#'
#'   #----------
#'
#'   # Extract the confidence limits for the mean
#'
#'   estimateCensored.obj$interval$limits
#'   #     LCL      UCL
#'   # 91.7801 103.7839
#'
#'   #----------
#'
#'   # Clean up
#'
#'   rm(dat, censored, estimateCensored.obj)
#' }
#' @rawRd
#' \keyword{classes}
NULL

#' S3 Class "gof"
#' @name gof.object
#' @aliases gof
#' @description
#' Objects of S3 class \code{"gof"} are returned by the \pkg{EnvStats} function
#'   \code{\link{gofTest}} when just the \code{x} argument is supplied.
#' @rawRd
#' \docType{class}
#' @rawRd
#' \details{
#'   Objects of S3 class \code{"gof"} are lists that contain
#'   information about the assumed distribution, the estimated or
#'   user-supplied distribution parameters, and the test statistic
#'   and p-value.
#' }
#' @rawRd
#' \section{Methods}{
#'   Generic functions that have methods for objects of class
#'   \code{"gof"} include: \cr
#'   \code{\link{print}}, \code{\link{plot}}.
#' }
#' @rawRd
#' \value{
#'   \strong{Required Components} \cr
#'   The following components must be included in a legitimate list of
#'   class \code{"gof"}.
#'
#'   \item{distribution}{a character string indicating the name of the
#'     assumed distribution (see \cr
#'     \code{\link{Distribution.df}}).}
#'   \item{dist.abb}{a character string containing the abbreviated name
#'     of the distribution (see \cr
#'     \code{\link{Distribution.df}}).}
#'   \item{distribution.parameters}{a numeric vector
#'     with a names attribute containing the names and values of the
#'     estimated or user-supplied distribution parameters associated
#'     with the assumed distribution.}
#'   \item{n.param.est}{a scalar indicating the number of distribution
#'     parameters estimated prior to performing the goodness-of-fit
#'     test. The value of this component will be \code{0} if the parameters
#'     were supplied by the user.}
#'   \item{estimation.method}{a character string indicating the method
#'     used to compute the estimated parameters.  The value of this
#'     component will depend on the available estimation methods
#'     (see \code{\link{Distribution.df}}).  The value of this component
#'     will be \code{NULL} if the parameters were supplied by the user.}
#'   \item{statistic}{a numeric scalar with a names attribute containing
#'     the name and value of the goodness-of-fit statistic.}
#'   \item{sample.size}{a numeric scalar containing the number
#'     of non-missing observations in the sample used for the
#'     goodness-of-fit test.}
#'   \item{parameters}{numeric vector with a names attribute containing
#'     the name(s) and value(s) of the parameter(s) associated with the
#'     test statistic given in the \code{statistic} component.}
#'   \item{z.value}{(except when \code{test="chisq"} or \code{test="ks"})
#'     numeric scalar containing the z-value associated with the
#'     goodness-of-fit statistic.}
#'   \item{p.value}{numeric scalar containing the p-value associated with
#'     the goodness-of-fit statistic.}
#'   \item{alternative}{character string indicating the alternative hypothesis.}
#'   \item{method}{character string indicating the name of the
#'     goodness-of-fit test (e.g., \cr
#'     \code{"Shapiro-Wilk GOF"}).}
#'   \item{data}{numeric vector containing the data actually used for
#'     the goodness-of-fit test (i.e., the original data without any
#'     missing or infinite values).}
#'   \item{data.name}{character string indicating the name of the data
#'     object used for the goodness-of-fit test.}
#'   \item{bad.obs}{numeric scalar indicating the number of missing (\code{NA}),
#'     undefined (\code{NaN}) and/or infinite (\code{Inf}, \code{-Inf})
#'     values that were removed from the data object prior to performing
#'     the goodness-of-fit test.}
#'   \cr
#'
#'   \emph{NOTE}: when the function \code{\link{gofTest}} is called with
#'   both arguments \code{x} and \code{y} and \code{test="ks"}, it
#'   returns an object of class \code{\link[=gofTwoSample.object]{"gofTwoSample"}}.
#'   No specific parametric distribution is assumed, so the value of the component
#'   \code{distribution} is \code{"Equal"} and the following components
#'   are omitted: \code{dist.abb}, \code{distribution.parameters},
#'   \code{n.param.est}, \code{estimation.method}, and \code{z.value}. \cr
#'
#'   \strong{Optional Components} \cr
#'   The following components are included in the result of
#'   calling \code{\link{gofTest}} with the argument \cr
#'   \code{test="chisq"} and may be used by the function
#'   \code{\link{plot.gof}}:
#'
#'   \item{cut.points}{numeric vector containing the cutpoints used to
#'     define the cells.}
#'   \item{counts}{numeric vector containing the observed number of
#'     counts for each cell.}
#'   \item{expected}{numeric vector containing the expected number of
#'     counts for each cell.}
#'   \item{X2.components}{numeric vector containing the contribution of
#'     each cell to the chi-square statistic.}
#' }
#' @rawRd
#' \note{
#'   Since objects of class \code{"gof"} are lists, you may extract
#'   their components with the \code{$} and \code{[[} operators.
#' }
#' @rawRd
#' \author{
#'   Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \seealso{
#'   \code{\link{gofTest}}, \code{\link{print.gof}}, \code{\link{plot.gof}},
#'   \link[=FcnsByCatGOFTests]{Goodness-of-Fit Tests},
#'   \code{\link{Distribution.df}}, \code{\link{gofCensored.object}}.
#' }
#' @rawRd
#' \examples{
#'   # Create an object of class "gof", then print it out.
#'   # (Note: the call to set.seed simply allows you to reproduce
#'   # this example.)
#'
#'   set.seed(250)
#'
#'   dat <- rnorm(20, mean = 3, sd = 2)
#'
#'   gof.obj <- gofTest(dat)
#'
#'   mode(gof.obj)
#'   #[1] "list"
#'
#'   class(gof.obj)
#'   #[1] "gof"
#'
#'   names(gof.obj)
#'   # [1] "distribution"            "dist.abb"
#'   # [3] "distribution.parameters" "n.param.est"
#'   # [5] "estimation.method"       "statistic"
#'   # [7] "sample.size"             "parameters"
#'   # [9] "z.value"                 "p.value"
#'   #[11] "alternative"             "method"
#'   #[13] "data"                    "data.name"
#'   #[15] "bad.obs"
#'
#'   gof.obj
#'
#'   #Results of Goodness-of-Fit Test
#'   #-------------------------------
#'   #
#'   #Test Method:                     Shapiro-Wilk GOF
#'   #
#'   #Hypothesized Distribution:       Normal
#'   #
#'   #Estimated Parameter(s):          mean = 2.861160
#'   #                                 sd   = 1.180226
#'   #
#'   #Estimation Method:               mvue
#'   #
#'   #Data:                            dat
#'   #
#'   #Sample Size:                     20
#'   #
#'   #Test Statistic:                  W = 0.9640724
#'   #
#'   #Test Statistic Parameter:        n = 20
#'   #
#'   #P-value:                         0.6279872
#'   #
#'   #Alternative Hypothesis:          True cdf does not equal the
#'   #                                 Normal Distribution.
#'
#'   #==========
#'
#'   # Extract the p-value
#'   #--------------------
#'
#'   gof.obj$p.value
#'   #[1] 0.6279872
#'
#'   #==========
#'
#'   # Plot the results of the test
#'   #-----------------------------
#'
#'   dev.new()
#'   plot(gof.obj)
#'
#'   #==========
#'
#'   # Clean up
#'   #---------
#'   rm(dat, gof.obj)
#'   graphics.off()
#' }
#' @rawRd
#' \keyword{classes}
NULL

#' S3 Class "gofCensored"
#' @name gofCensored.object
#' @aliases gofCensored
#' @description
#' Objects of S3 class \code{"gofCensored"} are returned by the \pkg{EnvStats} function
#'   \code{\link{gofTestCensored}}.
#' @rawRd
#' \docType{class}
#' @rawRd
#' \details{
#'   Objects of S3 class \code{"gofCensored"} are lists that contain
#'   information about the assumed distribution, the amount of censoring,
#'   the estimated or user-supplied distribution parameters, and the test
#'   statistic and p-value.
#' }
#' @rawRd
#' \section{Methods}{
#'   Generic functions that have methods for objects of class
#'   \code{"gofCensored"} include: \cr
#'   \code{\link{print}}, \code{\link{plot}}.
#' }
#' @rawRd
#' \value{
#'   \strong{Required Components} \cr
#'   The following components must be included in a legitimate list of
#'   class \code{"gofCensored"}.
#'
#'   \item{distribution}{a character string indicating the name of the
#'     assumed distribution (see \cr
#'     \code{\link{Distribution.df}}).}
#'   \item{dist.abb}{a character string containing the abbreviated name
#'     of the distribution (see \cr
#'     \code{\link{Distribution.df}}).}
#'   \item{distribution.parameters}{a numeric vector
#'     with a names attribute containing the names and values of the
#'     estimated or user-supplied distribution parameters associated
#'     with the assumed distribution.}
#'   \item{n.param.est}{a scalar indicating the number of distribution
#'     parameters estimated prior to performing the goodness-of-fit
#'     test. The value of this component will be \code{0} if the parameters
#'     were supplied by the user.}
#'   \item{estimation.method}{a character string indicating the method
#'     used to compute the estimated parameters.  The value of this
#'     component will depend on the available estimation methods
#'     (see \code{\link{Distribution.df}}).  The value of this component
#'     will be \code{NULL} if the parameters were supplied by the user.}
#'   \item{statistic}{a numeric scalar with a names attribute containing
#'     the name and value of the goodness-of-fit statistic.}
#'   \item{sample.size}{a numeric scalar containing the number
#'     of non-missing observations in the sample used for the
#'     goodness-of-fit test.}
#'   \item{censoring.side}{character string indicating whether the data are
#'     left- or right-censored.}
#'   \item{censoring.levels}{numeric scalar or vector indicating the censoring level(s).}
#'   \item{percent.censored}{numeric scalar indicating the percent of non-missing
#'     observations that are censored.}
#'   \item{parameters}{numeric vector with a names attribute containing
#'     the name(s) and value(s) of the parameter(s) associated with the
#'     test statistic given in the \code{statistic} component.}
#'   \item{z.value}{(except when \code{test="chisq"} or \code{test="ks"})
#'     numeric scalar containing the z-value associated with the
#'     goodness-of-fit statistic.}
#'   \item{p.value}{numeric scalar containing the p-value associated with
#'     the goodness-of-fit statistic.}
#'   \item{alternative}{character string indicating the alternative hypothesis.}
#'   \item{method}{character string indicating the name of the
#'     goodness-of-fit test (e.g., \cr
#'     \code{"Shapiro-Wilk GOF"}).}
#'   \item{data.name}{character string indicating the name of the data
#'     object used for the goodness-of-fit test.}
#'   \item{censored}{logical vector indicating which observations are censored.}
#'   \item{censoring.name}{character string indicating the name of the object
#'     used to indicate the censoring.} \cr
#'
#'   \strong{Optional Components} \cr
#'   The following components are included when the argument \code{keep.data} is
#'   set to \code{TRUE} in the call to the function producing the
#'   object of class \code{"gofCensored"}.
#'   \item{data}{numeric vector containing the data actually used for
#'     the goodness-of-fit test (i.e., the original data without any
#'     missing or infinite values).}
#'   \item{censored}{logical vector indicating the censoring status of the data
#'     actually used for the goodness-of-fit test.} \cr
#'
#'   The following component is included when the data object
#'   contains missing (\code{NA}), undefined (\code{NaN}) and/or infinite
#'   (\code{Inf}, \code{-Inf}) values.
#'
#'   \item{bad.obs}{numeric scalar indicating the number of missing (\code{NA}),
#'     undefined (\code{NaN}) and/or infinite (\code{Inf}, \code{-Inf})
#'     values that were removed from the data object prior to performing
#'     the goodness-of-fit test.}
#' }
#' @rawRd
#' \note{
#'   Since objects of class \code{"gofCensored"} are lists, you may extract
#'   their components with the \code{$} and \code{[[} operators.
#' }
#' @rawRd
#' \author{
#'   Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \seealso{
#'   \code{\link{gofTestCensored}}, \code{\link{print.gofCensored}},
#'   \code{\link{plot.gofCensored}},
#'   \link[=FcnsByCatCensoredData]{Censored Data},
#'   \link[=FcnsByCatGOFTests]{Goodness-of-Fit Tests},
#'   \code{\link{Distribution.df}}, \code{gof.object}.
#' }
#' @rawRd
#' \examples{
#'   # Create an object of class "gofCensored", then print it out.
#'   #------------------------------------------------------------
#'
#'   gofCensored.obj <- with(EPA.09.Ex.15.1.manganese.df,
#'     gofTestCensored(Manganese.ppb, Censored, test = "sf"))
#'
#'   mode(gofCensored.obj)
#'   #[1] "list"
#'
#'   class(gofCensored.obj)
#'   #[1] "gofCensored"
#'
#'   names(gofCensored.obj)
#'   # [1] "distribution"            "dist.abb"
#'   # [3] "distribution.parameters" "n.param.est"
#'   # [5] "estimation.method"       "statistic"
#'   # [7] "sample.size"             "censoring.side"
#'   # [9] "censoring.levels"        "percent.censored"
#'   #[11] "parameters"              "z.value"
#'   #[13] "p.value"                 "alternative"
#'   #[15] "method"                  "data"
#'   #[17] "data.name"               "censored"
#'   #[19] "censoring.name"          "bad.obs"
#'
#'   gofCensored.obj
#'
#'   #Results of Goodness-of-Fit Test
#'   #Based on Type I Censored Data
#'   #-------------------------------
#'   #
#'   #Test Method:                     Shapiro-Francia GOF
#'   #                                 (Multiply Censored Data)
#'   #
#'   #Hypothesized Distribution:       Normal
#'   #
#'   #Censoring Side:                  left
#'   #
#'   #Censoring Level(s):              2 5
#'   #
#'   #Estimated Parameter(s):          mean = 15.23508
#'   #                                 sd   = 30.62812
#'   #
#'   #Estimation Method:               MLE
#'   #
#'   #Data:                            Manganese.ppb
#'   #
#'   #Censoring Variable:              Censored
#'   #
#'   #Sample Size:                     25
#'   #
#'   #Percent Censored:                24%
#'   #
#'   #Test Statistic:                  W = 0.8368016
#'   #
#'   #Test Statistic Parameters:       N     = 25.00
#'   #                                 DELTA =  0.24
#'   #
#'   #P-value:                         0.004662658
#'   #
#'   #Alternative Hypothesis:          True cdf does not equal the
#'   #                                 Normal Distribution.
#'
#'   #==========
#'
#'   # Extract the p-value
#'   #--------------------
#'
#'   gofCensored.obj$p.value
#'   #[1] 0.004662658
#'
#'   #==========
#'
#'   # Plot the results of the test
#'   #-----------------------------
#'
#'   dev.new()
#'   plot(gofCensored.obj)
#'
#'   #==========
#'
#'   # Clean up
#'   #---------
#'   rm(gofCensored.obj)
#'   graphics.off()
#' }
#' @rawRd
#' \keyword{classes}
NULL

#' S3 Class "gofGroup"
#' @name gofGroup.object
#' @aliases gofGroup
#' @description
#' Objects of S3 class \code{"gofGroup"} are returned by the \pkg{EnvStats} function
#'   \code{\link{gofGroupTest}}.
#' @rawRd
#' \docType{class}
#' @rawRd
#' \details{
#'   Objects of S3 class \code{"gofGroup"} are lists that contain
#'   information about the assumed distribution, the estimated or
#'   user-supplied distribution parameters, and the test statistic
#'   and p-value.
#' }
#' @rawRd
#' \section{Methods}{
#'   Generic functions that have methods for objects of class
#'   \code{"gofGroup"} include: \cr
#'   \code{\link{print}}, \code{\link{plot}}.
#' }
#' @rawRd
#' \value{
#'   \strong{Required Components} \cr
#'   The following components must be included in a legitimate list of
#'   class \code{"gofGroup"}.
#'
#'   \item{distribution}{a character string indicating the name of the
#'     assumed distribution (see \cr
#'     \code{\link{Distribution.df}}).}
#'   \item{dist.abb}{a character string containing the abbreviated name
#'     of the distribution (see \cr
#'     \code{\link{Distribution.df}}).}
#'   \item{statistic}{a numeric scalar with a names attribute containing
#'     the name and value of the goodness-of-fit statistic.}
#'   \item{sample.size}{a numeric scalar containing the number
#'     of non-missing observations in the sample used for the
#'     goodness-of-fit test.}
#'   \item{parameters}{numeric vector with a names attribute containing
#'     the name(s) and value(s) of the parameter(s) associated with the
#'     test statistic given in the \code{statistic} component.}
#'   \item{p.value}{numeric scalar containing the p-value associated with
#'     the goodness-of-fit statistic.}
#'   \item{alternative}{character string indicating the alternative hypothesis.}
#'   \item{method}{character string indicating the name of the
#'     goodness-of-fit test (e.g., \cr
#'     \code{"Wilk-Shapiro GOF (Normal Scores)"}).}
#'   \item{data.name}{character string indicating the name of the data
#'     object used for the goodness-of-fit test.}
#'   \item{grouping.variable}{character string indicating the name of the variable
#'     defining the groups.}
#'   \item{bad.obs}{numeric vector indicating the number of missing (\code{NA}),
#'     undefined (\code{NaN}) and/or infinite (\code{Inf}, \code{-Inf})
#'     values that were removed from each group and the grouping variable
#'     prior to performing the goodness-of-fit test.}
#'   \item{n.groups}{numeric scalar containing the number of groups.}
#'   \item{group.names}{character vector containing the levels of the grouping
#'     variable, i.e., the names of each of the groups.}
#'   \item{group.scores}{numeric vector containing the individual statistics for
#'     each group.}
#'
#'   \strong{Optional Component} \cr
#'   The following component is included when \code{\link{gofGroupTest}} is
#'   called with a formula for the first argument and a \code{data} argument.
#'
#'   \item{parent.of.data}{character string indicating the name of the object supplied
#'     in the \code{data} argument.}
#' }
#' @rawRd
#' \note{
#'   Since objects of class \code{"gofGroup"} are lists, you may extract
#'   their components with the \code{$} and \code{[[} operators.
#' }
#' @rawRd
#' \author{
#'   Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \seealso{
#'   \code{\link{gofGroupTest}}, \code{\link{print.gofGroup}}, \code{\link{plot.gofGroup}},
#'   \link[=FcnsByCatGOFTests]{Goodness-of-Fit Tests},
#'   \code{\link{Distribution.df}}.
#' }
#' @rawRd
#' \examples{
#'   # Create an object of class "gofGroup", then print it out.
#'
#'   # Example 10-4 of USEPA (2009, page 10-20) gives an example of
#'   # simultaneously testing the assumption of normality for nickel
#'   # concentrations (ppb) in groundwater collected at 4 monitoring
#'   # wells over 5 months.  The data for this example are stored in
#'   # EPA.09.Ex.10.1.nickel.df.
#'
#'   gofGroup.obj <- gofGroupTest(Nickel.ppb ~ Well,
#'     data = EPA.09.Ex.10.1.nickel.df)
#'
#'   mode(gofGroup.obj)
#'   #[1] "list"
#'
#'   class(gofGroup.obj)
#'   #[1] "gofGroup"
#'
#'   names(gofGroup.obj)
#'   # [1] "distribution"      "dist.abb"          "statistic"
#'   # [4] "sample.size"       "parameters"        "p.value"
#'   # [7] "alternative"       "method"            "data.name"
#'   #[10] "grouping.variable" "parent.of.data"    "bad.obs"
#'   #[13] "n.groups"          "group.names"       "group.scores"
#'
#'   gofGroup.obj
#'   #Results of Group Goodness-of-Fit Test
#'   #-------------------------------------
#'   #
#'   #Test Method:                     Wilk-Shapiro GOF (Normal Scores)
#'   #
#'   #Hypothesized Distribution:       Normal
#'   #
#'   #Data:                            Nickel.ppb
#'   #
#'   #Grouping Variable:               Well
#'   #
#'   #Data Source:                     EPA.09.Ex.10.1.nickel.df
#'   #
#'   #Number of Groups:                4
#'   #
#'   #Sample Sizes:                    Well.1 = 5
#'   #                                 Well.2 = 5
#'   #                                 Well.3 = 5
#'   #                                 Well.4 = 5
#'   #
#'   #Test Statistic:                  z (G) = -3.658696
#'   #
#'   #P-values for
#'   #Individual Tests:                Well.1 = 0.03510747
#'   #                                 Well.2 = 0.02385344
#'   #                                 Well.3 = 0.01120775
#'   #                                 Well.4 = 0.10681461
#'   #
#'   #P-value for
#'   #Group Test:                      0.0001267509
#'   #
#'   #Alternative Hypothesis:          At least one group
#'   #                                 does not come from a
#'   #                                 Normal Distribution.
#'
#'   #==========
#'
#'   # Extract the p-values
#'   #---------------------
#'
#'   gofGroup.obj$p.value
#'   #      Well.1       Well.2       Well.3       Well.4        z (G)
#'   #0.0351074733 0.0238534406 0.0112077511 0.1068146088 0.0001267509
#'
#'   #==========
#'
#'   # Plot the results of the test
#'   #-----------------------------
#'
#'   dev.new()
#'   plot(gofGroup.obj)
#'
#'   #==========
#'
#'   # Clean up
#'   #---------
#'   rm(gofGroup.obj)
#'   graphics.off()
#' }
#' @rawRd
#' \keyword{classes}
NULL

#' S3 Class "gofOutlier"
#' @name gofOutlier.object
#' @aliases gofOutlier
#' @description
#' Objects of S3 class \code{"gofOutlier"} are returned by the \pkg{EnvStats} function
#'   \code{\link{rosnerTest}}.
#' @rawRd
#' \docType{class}
#' @rawRd
#' \details{
#'   Objects of S3 class \code{"gofOutlier"} are lists that contain
#'   information about the assumed distribution, the test statistics,
#'   the Type I error level, and the number of outliers detected.
#' }
#' @rawRd
#' \section{Methods}{
#'   Generic functions that have methods for objects of class
#'   \code{"gofOutlier"} include: \cr
#'   \code{\link{print}}.
#' }
#' @rawRd
#' \value{
#'   \strong{Required Components} \cr
#'   The following components must be included in a legitimate list of
#'   class \code{"gofOutlier"}.
#'
#'   \item{distribution}{a character string indicating the name of the
#'     assumed distribution (see \cr
#'     \code{\link{Distribution.df}}).}
#'   \item{statistic}{a numeric vector with a names attribute containing
#'     the names and values of the outlier test statistic for each outlier tested.}
#'   \item{sample.size}{a numeric scalar containing the number
#'     of non-missing observations in the sample used for the
#'     outlier test.}
#'   \item{parameters}{numeric vector with a names attribute containing
#'     the name(s) and value(s) of the parameter(s) associated with the
#'     test statistic given in the \code{statistic} component.}
#'   \item{alpha}{numeric scalar indicating the Type I error level.}
#'   \item{crit.value}{numeric vector containing the critical values associated with
#'     the test for each outlier.}
#'   \item{alternative}{character string indicating the alternative hypothesis.}
#'   \item{method}{character string indicating the name of the outlier test.}
#'   \item{data}{numeric vector containing the data actually used for
#'     the outlier test (i.e., the original data without any missing or infinite values).}
#'   \item{data.name}{character string indicating the name of the data
#'     object used for the goodness-of-fit test.}
#'   \item{all.stats}{data frame containing all of the results of the test.}
#'   \cr
#'
#'   \strong{Optional Components} \cr
#'   The following component is included when the data object
#'   contains missing (\code{NA}), undefined (\code{NaN}) and/or infinite
#'   (\code{Inf}, \code{-Inf}) values.
#'
#'   \item{bad.obs}{numeric scalar indicating the number of missing (\code{NA}),
#'     undefined (\code{NaN}) and/or infinite (\code{Inf}, \code{-Inf})
#'     values that were removed from the data object prior to performing
#'     the test for outliers.}
#' }
#' @rawRd
#' \note{
#'   Since objects of class \code{"gofOutlier"} are lists, you may extract
#'   their components with the \code{$} and \code{[[} operators.
#' }
#' @rawRd
#' \author{
#'   Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \seealso{
#'   \code{\link{rosnerTest}}, \code{\link{print.gofOutlier}},
#'   \link[=FcnsByCatGOFTests]{Goodness-of-Fit Tests}.
#' }
#' @rawRd
#' \examples{
#'   # Create an object of class "gofOutlier", then print it out.
#'   # (Note: the call to set.seed simply allows you to reproduce
#'   # this example.)
#'
#'   set.seed(250)
#'
#'   dat <- c(rnorm(30, mean = 3, sd = 2), rnorm(3, mean = 10, sd = 1))
#'
#'   gofOutlier.obj <- rosnerTest(dat, k = 4)
#'
#'   mode(gofOutlier.obj)
#'   #[1] "list"
#'
#'   class(gofOutlier.obj)
#'   #[1] "gofOutlier"
#'
#'   names(gofOutlier.obj)
#'   # [1] "distribution" "statistic"    "sample.size"  "parameters"
#'   # [5] "alpha"        "crit.value"   "n.outliers"   "alternative"
#'   # [9] "method"       "data"         "data.name"    "bad.obs"
#'   #[13] "all.stats"
#'
#'   gofOutlier.obj
#'
#'   #Results of Outlier Test
#'   #-------------------------
#'   #
#'   #Test Method:                     Rosner's Test for Outliers
#'   #
#'   #Hypothesized Distribution:       Normal
#'   #
#'   #Data:                            dat
#'   #
#'   #Sample Size:                     33
#'   #
#'   #Test Statistics:                 R.1 = 2.848514
#'   #                                 R.2 = 3.086875
#'   #                                 R.3 = 3.033044
#'   #                                 R.4 = 2.380235
#'   #
#'   #Test Statistic Parameter:        k = 4
#'   #
#'   #Alternative Hypothesis:          Up to 4 observations are not
#'   #                                 from the same Distribution.
#'   #
#'   #Type I Error:                    5%
#'   #
#'   #Number of Outliers Detected:     3
#'   #
#'   #  i   Mean.i     SD.i      Value Obs.Num    R.i+1 lambda.i+1 Outlier
#'   #1 0 3.549744 2.531011 10.7593656      33 2.848514   2.951949    TRUE
#'   #2 1 3.324444 2.209872 10.1460427      31 3.086875   2.938048    TRUE
#'   #3 2 3.104392 1.856109  8.7340527      32 3.033044   2.923571    TRUE
#'   #4 3 2.916737 1.560335 -0.7972275      25 2.380235   2.908473   FALSE
#'
#'   #==========
#'
#'   # Extract the data frame with all the test results
#'   #-------------------------------------------------
#'
#'   gofOutlier.obj$all.stats
#'   #  i   Mean.i     SD.i      Value Obs.Num    R.i+1 lambda.i+1 Outlier
#'   #1 0 3.549744 2.531011 10.7593656      33 2.848514   2.951949    TRUE
#'   #2 1 3.324444 2.209872 10.1460427      31 3.086875   2.938048    TRUE
#'   #3 2 3.104392 1.856109  8.7340527      32 3.033044   2.923571    TRUE
#'   #4 3 2.916737 1.560335 -0.7972275      25 2.380235   2.908473   FALSE
#'
#'   #==========
#'
#'   # Clean up
#'   #---------
#'   rm(dat, gofOutlier.obj)
#' }
#' @rawRd
#' \keyword{classes}
NULL

#' S3 Class "gofTwoSample"
#' @name gofTwoSample.object
#' @aliases gofTwoSample
#' @description
#' Objects of S3 class \code{"gofTwoSample"} are returned by the \pkg{EnvStats} function
#'   \code{\link{gofTest}} when both the \code{x} and \code{y} arguments are supplied.
#' @rawRd
#' \docType{class}
#' @rawRd
#' \details{
#'   Objects of S3 class \code{"gofTwoSample"} are lists that contain
#'   information about the assumed distribution, the estimated or
#'   user-supplied distribution parameters, and the test statistic
#'   and p-value.
#' }
#' @rawRd
#' \section{Methods}{
#'   Generic functions that have methods for objects of class
#'   \code{"gofTwoSample"} include: \cr
#'   \code{\link{print}}, \code{\link{plot}}.
#' }
#' @rawRd
#' \value{
#'   \strong{Required Components} \cr
#'   The following components must be included in a legitimate list of
#'   class \code{"gofTwoSample"}.
#'
#'   \item{distribution}{a character string with the value \code{"Equal"}.}
#'   \item{statistic}{a numeric scalar with a names attribute containing
#'     the name and value of the goodness-of-fit statistic.}
#'   \item{sample.size}{a numeric scalar containing the number
#'     of non-missing observations in the sample used for the
#'     goodness-of-fit test.}
#'   \item{parameters}{numeric vector with a names attribute containing
#'     the name(s) and value(s) of the parameter(s) associated with the
#'     test statistic given in the \code{statistic} component.}
#'   \item{p.value}{numeric scalar containing the p-value associated with
#'     the goodness-of-fit statistic.}
#'   \item{alternative}{character string indicating the alternative hypothesis.}
#'   \item{method}{character string indicating the name of the
#'     goodness-of-fit test.}
#'   \item{data}{a list of length 2 containing the numeric vectors actually used for
#'     the goodness-of-fit test (i.e., the original data but with any
#'     missing or infinite values removed).}
#'   \item{data.name}{a character vector of length 2 indicating the name of the data
#'     object used for the \code{x} argument and the name of the data object used
#'     for the \code{y} argument in the goodness-of-fit test.}
#'
#'   \strong{Optional Component} \cr
#'   The following component is included when the arguments \code{x} and/or \code{y}
#'   contain missing (\code{NA}), undefined (\code{NaN}) and/or infinite
#'   (\code{Inf}, \code{-Inf}) values.
#'
#'   \item{bad.obs}{numeric vector of length 2 indicating the number of missing (\code{NA}),
#'     undefined (\code{NaN}) and/or infinite (\code{Inf}, \code{-Inf})
#'     values that were removed from the data in the \code{x} and \code{y} arguments
#'     prior to performing the goodness-of-fit test.}
#' }
#' @rawRd
#' \note{
#'   Since objects of class \code{"gofTwoSample"} are lists, you may extract
#'   their components with the \code{$} and \code{[[} operators.
#' }
#' @rawRd
#' \author{
#'   Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \seealso{
#'   \code{\link{print.gofTwoSample}}, \code{\link{plot.gofTwoSample}},
#'   \link[=FcnsByCatGOFTests]{Goodness-of-Fit Tests}.
#' }
#' @rawRd
#' \examples{
#'   # Create an object of class "gofTwoSample", then print it out.
#'
#'   # Generate 20 observations from a normal distribution with mean=3 and sd=2, and
#'   # generate 10 observaions from a normal distribution with mean=2 and sd=2 then
#'   # test whether these sets of observations come from the same distribution.
#'   # (Note: the call to set.seed simply allows you to reproduce this example.)
#'
#'   set.seed(300)
#'   dat1 <- rnorm(20, mean = 3, sd = 2)
#'   dat2 <- rnorm(10, mean = 1, sd = 2)
#'   gofTest(x = dat1, y = dat2, test = "ks")
#'
#'   #Results of Goodness-of-Fit Test
#'   #-------------------------------
#'   #
#'   #Test Method:                     2-Sample K-S GOF
#'   #
#'   #Hypothesized Distribution:       Equal
#'   #
#'   #Data:                            x = dat1
#'   #                                 y = dat2
#'   #
#'   #Sample Sizes:                    n.x = 20
#'   #                                 n.y = 10
#'   #
#'   #Test Statistic:                  ks = 0.7
#'   #
#'   #Test Statistic Parameters:       n = 20
#'   #                                 m = 10
#'   #
#'   #P-value:                         0.001669561
#'   #
#'   #Alternative Hypothesis:          The cdf of 'dat1' does not equal
#'   #                                 the cdf of 'dat2'.
#'
#'   #----------
#'   # Clean up
#'
#'   rm(dat1, dat2)
#' }
#' @rawRd
#' \keyword{classes}
NULL

#' Generalized Pivotal Quantity for Confidence Interval for the Mean of a Normal Distribution Based on Censored Data
#' @name gpqCiNormCensored
#' @aliases gpqCiNormSinglyCensored gpqCiNormMultiplyCensored
#' @description
#' Generate a generalized pivotal quantity (GPQ) for a confidence interval for the
#'   mean of a \link[stats:Normal]{Normal distribution} based on singly or multiply
#'   censored data.
#' @usage
#' gpqCiNormSinglyCensored(n, n.cen, probs, nmc, method = "mle",
#'     censoring.side = "left", seed = NULL, names = TRUE)
#'
#'   gpqCiNormMultiplyCensored(n, cen.index, probs, nmc, method = "mle",
#'     censoring.side = "left", seed = NULL, names = TRUE)
#' @rawRd
#' \arguments{
#'   \item{n}{
#'   positive integer \eqn{\ge 3} indicating the sample size.
#' }
#'   \item{n.cen}{
#'   for the case of singly censored data, a positive integer indicating the number of
#'   censored observations.  The value of \code{n.cen} must be between \code{1} and
#'   \code{n-2}, inclusive.
#' }
#'   \item{cen.index}{
#'   for the case of multiply censored data, a sorted vector of unique integers
#'   indicating the indices of the censored observations when the observations are
#'   \dQuote{ordered}.  The length of \code{cen.index} must be between \code{1} and
#'   \code{n-2}, inclusive, and the values of \code{cen.index} must be between
#'   \code{1} and \code{n}.
#' }
#'   \item{probs}{
#'   numeric vector of values between 0 and 1 indicating the confidence level(s)
#'   associated with the GPQ(s).
#' }
#'   \item{nmc}{
#'   positive integer \eqn{\ge 10} indicating the number of Monte Carlo trials to run
#'   in order to compute the GPQ(s).
#' }
#'   \item{method}{
#'   character string indicating the method to use for parameter estimation.  \cr
#'   \cr
#'   For singly censored data, possible values are \code{"mle"} (the default),
#'   \code{"bcmle"}, \code{"qq.reg"}, \code{"qq.reg.w.cen.level"},
#'   \code{"impute.w.qq.reg"}, \cr
#'   \code{"impute.w.qq.reg.w.cen.level"},
#'   \code{"impute.w.mle"}, \cr
#'   \code{"iterative.impute.w.qq.reg"},
#'   \code{"m.est"}, and \code{"half.cen.level"}.  See the help file for
#'   \code{\link{enormCensored}} for details. \cr
#'   \cr
#'   For multiply censored data, possible values are \code{"mle"} (the default),
#'   \code{"qq.reg"}, \code{"impute.w.qq.reg"}, and \code{"half.cen.level"}.
#'   See the help file for \code{\link{enormCensored}} for details.
#' }
#'   \item{censoring.side}{
#'   character string indicating on which side the censoring occurs.  The possible
#'   values are \code{"left"} (the default) and \code{"right"}.
#' }
#'   \item{seed}{
#'   positive integer to pass to the function \code{\link{set.seed}}.  This argument is
#'   ignored if \code{seed=NULL} (the default).  Using the \code{seed} argument lets you
#'   reproduce the exact same result if all other arguments stay the same.
#' }
#'   \item{names}{
#'   a logical scalar passed to \code{\link{quantile}} indicating whether to add a
#'   names attribute to the resulting GPQ(s).  The default value is \code{names=TRUE}.
#' }
#' }
#' @rawRd
#' \details{
#'   The functions \code{gpqCiNormSinglyCensored} and \code{gpqCiNormMultiplyCensored}
#'   are called by \cr
#'   \code{\link{enormCensored}} when \code{ci.method="gpq"}.  They are
#'   used to construct generalized pivotal quantities to create confidence intervals
#'   for the mean \eqn{\mu} of an assumed normal distribution.
#'
#'   This idea was introduced by Schmee et al. (1985) in the context of Type II singly
#'   censored data.  The function
#'   \code{gpqCiNormSinglyCensored} generates GPQs using a modification of
#'   Algorithm 12.1 of Krishnamoorthy and Mathew (2009, p. 329).  Algorithm 12.1 is
#'   used to generate GPQs for a tolerance interval.  The modified algorithm for
#'   generating GPQs for confidence intervals for the mean \eqn{\mu} is as follows:
#'   \enumerate{
#'     \item Generate a random sample of \eqn{n} observations from a standard normal
#'       (i.e., N(0,1)) distribution and let \eqn{z_{(1)}, z_{(2)}, \ldots, z_{(n)}}
#'       denote the ordered (sorted) observations.
#'     \item Set the smallest \code{n.cen} observations as censored.
#'     \item Compute the estimates of \eqn{\mu} and \eqn{\sigma} by calling
#'       \code{\link{enormCensored}} using the method
#'       specified by the \code{method} argument, and denote these estimates as
#'       \eqn{\hat{\mu}^*, \; \hat{\sigma}^*}.
#'     \item Compute the t-like pivotal quantity
#'       \eqn{\hat{t} = \hat{\mu}^*/\hat{\sigma}^*}.
#'     \item Repeat steps 1-4 \code{nmc} times to produce an empirical distribution of
#'       the t-like pivotal quantity.
#'   }
#'   A two-sided \eqn{(1-\alpha)100\%} confidence interval for \eqn{\mu} is then
#'   computed as:
#'   \deqn{[\hat{\mu} - \hat{t}_{1-(\alpha/2)} \hat{\sigma}, \; \hat{\mu} - \hat{t}_{\alpha/2} \hat{\sigma}]}
#'   where \eqn{\hat{t}_p} denotes the \eqn{p}'th empirical quantile of the
#'   \code{nmc} generated \eqn{\hat{t}} values.
#'
#'   Schmee at al. (1985) derived this method in the context of Type II singly censored
#'   data (for which these limits are exact within Monte Carlo error), but state that
#'   according to Regal (1982) this method produces confidence intervals that are
#'   close apporximations to the correct limits for Type I censored data.
#'
#'   The function
#'   \code{gpqCiNormMultiplyCensored} is an extension of this idea to multiply censored
#'   data.  The algorithm is the same as for singly censored data, except
#'   Step 2 changes to: \cr
#'
#'   2. Set observations as censored for elements of the argument \code{cen.index}
#'      that have the value \code{TRUE}.
#'
#'   The functions \code{gpqCiNormSinglyCensored} and \code{gpqCiNormMultiplyCensored} are
#'   computationally intensive and provided to the user to allow you to create your own
#'   tables.
#' }
#' @rawRd
#' \value{
#'   a numeric vector containing the GPQ(s).
#' }
#' @rawRd
#' \references{
#'   Krishnamoorthy K., and T. Mathew. (2009).
#'   \emph{Statistical Tolerance Regions: Theory, Applications, and Computation}.
#'   John Wiley and Sons, Hoboken.
#'
#'   Regal, R. (1982).  Applying Order Statistic Censored Normal Confidence Intervals
#'   to Time Censored Data.  Unpublished manuscript, University of Minnesota, Duluth,
#'   Department of Mathematical Sciences.
#'
#'   Schmee, J., D.Gladstein, and W. Nelson. (1985).  Confidence Limits for Parameters
#'   of a Normal Distribution from Singly Censored Samples, Using Maximum Likelihood.
#'   \emph{Technometrics} \bold{27}(2) 119--128.
#' }
#' @rawRd
#' \author{
#'     Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \seealso{
#'   \code{\link{enormCensored}}, \code{\link{estimateCensored.object}}.
#' }
#' @rawRd
#' \examples{
#'   # Reproduce the entries for n=10 observations with n.cen=6 in Table 4
#'   # of Schmee et al. (1985, p.122).
#'   #
#'   # Notes:
#'   # 1. This table applies to right-censored data, and the
#'   #    quantity "r" in this table refers to the number of
#'   #    uncensored observations.
#'   #
#'   # 2. Passing a value for the argument "seed" simply allows
#'   #    you to reproduce this example.
#'
#'   # NOTE:  Here to save computing time for the sake of example, we will specify
#'   #        just 100 Monte Carlos, whereas Krishnamoorthy and Mathew (2009)
#'   #        suggest *10,000* Monte Carlos.
#'
#'   # Here are the values given in Schmee et al. (1985):
#'   Schmee.values <- c(-3.59, -2.60, -1.73, -0.24, 0.43, 0.58, 0.73)
#'   probs <- c(0.025, 0.05, 0.1, 0.5, 0.9, 0.95, 0.975)
#'   names(Schmee.values) <- paste(probs * 100, "\%", sep = "")
#'
#'   Schmee.values
#'   # 2.5%    5%   10%   50%   90%   95% 97.5%
#'   #-3.59 -2.60 -1.73 -0.24  0.43  0.58  0.73
#'
#'   gpqs <- gpqCiNormSinglyCensored(n = 10, n.cen = 6, probs = probs,
#'     nmc = 100, censoring.side = "right", seed = 529)
#'
#'   round(gpqs, 2)
#'   # 2.5%    5%   10%   50%   90%   95% 97.5%
#'   #-2.46 -2.03 -1.38 -0.14  0.54  0.65  0.84
#'
#'   # This is what you get if you specify nmc = 1000 with the
#'   # same value for seed:
#'   #-----------------------------------------------
#'   # 2.5%    5%   10%   50%   90%   95% 97.5%
#'   #-3.50 -2.49 -1.67 -0.25  0.41  0.57  0.71
#'
#'
#'   # Clean up
#'   #---------
#'   rm(Schmee.values, probs, gpqs)
#'
#'   #==========
#'
#'   # Example of using gpqCiNormMultiplyCensored
#'   #-------------------------------------------
#'
#'   # Consider the following set of multiply left-censored data:
#'   dat <- 12:16
#'   censored <- c(TRUE, FALSE, TRUE, FALSE, FALSE)
#'
#'   # Since the data are "ordered" we can identify the indices of the
#'   # censored observations in the ordered data as follow:
#'
#'   cen.index <- (1:length(dat))[censored]
#'   cen.index
#'   #[1] 1 3
#'
#'   # Now we can generate a GPQ using gpqCiNormMultiplyCensored.
#'   # Here we'll generate a GPQs to use to create a
#'   # 95% confidence interval for left-censored data.
#'
#'   # NOTE:  Here to save computing time for the sake of example, we will specify
#'   #        just 100 Monte Carlos, whereas Krishnamoorthy and Mathew (2009)
#'   #        suggest *10,000* Monte Carlos.
#'
#'   gpqCiNormMultiplyCensored(n = 5, cen.index = cen.index,
#'     probs = c(0.025, 0.975), nmc = 100, seed = 237)
#'   #     2.5%     97.5%
#'   #-1.315592  1.848513
#'
#'   #----------
#'
#'   # Clean up
#'   #---------
#'   rm(dat, censored, cen.index)
#' }
#' @rawRd
#' \keyword{ distribution }
#' @rawRd
#' \keyword{ htest }
NULL

#' Generalized Pivotal Quantity for Tolerance Interval for a Normal Distribution Based on Censored Data
#' @name gpqTolIntNormCensored
#' @aliases gpqTolIntNormSinglyCensored gpqTolIntNormMultiplyCensored
#' @description
#' Generate a generalized pivotal quantity (GPQ) for a tolerance interval for a Normal
#'   distribution based on singly or multiply censored data.
#' @usage
#' gpqTolIntNormSinglyCensored(n, n.cen, p, probs, nmc, method = "mle",
#'     censoring.side = "left", seed = NULL, names = TRUE)
#'
#'   gpqTolIntNormMultiplyCensored(n, cen.index, p, probs, nmc, method = "mle",
#'     censoring.side = "left", seed = NULL, names = TRUE)
#' @rawRd
#' \arguments{
#'   \item{n}{
#'   positive integer \eqn{\ge 3} indicating the sample size.
#' }
#'   \item{n.cen}{
#'   for the case of singly censored data, a positive integer indicating the number of
#'   censored observations.  The value of \code{n.cen} must be between \code{1} and
#'   \code{n-2}, inclusive.
#' }
#'   \item{cen.index}{
#'   for the case of multiply censored data, a sorted vector of unique integers indicating the
#'   indices of the censored observations when the observations are \dQuote{ordered}.
#'   The length of \code{cen.index} must be between \code{1} and \code{n-2}, inclusive, and
#'   the values of \code{cen.index} must be between \code{1} and \code{n}.
#' }
#'   \item{p}{
#'   numeric scalar strictly greater than 0 and strictly less than 1 indicating the quantile
#'   for which to generate the GPQ(s) (i.e., the coverage associated with a one-sided
#'   tolerance interval).
#' }
#'   \item{probs}{
#'   numeric vector of values between 0 and 1 indicating the confidence level(s) associated
#'   with the GPQ(s).
#' }
#'   \item{nmc}{
#'   positive integer \eqn{\ge 10} indicating the number of Monte Carlo trials to run in order
#'   to compute the GPQ(s).
#' }
#'   \item{method}{
#'   character string indicating the method to use for parameter estimation.  \cr
#'   \cr
#'   For singly censored data, possible values are \code{"mle"} (the default), \code{"bcmle"},
#'   \code{"qq.reg"}, \code{"qq.reg.w.cen.level"}, \code{"impute.w.qq.reg"}, \cr
#'   \code{"impute.w.qq.reg.w.cen.level"}, \code{"impute.w.mle"}, \cr
#'   \code{"iterative.impute.w.qq.reg"},
#'   \code{"m.est"}, and \code{"half.cen.level"}.  See the help file for \code{\link{enormCensored}}
#'   for details. \cr
#'   \cr
#'   For multiply censored data, possible values are \code{"mle"} (the default), \code{"qq.reg"},
#'   \code{"impute.w.qq.reg"}, and \code{"half.cen.level"}.  See the help file for \cr
#'   \code{\link{enormCensored}} for details.
#' }
#'   \item{censoring.side}{
#'   character string indicating on which side the censoring occurs.  The possible values are
#'   \code{"left"} (the default) and \code{"right"}.
#' }
#'   \item{seed}{
#'   positive integer to pass to the function \code{\link{set.seed}}.  This argument is
#'   ignored if \code{seed=NULL} (the default).  Using the \code{seed} argument lets you
#'   reproduce the exact same result if all other arguments stay the same.
#' }
#'   \item{names}{
#'   a logical scalar passed to \code{\link{quantile}} indicating whether to add a
#'   names attribute to the resulting GPQ(s).  The default value is \code{names=TRUE}.
#' }
#' }
#' @rawRd
#' \details{
#'   The function \code{gpqTolIntNormSinglyCensored} generates GPQs as described in Algorithm 12.1
#'   of Krishnamoorthy and Mathew (2009, p. 329).  The function
#'   \code{gpqTolIntNormMultiplyCensored} is an extension of this idea to multiply censored data.
#'   These functions are called by \cr
#'   \code{\link{tolIntNormCensored}} when \code{ti.method="gpq"},
#'   and also by \code{\link{eqnormCensored}} when \code{ci=TRUE} and \code{ci.method="gpq"}.  See
#'   the help files for these functions for an explanation of GPQs.
#'
#'   Note that technically these are only GPQs if the data are Type II censored.  However,
#'   Krishnamoorthy and Mathew (2009, p. 328) state that in the case of Type I censored data these
#'   quantities should approximate the true GPQs and the results appear to be satisfactory, even
#'   for small sample sizes.
#'
#'   The functions \code{gpqTolIntNormSinglyCensored} and \code{gpqTolIntNormMultiplyCensored} are
#'   computationally intensive and provided to the user to allow you to create your own tables.
#' }
#' @rawRd
#' \value{
#'   a numeric vector containing the GPQ(s).
#' }
#' @rawRd
#' \references{
#'   Krishnamoorthy K., and T. Mathew. (2009).
#'   \emph{Statistical Tolerance Regions: Theory, Applications, and Computation}.
#'   John Wiley and Sons, Hoboken.
#' }
#' @rawRd
#' \author{
#'     Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \note{
#'   Tolerance intervals have long been applied to quality control and
#'   life testing problems (Hahn, 1970b,c; Hahn and Meeker, 1991; Krishnamoorthy and Mathew, 2009).
#'   References that discuss tolerance intervals in the context of environmental monitoring include:
#'   Berthouex and Brown (2002, Chapter 21), Gibbons et al. (2009),
#'   Millard and Neerchal (2001, Chapter 6), Singh et al. (2010b), and USEPA (2009).
#' }
#' @rawRd
#' \seealso{
#'   \code{\link{tolIntNormCensored}}, \code{\link{eqnormCensored}},
#'   \code{\link{enormCensored}}, \code{\link{estimateCensored.object}}.
#' }
#' @rawRd
#' \examples{
#'   # Reproduce the entries for n=10 observations with n.cen=1 in Table 12.2
#'   # of Krishnamoorthy and Mathew (2009, p.331).
#'   #
#'   # (Note: passing a value for the argument "seed" simply allows you to
#'   # reproduce this example.)
#'   #
#'   # NOTE:  Here to save computing time for the sake of example, we will specify
#'   #        just 100 Monte Carlos, whereas Krishnamoorthy and Mathew (2009)
#'   #        suggest *10,000* Monte Carlos.
#'
#'   gpqTolIntNormSinglyCensored(n = 10, n.cen = 1, p = 0.05, probs = 0.05,
#'     nmc = 100, seed = 529)
#'   #       5%
#'   #-3.483403
#'
#'
#'   gpqTolIntNormSinglyCensored(n = 10, n.cen = 1, p = 0.1, probs = 0.05,
#'     nmc = 100, seed = 497)
#'   #      5%
#'   #-2.66705
#'
#'
#'   gpqTolIntNormSinglyCensored(n = 10, n.cen = 1, p = 0.9, probs = 0.95,
#'     nmc = 100, seed = 623)
#'   #     95%
#'   #2.478654
#'
#'   gpqTolIntNormSinglyCensored(n = 10, n.cen = 1, p = 0.95, probs = 0.95,
#'     nmc = 100, seed = 623)
#'   #     95%
#'   #3.108452
#'
#'   #==========
#'
#'   # Example of using gpqTolIntNormMultiplyCensored
#'   #-----------------------------------------------
#'
#'   # Consider the following set of multiply left-censored data:
#'   dat <- 12:16
#'   censored <- c(TRUE, FALSE, TRUE, FALSE, FALSE)
#'
#'   # Since the data are "ordered" we can identify the indices of the
#'   # censored observations in the ordered data as follow:
#'
#'   cen.index <- (1:length(dat))[censored]
#'   cen.index
#'   #[1] 1 3
#'
#'   # Now we can generate a GPQ using gpqTolIntNormMultiplyCensored.
#'   # Here we'll generate a GPQ corresponding to an upper tolerance
#'   # interval with coverage 90% with 95% confidence for
#'   # left-censored data.
#'   # NOTE:  Here to save computing time for the sake of example, we will specify
#'   #        just 100 Monte Carlos, whereas Krishnamoorthy and Mathew (2009)
#'   #        suggest *10,000* Monte Carlos.
#'
#'   gpqTolIntNormMultiplyCensored(n = 5, cen.index = cen.index, p = 0.9,
#'     probs = 0.95, nmc = 100, seed = 237)
#'   #     95%
#'   #3.952052
#'
#'   #==========
#'
#'   # Clean up
#'   #---------
#'   rm(dat, censored, cen.index)
#' }
#' @rawRd
#' \keyword{ distribution }
#' @rawRd
#' \keyword{ htest }
NULL

#' S3 Classes "htest" and "htestEnvStats"
#' @name htest.htestEnvStats.object
#' @aliases htest.object htestEnvStats.object
#' @description
#' These classes of objects are returned by functions that perform hypothesis tests
#'   (e.g., the \R function \code{\link{t.test}} returns an object of class \code{"htest"},
#'   while the \pkg{EnvStats} functions \code{\link{quantileTest}} and
#'   \code{\link{kendallSeasonalTrendTest}} return objects of class \code{"htestEnvStats"}).
#'   Objects of class \code{"htest"} and \code{"htestEnvStats"} are lists that contain
#'   information about the null and alternative hypotheses, the estimated distribution
#'   parameters, the test statistic, the p-value, and (optionally) confidence intervals
#'   for distribution parameters.
#' @rawRd
#' \docType{class}
#' @rawRd
#' \details{
#'   Objects of S3 class \code{"htestEnvStats"} returned by \pkg{EnvStats} functions
#'   that perform hypothesis tests include: \cr
#'   \code{\link{chenTTest}} \cr
#'   \code{\link{kendallTrendTest}} \cr
#'   \code{\link{kendallSeasonalTrendTest}} \cr
#'   \code{\link{quantileTest}} \cr
#'   \code{\link{serialCorrelationTest}} \cr
#'   \code{\link{signTest}} \cr
#'   \code{\link{twoSampleLinearRankTest}} \cr
#'   \code{\link{varTest}} \cr
#'   \code{\link{varGroupTest}} \cr
#'   \code{\link{zTestGevdShape}}
#'
#'   These functions are listed in the help file \link[=FcnsByCatHypothTests]{Hypothesis Tests},
#'   along with other functions that perform hypothesis tests that return objects of different classes
#'   (e.g., the function \code{link{oneSamplePermutationTest}} returns an object of class
#'   \code{\link[=permutationTest.object]{"permutationTest"}}).
#'
#'   Note that functions that perform \link[=FcnsByCatGOFTests]{goodness-of-fit tests}
#'   return objects of class \code{\link[=gof.object]{"gof"}} or
#'   \code{\link[=gofTwoSample.object]{"gofTwoSample"}}.
#'
#'   Objects of class \code{"htestEnvStats"} generated by \pkg{EnvStats} functions may
#'   contain additional components called
#'   \code{estimation.method} (method used to estimate the population parameter(s)),
#'   \code{sample.size}, and
#'   \code{bad.obs} (number of missing (\code{NA}), undefined (\code{NaN}), or infinite
#'   (\code{Inf}, \code{-Inf}) values removed prior to performing the hypothesis test),
#'   and \code{interval} (a list with information about a confidence, prediction, or
#'   tolerance interval).
#' }
#' @rawRd
#' \section{Methods}{
#'   Generic functions that have methods for objects of class
#'   \code{"htestEnvStats"} include: \cr
#'   \code{\link[base:print]{print}}. Objects of class \code{"htest"}
#'   use the standard \R printing methods.
#' }
#' @rawRd
#' \value{
#'   \strong{Required Components} \cr
#'   The following components must be included in a legitimate list of
#'   class \code{"htest"} or \code{"htestEnvStats"}.
#'
#'   \item{null.value}{
#'   numeric vector containing the value(s) of the population parameter(s) specified by
#'   the null hypothesis.  This vector has a \code{names} attribute describing its
#'   elements.
#' }
#'   \item{alternative}{
#'   character string indicating the alternative hypothesis (the value of the input
#'   argument \code{alternative}).  Possible values are \code{"greater"}, \code{"less"},
#'   or \code{"two-sided"}.
#' }
#'   \item{method}{
#'   character string giving the name of the test used.
#' }
#'   \item{estimate}{
#'   numeric vector containing the value(s) of the estimated population parameter(s)
#'   involved in the null hypothesis.  This vector has a \code{names} attribute
#'   describing its element(s).
#' }
#'   \item{data.name}{
#'   character string containing the actual name(s) of the input data.
#' }
#'   \item{statistic}{
#'   numeric scalar containing the value of the test statistic, with a
#'   \code{names} attribute indicating the null distribution.
#' }
#'   \item{parameters}{
#'   numeric vector containing the parameter(s) associated with the null distribution of
#'   the test statistic.  This vector has a \code{names} attribute describing its
#'   element(s).
#' }
#'   \item{p.value}{
#'   numeric scalar containing the p-value for the test under the null hypothesis.
#' }
#'
#'   \strong{Optional Components} \cr
#'   The following component may optionally be included in an object of
#'   of class \code{"htest"} generated by \R functions that test hypotheses:
#'
#'   \item{conf.int}{
#'   numeric vector of length 2 containing lower and upper confidence limits for the
#'   estimated population parameter.  This vector has an attribute called
#'   \code{"conf.level"} that is a numeric scalar indicating the confidence level
#'   associated with the confidence interval.
#' }
#'
#'   The following components may be included in objects of class \code{"htestEnvStats"}
#'   generated by \pkg{EnvStats} functions:
#'
#'   \item{sample.size}{
#'   numeric scalar containing the number of non-missing observations in the sample used
#'   for the hypothesis test.
#' }
#'   \item{estimation.method}{
#'   character string containing the method used to compute the estimated distribution
#'   parameter(s).  The value of this component will depend on the available estimation
#'   methods (see \code{\link{Distribution.df}}).
#' }
#'   \item{bad.obs}{
#'   the number of missing (\code{NA}), undefined (\code{NaN}) and/or infinite
#'   (\code{Inf}, \code{-Inf}) values that were removed from the data object prior to
#'   performing the hypothesis test.
#' }
#'   \item{interval}{
#'   a list containing information about a confidence, prediction, or tolerance interval.
#' }
#' }
#' @rawRd
#' \note{
#'   Since objects of class \code{"htest"} and \code{"htestEnvStats"} are lists, you may extract
#'   their components with the \code{$} and \code{[[} operators.
#' }
#' @rawRd
#' \author{
#'   Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \seealso{
#'   \code{\link{print.htestEnvStats}}, \link{Hypothesis Tests}.
#' }
#' @rawRd
#' \examples{
#'   # Create an object of class "htestEnvStats", then print it out.
#'   #--------------------------------------------------------------
#'
#'   htestEnvStats.obj <- chenTTest(EPA.02d.Ex.9.mg.per.L.vec, mu = 30)
#'
#'   mode(htestEnvStats.obj)
#'   #[1] "list"
#'
#'   class(htestEnvStats.obj)
#'   #[1] "htestEnvStats"
#'
#'   names(htestEnvStats.obj)
#'   # [1] "statistic"   "parameters"  "p.value"     "estimate"
#'   # [5] "null.value"  "alternative" "method"      "sample.size"
#'   # [9] "data.name"   "bad.obs"     "interval"
#'
#'   htestEnvStats.obj
#'
#'   #Results of Hypothesis Test
#'   #--------------------------
#'   #
#'   #Null Hypothesis:                 mean = 30
#'   #
#'   #Alternative Hypothesis:          True mean is greater than 30
#'   #
#'   #Test Name:                       One-sample t-Test
#'   #                                 Modified for
#'   #                                 Positively-Skewed Distributions
#'   #                                 (Chen, 1995)
#'   #
#'   #Estimated Parameter(s):          mean = 34.566667
#'   #                                 sd   = 27.330598
#'   #                                 skew =  2.365778
#'   #
#'   #Data:                            EPA.02d.Ex.9.mg.per.L.vec
#'   #
#'   #Sample Size:                     60
#'   #
#'   #Test Statistic:                  t = 1.574075
#'   #
#'   #Test Statistic Parameter:        df = 59
#'   #
#'   #P-values:                        z               = 0.05773508
#'   #                                 t               = 0.06040889
#'   #                                 Avg. of z and t = 0.05907199
#'   #
#'   #Confidence Interval for:         mean
#'   #
#'   #Confidence Interval Method:      Based on z
#'   #
#'   #Confidence Interval Type:        Lower
#'   #
#'   #Confidence Level:                95%
#'   #
#'   #Confidence Interval:             LCL = 29.82
#'   #                                 UCL =   Inf
#'
#'   #==========
#'
#'   # Extract the test statistic
#'   #---------------------------
#'
#'   htestEnvStats.obj$statistic
#'   #       t
#'   #1.574075
#'
#'   #==========
#'
#'   # Clean up
#'   #---------
#'   rm(htestEnvStats.obj)
#' }
#' @rawRd
#' \keyword{classes}
NULL

#' S3 Class "htestCensored"
#' @name htestCensored.object
#' @description
#' This class of objects is returned by \pkg{EnvStats} functions that perform
#'   hypothesis tests based on \bold{\emph{censored}} data.
#'   Objects of class \code{"htestCensored"} are lists that contain information about
#'   the null and alternative hypotheses, the censoring side, the censoring levels,
#'   the percentage of observations that are censored,
#'   the estimated distribution parameters (if applicable), the test statistic,
#'   the p-value, and (optionally, if applicable)
#'   confidence intervals for distribution parameters.
#' @rawRd
#' \docType{class}
#' @rawRd
#' \details{
#'   Objects of S3 class \code{"htestCensored"} are returned by
#'   the functions listed in the section \bold{Hypothesis Tests}
#'   in the help file
#'   \link[=FcnsByCatCensoredData]{EnvStats Functions for Censored Data}.
#'   Currently, the only function listed is
#'   \code{\link{twoSampleLinearRankTestCensored}}.
#' }
#' @rawRd
#' \section{Methods}{
#'   Generic functions that have methods for objects of class
#'   \code{"htestCensored"} include: \cr
#'   \code{\link{print}}.
#' }
#' @rawRd
#' \value{
#'   \strong{Required Components} \cr
#'   The following components must be included in a legitimate list of
#'   class \code{"htestCensored"}.
#'
#'   \item{statistic}{
#'   numeric scalar containing the value of the test statistic, with a
#'   \code{names} attribute indicating the null distribution.
#' }
#'   \item{parameters}{
#'   numeric vector containing the parameter(s) associated with the null distribution of
#'   the test statistic.  This vector has a \code{names} attribute describing its
#'   element(s).
#' }
#'   \item{p.value}{
#'   numeric scalar containing the p-value for the test under the null hypothesis.
#' }
#'   \item{null.value}{
#'   numeric vector containing the value(s) of the population parameter(s) specified by
#'   the null hypothesis.  This vector has a \code{names} attribute describing its
#'   elements.
#' }
#'   \item{alternative}{
#'   character string indicating the alternative hypothesis (the value of the input
#'   argument \code{alternative}).  Possible values are \code{"greater"}, \code{"less"},
#'   or \code{"two-sided"}.
#' }
#'   \item{method}{
#'   character string giving the name of the test used.
#' }
#'   \item{sample.size}{
#'   numeric scalar containing the number of non-missing observations in the sample used
#'   for the hypothesis test.
#' }
#'   \item{data.name}{
#'   character string containing the actual name(s) of the input data.
#' }
#'   \item{bad.obs}{
#'   the number of missing (\code{NA}), undefined (\code{NaN}) and/or infinite
#'   (\code{Inf}, \code{-Inf}) values that were removed from the data object prior to
#'   performing the hypothesis test.
#' }
#'   \item{censoring.side}{character string indicating whether the data are
#'     left- or right-censored.
#' }
#'   \item{censoring.name}{character string indicating the name of the data object
#'     used to identify which values are censored.
#' }
#'   \item{censoring.levels}{numeric scalar or vector indicating the censoring level(s).
#' }
#'   \item{percent.censored}{numeric scalar indicating the percent of non-missing
#'     observations that are censored.
#' }
#'
#'   \strong{Optional Components} \cr
#'   The following component may optionally be included in an object of
#'   of class \code{"htestCensored"}:
#'
#'   \item{estimate}{
#'   numeric vector containing the value(s) of the estimated population parameter(s)
#'   involved in the null hypothesis.  This vector has a \code{names} attribute
#'   describing its element(s).
#' }
#'   \item{estimation.method}{
#'   character string containing the method used to compute the estimated distribution
#'   parameter(s).  The value of this component will depend on the available estimation
#'   methods (see \code{\link{Distribution.df}}).
#' }
#'   \item{interval}{
#'   a list containing information about a confidence, prediction, or tolerance interval.
#' }
#' }
#' @rawRd
#' \note{
#'   Since objects of class \code{"htestCensored"} are lists, you may extract
#'   their components with the \code{$} and \code{[[} operators.
#' }
#' @rawRd
#' \author{
#'   Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \seealso{
#'   \code{\link{print.htestCensored}}, \link{Censored Data}.
#' }
#' @rawRd
#' \examples{
#'   # Create an object of class "htestCensored", then print it out.
#'   #--------------------------------------------------------------
#'
#'   htestCensored.obj <-   with(EPA.09.Ex.16.5.PCE.df,
#'     twoSampleLinearRankTestCensored(
#'       x = PCE.ppb[Well.type == "Compliance"],
#'       x.censored = Censored[Well.type == "Compliance"],
#'       y = PCE.ppb[Well.type == "Background"],
#'       y.censored = Censored[Well.type == "Background"],
#'       test = "tarone-ware", alternative = "greater"))
#'
#'   mode(htestCensored.obj)
#'   #[1] "list"
#'
#'   class(htestCensored.obj)
#'   #[1] "htest"
#'
#'   names(htestCensored.obj)
#'   # [1] "statistic"         "parameters"        "p.value"
#'   # [4] "estimate"          "null.value"        "alternative"
#'   # [7] "method"            "estimation.method" "sample.size"
#'   #[10] "data.name"         "bad.obs"           "censoring.side"
#'   #[13] "censoring.name"    "censoring.levels"  "percent.censored"
#'
#'   htestCensored.obj
#'
#'   #Results of Hypothesis Test
#'   #Based on Censored Data
#'   #--------------------------
#'   #
#'   #Null Hypothesis:                 Fy(t) = Fx(t)
#'   #
#'   #Alternative Hypothesis:          Fy(t) > Fx(t) for at least one t
#'   #
#'   #Test Name:                       Two-Sample Linear Rank Test:
#'   #                                 Tarone-Ware Test
#'   #                                 with Hypergeometric Variance
#'   #
#'   #Censoring Side:                  left
#'   #
#'   #Data:                            x = PCE.ppb[Well.type == "Compliance"]
#'   #                                 y = PCE.ppb[Well.type == "Background"]
#'   #
#'   #Censoring Variable:              x = Censored[Well.type == "Compliance"]
#'   #                                 y = Censored[Well.type == "Background"]
#'   #
#'   #Sample Sizes:                    nx = 8
#'   #                                 ny = 6
#'   #
#'   #Percent Censored:                x = 12.5%
#'   #                                 y = 50.0%
#'   #
#'   #Test Statistics:                 nu     =  8.458912
#'   #                                 var.nu = 20.912407
#'   #                                 z      =  1.849748
#'   #
#'   #P-value:                         0.03217495
#'
#'   #==========
#'
#'   # Extract the test statistics
#'   #----------------------------
#'
#'   htestCensored.obj$statistic
#'   #       nu    var.nu         z
#'   # 8.458912 20.912407  1.849748
#'
#'   #==========
#'
#'   # Clean up
#'   #---------
#'   rm(htestCensored.obj)
#' }
#' @rawRd
#' \keyword{classes}
NULL

#' S3 Class "permutationTest"
#' @name permutationTest.object
#' @description
#' This class of objects is returned by functions that perform permutation tests.
#'   Objects of class \code{"permutationTest"} are lists that contain information about
#'   the null and alternative hypotheses, the estimated distribution parameters, the
#'   test statistic and the p-value.  They also contain the permutation distribution
#'   of the statistic (or a sample of the permutation distribution).
#' @rawRd
#' \docType{class}
#' @rawRd
#' \details{
#'   Objects of S3 class \code{"permutationTest"} are returned by any of the
#'   \pkg{EnvStats} functions that perform permutation tests.  Currently, these are:
#'   \code{\link{oneSamplePermutationTest}},  \cr
#'   \code{\link{twoSamplePermutationTestLocation}}, and
#'   \code{\link{twoSamplePermutationTestProportion}}.
#' }
#' @rawRd
#' \section{Methods}{
#'   Generic functions that have methods for objects of class
#'   \code{"permutationTest"} include: \cr
#'   \code{\link{print}}, \code{\link{plot}}.
#' }
#' @rawRd
#' \value{
#'   A legitimate list of class \code{"permutationTest"} includes the components
#'   listed in the help file for \code{\link{htest.object}}.  In addition, the following
#'   components must be included in a legitimate list of class \code{"permutationTest"}:
#'
#'   \strong{Required Components} \cr
#'   The following components must be included in a legitimate list of
#'   class \code{"permutationTest"}.
#'
#'   \item{stat.dist}{
#'   numeric vector containing values of the statistic for the permutation distribution.
#'   When \code{exact=FALSE}, the vector is comprised of values sampled from the full
#'   permutation distribution.
#' }
#'   \item{exact}{
#'   logical scalar indicating whether the exact permutation distribution was used for
#'   the test (\code{exact=TRUE}), or if instead the permutation distribution was
#'   sampled (\code{exact=FALSE}).
#' }
#'
#'   \strong{Optional Components} \cr
#'   The following component may optionally be included in an object of
#'   of class \code{"permutationTest"}:
#'
#'   \item{seed}{
#'   integer or vector of integers indicating the seed that was used for sampling the
#'   permutation distribution.  This component is present only if \code{exact=FALSE}.
#' }
#'   \item{prob.stat.dist}{
#'   numeric vector containing the probabilities associated with each element of
#'   the component \code{stat.dist}.  This component is only returned by the
#'   function \code{\link{twoSamplePermutationTestProportion}}.
#' }
#' }
#' @rawRd
#' \note{
#'   Since objects of class \code{"permutationTest"} are lists, you may extract
#'   their components with the \code{$} and \code{[[} operators.
#' }
#' @rawRd
#' \author{
#'   Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \seealso{
#'   \code{\link{print.permutationTest}}, \code{\link{plot.permutationTest}},
#'   \code{\link{oneSamplePermutationTest}},  \cr
#'   \code{\link{twoSamplePermutationTestLocation}},
#'   \code{\link{twoSamplePermutationTestProportion}}, \link{Hypothesis Tests}.
#' }
#' @rawRd
#' \examples{
#'   # Create an object of class "permutationTest", then print it and plot it.
#'   #------------------------------------------------------------------------
#'
#'   set.seed(23)
#'
#'   dat <- rlogis(10, location = 7, scale = 2)
#'
#'   permutationTest.obj <- oneSamplePermutationTest(dat, mu = 5,
#'     alternative = "greater", exact = TRUE)
#'
#'   mode(permutationTest.obj)
#'   #[1] "list"
#'
#'   class(permutationTest.obj)
#'   #[1] "permutationTest"
#'
#'   names(permutationTest.obj)
#'   # [1] "statistic"         "parameters"        "p.value"
#'   # [4] "estimate"          "null.value"        "alternative"
#'   # [7] "method"            "estimation.method" "sample.size"
#'   #[10] "data.name"         "bad.obs"           "stat.dist"
#'   #[13] "exact"
#'
#'   #==========
#'
#'   # Print the results of the test
#'   #------------------------------
#'   permutationTest.obj
#'
#'   #Results of Hypothesis Test
#'   #--------------------------
#'   #
#'   #Null Hypothesis:                 Mean (Median) = 5
#'   #
#'   #Alternative Hypothesis:          True Mean (Median) is greater than 5
#'   #
#'   #Test Name:                       One-Sample Permutation Test
#'   #                                 (Exact)
#'   #
#'   #Estimated Parameter(s):          Mean = 9.977294
#'   #
#'   #Data:                            dat
#'   #
#'   #Sample Size:                     10
#'   #
#'   #Test Statistic:                  Sum(x - 5) = 49.77294
#'   #
#'   #P-value:                         0.001953125
#'
#'   #==========
#'
#'   # Plot the results of the test
#'   #-----------------------------
#'   dev.new()
#'   plot(permutationTest.obj)
#'
#'   #==========
#'
#'   # Extract the test statistic
#'   #---------------------------
#'
#'   permutationTest.obj$statistic
#'   #Sum(x - 5)
#'   #  49.77294
#'
#'   #==========
#'
#'   # Clean up
#'   #---------
#'   rm(permutationTest.obj)
#'   graphics.off()
#' }
#' @rawRd
#' \keyword{classes}
NULL

#' Print Output of EnvStats Hypothesis Tests
#' @name print.htestEnvStats
#' @aliases print.htestEnvStats
#' @description
#' Print objects of class \code{"htestEnvStats"} by a simple
#'   \code{\link[base:print]{print}} method.
#'
#'   The function \code{print.htestEnvStats} formats and prints the results of
#'   performing a hypothesis test using one of the functions listed in the help
#'   file for \code{\link{htestEnvStats.object}}. This method is automatically
#'   called by the standard \R \code{\link[base:print]{print}} generic when given
#'   an object of class \code{"\link[=htestEnvStats.object]{htestEnvStats}"}.
#' @usage
#' \method{print}{htestEnvStats}(x, ...)
#' @rawRd
#' \arguments{
#'   \item{x}{
#'   an object of class \code{"htestEnvStats"}. See
#'   \code{\link{htestEnvStats.object}} for details.
#' }
#'   \item{\dots}{
#'   arguments that can be supplied to the \code{\link[base]{format}} function.
#' }
#' }
#' @rawRd
#' \details{
#'   This method prints null and alternative hypotheses, name of the test,
#'   estimated population parameter(s) involved in the null hypothesis, estimation
#'   method (if present), data name, sample size (if present), number of missing
#'   observations removed prior to performing the test (if present), value of the
#'   test statistic, parameters associated with the null distribution of the test
#'   statistic, p-value associated with the test statistic, and confidence
#'   interval for the population parameter (if present).
#' }
#' @rawRd
#' \value{
#'   Invisibly returns the input \code{x}.
#' }
#' @rawRd
#' \references{
#'   Chambers, J. M. and Hastie, T. J. (1992).  \emph{Statistical Models in S}.
#'   Wadsworth & Brooks/Cole.
#' }
#' @rawRd
#' \author{
#'   Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \seealso{
#'   \link{Hypothesis Tests}, \code{\link{htestEnvStats.object}},
#'   \code{\link[base:print]{print}}.
#' }
#' @rawRd
#' \examples{
#'   # Create an object of class "htestEnvStats", then print it out.
#'   #--------------------------------------------------------------
#'   htestEnvStats.obj <- chenTTest(EPA.02d.Ex.9.mg.per.L.vec, mu = 30)
#'
#'   mode(htestEnvStats.obj)
#'   #[1] "list"
#'
#'   class(htestEnvStats.obj)
#'   #[1] "htestEnvStats"
#'
#'   names(htestEnvStats.obj)
#'   # [1] "statistic"   "parameters"  "p.value"     "estimate"
#'   # [5] "null.value"  "alternative" "method"      "sample.size"
#'   # [9] "data.name"   "bad.obs"     "interval"
#'
#'   htestEnvStats.obj
#'
#'   #==========
#'
#'   # Extract the test statistic
#'   #---------------------------
#'
#'   htestEnvStats.obj$statistic
#'
#'   #==========
#'
#'   # Clean up
#'   #---------
#'   rm(htestEnvStats.obj)
#' }
#' @rawRd
#' \keyword{print}
NULL

#' S3 Class "summaryStats"
#' @name summaryStats.object
#' @description
#' Objects of S3 class \code{"summaryStats"} are returned by the functions
#'   \code{\link{summaryStats}} and \code{\link{summaryFull}}.
#' @rawRd
#' \docType{class}
#' @rawRd
#' \details{
#'   Objects of S3 class \code{"summaryStats"} are matrices that contain
#'   information about the summary statistics.
#' }
#' @rawRd
#' \section{Methods}{
#'   Generic functions that have methods for objects of class
#'   \code{"summaryStats"} include: \cr
#'   \code{\link{print}}.
#' }
#' @rawRd
#' \value{
#'   \strong{Required Attributes} \cr
#'   The following attributes must be included in a legitimate matrix of
#'   class \code{"summaryStats"}.
#'
#'   \item{stats.in.rows}{logical scalar indicating whether the statistics
#'     are stored by row \cr
#'     (\code{stats.in.rows=TRUE}) or by column
#'     (\code{stats.in.rows=FALSE}).}
#'   \item{drop0trailing}{logical scalar indicating whether to drop trailing 0's
#'     when printing the summary statistics.}
#' }
#' @rawRd
#' \author{
#'   Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \seealso{
#'   \code{\link{summaryStats}}, \code{\link{summaryFull}}.
#' }
#' @rawRd
#' \examples{
#'   # Create an object of class "summaryStats", then print it out.
#'   #-------------------------------------------------------------
#'
#'   summaryStats.obj <- summaryStats(TCE.mg.per.L ~ Well,
#'     data = EPA.09.Table.9.1.TCE.df, digits = 3)
#'
#'   is.matrix(summaryStats.obj)
#'   #[1] TRUE
#'
#'   class(summaryStats.obj)
#'   #[1] "summaryStats"
#'
#'   attributes(summaryStats.obj)
#'   #$dim
#'   #[1] 2 8
#'   #
#'   #$dimnames
#'   #$dimnames[[1]]
#'   #[1] "Well.1" "Well.2"
#'   #
#'   #$dimnames[[2]]
#'   #[1] "N"       "Mean"    "SD"      "Median"  "Min"     "Max"
#'   #[7] "NA's"    "N.Total"
#'   #
#'   #
#'   #$class
#'   #[1] "summaryStats"
#'   #
#'   #$stats.in.rows
#'   #[1] FALSE
#'   #
#'   #$drop0trailing
#'   #[1] TRUE
#'
#'   summaryStats.obj
#'   #        N  Mean    SD Median   Min  Max NA's N.Total
#'   #Well.1 14 0.063 0.079  0.031 0.004 0.25    1      15
#'   #Well.2 13 0.118 0.020  0.110 0.099 0.17    2      15
#'
#'   #----------
#'
#'   # Clean up
#'   #---------
#'   rm(summaryStats.obj)
#' }
#' @rawRd
#' \keyword{classes}
NULL
