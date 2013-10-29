\name{FcnsByCatProbDists}
\alias{FcnsByCatProbDists}
\alias{Probability Distributions}
\alias{Probability Distributions and Random Numbers}
\title{
  EnvStats Probability Distributions and Random Numbers
}
\description{
  Listed below are all of the probability distributions available in \R and \pkg{EnvStats}.  
  Distributions with a description in \bold{bold} are new ones that are part of \pkg{EnvStats}.  
  For each distribution, there are functions for generating:  values for the probability 
  density function, values for the cumulative distribution function, quantiles, and 
  random numbers.

  The data frame \code{\link{Distribution.df}} contains information about 
  all of these probability distributions.
}
\details{
  \tabular{ll}{
  \emph{Distribution Abbreviation}        \tab \emph{Description} \cr
  \code{\link[stats:Beta]{beta}}          \tab Beta distribution. \cr
  \code{\link[stats:Binomial]{binom}}     \tab Binomial distribution. \cr
  \code{\link[stats:Cauchy]{cauchy}}      \tab Cauchy distribution. \cr
  \code{\link[=Chi]{chi}}                 \tab \bold{Chi distribution}. \cr
  \code{\link[stats:Chisquare]{chisq}}    \tab Chi-squared distribution. \cr
  \code{\link[stats:Exponential]{exp}}    \tab Exponential distribution. \cr
  \code{\link[=EVD]{evd}}                 \tab \bold{Extreme value distribution}. \cr
  \code{\link[stats:FDist]{f}}            \tab F-distribution. \cr
  \code{\link[stats:GammaDist]{gamma}}    \tab Gamma distribution.\cr
  \code{\link[=GammaAlt]{gammAlt}}        \tab \bold{Gamma distribution parameterized with mean and CV}. \cr
  \code{\link[=GEVD]{gevd}}               \tab \bold{Generalized extreme value distribution}. \cr
  \code{\link[stats:Geometric]{geom}}     \tab Geometric distribution. \cr
  \code{\link[stats:Hypergeometric]{hyper}} \tab Hypergeometric distribution. \cr
  \code{\link[stats:Logistic]{logis}}     \tab Logistic distribution. \cr
  \code{\link[stats:Lognormal]{lnorm}}    \tab Lognormal distribution. \cr
  \code{\link[=LognormalAlt]{lnormAlt}}   \tab \bold{Lognormal distribution parameterized with mean and CV}. \cr
  \code{\link[=LognormalMix]{lnormMix}}   \tab \bold{Mixture of two lognormal distributions}. \cr
  \code{\link[=LognormalMixAlt]{lnormMixAlt}} \tab \bold{Mixture of two lognormal distributions} \cr
                                          \tab \bold{parameterized by their means and CVs}. \cr
  \code{\link[=Lognormal3]{lnorm3}}       \tab \bold{Three-parameter lognormal distribution}. \cr
  \code{\link[=LognormalTrunc]{lnormTrunc}} \tab \bold{Truncated lognormal distribution}. \cr
  \code{\link[=LognormalTruncAlt]{lnormTruncAlt}} \tab \bold{Truncated lognormal distribution} \cr
                                          \tab \bold{parameterized by mean and CV}. \cr
  \code{\link[stats:NegBinomial]{nbinom}} \tab Negative binomial distribution. \cr
  \code{\link[stats:Normal]{norm}}        \tab Normal distribution. \cr
  \code{\link[=NormalMix]{normMix}}       \tab \bold{Mixture of two normal distributions}. \cr
  \code{\link[=NormalTrunc]{normTrunc}}   \tab \bold{Truncated normal distribution}. \cr
  \code{\link[=Pareto]{pareto}}           \tab \bold{Pareto distribution}. \cr
  \code{\link[stats:Poisson]{pois}}       \tab Poisson distribution. \cr
  \code{\link[stats:TDist]{t}}            \tab Student's t-distribution. \cr
  \code{\link[=Triangular]{tri}}          \tab \bold{Triangular distribution}. \cr
  \code{\link[stats:Uniform]{unif}}       \tab Uniform distribution. \cr
  \code{\link[stats:Weibull]{weibull}}    \tab Weibull distribution. \cr
  \code{\link[stats:Wilcoxon]{wilcox}}    \tab Wilcoxon rank sum distribution. \cr
  \code{\link[=ZeroModifiedLognormal]{zmlnorm}} \tab \bold{Zero-modified lognormal (delta) distribution}. \cr
  \code{\link[=ZeroModifiedLognormalAlt]{zmlnormAlt}} \tab \bold{Zero-modified lognormal (delta) distribution} \cr
                                          \tab \bold{parameterized with mean and CV}. \cr
  \code{\link[=ZeroModifiedNormal]{zmnorm}} \tab \bold{Zero-modified normal distribution}. \cr
  } 

  In addition, the functions \code{\link{evNormOrdStats}} and 
  \code{\link{evNormOrdStatsScalar}} compute expected values of order statistics 
  from a standard normal distribution.
}
\keyword{ package }
