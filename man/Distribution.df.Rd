\name{Distribution.df}
\alias{Distribution.df}
\docType{data}
\title{
  Data Frame Summarizing Available Probability Distributions and
  Estimation Methods
}
\description{
  Data frame summarizing information about available probability
  distributions in \R and the \pkg{EnvStats} package, and which
  distributions have associated functions for estimating distribution
  parameters.
}
\usage{Distribution.df}
\format{
  A data frame with 35 rows corresponding to 35 different available
  probability distributions, and 25 columns containing information
  associated with these probability distributions.
  \describe{
    \item{\code{Name}}{a character vector containing the name of
      the probability distribution (see the column labeled \bold{Name}
      in the table below).}
    \item{\code{Type}}{a character vector indicating the type of
      distribution (see the column labeled \bold{Type} in the table below).
      Possible values are \code{"Finite Discrete"}, \code{"Discrete"},
      \code{"Continuous"}, and \code{"Mixed"}.}
    \item{\code{Support.Min}}{a character vector indicating the minimum value
      the random variable can assume (see the column labeled \bold{Range} in
      the table below).  The reason this is a character vector instead of a
      numeric vector is because some distributions have a lower bound that
      depends on the value of a distribution parameter.  For example,
      the minimum value for a \link{Uniform} distribution is given by the
      value of the parameter \code{min}.}
    \item{\code{Support.Max}}{a character vector indicating the maximum value
      the random variable can assume (see the column labeled \bold{Range} in
      the table below).  The reason this is a character vector instead of a
      numeric vector is because some distributions have an upper bound that
      depends on the value of a distribution parameter.  For example,
      the maximum value for a \link{Uniform} distribution is given by the value
      of the parameter \code{max}.}
    \item{\code{Estimation.Method(s)}}{a character vector indicating the
      names of the methods available to estimate the distribution parameter(s)
      (see the column labeled \bold{Estimation Method(s)} in the table below).
      Possible values include \code{"mle"} (maximum likelihood), \code{"mme"}
      (method of moments), \code{"mmue"} (method of moments based on the
      unbiased estimate of variance), \code{"mvue"} (minimum variance unbiased),
      \code{"qmle"} (quasi-mle), etc., or some combination of these.  In
      cases where an estimator is more than one kind, a slash (\code{/}) is
      used to denote all methods covered by the single estimator.  For example,
      for the Binomial distribution, the sample proportion is the maximum
      likelihood, method of moments, and minimum variance unbiased estimator,
      so this method is denoted as \code{"mle/mme/mvue"}.  See the help files
      for the specific function listed under
      \link[=FcnsByCatEstDistParams]{Estimating Distribution Parameters} for an
      explanation of each of these estimation methods.}
    \item{\code{Quantile.Estimation.Method(s)}}{a character vector indicating
      the names of the methods available to estimate the distribution
      quantiles.  For many distributions, these are the same as
      \code{Estimation.Method(s)}.  See the help files for the specific
      function listed under
      \link[=FcnsByCatEstDistQuants]{Estimating Distribution Quantiles} for an
      explanation of each of these estimation methods.}
    \item{\code{Prediction.Interval.Method(s)}}{a character vector indicating
      the names of the methods available to create prediction intervals.  See
      the help files for the specific function listed under
      \link[=FcnsByCatPredInts]{Prediction Intervals} for an explanation of each of
      these estimation methods.}
    \item{\code{Singly.Censored.Estimation.Method(s)}}{a character vector
      indicating the names of the methods available to estimate the
      distribution parameter(s) for Type I singly-censored data.  See the
      help files for the specific function listed under
      \emph{Estimating Distribution Parameters} in the help file for
      \link[=FcnsByCatCensoredData]{Censored Data}
      for an explanation of each of these estimation methods.}
    \item{\code{Multiply.Censored.Estimation.Method(s)}}{a character vector
      indicating the names of the methods available to estimate the
      distribution parameter(s) for Type I multiply-censored data.  See the
      help files for the specific function listed under
      \emph{Estimating Distribution Parameters} in the help file for
      \link[=FcnsByCatCensoredData]{Censored Data}
      for an explanation of each of these estimation methods.}
    \item{\code{Number.parameters}}{a numeric vector indicating the number of
      parameters associated with the distribution (see the column labeled
      \bold{Parameters} in the table below).}
    \item{\code{Parameter.1}}{the columns labeled
      \code{Parameter.1}, \code{Parameter.2}, \ldots, \code{Parameter.5} are
      character vectors containing the names of the distribution parameters
      (see the column labeled \bold{Parameters} in the table below).  If a
      distribution has \eqn{n} parameters and \eqn{n < 5}, then the columns
      labeled \code{Parameter.n+1}, \ldots, \code{Parameter.5} are empty.  For
      example, the \link{Normal} distribution has only two parameters
      associated with it (\code{mean} and \code{sd}), so the fields in
      \code{Parameter.3}, \code{Parameter.4}, and \code{Parameter.5} are
      empty.}
    \item{\code{Parameter.2}}{see \code{Parameter.1}}
    \item{\code{Parameter.3}}{see \code{Parameter.1}}
    \item{\code{Parameter.4}}{see \code{Parameter.1}}
    \item{\code{Parameter.5}}{see \code{Parameter.1}}
    \item{\code{Parameter.1.Min}}{the columns labeled \code{Parameter.1.Min},
      \code{Parameter.2.Min}, \ldots, \cr
      \code{Parameter.5.Min} are character
      vectors containing the minimum values that can be assumed by the
      distribution parameters (see the column labeled \bold{Parameter Range(s)}
      in the table below).

      The reason these are character vectors instead of numeric vectors is
      because some parameters have a lower bound of \code{0} but must be
      strictly bigger than \code{0} (e.g., the parameter \code{sd} for the
      \link{Normal} distribution), in which case the lower bound is
      \code{.Machine$double.eps}, which may vary from machine to machine.
      Also, some parameters have a lower bound that depends on the value of
      another parameter.  For example, the parameter \code{max} for a
      \link{Uniform} distribution is bounded below by the value of the
      parameter \code{min}.

      If a distribution has \eqn{n} parameters and \eqn{n < 5}, then the
      columns labeled \code{Parameter.n+1.Min}, \ldots, \code{Parameter.5.Min}
      have the missing value code (\code{NA}).  For example, the \link{Normal}
      distribution has only two parameters associated with it (\code{mean}
      and \code{sd}) so the fields in \cr
      \code{Parameter.3.Min}, \code{Parameter.4.Min}, and \code{Parameter.5.Min}
      have \code{NA}s in them.}
    \item{\code{Parameter.2.Min}}{see \code{Parameter.1.Min}}
    \item{\code{Parameter.3.Min}}{see \code{Parameter.1.Min}}
    \item{\code{Parameter.4.Min}}{see \code{Parameter.1.Min}}
    \item{\code{Parameter.5.Min}}{see \code{Parameter.1.Min}}
    \item{\code{Parameter.1.Max}}{the columns labeled \code{Parameter.1.Max},
      \code{Parameter.2.Max}, \ldots, \cr
      \code{Parameter.5.Max} are character
      vectors containing the maximum values that can be assumed by the
      distribution parameters (see the column labeled \bold{Parameter Range(s)}
      in the table below).

      The reason these are character vectors instead of numeric vectors is
      because some parameters have an upper bound that depends on the value
      of another parameter.  For example, the parameter \code{min} for a
      \link{Uniform} distribution is bounded above by the value of the
      parameter \code{max}.

      If a distribution has \eqn{n} parameters and \eqn{n < 5}, then the
      columns labeled \code{Parameter.n+1.Max}, \ldots, \code{Parameter.5.Max}
      have the missing value code (\code{NA}).  For example, the \link{Normal}
      distribution has only two parameters associated with it (\code{mean}
      and \code{sd}) so the fields in \cr
      \code{Parameter.3.Max}, \code{Parameter.4.Max}, and \code{Parameter.5.Max}
      have \code{NA}s in them.}
    \item{\code{Parameter.2.Max}}{see \code{Parameter.1.Max}}
    \item{\code{Parameter.3.Max}}{see \code{Parameter.1.Max}}
    \item{\code{Parameter.4.Max}}{see \code{Parameter.1.Max}}
    \item{\code{Parameter.5.Max}}{see \code{Parameter.1.Max}}
  }
}
\details{
  The table below summarizes the probability distributions available in
  \R and \pkg{EnvStats}.  For each distribution, there are four
  associated functions for computing density values, percentiles, quantiles,
  and random numbers.  The form of the names of these functions are
  \code{d}\emph{abb}, \code{p}\emph{abb}, \code{q}\emph{abb}, and
  \code{r}\emph{abb}, where \emph{abb} is the abbreviated name of the
  distribution (see table below).  These functions are described in the
  help file with the name of the distribution (see the first column of the
  table below).  For example, the help file for \link{Beta} describes the
  behavior of \code{\link{dbeta}}, \code{\link{pbeta}}, \code{\link{qbeta}},
  and \code{\link{rbeta}}.

  For most distributions, there is also an associated function for
  estimating the distribution parameters, and the form of the names of
  these functions is \code{e}\emph{abb}, where \emph{abb} is the
  abbreviated name of the distribution (see table below).  All of these
  functions are listed in the help file
  \link[=FcnsByCatEstDistParams]{Estimating Distribution Parameters}.  For example,
  the function \code{\link{ebeta}} estimates the shape parameters of a
  Beta distribution based on a random sample of observations from
  this distribution.

  For some distributions, there are functions to estimate distribution
  parameters based on Type I censored data.  The form of the names of
  these functions is \code{e}\emph{abb}\code{SinglyCensored} for
  singly censored data and \code{e}\emph{abb}\code{MultiplyCensored} for
  multiply censored data.  All of these functions are listed under the heading
  \emph{Estimating Distribution Parameters} in the help file
  \link[=FcnsByCatCensoredData]{Censored Data}.

  \strong{Table 1a.  Available Distributions: Name, Abbreviation, Type, and Range}

  \tabular{llll}{
    \bold{Name}     \tab \bold{Abbreviation} \tab \bold{Type} \tab \bold{Range}            \cr
    \link{Beta}     \tab \code{beta}         \tab Continuous  \tab \eqn{[0, 1]}            \cr
                    \tab                     \tab             \tab                         \cr
    \link{Binomial} \tab \code{binom}        \tab Finite      \tab \eqn{[0, size]}         \cr
                    \tab                     \tab Discrete    \tab (integer)               \cr
                    \tab                     \tab             \tab                         \cr
    \link{Cauchy}   \tab \code{cauchy}       \tab Continuous  \tab \eqn{(-\infty, \infty)} \cr
                    \tab                     \tab             \tab                         \cr
    \link{Chi}      \tab \code{chi}          \tab Continuous  \tab \eqn{[0, \infty)}       \cr
                    \tab                     \tab             \tab                         \cr
    \link[=Chisquare]{Chi-square} \tab \code{chisq} \tab Continuous \tab \eqn{[0, \infty)} \cr
                    \tab                     \tab             \tab                         \cr
    \link{Exponential} \tab \code{exp}       \tab Continuous  \tab \eqn{[0, \infty)}       \cr
                    \tab                     \tab             \tab                         \cr
    \link[=EVD]{Extreme} \tab \code{evd}     \tab Continuous  \tab \eqn{(-\infty, \infty)} \cr
    \link[=EVD]{Value}   \tab                \tab             \tab                         \cr
                    \tab                     \tab             \tab                         \cr
    \link[=FDist]{F} \tab \code{f}           \tab Continuous  \tab \eqn{[0, \infty)}       \cr
                    \tab                     \tab             \tab                         \cr
    \link[=GammaDist]{Gamma} \tab \code{gamma} \tab Continuous \tab \eqn{[0, \infty)}      \cr
                    \tab                     \tab             \tab                         \cr
    \link[=GammaAlt]{Gamma} \tab \code{gammaAlt} \tab Continuous  \tab \eqn{[0, \infty)} \cr
    \link[=GammaAlt]{(Alternative)} \tab         \tab             \tab                 \cr
                    \tab                     \tab             \tab                         \cr
    \link[=GEVD]{Generalized} \tab \code{gevd} \tab Continuous \tab \eqn{(-\infty, \infty)} \cr
    \link[=GEVD]{Extreme}     \tab             \tab            \tab for \eqn{shape = 0}    \cr
    \link[=GEVD]{Value}       \tab             \tab            \tab                        \cr
                              \tab             \tab            \tab \eqn{(-\infty, location + \frac{scale}{shape}]} \cr
                              \tab             \tab            \tab for \eqn{shape > 0}    \cr
                              \tab             \tab            \tab                        \cr
                              \tab             \tab            \tab \eqn{[location + \frac{scale}{shape}, \infty)} \cr
                              \tab             \tab            \tab for \eqn{shape < 0}    \cr
                    \tab                     \tab             \tab                         \cr
    \link{Geometric} \tab \code{geom}        \tab Discrete    \tab \eqn{[0, \infty)}       \cr
                    \tab                     \tab             \tab (integer)               \cr
                    \tab                     \tab             \tab                         \cr
    \link{Hypergeometric} \tab \code{hyper}  \tab Finite      \tab \eqn{[0, min(k,m)]}     \cr
                          \tab               \tab Discrete    \tab (integer)               \cr
                    \tab                     \tab             \tab                         \cr
    \link{Logistic} \tab \code{logis}        \tab Continuous  \tab \eqn{(-\infty, \infty)} \cr
                    \tab                     \tab             \tab                         \cr
    \link{Lognormal} \tab \code{lnorm}   \tab Continuous  \tab \eqn{(0, \infty)}           \cr
                    \tab                     \tab             \tab                         \cr
    \link[=LognormalAlt]{Lognormal} \tab \code{lnormAlt} \tab Continuous  \tab \eqn{(0, \infty)} \cr
    \link[=LognormalAlt]{(Alternative)} \tab             \tab             \tab                   \cr
                    \tab                     \tab             \tab                               \cr
    \link[=LognormalMix]{Lognormal} \tab \code{lnormMix} \tab Continuous \tab \eqn{(0, \infty)}  \cr
    \link[=LognormalMix]{Mixture}   \tab                 \tab            \tab                    \cr
                    \tab                     \tab             \tab                               \cr
    \link[=LognormalMixAlt]{Lognormal} \tab \code{lnormMixAlt} \tab Continuous \tab \eqn{(0, \infty)} \cr
    \link[=LognormalMixAlt]{Mixture}   \tab                    \tab            \tab                   \cr
    \link[=LognormalMixAlt]{(Alternative)} \tab                \tab            \tab                   \cr
                    \tab                     \tab             \tab                         \cr
    \link[=Lognormal3]{Three-}    \tab \code{lnorm3} \tab Continuous \tab \eqn{[threshold, \infty)} \cr
    \link[=Lognormal3]{Parameter} \tab               \tab            \tab                           \cr
    \link[=Lognormal3]{Lognormal} \tab               \tab            \tab                           \cr
                    \tab                     \tab             \tab                         \cr
    \link[=LognormalTrunc]{Truncated} \tab \code{lnormTrunc}  \tab Continuous \tab \eqn{[min, max]} \cr
    \link[=LognormalTrunc]{Lognormal} \tab                    \tab            \tab                  \cr
                    \tab                     \tab             \tab                         \cr
    \link[=LognormalTruncAlt]{Truncated} \tab \code{lnormTruncAlt} \tab Continuous \tab \eqn{[min, max]} \cr
    \link[=LognormalTruncAlt]{Lognormal} \tab                 \tab                 \tab                  \cr
    \link[=LognormalTruncAlt]{(Alternative)} \tab             \tab                 \tab                  \cr
                    \tab                     \tab             \tab                         \cr
    \link[=NegBinomial]{Negative} \tab \code{nbinom} \tab Discrete \tab \eqn{[0, \infty)}  \cr
    \link[=NegBinomial]{Binomial} \tab               \tab          \tab (integer)          \cr
                    \tab                     \tab             \tab                         \cr
    \link{Normal} \tab \code{norm}       \tab Continuous  \tab \eqn{(-\infty, \infty)}     \cr
                    \tab                     \tab             \tab                         \cr
    \link[=NormalMix]{Normal}  \tab \code{normMix} \tab Continuous \tab \eqn{(-\infty, \infty)} \cr
    \link[=NormalMix]{Mixture} \tab                \tab            \tab                         \cr
                    \tab                     \tab             \tab                         \cr
    \link[=NormalTrunc]{Truncated} \tab \code{normTrunc} \tab Continuous \tab \eqn{[min, max]} \cr
    \link[=NormalTrunc]{Normal}    \tab                  \tab            \tab                  \cr
                    \tab                     \tab             \tab                         \cr
    \link{Pareto} \tab \code{pareto}     \tab Continuous  \tab \eqn{[location, \infty)}    \cr
                    \tab                     \tab             \tab                         \cr
    \link{Poisson} \tab \code{pois}      \tab Discrete    \tab \eqn{[0, \infty)}           \cr
                   \tab                  \tab             \tab (integer)                   \cr
                    \tab                     \tab             \tab                         \cr
    \link[=TDist]{Student's t} \tab \code{t} \tab Continuous \tab \eqn{(-\infty, \infty)}  \cr
                    \tab                     \tab             \tab                         \cr
    \link{Triangular} \tab \code{tri} \tab Continuous \tab \eqn{[min, max]}                \cr
                    \tab                     \tab             \tab                         \cr
    \link{Uniform} \tab \code{unif} \tab Continuous \tab \eqn{[min, max]}                  \cr
                    \tab                     \tab             \tab                         \cr
    \link{Weibull} \tab \code{weibull}   \tab Continuous  \tab \eqn{[0, \infty)}              \cr
                    \tab                     \tab             \tab                         \cr
    \link[=Wilcoxon]{Wilcoxon} \tab \code{wilcox} \tab Finite   \tab \eqn{[0, m n]}        \cr
    \link[=Wilcoxon]{Rank Sum} \tab               \tab Discrete \tab (integer)             \cr
                    \tab                     \tab             \tab                         \cr
    \link[=ZeroModifiedLognormal]{Zero-Modified} \tab \code{zmlnorm} \tab Mixed \tab \eqn{[0, \infty)} \cr
    \link[=ZeroModifiedLognormal]{Lognormal}     \tab                \tab       \tab                   \cr
    \link[=ZeroModifiedLognormal]{(Delta)}       \tab                \tab       \tab                   \cr
                    \tab                     \tab             \tab                         \cr
    \link[=ZeroModifiedLognormalAlt]{Zero-Modified} \tab \code{zmlnormAlt} \tab Mixed \tab \eqn{[0, \infty)} \cr
    \link[=ZeroModifiedLognormalAlt]{Lognormal}     \tab                \tab       \tab                \cr
    \link[=ZeroModifiedLognormalAlt]{(Delta)}       \tab                \tab       \tab                \cr
    \link[=ZeroModifiedLognormalAlt]{(Alternative)} \tab                \tab       \tab                \cr
                    \tab                     \tab             \tab                         \cr
    \link[=ZeroModifiedNormal]{Zero-Modified} \tab \code{zmnorm} \tab Mixed \tab \eqn{(-\infty, \infty)} \cr
    \link[=ZeroModifiedNormal]{Normal}        \tab               \tab       \tab           \cr
  }

  \strong{Table 1b.  Available Distributions: Name, Parameters, Parameter Default Values, Parameter Ranges, Estimation Method(s)}

  \tabular{lllll}{
                \tab                     \tab \bold{Default}  \tab \bold{Parameter}  \tab \bold{Estimation} \cr
    \bold{Name} \tab \bold{Parameter(s)} \tab \bold{Value(s)} \tab \bold{Range(s)}   \tab \bold{Method(s)}  \cr
    \link{Beta} \tab \code{shape1}       \tab                 \tab \eqn{(0, \infty)} \tab \link[=ebeta]{mle, mme, mmue} \cr
                \tab \code{shape2}       \tab                 \tab \eqn{(0, \infty)} \tab                   \cr
                \tab \code{ncp}          \tab \code{0}        \tab \eqn{(0, \infty)} \tab                   \cr
                \tab                     \tab                 \tab                   \tab                   \cr
    \link{Binomial} \tab \code{size}     \tab                 \tab \eqn{[0, \infty)} \tab \link[=ebinom]{mle/mme/mvue} \cr
                    \tab \code{prob}     \tab                 \tab \eqn{[0, 1]}      \tab                   \cr
                \tab                     \tab                 \tab                   \tab                   \cr
    \link{Cauchy} \tab \code{location}   \tab \code{0}        \tab \eqn{(-\infty, \infty)} \tab             \cr
                  \tab \code{scale}      \tab \code{1}        \tab \eqn{(0, \infty)}       \tab             \cr
                \tab                     \tab                 \tab                   \tab                   \cr
    \link{Chi}  \tab \code{df}           \tab                 \tab \eqn{(0, \infty)} \tab                   \cr
                \tab                     \tab                 \tab                   \tab                   \cr
    \link[=Chisquare]{Chi-square} \tab \code{df}  \tab        \tab \eqn{(0, \infty)} \tab                   \cr
                \tab \code{ncp}          \tab \code{0}        \tab \eqn{(-\infty, \infty)} \tab             \cr
                \tab                     \tab                 \tab                   \tab                   \cr
    \link{Exponential} \tab \code{rate}  \tab \code{1}        \tab \eqn{(0, \infty)} \tab \link[=eexp]{mle/mme} \cr
                \tab                     \tab                 \tab                   \tab                   \cr
    \link[=EVD]{Extreme} \tab \code{location} \tab \code{0}   \tab \eqn{ (-\infty, \infty)} \tab \link[=eevd]{mle, mme, mmue, pwme} \cr
    \link[=EVD]{Value}   \tab \code{scale}    \tab \code{1}   \tab \eqn{(0, \infty)}        \tab            \cr
                \tab                     \tab                 \tab                   \tab                   \cr
    \link[=FDist]{F} \tab \code{df1}     \tab                 \tab \eqn{(0, \infty)} \tab                   \cr
                     \tab \code{df2}     \tab                 \tab \eqn{(0, \infty)} \tab                   \cr
                     \tab \code{ncp}     \tab \code{0}        \tab \eqn{(0, \infty)} \tab                   \cr
                \tab                     \tab                 \tab                   \tab                   \cr
    \link[=GammaDist]{Gamma} \tab \code{shape} \tab           \tab \eqn{(0, \infty)} \tab \link[=egamma]{mle, bcmle, mme, mmue} \cr
                             \tab \code{scale} \tab \code{1}  \tab \eqn{(0, \infty)} \tab                   \cr
                \tab                     \tab                 \tab                   \tab                   \cr
    \link[=GammaAlt]{Gamma} \tab \code{mean} \tab         \tab \eqn{(0, \infty)} \tab \link[=egammaAlt]{mle, bcmle, mme, mmue} \cr
    \link[=GammaAlt]{(Alternative)} \tab \code{cv} \tab \code{1} \tab \eqn{(0, \infty)} \tab            \cr
                \tab                     \tab                 \tab                   \tab                   \cr
    \link[=GEVD]{Generalized} \tab \code{location} \tab \code{0} \tab \eqn{(-\infty, \infty)} \tab \link[=egevd]{mle, pwme, tsoe} \cr
    \link[=GEVD]{Extreme}     \tab \code{scale}    \tab \code{1} \tab \eqn{(0, \infty)}       \tab          \cr
    \link[=GEVD]{Value}       \tab \code{shape}    \tab \code{0} \tab \eqn{(-\infty, \infty)} \tab          \cr
                \tab                     \tab                 \tab                   \tab                   \cr
    \link{Geometric} \tab \code{prob}    \tab                 \tab \eqn{(0, 1)}      \tab \link[=egeom]{mle/mme, mvue} \cr
                \tab                     \tab                 \tab                   \tab                   \cr
    \link{Hypergeometric} \tab \code{m}  \tab                 \tab \eqn{[0, \infty)} \tab \link[=ehyper]{mle, mvue} \cr
                          \tab \code{n}  \tab                 \tab \eqn{[0, \infty)} \tab                   \cr
                          \tab \code{k}  \tab                 \tab \eqn{[1, m+n]}    \tab                   \cr
                \tab                     \tab                 \tab                   \tab                   \cr
    \link{Logistic} \tab \code{location} \tab \code{0}        \tab \eqn{(-\infty, \infty)} \tab \link[=elogis]{mle, mme, mmue} \cr
                    \tab \code{scale}    \tab \code{1}        \tab \eqn{(0, \infty)}       \tab             \cr
                \tab                     \tab                 \tab                   \tab                   \cr
    \link{Lognormal} \tab \code{meanlog} \tab \code{0}        \tab \eqn{(-\infty, \infty)}  \tab \link[=elnorm]{mle/mme, mvue} \cr
                     \tab \code{sdlog}   \tab \code{1}        \tab \eqn{(0, \infty)}        \tab            \cr
                \tab                     \tab                 \tab                   \tab                   \cr
    \link[=LognormalAlt]{Lognormal} \tab \code{mean} \tab \code{exp(1/2)} \tab \eqn{(0, \infty)} \tab \link[=elnormAlt]{mle, mme, mmue,} \cr
    \link[=LognormalAlt]{(Alternative)} \tab \code{cv} \tab \code{sqrt(exp(1)-1)} \tab \eqn{(0, \infty)}  \tab \link[=elnormAlt]{mvue, qmle} \cr
                \tab                     \tab                 \tab                   \tab                   \cr
    \link[=LognormalMix]{Lognormal} \tab \code{meanlog1} \tab \code{0} \tab \eqn{(-\infty, \infty)} \tab    \cr
    \link[=LognormalMix]{Mixture}   \tab \code{sdlog1}   \tab \code{1} \tab \eqn{(0, \infty)}       \tab    \cr
                                    \tab \code{meanlog2} \tab \code{0} \tab \eqn{(-\infty, \infty)} \tab    \cr
                                    \tab \code{sdlog2}   \tab \code{1} \tab \eqn{(0, \infty)}       \tab    \cr
                                    \tab \code{p.mix}    \tab \code{0.5} \tab \eqn{[0, 1]}          \tab    \cr
                \tab                     \tab                 \tab                   \tab                   \cr
    \link[=LognormalMixAlt]{Lognormal} \tab \code{mean1} \tab \code{exp(1/2)} \tab \eqn{(0, \infty)} \tab   \cr
    \link[=LognormalMixAlt]{Mixture}   \tab \code{cv1}   \tab \code{sqrt(exp(1)-1)} \tab \eqn{(0, \infty)} \tab \cr
    \link[=LognormalMixAlt]{(Alternative)} \tab \code{mean2} \tab \code{exp(1/2)} \tab \eqn{(0, \infty)} \tab   \cr
                                       \tab \code{cv2}   \tab \code{sqrt(exp(1)-1)} \tab \eqn{(0, \infty)} \tab \cr
                                       \tab \code{p.mix} \tab \code{0.5}      \tab \eqn{[0, 1]}       \tab  \cr
                \tab                     \tab                 \tab                   \tab                   \cr
    \link[=Lognormal3]{Three-}    \tab \code{meanlog} \tab \code{0} \tab \eqn{(-\infty, \infty)} \tab \link[=elnorm3]{lmle, mme,} \cr
    \link[=Lognormal3]{Parameter} \tab \code{sdlog}   \tab \code{1} \tab \eqn{(0, \infty)}       \tab \link[=elnorm3]{mmue, mmme,} \cr
    \link[=Lognormal3]{Lognormal} \tab \code{threshold} \tab \code{0} \tab \eqn{(-\infty, \infty)} \tab \link[=elnorm3]{royston.skew,} \cr
                                  \tab                \tab          \tab                         \tab \link[=elnorm3]{zero.skew} \cr
                \tab                     \tab                 \tab                   \tab                   \cr
    \link[=LognormalTrunc]{Truncated} \tab \code{meanlog} \tab \code{0} \tab \eqn{(-\infty, \infty)} \tab   \cr
    \link[=LognormalTrunc]{Lognormal} \tab \code{sdlog}   \tab \code{1} \tab \eqn{(0, \infty)}       \tab   \cr
                                      \tab \code{min}     \tab \code{0} \tab \eqn{[0, max)}          \tab   \cr
                                      \tab \code{max}     \tab \code{Inf} \tab \eqn{(min, \infty)}   \tab   \cr
                \tab                     \tab                 \tab                   \tab                   \cr
    \link[=LognormalTruncAlt]{Truncated} \tab \code{mean} \tab \code{exp(1/2)} \tab \eqn{(0, \infty)} \tab  \cr
    \link[=LognormalTruncAlt]{Lognormal} \tab \code{cv}   \tab \code{sqrt(exp(1)-1)} \tab \eqn{(0, \infty)} \tab \cr
    \link[=LognormalTruncAlt]{(Alternative)} \tab \code{min} \tab \code{0} \tab \eqn{[0, max)}       \tab   \cr
                                         \tab \code{max}  \tab \code{Inf} \tab \eqn{(min, \infty)}   \tab   \cr
                \tab                     \tab                 \tab                   \tab                   \cr
    \link[=NegBinomial]{Negative} \tab \code{size} \tab      \tab \eqn{[1, \infty)} \tab \link[=enbinom]{mle/mme, mvue} \cr
    \link[=NegBinomial]{Binomial} \tab \code{prob} \tab      \tab \eqn{(0, 1]}      \tab                    \cr
                                  \tab \code{mu}   \tab      \tab \eqn{(0, \infty)} \tab                    \cr
                \tab                     \tab                 \tab                   \tab                   \cr
    \link{Normal} \tab \code{mean}       \tab \code{0}        \tab \eqn{(-\infty, \infty)} \tab \link[=enorm]{mle/mme, mvue} \cr
                  \tab \code{sd}         \tab \code{1}        \tab \eqn{(0, \infty)}       \tab               \cr
                \tab                     \tab                 \tab                   \tab                   \cr
    \link[=NormalMix]{Normal}  \tab \code{mean1} \tab \code{0} \tab \eqn{(-\infty, \infty)} \tab \cr
    \link[=NormalMix]{Mixture} \tab \code{sd1}   \tab \code{1} \tab \eqn{(0, \infty)}       \tab \cr
                               \tab \code{mean2} \tab \code{0} \tab \eqn{(-\infty, \infty)} \tab \cr
                               \tab \code{sd2}   \tab \code{1} \tab \eqn{(0, \infty)}       \tab \cr
                               \tab \code{p.mix} \tab \code{0.5} \tab \eqn{[0, 1]}          \tab \cr
                \tab                     \tab                 \tab                   \tab                   \cr
    \link[=NormalTrunc]{Truncated} \tab \code{mean} \tab \code{0} \tab \eqn{(-\infty, \infty)} \tab   \cr
    \link[=NormalTrunc]{Normal}    \tab \code{sd}   \tab \code{1} \tab \eqn{(0, \infty)}       \tab   \cr
                                   \tab \code{min}  \tab \code{-Inf} \tab \eqn{(-\infty, max)} \tab   \cr
                                   \tab \code{max}  \tab \code{Inf} \tab \eqn{(min, \infty)}   \tab   \cr
                \tab                     \tab                 \tab                   \tab             \cr
    \link{Pareto} \tab \code{location} \tab          \tab \eqn{(0, \infty)} \tab \link[=epareto]{lse, mle} \cr
                  \tab \code{shape}    \tab \code{1} \tab \eqn{(0, \infty)} \tab          \cr
                \tab                     \tab                 \tab                   \tab \cr
    \link{Poisson} \tab \code{lambda} \tab \tab \eqn{(0, \infty)} \tab \link[=epois]{mle/mme/mvue} \cr
                \tab                     \tab                 \tab                   \tab                   \cr
    \link[=TDist]{Student's t} \tab \code{df}  \tab          \tab \eqn{(0, \infty)}       \tab \cr
                               \tab \code{ncp} \tab \code{0} \tab \eqn{(-\infty, \infty)} \tab \cr
                \tab                     \tab                 \tab                   \tab                   \cr
    \link{Triangular} \tab \code{min}  \tab \code{0}   \tab \eqn{(-\infty, max)} \tab \cr
                      \tab \code{max}  \tab \code{1}   \tab \eqn{(min, \infty)}  \tab \cr
                      \tab \code{mode} \tab \code{0.5} \tab \eqn{(min, max)}     \tab \cr
                \tab                     \tab                 \tab                   \tab                   \cr
    \link{Uniform} \tab \code{min} \tab \code{0} \tab \eqn{(-\infty, max)} \tab \link[=eunif]{mle, mme, mmue} \cr
                   \tab \code{max} \tab \code{1} \tab \eqn{(min, \infty)}  \tab                \cr
                \tab                     \tab                 \tab                   \tab                   \cr
    \link{Weibull} \tab \code{shape}     \tab                 \tab \eqn{(0, \infty)} \tab \link[=eweibull]{mle, mme, mmue}    \cr
                   \tab \code{scale}     \tab \code{1}        \tab \eqn{(0, \infty)} \tab                   \cr
                \tab                     \tab                 \tab                   \tab                   \cr
    \link[=Wilcoxon]{Wilcoxon} \tab \code{m} \tab \tab \eqn{[1, \infty)} \tab \cr
    \link[=Wilcoxon]{Rank Sum} \tab \code{n} \tab \tab \eqn{[1, \infty)} \tab \cr
                \tab                     \tab                 \tab                   \tab                   \cr
    \link[=ZeroModifiedLognormal]{Zero-Modified} \tab \code{meanlog} \tab \code{0} \tab \eqn{(-\infty, \infty)} \tab \link[=ezmlnorm]{mvue} \cr
    \link[=ZeroModifiedLognormal]{Lognormal}     \tab \code{sdlog}   \tab \code{1} \tab \eqn{(0, \infty)}       \tab    \cr
    \link[=ZeroModifiedLognormal]{(Delta)}       \tab \code{p.zero}  \tab \code{0.5} \tab \eqn{[0, 1]}          \tab    \cr
                \tab                     \tab                 \tab                   \tab                   \cr
    \link[=ZeroModifiedLognormalAlt]{Zero-Modified} \tab \code{mean} \tab \code{exp(1/2)} \tab \eqn{(0, \infty)} \tab \link[=ezmlnormAlt]{mvue} \cr
    \link[=ZeroModifiedLognormalAlt]{Lognormal}     \tab \code{cv}   \tab \code{sqrt(exp(1)-1)} \tab \eqn{(0, \infty)}       \tab      \cr
    \link[=ZeroModifiedLognormalAlt]{(Delta)}       \tab \code{p.zero} \tab \code{0.5} \tab \eqn{[0, 1]} \tab           \cr
    \link[=ZeroModifiedLognormalAlt]{(Alternative)} \tab             \tab              \tab                             \cr
                \tab                     \tab                 \tab                   \tab                   \cr
    \link[=ZeroModifiedNormal]{Zero-Modified} \tab \code{mean} \tab \code{0} \tab \eqn{(-\infty, \infty)} \tab \link[=ezmnorm]{mvue} \cr
    \link[=ZeroModifiedNormal]{Normal}        \tab \code{sd}   \tab \code{1} \tab \eqn{(0, \infty)}       \tab      \cr
                                              \tab \code{p.zero} \tab \code{0.5} \tab \eqn{[0, 1]}        \tab      \cr
  }
}
\source{
  The \pkg{EnvStats} package.
}
\references{
  Millard, S.P. (2013). \emph{EnvStats: An R Package for Environmental Statistics}.
  Springer, New York.  \url{https://link.springer.com/book/10.1007/978-1-4614-8456-1}.
}
\keyword{datasets}
