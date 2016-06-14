\name{ehyper}
\alias{ehyper}
\title{
  Estimate Parameter of a Hypergeometric Distribution
}
\description{
  Estimate \eqn{m}, the number of white balls in the urn, or 
  \eqn{m+n}, the total number of balls in the urn, for a 
  \link[stats:Hypergeometric]{hypergeometric distribution}.
}
\usage{
  ehyper(x, m = NULL, total = NULL, k, method = "mle")
}
\arguments{
  \item{x}{
  non-negative integer indicating the number of white balls out of a sample of 
  size \code{k} drawn without replacement from the urn.  Missing (\code{NA}), 
  undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) values are not 
  allowed.
}
  \item{m}{
  non-negative integer indicating the number of white balls in the urn.  
  You must supply \code{m} or \code{total}, but not both.  
  Missing values (\code{NA}s) are not allowed.
}
  \item{total}{
  positive integer indicating the total number of balls in the urn (i.e., 
  \code{m+n}).  You must supply \code{m} or \code{total}, but not both.
  Missing values (\code{NA}s) are not allowed.
}
  \item{k}{
  positive integer indicating the number of balls drawn without replacement from the 
  urn.  Missing values (\code{NA}s) are not allowed.
}
  \item{method}{
  character string specifying the method of estimation.  Possible values are 
  \code{"mle"} (maximum likelihood; the default) and \code{"mvue"} 
  (minimum variance unbiased).  The mvue method is only available when you 
  are estimating \eqn{m} (i.e., when you supply the argument \code{total}).
  See the DETAILS section for more information on these estimation methods. 
}
}
\details{
  Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) 
  values are not allowed.

  Let \eqn{x} be an observation from a 
  \link[stats:Hypergeometric]{hypergeometric distribution} with 
  parameters \code{m=}\eqn{M}, \code{n=}\eqn{N}, and \code{k=}\eqn{K}.  
  In \R nomenclature, \eqn{x} represents the number of white balls drawn out of a 
  sample of \eqn{K} balls drawn \emph{without} replacement from an urn containing 
  \eqn{M} white balls and \eqn{N} black balls.  The total number of balls in the 
  urn is thus \eqn{M+N}.  Denote the total number of balls by \eqn{T = M+N}.

  \bold{Estimation} \cr

  \emph{Estimating M, Given T and K are known} \cr
  When \eqn{T} and \eqn{K} are known, the maximum likelihood estimator (mle) of 
  \eqn{M} is given by (Forbes et al., 2011):
  \deqn{\hat{M}_{mle} = floor[(T + 1) x / K] \;\;\;\; (1)}
  where \eqn{floor()} represents the \code{\link[base:Round]{floor}} function.  
  That is, \eqn{floor(y)} is the largest integer less than or equal to \eqn{y}. 

  If the quantity \eqn{floor[(T + 1) x / K]} is an integer, then the mle of 
  \eqn{M} is also given by (Johnson et al., 1992, p.263):
  \deqn{\hat{M}_{mle} = [(T + 1) x / K] - 1 \;\;\;\; (2)}
  which is what the function \code{ehyper} uses for this case.

  The minimum variance unbiased estimator (mvue) of \eqn{M} is given by 
  (Forbes et al., 2011):
  \deqn{\hat{M}_{mvue} = (T x / K) \;\;\;\; (3)}
  \cr

  \emph{Estimating T, given M and K are known} \cr
  When \eqn{M} and \eqn{K} are known, the maximum likelihood estimator (mle) of 
  \eqn{T} is given by (Forbes et al., 2011):
  \deqn{\hat{T}_{mle} = floor(K M / x) \;\;\;\; (4)}
}
\value{
  a list of class \code{"estimate"} containing the estimated parameters and other information.  
  See \cr
  \code{\link{estimate.object}} for details.
}
\references{
  Forbes, C., M. Evans, N. Hastings, and B. Peacock. (2011).  Statistical Distributions. 
  Fourth Edition. John Wiley and Sons, Hoboken, NJ.

  Johnson, N. L., S. Kotz, and A. Kemp. (1992).  
  \emph{Univariate Discrete Distributions}.  Second Edition. John Wiley and Sons, 
  New York, Chapter 6.
}
\author{
    Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  The \link[stats:Hypergeometric]{hypergeometric distribution} can be described by 
  an urn model with \eqn{M} white balls and \eqn{N} black balls.  If \eqn{K} balls 
  are drawn \emph{with} replacement, then the number of white balls in the sample 
  of size \eqn{K} follows a \link[stats:Binomial]{binomial distribution} with 
  parameters \code{size=}\eqn{K} and \code{prob=}\eqn{M/(M+N)}.  If \eqn{K} balls are 
  drawn \emph{without} replacement, then the number of white balls in the sample of 
  size \eqn{K} follows a \link[stats:Hypergeometric]{hypergeometric distribution} 
  with parameters \code{m=}\eqn{M}, \code{n=}\eqn{N}, and \code{k=}\eqn{K}.

  The name \dQuote{hypergeometric} comes from the fact that the probabilities 
  associated with this distribution can be written as successive terms in the 
  expansion of a function of a Gaussian hypergeometric series.

  The hypergeometric distribution is applied in a variety of fields, including 
  quality control and estimation of animal population size.  It is also the 
  distribution used to compute probabilities for 
  \link[stats:fisher.test]{Fishers's exact test} for a 2x2 contingency table.
}
\seealso{
  \link[stats]{Hypergeometric}.
}
\examples{
  # Generate an observation from a hypergeometric distribution with 
  # parameters m=10, n=30, and k=5, then estimate the parameter m. 
  # Note: the call to set.seed simply allows you to reproduce this example. 
  # Also, the only parameter actually estimated is m; once m is estimated, 
  # n is computed by subtracting the estimated value of m (8 in this example) 
  # from the given of value of m+n (40 in this example).  The parameters 
  # n and k are shown in the output in order to provide information on 
  # all of the parameters associated with the hypergeometric distribution.

  set.seed(250) 
  dat <- rhyper(nn = 1, m = 10, n = 30, k = 5) 
  dat 
  #[1] 1   

  ehyper(dat, total = 40, k = 5) 

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            Hypergeometric
  #
  #Estimated Parameter(s):          m =  8
  #                                 n = 32
  #                                 k =  5
  #
  #Estimation Method:               mle for 'm'
  #
  #Data:                            dat
  #
  #Sample Size:                     1

  #----------

  # Use the same data as in the previous example, but estimate m+n instead. 
  # Note: The only parameter estimated is m+n. Once this is estimated, 
  # n is computed by subtracting the given value of m (10 in this case) 
  # from the estimated value of m+n (50 in this example).

  ehyper(dat, m = 10, k = 5)

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            Hypergeometric
  #
  #Estimated Parameter(s):          m = 10
  #                                 n = 40
  #                                 k =  5
  #
  #Estimation Method:               mle for 'm+n'
  #
  #Data:                            dat
  #
  #Sample Size:                     1


  #----------

  # Clean up
  #---------
  rm(dat)
}
\keyword{ distribution }
\keyword{ htest }
