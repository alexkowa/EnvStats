\name{base}
\alias{base}
\title{
  Base \eqn{b} Representation of a Number
}
\description{
  For any number represented in base 10, compute the representation in any user-specified base.
}
\usage{
  base(n, base = 10, num.digits = max(0, floor(log(n, base))) + 1)
}
\arguments{
  \item{n}{
  a non-negative integer (base 10).
}
  \item{base}{
  a positive integer greater than 1 indicating what base to represent \code{n} in.
}
  \item{num.digits}{
  a positive integer indicating how many digits to use to represent \code{n} in base \code{base}.  
  By default, \code{num.digits} is equal to just the number of required digits 
  (i.e., \code{max(0, floor(log(n, base))) + 1)}.  Setting \code{num.digits} to a larger number 
  than this will result in 0's padding the left.
}
}
\details{
  If \eqn{b} is a positive integer greater than 1, and \eqn{n} is a positive integer, 
  then \eqn{n} can be expressed uniquely in the form

  \deqn{n = a_kb^k + a_{k-1}b^{k-1} + \ldots + a_1b + a0}

  where \eqn{k} is a non-negative integer, the coefficients \eqn{a_0, a_1, \ldots, a_k}
  are non-negative integers less than \eqn{b}, and \eqn{a_k > 0} 
  (Rosen, 1988, p.105).  The function \code{base} computes the coefficients 
  \eqn{a_0, a_1, \ldots, a_k}.
}
\value{
  A numeric vector of length \code{num.digits} showing the representation of \code{n} in base \code{base}.
}
\references{
  Rosen, K.H. (1988). \emph{Discrete Mathematics and Its Applications}.  Random House, New York, 
  pp.105-107.
}
\author{
    Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  The function \code{base} is included in \pkg{EnvStats} because it 
  is called by the function \cr 
  \code{\link{oneSamplePermutationTest}}.
}
\seealso{
  \code{\link{oneSamplePermutationTest}}.
}
\examples{
  # Compute the value of 7 in base 2.

  base(7, 2) 
  #[1] 1 1 1 

  base(7, 2, num.digits=5) 
  #[1] 0 0 1 1 1
}
\keyword{ math }
