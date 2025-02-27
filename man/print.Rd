\name{print}
\alias{print}
\alias{print.default}
\title{
  Print Values
}
\description{
  The \pkg{EnvStats} function \code{print} is a generic function for 
  printing its argument and returning it invisibly (via \code{\link{invisible}(x)}). 
  The function invokes particular \code{\link{methods}} which 
  depend on the \code{\link{class}} of the first argument.  
  When given an argument of class \code{"htest"}, the \pkg{EnvStats} function \code{print.default} 
  calls the \pkg{EnvStats} function \code{link{print.htest}}, otherwise it calls the \R  
  function \code{\link[base:print.default]{print.default}}.

  The \pkg{EnvStats} functions \code{print} and \code{print.default} have been 
  created in order to comply with CRAN policies, because \pkg{EnvStats} contains a 
  modified version of the \R function \code{\link{print.htest}}.  
}
\usage{
print(x, ...)

\method{print}{default}(x, ...)
}
\arguments{
  \item{x}{
  an object to be printed and used to select a printing method.
}
  \item{\dots}{
  further arguments passed to or from other methods.  See the help files for 
  the R functions \code{\link[base:print]{print}} and 
  \code{\link[base:print.default]{print.default}} for more information.
}
}
\details{
  The \pkg{EnvStats} function \code{print} is a generic function for 
  printing its argument and returning it invisibly (via \code{\link{invisible}(x)}). 
  The function invokes particular \code{\link{methods}} which 
  depend on the \code{\link{class}} of the first argument.  
  When given an argument of class \code{"htest"}, the \pkg{EnvStats} function \code{print.default} 
  calls the \pkg{EnvStats} function \code{link{print.htest}}, otherwise it calls the \R  
  function \code{\link[base:print.default]{print.default}}.

  The \pkg{EnvStats} functions \code{print} and \code{print.default} have been 
  created in order to comply with CRAN policies, because \pkg{EnvStats} contains a 
  modified version of the \R function \code{\link{print.htest}}.  When \pkg{EnvStats} 
  is loaded, objects of class \code{"htest"} will be printed using the 
  \pkg{EnvStats} version of \code{\link{print.htest}}.

  See the help files for the \R functions \code{\link[base:print]{print}} and 
  \code{\link[base:print.default]{print.default}}. 
}
\value{
  See the help files for the \R functions \code{\link[base:print]{print}} and 
  \code{\link[base:print.default]{print.default}}.  
}
\references{
  Chambers, J.M., and Hastie, T.J., eds. (1992). \emph{Statistical Models in S}. 
  Chapman and Hall/CRC, Boca Raton, FL.
}
\author{
  R Development Core Team for code for \R versions of \code{print} and \code{print.default}.
 
  Steven P. Millard for \pkg{EnvStats} version of \code{print.default}; \email{EnvStats@ProbStatInfo.com})
}
\seealso{
  \R help file for \code{\link[base:print]{print}},
  \R help file for \code{\link[base:print.default]{print.default}}, 
  \code{\link{print.htest}}.
}
\keyword{print}
