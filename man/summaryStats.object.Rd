\name{summaryStats.object}
\alias{summaryStats.object}
\docType{class}
\title{
  S3 Class "summaryStats"
}
\description{
  Objects of S3 class \code{"summaryStats"} are returned by the functions 
  \code{\link{summaryStats}} and \code{\link{summaryFull}}.
}
\details{
  Objects of S3 class \code{"summaryStats"} are matrices that contain 
  information about the summary statistics.
}
\section{Methods}{
  Generic functions that have methods for objects of class 
  \code{"summaryStats"} include: \cr
  \code{\link{print}}.
}
\value{
  \strong{Required Attributes} \cr
  The following attributes must be included in a legitimate matrix of 
  class \code{"summaryStats"}.

  \item{stats.in.rows}{logical scalar indicating whether the statistics 
    are stored by row \cr
    (\code{stats.in.rows=TRUE}) or by column 
    (\code{stats.in.rows=FALSE}).}
  \item{drop0trailing}{logical scalar indicating whether to drop trailing 0's 
    when printing the summary statistics.}
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\seealso{
  \code{\link{summaryStats}}, \code{\link{summaryFull}}.
}
\examples{
  # Create an object of class "summaryStats", then print it out. 
  #-------------------------------------------------------------

  summaryStats.obj <- summaryStats(TCE.mg.per.L ~ Well, 
    data = EPA.09.Table.9.1.TCE.df, digits = 3) 

  is.matrix(summaryStats.obj) 
  #[1] TRUE

  class(summaryStats.obj) 
  #[1] "summaryStats" 

  attributes(summaryStats.obj) 
  #$dim
  #[1] 2 8
  #
  #$dimnames
  #$dimnames[[1]]
  #[1] "Well.1" "Well.2"
  #
  #$dimnames[[2]]
  #[1] "N"       "Mean"    "SD"      "Median"  "Min"     "Max"    
  #[7] "NA's"    "N.Total"
  #
  #
  #$class
  #[1] "summaryStats"
  #
  #$stats.in.rows
  #[1] FALSE
  #
  #$drop0trailing
  #[1] TRUE

  summaryStats.obj 
  #        N  Mean    SD Median   Min  Max NA's N.Total
  #Well.1 14 0.063 0.079  0.031 0.004 0.25    1      15
  #Well.2 13 0.118 0.020  0.110 0.099 0.17    2      15

  #----------

  # Clean up
  #---------
  rm(summaryStats.obj)
}
\keyword{classes}

