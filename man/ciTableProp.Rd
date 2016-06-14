\name{ciTableProp}
\alias{ciTableProp}
\title{
  Table of Confidence Intervals for Proportion or Difference Between Two Proportions
}
\description{
  Create a table of confidence intervals for probability of "success" for a 
  binomial distribution or the difference between two proportions following 
  Bacchetti (2010), by varying the estimated proportion or differene between 
  the two estimated proportions given the sample size(s).
}
\usage{
  ciTableProp(n1 = 10, p1.hat = c(0.1, 0.2, 0.3), n2 = n1, 
    p2.hat.minus.p1.hat = c(0.2, 0.1, 0), sample.type = "two.sample", 
    ci.type = "two.sided", conf.level = 0.95, digits = 2, ci.method = "score", 
    correct = TRUE, tol = 10^-(digits + 1))
}
\arguments{
  \item{n1}{
  positive integer greater than 1 specifying the sample size when \cr
  \code{sample.type="one.sample"} or the sample size for group 1 when \cr
  \code{sample.type="two.sample"}.  The default value is \code{n1=10}.
}
  \item{p1.hat}{
  numeric vector of values between 0 and 1 indicating the estimated proportion 
  (\code{sample.type="one.sample"}) or the estimated proportion for group 1 \cr
  (\code{sample.type="two.sample"}).  The default value is \code{c(0.1, 0.2, 0.3)}.
  Missing (\code{NA}), undefined (\code{NaN}), an infinite 
  (\code{-Inf}, \code{Inf}) values are not allowed.
}
  \item{n2}{
  positive integer greater than 1 specifying the sample size for group 2 when 
  \code{sample.type="two.sample"}.  The default value is \code{n2=n1}, i.e., 
  equal sample sizes.  This argument is ignored when \code{sample.type="one.sample"}.
}
  \item{p2.hat.minus.p1.hat}{
  numeric vector indicating the assumed difference between the two sample proportions 
  when \code{sample.type="two.sample"}.  The default value is \code{c(0.2, 0.1, 0)}.  
  Missing (\code{NA}), undefined (\code{NaN}), an infinite 
  (\code{-Inf}, \code{Inf}) values are not allowed.  This argument is ignored when 
  \code{sample.type="one.sample"}.
}
  \item{sample.type}{
  character string specifying whether to create confidence intervals for the difference 
  between two proportions (\code{sample.type="two.sample"}; the default) or confidence 
  intervals for a single proportion (\code{sample.type="one.sample"}).
}
  \item{ci.type}{
  character string indicating what kind of confidence interval to compute.  The 
  possible values are \code{"two-sided"} (the default), \code{"lower"}, and 
  \code{"upper"}.
}
  \item{conf.level}{
  a scalar between 0 and 1 indicating the confidence level of the confidence interval.  
  The default value is \code{conf.level=0.95}.
}
  \item{digits}{
  positive integer indicating how many decimal places to display in the table.  The 
  default value is \code{digits=2}.
}
  \item{ci.method}{
  character string indicating the method to use to construct the confidence interval.  
  The default value is \code{ci.method="score"} (i.e., the score method; see the 
  help file for \code{\link{prop.test}}), which is the only method available when \cr
  \code{sample.type="two.sample"}.  When \code{sample.type="one.sample"}, you may 
  also set \code{ci.method="exact"} (i.e., the exact method).
}
  \item{correct}{
  logical scalar indicating whether to use the correction for continuity when \cr
  \code{ci.method="score"} (see the help file for \code{\link{prop.test}}).  The 
  default value is \code{correct=TRUE}.
}
  \item{tol}{
  numeric scalar indicating how close the values of the adjusted elements of \cr
  \code{p2.hat.minus.p1.hat} have to be in order to provide a simply display 
  of confidence intervals (see DETAILS section below).  The default value is \cr
  \code{tol=10^-(digits + 1)}.
}
}
\details{
  \emph{One-Sample Case} (\code{sample.type="one.sample"}) \cr
  For the one-sample case, the function \code{ciTableProp} calls the \R function 
  \code{\link{prop.test}} when \cr
  \code{ci.method="score"}, and calls the \R function 
  \code{\link{binom.test}}, when \code{ci.method="exact"}.  To ensure that the 
  user-supplied values of \code{p1.hat} are valid for the given user-supplied values 
  of \code{n1}, values for the argument \code{x} to the function 
  \code{\link{prop.test}} or \code{\link{binom.test}} are computed using the formula

  \code{x <- unique(round((p1.hat * n1), 0))}

  and the argument \code{p.hat} is then adjusted using the formula

  \code{p.hat <- x/n1}
  \cr

  \emph{Two-Sample Case} (\code{sample.type="two.sample"}) \cr
  For the two-sample case, the function \code{ciTableProp} calls the \R function 
  \code{\link{prop.test}}.  To ensure that the user-supplied values of \code{p1.hat} 
  are valid for the given user-supplied values of \code{n1}, the values for the 
  first component of the argument \code{x} to the function 
  \code{\link{prop.test}} are computed using the formula

  \code{x1 <- unique(round((p1.hat * n1), 0))} \cr

  and the argument \code{p1.hat} is then adjusted using the formula

  \code{p1.hat <- x1/n1}

  Next, the estimated proportions from group 2 are computed by adding together all 
  possible combinations from the elements of \code{p1.hat} and 
  \code{p2.hat.minus.p1.hat}.  These estimated proportions from group 2 are then 
  adjusted using the formulas:

  \code{x2.rep <- round((p2.hat.rep * n2), 0)} \cr
  \code{p2.hat.rep <- x2.rep/n2}

  If any of these adjusted proportions from group 2 are \eqn{\le 0} or \eqn{\ge 1} 
  the function terminates with a message indicating that impossible 
  values have been supplied. 

  In cases where the sample sizes are small there may be instances where the 
  user-supplied values of \code{p1.hat} and/or \code{p2.hat.minus.p1.hat} are not 
  attainable.  The argument \code{tol} is used to determine whether to return 
  the table in conventional form or whether it is necessary to modify the table 
  to include twice as many columns (see EXAMPLES section below).
}
\value{
  a data frame with elements that are character strings indicating the 
  confidence intervals.

  When \code{sample.type="two.sample"}, a data frame with the rows varying 
  the estimated proportion for group 1 (i.e., the values of \code{p1.hat}) and 
  the columns varying the estimated difference between the proportions from 
  group 2 and group 1 (i.e., the values of \code{p2.hat.minus.p1.hat}).  In cases 
  where the sample sizes are small, it may not be possible to obtain certain 
  differences for given values of \code{p1.hat}, in which case the returned 
  data frame contains twice as many columns indicating the actual difference 
  in one column and the compute confidence interval next to it (see EXAMPLES 
  section below).

  When \code{sample.type="one.sample"}, a 1-row data frame with the columns 
  varying the estimated proportion (i.e., the values of \code{p1.hat}).
}
\references{
  Bacchetti, P. (2010).  Current sample size conventions: Flaws, Harms, and 
    Alternatives.  \emph{BMC Medicine} \bold{8}, 17--23.

  Also see the references in the help files for \code{\link{prop.test}} and 
  \code{\link{binom.test}}.
}
\author{
    Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  Bacchetti (2010) presents strong arguments against the current convention in 
  scientific research for computing sample size that is based on formulas that 
  use a fixed Type I error (usually 5\%) and a fixed minimal power (often 80\%) 
  without regard to costs.  He notes that a key input to these formulas is a 
  measure of variability (usually a standard deviation) that is difficult to 
  measure accurately "unless there is so much preliminary data that the study 
  isn't really needed."  Also, study designers often avoid defining what a 
  scientifically meaningful difference is by presenting sample size results in 
  terms of the effect size (i.e., the difference of interest divided by the 
  elusive standard deviation).  Bacchetti (2010) encourages study designers to use 
  simple tables in a sensitivity analysis to see what results of a study may look 
  like for low, moderate, and high rates of variability and large, intermediate, 
  and no underlying differences in the populations or processes being studied.
}
\seealso{
  \code{\link{prop.test}}, \code{\link{binom.test}}, \code{\link{ciTableMean}}, 
  \code{\link{ciBinomHalfWidth}}, \code{\link{ciBinomN}}, 
  \code{\link{plotCiBinomDesign}}.
}
\examples{
  # Reproduce Table 1 in Bacchetti (2010).  This involves planning a study with 
  # n1 = n2 = 935 subjects per group, where Group 1 is the control group and 
  # Group 2 is the treatment group.  The outcome in the study is proportion of 
  # subjects with serious outcomes or death.  A negative value for the difference 
  # in proportions between groups (Group 2 proportion - Group 1 proportion) 
  # indicates the treatment group has a better outcome.  In this table, the 
  # proportion of subjects in Group 1 with serious outcomes or death is set 
  # to 3%, 6.5%, and 12%, and the difference in proportions between the two 
  # groups is set to -2.8 percentage points, -1.4 percentage points, and 0.

  ciTableProp(n1 = 935, p1.hat = c(0.03, 0.065, 0.12), n2 = 935, 
    p2.hat.minus.p1.hat = c(-0.028, -0.014, 0), digits = 3)
  #                  Diff=-0.028      Diff=-0.014           Diff=0
  #P1.hat=0.030 [-0.040, -0.015] [-0.029,  0.001] [-0.015,  0.015]
  #P1.hat=0.065 [-0.049, -0.007] [-0.036,  0.008] [-0.022,  0.022]
  #P1.hat=0.120 [-0.057,  0.001] [-0.044,  0.016] [-0.029,  0.029]

  #==========

  # Show how the returned data frame has to be modified for cases of small 
  # sample sizes where not all user-supplied differenes are possible.

  ciTableProp(n1 = 5, n2 = 5, p1.hat = c(0.3, 0.6, 0.12), p2.hat = c(0.2, 0.1, 0))
  #           Diff            CI Diff            CI Diff            CI
  #P1.hat=0.4  0.2 [-0.61, 1.00]  0.0 [-0.61, 0.61]    0 [-0.61, 0.61]
  #P1.hat=0.6  0.2 [-0.55, 0.95]  0.2 [-0.55, 0.95]    0 [-0.61, 0.61]
  #P1.hat=0.2  0.2 [-0.55, 0.95]  0.2 [-0.55, 0.95]    0 [-0.50, 0.50]

  #==========

  # Suppose we are planning a study to compare the proportion of nondetects at 
  # a background and downgradient well, and we can use ciTableProp to look how 
  # the confidence interval for the difference between the two proportions using 
  # say 36 quarterly samples at each well varies with the observed estimated 
  # proportions.  Here we'll let the argument "p1.hat" denote the proportion of 
  # nondetects observed at the downgradient well and set this equal to 
  # 20%, 40% and 60%.  The argument "p2.hat.minus.p1.hat" represents the proportion 
  # of nondetects at the background well minus the proportion of nondetects at the 
  # downgradient well.

  ciTableProp(n1 = 36, p1.hat = c(0.2, 0.4, 0.6), n2 = 36, 
    p2.hat.minus.p1.hat = c(0.3, 0.15, 0))
  #                Diff=0.31     Diff=0.14        Diff=0
  #P1.hat=0.19 [ 0.07, 0.54] [-0.09, 0.37] [-0.18, 0.18]
  #P1.hat=0.39 [ 0.06, 0.55] [-0.12, 0.39] [-0.23, 0.23]
  #P1.hat=0.61 [ 0.09, 0.52] [-0.10, 0.38] [-0.23, 0.23]

  # We see that even if the observed difference in the proportion of nondetects 
  # is about 15 percentage points, all of the confidence intervals for the 
  # difference between the proportions of nondetects at the two wells contain 0, 
  # so if a difference of 15 percentage points is important to substantiate, we 
  # may need to increase our sample sizes.
}
\keyword{ distribution }
\keyword{ design }
\keyword{ htest }