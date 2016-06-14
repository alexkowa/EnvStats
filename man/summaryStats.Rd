\name{summaryStats}
\alias{summaryStats}
\alias{summaryStats.formula}
\alias{summaryStats.default}
\alias{summaryStats.factor}
\alias{summaryStats.logical}
\alias{summaryStats.character}
\alias{summaryStats.data.frame}
\alias{summaryStats.matrix}
\alias{summaryStats.list}
\title{
  Summary Statistics
}
\description{
  \code{summaryStats} is a generic function used to produce summary statistics, confidence intervals, 
  and results of hypothesis tests.  The function invokes particular \code{\link{methods}} which 
  depend on the \code{\link{class}} of the first argument. 

  The summary statistics include: sample size, number of missing values, 
  mean, standard deviation, median, min, and max.  Optional additional summary statistics include 
  1st quartile, 3rd quartile, and stadard error.
}
\usage{
summaryStats(object, ...)

\method{summaryStats}{formula}(object, data = NULL, subset, 
  na.action = na.pass, ...)

\method{summaryStats}{default}(object, group = NULL, 
    drop.unused.levels = TRUE, se = FALSE, quartiles = FALSE, 
    digits = max(3, getOption("digits") - 3), 
    digit.type = "round", drop0trailing = TRUE, 
    show.na = TRUE, show.0.na = FALSE, p.value = FALSE, 
    p.value.digits = 2, p.value.digit.type = "signif", 
    test = "parametric", paired = FALSE, test.arg.list = NULL, 
    combine.groups = p.value, rm.group.na = TRUE, 
    group.p.value.type = NULL, alternative = "two.sided", 
    ci = NULL, ci.between = NULL, conf.level = 0.95, 
    stats.in.rows = FALSE, 
    data.name = deparse(substitute(object)), ...)

\method{summaryStats}{factor}(object, group = NULL, 
    drop.unused.levels = TRUE, 
    digits = max(3, getOption("digits") - 3), 
    digit.type = "round", drop0trailing = TRUE,  
    show.na = TRUE, show.0.na = FALSE, p.value = FALSE, 
    p.value.digits = 2, p.value.digit.type = "signif", 
    test = "chisq", test.arg.list = NULL, combine.levels = TRUE, 
    combine.groups = FALSE, rm.group.na = TRUE, 
    ci = p.value & test != "chisq", conf.level = 0.95, 
    stats.in.rows = FALSE, ...)

\method{summaryStats}{character}(object, ...)

\method{summaryStats}{logical}(object, ...)

\method{summaryStats}{data.frame}(object, ...)

\method{summaryStats}{matrix}(object, ...)

\method{summaryStats}{list}(object, ...)
}
\arguments{
  \item{object}{
  an object for which summary statistics are desired.  In the default method, 
  the argument \code{object} can be a numeric vector, factor, character vector, 
  logical vector, data frame, matrix, or list.  
  When \code{object} is a character or logical vector, it is coerced to 
  be a factor.  
  When \code{object} is a data frame, all columns must be numeric or all columns must be factors.  
  When \code{object} is a matrix, it must be a numeric or character matrix.  
  When \code{object} is a list, all components must be numeric vectors 
  or all components must be factors.
  In the formula method, a symbolic specification of the form \code{y ~ g} 
  can be given, indicating the observations in the vector \code{y} are to be grouped 
  according to the levels of the factor \code{g} (the form \code{y ~ 1} indicates no grouping).  
  \code{NA}s are allowed in the data.
}
  \item{data}{
  when \code{object} is a formula, \code{data} specifies an optional data frame, list or 
  environment (or object coercible by \code{as.data.frame} to a data frame) containing the 
  variables in the model.  If not found in \code{data}, the variables are taken from 
  \code{environment(formula)}, typically the environment from which \cr
  \code{summaryStats} is called.
}
  \item{subset}{
  when \code{object} is a formula, \code{subset} specifies an optional vector specifying 
  a subset of observations to be used.
}
  \item{na.action}{
  when \code{object} is a formula, \code{na.action} specifies a function which indicates 
  what should happen when the data contain \code{NA}s. The default is \code{\link{na.pass}}.
}
  \item{group}{
  when \code{object} is a numeric vector or factor, \code{group} is a factor or character vector 
  indicating which group each observation belongs to.  When \code{object} is a matrix or data frame
  this argument is ignored and the columns define the groups.  When \code{object} is a formula, 
  this argument is ignored and the right-hand side of the formula specifies the grouping variable.
}
  \item{drop.unused.levels}{
  when \code{drop.unused.levels=TRUE}, groups with no observations are dropped.
}
  \item{se}{
  for numeric data, logical scalar indicating whether to include 
  the standard error of the mean in the summary statistics.  
  The default value is \code{se=FALSE}.
}
  \item{quartiles}{
  for numeric data, logical scalar indicating whether to include 
  the estimated 25th and 75th percentiles in the summary statistics.  
  The default value is \cr
  \code{quartiles=FALSE}.
}
  \item{digits}{
  integer indicating the number of digits to use for the summary statistics.  
  When \code{digit.type="signif"}, \code{digits} indicates the number of significant 
  digits.  When \code{digit.type="round"}, \code{digits} indicates the number of 
  decimal places to round to.  The default value is \code{max(3, getOption("digits") - 3)}, 
  that is, the maximum of 3 versus the current setting of the \code{"digits"} component 
  of \code{.Options} minus 3.
}
  \item{digit.type}{
  character string indicating whether the \code{digits} argument refers to significant digits 
  (\code{digit.type="signif"}), or how many decimal places to round to \cr
  (\code{digit.type="round"}, the default).
}
  \item{drop0trailing}{
  logical scalar indicating whether to drop trailing 0's when printing the summary statistics.  
  The value of this argument is added as an attribute to the returned list and is used by the 
  \code{\link{print.summaryStats}} function.  The default value is \code{TRUE}.
}
  \item{show.na}{
  logical scalar indicating whether to return the number of missing values.  
  The default value is \code{show.na=TRUE}.
}
  \item{show.0.na}{
  logical scalar indicating whether to diplay the number of missing values in the case when 
  there are no missing values.  The default value is \cr
  \code{show.0.na=FALSE}.
}
  \item{p.value}{
  logical scalar indicating whether to return the p-value associated with a test of hypothesis.  
  The default value is \code{p.value=FALSE}.  
  \bold{Numeric data:}  if there are no groups the p-value is associated with the t-test to test 
  whether the mean is different from 0; if there are groups see the explanation for the argument 
  \code{group.p.value.type} below.  \bold{Factors:}  the p-value is associated with the test 
  specified by the argument \code{test} (see below).
}
  \item{p.value.digits}{
  integer indicating the number of digits to use for the p-value.  When \cr
  \code{p.value.digit.type="signif"}, \code{p.value.digits} indicates the 
  number of significant digits.  When \code{p.value.digit.type="round"}, \code{p.value.digits} 
  indicates the number of decimal places to round to.  The default value is \cr
  \code{p.value.digits=2}.
}
  \item{p.value.digit.type}{
  character string indicating whether the \code{p.value.digits} argument refers to 
  significant digits (\code{p.value.digit.type="signif"}, the default), or how many 
  decimal places to round to (\code{p.value.digit.type="round"}).
}
  \item{test}{
  \bold{Numeric data:}  character string indicating whether to compute p-values and confidence 
  intervals based on parametric (\code{test="parametric"}; the default) or nonparametric 
  (\code{test="nonparametric"}) tests when \code{p.value=TRUE} and/or \code{ci=TRUE}.  
  When \code{test="parametric"}, confidence intervals are based on the t-test (see \code{\link{t.test}}) 
  and p-values are based on the t-test or F-test (see \code{\link{anova.lm}}).  
  When \code{test="nonparametric"}, confidence intervals are based on the Wilcoxon rank sum test 
  (see \code{\link{wilcox.test}}) and p-values are based on the Wilcoxon rank sum test or 
  the Kruskal-Wallis rank sum test \cr
  (see \code{\link{kruskal.test}}).

  \bold{Factors:}  character string indicating which test to perform when \cr
  \code{p.value=TRUE}.  Possible values are 
  \code{test="chisq"} for the chi-squared test as performed by the function \code{\link{chisq.test}} (the default), 
  \code{test="prop"} for the chi-squared test as performed by the function \code{\link{prop.test}}, 
  \code{test="fisher"} for Fisher's exact test as performed by the function \code{\link{fisher.test}}, and \cr
  \code{test="binom"} for the one-sample exact binomial test as performed by \cr
  \code{\link{binom.test}}.  
  The chi-squared test as performed by \code{\link{prop.test}} is only available when the number of levels 
  in \code{object} is 2 and either \code{group} is not supplied or the number of levels in \code{group} is 2.  
  Fisher's exact test is only available when the number of levels in \code{group} is \eqn{\ge 2}.  
  The exact binomial test is only available when \code{group} is not supplied and the number of levels 
  in \code{object} is 2. 
}
  \item{paired}{
  applicable only to the case when there are two groups: \cr
  logical scalar indicating whether the observations in the first group are paired with those 
  in the second group.  The default is \code{paired=FALSE}.  
  NOTE:  If the argument \code{test.arg.list} (see below) contains a component named \code{paired}, 
  the value of that component is set to the value of the argument \code{paired}.
}
  \item{test.arg.list}{
  a list with additional arguments to pass to the test used to compute p-values and confidence 
  intervals.  For numeric data, when \code{test="parametric"}, \cr
  \code{p.value=TRUE}, 
  \code{group.p.value.type="between"} and there are two groups, if this argument is 
  \code{NULL} or does not contain a component named \cr
  \code{var.equal}, it will be modified to contain the component \code{var.equal=TRUE}.  
  Note that this overrides the default behavior of \code{\link{t.test}} when there are two groups.  \cr
  NOTE: If \code{test.arg.list} contains a component named \code{paired}, the value of 
  that component is set to the value of the argument \code{paired} (see above).
}
  \item{combine.groups}{
  logical scalar indicating whether to show summary statistics for all groups combined.  
  \bold{Numeric data:}  the default value is \code{TRUE} if \code{p.value=TRUE}, otherwise \code{FALSE}.  
  \bold{Factors:}  the default value is \code{FALSE}.
}
  \item{rm.group.na}{
  logical scalar indicating whether to remove missing values from the \code{group} argument.  
  If \code{rm.group.na=FALSE} and \code{group} contains missing values then an error is returned.  
  If \code{rm.group.na=TRUE} and \code{group} contains missing values then a warning is issued.  
  By default \code{rm.group.na=TRUE}.
}
  \item{group.p.value.type}{
  for numeric data, character string indicating which p-value(s) to compute when 
  there is more than one group.  When \code{group.p.value.type="between"} (the default when 
  \code{combine.groups=TRUE}), the p-value is associated with the two-sample t-test (or the 
  Wilcoxon rank sum test) in the case of two groups, and the one-way analysis of variance F-test 
  (or Krukal-Wallis test) in the case of three or more groups to test whether the group 
  means (locations) are different from each other.  When \code{group.p.value.type="within"} 
  (the default when \code{combine.groups=FALSE}), the computed p-values for each group are 
  associated with the one-sample t-test (or Wilcox signed rank test) to test whether the group 
  mean (location) is different from 0.
}
  \item{alternative}{
  for numeric data, character string indicating which alternative to assume 
  for p-values and confidence intervals.  Possible values are \code{"two.sided"} (the default), 
  \code{"less"}, and \code{"greater"}.  This argument is 
  ignored for p-values in the case of three or more groups when \code{group.p.value.type="between"}, 
  and is ignored for confidence intervals in the case of three or more groups when \cr
  \code{ci.between=TRUE}.
}
  \item{ci}{
  \bold{Numeric data:}  logical scalar indicating whether to compute a confidence interval 
  for the mean or each group mean.  The default value is \code{FALSE} unless 
  \code{p.value=TRUE} and there are no groups, or when 
  \code{p.value=TRUE} and there are groups and \code{group.p.value.type="within"}. 

  \bold{Factors:}  logical scalar indicating whether to compute a confidence interval.  A confidence 
  interval is computed only if the number of levels in \code{object} is 2.  When \code{group} is not 
  supplied, if \code{ci=TRUE} and \code{test="prop"} or \cr
  \code{test="binom"}, a confidence interval for 
  the \emph{percent} (not probability) of the first level of \code{object} is computed.  
  When \code{group} is supplied and the number of levels in \code{group} is 2, if \code{ci=TRUE} and 
  \code{test="prop"}, a confidence interval for the difference between \emph{percents} (not proportions) 
  is computed, and if \code{test="fisher"} a confidence interval for the odds ratio is computed.
}
  \item{ci.between}{
  for numeric data, logical scalar indicating whether to compute a confidence interval 
  for the difference between group means when there are two groups.  
  The default value is \code{ci.between=TRUE} when \code{p.value=TRUE} 
  and \cr
  \code{group.p.value.type="between"}, otherwise this argument is ignored.
}
  \item{conf.level}{
  numeric scalar between 0 and 1 indicating the confidence level associated with the confidence intervals.  
  The default value is \code{conf.level=0.95}.
}
  \item{stats.in.rows}{
  logical scalar indicating whether to show the summary statistics in the rows or columns of the 
  output.  The default is \code{stats.in.rows=FALSE}.
}
  \item{data.name}{
  character string indicating the name of the data used for the summary statistics.
}
  \item{combine.levels}{
  for factors, a logical scalar indicating whether to compute summary statistics based on 
  combining all levels of a factor.
}
  \item{\dots}{
  additional arguments affecting the summary statistics produced.
}
}
\value{
  an object of class \code{"summaryStats"} (see \code{\link{summaryStats.object}}.  
  Objects of class \code{"summaryStats"} are numeric matrices that contain the 
  summary statisics produced by a call to \code{summaryStats} or \code{\link{summaryFull}}.  
  These objects have a special printing method that by default removes 
  trailing zeros for sample size entries and prints blanks for statistics that are 
  normally displayed as \code{NA} (see \code{\link{print.summaryStats}}).

  Summary statistics for numeric data include sample size, mean, standard deviation, median, 
  min, and max.  Options include the standard error of the mean (when \code{se=TRUE}), 
  the estimated quartiles (when \code{quartiles=TRUE}), p-values (when \code{p.value=TRUE}),
  and/or confidence intervals (when \code{ci=TRUE} and/or \code{ci.between=TRUE}).

  Summary statistics for factors include the sample size for each level of the factor and the 
  percent of the total for that level.  Options include a p-value (when \code{p.value=TRUE}).

  Note that unlike the \R function \code{\link{summary}} and the \pkg{EnvStats} function 
  \code{\link{summaryFull}}, by default the \code{digits} argument for the \pkg{EnvStats} function 
  \code{summaryStats} refers to how many decimal places to round to, not how many 
  significant digits to use (see the explanation of the argument \code{digit.type} above).
}
\references{
  Berthouex, P.M., and L.C. Brown. (2002). 
  \emph{Statistics for Environmental Engineers, Second Edition}. 
  Lewis Publishers, Boca Raton, FL.

  Millard, S.P., and N.K. Neerchal. (2001). \emph{Environmental Statistics with S-PLUS}. 
  CRC Press, Boca Raton, FL.

  Zar, J.H. (2010). \emph{Biostatistical Analysis}. Fifth Edition. 
  Prentice-Hall, Upper Saddle River, NJ, Chapter 24.
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\seealso{
  \code{\link{summary}}, \code{\link{summaryFull}}, \code{\link{t.test}}, \code{\link{anova.lm}}, 
  \code{\link{wilcox.test}}, \code{\link{kruskal.test}}, 
  \code{\link{chisq.test}}, \cr
  \code{\link{fisher.test}}, \code{\link{binom.test}}.
}
\examples{
  # The guidance document USEPA (1994b, pp. 6.22--6.25)
  # contains measures of 1,2,3,4-Tetrachlorobenzene (TcCB)
  # concentrations (in parts per billion) from soil samples
  # at a Reference area and a Cleanup area. These data are strored
  # in the data frame EPA.94b.tccb.df.

  #----------
  # First, create summary statistics by area based on the log-transformed data. 

  summaryStats(log10(TcCB) ~ Area, data = EPA.94b.tccb.df)
  #           N    Mean     SD  Median     Min    Max
  #Cleanup   77 -0.2377 0.5908 -0.3665 -1.0458 2.2270
  #Reference 47 -0.2691 0.2032 -0.2676 -0.6576 0.1239

  #----------
  # Now create summary statistics by area based on the log-transformed data 
  # and use the t-test to compare the areas.

  summaryStats(log10(TcCB) ~ Area, data = EPA.94b.tccb.df, p.value = TRUE)

  summaryStats(log10(TcCB) ~ Area, data = EPA.94b.tccb.df, 
    p.value = TRUE, stats.in.rows = TRUE)
  #                Cleanup  Reference Combined
  #N                77       47       124     
  #Mean             -0.2377  -0.2691   -0.2496
  #SD                0.5908   0.2032    0.481 
  #Median           -0.3665  -0.2676   -0.3143
  #Min              -1.0458  -0.6576   -1.0458
  #Max               2.227    0.1239    2.227 
  #Diff                                -0.0313
  #p.value.between                      0.73  
  #95%.LCL.between                     -0.2082
  #95%.UCL.between                      0.1456

  #====================================================================

  # Page 9-3 of USEPA (2009) lists trichloroethene 
  # concentrations (TCE; mg/L) collected from groundwater at two wells.
  # Here, the seven non-detects have been set to their detection limit.

  #----------
  # First, compute summary statistics for all TCE observations.

  summaryStats(TCE.mg.per.L ~ 1, data = EPA.09.Table.9.1.TCE.df, 
    digits = 3, data.name = "TCE")
  #     N Mean    SD Median   Min  Max NA's N.Total
  #TCE 27 0.09 0.064    0.1 0.004 0.25    3      30

  summaryStats(TCE.mg.per.L ~ 1, data = EPA.09.Table.9.1.TCE.df,
    se = TRUE, quartiles = TRUE, digits = 3, data.name = "TCE")
  #     N Mean    SD    SE Median   Min  Max 1st Qu. 3rd Qu. NA's N.Total
  #TCE 27 0.09 0.064 0.012    0.1 0.004 0.25   0.031    0.12    3      30

  #----------
  # Now compute summary statistics by well.

  summaryStats(TCE.mg.per.L ~ Well, data = EPA.09.Table.9.1.TCE.df, 
    digits = 3)
  #        N  Mean    SD Median   Min  Max NA's N.Total
  #Well.1 14 0.063 0.079  0.031 0.004 0.25    1      15
  #Well.2 13 0.118 0.020  0.110 0.099 0.17    2      15

  summaryStats(TCE.mg.per.L ~ Well, data = EPA.09.Table.9.1.TCE.df,
    digits = 3, stats.in.rows = TRUE)
  #        Well.1 Well.2
  #N       14     13    
  #Mean     0.063  0.118
  #SD       0.079  0.02 
  #Median   0.031  0.11 
  #Min      0.004  0.099
  #Max      0.25   0.17 
  #NA's     1      2    
  #N.Total 15     15 

  # If you want to keep trailing 0's, use the drop0trailing argument:
  summaryStats(TCE.mg.per.L ~ Well, data = EPA.09.Table.9.1.TCE.df,
    digits = 3, stats.in.rows = TRUE, drop0trailing = FALSE)
  #        Well.1 Well.2
  #N       14.000 13.000
  #Mean     0.063  0.118
  #SD       0.079  0.020
  #Median   0.031  0.110
  #Min      0.004  0.099
  #Max      0.250  0.170
  #NA's     1.000  2.000
  #N.Total 15.000 15.000

  #====================================================================

  # Page 13-3 of USEPA (2009) lists iron concentrations (ppm) in 
  # groundwater collected from 6 wells.  

  #----------
  # First, compute summary statistics for each well.

  summaryStats(Iron.ppm ~ Well, data = EPA.09.Ex.13.1.iron.df, 
    combine.groups = FALSE, digits = 2, stats.in.rows = TRUE)
  #       Well.1 Well.2 Well.3 Well.4 Well.5 Well.6
  #N        4      4      4      4      4      4   
  #Mean    47.01  55.73  90.86  70.43 145.24 156.32
  #SD      12.4   20.34  59.35  25.95  92.16  51.2 
  #Median  50.05  57.05  76.73  76.95 137.66 171.93
  #Min     29.96  32.14  39.25  34.12  60.95  83.1 
  #Max     57.97  76.71 170.72  93.69 244.69 198.34

  #----------
  # Note the large differences in standard deviations between wells.
  # Compute summary statistics for log(Iron), by Well.

  summaryStats(log(Iron.ppm) ~ Well, data = EPA.09.Ex.13.1.iron.df, 
    combine.groups = FALSE, digits = 2, stats.in.rows = TRUE)
  #       Well.1 Well.2 Well.3 Well.4 Well.5 Well.6
  #N      4      4      4      4      4      4     
  #Mean   3.82   3.97   4.35   4.19   4.8    5     
  #SD     0.3    0.4    0.66   0.45   0.7    0.4   
  #Median 3.91   4.02   4.29   4.34   4.8    5.14  
  #Min    3.4    3.47   3.67   3.53   4.11   4.42  
  #Max    4.06   4.34   5.14   4.54   5.5    5.29

  #----------
  # Include confidence intervals for the mean log(Fe) concentration
  # at each well, and also the p-value from the one-way 
  # analysis of variance to test for a difference in well means.

  summaryStats(log(Iron.ppm) ~ Well, data = EPA.09.Ex.13.1.iron.df, 
    digits = 1, ci = TRUE, p.value = TRUE, stats.in.rows = TRUE)
  #                Well.1 Well.2 Well.3 Well.4 Well.5 Well.6 Combined
  #N                4      4      4      4      4      4     24      
  #Mean             3.8    4      4.3    4.2    4.8    5      4.4    
  #SD               0.3    0.4    0.7    0.5    0.7    0.4    0.6    
  #Median           3.9    4      4.3    4.3    4.8    5.1    4.3    
  #Min              3.4    3.5    3.7    3.5    4.1    4.4    3.4    
  #Max              4.1    4.3    5.1    4.5    5.5    5.3    5.5    
  #95%.LCL          3.3    3.3    3.3    3.5    3.7    4.4    4.1    
  #95%.UCL          4.3    4.6    5.4    4.9    5.9    5.6    4.6    
  #p.value.between                                            0.025 

  #====================================================================

  # Using the built-in dataset HairEyeColor, summarize the frequencies 
  # of hair color and test whether there is a difference in proportions.
  # NOTE:  The data that was originally factor data has already been 
  #        collapsed into frequency counts by catetory in the object 
  #        HairEyeColor.  In the examples in this section, we recreate 
  #        the factor objects in order to show how summaryStats works 
  #        for factor objects.

  Hair <- apply(HairEyeColor, 1, sum)
  Hair
  #Black Brown   Red Blond 
  #  108   286    71   127

  Hair.color <- names(Hair)
  Hair.fac <- factor(rep(Hair.color, times = Hair), 
    levels = Hair.color)

  #----------

  # Compute summary statistics and perform the chi-square test 
  # for equal proportions of hair color

  summaryStats(Hair.fac, digits = 1, p.value = TRUE)
  #           N   Pct ChiSq_p
  #Black    108  18.2        
  #Brown    286  48.3        
  #Red       71  12.0        
  #Blond    127  21.5        
  #Combined 592 100.0 2.5e-39

  #----------
  # Now test the hypothesis that 10% of the population from which 
  # this sample was drawn has Red hair, and compute a 95% confidence 
  # interval for the percent of subjects with red hair.

  Red.Hair.fac <- factor(Hair.fac == "Red", levels = c(TRUE, FALSE), 
    labels = c("Red", "Not Red"))

  summaryStats(Red.Hair.fac, digits = 1, p.value = TRUE, 
    ci = TRUE, test = "binom", test.arg.list = list(p = 0.1))
  #           N Pct Exact_p 95%.LCL 95%.UCL
  #Red       71  12             9.5    14.9
  #Not Red  521  88                        
  #Combined 592 100    0.11

  #----------
  # Now test whether the percent of people with Green eyes is the 
  # same for people with and without Red hair.

  HairEye <- apply(HairEyeColor, 1:2, sum)
  Hair.color <- rownames(HairEye)
  Eye.color  <- colnames(HairEye)

  n11 <-     HairEye[Hair.color == "Red", Eye.color == "Green"]
  n12 <- sum(HairEye[Hair.color == "Red", Eye.color != "Green"])
  n21 <- sum(HairEye[Hair.color != "Red", Eye.color == "Green"])
  n22 <- sum(HairEye[Hair.color != "Red", Eye.color != "Green"])

  Hair.fac <- factor(rep(c("Red", "Not Red"), c(n11+n12, n21+n22)), 
    levels = c("Red", "Not Red"))
  Eye.fac  <- factor(c(rep("Green", n11), rep("Not Green", n12), 
    rep("Green", n21), rep("Not Green", n22)), 
    levels = c("Green", "Not Green"))


  #----------
  # Here are the results using the chi-square test and computing 
  # confidence limits for the difference between the two percentages

  summaryStats(Eye.fac, group = Hair.fac, digits = 1, 
    p.value = TRUE, ci = TRUE, test = "prop", 
    stats.in.rows = TRUE, test.arg.list = list(correct = FALSE))
  #                Green Not Green Combined
  #Red(N)           14    57        71     
  #Red(Pct)         19.7  80.3     100     
  #Not Red(N)       50   471       521     
  #Not Red(Pct)      9.6  90.4     100     
  #ChiSq_p                           0.01  
  #95%.LCL.between                   0.5   
  #95%.UCL.between                  19.7

  #----------
  # Here are the results using Fisher's exact test and computing 
  # confidence limits for the odds ratio

  summaryStats(Eye.fac, group = Hair.fac, digits = 1, 
    p.value = TRUE, ci = TRUE, test = "fisher", 
    stats.in.rows = TRUE)
  #             Green Not Green Combined
  #Red(N)        14    57        71     
  #Red(Pct)      19.7  80.3     100     
  #Not Red(N)    50   471       521     
  #Not Red(Pct)   9.6  90.4     100     
  #Fisher_p                       0.015 
  #95%.LCL.OR                     1.1   
  #95%.UCL.OR                     4.6 

  rm(Hair, Hair.color, Hair.fac, Red.Hair.fac, HairEye, Eye.color, 
    n11, n12, n21, n22, Eye.fac)

  #====================================================================

  # The data set EPA.89b.cadmium.df contains information on 
  # cadmium concentrations in groundwater collected from a
  # background and compliance well.  Compare detection frequencies 
  # between the well types and test for a difference using 
  # Fisher's exact test.

  summaryStats(factor(Censored) ~ Well.type, data = EPA.89b.cadmium.df, 
    digits = 1, p.value = TRUE, test = "fisher")


  summaryStats(factor(Censored) ~ Well.type, data = EPA.89b.cadmium.df, 
    digits = 1, p.value = TRUE, test = "fisher", stats.in.rows = TRUE)
  #                FALSE TRUE  Combined
  #Background(N)     8    16    24     
  #Background(Pct)  33.3  66.7 100     
  #Compliance(N)    24    40    64     
  #Compliance(Pct)  37.5  62.5 100     
  #Fisher_p                      0.81  
  #95%.LCL.OR                    0.3   
  #95%.UCL.OR                    2.5

  #====================================================================

  #--------------------
  # Paired Observations
  #--------------------

  # The data frame ACE.13.TCE.df contians paired observations of 
  # trichloroethylene (TCE; mg/L) at 10 groundwater monitoring wells 
  # before and after remediation.
  #
  # Compare TCE concentrations before and after remediation and 
  # use a paired t-test to test for a difference between periods.

  summaryStats(TCE.mg.per.L ~ Period, data = ACE.13.TCE.df, 
    p.value = TRUE, paired = TRUE)


  summaryStats(TCE.mg.per.L ~ Period, data = ACE.13.TCE.df, 
    p.value = TRUE, paired = TRUE, stats.in.rows = TRUE)
  #                       Before   After    Combined
  #N                       10       10       20     
  #Mean                    21.624    3.6329  12.6284
  #SD                      13.5113   3.5544  13.3281
  #Median                  20.3      2.48     8.475 
  #Min                      5.96     0.272    0.272 
  #Max                     41.5     10.7     41.5   
  #Diff                                     -17.9911
  #paired.p.value.between                     0.0027
  #95%.LCL.between                          -27.9097
  #95%.UCL.between                           -8.0725

  #==========

  # Repeat the last example, but use a one-sided alternative since 
  # remediation should decrease TCE concentration.

  summaryStats(TCE.mg.per.L ~ Period, data = ACE.13.TCE.df, 
    p.value = TRUE, paired = TRUE, alternative = "less")


  summaryStats(TCE.mg.per.L ~ Period, data = ACE.13.TCE.df, 
    p.value = TRUE, paired = TRUE, alternative = "less", 
    stats.in.rows = TRUE)
  #                            Before   After    Combined
  #N                            10       10       20     
  #Mean                         21.624    3.6329  12.6284
  #SD                           13.5113   3.5544  13.3281
  #Median                       20.3      2.48     8.475 
  #Min                           5.96     0.272    0.272 
  #Max                          41.5     10.7     41.5   
  #Diff                                          -17.9911
  #paired.p.value.between.less                     0.0013
  #95%.LCL.between                                   -Inf
  #95%.UCL.between                                -9.9537
}
\keyword{ univar }
\keyword{ htest }