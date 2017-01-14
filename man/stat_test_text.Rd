\name{stat_test_text}
\alias{stat_test_text}
\title{
  Add Text to a ggplot2 Plot Indicating the Results of a Hypothesis Test  
}
\description{
  For a strip plot or scatterplot produced using the package \link[ggplot2]{ggplot2}
  (e.g., with \code{\link[ggplot2]{geom_point}}), 
  add text indicating the results of a hypothesis test comparing locations 
  betweeen groups, where the groups are defined based on the unique \eqn{x}-values.
}
\usage{
  stat_test_text(mapping = NULL, data = NULL, 
    geom = ifelse(text.box, "label", "text"), position = "identity", 
    na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, 
    y.pos = NULL, y.expand.factor = 0.35, test = "parametric", 
    paired = FALSE, test.arg.list = list(), two.lines = TRUE, 
    p.value.digits = 3, p.value.digit.type = "round", 
    location.digits = 1, location.digit.type = "round", 
    nsmall = ifelse(location.digit.type == "round", location.digits, 0), 
    text.box = FALSE, alpha = 1, angle = 0, color = "black", 
    family = "", fontface = "plain", hjust = 0.5, 
    label.padding = ggplot2::unit(0.25, "lines"), 
    label.r = ggplot2::unit(0.15, "lines"), label.size = 0.25, 
    lineheight = 1.2, size = 4, vjust = 0.5, ...)
}
\arguments{
  \item{mapping, data, position, na.rm, show.legend, inherit.aes}{
  See the help file for \code{\link[ggplot2]{geom_text}}.
}
  \item{geom}{
  Character string indicating which \code{geom} to use to display the text.   
  Setting \code{geom="text"} will use \code{\link[ggplot2]{geom_text}} to display the text, and 
  setting \code{geom="label"} will use \code{\link[ggplot2]{geom_label}} to display the text.  
  The default value is \code{geom="text"} unless the user sets \code{text.box=TRUE}.
}
  \item{y.pos}{
  Numeric scalar indicating the \eqn{y}-position of the text (i.e., the value of the 
  argument \code{y} that will be used in the call to \code{\link[ggplot2]{geom_text}} or 
  \code{\link[ggplot2]{geom_label}}).  The default value is \code{y.pos=NULL}, in which 
  case \code{y.pos} is set to the maximum value of all \eqn{y}-values plus 
  a proportion of the range of all \eqn{y}-values, where the proportion is 
  determined by the argument \code{y.expand.factor} (see below).
}
  \item{y.expand.factor}{
  For the case when \code{y.pos=NULL}, a numeric scalar indicating the proportion 
  by which the range of all \eqn{y}-values should be multiplied by before adding 
  this value to the maximum value of all \eqn{y}-values in order to compute the 
  value of the argument \code{y.pos} (see above).  
  The default value is \code{y.expand.factor=0.35}.
}
  \item{test}{
  A character string indicating whether to use a standard parametric test 
  (\code{test="parametric"}, the default) or nonparametric test 
  (\code{test="nonparametric"}) to compare groups.
}
  \item{paired}{
  For the case of two groups, a logical scalar indicating whether the data 
  should be considered to be paired.  The default value is \code{paired=FALSE}.

  NOTE: if the argument \code{test.arg.list} is supplied and it includes a 
  component named \code{paired}, the value of that component 
  is overriden by the value of the argument \code{paired}.
}
  \item{test.arg.list}{
  An optional list of arguments to pass to the function used to test for 
  group differences in location.  The default value is an empty list:  
  \code{test.arg.list=list()}.  In particular, when there are two groups, 
  \code{ci.and.test="parametric"}, and \code{ci.arg.list} does not contain 
  a component specifying the value for \code{var.equal}, this argument is 
  updated to include the component \code{var.equal=TRUE}, which is \bold{not} 
  the default behavior of \code{\link[stats]{t.test}}.

  NOTE: If \code{test.arg.list} contains a component named \code{"paired"}, 
  the value of that component is set to the value of the argument \code{paired} 
  (see above).
}
  \item{two.lines}{
  For the case of one or two groups, a logical scalar indicating whether the 
  associated confidence interval should be be displayed on a second line 
  instead of on the same line as the p-value.  The default is \code{two.lines=TRUE}.
}
  \item{p.value.digits}{
  An integer indicating the number of digits to use for displaying the 
  p-value.  When \code{p.value.digit.type="round"} (see below) 
  the argument \code{p.value.digits} indicates the number of digits to round to, 
  and when \code{p.value.digit.type="signif"} the argument \code{p.value.digits} 
  indicates the number of significant digits to display.  
  The default value is \code{p.value.digits=3}.
}
  \item{p.value.digit.type}{
  A character string indicating whether the \code{p.value.digits} argument (see above) 
  refers to significant digits (\code{p.value.digit.type="signif"}), or how many decimal 
  places to round to (\code{p.value.digit.type="round"}, the default).
}
  \item{location.digits}{
  For the case of one or two groups, an integer indicating the number of digits 
  to use for displaying the associated confidence interval.    
  When \code{location.digit.type="round"} (see below) 
  the argument \code{location.digits} indicates the number of digits to round to, 
  and when \code{location.digit.type="signif"} the argument \code{location.digits} 
  indicates the number of significant digits to display.  
  The default value is \code{location.digits=1}.
}
  \item{location.digit.type}{
  For the case of one or two groups, a character string indicating 
  whether the \code{location.digits} argument (see above) 
  refers to significant digits \cr
  (\code{location.digit.type="signif"}), or how many decimal 
  places to round to (\code{location.digit.type="round"}; the default).
}
  \item{nsmall}{
  For the case of one or two groups, an integer passed to the function 
  \code{\link{format}} indicating the the minimum number of digits to use 
  to the right of the decimal point for the associated confidence interval.
  The default value is \code{nsmall=digits} when \code{digit.type="round"} and 
  \code{nsmall=0} when \code{digit.type="signif"}.  When \code{nsmall} is greater than 0, 
  the two confidence limits will have the same number of digits to the 
  right of the decimal point (including, possibly, trailing zeros). 
  To omit trailing zeros, set \code{nsmall=0}.
}
  \item{text.box}{
  Logical scalar indicating whether to surround the text with a text box (i.e., 
  whether to use \code{\link[ggplot2]{geom_label}} instead of 
  \code{\link[ggplot2]{geom_text}}).  This argument can be overridden by simply 
  specifying the argument \code{geom}.
}
  \item{alpha, angle, color, family, fontface, hjust, vjust, lineheight, size}{
  See the help file for \code{\link[ggplot2]{geom_text}} and 
  the vignette \bold{Aesthetic specifications} at 
  \url{https://cran.r-project.org/package=ggplot2/vignettes/ggplot2-specs.html}.
}
  \item{label.padding, label.r, label.size}{
  See the help file for \code{\link[ggplot2]{geom_text}}.
}
  \item{\dots}{
  Other arguments passed on to \code{\link[ggplot2]{layer}}. 
}
}
\details{
  The table below shows which hypothesis tests are performed based on the number of groups 
  and the values of the arguments \code{test} and \code{paired}.

  \tabular{cllll}{
                    \tab                        \tab               \tab                           \tab \bold{Function}                   \cr
    \bold{# Groups} \tab \code{test}            \tab \code{paired} \tab \bold{Name}               \tab \bold{Called}                     \cr
                    \tab                        \tab               \tab                           \tab                                   \cr
         1          \tab \code{"parametric"}    \tab               \tab One-Sample t-test         \tab \code{\link[stats]{t.test}}       \cr 
                    \tab                        \tab               \tab                           \tab                                   \cr
                    \tab \code{"nonparametric"} \tab               \tab Wilcoxon Signed Rank Test \tab \code{\link[stats]{wilcox.test}}  \cr 
                    \tab                        \tab               \tab                           \tab                                   \cr
                    \tab                        \tab               \tab                           \tab                                   \cr
         2          \tab \code{"parametric"}    \tab \code{FALSE}  \tab Two-Sample t-test         \tab \code{\link[stats]{t.test}}       \cr 
                    \tab                        \tab \code{TRUE}   \tab Paired t-test             \tab \code{\link[stats]{t.test}}       \cr 
                    \tab                        \tab               \tab                           \tab                                   \cr
                    \tab \code{"nonparametric"} \tab \code{FALSE}  \tab Wilcoxon Rank Sum Test    \tab \code{\link[stats]{wilcox.test}}  \cr 
                    \tab                        \tab \code{TRUE}   \tab Wilcoxon Signed Rank Test \tab \code{\link[stats]{wilcox.test}}  \cr 
                    \tab                        \tab               \tab on Paired Differences     \tab                                   \cr
                    \tab                        \tab               \tab                           \tab                                   \cr
                    \tab                        \tab               \tab                           \tab                                   \cr
    \eqn{\ge} 3     \tab \code{"parametric"}    \tab               \tab Analysis of Variance      \tab \code{\link[stats]{aov}}          \cr 
                    \tab                        \tab               \tab                           \tab \code{\link[stats]{summary.aov}}  \cr
                    \tab                        \tab               \tab                           \tab                                   \cr
                    \tab \code{"nonparametric"} \tab               \tab Kruskal-Wallis Test       \tab \code{\link[stats]{kruskal.test}} \cr 
  }

  See the help file for \code{\link[ggplot2]{geom_text}} for details about how 
  \code{\link[ggplot2]{geom_text}} and \code{\link[ggplot2]{geom_label}} work.

  See the vignette \bold{Extending ggplot2} at  
  \url{https://cran.r-project.org/package=ggplot2/vignettes/extending-ggplot2.html} 
  for information on how to create a new stat.
}
\references{
  Wickham, H. (2016).  \emph{ggplot2:  Elegant Graphics for Data Analysis (Use R!)}.  
  Second Edition.  Springer. 
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  The function \code{stat_test_text} is called by the function \code{\link{geom_stripchart}}.
}
\seealso{
  \code{\link{geom_stripchart}}, \code{\link{stat_mean_sd_text}}, 
  \code{\link{stat_median_iqr_text}}, \code{\link{stat_n_text}}, 
  \code{\link[ggplot2]{geom_text}}, \code{\link[ggplot2]{geom_label}}, 
  \code{\link[stats]{t.test}}, \code{\link[stats]{wilcox.test}}, 
  \code{\link[stats]{aov}}, \code{\link[stats]{summary.aov}}, 
  \code{\link[stats]{kruskal.test}}.
}
\examples{

  # First, load and attach the ggplot2 package.
  #--------------------------------------------

  library(ggplot2)

  #==========

  # Example 1:

  # Using the built-in data frame mtcars, 
  # plot miles per gallon vs. number of cylinders
  # using different colors for each level of the number of cylinders.
  #------------------------------------------------------------------

  p <- ggplot(mtcars, aes(x = factor(cyl), y = mpg, color = factor(cyl))) + 
    theme(legend.position = "none")

  p + geom_point(show.legend = FALSE) + 
    labs(x = "Number of Cylinders", y = "Miles per Gallon")


  # Now add text indicating the sample size and 
  # mean and standard deviation for each level of cylinder, and 
  # test for the difference in means between groups.
  #------------------------------------------------------------

  dev.new()
  p + geom_point() + 
    stat_n_text() + stat_mean_sd_text() + 
    stat_test_text() + 
    labs(x = "Number of Cylinders", y = "Miles per Gallon")
  
  #==========

  # Example 2:

  # Repeat Example 1, but show text indicating the median and IQR, 
  # and use the nonparametric test.
  #---------------------------------------------------------------

  dev.new()
  p + geom_point() + 
    stat_n_text() + stat_median_iqr_text() + 
    stat_test_text(test = "nonparametric") + 
    labs(x = "Number of Cylinders", y = "Miles per Gallon")
 
  #==========

  # Example 3:

  # Repeat Example 1, but use only the groups with 
  # 4 and 8 cylinders.
  #-----------------------------------------------

  p <- ggplot(subset(mtcars, cyl \%in\% c(4, 8)), 
    aes(x = factor(cyl), y = mpg, color = cyl)) + 
    theme(legend.position = "none")

  dev.new()
  p + geom_point() + 
    stat_n_text() + stat_mean_sd_text() + 
    stat_test_text() + 
    labs(x = "Number of Cylinders", y = "Miles per Gallon")

  #==========

  # Example 4:

  # Repeat Example 3, but 
  # 1) facet by transmission type,
  # 2) make the text smaller,
  # 3) put the text for the test results in a text box
  #    and make them blue.
  #---------------------------------------------------

  dev.new()
  p + geom_point() + 
    stat_n_text(size = 3) + stat_mean_sd_text(size = 3) + 
    stat_test_text(size = 3, text.box = TRUE, color = "blue") +
    facet_wrap(~ am, labeller = label_both) +  
    labs(x = "Number of Cylinders", y = "Miles per Gallon")

  #==========

  # Clean up
  #---------

  graphics.off()
  rm(p)
}
\keyword{aplot}
