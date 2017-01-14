\name{stat_median_iqr_text}
\alias{stat_median_iqr_text}
\title{
  Add Text Indicating the Median and Interquartile Range to a ggplot2 Plot 
}
\description{
  For a strip plot or scatterplot produced using the package \link[ggplot2]{ggplot2}
  (e.g., with \code{\link[ggplot2]{geom_point}}), 
  for each value on the \eqn{x}-axis, add text indicating the median and 
  interquartile range (IQR) of the \eqn{y}-values for that particular \eqn{x}-value.
}
\usage{
  stat_median_iqr_text(mapping = NULL, data = NULL, 
    geom = ifelse(text.box, "label", "text"), 
    position = "identity", na.rm = FALSE, show.legend = NA, 
    inherit.aes = TRUE, y.pos = NULL, y.expand.factor = 0.2, 
    digits = 1, digit.type = "round", 
    nsmall = ifelse(digit.type == "round", digits, 0), text.box = FALSE, 
    alpha = 1, angle = 0, color = "black", family = "", fontface = "plain", 
    hjust = 0.5, label.padding = ggplot2::unit(0.25, "lines"), 
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
  The default value is \code{y.expand.factor=0.2}.
}
  \item{digits}{
  Integer indicating the number of digits to use for displaying the 
  median and interquartile range.  When \code{digit.type="round"} (see below) 
  the argument \code{digits} indicates the number of digits to round to, and when 
  \code{digit.type="signif"} the argument \code{digits} indicates the number of 
  significant digits to display.  The default value is \code{digits=1}.
}
  \item{digit.type}{
  Character string indicating whether the \code{digits} argument (see above) refers to significant
  digits (\code{digit.type="signif"}), or how many decimal places to round to 
  (\code{digit.type="round"}, the default).
}
  \item{nsmall}{
  Integer passed to the function \code{\link{format}} indicating the the minimum number of
  digits to the right of the decimal point for the computed median and interquartile range.
  The default value is \code{nsmall=digits} when \code{digit.type="round"} and 
  \code{nsmall=0} when \code{digit.type="signif"}.  When \code{nsmall} is greater than 0, 
  all computed medians and interquartile ranges will have the same number of digits to the 
  right of the decimal point (including, possibly, trailing zeros). To omit trailing zeros, set
  \code{nsmall=0}.
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
  The function \code{stat_median_iqr_text} is called by the function \code{\link{geom_stripchart}}.
}
\seealso{
  \code{\link{geom_stripchart}}, \code{\link{stat_mean_sd_text}}, 
  \code{\link{stat_n_text}}, \code{\link{stat_test_text}}, 
  \code{\link[ggplot2]{geom_text}}, \code{\link[ggplot2]{geom_label}}, 
  \code{\link[stats]{median}}, \code{\link{iqr}}.
}
\examples{

  # First, load and attach the ggplot2 package.
  #--------------------------------------------

  library(ggplot2)

  #====================

  # Example 1:

  # Using the built-in data frame mtcars, 
  # plot miles per gallon vs. number of cylinders
  # using different colors for each level of the number of cylinders.
  #------------------------------------------------------------------

  p <- ggplot(mtcars, aes(x = factor(cyl), y = mpg, color = factor(cyl))) + 
    theme(legend.position = "none")

  p + geom_point() + 
    labs(x = "Number of Cylinders", y = "Miles per Gallon")


  # Now add text indicating the median and interquartile range 
  # for each level of cylinder.
  #-----------------------------------------------------------

  dev.new()
  p + geom_point() + 
    stat_median_iqr_text() + 
    labs(x = "Number of Cylinders", y = "Miles per Gallon")
  
  #====================

  # Example 2:

  # Repeat Example 1, but:
  # 1) facet by transmission type, 
  # 2) make the size of the text smaller.
  #--------------------------------------

  dev.new()
  p + geom_point() + 
    stat_median_iqr_text(size = 2.75) + 
    facet_wrap(~ am, labeller = label_both) +  
    labs(x = "Number of Cylinders", y = "Miles per Gallon")
 
  #====================

  # Example 3:

  # Repeat Example 1, but specify the y-position for the text.
  #-----------------------------------------------------------

  dev.new()
  p + geom_point() + 
    stat_median_iqr_text(y.pos = 36) + 
    labs(x = "Number of Cylinders", y = "Miles per Gallon")


  #====================

  # Example 4:

  # Repeat Example 1, but show the 
  # median and interquartile range in a text box.
  #----------------------------------------------

  dev.new()
  p + geom_point() + 
    stat_median_iqr_text(text.box = TRUE) + 
    labs(x = "Number of Cylinders", y = "Miles per Gallon")
  
  #====================

  # Example 5:

  # Repeat Example 1, but use the color brown for the text.
  #--------------------------------------------------------
 
  dev.new()
  p + geom_point() + 
    stat_median_iqr_text(color = "brown") + 
    labs(x = "Number of Cylinders", y = "Miles per Gallon")

  #====================

  # Example 6:

  # Repeat Example 1, but:
  # 1) use the same colors for the text that are used for each group, 
  # 2) use the bold monospaced font.
  #------------------------------------------------------------------
 
  mat <- ggplot_build(p)$data[[1]]
  group <- mat[, "group"]
  colors <- mat[match(1:max(group), group), "colour"]

  dev.new()
  p + geom_point() + 
    stat_median_iqr_text(color = colors,  
      family = "mono", fontface = "bold") + 
    labs(x = "Number of Cylinders", y = "Miles per Gallon")
 
  #====================

  # Clean up
  #---------

  graphics.off()
  rm(p, mat, group, colors)
}
\keyword{aplot}
