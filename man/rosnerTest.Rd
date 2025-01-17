\name{rosnerTest}
\alias{rosnerTest}
\alias{outlier test}
\alias{Outlier Test}
\alias{Rosner}
\title{
  Rosner's Test for Outliers
}
\description{
  Perform Rosner's generalized extreme Studentized deviate test for up to 
  \eqn{k} potential outliers in a dataset, assuming the data without any outliers come 
  from a normal (Gaussian) distribution.
}
\usage{
  rosnerTest(x, k = 3, alpha = 0.05, warn = TRUE)
}
\arguments{
  \item{x}{
  numeric vector of observations.  
  Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) 
  values are allowed but will be removed.  There must be at least 10 non-missing, finite 
  observations in \code{x}.
}
  \item{k}{
  positive integer indicating the number of suspected outliers.  The argument \code{k} 
  must be between 1 and \eqn{n-2} where \eqn{n} denotes the number of non-missing, finite 
  values in the arguemnt \code{x}.  The default value is \code{k=3}.
}
  \item{alpha}{
  numeric scalar between 0 and 1 indicating the Type I error associated with the 
  test of hypothesis.  The default value is \code{alpha=0.05}.
}
  \item{warn}{
  logical scalar indicating whether to issue a warning (\code{warn=TRUE}; the default) 
  when the number of non-missing, finite values in \code{x} and the value of \code{k} are such 
  that the assumed Type I error level might not be maintained.  See the DETAILS section below. 
}
}
\details{
  Let \eqn{x_1, x_2, \ldots, x_n} denote the \eqn{n} observations.  We assume that 
  \eqn{n-k} of these observations come from the same normal (Gaussian) distribution, and 
  that the \eqn{k} most \dQuote{extreme} observations may or may not represent observations 
  from a different distribution.  Let \eqn{x^{*}_1, x^{*}_2, \ldots, x^{*}_{n-i}} denote 
  the \eqn{n-i} observations left after omiting the \eqn{i} most extreme observations, where  
  \eqn{i = 0, 1, \ldots, k-1}.  Let \eqn{\bar{x}^{(i)}} and \eqn{s^{(i)}} denote the 
  mean and standard deviation, respectively, of the \eqn{n-i} observations in the data 
  that remain after removing the \eqn{i} most extreme observations.  
  Thus, \eqn{\bar{x}^{(0)}} and \eqn{s^{(0)}} denote the 
  mean and standard deviation for the full sample, and in general
  \deqn{\bar{x}^{(i)} = \frac{1}{n-i}\sum_{j=1}^{n-i} x^{*}_j \;\;\;\;\;\; (1)}
  \deqn{s^{(i)} = \sqrt{\frac{1}{n-i-1} \sum_{j=1}^{n-i} (x^{*}_j - \bar{x}^{(i)})^2} \;\;\;\;\;\; (2)} 

  For a specified value of \eqn{i}, the most extreme observation \eqn{x^{(i)}} is the one 
  that is the greatest distance from the mean for that data set, i.e., 
  \deqn{x^{(i)} = \max_{j=1,2,\ldots,n-i} |x^{*}_j - \bar{x}^{(i)}| \;\;\;\;\;\; (3)}
  Thus, an extreme observation may be the smallest or the largest one in that data set.

  Rosner's test is based on the \eqn{k} statistics 
  \eqn{R_1, R_2, \ldots, R_k}, which represent the extreme Studentized deviates computed 
  from successively reduced samples of size \eqn{n, n-1, \ldots, n-k+1}:
  \deqn{R_{i+1} = \frac{|x^{(i)} - \bar{x}^{(i)}|}{s^{(i)}} \;\;\;\;\;\; (4)}
  Critical values for \eqn{R_{i+1}} are denoted \eqn{\lambda_{i+1}} and are computed as:
  \deqn{\lambda_{i+1} = \frac{t_{p, n-i-2} (n-i-1)}{\sqrt{(n-i-2 + t^{2}_{p, n-i-2}) (n-i)}} \;\;\;\;\;\; (5)}
  where \eqn{t_{p, \nu}} denotes the \eqn{p}'th quantile of 
  \link[stats:TDist]{Student's t-distribution} with 
  \eqn{\nu} degrees of freedom, and in this case
  \deqn{p = 1 - \frac{\alpha/2}{n - i} \;\;\;\;\;\; (6)}
  where \eqn{\alpha} denotes the Type I error level.

  The algorithm for determining the number of outliers is as follows:
  \enumerate{
  \item Compare \eqn{R_k} with \eqn{\lambda_k}.  If \eqn{R_k > \lambda_k} then 
        conclude the \eqn{k} most extreme values are outliers.
  \item If \eqn{R_k \le \lambda_k} then compare \eqn{R_{k-1}} with \eqn{\lambda_{k-1}}.  
        If \eqn{R_{k-1} > \lambda_{k-1}} then conclude the \eqn{k-1} most extreme values 
        are outliers.
  \item Continue in this fashion until a certain number of outliers have been identified 
        or Rosner's test finds no outliers at all.
  }
 
  Based on a study using N=1,000 simulations, Rosner's (1983) Table 1 shows the estimated  
  true Type I error of declaring at least one outlier when none exists for various 
  sample sizes \eqn{n} ranging from 10 to 100, and the declared maximum number of outliers 
  \eqn{k} ranging from 1 to 10.  Based on that table, Roser (1983) declared that for an 
  assumed Type I error level of 0.05, as long as \eqn{n \ge 25}, the estimated 
  \eqn{\alpha} levels are quite close to 0.05, and that similar results were obtained 
  assuming a Type I error level of 0.01.  However, the table below is an expanded version 
  of Rosner's (1983) Table 1 and shows results based on N=10,000 simulations.  
  You can see that for an assumed Type I error of 0.05, the test maintains the Type I error 
  fairly well for sample sizes as small as \eqn{n = 3} as long as \eqn{k = 1}, and for 
  \eqn{n \ge 15}, as long as \eqn{k \le 2}.    
  Also, for an assumed Type I error of 0.01, the test maintains the Type I error fairly 
  well for sample sizes as small as \eqn{n = 15} as long as \eqn{k \le 7}.  

  Based on these results, when \code{warn=TRUE}, a warning is issued for the following cases
  indicating that the assumed Type I error may not be correct:
  \itemize{
    \item \code{alpha} is greater than \code{0.01}, the sample size is less than 15, and 
      \code{k} is greater than \code{1}.

    \item \code{alpha} is greater than \code{0.01}, 
      the sample size is at least 15 and less than 25, and 
      \code{k} is greater than \code{2}.

    \item \code{alpha} is less than or equal to \code{0.01}, the sample size is less than 15, and 
      \code{k} is greater than \code{1}. 

    \item \code{k} is greater than \code{10}, or greater than the floor of half of the sample size 
      (i.e., greater than the greatest integer less than or equal to half of the sample size).  
      A warning is given for this case because simulations have not been done for this case. \cr
  } 

  \bold{Table 1a.}  Observed Type I Error Levels based on 10,000 Simulations, \eqn{n =} 3 to 5.
  \tabular{llllllll}{
            \tab         \tab \bold{Assumed}     \tab \eqn{\alpha=0.05} \tab               \tab \bold{Assumed}     \tab \eqn{\alpha=0.01} \tab               \cr
    \eqn{n} \tab \eqn{k} \tab \eqn{\hat{\alpha}} \tab \emph{95\% LCL} \tab \emph{95\% UCL} \tab \eqn{\hat{\alpha}} \tab \emph{95\% LCL} \tab \emph{95\% UCL} \cr
     3      \tab 1       \tab 0.047              \tab 0.043           \tab 0.051           \tab 0.009              \tab 0.007           \tab 0.01            \cr
     4      \tab 1       \tab 0.049              \tab 0.045           \tab 0.053           \tab 0.010              \tab 0.008           \tab 0.012           \cr
            \tab 2       \tab 0.107              \tab 0.101           \tab 0.113           \tab 0.021              \tab 0.018           \tab 0.024           \cr
     5      \tab 1       \tab 0.048              \tab 0.044           \tab 0.053           \tab 0.008              \tab 0.006           \tab 0.009           \cr
            \tab 2       \tab 0.095              \tab 0.090           \tab 0.101           \tab 0.020              \tab 0.018           \tab 0.023           
  }
  \bold{Table 1b.}  Observed Type I Error Levels based on 10,000 Simulations, \eqn{n =} 6 to 10.
  \tabular{llllllll}{
            \tab         \tab \bold{Assumed}     \tab \eqn{\alpha=0.05} \tab               \tab \bold{Assumed}     \tab \eqn{\alpha=0.01} \tab               \cr
    \eqn{n} \tab \eqn{k} \tab \eqn{\hat{\alpha}} \tab \emph{95\% LCL} \tab \emph{95\% UCL} \tab \eqn{\hat{\alpha}} \tab \emph{95\% LCL} \tab \emph{95\% UCL} \cr
     6      \tab 1       \tab 0.048              \tab 0.044           \tab 0.053           \tab 0.010              \tab 0.009           \tab 0.012           \cr
            \tab 2       \tab 0.085              \tab 0.080           \tab 0.091           \tab 0.017              \tab 0.015           \tab 0.020           \cr
            \tab 3       \tab 0.141              \tab 0.134           \tab 0.148           \tab 0.028              \tab 0.025           \tab 0.031           \cr
     7      \tab 1       \tab 0.048              \tab 0.044           \tab 0.053           \tab 0.013              \tab 0.011           \tab 0.015           \cr
            \tab 2       \tab 0.080              \tab 0.075           \tab 0.086           \tab 0.017              \tab 0.015           \tab 0.020           \cr
            \tab 3       \tab 0.112              \tab 0.106           \tab 0.118           \tab 0.022              \tab 0.019           \tab 0.025           \cr
     8      \tab 1       \tab 0.048              \tab 0.044           \tab 0.053           \tab 0.011              \tab 0.009           \tab 0.013           \cr
            \tab 2       \tab 0.080              \tab 0.074           \tab 0.085           \tab 0.017              \tab 0.014           \tab 0.019           \cr
            \tab 3       \tab 0.102              \tab 0.096           \tab 0.108           \tab 0.020              \tab 0.017           \tab 0.023           \cr
            \tab 4       \tab 0.143              \tab 0.136           \tab 0.150           \tab 0.028              \tab 0.025           \tab 0.031           \cr
     9      \tab 1       \tab 0.052              \tab 0.048           \tab 0.057           \tab 0.010              \tab 0.008           \tab 0.012           \cr
            \tab 2       \tab 0.069              \tab 0.064           \tab 0.074           \tab 0.014              \tab 0.012           \tab 0.016           \cr
            \tab 3       \tab 0.097              \tab 0.091           \tab 0.103           \tab 0.018              \tab 0.015           \tab 0.021           \cr
            \tab 4       \tab 0.120              \tab 0.114           \tab 0.126           \tab 0.024              \tab 0.021           \tab 0.027           \cr
    10      \tab 1       \tab 0.051              \tab 0.047           \tab 0.056           \tab 0.010              \tab 0.008           \tab 0.012           \cr
            \tab 2       \tab 0.068              \tab 0.063           \tab 0.073           \tab 0.012              \tab 0.010           \tab 0.014           \cr
            \tab 3       \tab 0.085              \tab 0.080           \tab 0.091           \tab 0.015              \tab 0.013           \tab 0.017           \cr
            \tab 4       \tab 0.106              \tab 0.100           \tab 0.112           \tab 0.021              \tab 0.018           \tab 0.024           \cr
            \tab 5       \tab 0.135              \tab 0.128           \tab 0.142           \tab 0.025              \tab 0.022           \tab 0.028           
  }
  \bold{Table 1c.}  Observed Type I Error Levels based on 10,000 Simulations, \eqn{n =} 11 to 15.
  \tabular{llllllll}{
            \tab         \tab \bold{Assumed}     \tab \eqn{\alpha=0.05} \tab               \tab \bold{Assumed}     \tab \eqn{\alpha=0.01} \tab               \cr
    \eqn{n} \tab \eqn{k} \tab \eqn{\hat{\alpha}} \tab \emph{95\% LCL} \tab \emph{95\% UCL} \tab \eqn{\hat{\alpha}} \tab \emph{95\% LCL} \tab \emph{95\% UCL} \cr
    11      \tab 1       \tab 0.052              \tab 0.048           \tab 0.056           \tab 0.012              \tab 0.010           \tab 0.014           \cr
            \tab 2       \tab 0.070              \tab 0.065           \tab 0.075           \tab 0.014              \tab 0.012           \tab 0.017           \cr
            \tab 3       \tab 0.082              \tab 0.077           \tab 0.088           \tab 0.014              \tab 0.011           \tab 0.016           \cr
            \tab 4       \tab 0.101              \tab 0.095           \tab 0.107           \tab 0.019              \tab 0.016           \tab 0.021           \cr
            \tab 5       \tab 0.116              \tab 0.110           \tab 0.123           \tab 0.022              \tab 0.019           \tab 0.024           \cr
    12      \tab 1       \tab 0.052              \tab 0.047           \tab 0.056           \tab 0.011              \tab 0.009           \tab 0.013           \cr
            \tab 2       \tab 0.067              \tab 0.062           \tab 0.072           \tab 0.011              \tab 0.009           \tab 0.013           \cr
            \tab 3       \tab 0.074              \tab 0.069           \tab 0.080           \tab 0.016              \tab 0.013           \tab 0.018           \cr
            \tab 4       \tab 0.088              \tab 0.082           \tab 0.093           \tab 0.016              \tab 0.014           \tab 0.019           \cr
            \tab 5       \tab 0.099              \tab 0.093           \tab 0.105           \tab 0.016              \tab 0.013           \tab 0.018           \cr
            \tab 6       \tab 0.117              \tab 0.111           \tab 0.123           \tab 0.021              \tab 0.018           \tab 0.023           \cr
    13      \tab 1       \tab 0.048              \tab 0.044           \tab 0.052           \tab 0.010              \tab 0.008           \tab 0.012           \cr
            \tab 2       \tab 0.064              \tab 0.059           \tab 0.069           \tab 0.014              \tab 0.012           \tab 0.016           \cr
            \tab 3       \tab 0.070              \tab 0.065           \tab 0.075           \tab 0.013              \tab 0.011           \tab 0.015           \cr
            \tab 4       \tab 0.079              \tab 0.074           \tab 0.084           \tab 0.014              \tab 0.012           \tab 0.017           \cr
            \tab 5       \tab 0.088              \tab 0.083           \tab 0.094           \tab 0.015              \tab 0.013           \tab 0.018           \cr
            \tab 6       \tab 0.109              \tab 0.103           \tab 0.115           \tab 0.020              \tab 0.017           \tab 0.022           \cr
    14      \tab 1       \tab 0.046              \tab 0.042           \tab 0.051           \tab 0.009              \tab 0.007           \tab 0.011           \cr
            \tab 2       \tab 0.062              \tab 0.057           \tab 0.066           \tab 0.012              \tab 0.010           \tab 0.014           \cr
            \tab 3       \tab 0.069              \tab 0.064           \tab 0.074           \tab 0.012              \tab 0.010           \tab 0.014           \cr
            \tab 4       \tab 0.077              \tab 0.072           \tab 0.082           \tab 0.015              \tab 0.013           \tab 0.018           \cr
            \tab 5       \tab 0.084              \tab 0.079           \tab 0.090           \tab 0.016              \tab 0.013           \tab 0.018           \cr
            \tab 6       \tab 0.091              \tab 0.085           \tab 0.097           \tab 0.017              \tab 0.014           \tab 0.019           \cr
            \tab 7       \tab 0.107              \tab 0.101           \tab 0.113           \tab 0.018              \tab 0.016           \tab 0.021           \cr
    15      \tab 1       \tab 0.054              \tab 0.050           \tab 0.059           \tab 0.010              \tab 0.008           \tab 0.012           \cr
            \tab 2       \tab 0.057              \tab 0.053           \tab 0.062           \tab 0.010              \tab 0.008           \tab 0.012           \cr
            \tab 3       \tab 0.065              \tab 0.060           \tab 0.069           \tab 0.013              \tab 0.011           \tab 0.016           \cr
            \tab 4       \tab 0.073              \tab 0.068           \tab 0.078           \tab 0.014              \tab 0.011           \tab 0.016           \cr
            \tab 5       \tab 0.074              \tab 0.069           \tab 0.079           \tab 0.012              \tab 0.010           \tab 0.014           \cr
            \tab 6       \tab 0.086              \tab 0.081           \tab 0.092           \tab 0.015              \tab 0.013           \tab 0.017           \cr
            \tab 7       \tab 0.099              \tab 0.094           \tab 0.105           \tab 0.018              \tab 0.015           \tab 0.020           
  }
  \bold{Table 1d.}  Observed Type I Error Levels based on 10,000 Simulations, \eqn{n =} 16 to 20.
  \tabular{llllllll}{
            \tab         \tab \bold{Assumed}     \tab \eqn{\alpha=0.05} \tab               \tab \bold{Assumed}     \tab \eqn{\alpha=0.01} \tab               \cr
    \eqn{n} \tab \eqn{k} \tab \eqn{\hat{\alpha}} \tab \emph{95\% LCL} \tab \emph{95\% UCL} \tab \eqn{\hat{\alpha}} \tab \emph{95\% LCL} \tab \emph{95\% UCL} \cr
    16      \tab 1       \tab 0.052              \tab 0.048           \tab 0.057           \tab 0.010              \tab 0.008           \tab 0.012           \cr
            \tab 2       \tab 0.055              \tab 0.051           \tab 0.059           \tab 0.011              \tab 0.009           \tab 0.013           \cr
            \tab 3       \tab 0.068              \tab 0.063           \tab 0.073           \tab 0.011              \tab 0.009           \tab 0.013           \cr
            \tab 4       \tab 0.074              \tab 0.069           \tab 0.079           \tab 0.015              \tab 0.013           \tab 0.017           \cr
            \tab 5       \tab 0.077              \tab 0.072           \tab 0.082           \tab 0.015              \tab 0.013           \tab 0.018           \cr
            \tab 6       \tab 0.075              \tab 0.070           \tab 0.080           \tab 0.013              \tab 0.011           \tab 0.016           \cr
            \tab 7       \tab 0.087              \tab 0.082           \tab 0.093           \tab 0.017              \tab 0.014           \tab 0.020           \cr
            \tab 8       \tab 0.096              \tab 0.090           \tab 0.101           \tab 0.016              \tab 0.014           \tab 0.019           \cr
    17      \tab 1       \tab 0.047              \tab 0.043           \tab 0.051           \tab 0.008              \tab 0.007           \tab 0.010           \cr
            \tab 2       \tab 0.059              \tab 0.054           \tab 0.063           \tab 0.011              \tab 0.009           \tab 0.013           \cr
            \tab 3       \tab 0.062              \tab 0.057           \tab 0.067           \tab 0.012              \tab 0.010           \tab 0.014           \cr
            \tab 4       \tab 0.070              \tab 0.065           \tab 0.075           \tab 0.012              \tab 0.009           \tab 0.014           \cr
            \tab 5       \tab 0.069              \tab 0.064           \tab 0.074           \tab 0.012              \tab 0.010           \tab 0.015           \cr
            \tab 6       \tab 0.071              \tab 0.066           \tab 0.076           \tab 0.015              \tab 0.012           \tab 0.017           \cr
            \tab 7       \tab 0.081              \tab 0.076           \tab 0.087           \tab 0.014              \tab 0.012           \tab 0.016           \cr
            \tab 8       \tab 0.083              \tab 0.078           \tab 0.088           \tab 0.015              \tab 0.013           \tab 0.017           \cr
    18      \tab 1       \tab 0.051              \tab 0.047           \tab 0.055           \tab 0.010              \tab 0.008           \tab 0.012           \cr
            \tab 2       \tab 0.056              \tab 0.052           \tab 0.061           \tab 0.012              \tab 0.010           \tab 0.014           \cr
            \tab 3       \tab 0.065              \tab 0.060           \tab 0.070           \tab 0.012              \tab 0.010           \tab 0.015           \cr
            \tab 4       \tab 0.065              \tab 0.060           \tab 0.070           \tab 0.013              \tab 0.011           \tab 0.015           \cr
            \tab 5       \tab 0.069              \tab 0.064           \tab 0.074           \tab 0.012              \tab 0.010           \tab 0.014           \cr
            \tab 6       \tab 0.068              \tab 0.063           \tab 0.073           \tab 0.014              \tab 0.011           \tab 0.016           \cr
            \tab 7       \tab 0.072              \tab 0.067           \tab 0.077           \tab 0.014              \tab 0.011           \tab 0.016           \cr
            \tab 8       \tab 0.076              \tab 0.071           \tab 0.081           \tab 0.012              \tab 0.010           \tab 0.014           \cr
            \tab 9       \tab 0.081              \tab 0.076           \tab 0.086           \tab 0.012              \tab 0.010           \tab 0.014           \cr
    19      \tab 1       \tab 0.051              \tab 0.046           \tab 0.055           \tab 0.008              \tab 0.006           \tab 0.010           \cr
            \tab 2       \tab 0.059              \tab 0.055           \tab 0.064           \tab 0.012              \tab 0.010           \tab 0.014           \cr
            \tab 3       \tab 0.059              \tab 0.054           \tab 0.064           \tab 0.011              \tab 0.009           \tab 0.013           \cr
            \tab 4       \tab 0.061              \tab 0.057           \tab 0.066           \tab 0.012              \tab 0.010           \tab 0.014           \cr
            \tab 5       \tab 0.067              \tab 0.062           \tab 0.072           \tab 0.013              \tab 0.010           \tab 0.015           \cr
            \tab 6       \tab 0.066              \tab 0.061           \tab 0.071           \tab 0.011              \tab 0.009           \tab 0.013           \cr
            \tab 7       \tab 0.069              \tab 0.064           \tab 0.074           \tab 0.013              \tab 0.011           \tab 0.015           \cr
            \tab 8       \tab 0.074              \tab 0.069           \tab 0.079           \tab 0.012              \tab 0.010           \tab 0.014           \cr
            \tab 9       \tab 0.082              \tab 0.077           \tab 0.087           \tab 0.015              \tab 0.013           \tab 0.018           \cr
    20      \tab 1       \tab 0.053              \tab 0.048           \tab 0.057           \tab 0.011              \tab 0.009           \tab 0.013           \cr
            \tab 2       \tab 0.056              \tab 0.052           \tab 0.061           \tab 0.010              \tab 0.008           \tab 0.012           \cr
            \tab 3       \tab 0.060              \tab 0.056           \tab 0.065           \tab 0.009              \tab 0.007           \tab 0.011           \cr
            \tab 4       \tab 0.063              \tab 0.058           \tab 0.068           \tab 0.012              \tab 0.010           \tab 0.014           \cr
            \tab 5       \tab 0.063              \tab 0.059           \tab 0.068           \tab 0.014              \tab 0.011           \tab 0.016           \cr
            \tab 6       \tab 0.063              \tab 0.058           \tab 0.067           \tab 0.011              \tab 0.009           \tab 0.013           \cr
            \tab 7       \tab 0.065              \tab 0.061           \tab 0.070           \tab 0.011              \tab 0.009           \tab 0.013           \cr
            \tab 8       \tab 0.070              \tab 0.065           \tab 0.076           \tab 0.012              \tab 0.010           \tab 0.014           \cr
            \tab 9       \tab 0.076              \tab 0.070           \tab 0.081           \tab 0.013              \tab 0.011           \tab 0.015           \cr
            \tab 10      \tab 0.081              \tab 0.076           \tab 0.087           \tab 0.012              \tab 0.010           \tab 0.014           
  }
  \bold{Table 1e.}  Observed Type I Error Levels based on 10,000 Simulations, \eqn{n =} 21 to 25.
  \tabular{llllllll}{
            \tab         \tab \bold{Assumed}     \tab \eqn{\alpha=0.05} \tab               \tab \bold{Assumed}     \tab \eqn{\alpha=0.01} \tab               \cr
    \eqn{n} \tab \eqn{k} \tab \eqn{\hat{\alpha}} \tab \emph{95\% LCL} \tab \emph{95\% UCL} \tab \eqn{\hat{\alpha}} \tab \emph{95\% LCL} \tab \emph{95\% UCL} \cr
    21      \tab 1       \tab 0.054              \tab 0.049           \tab 0.058           \tab 0.013              \tab 0.011           \tab 0.015           \cr
            \tab 2       \tab 0.054              \tab 0.049           \tab 0.058           \tab 0.012              \tab 0.010           \tab 0.014           \cr
            \tab 3       \tab 0.058              \tab 0.054           \tab 0.063           \tab 0.012              \tab 0.010           \tab 0.014           \cr
            \tab 4       \tab 0.058              \tab 0.054           \tab 0.063           \tab 0.011              \tab 0.009           \tab 0.013           \cr
            \tab 5       \tab 0.064              \tab 0.059           \tab 0.069           \tab 0.013              \tab 0.011           \tab 0.016           \cr
            \tab 6       \tab 0.066              \tab 0.061           \tab 0.071           \tab 0.012              \tab 0.010           \tab 0.015           \cr
            \tab 7       \tab 0.063              \tab 0.058           \tab 0.068           \tab 0.013              \tab 0.011           \tab 0.015           \cr
            \tab 8       \tab 0.066              \tab 0.061           \tab 0.071           \tab 0.010              \tab 0.008           \tab 0.012           \cr
            \tab 9       \tab 0.073              \tab 0.068           \tab 0.078           \tab 0.013              \tab 0.011           \tab 0.015           \cr
            \tab 10      \tab 0.071              \tab 0.066           \tab 0.076           \tab 0.012              \tab 0.010           \tab 0.014           \cr
    22      \tab 1       \tab 0.047              \tab 0.042           \tab 0.051           \tab 0.010              \tab 0.008           \tab 0.012           \cr
            \tab 2       \tab 0.058              \tab 0.053           \tab 0.062           \tab 0.012              \tab 0.010           \tab 0.015           \cr
            \tab 3       \tab 0.056              \tab 0.052           \tab 0.061           \tab 0.010              \tab 0.008           \tab 0.012           \cr
            \tab 4       \tab 0.059              \tab 0.055           \tab 0.064           \tab 0.012              \tab 0.010           \tab 0.014           \cr
            \tab 5       \tab 0.061              \tab 0.057           \tab 0.066           \tab 0.009              \tab 0.008           \tab 0.011           \cr
            \tab 6       \tab 0.063              \tab 0.058           \tab 0.068           \tab 0.013              \tab 0.010           \tab 0.015           \cr
            \tab 7       \tab 0.065              \tab 0.060           \tab 0.070           \tab 0.013              \tab 0.010           \tab 0.015           \cr
            \tab 8       \tab 0.065              \tab 0.060           \tab 0.070           \tab 0.014              \tab 0.012           \tab 0.016           \cr
            \tab 9       \tab 0.065              \tab 0.060           \tab 0.070           \tab 0.012              \tab 0.010           \tab 0.014           \cr
            \tab 10      \tab 0.067              \tab 0.062           \tab 0.072           \tab 0.012              \tab 0.009           \tab 0.014           \cr
    23      \tab 1       \tab 0.051              \tab 0.047           \tab 0.056           \tab 0.008              \tab 0.007           \tab 0.010           \cr
            \tab 2       \tab 0.056              \tab 0.052           \tab 0.061           \tab 0.010              \tab 0.009           \tab 0.012           \cr
            \tab 3       \tab 0.056              \tab 0.052           \tab 0.061           \tab 0.011              \tab 0.009           \tab 0.013           \cr
            \tab 4       \tab 0.062              \tab 0.057           \tab 0.066           \tab 0.011              \tab 0.009           \tab 0.013           \cr
            \tab 5       \tab 0.061              \tab 0.056           \tab 0.065           \tab 0.010              \tab 0.009           \tab 0.012           \cr
            \tab 6       \tab 0.060              \tab 0.055           \tab 0.064           \tab 0.012              \tab 0.010           \tab 0.014           \cr
            \tab 7       \tab 0.062              \tab 0.057           \tab 0.066           \tab 0.011              \tab 0.009           \tab 0.013           \cr
            \tab 8       \tab 0.063              \tab 0.058           \tab 0.068           \tab 0.012              \tab 0.010           \tab 0.014           \cr
            \tab 9       \tab 0.066              \tab 0.061           \tab 0.071           \tab 0.012              \tab 0.010           \tab 0.014           \cr
            \tab 10      \tab 0.068              \tab 0.063           \tab 0.073           \tab 0.014              \tab 0.012           \tab 0.017           \cr
    24      \tab 1       \tab 0.051              \tab 0.046           \tab 0.055           \tab 0.010              \tab 0.008           \tab 0.012           \cr
            \tab 2       \tab 0.056              \tab 0.051           \tab 0.060           \tab 0.011              \tab 0.009           \tab 0.013           \cr
            \tab 3       \tab 0.058              \tab 0.053           \tab 0.062           \tab 0.010              \tab 0.008           \tab 0.012           \cr
            \tab 4       \tab 0.060              \tab 0.056           \tab 0.065           \tab 0.013              \tab 0.011           \tab 0.015           \cr
            \tab 5       \tab 0.057              \tab 0.053           \tab 0.062           \tab 0.012              \tab 0.010           \tab 0.014           \cr
            \tab 6       \tab 0.065              \tab 0.060           \tab 0.069           \tab 0.011              \tab 0.009           \tab 0.013           \cr
            \tab 7       \tab 0.062              \tab 0.057           \tab 0.066           \tab 0.012              \tab 0.010           \tab 0.014           \cr
            \tab 8       \tab 0.060              \tab 0.055           \tab 0.065           \tab 0.012              \tab 0.010           \tab 0.014           \cr
            \tab 9       \tab 0.066              \tab 0.061           \tab 0.071           \tab 0.012              \tab 0.010           \tab 0.014           \cr
            \tab 10      \tab 0.064              \tab 0.059           \tab 0.068           \tab 0.012              \tab 0.010           \tab 0.015           \cr
    25      \tab 1       \tab 0.054              \tab 0.050           \tab 0.059           \tab 0.012              \tab 0.009           \tab 0.014           \cr
            \tab 2       \tab 0.055              \tab 0.051           \tab 0.060           \tab 0.010              \tab 0.008           \tab 0.012           \cr
            \tab 3       \tab 0.057              \tab 0.052           \tab 0.062           \tab 0.011              \tab 0.009           \tab 0.013           \cr
            \tab 4       \tab 0.055              \tab 0.051           \tab 0.060           \tab 0.011              \tab 0.009           \tab 0.013           \cr
            \tab 5       \tab 0.060              \tab 0.055           \tab 0.065           \tab 0.012              \tab 0.010           \tab 0.014           \cr
            \tab 6       \tab 0.060              \tab 0.055           \tab 0.064           \tab 0.011              \tab 0.009           \tab 0.013           \cr
            \tab 7       \tab 0.057              \tab 0.052           \tab 0.061           \tab 0.011              \tab 0.009           \tab 0.013           \cr
            \tab 8       \tab 0.062              \tab 0.058           \tab 0.067           \tab 0.011              \tab 0.009           \tab 0.013           \cr
            \tab 9       \tab 0.058              \tab 0.053           \tab 0.062           \tab 0.012              \tab 0.010           \tab 0.014           \cr
            \tab 10      \tab 0.061              \tab 0.057           \tab 0.066           \tab 0.010              \tab 0.008           \tab 0.012           
  }
  \bold{Table 1f.}  Observed Type I Error Levels based on 10,000 Simulations, \eqn{n =} 26 to 30.
  \tabular{llllllll}{
            \tab         \tab \bold{Assumed}     \tab \eqn{\alpha=0.05} \tab               \tab \bold{Assumed}     \tab \eqn{\alpha=0.01} \tab               \cr
    \eqn{n} \tab \eqn{k} \tab \eqn{\hat{\alpha}} \tab \emph{95\% LCL} \tab \emph{95\% UCL} \tab \eqn{\hat{\alpha}} \tab \emph{95\% LCL} \tab \emph{95\% UCL} \cr
    26      \tab 1       \tab 0.051              \tab 0.047           \tab 0.055           \tab 0.012              \tab 0.010           \tab 0.014           \cr
            \tab 2       \tab 0.057              \tab 0.053           \tab 0.062           \tab 0.013              \tab 0.011           \tab 0.015           \cr
            \tab 3       \tab 0.055              \tab 0.050           \tab 0.059           \tab 0.012              \tab 0.010           \tab 0.014           \cr
            \tab 4       \tab 0.055              \tab 0.051           \tab 0.060           \tab 0.010              \tab 0.008           \tab 0.012           \cr
            \tab 5       \tab 0.058              \tab 0.054           \tab 0.063           \tab 0.011              \tab 0.009           \tab 0.013           \cr
            \tab 6       \tab 0.061              \tab 0.056           \tab 0.066           \tab 0.012              \tab 0.010           \tab 0.014           \cr
            \tab 7       \tab 0.059              \tab 0.054           \tab 0.064           \tab 0.011              \tab 0.009           \tab 0.013           \cr
            \tab 8       \tab 0.060              \tab 0.056           \tab 0.065           \tab 0.010              \tab 0.008           \tab 0.012           \cr
            \tab 9       \tab 0.060              \tab 0.056           \tab 0.065           \tab 0.011              \tab 0.009           \tab 0.013           \cr
            \tab 10      \tab 0.061              \tab 0.056           \tab 0.065           \tab 0.011              \tab 0.009           \tab 0.013           \cr
    27      \tab 1       \tab 0.050              \tab 0.046           \tab 0.054           \tab 0.009              \tab 0.007           \tab 0.011           \cr
            \tab 2       \tab 0.054              \tab 0.050           \tab 0.059           \tab 0.011              \tab 0.009           \tab 0.013           \cr
            \tab 3       \tab 0.062              \tab 0.057           \tab 0.066           \tab 0.012              \tab 0.010           \tab 0.014           \cr
            \tab 4       \tab 0.063              \tab 0.058           \tab 0.068           \tab 0.011              \tab 0.009           \tab 0.013           \cr
            \tab 5       \tab 0.051              \tab 0.047           \tab 0.055           \tab 0.010              \tab 0.008           \tab 0.012           \cr
            \tab 6       \tab 0.058              \tab 0.053           \tab 0.062           \tab 0.011              \tab 0.009           \tab 0.013           \cr
            \tab 7       \tab 0.060              \tab 0.056           \tab 0.065           \tab 0.010              \tab 0.008           \tab 0.012           \cr
            \tab 8       \tab 0.056              \tab 0.052           \tab 0.061           \tab 0.010              \tab 0.008           \tab 0.012           \cr
            \tab 9       \tab 0.061              \tab 0.056           \tab 0.066           \tab 0.012              \tab 0.010           \tab 0.014           \cr
            \tab 10      \tab 0.055              \tab 0.051           \tab 0.060           \tab 0.008              \tab 0.006           \tab 0.010           \cr
    28      \tab 1       \tab 0.049              \tab 0.045           \tab 0.053           \tab 0.010              \tab 0.008           \tab 0.011           \cr
            \tab 2       \tab 0.057              \tab 0.052           \tab 0.061           \tab 0.011              \tab 0.009           \tab 0.013           \cr
            \tab 3       \tab 0.056              \tab 0.052           \tab 0.061           \tab 0.012              \tab 0.009           \tab 0.014           \cr
            \tab 4       \tab 0.057              \tab 0.053           \tab 0.062           \tab 0.011              \tab 0.009           \tab 0.013           \cr
            \tab 5       \tab 0.057              \tab 0.053           \tab 0.062           \tab 0.010              \tab 0.008           \tab 0.012           \cr
            \tab 6       \tab 0.056              \tab 0.051           \tab 0.060           \tab 0.010              \tab 0.008           \tab 0.012           \cr
            \tab 7       \tab 0.057              \tab 0.052           \tab 0.061           \tab 0.010              \tab 0.008           \tab 0.012           \cr
            \tab 8       \tab 0.058              \tab 0.054           \tab 0.063           \tab 0.011              \tab 0.009           \tab 0.013           \cr
            \tab 9       \tab 0.054              \tab 0.050           \tab 0.058           \tab 0.011              \tab 0.009           \tab 0.013           \cr
            \tab 10      \tab 0.062              \tab 0.057           \tab 0.067           \tab 0.011              \tab 0.009           \tab 0.013           \cr
    29      \tab 1       \tab 0.049              \tab 0.045           \tab 0.053           \tab 0.011              \tab 0.009           \tab 0.013           \cr
            \tab 2       \tab 0.053              \tab 0.048           \tab 0.057           \tab 0.010              \tab 0.008           \tab 0.012           \cr
            \tab 3       \tab 0.056              \tab 0.051           \tab 0.060           \tab 0.010              \tab 0.009           \tab 0.012           \cr
            \tab 4       \tab 0.055              \tab 0.050           \tab 0.059           \tab 0.010              \tab 0.008           \tab 0.012           \cr
            \tab 5       \tab 0.056              \tab 0.051           \tab 0.060           \tab 0.010              \tab 0.008           \tab 0.012           \cr
            \tab 6       \tab 0.057              \tab 0.053           \tab 0.062           \tab 0.012              \tab 0.010           \tab 0.014           \cr
            \tab 7       \tab 0.055              \tab 0.050           \tab 0.059           \tab 0.010              \tab 0.008           \tab 0.012           \cr
            \tab 8       \tab 0.057              \tab 0.052           \tab 0.061           \tab 0.011              \tab 0.009           \tab 0.013           \cr
            \tab 9       \tab 0.056              \tab 0.051           \tab 0.061           \tab 0.011              \tab 0.009           \tab 0.013           \cr
            \tab 10      \tab 0.057              \tab 0.052           \tab 0.061           \tab 0.011              \tab 0.009           \tab 0.013           \cr
    30      \tab 1       \tab 0.050              \tab 0.046           \tab 0.054           \tab 0.009              \tab 0.007           \tab 0.011           \cr
            \tab 2       \tab 0.054              \tab 0.049           \tab 0.058           \tab 0.011              \tab 0.009           \tab 0.013           \cr
            \tab 3       \tab 0.056              \tab 0.052           \tab 0.061           \tab 0.012              \tab 0.010           \tab 0.015           \cr
            \tab 4       \tab 0.054              \tab 0.049           \tab 0.058           \tab 0.010              \tab 0.008           \tab 0.012           \cr
            \tab 5       \tab 0.058              \tab 0.053           \tab 0.063           \tab 0.012              \tab 0.010           \tab 0.014           \cr
            \tab 6       \tab 0.062              \tab 0.058           \tab 0.067           \tab 0.012              \tab 0.010           \tab 0.014           \cr
            \tab 7       \tab 0.056              \tab 0.052           \tab 0.061           \tab 0.012              \tab 0.010           \tab 0.014           \cr
            \tab 8       \tab 0.059              \tab 0.054           \tab 0.064           \tab 0.011              \tab 0.009           \tab 0.013           \cr
            \tab 9       \tab 0.056              \tab 0.052           \tab 0.061           \tab 0.010              \tab 0.009           \tab 0.012           \cr
            \tab 10      \tab 0.058              \tab 0.053           \tab 0.062           \tab 0.012              \tab 0.010           \tab 0.015           
  }
  \bold{Table 1g.}  Observed Type I Error Levels based on 10,000 Simulations, n = 31 to 35.
  \tabular{llllllll}{
            \tab         \tab \bold{Assumed}     \tab \eqn{\alpha=0.05} \tab               \tab \bold{Assumed}     \tab \eqn{\alpha=0.01} \tab               \cr
    \eqn{n} \tab \eqn{k} \tab \eqn{\hat{\alpha}} \tab \emph{95\% LCL} \tab \emph{95\% UCL} \tab \eqn{\hat{\alpha}} \tab \emph{95\% LCL} \tab \emph{95\% UCL} \cr
    31      \tab 1       \tab 0.051              \tab 0.047           \tab 0.056           \tab 0.009              \tab 0.007           \tab 0.011           \cr
            \tab 2       \tab 0.054              \tab 0.050           \tab 0.059           \tab 0.010              \tab 0.009           \tab 0.012           \cr
            \tab 3       \tab 0.053              \tab 0.049           \tab 0.058           \tab 0.010              \tab 0.008           \tab 0.012           \cr
            \tab 4       \tab 0.055              \tab 0.050           \tab 0.059           \tab 0.010              \tab 0.008           \tab 0.012           \cr
            \tab 5       \tab 0.053              \tab 0.049           \tab 0.057           \tab 0.011              \tab 0.009           \tab 0.013           \cr
            \tab 6       \tab 0.055              \tab 0.050           \tab 0.059           \tab 0.010              \tab 0.008           \tab 0.012           \cr
            \tab 7       \tab 0.055              \tab 0.050           \tab 0.059           \tab 0.012              \tab 0.010           \tab 0.014           \cr
            \tab 8       \tab 0.056              \tab 0.051           \tab 0.060           \tab 0.010              \tab 0.008           \tab 0.012           \cr
            \tab 9       \tab 0.057              \tab 0.053           \tab 0.062           \tab 0.011              \tab 0.009           \tab 0.013           \cr
            \tab 10      \tab 0.058              \tab 0.053           \tab 0.062           \tab 0.011              \tab 0.009           \tab 0.013           \cr
    32      \tab 1       \tab 0.054              \tab 0.049           \tab 0.058           \tab 0.010              \tab 0.008           \tab 0.012           \cr
            \tab 2       \tab 0.054              \tab 0.050           \tab 0.059           \tab 0.010              \tab 0.008           \tab 0.012           \cr
            \tab 3       \tab 0.052              \tab 0.047           \tab 0.056           \tab 0.009              \tab 0.007           \tab 0.011           \cr
            \tab 4       \tab 0.056              \tab 0.051           \tab 0.060           \tab 0.011              \tab 0.009           \tab 0.013           \cr
            \tab 5       \tab 0.056              \tab 0.052           \tab 0.061           \tab 0.011              \tab 0.009           \tab 0.013           \cr
            \tab 6       \tab 0.055              \tab 0.051           \tab 0.060           \tab 0.011              \tab 0.009           \tab 0.013           \cr
            \tab 7       \tab 0.055              \tab 0.051           \tab 0.060           \tab 0.010              \tab 0.008           \tab 0.012           \cr
            \tab 8       \tab 0.055              \tab 0.051           \tab 0.060           \tab 0.010              \tab 0.008           \tab 0.012           \cr
            \tab 9       \tab 0.057              \tab 0.053           \tab 0.062           \tab 0.012              \tab 0.010           \tab 0.014           \cr
            \tab 10      \tab 0.054              \tab 0.050           \tab 0.059           \tab 0.010              \tab 0.008           \tab 0.012           \cr
    33      \tab 1       \tab 0.051              \tab 0.046           \tab 0.055           \tab 0.011              \tab 0.009           \tab 0.013           \cr
            \tab 2       \tab 0.055              \tab 0.051           \tab 0.060           \tab 0.011              \tab 0.009           \tab 0.013           \cr
            \tab 3       \tab 0.056              \tab 0.052           \tab 0.061           \tab 0.010              \tab 0.008           \tab 0.012           \cr
            \tab 4       \tab 0.052              \tab 0.048           \tab 0.057           \tab 0.010              \tab 0.008           \tab 0.012           \cr
            \tab 5       \tab 0.055              \tab 0.050           \tab 0.059           \tab 0.010              \tab 0.008           \tab 0.012           \cr
            \tab 6       \tab 0.058              \tab 0.053           \tab 0.062           \tab 0.011              \tab 0.009           \tab 0.013           \cr
            \tab 7       \tab 0.057              \tab 0.052           \tab 0.061           \tab 0.010              \tab 0.008           \tab 0.012           \cr
            \tab 8       \tab 0.058              \tab 0.054           \tab 0.063           \tab 0.011              \tab 0.009           \tab 0.013           \cr
            \tab 9       \tab 0.057              \tab 0.053           \tab 0.062           \tab 0.012              \tab 0.010           \tab 0.014           \cr
            \tab 10      \tab 0.055              \tab 0.051           \tab 0.060           \tab 0.011              \tab 0.009           \tab 0.013           \cr
    34      \tab 1       \tab 0.052              \tab 0.048           \tab 0.056           \tab 0.009              \tab 0.007           \tab 0.011           \cr
            \tab 2       \tab 0.053              \tab 0.049           \tab 0.058           \tab 0.011              \tab 0.009           \tab 0.013           \cr
            \tab 3       \tab 0.055              \tab 0.050           \tab 0.059           \tab 0.012              \tab 0.010           \tab 0.014           \cr
            \tab 4       \tab 0.056              \tab 0.052           \tab 0.061           \tab 0.010              \tab 0.008           \tab 0.012           \cr
            \tab 5       \tab 0.053              \tab 0.048           \tab 0.057           \tab 0.009              \tab 0.007           \tab 0.011           \cr
            \tab 6       \tab 0.055              \tab 0.050           \tab 0.059           \tab 0.010              \tab 0.008           \tab 0.012           \cr
            \tab 7       \tab 0.052              \tab 0.048           \tab 0.057           \tab 0.012              \tab 0.010           \tab 0.014           \cr
            \tab 8       \tab 0.055              \tab 0.050           \tab 0.059           \tab 0.009              \tab 0.008           \tab 0.011           \cr
            \tab 9       \tab 0.055              \tab 0.051           \tab 0.060           \tab 0.011              \tab 0.009           \tab 0.013           \cr
            \tab 10      \tab 0.054              \tab 0.049           \tab 0.058           \tab 0.010              \tab 0.008           \tab 0.012           \cr
    35      \tab 1       \tab 0.051              \tab 0.046           \tab 0.055           \tab 0.010              \tab 0.009           \tab 0.012           \cr
            \tab 2       \tab 0.054              \tab 0.049           \tab 0.058           \tab 0.010              \tab 0.009           \tab 0.012           \cr
            \tab 3       \tab 0.055              \tab 0.050           \tab 0.059           \tab 0.010              \tab 0.009           \tab 0.012           \cr
            \tab 4       \tab 0.053              \tab 0.048           \tab 0.057           \tab 0.011              \tab 0.009           \tab 0.013           \cr
            \tab 5       \tab 0.056              \tab 0.051           \tab 0.061           \tab 0.011              \tab 0.009           \tab 0.013           \cr
            \tab 6       \tab 0.055              \tab 0.051           \tab 0.059           \tab 0.012              \tab 0.010           \tab 0.014           \cr
            \tab 7       \tab 0.054              \tab 0.050           \tab 0.059           \tab 0.011              \tab 0.009           \tab 0.013           \cr
            \tab 8       \tab 0.054              \tab 0.049           \tab 0.058           \tab 0.011              \tab 0.009           \tab 0.013           \cr
            \tab 9       \tab 0.061              \tab 0.056           \tab 0.066           \tab 0.012              \tab 0.010           \tab 0.014           \cr
            \tab 10      \tab 0.053              \tab 0.048           \tab 0.057           \tab 0.011              \tab 0.009           \tab 0.013           
  }
  \bold{Table 1h.}  Observed Type I Error Levels based on 10,000 Simulations, n = 36 to 40.
  \tabular{llllllll}{
            \tab         \tab \bold{Assumed}     \tab \eqn{\alpha=0.05} \tab               \tab \bold{Assumed}     \tab \eqn{\alpha=0.01} \tab               \cr
    \eqn{n} \tab \eqn{k} \tab \eqn{\hat{\alpha}} \tab \emph{95\% LCL} \tab \emph{95\% UCL} \tab \eqn{\hat{\alpha}} \tab \emph{95\% LCL} \tab \emph{95\% UCL} \cr
    36      \tab 1       \tab 0.047              \tab 0.043           \tab 0.051           \tab 0.010              \tab 0.008           \tab 0.012           \cr
            \tab 2       \tab 0.058              \tab 0.053           \tab 0.062           \tab 0.012              \tab 0.010           \tab 0.015           \cr
            \tab 3       \tab 0.052              \tab 0.047           \tab 0.056           \tab 0.009              \tab 0.007           \tab 0.011           \cr
            \tab 4       \tab 0.052              \tab 0.048           \tab 0.056           \tab 0.012              \tab 0.010           \tab 0.014           \cr
            \tab 5       \tab 0.052              \tab 0.048           \tab 0.057           \tab 0.010              \tab 0.008           \tab 0.012           \cr
            \tab 6       \tab 0.055              \tab 0.051           \tab 0.059           \tab 0.012              \tab 0.010           \tab 0.014           \cr
            \tab 7       \tab 0.053              \tab 0.048           \tab 0.057           \tab 0.011              \tab 0.009           \tab 0.013           \cr
            \tab 8       \tab 0.056              \tab 0.051           \tab 0.060           \tab 0.012              \tab 0.010           \tab 0.014           \cr
            \tab 9       \tab 0.056              \tab 0.051           \tab 0.060           \tab 0.011              \tab 0.009           \tab 0.013           \cr
            \tab 10      \tab 0.056              \tab 0.051           \tab 0.060           \tab 0.011              \tab 0.009           \tab 0.013           \cr
    37      \tab 1       \tab 0.050              \tab 0.046           \tab 0.055           \tab 0.010              \tab 0.008           \tab 0.012           \cr
            \tab 2       \tab 0.054              \tab 0.049           \tab 0.058           \tab 0.011              \tab 0.009           \tab 0.013           \cr
            \tab 3       \tab 0.054              \tab 0.049           \tab 0.058           \tab 0.011              \tab 0.009           \tab 0.013           \cr
            \tab 4       \tab 0.054              \tab 0.050           \tab 0.058           \tab 0.010              \tab 0.008           \tab 0.012           \cr
            \tab 5       \tab 0.054              \tab 0.049           \tab 0.058           \tab 0.010              \tab 0.008           \tab 0.012           \cr
            \tab 6       \tab 0.054              \tab 0.050           \tab 0.058           \tab 0.011              \tab 0.009           \tab 0.013           \cr
            \tab 7       \tab 0.055              \tab 0.051           \tab 0.060           \tab 0.010              \tab 0.008           \tab 0.012           \cr
            \tab 8       \tab 0.055              \tab 0.050           \tab 0.059           \tab 0.011              \tab 0.009           \tab 0.013           \cr
            \tab 9       \tab 0.053              \tab 0.049           \tab 0.058           \tab 0.011              \tab 0.009           \tab 0.013           \cr
            \tab 10      \tab 0.049              \tab 0.045           \tab 0.054           \tab 0.009              \tab 0.007           \tab 0.011           \cr
    38      \tab 1       \tab 0.049              \tab 0.045           \tab 0.053           \tab 0.009              \tab 0.007           \tab 0.011           \cr
            \tab 2       \tab 0.052              \tab 0.047           \tab 0.056           \tab 0.008              \tab 0.007           \tab 0.010           \cr
            \tab 3       \tab 0.054              \tab 0.050           \tab 0.059           \tab 0.011              \tab 0.009           \tab 0.013           \cr
            \tab 4       \tab 0.055              \tab 0.050           \tab 0.059           \tab 0.011              \tab 0.009           \tab 0.013           \cr
            \tab 5       \tab 0.056              \tab 0.052           \tab 0.061           \tab 0.012              \tab 0.010           \tab 0.014           \cr
            \tab 6       \tab 0.055              \tab 0.050           \tab 0.059           \tab 0.011              \tab 0.009           \tab 0.013           \cr
            \tab 7       \tab 0.049              \tab 0.045           \tab 0.053           \tab 0.009              \tab 0.007           \tab 0.011           \cr
            \tab 8       \tab 0.052              \tab 0.048           \tab 0.057           \tab 0.010              \tab 0.008           \tab 0.012           \cr
            \tab 9       \tab 0.054              \tab 0.050           \tab 0.059           \tab 0.010              \tab 0.009           \tab 0.012           \cr
            \tab 10      \tab 0.055              \tab 0.050           \tab 0.059           \tab 0.011              \tab 0.009           \tab 0.013           \cr
    39      \tab 1       \tab 0.047              \tab 0.043           \tab 0.051           \tab 0.010              \tab 0.008           \tab 0.012           \cr
            \tab 2       \tab 0.055              \tab 0.051           \tab 0.059           \tab 0.010              \tab 0.008           \tab 0.012           \cr
            \tab 3       \tab 0.053              \tab 0.049           \tab 0.057           \tab 0.010              \tab 0.008           \tab 0.012           \cr
            \tab 4       \tab 0.053              \tab 0.049           \tab 0.058           \tab 0.010              \tab 0.009           \tab 0.012           \cr
            \tab 5       \tab 0.052              \tab 0.048           \tab 0.057           \tab 0.010              \tab 0.008           \tab 0.012           \cr
            \tab 6       \tab 0.053              \tab 0.049           \tab 0.058           \tab 0.010              \tab 0.008           \tab 0.012           \cr
            \tab 7       \tab 0.057              \tab 0.052           \tab 0.061           \tab 0.011              \tab 0.009           \tab 0.013           \cr
            \tab 8       \tab 0.057              \tab 0.053           \tab 0.062           \tab 0.012              \tab 0.010           \tab 0.014           \cr
            \tab 9       \tab 0.050              \tab 0.046           \tab 0.055           \tab 0.010              \tab 0.008           \tab 0.012           \cr
            \tab 10      \tab 0.056              \tab 0.051           \tab 0.060           \tab 0.011              \tab 0.009           \tab 0.013           \cr
    40      \tab 1       \tab 0.049              \tab 0.045           \tab 0.054           \tab 0.010              \tab 0.008           \tab 0.012           \cr
            \tab 2       \tab 0.052              \tab 0.048           \tab 0.057           \tab 0.010              \tab 0.009           \tab 0.012           \cr
            \tab 3       \tab 0.055              \tab 0.050           \tab 0.059           \tab 0.011              \tab 0.009           \tab 0.013           \cr
            \tab 4       \tab 0.054              \tab 0.050           \tab 0.059           \tab 0.011              \tab 0.009           \tab 0.013           \cr
            \tab 5       \tab 0.054              \tab 0.050           \tab 0.059           \tab 0.010              \tab 0.008           \tab 0.012           \cr
            \tab 6       \tab 0.049              \tab 0.045           \tab 0.053           \tab 0.010              \tab 0.008           \tab 0.012           \cr
            \tab 7       \tab 0.056              \tab 0.051           \tab 0.060           \tab 0.011              \tab 0.009           \tab 0.013           \cr
            \tab 8       \tab 0.054              \tab 0.050           \tab 0.059           \tab 0.011              \tab 0.009           \tab 0.013           \cr
            \tab 9       \tab 0.047              \tab 0.043           \tab 0.052           \tab 0.010              \tab 0.008           \tab 0.011           \cr
            \tab 10      \tab 0.058              \tab 0.054           \tab 0.063           \tab 0.010              \tab 0.008           \tab 0.012           
  }
}
\value{
  A list of class \code{"gofOutlier"} containing the results of the hypothesis test.  
  See the help file for \code{\link{gofOutlier.object}} for details.
}
\references{
  Barnett, V., and T. Lewis. (1995).  \emph{Outliers in Statistical Data}.  Third Edition.  
  John Wiley & Sons, Chichester, UK, pp. 235--236.

  Gilbert, R.O. (1987). \emph{Statistical Methods for Environmental Pollution Monitoring}.  
  Van Nostrand Reinhold, NY, pp.188--191.

  McBean, E.A, and F.A. Rovers. (1992).  Estimation of the Probability of Exceedance of 
  Contaminant Concentrations.  \emph{Ground Water Monitoring Review} \bold{Winter}, 
  pp. 115--119.

  McNutt, M. (2014).  Raising the Bar.  \emph{Science} \bold{345}(6192), p. 9.

  Rosner, B. (1975).  On the Detection of Many Outliers.  
  \emph{Technometrics} \bold{17}, 221--227.

  Rosner, B. (1983).  Percentage Points for a Generalized ESD Many-Outlier Procedure.  
  \emph{Technometrics} \bold{25}, 165--172.

  USEPA. (2006).  \emph{Data Quality Assessment:  A Reviewer's Guide}.  EPA QA/G-9R.  
  EPA/240/B-06/002, February 2006.  Office of Environmental Information, U.S. 
  Environmental Protection Agency, Washington, D.C.

  USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA 
  Facilities, Unified Guidance}.  EPA 530/R-09-007, March 2009.  Office of Resource 
  Conservation and Recovery Program Implementation and Information Division.  U.S. 
  Environmental Protection Agency, Washington, D.C., pp. 12-10 to 12-14.

  USEPA. (2013a).  \emph{ProUCL Version 5.0.00 Technical Guide}.  EPA/600/R-07/041, 
  September 2013.  Office of Research and Development.  U.S. Environmental Protection 
  Agency, Washington, D.C., pp. 190--195.

  USEPA. (2013b).  \emph{ProUCL Version 5.0.00 User Guide}.  EPA/600/R-07/041, 
  September 2013.  Office of Research and Development.  U.S. Environmental Protection 
  Agency, Washington, D.C., pp. 190--195.
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  Rosner's test is a commonly used test for \dQuote{outliers} when you are willing to 
  assume that the data without outliers follows a normal (Gaussian) distribution.  It is 
  designed to avoid \emph{masking}, which occurs when an outlier goes undetected because 
  it is close in value to another outlier.  

  Rosner's test is a kind of discordancy test (Barnett and Lewis, 1995).  The test 
  statistic of a discordancy test is usually a ratio:  the numerator is the difference 
  between the suspected outlier and some summary statistic of the data set 
  (e.g., mean, next largest observation, etc.), while the denominator is always a measure 
  of spread within the data (e.g., standard deviation, range, etc.).  Both USEPA (2009) 
  and USEPA (2013a,b) discuss two commonly used discordancy tests:  Dixon's test and 
  Rosner's test.  Both of these tests assume that all of the data that are not outliers 
  come from a normal (Gaussian) distribution.  

  There are many forms of Dixon's test (Barnett and Lewis, 1995).  The one presented in 
  USEPA (2009) and USEPA (20013a,b) assumes just one outlier (Dixon, 1953).  This test 
  is vulnerable to "masking" in which the presence of several outliers masks the fact 
  that even one outlier is present.  There are also other forms of Dixon's test that 
  allow for more than one outlier based on a sequence of sub-tests, but these tests are 
  also vulnerable to masking.

  Rosner's test allows you to test for several possible outliers and avoids the problem of 
  masking.  Rosner's test requires you to set the number of suspected outliers, \eqn{k}, 
  in advance.  As in the case of Dixon's test, there are several forms of Rosner's test, 
  so you need to be aware of which one you are using.  The form of Rosner's test presented in 
  USEPA (2009) is based on the extreme Studentized deviate (ESD) (Rosner, 1975), whereas the 
  form of Rosner's test performed by the \pkg{EnvStats} function \code{rosnerTest} and 
  presented in USEPA (2013a,b) is based on the \bold{generalized} ESD (Rosner, 1983; Gilbert, 1987).  
  USEPA (2013a, p. 190) cites both Rosner (1975) and Rosner (1983), but presents only the 
  test given in Rosner (1983).  Rosner's test based on the ESD has the appropriate Type I 
  error level if there are no outliers in the dataset, but if there are actually say \eqn{m} 
  outliers, where \eqn{m < k}, then the ESD version of Rosner's test tends to declare 
  more than \eqn{m} outliers with a probability that is greater than the stated Type I 
  error level (referred to as \dQuote{swamping}).  Rosner's test based on the 
  generalized ESD fixes this problem.  USEPA (2013a, pp. 17, 191) incorrectly states that 
  the generalized ESD version of Rosner's test is vulnerable to masking.  Surprisingly, 
  the well-known book on statistical outliers by Barnett and Lewis (1995) does not 
  discuss Rosner's generalized ESD test.

  As noted, using Rosner's test requires specifying the number of suspected outliers, 
  \eqn{k}, in advance.  USEPA (2013a, pp.190-191) states:  
  \dQuote{A graphical display (Q-Q plot) can be used to identify suspected outliers 
  needed to perform the Rosner test}, and USEPA (2009, p. 12-11) notes:  
  \dQuote{A potential drawback of Rosner's test is that the user must first identify 
  the maximum number of potential outliers (k) prior to running the test.  Therefore, 
  this requirement makes the test ill-advised as an automatic outlier screening tool, 
  and somewhat reliant on the user to identify candidate outliers.}

  When observations contain non-detect values (NDs), USEPA (2013a, p. 191) states:  
  \dQuote{one may replace the NDs by their respective detection limits (DLs), DL/2, or may 
  just ignore them ....}  This is bad advice, as this method of dealing with non-detects 
  will produce Type I error rates that are not correct. \cr

  \bold{OUTLIERS ARE NOT NECESSARILY INCORRECT VALUES}\cr  
  Whether an observation is an \dQuote{outlier} depends on the underlying assumed 
  statistical model.  McBean and Rovers (1992) state: \cr   
  \dQuote{It may be possible to ignore the outlier if a physical rationale is available but, 
  failing that, the value must be included ....  Note that the use of statistics does not 
  interpret the facts, it simply makes the facts easier to see.  Therefore, it is incumbent 
  on the analyst to identify whether or not the high value ... is truly representative of 
  the chemical being monitored or, instead, is an outlier for reasons such as a result of 
  sampling or laboratory error.}

  USEPA (2006, p.51) states:  \cr
  \dQuote{If scientific reasoning does not explain the outlier, it should not be 
  discarded from the data set.}    

  Finally, an editorial by the Editor-in-Chief of the journal \emph{Science} deals with 
  this topic (McNutt, 2014).

  You can use the functions \code{\link{qqPlot}} and \code{\link{gofTest}} to explore 
  other possible statistical models for the data, or you can use nonparametric statistics 
  if you do not want to assume a particular distribution.
}
\seealso{
  \code{\link{gofTest}}, \code{\link{gofOutlier.object}}, \code{\link{print.gofOutlier}}, 
  \link{Normal}, \code{\link{qqPlot}}.
}
\examples{
  # Combine 30 observations from a normal distribution with mean 3 and 
  # standard deviation 2, with 3 observations from a normal distribution 
  # with mean 10 and standard deviation 1, then run Rosner's Test on these 
  # data, specifying k=4 potential outliers based on looking at the 
  # normal Q-Q plot. 
  # (Note: the call to set.seed simply allows you to reproduce 
  # this example.)

  set.seed(250) 

  dat <- c(rnorm(30, mean = 3, sd = 2), rnorm(3, mean = 10, sd = 1)) 

  dev.new()
  qqPlot(dat)

  rosnerTest(dat, k = 4)

  #Results of Outlier Test
  #-------------------------
  #
  #Test Method:                     Rosner's Test for Outliers
  #
  #Hypothesized Distribution:       Normal
  #
  #Data:                            dat
  #
  #Sample Size:                     33
  #
  #Test Statistics:                 R.1 = 2.848514
  #                                 R.2 = 3.086875
  #                                 R.3 = 3.033044
  #                                 R.4 = 2.380235
  #
  #Test Statistic Parameter:        k = 4
  #
  #Alternative Hypothesis:          Up to 4 observations are not
  #                                 from the same Distribution.
  #
  #Type I Error:                    5%
  #
  #Number of Outliers Detected:     3
  #
  #  i   Mean.i     SD.i      Value Obs.Num    R.i+1 lambda.i+1 Outlier
  #1 0 3.549744 2.531011 10.7593656      33 2.848514   2.951949    TRUE
  #2 1 3.324444 2.209872 10.1460427      31 3.086875   2.938048    TRUE
  #3 2 3.104392 1.856109  8.7340527      32 3.033044   2.923571    TRUE
  #4 3 2.916737 1.560335 -0.7972275      25 2.380235   2.908473   FALSE

  #----------
  # Clean up

  rm(dat)
  graphics.off()

  #--------------------------------------------------------------------

  # Example 12-4 of USEPA (2009, page 12-12) gives an example of 
  # using Rosner's test to test for outliers in napthalene measurements (ppb)
  # taken at 5 background wells over 5 quarters.  The data for this example 
  # are stored in EPA.09.Ex.12.4.naphthalene.df.

  EPA.09.Ex.12.4.naphthalene.df
  #   Quarter Well Naphthalene.ppb
  #1        1 BW.1            3.34
  #2        2 BW.1            5.39
  #3        3 BW.1            5.74
  # ...
  #23       3 BW.5            5.53
  #24       4 BW.5            4.42
  #25       5 BW.5           35.45

  longToWide(EPA.09.Ex.12.4.naphthalene.df, "Naphthalene.ppb", "Quarter", "Well", 
    paste.row.name = TRUE)
  #          BW.1 BW.2  BW.3 BW.4  BW.5
  #Quarter.1 3.34 5.59  1.91 6.12  8.64
  #Quarter.2 5.39 5.96  1.74 6.05  5.34
  #Quarter.3 5.74 1.47 23.23 5.18  5.53
  #Quarter.4 6.88 2.57  1.82 4.43  4.42
  #Quarter.5 5.85 5.39  2.02 1.00 35.45


  # Look at Q-Q plots for both the raw and log-transformed data
  #------------------------------------------------------------

  dev.new()
  with(EPA.09.Ex.12.4.naphthalene.df, 
    qqPlot(Naphthalene.ppb, add.line = TRUE, 
      main = "Figure 12-6.  Naphthalene Probability Plot"))

  dev.new()
  with(EPA.09.Ex.12.4.naphthalene.df, 
    qqPlot(Naphthalene.ppb, dist = "lnorm", add.line = TRUE, 
      main = "Figure 12-7.  Log Naphthalene Probability Plot"))


  # Test for 2 potential outliers on the original scale:
  #-----------------------------------------------------

  with(EPA.09.Ex.12.4.naphthalene.df, rosnerTest(Naphthalene.ppb, k = 2))

  #Results of Outlier Test
  #-------------------------
  #
  #Test Method:                     Rosner's Test for Outliers
  #
  #Hypothesized Distribution:       Normal
  #
  #Data:                            Naphthalene.ppb
  #
  #Sample Size:                     25
  #
  #Test Statistics:                 R.1 = 3.930957
  #                                 R.2 = 4.160223
  #
  #Test Statistic Parameter:        k = 2
  #
  #Alternative Hypothesis:          Up to 2 observations are not
  #                                 from the same Distribution.
  #
  #Type I Error:                    5%
  #
  #Number of Outliers Detected:     2
  #
  #  i  Mean.i     SD.i Value Obs.Num    R.i+1 lambda.i+1 Outlier
  #1 0 6.44240 7.379271 35.45      25 3.930957   2.821681    TRUE
  #2 1 5.23375 4.325790 23.23      13 4.160223   2.801551    TRUE

  #----------
  # Clean up

  graphics.off()
}
\keyword{ htest }
\keyword{ models }
