\name{ppointsCensored}
\alias{ppointsCensored}
\title{
  Plotting Positions for Type I Censored Data
}
\description{
  Returns a list of \dQuote{ordered} observations and associated plotting positions based on
  Type I left-censored or right-censored data.  These plotting positions may be used to
  construct empirical cumulative distribution plots or quantile-quantile plots, or to estimate
  distribution parameters.
}
\usage{
  ppointsCensored(x, censored, censoring.side = "left",
    prob.method = "michael-schucany", plot.pos.con = 0.375)
}
\arguments{
  \item{x}{
  numeric vector of observations.  Missing (\code{NA}), undefined (\code{NaN}), and
  infinite (\code{Inf}, \code{-Inf}) values are allowed but will be removed.
}
  \item{censored}{
  numeric or logical vector indicating which values of \code{x} are censored.  This must be the
  same length as \code{x}.  If the mode of \code{censored} is \code{"logical"}, \code{TRUE} values
  correspond to elements of \code{x} that are censored, and \code{FALSE} values correspond to
  elements of \code{x} that are not censored.  If the mode of \code{censored} is \code{"numeric"},
  it must contain only \code{1}'s and \code{0}'s; \code{1} corresponds to \code{TRUE} and
  \code{0} corresponds to \code{FALSE}.  Missing (\code{NA}) values are allowed but will be removed.
}
  \item{censoring.side}{
  character string indicating on which side the censoring occurs.  The possible values are
  \code{"left"} (the default) and \code{"right"}.
}
  \item{prob.method}{
  character string indicating what method to use to compute the plotting positions
  (empirical probabilities).  Possible values are: \cr
  \code{"kaplan-meier"} (product-limit method of Kaplan and Meier (1958)), \cr
  \code{"modified kaplan-meier"} (modification of Kaplan-Meier method), \cr
  \code{"nelson"} (hazard plotting method of Nelson (1972)), \cr
  \code{"michael-schucany"} (generalization of the product-limit method due to Michael and Schucany (1986)), and \cr
  \code{"hirsch-stedinger"} (generalization of the product-limit method due to Hirsch and Stedinger (1987)). \cr

  The default value is \code{prob.method="michael-schucany"}.

  The \code{"nelson"} method is only available for \code{censoring.side="right"}, and
  the \code{"modified kaplan-meier"} method is only available for \cr
  \code{censoring.side="left"}.  See the DETAILS section for more explanation.
}
  \item{plot.pos.con}{
  numeric scalar between 0 and 1 containing the value of the plotting position constant.
  The default value is \code{plot.pos.con=0.375}.  See the DETAILS section for more information.
  This argument is used only if \code{prob.method} is equal to \code{"michael-schucany"} or
  \code{"hirsch-stedinger"}.
}
}
\details{
  Methods for computing plotting positions for complete data sets
  (no censored observations) are discussed in D'Agostino, R.B. (1986a) and
  Cleveland (1993).  For data sets with censored observations, these methods
  must be modified.  The function \code{ppointsCensored} allows you to compute
  plotting positions based on any of the following methods:
  \itemize{
  \item Product-limit method of Kaplan and Meier (1958) (\code{prob.method="kaplan-meier"}).
  \item Hazard plotting method of Nelson (1972) (\code{prob.method="nelson"}).
  \item Generalization of the product-limit method due to Michael and Schucany (1986) \cr
  (\code{prob.method="michael-schucany"}) (the default).
  \item Generalization of the product-limit method due to Hirsch and Stedinger (1987) \cr
  (\code{prob.method="hirsch-stedinger"}).
  }

  Let \eqn{\underline{x}} denote a random sample of \eqn{N} observations from
  some distribution.  Assume \eqn{n} (\eqn{0 < n < N}) of these
  observations are known and \eqn{c} (\eqn{c=N-n}) of these observations are
  all censored below (left-censored) or all censored above (right-censored) at
  \eqn{k} fixed censoring levels
  \deqn{T_1, T_2, \ldots, T_K; \; K \ge 1 \;\;\;\;\;\; (1)}
  For the case when \eqn{K \ge 2}, the data are said to be Type I
  \bold{\emph{multiply censored}}.  For the case when \eqn{K=1},
  set \eqn{T = T_1}.  If the data are left-censored
  and all \eqn{n} known observations are greater
  than or equal to \eqn{T}, or if the data are right-censored and all \eqn{n}
  known observations are less than or equal to \eqn{T}, then the data are
  said to be Type I \bold{\emph{singly censored}} (Nelson, 1982, p.7), otherwise
  they are considered to be Type I multiply censored.

  Let \eqn{c_j} denote the number of observations censored below or above censoring
  level \eqn{T_j} for \eqn{j = 1, 2, \ldots, K}, so that
  \deqn{\sum_{i=1}^K c_j = c \;\;\;\;\;\; (2)}
  Let \eqn{x_{(1)}, x_{(2)}, \ldots, x_{(N)}} denote the \dQuote{ordered} observations,
  where now \dQuote{observation} means either the actual observation (for uncensored
  observations) or the censoring level (for censored observations).  For
  right-censored data, if a censored observation has the same value as an
  uncensored one, the uncensored observation should be placed first.
  For left-censored data, if a censored observation has the same value as an
  uncensored one, the censored observation should be placed first.

  Note that in this case the quantity \eqn{x_{(i)}} does not necessarily represent
  the \eqn{i}'th \dQuote{largest} observation from the (unknown) complete sample.

  Finally, let \eqn{\Omega} (omega) denote the set of \eqn{n} subscripts in the
  \dQuote{ordered} sample that correspond to uncensored observations.
  \cr

  \bold{Product-Limit Method of Kaplan and Meier} (\code{prob.method="kaplan-meier"}) \cr
  For complete data sets (no censored observations), the
  \bold{\emph{empirical probabilities}} estimator of the cumulative distribution
  function evaluated at the \eqn{i}'th ordered observation is given by
  (D'Agostino, 1986a, p.8):
  \deqn{\hat{F}[x_{(i)}] = \hat{p}_i = \frac{\#[x_j \le x_{(i)}]}{n} \;\;\;\;\;\; (3)}
  where \eqn{\#[x_j \le x_{(i)}]} denotes the number of observations less than
  or equal to \eqn{x_{(i)}} (see the help file for \code{\link{ecdfPlot}}).
  Kaplan and Meier (1958) extended this method of computing the empirical cdf to
  the case of right-censored data.
  \cr

  \emph{Right-Censored Data} (\code{censoring.side="right"})\cr
  Let \eqn{S(t)} denote the survival function evaluated at \eqn{t}, that is:
  \deqn{S(t) = 1 - F(t) = Pr(X > t) \;\;\;\;\;\; (4)}
  Kaplan and Meier (1958) show that a nonparametric estimate of the survival
  function at the \eqn{i}'th ordered observation that is not censored
  (i.e., \eqn{i \in \Omega}), is given by:
  \tabular{lcl}{
  \eqn{\hat{S}[x_{(i)}]} \tab \eqn{=} \tab \eqn{\widehat{Pr}[X > x_{(i)}]} \cr
                         \tab \eqn{=} \tab \eqn{\widehat{Pr}[X > x_{(1)}]} \cr
                         \tab         \tab \eqn{\;\; \widehat{Pr}[X > x_{(2)} | X > x_{(1)}] \;\; \cdots} \cr
                         \tab         \tab \eqn{\;\; \widehat{Pr}[X > x_{(i)} | X > x_{(i-1)}]} \cr
                         \tab \eqn{=} \tab \eqn{\prod_{j \in \Omega, j \le i} \frac{n_j - d_j}{n_j}, \;\; i \in \Omega \;\;\;\;\; (5)}
  }
  where \eqn{n_j} is the number of observations (uncensored or censored) with values
  greater than or equal to \eqn{x_{(j)}}, and \eqn{d_j} denotes the number of
  uncensored observations exactly equal to \eqn{x_{(j)}} (if there are no tied
  uncensored observations then \eqn{d_j} will equal 1 for all values of \eqn{j}).
  (See also Lee and Wang, 2003, pp. 64--69; Michael and Schucany, 1986).  By convention,
  the estimate of the survival function at a censored observation is set equal to
  the estimated value of the survival function at the largest uncensored observation
  less than or equal to that censoring level.  If there are no uncensored observations
  less than or equal to a particular censoring level, the estimate of the survival
  function is set to 1 for that censoring level.

  Thus the Kaplan-Meier plotting position at the \eqn{i}'th ordered observation
  that is not censored (i.e., \eqn{i \in \Omega}), is given by:
  \deqn{\hat{p}_i = \hat{F}[x_{(i)}] = 1 - \prod_{j \in \Omega, j \le i} \frac{n_j - d_j}{n_j} \;\;\;\;\;\; (6)}

  The plotting position for a censored observation is set equal to the plotting
  position associated with the largest uncensored observation less than or equal to
  that censoring level.  If there are no uncensored observations less than or equal
  to a particular censoring level, the plotting position is set to 0 for that
  censoring level.

  As an example, consider the following right-censored data set:
  \deqn{3, \ge4, \ge4, 5, 5, 6}
  The table below shows how the plotting positions are computed.
  \tabular{llllll}{
  \eqn{i} \tab \eqn{x_{(i)}} \tab \eqn{n_i} \tab \eqn{d_i} \tab \eqn{\frac{n_i-d_i}{n_i}} \tab Plotting Position \cr
   1      \tab \eqn{3}       \tab \eqn{6}   \tab \eqn{1}   \tab \eqn{5/6}                 \tab \eqn{1 - (5/6) = 0.167} \cr
   2      \tab \eqn{\ge4}    \tab           \tab           \tab                           \tab                       \cr
   3      \tab \eqn{\ge4}    \tab           \tab           \tab                           \tab                       \cr
   4      \tab \eqn{5}       \tab \eqn{3}   \tab \eqn{2}   \tab \eqn{1/3}                 \tab \eqn{1 - (5/6)(1/3) = 0.722} \cr
   5      \tab \eqn{5}       \tab           \tab           \tab                           \tab \eqn{0.722} \cr
   6      \tab \eqn{6}       \tab \eqn{1}   \tab \eqn{1}   \tab \eqn{0/1}                 \tab \eqn{1 - (5/6)(1/3)(0/1) = 1}
  }
  Note that for complete data sets, Equation (6) reduces to Equation (3).
  \cr

  \emph{Left-Censored Data} (\code{censoring.side="left"}) \cr
  Gillespie et al. (2010) give formulas for the Kaplan-Meier estimator for the case of
  left-cesoring (\code{censoring.side="left"}).  In this case, the plotting position
  for the \eqn{i}'th ordered observation, assuming it is not censored, is computed as:
  \deqn{\hat{p}_i = \hat{F}[x_{(i)}] = \prod_{j \in \Omega, j > i} \frac{n_j - d_j}{n_j} \;\;\;\;\;\; (7)}
  where \eqn{n_j} is the number of observations (uncensored or censored) with values
  less than or equal to \eqn{x_{(j)}}, and \eqn{d_j} denotes the number of
  uncensored observations exactly equal to \eqn{x_{(j)}} (if there are no tied
  uncensored observations then \eqn{d_j} will equal 1 for all values of \eqn{j}).
  The plotting position is equal to 1 for the largest uncensored order statistic.

  As an example, consider the following left-censored data set:
  \deqn{3, <4, <4, 5, 5, 6}
  The table below shows how the plotting positions are computed.
  \tabular{llllll}{
  \eqn{i} \tab \eqn{x_{(i)}} \tab \eqn{n_i} \tab \eqn{d_i} \tab \eqn{\frac{n_i-d_i}{n_i}} \tab Plotting Position \cr
   1      \tab \eqn{3}       \tab \eqn{1}   \tab \eqn{1}   \tab \eqn{0/1}                 \tab \eqn{1(5/6)(3/5) = 0.5} \cr
   2      \tab \eqn{<4}      \tab           \tab           \tab                           \tab                       \cr
   3      \tab \eqn{<4}      \tab           \tab           \tab                           \tab                       \cr
   4      \tab \eqn{5}       \tab \eqn{5}   \tab \eqn{2}   \tab \eqn{3/5}                 \tab \eqn{0.833} \cr
   5      \tab \eqn{5}       \tab           \tab           \tab                           \tab \eqn{1(5/6) = 0.833} \cr
   6      \tab \eqn{6}       \tab \eqn{6}   \tab \eqn{1}   \tab \eqn{5/6}                 \tab \eqn{1}
  }
  Note that for complete data sets, Equation (7) reduces to Equation (3).
  \cr

  \bold{Modified Kaplan-Meier Method} (\code{prob.method="modified kaplan-meier"}) \cr
  (Left-Censored Data Only.)  For left-censored data, the modified Kaplan-Meier
  method is the same as the Kaplan-Meier method, except that for the largest
  uncensored order statistic, the plotting position is not set to 1 but rather is
  set equal to the Blom plotting position:  \eqn{(N - 0.375)/(N + 0.25)}.  This method
  is useful, for example, when creating \link[=qqPlotCensored]{Quantile-Quantile plots}.
  \cr

  \bold{Hazard Plotting Method of Nelson} (\code{prob.method="nelson"}) \cr
  (Right-Censored Data Only.)  For right-censored data, Equation (5) can be
  re-written as:
  \deqn{\hat{S}[x_{(i)}] = \prod_{j \in \Omega, j \le i} \frac{N-j}{N-j+1}, \;\; i \in \Omega \;\;\;\;\;\; (8)}
  Nelson (1972) proposed the following formula for plotting positions for
  the uncensored observations in the context of estimating the hazard function
  (see Michael and Schucany,1986, p.469):
  \deqn{\hat{p}_i = \hat{F}[x_{(i)}] = 1 - \prod_{j \in \Omega, j \le i} exp(\frac{-1}{N-j+1}) \;\;\;\;\;\; (9)}
  See Lee and Wang (2003) for more information about the hazard function.

  As for the Kaplan and Meier (1958) method, the plotting position for a censored
  observation is set equal to the plotting position associated with the largest
  uncensored observation less than or equal to that censoring level.  If there are
  no uncensored observations less than or equal to a particular censoring level,
  the plotting position is set to 0 for that censoring level.
  \cr

  \bold{Generalization of Product-Limit Method, Michael and Schucany} \cr
  (\code{prob.method="michael-schucany"}) \cr
  For complete data sets, the disadvantage of using Equation (3) above to define
  plotting positions is that it implies the largest observed value is the maximum
  possible value of the distribution (the \eqn{100}'th percentile).  This may be
  satisfactory if the underlying distribution is known to be discrete, but it is
  usually not satisfactory if the underlying distribution is known to be continuous.

  A more frequently used formula for plotting positions for complete data sets is
  given by:
  \deqn{\hat{F}[x_{(i)}] = \hat{p}_i = \frac{i - a}{N - 2a + 1} \;\;\;\;\;\; (10)}
  where \eqn{0 \le a \le 1} (Cleveland, 1993, p. 18; D'Agostino, 1986a, pp. 8,25).
  The value of \eqn{a} is usually chosen so that the plotting positions are
  approximately unbiased (i.e., approximate the mean of their distribution) or else
  approximate the median value of their distribution (see the help file for
  \code{\link{ecdfPlot}}).  Michael and Schucany (1986) extended this method for
  both left- and right-censored data sets.
  \cr

  \emph{Right-Censored Data} (\code{censoring.side="right"})\cr
  For right-censored data sets, the plotting positions for the uncensored
  observations are computed as:
  \deqn{\hat{p}_i = 1 - \frac{N-a+1}{N-2a+1} \prod_{j \in \Omega, j \le i} \frac{N-j-a+1}{N-j-a+2} \;\; i \in \Omega \;\;\;\;\;\; (11)}
  Note that the plotting positions proposed by Herd (1960) and Johnson (1964) are a
  special case of Equation (11) with \eqn{a=0}.  Equation (11) reduces to Equation (10)
  in the case of complete data sets.  Note that unlike the Kaplan-Meier method,
  plotting positions associated with tied uncensored observations are not the same
  (just as in the case for complete data using Equation (10)).

  As for the Kaplan and Meier (1958) method, for right-censored data the plotting
  position for a censored observation is set equal to the plotting position associated
  with the largest uncensored observation less than or equal to that censoring level.
  If there are no uncensored observations less than or equal to a particular censoring
  level, the plotting position is set to 0 for that censoring level.
  \cr

  \emph{Left-Censored Data} (\code{censoring.side="left"}) \cr
  For left-censored data sets the plotting positions are computed as:
  \deqn{\hat{p}_i = \frac{N-a+1}{N-2a+1} \prod_{j \in \Omega, j \ge i} \frac{j-a}{j-a+1} \;\; i \in \Omega \;\;\;\;\;\; (12)}
  Equation (12) reduces to Equation (10) in the case of complete data
  sets.  Note that unlike the Kaplan-Meier method, plotting positions associated with
  tied uncensored observations are not the same (just as in the case for complete
  data using Equation (10)).

  For left-censored data, the plotting position for a censored observation is set
  equal to the plotting position associated with the smallest uncensored observation
  greater than or equal to that censoring level.  If there are no uncensored
  observations greater than or equal to a particular censoring level, the plotting
  position is set to 1 for that censoring level.
  \cr

  \bold{Generalization of Product-Limit Method, Hirsch and Stedinger} \cr
  (\code{prob.method="hirsch-stedinger"}) \cr
  Hirsch and Stedinger (1987) use a slightly different approach than Kaplan and Meier
  (1958) and Michael and Schucany (1986) to derive a nonparametric estimate of the
  survival function (probability of exceedance) in the context of left-censored data.
  First they estimate the value of the survival function at each of the censoring
  levels.  The value of the survival function for an uncensored observation between
  two adjacent censoring levels is then computed by linear interpolation (in the form
  of a plotting position).  See also Helsel and Cohn (1988).

  The discussion below presents an extension of the method of Hirsch and Stedinger
  (1987) to the case of right-censored data, and then presents the original derivation
  due to Hirsch and Stedinger (1987) for left-censored data.

  \emph{Right-Censored Data} (\code{censoring.side="right"})\cr
  For right-censored data, the survival function is estimated as follows.
  For the \eqn{j}'th censoring level (\eqn{j = 0, 1, \ldots, K}), write the
  value of the survival function as:
  \tabular{lcl}{
  \eqn{S(T_j)} \tab \eqn{=}  \tab \eqn{Pr[X > T_j]} \cr
               \tab \eqn{=}  \tab \eqn{Pr[X > T_{j+1}] + Pr[T_j < X \le T_{j+1}]} \cr
               \tab \eqn{=}  \tab \eqn{S(T_{j+1}) + Pr[T_j < X \le T_{j+1} | X > T_j] Pr[X > T_j]} \cr
               \tab \eqn{=}  \tab \eqn{S(T_{j+1}) + Pr[T_j < X \le T_{j+1} | X > T_j] S(T_j) \;\;\;\;\;\; (13)}
  }
  where
  \deqn{T_0 = -\infty, \;\;\;\;\;\; (14)}
  \deqn{T_{K+1} = \infty \;\;\;\;\;\; (15)}
  Now set
  \tabular{lcl}{
  \eqn{A_j} \tab \eqn{=} \tab # uncensored observations in \eqn{(T_j, T_{j+1}] \;\;\;\;\;\; (16)} \cr
  \eqn{B_j} \tab \eqn{=} \tab # observations in \eqn{(T_{j+1}, \infty) \;\;\;\;\;\; (17)}
  }
  for \eqn{j = 0, 1, \ldots, K}.  Then the method of moments estimator of the
  conditional probability in Equation (13) is given by:
  \deqn{\widehat{Pr}[T_j < X \le T_{j+1} | X > T_j] = \frac{A_j}{A_j + B_j} \;\;\;\;\;\; (18)}
  Hence, by equations (13) and (18) we have
  \deqn{\hat{S}(T_j) = \hat{S}(T_{j+1}) + (\frac{A_j}{A_j + B_j}) \hat{S}(T_{j}) \;\;\;\;\;\; (19)}
  which can be rewritten as:
  \deqn{\hat{S}(T_{j+1}) = \hat{S}(T_j) [1 - (\frac{A_j}{A_j + B_j})] \;\;\;\;\;\; (20)}
  Equation (20) can be solved interatively for \eqn{j = 1, 2, \ldots, K}.  Note that
  \deqn{\hat{S}(T_0) = \hat{S}(-\infty) = S(-\infty) = 1 \;\;\;\;\;\; (21)}
  \deqn{\hat{S}(T_{K+1}) = \hat{S}(\infty) = S(\infty) = 0 \;\;\;\;\;\; (22)}

  Once the values of the survival function at the censoring levels are computed, the
  plotting positions for the \eqn{A_j} uncensored observations in the interval
  \eqn{(T_J, T_{j+1}]} (\eqn{j = 0, 1, \ldots, K}) are computed as
  \deqn{\hat{p}_i = [1 - \hat{S}(T_j)] + [\hat{S}(T_j) - \hat{S}(T_{j+1})] \frac{r-a}{A_j - 2a + 1} \;\;\;\;\;\; (23)}
  where \eqn{a} denotes the plotting position constant, \eqn{0 \le a \le 1}, and
  \eqn{r} denotes the rank of the \eqn{i}'th observation among the \eqn{A_j}
  uncensored observations in the interval \eqn{(T_J, T_{j+1}]}.
  (Tied observations are given distinct ranks.)

  For the \eqn{c_j} observations censored at censoring level \eqn{T_j}
  (\eqn{j = 1, 2, \ldots, K}), the plotting positions are computed as:
  \deqn{\hat{p}_i = 1 - [\hat{S}(T_j) \frac{r-a}{c_j - 2a + 1}]  \;\;\;\;\;\; (24)}
  where \eqn{r} denotes the rank of the \eqn{i}'th observation among the \eqn{c_j}
  observations censored at censoring level \eqn{T_j}.  Note that all the
  observations censored at the same censoring level are given distinct ranks,
  even though there is no way to distinguish between them.
  \cr

  \emph{Left-Censored Data} (\code{censoring.side="left"}) \cr
  For left-censored data, Hirsch and Stedinger (1987) modify the definition of the
  survival function as follows:
  \deqn{S^*(t) = Pr[X \ge t] \;\;\;\;\;\; (25)}
  For continuous distributions, the functions in Equations (4) and (25) are identical.

  Hirsch and Stedinger (1987) show that for the \eqn{j}'th censoring level
  (\eqn{j = 0, 1, \ldots, K}), the value of the survival function can be written as:
  \tabular{lcl}{
  \eqn{S(T_j)} \tab \eqn{=}  \tab \eqn{Pr[X \ge T_j]} \cr
               \tab \eqn{=}  \tab \eqn{Pr[X \ge T_{j+1}] + Pr[T_j \le X < T_{j+1}]} \cr
               \tab \eqn{=}  \tab \eqn{S^*(T_{j+1}) + Pr[T_j \le X < T_{j+1} | X < T_{j+1}] Pr[X < T_{j+1}]} \cr
               \tab \eqn{=}  \tab \eqn{S^*(T_{j+1}) + Pr[T_j \le X < T_{j+1} | X < T_j] [1 - S^*(T_j)] \;\;\;\;\;\; (26)}
  }
  where \eqn{T_0} and \eqn{T_{K+1}} are defined in Equations (14) and (15).

  Now set
  \tabular{lcl}{
  \eqn{A_j} \tab \eqn{=} \tab # uncensored observations in \eqn{[T_j, T_{j+1}) \;\;\;\;\;\; (27)} \cr
  \eqn{B_j} \tab \eqn{=} \tab # observations in \eqn{(-\infty, T_j) \;\;\;\;\;\; (28)}
  }
  for \eqn{j = 0, 1, \ldots, K}.  Then the method of moments estimator of the
  conditional probability in Equation (26) is given by:
  \deqn{Pr[T_j \le X < T_{j+1} | X < T_{j+1}] = \frac{A_j}{A_j + B_j} \;\;\;\;\;\; (29)}
  Hence, by Equations (26) and (29) we have
  \deqn{\hat{S}(T_j) = \hat{S}(T_{j+1}) + (\frac{A_j}{A_j + B_j}) \hat{S}(T_{j}) \;\;\;\;\;\; (30)}
  which can be solved interatively for \eqn{j = 1, 2, \ldots, K}.  Note that
  \deqn{\widehat{S^*}(T_{K+1}) = \widehat{S^*}(\infty) = S^*(\infty) = 0 \;\;\;\;\;\; (31)}
  \deqn{\widehat{S^*}(T_0) = \widehat{S^*}(-\infty) = S^*(-\infty) = 1 \;\;\;\;\;\; (32)}

  Once the values of the survival function at the censoring levels are computed, the
  plotting positions for the \eqn{A_j} uncensored observations in the interval
  \eqn{[T_J, T_{j+1})} (\eqn{j = 0, 1, \ldots, K}) are computed as
  \deqn{\hat{p}_i = [1 - \widehat{S^*}(T_j)] + [\widehat{S^*}(T_j) - \widehat{S^*}(T_{j+1})] \frac{r-a}{A_j - 2a + 1} \;\;\;\;\;\; (33)}
  where \eqn{a} denotes the plotting position constant, \eqn{0 \le a \le 0.5}, and
  \eqn{r} denotes the rank of the \eqn{i}'th observation among the \eqn{A_j}
  uncensored observations in the interval \eqn{[T_J, T_{j+1})}.
  (Tied observations are given distinct ranks.)

  For the \eqn{c_j} observations censored at censoring level \eqn{T_j}
  (\eqn{j = 1, 2, \ldots, K}), the plotting positions are computed as:
  \deqn{\hat{p}_i = [1 - \widehat{S^*}(T_j)] \frac{r-a}{c_j - 2a + 1}  \;\;\;\;\;\; (34)}
  where \eqn{r} denotes the rank of the \eqn{i}'th observation among the \eqn{c_j}
  observations censored at censoring level \eqn{T_j}.  Note that all the
  observations censored at the same censoring level are given distinct ranks,
  even though there is no way to distinguish between them.
}
\value{
  \code{ppointsCensored} returns a list with the following components:

  \item{Order.Statistics}{numeric vector of the \dQuote{ordered} observations.}
  \item{Cumulative.Probabilities}{numeric vector of the associated plotting positions.}
  \item{Censored}{logical vector indicating which of the ordered observations are censored.}
  \item{Censoring.Side}{character string indicating whether the data are left- or right-censored.
    This is same value as the argument \code{censoring.side}.}
  \item{Prob.Method}{character string indicating what method was used to compute the plotting positions.
    This is the same value as the argument \code{prob.method}.}

  Optional Component (only present when \code{prob.method="michael-schucany"} or \cr
  \code{prob.method="hirsch-stedinger"}):
  \item{Plot.Pos.Con}{numeric scalar containing the value of the plotting position constant that was used.
    This is the same as the argument \code{plot.pos.con}.}
}
\references{
  Chambers, J.M., W.S. Cleveland, B. Kleiner, and P.A. Tukey. (1983).
  \emph{Graphical Methods for Data Analysis}. Duxbury Press, Boston, MA, pp.11-16.

  Cleveland, W.S. (1993). \emph{Visualizing Data}. Hobart Press, Summit, New Jersey, 360pp.

  D'Agostino, R.B. (1986a). Graphical Analysis.
  In: D'Agostino, R.B., and M.A. Stephens, eds. \emph{Goodness-of Fit Techniques}.
  Marcel Dekker, New York, Chapter 2, pp.7-62.

  Gillespie, B.W., Q. Chen, H. Reichert, A. Franzblau, E. Hedgeman, J. Lepkowski,
  P. Adriaens, A. Demond, W. Luksemburg, and D.H. Garabrant. (2010).  Estimating Population
  Distributions When Some Data Are Below a Limit of Detection by Using a Reverse
  Kaplan-Meier Estimator.  \emph{Epidemiology} \bold{21}(4), S64--S70.

  Helsel, D.R. (2012). \emph{Statistics for Censored Environmental Data Using Minitab and R,
  Second Edition}.  John Wiley & Sons, Hoboken, New Jersey.

  Helsel, D.R., and T.A. Cohn. (1988). Estimation of Descriptive Statistics for Multiply Censored
  Water Quality Data. \emph{Water Resources Research} \bold{24}(12), 1997-2004.

  Hirsch, R.M., and J.R. Stedinger. (1987). Plotting Positions for Historical Floods and Their Precision.
  \emph{Water Resources Research} \bold{23}(4), 715-727.

  Kaplan, E.L., and P. Meier. (1958). Nonparametric Estimation From Incomplete Observations.
  \emph{Journal of the American Statistical Association} \bold{53}, 457-481.

  Lee, E.T., and J. Wang. (2003). \emph{Statistical Methods for Survival Data Analysis,
  Third Edition}.  John Wiley and Sons, New York.

  Michael, J.R., and W.R. Schucany. (1986). Analysis of Data from Censored Samples.
  In D'Agostino, R.B., and M.A. Stephens, eds. \emph{Goodness-of Fit Techniques}.
  Marcel Dekker, New York, 560pp, Chapter 11, 461-496.

  Nelson, W. (1972). Theory and Applications of Hazard Plotting for Censored Failure Data.
  \emph{Technometrics} \bold{14}, 945-966.

  USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
  EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.
  U.S. Environmental Protection Agency, Washington, D.C. Chapter 15.

  USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
  EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
  U.S. Environmental Protection Agency, Washington, D.C.
}
\author{
    Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  For censored data sets, plotting positions may be used to construct empirical cumulative distribution
  plots (see \code{\link{ecdfPlotCensored}}), construct quantile-quantile plots
  (see \code{\link{qqPlotCensored}}), or to estimate distribution parameters
  (see \code{\link{FcnsByCatCensoredData}}).

  The function \code{\link[survival]{survfit}} in the built-in \R library
  \pkg{survival} computes the survival function for right-censored, left-censored, or
  interval-censored data.  Calling \code{\link[survival]{survfit}} with
  \code{type="kaplan-meier"} will produce similar results to calling
  \code{ppointsCensored} with \code{prob.method="kaplan-meier"}.  Also, calling
  \code{\link[survival]{survfit}} with \code{type="fh2"} will produce similar results
  to calling \code{ppointsCensored} with \code{prob.method="nelson"}.

  Helsel and Cohn (1988, p.2001) found very little effect of changing the value of the plotting position
  constant when using the method of Hirsch and Stedinger (1987) to compute plotting positions for
  multiply left-censored data.  In general, there will be very little difference between plotting positions
  computed by the different methods except in the case of very small samples and a large amount of
  censoring.
}
\seealso{
  \code{\link{ppoints}}, \code{\link{ecdfPlot}}, \code{\link{qqPlot}},
  \code{\link{ecdfPlotCensored}}, \code{\link{qqPlotCensored}},
  \code{\link[survival]{survfit}}.
}
\examples{
  # Generate 20 observations from a normal distribution with mean=20 and sd=5,
  # censor all observations less than 18, then compute plotting positions for
  # this data set.  Compare the plotting positions to the plotting positions
  # for the uncensored data set.  Note that the plotting positions for the
  # censored data set start at the first ordered uncensored observation and
  # that for values of x > 18 the plotting positions for the two data sets are
  # exactly the same. This is because there is only one censoring level and
  # no uncensored observations fall below the censored observations.
  # (Note: the call to set.seed simply allows you to reproduce this example.)

  set.seed(333)
  x <- rnorm(20, mean=20, sd=5)
  censored <- x < 18
  censored
  # [1] FALSE FALSE  TRUE FALSE  TRUE FALSE FALSE FALSE FALSE  TRUE  TRUE  TRUE
  #[13] FALSE  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE

  sum(censored)
  #[1] 7

  new.x <- x
  new.x[censored] <- 18
  round(sort(new.x),1)
  # [1] 18.0 18.0 18.0 18.0 18.0 18.0 18.0 18.1 18.7 19.6 20.2 20.3 20.6 21.4
  #[15] 21.8 21.8 23.2 26.2 26.8 29.7

  p.list <- ppointsCensored(new.x, censored)
  p.list
  #$Order.Statistics
  # [1] 18.00000 18.00000 18.00000 18.00000 18.00000 18.00000 18.00000 18.09771
  # [9] 18.65418 19.58594 20.21931 20.26851 20.55296 21.38869 21.76359 21.82364
  #[17] 23.16804 26.16527 26.84336 29.67340
  #
  #$Cumulative.Probabilities
  # [1] 0.3765432 0.3765432 0.3765432 0.3765432 0.3765432 0.3765432 0.3765432
  # [8] 0.3765432 0.4259259 0.4753086 0.5246914 0.5740741 0.6234568 0.6728395
  #[15] 0.7222222 0.7716049 0.8209877 0.8703704 0.9197531 0.9691358
  #
  #$Censored
  # [1]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE
  #[13] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
  #
  #$Censoring.Side
  #[1] "left"
  #
  #$Prob.Method
  #[1] "michael-schucany"
  #
  #$Plot.Pos.Con
  #[1] 0.375

  #----------

  # Round off plotting positions to two decimal places
  # and compare to plotting positions that ignore censoring
  #--------------------------------------------------------

  round(p.list$Cum, 2)
  # [1] 0.38 0.38 0.38 0.38 0.38 0.38 0.38 0.38 0.43 0.48 0.52 0.57 0.62 0.67
  #[15] 0.72 0.77 0.82 0.87 0.92 0.97

  round(ppoints(x, a=0.375), 2)
  # [1] 0.03 0.08 0.13 0.18 0.23 0.28 0.33 0.38 0.43 0.48 0.52 0.57 0.62 0.67
  #[15] 0.72 0.77 0.82 0.87 0.92 0.97

  #----------

  # Clean up
  #---------
  rm(x, censored, new.x, p.list)

  #----------------------------------------------------------------------------

  # Reproduce the example in Appendix B of Helsel and Cohn (1988).  The data
  # are stored in Helsel.Cohn.88.appb.df.  This data frame contains 18
  # observations, of which 9 are censored below one of 2 distinct censoring
  # levels.

  Helsel.Cohn.88.app.b.df
  #   Conc.orig Conc Censored
  #1         <1    1     TRUE
  #2         <1    1     TRUE
  #...
  #17        33   33    FALSE
  #18        50   50    FALSE

  p.list <- with(Helsel.Cohn.88.app.b.df,
    ppointsCensored(Conc, Censored, prob.method="hirsch-stedinger", plot.pos.con=0))
  lapply(p.list[1:2], round, 3)
  #$Order.Statistics
  # [1]  1  1  1  1  1  1  3  7  9 10 10 10 12 15 20 27 33 50
  #
  #$Cumulative.Probabilities
  # [1] 0.063 0.127 0.190 0.254 0.317 0.381 0.500 0.556 0.611 0.167 0.333 0.500
  #[13] 0.714 0.762 0.810 0.857 0.905 0.952

  # Clean up
  #---------
  rm(p.list)

  #----------------------------------------------------------------------------

  # Example 15-1 of USEPA (2009, page 15-10) gives an example of
  # computing plotting positions based on censored manganese
  # concentrations (ppb) in groundwater collected at 5 monitoring
  # wells.  The data for this example are stored in
  # EPA.09.Ex.15.1.manganese.df.

  EPA.09.Ex.15.1.manganese.df
  #   Sample   Well Manganese.Orig.ppb Manganese.ppb Censored
  #1       1 Well.1                 <5           5.0     TRUE
  #2       2 Well.1               12.1          12.1    FALSE
  #3       3 Well.1               16.9          16.9    FALSE
  #4       4 Well.1               21.6          21.6    FALSE
  #5       5 Well.1                 <2           2.0     TRUE
  #...
  #21      1 Well.5               17.9          17.9    FALSE
  #22      2 Well.5               22.7          22.7    FALSE
  #23      3 Well.5                3.3           3.3    FALSE
  #24      4 Well.5                8.4           8.4    FALSE
  #25      5 Well.5                 <2           2.0     TRUE

  p.list.EPA <- with(EPA.09.Ex.15.1.manganese.df,
    ppointsCensored(Manganese.ppb, Censored,
      prob.method = "kaplan-meier"))
  data.frame(Mn = p.list.EPA$Order.Statistics, Censored = p.list.EPA$Censored,
    CDF = p.list.EPA$Cumulative.Probabilities)
  #      Mn Censored  CDF
  #1    2.0     TRUE 0.21
  #2    2.0     TRUE 0.21
  #3    2.0     TRUE 0.21
  #4    3.3    FALSE 0.28
  #5    5.0     TRUE 0.28
  #6    5.0     TRUE 0.28
  #7    5.0     TRUE 0.28
  #8    5.3    FALSE 0.32
  #9    6.3    FALSE 0.36
  #10   7.7    FALSE 0.40
  #11   8.4    FALSE 0.44
  #12   9.5    FALSE 0.48
  #13  10.0    FALSE 0.52
  #14  11.9    FALSE 0.56
  #15  12.1    FALSE 0.60
  #16  12.6    FALSE 0.64
  #17  16.9    FALSE 0.68
  #18  17.9    FALSE 0.72
  #19  21.6    FALSE 0.76
  #20  22.7    FALSE 0.80
  #21  34.5    FALSE 0.84
  #22  45.9    FALSE 0.88
  #23  53.6    FALSE 0.92
  #24  77.2    FALSE 0.96
  #25 106.3    FALSE 1.00

  #----------

  # Clean up
  #---------
  rm(p.list.EPA)
}
\keyword{distribution}
\keyword{dplot}
