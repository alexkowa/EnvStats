// land.h -- Land (1971) confidence limits for mu + lambda*sigma^2, computed
// from first principles.  No lookup tables, no interpolation, no dependencies.
//
// Reference: C. E. Land, "Confidence Intervals for Linear Functions of the
// Normal Mean and Variance", Ann. Math. Statist. 42(4) (1971) 1187-1205.
// Specification and derivation: reports/H_ALGORITHM.md (from the project root).
// Equation numbers below are Land's, as re-derived in that document.
//
// The whole computation reduces to the quantiles of a one-dimensional density
//
//     rho(w)  proportional to  (1 - w^2)^k * exp(c*w)      on (-1, 1)
//     k = nu/2 - 1,   c = (nu + 1)*xi
//
// with t(nu, xi, alpha) = sqrt(nu)*q/sqrt(1 - q^2) at the alpha-quantile q.
// Everything else is root-finding on top of that.

#ifndef LAND_H_INCLUDED
#define LAND_H_INCLUDED

#include <cstddef>

namespace land {

// ---------------------------------------------------------------- results

struct TwoSidedT {
    double t1;  // lower critical value
    double t2;  // upper critical value
};

struct HPair {
    double lower;  // H for the lower confidence limit (negative)
    double upper;  // H for the upper confidence limit (positive)
};

struct Limits {
    double lower;  // confidence limit for mu + sigma^2/2, log scale
    double upper;
};

// ------------------------------------------------- Land critical values

// One-sided critical value t(nu, xi, alpha) -- Land's Table 1.
// alpha is Land's *tail index* (a lower-tail probability), not a confidence
// level.  Requires nu >= 2; returns NaN below that (section 6 of the paper
// proves the confidence set need not be an interval at nu = 1).
double t_land(double nu, double xi, double alpha);

// Two-sided UMPU pair (t1, t2) solving Land (7.1)-(7.2) -- his Table 2.
// alpha is the *total* tail probability, so the interval has coverage 1-alpha.
// This is the unbiased interval, not the equal-tailed one; the two differ
// because the conditional density is skewed (see H_ALGORITHM.md section 1.6).
TwoSidedT t_land_two_sided(double nu, double xi, double alpha);

// Equal-tailed pair, for comparison: (t(nu,xi,alpha/2), t(nu,xi,1-alpha/2)).
TwoSidedT t_equal_tailed(double nu, double xi, double alpha);

// ------------------------------------------------------- the H statistic

// ProUCL's H, defined by  m_limit = ybar + sy^2/2 + sy*H/sqrt(n-1).
// alpha is Land's tail index; alpha = 1 - ConfCoef.  So H(n, sy, 0.05) is the
// entry ProUCL stores in its H95 table at (sd = sy, df = n-1).
double H(double n, double sy, double alpha);

// Land's one-sided confidence limit for mu + sigma^2/2 on the log scale.
double land_limit(double n, double ybar, double sy, double alpha);

// One-sided UCL for the lognormal mean at confidence `conf`.
double lognormal_ucl(double n, double ybar, double sy, double conf);
double lognormal_lcl(double n, double ybar, double sy, double conf);

// Two-sided H pair at two-sided confidence `conf`, as Land's UMPU interval.
// Both endpoints come from the same solve family as H(), but with t1/t2 in
// place of the one-sided critical value.
HPair H_two_sided_umpu(double n, double sy, double conf);

// Two-sided H pair the way ProUCL builds it: two one-sided limits at
// (1-conf)/2 and 1-(1-conf)/2.  Correct coverage, wider than UMPU.
HPair H_two_sided_equal_tailed(double n, double sy, double conf);

// Two-sided interval for the lognormal mean, either construction.
Limits lognormal_interval_umpu(double n, double ybar, double sy, double conf);
Limits lognormal_interval_equal_tailed(double n, double ybar, double sy, double conf);

// ------------------------------------------------------------- utilities

// Student-t quantile, computed from the regularised incomplete beta function.
// Independent of the Land kernel, so it can be used to check it.
double students_t_quantile(double nu, double p);
double students_t_cdf(double nu, double t);

// Exact nu = 2 closed form for H (k = 0, the density is a pure exponential and
// (5.5) integrates).  An analytic oracle for the quadrature path.
double H_nu2_exact(double sy, double alpha);

// Number of kernel quadrature evaluations since the last reset -- for cost
// reporting only.
long long eval_count();
void reset_eval_count();

}  // namespace land

#endif  // LAND_H_INCLUDED
