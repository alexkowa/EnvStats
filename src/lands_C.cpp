// Copyright (c) 2026 Neptune and Company, Inc.  Author: John H. Carson Jr.
// Contributed to EnvStats under the GNU General Public License, version 3 or
// later (GPL (>= 3)), the licence of the package.
//
// lands_C.cpp -- Rcpp entry point for Land's C, used by lands.C() and hence by
// ci.land(), ci.lnorm.land() and elnormAlt(ci.method = "land").
//
// Computes Land's C (the H statistic) directly from Land (1971) with the
// table-free solver in land.cpp (C++17 standard library only; no R or Rcpp
// dependency).  Neptune and Company also maintains land.cpp and land.h in its
// landh package; changes should be made there first and then copied here.

#include <Rcpp.h>

#include "land.h"

// lands_C_cpp(S, nu, conf.level) is Land's C: the limit for beta = mu +
// lambda*sigma^2 is  beta.hat + (k*S/sqrt(nu)) * C  (see ci.land).  conf.level
// is the lower-tail probability of the limit, so 0.95 gives the 95% upper limit
// and 0.05 the 95% lower limit.  land::H(n, sy, alpha) takes Land's tail index
// alpha = 1 - conf.level and n = nu + 1.
//
// Returns NaN on failure; lands.C() turns that into an error.

// [[Rcpp::export]]
double lands_C_cpp(double S, double nu, double conf_level) {
    return land::H(nu + 1.0, S, 1.0 - conf_level);
}
