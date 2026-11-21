// Copyright (c) 2026 Neptune and Company, Inc.  Author: John H. Carson Jr.
// Contributed to EnvStats under the GNU General Public License, version 3 or
// later (GPL (>= 3)), the licence of the package.
//
// land.cpp -- implementation.  See land.h.
//
// Numerical policy, forced by the range of the problem (H_ALGORITHM.md section 6):
// over ProUCL's own grid k spans [0, 499] and c spans [-2.9e4, 0), so exp(c*w)
// spans e^(+-29000).  Three rules follow and are applied everywhere below.
//
//   1. Work in theta = asin(w).  The integrand becomes cos(theta)^(2k+1) *
//      exp(c*sin theta), smooth with no endpoint singularity for any k > -1.
//   2. Evaluate the integrand relative to its own log-maximum.  Never form
//      exp(c*sin theta) on its own.
//   3. Truncate to the range where the normalised integrand is above underflow,
//      and integrate only there.  At k = 499 that range is ~0.05 radians out of
//      pi; without truncation an adaptive rule spends its whole budget on zeros,
//      and the quantile solve gets a 0/0 bracket.

#include "land.h"

#include <algorithm>
#include <cmath>
#include <limits>
#include <vector>

namespace land {
namespace {

constexpr double PI     = 3.14159265358979323846;
constexpr double HALFPI = 1.57079632679489661923;
const double     NaN    = std::numeric_limits<double>::quiet_NaN();

// Theta is kept this far inside +-pi/2.  The integrand there is O(EDGE^(2k+1))
// relative to the peak, so the excluded tail is below 1e-28 for every k >= 1/2
// and below 5e-29 in absolute mass at k = 0.
constexpr double EDGE = 1e-14;

// exp() underflow guard: below this the normalised integrand is exactly 0.
constexpr double LOG_UNDERFLOW = -745.0;
// Support truncation level.  Slightly inside LOG_UNDERFLOW so the bracket
// endpoints evaluate to strictly positive, non-denormal values.
constexpr double LOG_SUPPORT = -700.0;

long long g_evals = 0;

// -------------------------------------------------------------- quadrature
//
// 15-point Gauss-Kronrod with the embedded 7-point Gauss rule for the error
// estimate (QUADPACK's dqk15 nodes), driven by global adaptive bisection:
// repeatedly split the segment with the largest error estimate.  Global rather
// than recursive-local because the integrand here is a single narrow spike
// whose location is known but whose width varies by four orders of magnitude.

const double XGK[8] = {
    0.991455371120812639206854697526329, 0.949107912342758524526189684047851,
    0.864864423359769072789712788640926, 0.741531185599394439863864773280788,
    0.586087235467691130294144838258730, 0.405845151377397166906606412076961,
    0.207784955007898467600689403773245, 0.000000000000000000000000000000000};
const double WGK[8] = {
    0.022935322010529224963732008058970, 0.063092092629978553290700663189204,
    0.104790010322250183839876322541518, 0.140653259715525918745189590510238,
    0.169004726639267902826583426598550, 0.190350578064785409913256402421014,
    0.204432940075298892414161999234649, 0.209482141084727828012999174891714};
// 7-point Gauss weights, applying to XGK[1], XGK[3], XGK[5], XGK[7].
const double WG[4] = {
    0.129484966168869693270611432679082, 0.279705391489276667901467771423780,
    0.381830050505118944950369775488975, 0.417959183673469387755102040816327};

struct Seg {
    double a, b, val, err;
};

template <class F>
Seg gk15(const F& f, double a, double b) {
    const double c = 0.5 * (a + b), h = 0.5 * (b - a);
    double fc = f(c);
    double rk = WGK[7] * fc;
    double rg = WG[3] * fc;
    for (int i = 0; i < 7; ++i) {
        const double dx = h * XGK[i];
        const double fs = f(c - dx) + f(c + dx);
        rk += WGK[i] * fs;
        if (i & 1) rg += WG[i >> 1] * fs;
    }
    Seg s;
    s.a   = a;
    s.b   = b;
    s.val = rk * h;
    s.err = std::fabs((rk - rg) * h);
    return s;
}

// Integrate f over [a, b].  epsrel is relative to the running total.
template <class F>
double integrate(const F& f, double a, double b, double epsrel = 1e-13,
                 std::size_t max_seg = 500) {
    if (!(b > a)) return 0.0;
    std::vector<Seg> segs;
    segs.reserve(max_seg + 1);
    segs.push_back(gk15(f, a, b));

    while (segs.size() < max_seg) {
        double total = 0.0, err = 0.0;
        std::size_t worst = 0;
        for (std::size_t i = 0; i < segs.size(); ++i) {
            total += segs[i].val;
            err += segs[i].err;
            if (segs[i].err > segs[worst].err) worst = i;
        }
        if (err <= epsrel * std::fabs(total)) break;
        // A segment narrower than the double spacing of its own endpoints
        // cannot be split further; stop rather than spin.
        const Seg w = segs[worst];
        const double mid = 0.5 * (w.a + w.b);
        if (!(mid > w.a && mid < w.b)) break;
        segs[worst] = gk15(f, w.a, mid);
        segs.push_back(gk15(f, mid, w.b));
    }
    double total = 0.0;
    for (const Seg& s : segs) total += s.val;
    return total;
}

// ------------------------------------------------------------ root finding

// Brent's method.  Every call site below supplies a guaranteed sign-changing
// bracket, so this cannot fail to converge.
template <class F>
double brent(const F& f, double a, double b, double tol = 1e-15,
             int max_iter = 200) {
    double fa = f(a), fb = f(b);
    if (fa == 0.0) return a;
    if (fb == 0.0) return b;
    if (fa * fb > 0.0) return NaN;  // caller bug; never reached in this file
    if (std::fabs(fa) < std::fabs(fb)) {
        std::swap(a, b);
        std::swap(fa, fb);
    }
    double c = a, fc = fa, d = b - a, e = d;
    for (int it = 0; it < max_iter; ++it) {
        if (fb * fc > 0.0) {
            c  = a;
            fc = fa;
            d = e = b - a;
        }
        if (std::fabs(fc) < std::fabs(fb)) {
            a = b;  b = c;  c = a;
            fa = fb;  fb = fc;  fc = fa;
        }
        const double tol1 = 2.0 * 2.22e-16 * std::fabs(b) + 0.5 * tol;
        const double xm   = 0.5 * (c - b);
        if (std::fabs(xm) <= tol1 || fb == 0.0) return b;
        if (std::fabs(e) >= tol1 && std::fabs(fa) > std::fabs(fb)) {
            double p, q, r, s = fb / fa;
            if (a == c) {
                p = 2.0 * xm * s;
                q = 1.0 - s;
            } else {
                q = fa / fc;
                r = fb / fc;
                p = s * (2.0 * xm * q * (q - r) - (b - a) * (r - 1.0));
                q = (q - 1.0) * (r - 1.0) * (s - 1.0);
            }
            if (p > 0.0) q = -q;
            p = std::fabs(p);
            if (2.0 * p < std::min(3.0 * xm * q - std::fabs(tol1 * q),
                                   std::fabs(e * q))) {
                e = d;
                d = p / q;
            } else {
                d = xm;
                e = d;
            }
        } else {
            d = xm;
            e = d;
        }
        a  = b;
        fa = fb;
        b += (std::fabs(d) > tol1) ? d : std::copysign(tol1, xm);
        fb = f(b);
    }
    return b;
}

// -------------------------------------------------------- the Land kernel
//
// Encapsulates one (k, c) pair: peak location, truncated support, the mass
// integrals.  Constructing it is the expensive part, so the root solves that
// need many quantiles of the same kernel build it once.

struct Kernel {
    double k, c;
    double theta_peak;
    double log_max;
    double lo, hi;      // truncated support
    double mass0;       // integral of the normalised integrand
    double mass1;       // integral of sin(theta) * normalised integrand
    double mass_left;   // mass0 restricted to [lo, theta_peak]
    bool   mass1_ready;

    explicit Kernel(double nu, double xi)
        : k(nu / 2.0 - 1.0), c((nu + 1.0) * xi), mass1(0.0), mass1_ready(false) {
        theta_peak = peak();
        log_max    = energy(theta_peak);
        support();
        mass_left = integrate([this](double t) { return (*this)(t); }, lo, theta_peak);
        mass0 = mass_left +
                integrate([this](double t) { return (*this)(t); }, theta_peak, hi);
    }

    // log of the unnormalised integrand: (2k+1) log cos(theta) + c sin(theta).
    double energy(double th) const {
        const double ct = std::cos(th);
        if (!(ct > 0.0)) return -std::numeric_limits<double>::infinity();
        return (2.0 * k + 1.0) * std::log(ct) + c * std::sin(th);
    }

    // The integrand, normalised so that its peak value is exactly 1.
    double operator()(double th) const {
        ++g_evals;
        const double e = energy(th) - log_max;
        return (e > LOG_UNDERFLOW) ? std::exp(e) : 0.0;
    }

    // Maximiser of `energy`.  Solves w^2 + (2k/c) w - 1 = 0 for the root in
    // (-1, 1).  The two roots multiply to -1, so exactly one is interior and
    // which one depends on sign(c) -- and for lambda = 1/2, c < 0 always.
    // Written in the cancellation-free form; the textbook (-a + sqrt(a^2+4))/2
    // loses most of its digits once |2k/c| is large, which is the entire
    // large-nu regime.
    double peak() const {
        if (c == 0.0) return 0.0;
        if (k == 0.0) return std::copysign(HALFPI - EDGE, c);
        const double a = 2.0 * k / c;
        const double q = -(a + std::copysign(std::sqrt(a * a + 4.0), a)) / 2.0;
        double w = -1.0 / q;  // the interior root
        w = std::max(-1.0 + 1e-15, std::min(1.0 - 1e-15, w));
        return std::asin(w);
    }

    // Narrow [lo, hi] to where the normalised integrand exceeds e^LOG_SUPPORT.
    // `energy` is monotone on each side of the peak, so one Brent call each.
    void support() {
        const double g_lo_end = -HALFPI + EDGE, g_hi_end = HALFPI - EDGE;
        auto g = [this](double th) { return energy(th) - log_max - LOG_SUPPORT; };
        lo = g_lo_end;
        hi = g_hi_end;
        if (g(g_lo_end) < 0.0) lo = brent(g, g_lo_end, theta_peak, 1e-16);
        if (g(g_hi_end) < 0.0) hi = brent(g, theta_peak, g_hi_end, 1e-16);
    }

    // P(Theta <= th), with the split at the peak so neither piece straddles it.
    double cdf(double th) const {
        if (th <= lo) return 0.0;
        if (th >= hi) return 1.0;
        auto f = [this](double t) { return (*this)(t); };
        const double num = (th <= theta_peak)
                               ? integrate(f, lo, th)
                               : mass_left + integrate(f, theta_peak, th);
        return num / mass0;
    }

    double quantile(double alpha) const {
        auto g = [this, alpha](double th) { return cdf(th) - alpha; };
        return brent(g, lo, hi, 1e-15);
    }

    // First moment of the normalised integrand, for the unbiasedness condition
    // (7.2).  Computed lazily: only the two-sided path needs it.
    double first_moment() {
        if (!mass1_ready) {
            auto f = [this](double t) { return std::sin(t) * (*this)(t); };
            mass1 = integrate(f, lo, theta_peak) + integrate(f, theta_peak, hi);
            mass1_ready = true;
        }
        return mass1;
    }

    // Integrals over a sub-range, splitting at the peak when it is interior.
    double mass0_over(double a, double b) const {
        auto f = [this](double t) { return (*this)(t); };
        if (a < theta_peak && theta_peak < b)
            return integrate(f, a, theta_peak) + integrate(f, theta_peak, b);
        return integrate(f, a, b);
    }
    double mass1_over(double a, double b) const {
        auto f = [this](double t) { return std::sin(t) * (*this)(t); };
        if (a < theta_peak && theta_peak < b)
            return integrate(f, a, theta_peak) + integrate(f, theta_peak, b);
        return integrate(f, a, b);
    }
};

inline double t_from_theta(double nu, double th) {
    return std::sqrt(nu) * std::tan(th);
}

// ------------------------------------------- incomplete beta / Student t

double betacf(double a, double b, double x) {
    const double tiny = 1e-300;
    const double qab = a + b, qap = a + 1.0, qam = a - 1.0;
    double c = 1.0, d = 1.0 - qab * x / qap;
    if (std::fabs(d) < tiny) d = tiny;
    d = 1.0 / d;
    double h = d;
    for (int m = 1; m <= 300; ++m) {
        const int m2 = 2 * m;
        double aa = m * (b - m) * x / ((qam + m2) * (a + m2));
        d = 1.0 + aa * d;
        if (std::fabs(d) < tiny) d = tiny;
        c = 1.0 + aa / c;
        if (std::fabs(c) < tiny) c = tiny;
        d = 1.0 / d;
        h *= d * c;
        aa = -(a + m) * (qab + m) * x / ((a + m2) * (qap + m2));
        d = 1.0 + aa * d;
        if (std::fabs(d) < tiny) d = tiny;
        c = 1.0 + aa / c;
        if (std::fabs(c) < tiny) c = tiny;
        d = 1.0 / d;
        const double del = d * c;
        h *= del;
        if (std::fabs(del - 1.0) < 3e-16) break;
    }
    return h;
}

// Regularised incomplete beta I_x(a, b).
double betai(double a, double b, double x) {
    if (x <= 0.0) return 0.0;
    if (x >= 1.0) return 1.0;
    const double bt = std::exp(std::lgamma(a + b) - std::lgamma(a) - std::lgamma(b) +
                               a * std::log(x) + b * std::log1p(-x));
    if (x < (a + 1.0) / (a + b + 2.0)) return bt * betacf(a, b, x) / a;
    return 1.0 - bt * betacf(b, a, 1.0 - x) / b;
}

}  // namespace

// ================================================================= public

long long eval_count() { return g_evals; }
void reset_eval_count() { g_evals = 0; }

double students_t_cdf(double nu, double t) {
    const double x = nu / (nu + t * t);
    const double p = 0.5 * betai(0.5 * nu, 0.5, x);
    return (t <= 0.0) ? p : 1.0 - p;
}

double students_t_quantile(double nu, double p) {
    if (!(p > 0.0 && p < 1.0)) return NaN;
    auto g = [nu, p](double t) { return students_t_cdf(nu, t) - p; };
    double b = 1.0;
    while (g(-b) > 0.0 || g(b) < 0.0) {
        b *= 4.0;
        if (b > 1e300) return NaN;
    }
    return brent(g, -b, b, 1e-14);
}

double t_land(double nu, double xi, double alpha) {
    if (!(nu >= 2.0)) return NaN;  // section 1.7: not an interval below nu = 2
    if (!(alpha > 0.0 && alpha < 1.0)) return NaN;
    const Kernel K(nu, xi);
    return t_from_theta(nu, K.quantile(alpha));
}

TwoSidedT t_equal_tailed(double nu, double xi, double alpha) {
    TwoSidedT r;
    r.t1 = t_land(nu, xi, 0.5 * alpha);
    r.t2 = t_land(nu, xi, 1.0 - 0.5 * alpha);
    return r;
}

TwoSidedT t_land_two_sided(double nu, double xi, double alpha) {
    TwoSidedT r{NaN, NaN};
    if (!(nu >= 2.0)) return r;
    if (!(alpha > 0.0 && alpha < 1.0)) return r;

    // At xi = 0 the density is symmetric, the unbiasedness condition (7.2)
    // holds identically (both sides are 0) and the Newton system is singular.
    // The unbiased interval is then the equal-tailed one.  Land, p. 1202.
    if (xi == 0.0) {
        r.t2 = students_t_quantile(nu, 1.0 - 0.5 * alpha);
        r.t1 = -r.t2;
        return r;
    }

    Kernel K(nu, xi);
    const double T0 = K.mass0, T1 = K.first_moment();
    const double target0 = (1.0 - alpha) * T0, target1 = (1.0 - alpha) * T1;

    // Start from the equal-tailed pair and Newton onto (7.1)-(7.2).  The
    // Jacobian is [[-f(th1), f(th2)], [-sin(th1)f(th1), sin(th2)f(th2)]] with
    // determinant f(th1)f(th2)(sin th1 - sin th2), nonzero whenever th1 < th2
    // and both are inside the support -- which the clamping below enforces.
    double th1 = K.quantile(0.5 * alpha);
    double th2 = K.quantile(1.0 - 0.5 * alpha);

    for (int it = 0; it < 60; ++it) {
        const double F1 = K.mass0_over(th1, th2) - target0;
        const double F2 = K.mass1_over(th1, th2) - target1;
        if (std::fabs(F1) <= 1e-14 * T0 &&
            std::fabs(F2) <= 1e-14 * std::max(std::fabs(T1), T0))
            break;

        const double f1 = K(th1), f2 = K(th2);
        const double s1 = std::sin(th1), s2 = std::sin(th2);
        const double det = f1 * f2 * (s1 - s2);
        if (!(std::fabs(det) > 0.0)) break;
        // Solve J * d = -F.
        double d1 = (-F1 * s2 * f2 + F2 * f2) / det;
        double d2 = (-F1 * s1 * f1 + F2 * f1) / det;

        // Damp so the pair stays ordered and inside the support; a full Newton
        // step from the equal-tailed start can otherwise cross th1 past th2 at
        // large |c|, where the density is extremely asymmetric.
        double s = 1.0;
        for (int b = 0; b < 60; ++b) {
            const double n1 = th1 + s * d1, n2 = th2 + s * d2;
            if (n1 > K.lo && n2 < K.hi && n2 - n1 > 1e-13) break;
            s *= 0.5;
        }
        th1 += s * d1;
        th2 += s * d2;
        th1 = std::max(th1, K.lo);
        th2 = std::min(th2, K.hi);
    }
    r.t1 = t_from_theta(nu, th1);
    r.t2 = t_from_theta(nu, th2);
    return r;
}

namespace {

// The implicit solve of H_ALGORITHM.md section 2.2.  `crit` maps (nu, xi) to
// the critical value in use -- the one-sided t for H(), or t1/t2 for the
// two-sided endpoints.  Everything else is identical.
//
//   p  = z * t / sqrt(nu + t^2)            (5.1)
//   z  = sqrt((n-1) sy^2 / n + p^2)        (4.2)
//   xi = -z/2                              lambda = 1/2, so xi < 0 always
//
// phi(p) = p - p(z(p)) is strictly increasing (section 1.5 guarantees the
// confidence set is an interval, which is the same statement), so outward
// bracket expansion from 0 followed by Brent cannot fail.
template <class Crit>
double solve_p(double n, double sy, const Crit& crit) {
    const double nu = n - 1.0;
    const double A  = nu * sy * sy / n;
    auto phi = [&](double p) {
        const double z = std::sqrt(A + p * p);
        const double t = crit(nu, -0.5 * z);
        return p - z * t / std::sqrt(nu + t * t);
    };
    const double step = std::max(1.0, sy * sy + 4.0 * sy);
    double lo = 0.0, hi = 0.0;
    for (int i = 0; i < 200 && phi(lo) > 0.0; ++i) lo -= step;
    for (int i = 0; i < 200 && phi(hi) < 0.0; ++i) hi += step;
    return brent(phi, lo, hi, 1e-14);
}

inline double H_from_p(double n, double sy, double p) {
    return std::sqrt(n - 1.0) * (-p - 0.5 * sy * sy) / sy;
}

bool bad_input(double n, double sy) {
    return !(n >= 3.0) || !(sy > 0.0);
}

}  // namespace

double H(double n, double sy, double alpha) {
    if (bad_input(n, sy)) return NaN;
    if (!(alpha > 0.0 && alpha < 1.0)) return NaN;
    const double p = solve_p(n, sy, [alpha](double nu, double xi) {
        return t_land(nu, xi, alpha);
    });
    return H_from_p(n, sy, p);
}

double land_limit(double n, double ybar, double sy, double alpha) {
    if (bad_input(n, sy)) return NaN;
    const double p = solve_p(n, sy, [alpha](double nu, double xi) {
        return t_land(nu, xi, alpha);
    });
    return ybar - p;
}

double lognormal_ucl(double n, double ybar, double sy, double conf) {
    return std::exp(land_limit(n, ybar, sy, 1.0 - conf));
}

double lognormal_lcl(double n, double ybar, double sy, double conf) {
    return std::exp(land_limit(n, ybar, sy, conf));
}

HPair H_two_sided_umpu(double n, double sy, double conf) {
    HPair r{NaN, NaN};
    if (bad_input(n, sy)) return r;
    if (!(conf > 0.0 && conf < 1.0)) return r;
    const double alpha = 1.0 - conf;

    // T_m = sqrt(n)(ybar - m)/sy decreases as m increases, so the acceptance
    // boundary T_m = t2 (the larger critical value) is the *lower* endpoint of
    // the interval for m, and T_m = t1 the upper.  Each endpoint is its own
    // implicit solve because xi depends on m.
    const double p_lo = solve_p(n, sy, [alpha](double nu, double xi) {
        return t_land_two_sided(nu, xi, alpha).t2;
    });
    const double p_hi = solve_p(n, sy, [alpha](double nu, double xi) {
        return t_land_two_sided(nu, xi, alpha).t1;
    });
    r.lower = H_from_p(n, sy, p_lo);
    r.upper = H_from_p(n, sy, p_hi);
    return r;
}

HPair H_two_sided_equal_tailed(double n, double sy, double conf) {
    HPair r{NaN, NaN};
    if (bad_input(n, sy)) return r;
    if (!(conf > 0.0 && conf < 1.0)) return r;
    const double half = 0.5 * (1.0 - conf);
    r.lower = H(n, sy, 1.0 - half);  // large tail index -> negative H
    r.upper = H(n, sy, half);
    return r;
}

namespace {
Limits interval_from(double n, double ybar, double sy, HPair h) {
    Limits L;
    const double base = ybar + 0.5 * sy * sy, scale = sy / std::sqrt(n - 1.0);
    L.lower = std::exp(base + scale * h.lower);
    L.upper = std::exp(base + scale * h.upper);
    return L;
}
}  // namespace

Limits lognormal_interval_umpu(double n, double ybar, double sy, double conf) {
    return interval_from(n, ybar, sy, H_two_sided_umpu(n, sy, conf));
}

Limits lognormal_interval_equal_tailed(double n, double ybar, double sy, double conf) {
    return interval_from(n, ybar, sy, H_two_sided_equal_tailed(n, sy, conf));
}

// nu = 2 means k = 0, the density is exp(beta*u) and (5.5) integrates:
//     exp(beta*p) = alpha*exp(beta*z) + (1-alpha)*exp(-beta*z)
// Evaluated with logaddexp.  Written literally it overflows for z >~ 500 and
// the grid reaches z ~ 9975; the resulting inf makes phi return NaN and the
// bracket-expansion loop below would spin, because every comparison against
// NaN is false.
double H_nu2_exact(double sy, double alpha) {
    if (!(sy > 0.0) || !(alpha > 0.0 && alpha < 1.0)) return NaN;
    const double beta = -1.5;  // -(nu+1)*lambda with nu = 2, lambda = 1/2
    const double A    = 2.0 * sy * sy / 3.0;
    auto p_of_z = [&](double z) {
        const double a = std::log(alpha) + beta * z;
        const double b = std::log1p(-alpha) - beta * z;
        const double M = std::max(a, b);
        return (M + std::log(std::exp(a - M) + std::exp(b - M))) / beta;
    };
    auto phi = [&](double p) { return p - p_of_z(std::sqrt(A + p * p)); };
    const double step = std::max(1.0, sy * sy + 4.0 * sy);
    double lo = 0.0, hi = 0.0;
    for (int i = 0; i < 200 && phi(lo) > 0.0; ++i) lo -= step;
    for (int i = 0; i < 200 && phi(hi) < 0.0; ++i) hi += step;
    const double p = brent(phi, lo, hi, 1e-15);
    return std::sqrt(2.0) * (-p - 0.5 * sy * sy) / sy;
}

}  // namespace land
