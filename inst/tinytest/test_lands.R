library(EnvStats)

# Land's method for the lognormal mean: lands.C, used by ci.land, ci.lnorm.land
# and elnormAlt(ci.method = "land"), computes Land's C in compiled code
# (src/land.cpp).  Reference values are Land's C recomputed independently from
# Land (1971) section 5 (Python/SciPy); the n = 101-251 cells also round to the
# published values 3.295 4.569 3.148 4.334 3.113 4.278.

lands.C <- EnvStats:::lands.C

# Errors become NA so that a broken build reports failures rather than
# aborting the test file.
safe <- function(expr) tryCatch(expr, error = function(e) NA_real_)

## Moderate n: EnvStats <= 3.1.0 was silently wrong by up to 15% at nu = 200
ref <- data.frame(
  S  = c(2, 3, 2, 3, 2, 3),
  nu = c(100, 100, 200, 200, 250, 250),
  C  = c(3.295228, 4.568771, 3.147987, 4.334330, 3.112883, 4.278391)
)
got <- mapply(function(S, nu) safe(lands.C(S, nu, 0.95)), ref$S, ref$nu)
expect_equal(got, ref$C, tolerance = 1e-6)

## Large n: EnvStats <= 3.1.0 failed with "non-finite function value" or
## "not of opposite sign"
expect_equal(safe(lands.C(S = 0.5, nu = 499, conf.level = 0.95)), 1.7793591, tolerance = 1e-6)
expect_true(is.finite(safe(lands.C(S = 5, nu = 999, conf.level = 0.95))))
expect_true(is.finite(safe(lands.C(S = 9, nu = 250, conf.level = 0.90))))

## Very small n: EnvStats <= 3.1.0 could return C with the wrong sign
## (a UCL below the estimate)
expect_equal(safe(lands.C(S = 3, nu = 2, conf.level = 0.95)), 39.2272993, tolerance = 1e-7)
expect_equal(safe(lands.C(S = 10, nu = 4, conf.level = 0.995)), 183.2828, tolerance = 1e-6)

x <- exp(c(-3, 0, 3))
fit <- elnormAlt(x, ci = TRUE, ci.type = "upper", ci.method = "land")
expect_true(isTRUE(fit$interval$limits[["UCL"]] > fit$parameters[["mean"]]))
expect_equal(fit$interval$limits[["UCL"]], 1.240401e38, tolerance = 1e-6)

## Limiting case: as S -> 0, C -> the Student t quantile scaled by
## sqrt(nu / (nu + 1)); the gap is first order in S
for (nu in c(2, 9, 99))
  for (cf in c(0.05, 0.95))
    expect_equal(safe(lands.C(1e-6, nu, cf)), qt(cf, nu) * sqrt(nu / (nu + 1)),
                 tolerance = 1e-5)

## Symmetry of the lower and upper limits is NOT expected (the conditional
## density is skewed), but the lower C must be below the upper C
expect_true(safe(lands.C(2, 20, 0.05)) < safe(lands.C(2, 20, 0.95)))

## elnormAlt end to end at n = 500 (EnvStats <= 3.1.0 failed with an error)
set.seed(1)
y <- rlnorm(500, 2, 0.5)
u <- safe(elnormAlt(y, ci = TRUE, ci.type = "upper",
                    ci.method = "land")$interval$limits[["UCL"]])
expect_equal(u, 8.843541, tolerance = 1e-6)

## Two-sided interval: equal-tailed, from the 2.5% and 97.5% limits
set.seed(2)
z <- rlnorm(40, 0, 1.5)
two <- elnormAlt(z, ci = TRUE, ci.method = "land")$interval$limits
lo  <- elnormAlt(z, ci = TRUE, ci.type = "lower", conf.level = 0.975)$interval$limits[["LCL"]]
hi  <- elnormAlt(z, ci = TRUE, ci.type = "upper", conf.level = 0.975)$interval$limits[["UCL"]]
expect_equal(unname(two), c(lo, hi), tolerance = 1e-10)

## Argument checks
expect_error(elnormAlt(c(1, 2), ci = TRUE, ci.method = "land"))
expect_error(lands.C(S = 0, nu = 10, conf.level = 0.95))
expect_error(lands.C(S = 1, nu = 1, conf.level = 0.95))
expect_error(lands.C(S = 1, nu = 10, conf.level = 1))
