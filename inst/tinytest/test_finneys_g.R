library(EnvStats)

## Finney's g (MVUE of the lognormal mean; companion of Land's method in the
## default elnormAlt call).  Reference values are the series summed in
## extended precision (mpmath, precision sized to the cancellation) and
## cross-checked with mpmath.hyp0f1.  EnvStats <= 3.1.0 summed the alternating
## series (z < 0) in double precision: 6e-3 relative error at m = 20, z = -20,
## and the wrong sign at z = -50 and -100.
finneys.g <- EnvStats:::finneys.g
fg.ref <- data.frame(
  m = c(1, 3, 20, 20, 20, 99, 299, 999, 20, 20),
  z = c(-100, -2, -20, -50, -100, -5, -300, -50, 5, 100),
  g = c(-0.8390715290764524, 0.04704000268662241, -5.567006032078406e-08,
        -7.687872702118442e-09, -1.380564728900597e-09, 0.005349678614832345,
        2.850768459541573e-89, 1.111614603793767e-23, 58.01378060689389,
        2.399579890661098e+17))
expect_equal(mapply(finneys.g, fg.ref$m, fg.ref$z), fg.ref$g, tolerance = 1e-11)
expect_equal(finneys.g(1, -100), cos(10), tolerance = 1e-12)   # m = 1: g = cos(2 sqrt(-y))
expect_equal(finneys.g(10, 0), 1)
expect_equal(finneys.g(c(5, 20), c(-1, 2)), c(finneys.g(5, -1), finneys.g(20, 2)))
expect_error(finneys.g(0.5, 1))

