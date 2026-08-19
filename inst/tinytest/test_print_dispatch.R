library(EnvStats)

htest.obj <- chenTTest(EPA.02d.Ex.9.mg.per.L.vec, mu = 30)
htest.output <- capture.output(print(htest.obj))
expect_true(
  any(grepl("Results of Hypothesis Test", htest.output, fixed = TRUE))
)

if (requireNamespace("boot", quietly = TRUE)) {
  set.seed(131)
  boot.obj <- boot::boot(
    data = 1:5,
    statistic = function(data, indices) mean(data[indices]),
    R = 5
  )

  print.output <- capture.output(print(boot.obj))
  expect_true(
    any(grepl("ORDINARY NONPARAMETRIC BOOTSTRAP", print.output, fixed = TRUE))
  )
}
