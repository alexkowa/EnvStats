library(EnvStats)

set.seed(1274)

values <- c(
  120, 103, 92, 85, 81, 76,
  72, 72, 72, 72,
  55, 47, 46, 46, 45, 44,
  39, 38, 36, 34, 34
)

is_non_detect <- c(
  FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
  TRUE, TRUE, TRUE, TRUE,
  TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
  TRUE, TRUE, TRUE, TRUE, TRUE
)

result <- tryCatch(
  elnormAltCensored(
    values,
    is_non_detect,
    method = "rROS",
    ci = TRUE,
    ci.type = "two-sided",
    ci.method = "bootstrap",
    n.bootstraps = 44,
    conf.level = 0.90
  ),
  error = identity
)

expect_false(inherits(result, "error"))
expect_true(result$interval$too.few.distinct.obs.count > 0)
expect_false(any(result$interval$limits == 0, na.rm = TRUE))
