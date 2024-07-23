library(EnvStats)

set.seed(479)
x <- c(4, 4.2, 0.61606, 5.27628, 3, 0.82952, 4, 4, 4, 4, 4, 0.5, 2,
       3.56, 4, 4, 4, 5.1, 1.25, 4, 4)
tf <- c(TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE,
        TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE,
        TRUE)

ci <- enparCensored(x, tf,
              ci=TRUE, ci.method="bootstrap", ci.type="upper",
              n.bootstraps=5)
tinytest::expect_true(all(round(ci$interval$limits,2)==
                             round(c(0, 3.3973486, 0, 2.87404, 0, 2.488456),2)))
