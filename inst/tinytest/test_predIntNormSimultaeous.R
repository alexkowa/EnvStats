library(EnvStats)

set.seed(479)
dat <- rnorm(8, mean = 10, sd = 2)

# predIntNormSimultaneous test"
  x <- predIntNormSimultaneous(dat, k = 1, m = 3)
  expect_true(all(x$parameters-c(10.269773,2.210246)<1e-6))


# enorm test"
  x <- enorm(dat)
  expect_true(all(x$parameters-c(10.269773,2.210246)<1e-6))

# predIntNormSimultaneous test 1"
  x <- predIntNormSimultaneous(enorm(dat), k = 1, m = 3)
  expect_true(all(round(x$interval$limits,2)==c(-Inf,11.4)))


# predIntNormSimultaneous test CA"
  x <- predIntNormSimultaneous(dat, m = 3, rule = "CA")$interval$limits["UPL"]

  expect_true(round(x,2)==13.04)


# predIntNormSimultaneous test Modified.CA"
  x <- predIntNormSimultaneous(dat, rule = "Modified.CA")$interval$limits["UPL"]

  expect_true(round(x,2)==12.12)


# predIntNormSimultaneous test 2"
  x <- predIntNormSimultaneous(dat, k = 1, m = 3)$interval$limits["UPL"]
  expect_true(round(x,2)==11.4)


# predIntNormSimultaneous test 3"
  x <- predIntNormSimultaneous(dat, k = 1, m = 3, r = 10)$interval$limits["UPL"]
  expect_true(round(x,2)==13.28)


# predIntNormSimultaneous test 4"
  nc <- 10
  nw <- 50
  SWFPR <- 0.1
  conf.level <- (1 - SWFPR)^(1 / (nc * nw))
  log.Sulfate <- EPA.09.Ex.19.1.sulfate.df$log.Sulfate.mg.per.l
  pred.int.list.log <-
    predIntNormSimultaneous(x = log.Sulfate, k = 1, m = 3, r = 2,
                            rule = "k.of.m", pi.type = "upper", conf.level = conf.level)
  expect_true(round(exp(pred.int.list.log$interval$limits["UPL"]),2)==159.55)

