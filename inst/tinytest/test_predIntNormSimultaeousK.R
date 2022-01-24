library(EnvStats)

# predIntNormSimultaneousK test"
  x <- predIntNormSimultaneousK(n = 8, k = 1, m = 3)
  expect_true(round(x,2)==.51)


# predIntNormSimultaneousK CA test"
  x <- predIntNormSimultaneousK(n = 8, m = 3, rule = "CA")
  expect_true(round(x,2)==1.25)


# predIntNormSimultaneousK Modified.CA test"
  x <- predIntNormSimultaneousK(n = 8, rule = "Modified.CA")
  expect_true(round(x,2)==0.84)


# predIntNormSimultaneousK test 2"
  x <- predIntNormSimultaneousK(n = 8, k = 1, m = 3, r = 10)
  expect_true(round(x,2)==1.36)


# predIntNormSimultaneousK test 3"
  x <- predIntNormSimultaneousK(n = 8, k = 1, m = 3, r = 10)
  expect_true(round(x,2)==1.36)

nc <- 10
nw <- 50
SWFPR <- 0.1
conf.level <- (1 - SWFPR)^(1 / (nc * nw))

x <-- predIntNormSimultaneousK(n = 25, k = 1, m = 3, r = 2,
                         rule = "k.of.m", pi.type = "upper", conf.level = conf.level)
expect_true(round(x,2)==-2.01)

