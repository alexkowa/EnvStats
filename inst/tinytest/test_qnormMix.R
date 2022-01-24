library(EnvStats)


#qnormMix 1"
  q <- qnormMix(.95,mean1=-1,sd1=1, mean2=4,sd2=1, p.mix=.2)
  expect_identical(round(q,2),4.67)


#qnormMix 2"
  q <- qnormMix(.95,mean1=-1.5,sd1=1, mean2=6,sd2=1, p.mix=.2)
  expect_identical(round(q,2),6.67)


#qnormMix 3"
  q <- qnormMix(.95,mean1=-5,sd1=1, mean2=5,sd2=1, p.mix=.5)
  expect_identical(round(q,2),6.28)


#qnormMix 4"
  q <- qnormMix(0.5, 10, 2, 20, 2, 0.1)
  expect_identical(round(q,2),10.28)


