library(ggplot2)
library(EnvStats)
before <- c(12.2, 14.6, 13.4, 11.2, 12.7, 10.4, 15.8, 13.9, 9.5, 14.2)
after <- c(13.5, 15.2, 13.6, 12.8, 13.7, 11.3, 16.5, 13.4, 8.7, 14.6)
data <- data.frame(subject = rep(c(1:10), 2),
                                       time = rep(c("before", "after"), each = 10),
                                       score = c(before, after))
ggplot(data,aes(x=time,y=score))+geom_point()+stat_test_text(paired=TRUE)
ggplot(data,aes(x=time,y=score))+geom_point()+stat_test_text()
