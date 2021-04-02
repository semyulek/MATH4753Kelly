x<-rnorm(40,20,5)
library(Intro2R)
data(ddt)
x <-ddt$LENGTH
t<-qt(1-.05/2,length(x)-1)
mp<-c(-1,1)
ci <- mean(x)+mp*t*sd(x)/sqrt(length(x))
ci
boxplot(x)
