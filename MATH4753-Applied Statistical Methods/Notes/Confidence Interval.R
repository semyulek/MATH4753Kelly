x<-rnorm(40,20,5)
library(Intro2R)
data(ddt)
x <-ddt$LENGTH
t<-qt(1-.05/2,length(x)-1)
mp<-c(-1,1)
ci <- mean(x)+mp*t*sd(x)/sqrt(length(x))
ci
boxplot(x)

t.test(x,conf.level = .95)$conf.int

y1=rnorm(30,20,5)
y2=rnorm(40,15,8)

t.test(y1,y2,conf.level = .95,var.equal = F) #mu1-mu2, so y1 then y2
t.test(y1,y2,conf.level = .95,var.equal = F)$conf.int #mu1-mu2, so y1 then y2
t.test(y2,y1,conf.level = .95,var.equal = F) #mu2-mu1, so y2 then y1

var.test(y1,y2)$conf.int
# sigma1^2/sigma2^2

t.test(y1,y2,conf.level = .95,var.equal = T)$conf.int #mu1-mu2, so y1 then y2

y1=rnorm(30,20,20)
y2=rnorm(40,15,8)
var.test(y1,y2)$conf.int # does not contain 1 in the interval, so variance is not equal

t.test(y1,y2,conf.level = .95,var.equal = F) # plausible the mu1=mu2 since 0 is in conf.int
