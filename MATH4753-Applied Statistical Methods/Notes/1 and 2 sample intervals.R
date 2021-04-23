# 1 sample
x<-rnorm(30,10,5)
t.test(x = x,mu = 8,conf.level = .95)
qt(1-.05/2,30-1)

# 2 sample
y<-rnorm(40,8,5)
t.test(x,y,mu = 0,var.equal = T,conf.level = .95)
qt(1-.5/2,30+40-2)
var.test(x,y)
