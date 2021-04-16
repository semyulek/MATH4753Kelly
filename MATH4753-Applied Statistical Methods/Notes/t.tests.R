y<-0:10
p<-dbinom(y,size = 10,prob = 1/2)
names(p)<-y
coll <-rep(c(1,2,1),c(2,7,2))
windows();barplot(p, col = coll)


x<-rnorm(35,40,10)#fake since normally we don't know mu or sigma
t.test(x,mu = 25,conf.level = .95)
#t is t calc
# pvalue is evidence against null
qt(1-.05/2,35-1)
