ddt <- read.csv(file.choose())
head(ddt)
ddt$RIVER
ddt$LENGTH -> ll
mean(ll)
sd(ll)
with(ddt, ddt[LENGTH>50,])
names(ddt)
ddt$WEIGHT -> ww
sd(ww)
mean(ll)
