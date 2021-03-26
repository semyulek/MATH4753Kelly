# fake data
x <- rnorm(30,10,3)
ssq <-  var(x)
alpha <- .05
n <- 30
L <- (n-1)*ssq/qchisq(1-alpha/2,n-1)
U <- (n-1)*ssq/qchisq(alpha/2,n-1)
c(L,U)
#95% confidence interval for sigma
