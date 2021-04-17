#' @title Calculates 95% confidence interval for mu of the data
#'
#' @param x The sample data we want to estimate mu for
#'
#' @return A 95% confidence interval.
#' @export
#'
#' @examples
#' #sam=rnorm(10,mean=10,sd=5);myci(sam)
myci<-function(x){
  t=qt(1-.05/2,df = length(x) - 1)
  mp <- c(-1,1)
  ci <- mean(x)+mp*t*sd(x)/sqrt(length(x))
  ci
}
